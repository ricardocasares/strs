// A thin promise wrapper over IndexedDB. Elm decides what is stored; this only knows the two
// object stores and how to read and write them.

import type { FromElm, JsonValue } from "./Main.elm";

type Operation = Extract<FromElm, { tag: "Write" }>["operations"][number];

const request = <T>(req: IDBRequest<T>) =>
  new Promise<T>((resolve, reject) => {
    req.onsuccess = () => resolve(req.result);
    req.onerror = () => reject(req.error);
  });

const connection = new Promise<IDBDatabase>((resolve, reject) => {
  const req = indexedDB.open("strs", 1);
  req.onupgradeneeded = () => {
    req.result.createObjectStore("repos", { keyPath: ["login", "id"] }).createIndex("login", "login");
    req.result.createObjectStore("accounts", { keyPath: "login" });
  };
  req.onsuccess = () => resolve(req.result);
  req.onerror = () => reject(req.error);
});

// Everything in the stores was written by Elm as JSON, so it reads back as JSON: IndexedDB
// types its results `any`, which the return types narrow without assertions.
export const loadUser = async (login: string): Promise<{ repos: JsonValue; account: JsonValue }> => {
  const tx = (await connection).transaction(["repos", "accounts"]);
  const [repos, account] = await Promise.all([
    request(tx.objectStore("repos").index("login").getAll(IDBKeyRange.only(login))),
    request(tx.objectStore("accounts").get(login)),
  ]);
  return { repos, account: account ?? null };
};

export const loadAccounts = async (): Promise<JsonValue> =>
  request((await connection).transaction("accounts").objectStore("accounts").getAll());

// Keys are what Elm encodes for them: a login, or a login and a repo id.
const isKey = (value: JsonValue): value is string | number | (string | number)[] =>
  typeof value === "string" ||
  typeof value === "number" ||
  (Array.isArray(value) && value.every((part) => typeof part === "string" || typeof part === "number"));

export const write = async (operations: Operation[]) => {
  const tx = (await connection).transaction(["repos", "accounts"], "readwrite");
  for (const op of operations) {
    const store = tx.objectStore(op.store);
    switch (op.tag) {
      case "Put":
        store.put(op.value);
        break;
      case "Delete":
        if (!isKey(op.key)) throw new Error(`Not a key: ${JSON.stringify(op.key)}`);
        store.delete(op.key);
        break;
      default:
        op satisfies never;
    }
  }
  await new Promise<void>((resolve, reject) => {
    tx.oncomplete = () => resolve();
    tx.onerror = () => reject(tx.error);
    tx.onabort = () => reject(tx.error ?? new Error("Write aborted"));
  });
};
