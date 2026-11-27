// A thin promise wrapper over IndexedDB. Elm decides what is stored; this only knows the two
// object stores and how to read and write them.

type Operation =
  | { tag: "Put"; store: "repos" | "accounts"; value: unknown }
  | { tag: "Delete"; store: "repos" | "accounts"; key: unknown };

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

export const loadUser = async (login: string) => {
  const tx = (await connection).transaction(["repos", "accounts"]);
  const [repos, account] = await Promise.all([
    request(tx.objectStore("repos").index("login").getAll(IDBKeyRange.only(login))),
    request(tx.objectStore("accounts").get(login)),
  ]);
  return { repos, account: account ?? null };
};

export const loadAccounts = async () =>
  request((await connection).transaction("accounts").objectStore("accounts").getAll());

export const write = async (operations: Operation[]) => {
  const tx = (await connection).transaction(["repos", "accounts"], "readwrite");
  for (const op of operations) {
    const store = tx.objectStore(op.store);
    if (op.tag === "Put") store.put(op.value);
    else store.delete(op.key as IDBValidKey);
  }
  await new Promise<void>((resolve, reject) => {
    tx.oncomplete = () => resolve();
    tx.onerror = () => reject(tx.error);
    tx.onabort = () => reject(tx.error ?? new Error("Write aborted"));
  });
};
