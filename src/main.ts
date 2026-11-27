import "./UI/elmcn-dialog";
import { Elm, type Flags } from "./Main.elm";
import * as idb from "./idb";

//-- FLAGS

const media = window.matchMedia("(prefers-color-scheme: dark)");

const setting = (key: string) => {
  try {
    return localStorage.getItem(key);
  } catch {
    return null;
  }
};

const blockedUntil = Number(setting("blockedUntil"));

const theme = (value: string | null): Flags["theme"] =>
  value === "light" || value === "dark" || value === "system" ? value : null;

//-- ELEMENTS

// <in-view> fires `visible` when it scrolls near the viewport, for loading more results.
customElements.define(
  "in-view",
  class extends HTMLElement {
    #observer = new IntersectionObserver(
      (entries) => entries.some((entry) => entry.isIntersecting) && this.dispatchEvent(new Event("visible")),
      { rootMargin: "800px 0px" },
    );
    connectedCallback() {
      this.#observer.observe(this);
    }
    disconnectedCallback() {
      this.#observer.disconnect();
    }
  },
);

//-- INIT

const app = Elm.Main.init({
  flags: {
    theme: theme(setting("theme")),
    systemDark: media.matches,
    blockedUntil: Number.isFinite(blockedUntil) && blockedUntil > 0 ? blockedUntil : null,
    api: import.meta.env.VITE_GITHUB_API ?? "https://api.github.com",
    now: Date.now(),
    sidebarOpen: setting("sidebar") === null ? null : setting("sidebar") === "open",
  },
});

const fail = (error: unknown) =>
  app.ports.interopToElm.send({
    tag: "StorageFailed",
    message: error instanceof Error ? error.message : String(error),
  });

//-- PORTS:SEND

media.addEventListener("change", (event) =>
  app.ports.interopToElm.send({ tag: "SystemThemeChanged", dark: event.matches }),
);

//-- PORTS:SUBSCRIBE

app.ports.interopFromElm.subscribe((fromElm) => {
  switch (fromElm.tag) {
    case "LoadUser":
      return idb
        .loadUser(fromElm.login)
        .then(({ repos, account }) =>
          app.ports.interopToElm.send({ tag: "UserLoaded", login: fromElm.login, repos, account }),
        )
        .catch(fail);

    case "LoadAccounts":
      return idb
        .loadAccounts()
        .then((accounts) => app.ports.interopToElm.send({ tag: "AccountsLoaded", accounts }))
        .catch(fail);

    case "Write":
      return idb.write(fromElm.operations).catch(fail);

    case "SaveSetting":
      try {
        if (fromElm.value === null) localStorage.removeItem(fromElm.key);
        else localStorage.setItem(fromElm.key, fromElm.value);
      } catch {
        // Private mode: settings last for the session.
      }
      return;

    case "OpenUrl":
      return window.open(fromElm.url, "_blank", "noopener");

    // A message Elm can send but this switch doesn't handle fails to type-check here.
    default:
      return fromElm satisfies never;
  }
});
