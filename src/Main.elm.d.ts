export type JsonObject = { [Key in string]?: JsonValue };
export type JsonArray = JsonValue[];

/**
Matches any valid JSON value.
Source: https://github.com/sindresorhus/type-fest/blob/master/source/basic.d.ts
*/
export type JsonValue =
  | string
  | number
  | boolean
  | null
  | JsonObject
  | JsonArray;

export interface ElmApp {
  ports: {
    interopFromElm: PortFromElm<FromElm>;
    interopToElm: PortToElm<ToElm>;
    [key: string]: UnknownPort;
  };
}

export type FromElm = { tag : "OpenTab"; url : string } | { key : string; tag : "SaveSetting"; value : string | null } | { operations : ({ key : JsonValue; store : "accounts" | "repos"; tag : "Delete" } | { store : "accounts" | "repos"; tag : "Put"; value : JsonValue })[]; tag : "Write" } | { login : string; tag : "LoadUser" };

export type ToElm = { dark : boolean; tag : "SystemThemeChanged" } | { account : JsonValue; login : string; repos : JsonValue; tag : "UserLoaded" } | { message : string; tag : "StorageFailed" };

export type Flags = { blockedUntil : number | null; commit : string | null; now : number; sidebarOpen : boolean | null; systemDark : boolean; theme : ("light" | "dark" | "system") | null };

export namespace Main {
  function init(options: { node?: HTMLElement | null; flags: Flags }): ElmApp;
}

export as namespace Elm;

export { Elm };

export type UnknownPort = PortFromElm<unknown> | PortToElm<unknown> | undefined;

export type PortFromElm<Data> = {
  subscribe(callback: (fromElm: Data) => void): void;
  unsubscribe(callback: (fromElm: Data) => void): void;
};

export type PortToElm<Data> = { send(data: Data): void };
