// Records a user's stars for the mock API in dev/mock-github.ts: bun run record <login>
// It goes through the authenticated gh CLI, so the unauthenticated budget the app uses
// isn't touched.
import { mkdir } from "node:fs/promises";
import { join, relative } from "node:path";
import { $ } from "bun";

const perPage = 100;
const login = Bun.argv[2];

// GitHub logins: letters, digits and single hyphens, which also keeps the path inside fixtures.
if (login === undefined || !/^[a-z\d](?:-?[a-z\d])*$/i.test(login)) {
  console.error("usage: bun run record <login>");
  process.exit(1);
}

const dir = join(import.meta.dir, "fixtures", login);
await mkdir(dir, { recursive: true });

// Saves gh's output as it is, so fixtures are byte for byte what GitHub sent.
const record = async (file: string, args: string[]): Promise<unknown> => {
  const body = await $`gh api ${args}`.text();
  await Bun.write(join(dir, file), body);
  return JSON.parse(body);
};

await record("user.json", [`users/${login}`]);

// The star+json type adds when each repo was starred. A short page is the last one.
let pages = 0;
let full = true;
while (full) {
  pages += 1;
  const stars = await record(`page-${pages}.json`, [
    "-H",
    "Accept: application/vnd.github.star+json",
    `users/${login}/starred?per_page=${perPage}&page=${pages}`,
  ]);
  full = Array.isArray(stars) && stars.length === perPage;
}

console.log(`Recorded ${pages} pages into ${relative(process.cwd(), dir)}`);
