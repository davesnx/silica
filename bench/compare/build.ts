import { copyFileSync, mkdirSync, readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { transformAsync } from "@babel/core";
import { build, type Plugin } from "esbuild";

const here = dirname(fileURLToPath(import.meta.url));

const solidJsx: Plugin = {
  name: "solid-jsx",
  setup(b) {
    b.onLoad({ filter: /\.jsx$/ }, async (args) => {
      const result = await transformAsync(readFileSync(args.path, "utf8"), {
        filename: args.path,
        presets: [["babel-preset-solid", { generate: "dom" }]],
        babelrc: false,
        configFile: false,
      });
      if (!result?.code) throw new Error(`babel produced no code for ${args.path}`);
      return { contents: result.code, loader: "js" };
    });
  },
};

const apps: Record<string, { entry: string; plugins: Plugin[] }> = {
  vanilla: { entry: "vanilla/main.js", plugins: [] },
  react: { entry: "react/main.jsx", plugins: [] },
  solid: { entry: "solid/main.jsx", plugins: [solidJsx] },
};

for (const [name, app] of Object.entries(apps)) {
  const out = resolve(here, "dist", name);
  mkdirSync(out, { recursive: true });
  await build({
    entryPoints: [resolve(here, app.entry)],
    bundle: true,
    minify: true,
    format: "esm",
    jsx: "automatic",
    define: { "process.env.NODE_ENV": '"production"' },
    conditions: ["browser", "production"],
    plugins: app.plugins,
    outfile: resolve(out, "main.js"),
    logLevel: "warning",
  });
  copyFileSync(resolve(here, "shared/index.html"), resolve(out, "index.html"));
}
