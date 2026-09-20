import { Plugin } from "@opencode/plugin"
import { execFile } from "node:child_process"
import { promisify } from "node:util"

const run = promisify(execFile)

async function applyDirenv(cwd: string, env: Record<string, string>) {
  try {
    const { stdout } = await run("direnv", ["export", "json"], { cwd })
    const localEnv = JSON.parse(stdout) as Record<string, string>
    Object.assign(env, localEnv)
    console.log(`[direnv] .envrc loaded (file=${localEnv.DIRENV_FILE}, cwd=${cwd})`)
  } catch (err) {
    console.error(`[direnv] .envrc failed to load (cwd=${cwd})`, err)
  }
}

export default {
  // OpenCode V2: loader reads `id` + `setup`, ignores `server()`
  ...Plugin.define({
    id: "direnv",
    async setup(ctx) {
      await ctx.shell.hook("create.before", async (event) => {
        await applyDirenv(event.cwd, event.env)
      })
    },
  }),

  // OpenCode V1 (>= 1.18.29): loader calls `server()`, ignores `id`/`setup`
  async server() {
    return {
      "shell.env": async (
        input: { cwd: string },
        output: { env: Record<string, string> },
      ) => {
        await applyDirenv(input.cwd, output.env)
      },
    }
  },
}
