import type { Plugin } from "@opencode-ai/plugin"

export const DirenvPlugin: Plugin = async ({ project, client, $, directory, worktree }) => {
  return {
    async "shell.env"(input, output) {
      try {
        const localEnv = await $`direnv export json`.cwd(input.cwd).json();
        Object.assign(output.env, localEnv)
        client.app.log({
          body: {
            service: "direnv",
            level: "info",
            message: ".envrc loaded",
            extra: { DIRENV_FILE: localEnv.DIRENV_FILE, cwd: input.cwd },
          },
        })
      } catch (err) {
        client.app.log({
          body: {
            service: "direnv",
            level: "error",
            message: ".envrc failed to load",
            extra: { cwd: input.cwd, err },
          },
        })
      }
    },
  }
}
