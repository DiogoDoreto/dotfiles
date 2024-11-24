# Using flakes for development

Create a `.envrc` file in the root of the project following this template:

```
nix_direnv_manual_reload
use flake <flake_path>
```

where `<flake_path>` could be `.` if the project has its own `flake.nix` file or
the path to a shared flake directory like `~/project/dotfiles/flakes/node`.

Run `direnv allow` to authorize the usage of that file.

Run `nix-direnv-reload` to update dependencies when needed.
