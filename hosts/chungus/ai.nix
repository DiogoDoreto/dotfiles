{ config, pkgs-unstable, ... }:

let
  whisper-pkg = pkgs-unstable.openai-whisper-cpp.override {
    config = {
      cudaSupport = true;
      rocmSupport = false;
    };
  };

  python = pkgs-unstable.python312.override {
    self = python;
    packageOverrides = pyfinal: pyprev: {
      grep-ast = pyprev.grep-ast.overridePythonAttrs rec {
        version = "0.8.1";
        src = pkgs-unstable.fetchPypi {
          inherit version;
          pname = "grep_ast";
          hash = "sha256-j68oX0QEKvR9xqRfHh+AKYZgSFY9dYpxmwU5ytJkGH8=";
        };
        dependencies = with pyfinal; [
          pathspec
          tree-sitter-language-pack
        ];
        doCheck = false; # tests are still depending on old tree-sitter-languages
      };
      tree-sitter-c-sharp = pyfinal.callPackage ./tree-sitter-c-sharp.nix { };
      tree-sitter-embedded-template = pyfinal.callPackage ./tree-sitter-embedded-template.nix { };
      tree-sitter-yaml = pyfinal.callPackage ./tree-sitter-yaml.nix { };
      tree-sitter-language-pack = pyfinal.callPackage ./tree-sitter-language-pack.nix { };
    };
  };

in {
  home = {
    packages = with pkgs-unstable; [
      whisper-pkg

      (aider-chat.overridePythonAttrs rec {
        version = "0.78.0";
        src = fetchFromGitHub {
          owner = "Aider-AI";
          repo = "aider";
          tag = "v${version}";
          hash = "sha256-6WrlhgHkoGRJnkY4XQOVBKLRVZ8u8ttulR9lm+WRKeg=";
        };
        dependencies = with python.pkgs; [
          # add new dep:
          tree-sitter-language-pack

          # old deps:
          aiohappyeyeballs
          aiohttp
          aiosignal
          annotated-types
          anyio
          attrs
          backoff
          beautifulsoup4
          certifi
          cffi
          charset-normalizer
          click
          configargparse
          diff-match-patch
          diskcache
          distro
          filelock
          flake8
          frozenlist
          fsspec
          gitdb
          gitpython
          grep-ast # UPDATED
          h11
          httpcore
          httpx
          huggingface-hub
          idna
          importlib-resources
          jinja2
          jiter
          json5
          jsonschema
          jsonschema-specifications
          litellm
          markdown-it-py
          markupsafe
          mccabe
          mdurl
          multidict
          networkx
          numpy
          openai
          packaging
          pathspec
          pexpect
          pillow
          prompt-toolkit
          psutil
          ptyprocess
          pycodestyle
          pycparser
          pydantic
          pydantic-core
          pydub
          pyflakes
          pygments
          pypandoc
          pyperclip
          python-dotenv
          pyyaml
          referencing
          regex
          requests
          rich
          rpds-py
          scipy
          smmap
          sniffio
          sounddevice
          socksio
          soundfile
          soupsieve
          tiktoken
          tokenizers
          tqdm
          tree-sitter # UPDDATED
          # REMOVE: tree-sitter-languages
          typing-extensions
          urllib3
          watchfiles
          wcwidth
          yarl
          zipp
          pip

          # Not listed in requirements
          mixpanel
          monotonic
          posthog
          propcache
          python-dateutil
        ];
      })

      (comfyuiPackages.comfyui.override {
        extensions = [
          comfyuiPackages.extensions.acly-inpaint
          comfyuiPackages.extensions.acly-tooling
          comfyuiPackages.extensions.cubiq-ipadapter-plus
          comfyuiPackages.extensions.fannovel16-controlnet-aux
        ];
        commandLineArgs = [ "--preview-method" "auto" ];
      })
      (comfyuiPackages.krita-with-extensions.override {
        krita-ai-diffusion = (comfyuiPackages.krita-ai-diffusion.overrideAttrs {
          src = fetchFromGitHub {
            owner = "Acly";
            repo = "krita-ai-diffusion";
            fetchSubmodules = true;
            rev = "9536fd028ed8b88525268b2c44585cbee50be3e8";
            hash = "sha256-EjMpM2+Cz7nDXF16s//D5XrC0sGOsnLVEmHcsDlvIdo=";
          };
          patches = [];
        });
      })
    ];
  };

  dog.programs.emacs = {
    whisperPackage = whisper-pkg;
    extraConfig = ''
      (setq whisper-model "large-v3-turbo")
    '';
  };

  # Ollama
  systemd.user.services = {
    ollama = {
      Unit.Description = "Run Ollama";
      Install.WantedBy = [ "default.target" ];
      Service.ExecStart = "${pkgs-unstable.ollama-cuda}/bin/ollama serve";
    };
  };

  # Aider Chat
  home.sessionVariables = {
    OLLAMA_API_BASE = "http://127.0.0.1:11434";
  };
  home.file = {
    ".aider.conf.yml" = {
      text = builtins.toJSON {
        architect = true;
        model = "ollama_chat/deepseek-r1:32b";
        editor-model = "ollama_chat/qwen2.5-coder:32b";
        dark-mode = true;
        attribute-author = false;
        attribute-committer = false;
        alias = [
          "r1:ollama_chat/deepseek-r1:32b"
          "r1-small:ollama_chat/deepseek-r1:14b"
          "qwen:ollama_chat/qwen2.5-coder:32b"
          "qwen-small:ollama_chat/qwen2.5-coder:14b"
        ];
      };
    };
    ".aider.model.settings.yml" = let
        deepseek = {
          name = "ollama_chat/deepseek-r1";
          # remove_reasoning = "think";
          use_temperature = false;
          use_repo_map = true;
          edit_format = "diff";
        };
        qwen = {
          name = "ollama_chat/qwen2.5-coder";
          use_repo_map = true;
          edit_format = "diff";
        };
      in {
      text = builtins.toJSON [
        (deepseek // {
          name = deepseek.name + ":14b";
          extra_params = { num_ctx = 48000; };
        })
        (deepseek // {
          name = deepseek.name + ":32b";
          extra_params = { num_ctx = 10000; };
        })
        (qwen // {
          name = qwen.name + ":14b";
          extra_params = { num_ctx = 32768; };
        })
        (qwen // {
          name = qwen.name + ":32b";
          extra_params = { num_ctx = 10000; };
        })
      ];
    };
  };

  # ComfyUI
  xdg.desktopEntries.comfyui = {
    name = "ComfyUI";
    categories = [ "Application" "Graphics" ];
    terminal = true;
    exec = toString (pkgs-unstable.writeShellScript "start-comfyui.sh" ''
      cd ${config.xdg.dataHome}/comfyui
      comfyui
    '');
  };
}
