{ config, lib, pkgs-unstable, ... }:

with lib;

let
  cfg = config.dog.programs.aider;

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
  options.dog.programs.aider = {
    enable = mkEnableOption "Aider-Chat";
    ollamaApiBase = mkOption {
      type = types.str;
      default = "http://127.0.0.1:11434";
    };
  };

  config = mkIf cfg.enable {
    home = {
      packages = with pkgs-unstable; [
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
      ];

      sessionVariables = {
        OLLAMA_API_BASE = cfg.ollamaApiBase;
      };

      file = {
        ".aider.conf.yml" = {
          text = builtins.toJSON {
            architect = true;
            model = "ollama_chat/qwq:latest";
            editor-model = "ollama_chat/qwen2.5-coder:32b";
            dark-mode = true;
            attribute-author = false;
            attribute-committer = false;
            alias = [
              "qwq:ollama_chat/qwq:latest"
              "qwen:ollama_chat/qwen2.5-coder:32b"
              "qwen-small:ollama_chat/qwen2.5-coder:14b"
            ];
          };
        };
        ".aider.model.settings.yml" = let
          qwen = {
            name = "ollama_chat/qwen2.5-coder";
            use_repo_map = true;
            edit_format = "diff";
          };
        in {
          text = builtins.toJSON [
            (qwen // {
              name = qwen.name + ":14b";
              extra_params = { num_ctx = 32768; };
            })
            (qwen // {
              name = qwen.name + ":32b";
              extra_params = { num_ctx = 10000; };
            })
            {
              name = "ollama_chat/qwq:latest";
              reasoning_tag = "think";
            }
          ];
        };
      };
    };
  };
}
