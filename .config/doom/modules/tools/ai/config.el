;;; config.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Diogo Doreto
;;
;; Author: Diogo Doreto
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(load! "whisper.el")
(load! "gptel-oneshot.el")

(use-package! mcp
  :after gptel
  :config
  (setq mcp-hub-servers
        `(("playwright" . (:command "podman"
                           :args ,(string-split "run -i --rm --replace --name playwright-mcp --init --pull=always mcr.microsoft.com/playwright/mcp"))))))

(defun +mcp/prepare-filesystem (root)
  (interactive "DProject root directory: ")
  (let* ((root (expand-file-name root))
         (cmd (string-split (format "podman run -i --rm --replace --name filesystem-mcp --mount type=bind,src=%s,dst=%s docker.io/mcp/filesystem %s" root root root))))
    (mcp-stop-server "filesystem")
    (gptel-mcp-disconnect '("filesystem"))
    (setf (alist-get "filesystem" mcp-hub-servers nil t #'string=)
          `(:command ,(car cmd) :args ,(cdr cmd)))
    (gptel-mcp-connect '("filesystem") nil t)))

(use-package! gptel
  :defer t
  :config
  (require 'gptel-integrations)
  (setq gptel-display-buffer-action nil)  ; if user changes this, popup manager will bow out
  (set-popup-rule!
    (lambda (bname _action)
      (and (null gptel-display-buffer-action)
           (buffer-local-value 'gptel-mode (get-buffer bname))))
    :select t
    :side 'right
    :width 80
    :quit nil
    :ttl nil)

  (defun dd/gptel-context--use-file-name (orig-fun buffer contexts)
    "Advice to prefer file name over buffer name in the first line."
    (insert
     (with-temp-buffer
       (funcall orig-fun buffer contexts)
       (when-let* ((filename (buffer-file-name buffer)))
         (goto-char (point-min))
         (when (looking-at "In buffer `.*`:")
           (delete-region (point) (line-end-position))
           (insert (format "In file `%s`:" filename))))
       (buffer-string))))

  (advice-add 'gptel-context--insert-buffer-string
              :around #'dd/gptel-context--use-file-name)

  (setq gptel--known-backends (assoc-delete-all "ChatGPT" gptel--known-backends))

  (load! "my-gptel-tools.el")

  (when (s-equals? "dogdot" (system-name))
    (gptel-make-ollama "Ollama"
      :host "chungus.home:11434"
      :stream t
      :models '(qwen2.5-coder:32b deepseek-r1:32b qwq)))

  (setq gptel-model 'grok-code-fast-1
        gptel-backend (gptel-make-gh-copilot "Copilot")
        gptel-default-mode 'org-mode
        gptel-confirm-tool-calls nil)

  (defun dd--gh-parse-enabled-models (api-response)
    "Return a list of models with policy state `enabled' from the API-RESPONSE."
    (let* ((data (cdr (assoc 'data api-response))))
      ;; (pp data)
      (delq nil
            (mapcar (lambda (model)
                      (let* ((policy (assoc 'policy model))
                             (state (when policy (cdr (assoc 'state (cdr policy)))))
                             (id (cdr (assoc 'id model))))
                        (when (and state (string= state "enabled"))
                          id)))
                    data))))

  (defun dd/gh-request-enabled-models ()
    "Call Github's models API and print the model-ids that are currently enabled"
    (interactive)
    (require 'request)
    (gptel--gh-auth)
    (request "https://api.githubcopilot.com/models"
      :sync t
      :type "GET"
      :headers `(("Authorization" . ,(concat "Bearer "
                                             (plist-get (gptel--gh-token gptel-backend) :token)))
                 ("copilot-integration-id" . "vscode-chat")
                 ("content-type" . "application/json"))
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (message "Github's enabled models: %S" (dd--gh-parse-enabled-models data))))))

  (setf (alist-get 'architect gptel-directives)
        "I want you to act as an IT Architect. I will provide some details about the functionality of an application or other digital product, and it will be your job to come up with ways to integrate it into the IT landscape. This could involve analyzing business requirements, performing a gap analysis and mapping the functionality of the new system to the existing IT landscape. Next steps are to create a solution design and, if needed, a physical network blueprint, definition of interfaces for system integration and a blueprint for the deployment environment.")

  (setf (alist-get 'sw-engineer gptel-directives)
        "You are a senior software engineer, programming expert, who provides precise answers, avoiding ambiguous responses.  Identify any complex or difficult-to-understand descriptions in the provided text.  Rewrite these descriptions to make them clearer and more accessible.  Take a deep breath, let's work this out in a step-by-step way to be sure we have the right answer.")

  (setf (alist-get 'assistant gptel-directives)
        "* Core Operating Principles for AI Assistant:

These principles serve as the foundational guidelines for all AI operations, ensuring optimal performance, reliability, and ethical conduct. Strict adherence is mandatory to provide the highest quality of service.

1. Optimal Execution: Execute tasks with precision and maximum efficiency, employing meticulous planning and judicious resource management.
2. Clear & Direct Communication: Deliver concise, relevant, and unambiguous responses, strictly avoiding verbosity, unverified speculation, or extraneous information.
3. Strategic Tool Utilization: Select and apply the most appropriate and effective tools and resources, always prioritizing authoritative and reliable sources.
4. Rigorous Output Validation: Thoroughly verify all generated outputs for accuracy, cross-referencing against independent and credible information sources.
5. Continuous Learning & Improvement: Actively analyze performance metrics, integrate new knowledge, and refine operational strategies to continuously enhance capabilities and adapt to evolving requirements.
6. Ethical & User-Centric Conduct: Maintain an unvaryingly neutral, professional, helpful, safe, unbiased, and ethical demeanor, consistently prioritizing user well-being, data privacy, and security.
7. Proactive Clarification & Intent Understanding: Diligently identify and resolve any ambiguities or gaps in instructions. Actively seek clarification to ensure a complete and accurate understanding of user intent before proceeding.
8. Transparent Reporting & Limitation Acknowledgment: Clearly communicate the outcome of all tasks (successful, partially fulfilled, or uncompletable), providing detailed and actionable explanations. Candidly acknowledge any inherent limitations of the AI or the current operational context.
9. Contextual Awareness & Adaptability: Continuously assess and adapt to the evolving context of the interaction and task. Tailor responses and actions to best fit the current situation and user needs.")

  (setf (alist-get 'prompt-writer gptel-directives)
        "As an Elite Prompt Engineer, your unwavering and paramount mission is to design and meticulously craft prompts that consistently elicit optimal, precisely accurate, and unequivocally actionable model responses. Your prompts are not mere instructions; they are architected as imperative, unambiguous specifications, firmly grounded upon these four foundational, non-negotiable pillars:

* Clarity: Eliminate all potential for misinterpretation through unambiguous language and explicit, direct instructions. Leave absolutely no conceptual void or room for subjective inference.
* Completeness: Ensure exhaustive coverage of all explicit and implicitly required information. The model must be holistically equipped with every critical datum, constraint, and contextual detail to execute its task.
* Specificity: Enforce rigorous, explicit constraints on all parameters. Precisely define response length, stylistic attributes, emotional tone, permissible content, and verbosity. Mandate exact output formats using formal schemas or illustrative examples.
* Testability: Engineer prompts to generate verifiable, predictably consistent, and unfailingly repeatable outcomes. This enables robust, automated evaluation and validation of model performance.

To consistently uphold this exacting standard and prevent costly inefficiencies and erroneous outputs, you are imperatively mandated to unequivocally adhere to the following strategic directives:

1. Deconstruct User Intent & Task (Holistic Analysis): Commence by conducting an exhaustive deconstruction of the overarching user intent and the precise task objective. Systematically decompose complex requests into discrete, logically sequenced components, meticulously identifying all requisite inputs, intricate internal processing logic, and the exact final output state.
2. Establish Persona, Audience & Context (Strategic Framing): Unequivocally establish the model's designated persona, the precise target audience for its generated content, and the operational context. These parameters definitively govern the appropriate tone, stylistic conventions, required knowledge domains, and the essential granularity of detail for optimal comprehension.
3. Define Strict Inclusions & Exclusions (Constraint Enforcement): Precisely delineate all mandatory content inclusions and explicitly prohibit all proscribed elements. Enforce stringent constraints on response length, stylistic attributes, emotional tone, verbosity, and permissible content, thereby precisely shaping and rigorously controlling the model's generative output.
4. Prescribe Output Format with Schema/Examples (Integrity & Parsability): Strictly mandate the precise output structure. Employ formal specifications (e.g., JSON Schema, XML, defined Markdown structures) and furnish high-fidelity, representative examples to unequivocally demonstrate the exact format, encompassing data types and hierarchies. This approach guarantees seamless, predictable parsing and robust integration into downstream systems.,
5. Implement Few-Shot Prompting (In-Context Learning & Behavioral Anchoring): Strategically implement Few-Shot Prompting by providing exemplary, high-quality input-output demonstrations. These examples must unequivocally demonstrate the desired behavior, articulate the underlying reasoning processes, and exemplify the precise output format. This practice critically augments model comprehension, substantially mitigates hallucination, and ensures superior response consistency.
6. Proactively Resolve Ambiguity & Document Assumptions (Transparency & Precision): Proactively identify and systematically eliminate all potential sources of ambiguity. If complete clarification is infeasible, explicitly articulate and document all well-reasoned assumptions directly within the prompt, thereby preempting misinterpretation and ensuring absolute transparency.
7. Architect for Maximal Robustness (Edge Case Mitigation): Engineer for Maximal Robustness by diligently anticipating and comprehensively addressing all conceivable edge cases. Foresee potential ambiguities, anomalous inputs, or significant deviations from nominal operating conditions. Construct prompts defensively to effectively preempt, manage, or gracefully mitigate these challenges, guaranteeing exceptionally resilient and robust performance across the full spectrum of operational scenarios.
8. Respect Model Capabilities & Acknowledge Limitations (Feasibility & Efficacy): Operate strictly within the established capabilities and acknowledged limitations of the target model. Refrain from formulating requests for outputs that are inherently impossible, demonstrably unreliable, or computationally intractable, thereby ensuring alignment with its validated operational capacity and maximizing efficacy.
9. Systematically Iterate, Rigorously Validate & Continuously Optimize (Performance & Refinement): Systematically engage in relentless testing of prompts against precisely defined success metrics and authentic, diverse real-world data. Methodically analyze model responses, meticulously gather comprehensive, actionable feedback, and iteratively refine prompts to achieve paramount clarity, maximal efficiency, and unassailable robustness, thereby propelling continuous improvement towards the delivery of high-fidelity, production-grade outcomes.

Absolute and unwavering adherence to these strategic directives is not merely encouraged—it is an existential requirement. They constitute the foundational bedrock for engineering prompts that unfailingly deliver efficient, maximally effective, and demonstrably superior model interactions, thereby directly contributing to the success of all downstream applications and user experiences."))

(use-package! aidermacs
  :defer t
  :config
  (setq aidermacs-backend 'vterm))

(use-package! copilot
  ;; :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("C-i" . 'copilot-accept-completion)
              ("M-i" . 'copilot-accept-completion-by-word)))

(map! :leader
      (:prefix ("l" . "LLM")
       :desc "Open chat buffer"  "o" #'gptel
       :desc "Send"              "RET" #'gptel-send
       :desc "Menu"              "l" #'gptel-menu
       :desc "Add buffer/region" "a" #'gptel-add
       :desc "Abort"             "x" #'gptel-abort
       :desc "Rewrite region" :v "r" #'gptel-rewrite
       :desc "Setup File System MCP" "f" #'+mcp/prepare-filesystem

       :desc "Buffer Copilot"    "i" #'copilot-mode
       :desc "Global Copilot"    "I" #'global-copilot-mode

       :desc "model=gpt-4.1"           "1" (cmd! (setq gptel-model 'gpt-4.1))
       :desc "model=gpt-5"             "2" (cmd! (setq gptel-model 'gpt-5))
       :desc "model=gpt-4o"            "3" (cmd! (setq gptel-model 'gpt-4o))
       :desc "model=claude-sonnet-4.5" "4" (cmd! (setq gptel-model 'claude-sonnet-4.5))
       :desc "model=grok-code-fast-1"  "5" (cmd! (setq gptel-model 'grok-code-fast-1))

       :desc "Aider" "d" #'aidermacs-transient-menu

       :desc "Whisper Run"       "w" #'whisper-run
       :desc "Whisper File"      "W" #'whisper-file))

(map! :leader
      (:prefix ("l ." . "OneShot cmds")
       :desc "Create commit"         "c" #'dd/gptel-create-commit
       :desc "Review staged changes" "r" #'dd/gptel-code-review-staged-changes))

(defun whisper-command (input-file)
  "Produces whisper command to be run on the INPUT-FILE."
  (let ((model-file (expand-file-name (format "ggml-%s.bin" whisper-model)
                                      whisper-cpp-models-directory)))
    (unless (file-exists-p model-file)
      (if (yes-or-no-p (format "Model [%s] does not exist. Download to %s?" whisper-model whisper-cpp-models-directory))
          (progn (whisper-download-model)
                 (user-error "Model is downloading. Please try recording again when completed."))
        (user-error "Cannot transcribe. %s does not exist." model-file)))
    `(,(expand-file-name "bin/whisper-cli" whisper-cpp-directory)
      "--model" ,model-file
      ,input-file)))

(setq whisper-return-cursor-to-start nil)

(defun whisper-download-model ()
  "Download a Whisper model using the external whisper-cpp downloader.

Creates the directory `whisper-cpp-models-directory' if needed, then runs the
\"whisper-cpp-download-ggml-model\" binary from `whisper-cpp-directory' to
download the model specified by `whisper-model'."
  (interactive)
  (make-directory whisper-cpp-models-directory t)
  (let ((default-directory whisper-cpp-models-directory)
        (download-cmd (expand-file-name "bin/whisper-cpp-download-ggml-model" whisper-cpp-directory)))
    (compile (concat download-cmd " " whisper-model))))

(defun dd/clean-whisper-transcription ()
  "Remove Whisper timestamps and join buffer text into a single line."
  (goto-char (point-min))
  (let ((timestamp-regexp (rx line-start "[" (+ (any digit space ?- ":.>")) "]" (* space))))
    (while (re-search-forward timestamp-regexp nil t)
      (replace-match "")))
  (goto-char (point-min))
  (let ((linebreak-regexp (rx (* space) "\n" (* space))))
    (while (re-search-forward linebreak-regexp nil t)
      (replace-match " "))))

(add-hook 'whisper-after-transcription-hook #'dd/clean-whisper-transcription)

(load! "whisper-config.el")
