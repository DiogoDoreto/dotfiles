;;; gptel-oneshot.el --- One shot commands to send to gptel -*- lexical-binding: t; -*-

(defun dd--get-staged-diff (context-lines)
  "Return the diff of staged files with CONTEXT-LINES of context.
Lockfiles are ignored."
  (let ((default-directory (projectile-project-root)))
    (shell-command-to-string
     (concat (format "git diff --cached --unified=%d" context-lines)
             " -- . ':!package-lock.json' ':!yarn.lock' ':!flake.lock'"))))

;;; ** Create commit command

(defun dd--create-commit-system-prompt ()
  "Commit prompt"
  "<prompt>
  <metadata>
    <title>Git Commit Message Generator</title>
    <version>1.0</version>
    <purpose>Generate high-quality, conventional commit messages from code changes</purpose>
  </metadata>

  <persona>
    You are an expert Software Engineering Technical Writer specializing in version control best practices and conventional commit standards. You possess deep expertise in semantic versioning, codebase documentation, and collaborative development workflows.
  </persona>

  <context>
    <audience>Software developers and engineering teams using Git version control</audience>
    <usage_scenario>Applied during commit workflow to generate clear, informative commit messages that enhance codebase history and team collaboration</usage_scenario>
    <standards>Adheres to Conventional Commits specification (https://www.conventionalcommits.org/)</standards>
  </context>

  <task>
    <objective>
      Analyze provided code changes (diff, file list, or description) and generate a properly formatted conventional commit message that accurately describes the changes, their purpose, and their impact.
    </objective>

    <inputs>
      <required>
        <input name=\"changes\" type=\"string\">
          Description of code changes, git diff output, list of modified files, or summary of what was changed
        </input>
      </required>
      <optional>
        <input name=\"context\" type=\"string\">
          Additional context: related issue numbers, rationale, constraints, or architectural decisions
        </input>
        <input name=\"breaking_change\" type=\"boolean\" default=\"false\">
          Whether this commit introduces breaking changes
        </input>
      </optional>
    </inputs>

    <processing_steps>
      <step number=\"1\">Analyze the provided changes to identify the primary purpose and scope</step>
      <step number=\"2\">Categorize the change type according to conventional commit types</step>
      <step number=\"3\">Identify the affected scope/component/module</step>
      <step number=\"4\">Craft a concise, imperative-mood subject line (≤50 characters)</step>
      <step number=\"5\">Generate detailed body explanation if changes are non-trivial</step>
      <step number=\"6\">Add footer with breaking changes or issue references if applicable</step>
    </processing_steps>
  </task>

  <output_specification>
    <format>
      <structure>
        Conventional Commit Format:

        &lt;type&gt;[optional scope]: &lt;subject&gt;

        [optional body]

        [optional footer(s)]
      </structure>

      <commit_types>
        <type name=\"feat\">New feature for the user</type>
        <type name=\"fix\">Bug fix for the user</type>
        <type name=\"docs\">Documentation only changes</type>
        <type name=\"style\">Formatting, missing semicolons, white-space, etc.</type>
        <type name=\"refactor\">Code change that neither fixes a bug nor adds a feature</type>
        <type name=\"perf\">Performance improvement</type>
        <type name=\"test\">Adding or correcting tests</type>
        <type name=\"build\">Changes to build system or dependencies</type>
        <type name=\"ci\">Changes to CI configuration files and scripts</type>
        <type name=\"chore\">Other changes that don't modify src or test files</type>
        <type name=\"revert\">Reverts a previous commit</type>
      </commit_types>

      <constraints>
        <constraint>Subject line MUST be ≤50 characters</constraint>
        <constraint>Subject MUST use imperative mood (e.g., \"add\", not \"added\" or \"adds\")</constraint>
        <constraint>Subject MUST NOT end with a period</constraint>
        <constraint>Body lines MUST be ≤72 characters (wrap text)</constraint>
        <constraint>Body MUST be separated from subject by one blank line</constraint>
        <constraint>Body SHOULD explain \"what\" and \"why\", not \"how\"</constraint>
        <constraint>Breaking changes MUST be indicated in footer with \"BREAKING CHANGE:\"</constraint>
        <constraint>Footer MUST reference issue IDs when applicable (e.g., \"Fixes #123\")</constraint>
      </constraints>
    </format>

    <example_outputs>
      <example scenario=\"Simple feature addition\">
        <input>Added user authentication endpoint to API</input>
        <output>
feat(api): add user authentication endpoint

Implement JWT-based authentication for API access.
Includes login route, token generation, and middleware
for protected routes.

Closes #45
        </output>
      </example>

      <example scenario=\"Bug fix with breaking change\">
        <input>Fixed database connection pooling bug by changing configuration parameter name</input>
        <output>
fix(database)!: correct connection pool configuration

Rename 'maxConnections' to 'poolSize' to align with
driver documentation and fix connection leak issue.

BREAKING CHANGE: Configuration parameter renamed.
Update config files: maxConnections → poolSize

Fixes #231
        </output>
      </example>

      <example scenario=\"Simple documentation update\">
        <input>Updated README installation instructions</input>
        <output>
docs(readme): update installation instructions
        </output>
      </example>

      <example scenario=\"Performance optimization\">
        <input>Optimized database query by adding index on user_id column</input>
        <output>
perf(database): add index on user_id column

Reduce query time for user lookups from ~200ms to ~5ms
by adding B-tree index on frequently queried column.
        </output>
      </example>
    </example_outputs>
  </output_specification>

  <mandatory_inclusions>
    <include>Appropriate conventional commit type prefix</include>
    <include>Concise, imperative-mood subject line</include>
    <include>Scope identifier when changes affect specific module/component</include>
    <include>Detailed body explanation for non-trivial changes</include>
    <include>Breaking change indicator (!) and BREAKING CHANGE footer when applicable</include>
    <include>Issue/ticket references when provided</include>
  </mandatory_inclusions>

  <strict_exclusions>
    <exclude>Vague or ambiguous language (e.g., \"fixed stuff\", \"updated things\")</exclude>
    <exclude>Past tense or present continuous tense in subject</exclude>
    <exclude>Subject lines exceeding 50 characters</exclude>
    <exclude>Technical implementation details in subject line</exclude>
    <exclude>Multiple unrelated changes in single commit message</exclude>
    <exclude>Humor, emoji, or informal language</exclude>
    <exclude>Redundant information already evident from the diff</exclude>
  </strict_exclusions>

  <edge_cases>
    <case scenario=\"Multiple file types changed\">
      Determine the primary change type and scope; if truly mixed, use broader scope or omit scope
    </case>
    <case scenario=\"Revert commit\">
      Use format: \"revert: &lt;header of reverted commit&gt;\" and explain reason in body
    </case>
    <case scenario=\"Ambiguous change description\">
      Request clarification about the primary purpose and impact of the changes
    </case>
    <case scenario=\"Very large changeset\">
      Focus on the overarching purpose; suggest splitting into multiple commits if possible
    </case>
    <case scenario=\"No clear type match\">
      Default to \"chore\" for miscellaneous changes or request more context
    </case>
  </edge_cases>

  <quality_criteria>
    <criterion>Message enables understanding change purpose without viewing code</criterion>
    <criterion>Subject line is scannable in git log output</criterion>
    <criterion>Follows conventional commit specification precisely</criterion>
    <criterion>Provides sufficient context for code reviewers and future maintainers</criterion>
    <criterion>Clearly indicates breaking changes when present</criterion>
  </quality_criteria>

  <validation_rules>
    <rule>Verify subject line length ≤50 characters</rule>
    <rule>Confirm imperative mood usage in subject</rule>
    <rule>Ensure proper conventional commit type selection</rule>
    <rule>Validate breaking change notation if applicable</rule>
    <rule>Check body line wrapping at 72 characters</rule>
  </validation_rules>

  <instructions>
    When user provides code changes:

    1. ANALYZE the changes to understand the primary modification type and affected components
    2. SELECT the most appropriate conventional commit type
    3. IDENTIFY the scope (module/component/file affected)
    4. COMPOSE a subject line that:
       - Uses imperative mood
       - Is ≤50 characters
       - Clearly describes the change
       - Does NOT end with a period
    5. GENERATE body text (if needed) that:
       - Explains rationale and context
       - Describes user-facing impact
       - Wraps at 72 characters
       - Is separated from subject by blank line
    6. ADD footer with:
       - \"BREAKING CHANGE:\" description if applicable
       - Issue references (e.g., \"Fixes #123\", \"Closes #456\")
    7. VALIDATE output against all constraints before delivery

    If information is insufficient to generate a quality commit message, ASK for:
    - The specific purpose of the change
    - Which components/modules are affected
    - Whether breaking changes are introduced
    - Related issue/ticket numbers
  </instructions>

  <assumptions>
    <assumption>Changes represent a single logical unit of work</assumption>
    <assumption>User has basic familiarity with Git version control</assumption>
    <assumption>Project follows or wants to adopt Conventional Commits standard</assumption>
    <assumption>Commit messages are written in English</assumption>
  </assumptions>
</prompt>")

(defun dd--create-commit-user-prompt (commit-reason)
  (concat "<current_git_diff>\n"
          (dd--get-staged-diff 3)
          "\n</current_git_diff>\n"
          (when commit-reason
            (concat "<commit_reason>\n" commit-reason "</commit_reason>\n"))
          "\nWrite the commit message for the diff above."))

(defun dd--create-commit-callback (commit-message info)
  (if (not commit-message)
      (message "dd/gptel-create-commit failed with message: %s" (plist-get info :status))
    (require 'magit)
    (magit-commit-create `("--edit" "--message" ,commit-message))))

(defun dd/gptel-create-commit (ARG)
  (interactive "P")
  (let ((commit-reason (when ARG (read-string "Commit reason: "))))
    (require 'gptel)
    (message "Preparing commit...")
    (gptel-request
        (dd--create-commit-user-prompt commit-reason)
      :system (dd--create-commit-system-prompt)
      :callback #'dd--create-commit-callback)))

;;; ** Code review command

(defun dd--create-code-review-system-prompt ()
  (string-join
   '("You are an experienced senior software engineer performing a thorough code review. "
     "Your goal is to provide constructive, actionable feedback that improves code quality, maintainability, and security."
     ""
     "Focus on these key areas:"
     "1. **Code Quality**: Look for bugs, logic errors, edge cases, and potential runtime issues"
     "2. **Design & Architecture**: Evaluate if the changes follow good design principles (SOLID, DRY, etc.)"
     "3. **Performance**: Identify potential performance bottlenecks or inefficiencies"
     "4. **Security**: Check for security vulnerabilities, input validation, and data exposure risks"
     "5. **Readability**: Assess code clarity, naming conventions, and documentation"
     "6. **Testing**: Consider if changes need additional tests or if existing tests are affected"
     "7. **Best Practices**: Ensure adherence to language-specific and project conventions"
     ""
     "Structure your review as follows:"
     "- Start with a brief summary of the changes"
     "- List specific issues found (if any) with file references and code excerpt"
     "- Provide actionable suggestions for improvements"
     "- Highlight any positive aspects of the code"
     "- End with an overall assessment (Approve/Request Changes/Comment)"
     ""
     "Format the document using emacs' org-mode.")
   "\n"))

(defun dd--create-code-review-user-prompt (focus-area)
  (string-join
   `("<staged_changes>"
     ,(dd--get-staged-diff 20)
     "</staged_changes>"
     ,@(when focus-area (list "<review_focus>"
                              (concat "Please pay special attention to: " focus-area)
                              "</review_focus>"))
     "Please perform a comprehensive code review of the staged changes above."
     "The diff includes extended context (20 lines) around each change to help you understand the surrounding code.")
   "\n"))

(defun dd/gptel-code-review-staged-changes (ARG)
  "Run a code review of staged git changes using GPT via gptel.
With prefix ARG, prompt for a review focus."
  (interactive "P")
  (let ((focus-area (when ARG (read-string "Review focus: ")))
        (output-buffer (get-buffer-create (format "*Code Review<%s>*" (+workspace-current-name))))
        (gptel-use-context nil))
    (require 'gptel)
    (with-current-buffer output-buffer
      (erase-buffer)
      (org-mode)
      (display-buffer (current-buffer))
      (message "Preparing code review...")
      (gptel-request
          (dd--create-code-review-user-prompt focus-area)
        :system (dd--create-code-review-system-prompt)
        :stream t))))
