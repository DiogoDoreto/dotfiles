;; -*- lexical-binding: t; no-byte-compile: t; -*-

(package! gptel :pin "96199593c107767ffe0b59ab2a1f996c112e4fdf")

(package! mcp :pin "2e947d2ddc8cbe655f846e23711e412d41f1bf6a")

(package! shell-maker :pin "2d6e1302b457468b9becdde9549ce4b5d10116ab")
(package! acp :pin "9ab9b8f25cd7f42955171d471da5c3d016d1ef5a")
(package! agent-shell :pin "f69a4a27101f3788b37ca10dc2c3aba9f9c6f5b8")

(package! copilot :pin "7ee4758bb748beac7d29e62de5d2e752ebafb858")

(package! ai-code
  :recipe (:host github :repo "tninja/ai-code-interface.el")
  :pin "332978dec4eae9211030eb6328616014d653225b")

;; ai-code-interface dependency:
(package! claude-code
  :recipe (:host github :repo "stevemolitor/claude-code.el")
  :pin "4a9914bd4161eb43f489820f9174c62390e5adc8")
