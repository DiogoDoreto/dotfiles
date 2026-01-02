;; -*- lexical-binding: t; no-byte-compile: t; -*-

(package! gptel :pin "93d58e4cce68c8f603562904f77a3e329eaf5a24")

(package! mcp :pin "125e0a4478ff1404880ea4e593f5e4ff0122cb83")

(package! copilot :pin "7ee4758bb748beac7d29e62de5d2e752ebafb858")

(package! ai-code
  :recipe (:host github :repo "tninja/ai-code-interface.el")
  :pin "332978dec4eae9211030eb6328616014d653225b")

;; ai-code-interface dependency:
(package! claude-code
  :recipe (:host github :repo "stevemolitor/claude-code.el")
  :pin "4a9914bd4161eb43f489820f9174c62390e5adc8")
