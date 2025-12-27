;; -*- lexical-binding: t; no-byte-compile: t; -*-

(package! gptel :pin "d14d8c12f33829ea1615ac5316d7534d4175fe23")

(package! mcp :pin "6132ffc05e8ba5ac188b7f6b54a0fd3192f110d1")

(package! copilot :pin "6a2ad80489b8a0d021df95293eb7ac370aea140b")

(package! ai-code-interface
  :recipe (:host github :repo "tninja/ai-code-interface.el")
  :pin "133bcb5b9cbbbde402d69e90788dfcabbeaf5b3c")

;; ai-code-interface dependency:
(package! claude-code
  :recipe (:host github :repo "stevemolitor/claude-code.el")
  :pin "4a9914bd4161eb43f489820f9174c62390e5adc8")
