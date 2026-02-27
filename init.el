;;; Literate Configuration
;; Loads org file and extracts all the elisp code
;; Sourced from "config.org" in the Emacs config directory
(require 'org)
(org-babel-load-file
 (expand-file-name "config.org" user-emacs-directory))
