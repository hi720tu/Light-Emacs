(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Official recommendation
(setq package-enable-at-startup nil)

;; Clean interface
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Adding relative line numbers
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)

;; Vertical window split as default
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; Smooth scrolling
(pixel-scroll-precision-mode t)
(setq scroll-step 1
      scroll-conservatively 10000
      scroll-margin 5)

;; Line highlight
(global-hl-line-mode 1)

;; Line Spacing
(setq-default line-spacing 0.12)

;; Main theme
(setq modus-themes-headings
      '((1 . (variable-pitch 1.5))
	(2 . (variable-pitch 1.3))
	(3 . (variable-pitch 1.1))
	(t . (variable-pitch 1.0))))
(load-theme 'modus-operandi-tinted t)

(set-face-attribute 'default nil :family "Iosevka")
(set-face-attribute 'fixed-pitch nil :family "Iosevka")
(set-face-attribute 'variable-pitch nil :family "Iosevka Aile")

(set-fontset-font t 'telugu "NTR")

(modify-all-frames-parameters
 '((right-divider-width . 40)
   (internal-border-width . 40)))
(dolist (face '(window-divider
		window-divider-first-pixel
		window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))

(add-hook 'org-mode-hook #'visual-line-mode)

(setq
 ;; Settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t
 org-startup-indented t
 org-startup-truncated nil
 ;; Styling
 org-indent-indentation-per-level 2
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-agenda-tags-column 0
 org-ellpisis "…")

(use-package org-roam
:ensure t
:custom
(org-roam-directory "~/Documents/RoamNotes")
(org-roam-node-display-template
 (concat "${title:*} " (propertize "${tags:10}" 'face 'font-lock-keyword-face)))
:bind (("C-c n t" . org-roam-buffer-toggle)
       ("C-c n f" . org-roam-node-find)
       ("C-c n i" . org-roam-node-insert)
       ("C-c n c" . org-roam-node-capture)
       ("C-c n g" . org-roam-graph))
:config
(org-roam-setup))

(setq org-return-follows-link t)

(use-package toc-org
  :ensure t
  :hook (org-mode . toc-org-mode))

(use-package org-modern
  :ensure t
  :config (global-org-modern-mode 1)
  (setq org-modern-star 'replace)
  (setq org-modern-hide-stars nil))

(use-package org-modern-indent
  :straight (org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent")
  :init (setq org-startup-indented t)
  :config (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package vertico
  :ensure t
  :init (vertico-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

(use-package marginalia
  :ensure t
  :init (marginalia-mode))

(use-package emmet-mode
  :ensure t
  :hook ((html-mode . emmet-mode)
	 (css-mode . emmet-mode)
	 (web-mode . emmet-mode)))

(defvar my-backup-dir "~/.emacs.d/backups/")
(defvar my-auto-save-dir "~/.emacs.d/auto-saves/")

(unless (file-exists-p my-backup-dir)
  (make-directory my-backup-dir t))
(unless (file-exists-p my-auto-save-dir)
  (make-directory my-auto-save-dir t))

(setq backup-directory-alist `(("." . ,my-backup-dir))
      auto-save-file-name-transforms `((".*" ,my-auto-save-dir t)))

;; Setup
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(set-language-environment "UTF-8")
