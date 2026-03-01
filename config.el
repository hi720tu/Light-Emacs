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

;; Global theme
(load-theme 'modus-operandi-tinted t)

;; Line highlight
(global-hl-line-mode 1)

;; Default font
(set-face-attribute 'default nil
		    :font "Martian Mono Nerd Font"
		    :height 120
		    :weight 'regular)

;; Font to be used during non-monospace contexts
(set-face-attribute 'variable-pitch nil
		    :font "Ubuntu"
		    :height 120
		    :weight 'medium)

;; Make sure new frames get it too
(add-to-list 'default-frame-alist
	     '(font . "Martian Mono Nerd Font-12"))

;; For Telugu script writing
(set-fontset-font t 'telugu "Noto Sans Telugu")

;; Line Spacing
(setq-default line-spacing 0.12)

(setq org-indent-indentation-per-level 2)

(add-hook 'org-mode-hook #'org-indent-mode)
(global-prettify-symbols-mode 1)

;; Special theme for code blocks
(custom-set-faces
     '(default ((t (:background "#fdf6e3" :foreground "#2e3440")))))

(setq org-src-fontify-natively t)

(custom-set-faces
 '(org-block ((t (:background "#e4e7ec" :extend t))))
 '(org-block-begin-line
   ((t (:background "#5e81ac" :foreground "#eceff4" :extend t))))
 '(org-block-end-line
   ((t (:background "#5e81ac" :foreground "#eceff4" :extend t)))))

;; Enable wrapping in Org-mode
(add-hook 'org-mode-hook 'visual-line-mode)

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

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode)
  :config (setq org-bullets-bullet-list '("✬" "ꕥ" "▶" "╰┈➤")))

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
