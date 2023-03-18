;; configure use-package to use and be managed by
;; straight.el
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; org - markdown language for emacs
(use-package org
  :config
  (setq org-src-tab-acts-natively t
	org-edit-src-content-indentation 0
	org-hide-emphasis-markers t)
  (add-to-list 'org-modules 'org-tempo)
  :hook
  (org-mode . variable-pitch-mode))
(use-package org-contrib
  :after
  org)
(use-package org-modern
  :after org
  :bind
  ("C-c m" . org-modern-mode))
  ;; :hook
  ;; (org-mode . org-modern-mode)
  ;; :init
  ;; (global-org-modern-mode))

(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode)
  :config
  (set-face-attribute 'fixed-pitch nil :font "Fira Mono")
  (set-face-attribute 'variable-pitch nil :font "Fira Sans"))

(use-package writeroom-mode
  :bind
  ("C-c w" . writeroom-mode))

;; doom-themes - colorschemes seen in DOOM Emacs
;; (use-package ef-themes
;;   ;; :config
;;   ;; (ef-themes-select 'ef-cyprus)
;;   :init
;;   (setq ef-themes-headings
;; 	'((1 . (variable-pitch 1.75))
;;           (2 . (variable-pitch 1.5))
;; 	  (3 . (variable-pitch 1.25))
;; 	  (4 . (variable-pitch 1.1))
;; 	  (5 . (variable-pitch))
;; 	  (6 . (variable-pitch))
;; 	  (7 . (variable-pitch))
;; 	  (8 . (variable-pitch))
;; 	  (t . (variable-pitch 2.0)))
;; 	)
  
;;   (setq ef-themes-mixed-fonts t
;; 	ef-themes-variable-pitch-ui nil)
  
;;   (setq ef-themes-common-palette-overrides
;; 	'((border-mode-line-active unspecified)
;;           (border-mode-line-inactive unspecified))))

;; (use-package doom-themes
;;   :init
;;   (load-theme 'doom-plain t))

(use-package modus-themes
  :config
  (load-theme 'modus-vivendi t)
  :init
  (setq modus-themes-headings
	'((1 . (variable-pitch 1.75))
          (2 . (variable-pitch 1.5))
	  (3 . (variable-pitch 1.25))
	  (4 . (variable-pitch 1.1))
	  (5 . (variable-pitch))
	  (6 . (variable-pitch))
	  (7 . (variable-pitch))
	  (8 . (variable-pitch))
	  (t . (variable-pitch 2.0)))
	)

  (setq modus-themes-common-palette-overrides
	'((border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)
	  ,@modus-themes-preset-overrides-faint))

  (setq modus-themes-mixed-fonts t
	modus-themes-org-blocks 'gray-background))

;; sudo-edit - edit files that need admin priviliges
(use-package sudo-edit
  :bind
  ("C-c C-r" . sudo-edit))

;; tree-sitter - advanced syntax highlighting
(use-package tree-sitter
  :hook
  (tree-sitter-after-on-hook . tree-sitter-hl-mode)
  :init
  (global-tree-sitter-mode))
(use-package tree-sitter-langs
  :after tree-sitter)

;; all-the-icons - unicode icons for Emacs, regardless of font
(use-package all-the-icons
  :if (display-graphic-p))

;; doom-modeline - fancy modeline from doom emacs
(use-package doom-modeline
  :init
  (setq size-indication-mode t
	column-number-mode t
        doom-modeline-height 30
        doom-modeline-hud nil
        doom-modeline-buffer-file-name-style 'auto
        doom-modeline-major-mode-icon nil
        doom-modeline-unicode-fallback nil
        doom-modeline-enable-word-count t
        doom-modeline-bar-width 3
        doom-modeline-indent-info t
	doom-modeline-minor-modes t
	doom-modeline-icon nil)
  (doom-modeline-mode 1))

;; dashboard - custom emacs startpage
(use-package dashboard
  :init
  (setq dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-banner-logo-title "Editor MACroS!"
        dashboard-startup-banner "~/.config/emacs/logo-nin.png"
        dashboard-center-content t)
  (dashboard-setup-startup-hook))

;; extra language support
;; (use-package haskell-mode)
;; (use-package lua-mode)
;; (use-package markdown-mode)
;; (use-package nix-mode)
;; Common Lisp
(use-package sly
  :bind
  ("C-c C-l" . sly))
;; Scheme and it's implementations
(use-package geiser)
(use-package geiser-guile)
;; YAML (alacritty)
(use-package yaml-mode)
