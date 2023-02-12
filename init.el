;; set font for emacs
(set-face-attribute 'default nil
                    :family "Cascadia Mono"
                    :height 160
		    )

;; clean up gui
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-visual-line-mode t)
(display-time-mode 1)
(fringe-mode -1)

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
  ;; modus-themes only
  (add-to-list 'org-modules 'org-tempo)
  :hook
  (org-mode . variable-pitch-mode))
(use-package org-contrib
  :after
  org)
(use-package org-modern
  :after org
  :bind
  ("C-c m" . org-modern-mode)
  :hook
  (org-mode . org-modern-mode)
  :init
  (global-org-modern-mode))

;; mixed-pitch - allows for mixed font sizes in text files
(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode)
  :config
  (set-face-attribute 'fixed-pitch nil :font "Fira Mono")
  (set-face-attribute 'variable-pitch nil :font "Fira Sans")
  :init
  (add-hook 'mixed-pitch-mode-hook #'solaire-mode-reset))

;; writeroom-mode - provides a distraction-free writing experience
(use-package writeroom-mode
  :bind
  ("C-c w" . writeroom-mode))

;; modus-themes - Highly accessible themes, conforming with the highest
;; standard for colour contrast between background and foreground values
;; (WCAG AAA). They also are optimised for users with red-green colour 
;; deficiency.
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
          (border-mode-line-inactive unspecified)))

  (setq modus-themes-mixed-fonts t
	modus-themes-org-blocks 'gray-background))

;; sudo-edit - edit files that need admin priviliges
(use-package sudo-edit
  :bind
  ("C-c C-r" . sudo-edit))

;; tree-sitter - advanced syntax highlighting
(use-package tree-sitter
  :init
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (global-tree-sitter-mode))
(use-package tree-sitter-langs
  :after tree-sitter)

(use-package all-the-icons
  :if (display-graphic-p))

;; doom-modeline - fancy modeline from doom emacs
(use-package doom-modeline
  :init
  (setq size-indication-mode t
	column-number-mode t
        doom-modeline-height 40
        doom-modeline-hud nil
        doom-modeline-buffer-file-name-style 'auto
        doom-modeline-major-mode-icon nil
        doom-modeline-unicode-fallback t
        doom-modeline-enable-word-count t
        doom-modeline-bar-width 3
        doom-modeline-indent-info t
	doom-modeline-minor-modes t
	doom-modeline-icon nil)
  (doom-modeline-mode 1)
  :config
  ;; Define the custom doom-modeline
  (doom-modeline-def-modeline 'my-simple-line
    '(matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info minor-modes input-method buffer-encoding major-mode process vcs checker))
  
  ;; Set default mode-line
  (add-hook 'doom-modeline-mode-hook
            (lambda ()
              (doom-modeline-set-modeline 'my-simple-line 'default)))
  
  (doom-modeline-mode -1))

;; dashboard - custom emacs startpage
(use-package dashboard
  :init
  (setq dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-banner-logo-title "Editor MACroS!"
        dashboard-startup-banner "~/.config/emacs/logo-nin.png"
        dashboard-center-content t)
  (dashboard-setup-startup-hook))
