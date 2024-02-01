(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(set-face-attribute 'default nil
                    :family "JetBrains Mono NL"
		    :height 180)
(set-face-attribute 'fixed-pitch nil
		    :font "JetBrains Mono NL"
		    :height 180)
(set-face-attribute 'variable-pitch nil
		    :font "JetBrains Mono NL"
		    :height 180)

(setq inhibit-startup-message t
      line-number-mode t
      size-indication-mode t
      column-number-mode t
      ring-bell-function 'ignore
      use-short-answers t
      native-compile-prune-cache t ; Emacs 29
      make-backup-files nil
      backup-inhibited nil ; Not sure if needed, given `make-backup-files'
      create-lockfiles nil)

(setq-default cursor-type 'bar)

;; (add-to-list 'default-frame-alist '(alpha-background . 90))

(setq major-mode-remap-alist
      '((c-mode  . c-ts-mode)
	(cpp-mode . cpp-ts-mode)
	(rust-mode . rust-ts-mode)))

;; Taken from Dominik Schlack's config:
;; https://gitlab.com/domsch1988/domacs-next

(defun my-modeline--major-mode-name ()
  "Return capitalized `major-mode' as a string."
  (capitalize (symbol-name major-mode)))

;; Taken from Dominik Schlack's config:
;; https://gitlab.com/domsch1988/domacs-next
;;
;; (defvar-local dom-line-major-mode
;;     '(:eval
;;       (list
;;        (propertize (my-modeline--major-mode-name) 'face 'bold)
;;        ))
;;   "Mode line construct to display the major mode.")

(defvar-local slam-line-major-mode
    '(:eval
      (list
       (propertize (my-modeline--major-mode-name) 'face 'normal)
       ))
  "Mode line construct to display the major mode.")

;; Taken from Dominik Schlack's config:
;; https://gitlab.com/domsch1988/domacs-next
;;
;; (defvar-local dom-line-buffer-name
;;     '(:eval
;;       (list
;;        " "
;;        (if (mode-line-window-selected-p)
;; 	   (propertize (format "%s" (buffer-name)) 'face 'bold)
;; 	 (propertize (format "%s" (buffer-name)) 'face 'shadow)))
;;       )
;;   "My Personal Modelines Buffername")

 (defvar-local slam-line-buffer-name
     '(:eval
	(list
	 " "
	 (if (mode-line-window-selected-p)
	     (propertize (format "%s" (buffer-name)) 'face 'bold)
	   (propertize (format "%s" (buffer-name)) 'face 'shadow)))
	)
   "My Personal Modelines Buffername")


(dolist (construct '(slam-line-major-mode
		     slam-line-buffer-name))
  (put construct 'risky-local-variable t))

(setq mode-line-compact nil)
(setq mode-line-right-align-edge 'right-margin)

;; (setq-default header-line-format
;; 		'(
;; 		  " "
		  
;; 		  )
;; 		)

(setq mode-line-position (list "L%l C%c"))

(setq-default mode-line-format
              '(
		"%e"
	        mode-line-front-space
	        "%*%+"
	        slam-line-buffer-name
		" "
		mode-line-position
	        mode-line-format-right-align
		(vc-mode vc-mode)
		" "
		slam-line-major-mode
		mode-line-misc-info
		))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; (global-visual-line-mode t)
(fringe-mode -1)
;; (display-time-mode t)

(global-set-key (kbd "C-c l") 'imenu)
(global-set-key (kbd "C-c t t") 'term)
(global-set-key (kbd "C-c t s") 'shell)
(global-set-key (kbd "C-c t e") 'eshell)

(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

(elpaca-wait)

(use-package org
  :init
  (setq org-src-tab-acts-natively t
	org-edit-src-content-indentation 0
	org-hide-emphasis-markers nil)
  (setq org-agenda-files '("~/.emacs.d/files/Weekly.org"
			   "~/.emacs.d/files/Todo.org"
			   "~/.emacs.d/files/Goals.org"
			   "~/.emacs.d/files/Ideas.org"))
  :config (add-to-list 'org-modules 'org-tempo)
  :bind (:map org-mode-map ("C-c i" . org-indent-mode))
  :hook (org-mode . auto-fill-mode))

(use-package org-roam
  :after org
  :init
  (setq org-roam-directory (file-truename "~/Documents/Org/Notes"))
  (org-roam-db-autosync-mode)
  :bind
  (("C-c n f" . org-roam-node-find)
   ("C-c n r" . org-roam-node-random)
   (:map org-mode-map
         (("C-c n i" . org-roam-node-insert)
          ("C-c n o" . org-id-get-create)
          ("C-c n t" . org-roam-tag-add)
          ("C-c n a" . org-roam-alias-add)
          ("C-c n l" . org-roam-buffer-toggle)))))

(use-package org-modern
  :hook (org-mode . org-modern-mode))

(use-package org-present
  :config
  (add-hook 'org-present-mode-hook
	    (lambda ()
	      (org-display-inline-images)
	      (org-present-hide-cursor)
	      (org-present-read-only)))
  (add-hook 'org-present-mode-quit-hook
	    (lambda ()
              (org-remove-inline-images)
              (org-present-show-cursor)
              (org-present-read-write))))

(use-package olivetti
  :defer t)

(use-package markdown-mode
  :defer t)

(use-package nix-mode
  :defer t)

(use-package seq
  :defer t)

(use-package modus-themes
  :init
  ;; (setq modus-themes-headings
  ;; 	'((1 . (variable-pitch 1.5))
  ;; 	  (2 . (variable-pitch 1.5))
  ;; 	  (3 . (variable-pitch 1.5))
  ;; 	  (4 . (variable-pitch 1.5))
  ;; 	  (5 . (variable-pitch 1.5))
  ;; 	  (6 . (variable-pitch 1.5))
  ;; 	  (7 . (variable-pitch 1.5))
  ;; 	  (8 . (variable-pitch 1.5))
  ;; 	  (t . (variable-pitch 1.5))))
  (setq modus-themes-mixed-fonts t
	modus-themes-variable-pitch-ui nil
	modus-themes-org-blocks 'gray-background)
  ;; (setq modus-themes-common-palette-overrides
  ;; 	'((border-mode-line-active unspecified)
  ;;         (border-mode-line-inactive unspecified)))
  :config
  (modus-themes-select 'modus-vivendi))
  ;; (run-at-time "08:00" (* 24 60 60) (lambda () (modus-themes-select 'modus-operandi)))
  ;; (run-at-time "20:00" (* 24 60 60) (lambda () (modus-themes-select 'modus-vivendi))))

;; (use-package modus-themes
;;   :defer t)

;; (use-package ef-themes
;;   :init
;;   ;; (setq ef-themes-headings
;;   ;; 	'((1 . (variable-pitch 1.5))
;;   ;; 	  (2 . (variable-pitch 1.5))
;;   ;; 	  (3 . (variable-pitch 1.5))
;;   ;; 	  (4 . (variable-pitch 1.5))
;;   ;; 	  (5 . (variable-pitch 1.5))
;;   ;; 	  (6 . (variable-pitch 1.5))
;;   ;; 	  (7 . (variable-pitch 1.5))
;;   ;; 	  (8 . (variable-pitch 1.5))
;;   ;; 	  (t . (variable-pitch 1.5))))
;;   (setq ef-themes-mixed-fonts t
;; 	ef-themes-variable-pitch-ui nil)
;;   :config
;;   (ef-themes-select-dark 'ef-maris-dark))
;;   ;; (run-at-time "08:00" (* 24 60 60) (lambda () (ef-themes-select-light 'ef-light)))
;;   ;; (run-at-time "20:00" (* 24 60 60) (lambda () (ef-themes-select-dark 'ef-dark))))

(use-package ef-themes
  :defer t)

(use-package dashboard
  :init
  (setq dashboard-set-heading-icons nil
        dashboard-set-file-icons nil
        dashboard-banner-logo-title "Bleep bloop!"
        dashboard-startup-banner 2
        dashboard-center-content t
	dashboard-filter-agenda-entry 'dashboard-no-filter-agenda
	dashboard-set-footer nil)
  (setq dashboard-items '((recents  . 8)
                        (bookmarks . 8)
                        (agenda . 12)))
  (setq dashboard-item-names '(("Agenda for the coming week:" . "Agenda:")))
  :config (dashboard-setup-startup-hook))

(use-package vertico
  :config (vertico-mode))

(use-package marginalia
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init (marginalia-mode))

(use-package keycast
  :bind ("C-c k" . keycast-mode-line-mode))

(use-package elfeed
  :bind ("C-x w" . elfeed))

(use-package elfeed-org
  :after elfeed
  :init (setq rmh-elfeed-org-files (list "~/.emacs.d/files/rss.org"))
  :config
  (elfeed-org))

(use-package spacious-padding
  :bind ("C-c s p" . spacious-padding-mode))
