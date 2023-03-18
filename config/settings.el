;; set font for emacs
(set-face-attribute 'default nil
                    :family "JetBrains Mono"
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
