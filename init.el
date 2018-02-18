(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t))
(package-initialize)

(set-face-attribute 'default nil :family "Consolas" :height 120)

(defun dw/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun dw/switch-project-hook ()
  (treemacs-projectile))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package nord-theme :ensure t)

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :config
  (which-key-setup-side-window-bottom))

(use-package avy :ensure t)

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode)
  (setq projectile-enable-caching t))

(use-package all-the-icons :ensure t)

(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    (use-package treemacs-evil :ensure t :demand t)
    (setq treemacs-change-root-without-asking t
          treemacs-follow-after-init          t
          treemacs-silent-filewatch           t
          treemacs-silent-refresh             t)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)))

(use-package treemacs-projectile
  :defer t
  :ensure t
  :config
  (setq treemacs-header-function #'treemacs-projectile-create-header))

(use-package hydra :ensure t)

(use-package cider-hydra :ensure t)

(use-package general :ensure t
  :config
  (progn
    (general-evil-setup t)
    (general-define-key
	:states '(normal visual insert emacs)
	:prefix "SPC"
	:non-normal-prefix "M-SPC"
	"SPC" '(counsel-M-x :which-key "M-x")
	"b" '(:ignore t :which-key "Buffer")
	"bk" '(dw/kill-this-buffer :which-key "Kill current buffer")
	"bb" '(ibuffer :which-key "List buffers")
	"f" '(:ignore t :which-key "File")
	"ff" 'treemacs-find-file
	"fo" 'treemacs-select-window
	"h" '(:ignore t :which-key "Help")
	"hb" '(describe-bindings :which-key "Describe bindings")
	"p" '(:ignore t :which-key "Project")
	"pp" '(projectile-switch-project :which-key "Switch project")
	"pb" '(projectile-ibuffer :which-key "Project buffers"))

    (general-define-key
	:states '(normal emacs)
	"j" 'evil-next-visual-line
	"k" 'evil-previous-visual-line)))

(add-hook 'projectile-after-switch-project-hook #'dw/switch-project-hook)

(use-package clojure-mode :ensure t)
(use-package cider :ensure t)

(use-package ivy :ensure t
  :init (ivy-mode 1)        ; enable ivy globally at startup
  :config
    (setq ivy-use-virtual-buffers t)   ; extend searching to bookmarks and …
    (setq ivy-height 20)               ; set height of the ivy window
    (setq ivy-count-format "(%d/%d) ") ; count format, from the ivy help page
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (cider-hydra clojure-mode evil-collection all-the-icons-dired ibuffer-sidebar treemacs nord-theme use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
