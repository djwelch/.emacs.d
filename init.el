(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq delete-old-versions -1 )		; delete excess backup versions silently
(setq version-control t )		; use version control
(setq vc-make-backup-files t )		; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file
(setq vc-follow-symlinks t )				       ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
(setq inhibit-startup-screen t )	; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore )	; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8 )	; use utf-8 by default
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
(setq default-fill-column 80)		; toggle wrapping text at the 80th character
(setq initial-scratch-message "Welcome in Emacs") ; print a default message in the empty scratch buffer opened at startup
(setq ediff-diff-program "C:/Progam Files/Git/usr/bin/diff.exe")

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t))
(package-initialize)

(set-face-attribute 'default nil :family "Consolas" :height 120)

(defun dw/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun dw/switch-project-hook ()
  (treemacs-projectile))

(defun dw/enable-olivetti-mode ()
  (olivetti-mode 1))

(defun dw/clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1))

(defun dw/hugo-server (&optional arg)
  (interactive "P")
  (let* ((default-directory (concat (expand-file-name hugo-base-dir) "/"))
         (proc (get-buffer-process hugo-buffer)))
    (if (and proc (process-live-p proc))
        (progn (interrupt-process proc)
               (message "Stopped Hugo server"))
      (start-process "hugo" hugo-buffer "hugo" "server" "--buildDrafts" "--watch" "-d" "dev")
      (message "Started Hugo server")
      (unless arg
        (browse-url "http://localhost:1313/")))))


(use-package evil :ensure t :init (setq evil-want-integration nil)
  :config
  (progn
    (setq evil-emacs-state-modes nil)
    (setq evil-insert-state-modes nil)
    (setq evil-motion-state-modes nil)
    (setq evil-normal-state-modes
	  (append evil-emacs-state-modes evil-insert-state-modes evil-normal-state-modes evil-motion-state-modes))
    (evil-mode 1)))
(use-package which-key :ensure t :init (which-key-mode) :config (which-key-setup-side-window-bottom))
(use-package nord-theme :ensure t)
(use-package ivy :ensure t :init (ivy-mode 1))
(use-package counsel :ensure t)
(use-package hydra :ensure t)
(use-package projectile :ensure t
  :config
  (progn
    (projectile-global-mode)
    (setq projectile-enable-caching t)
    (projectile-register-project-type 'hugo '("config.toml")
				      :compile "hugo"
				      :run "hugo server --watch -d dev")
    (projectile-register-project-type 'npm '("package.json")
				      :compile "npm install"
				      :test "npm test"
				      :run "npm start"
				      :test-suffix ".spec")))
(use-package treemacs :ensure t :defer t
  :config
  (progn
    (use-package treemacs-evil :ensure t :demand t)
    (setq treemacs-change-root-without-asking t
	  treemacs-follow-after-init          t
	  treemacs-silent-filewatch           t
	  treemacs-silent-refresh             t)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)))
(use-package treemacs-projectile :ensure t :defer t)
(use-package fill-column-indicator
  :ensure t
  :config
  (progn
    (add-hook 'clojure-mode-hook 'fci-mode)))
(use-package clojure-mode :ensure t)
(use-package cider :ensure t)
(use-package cider-hydra :ensure t)
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
;;(use-package paredit
;;  :ensure t
;;  :init
;;  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
;;  (add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
;;  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
;;  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
;;  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
;;  (add-hook 'ielm-mode-hook 'enable-paredit-mode)
;;  (add-hook 'json-mode-hook 'enable-paredit-mode))
(use-package company
  :ensure t
  :init (global-company-mode))
(use-package clj-refactor
  :ensure t
  :init
  (add-hook 'clojure-mode-hook 'dw/clojure-mode-hook))
(use-package olivetti :ensure t)
(use-package evil-collection :after evil :ensure t :config (evil-collection-init)) 
	  
(defhydra hydra-ibuffer-main (:color pink :hint nil)
  "
 ^Navigation^ | ^Mark^        | ^Actions^        | ^View^
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
  _k_:    ʌ   | _m_: mark     | _D_: delete      | _g_: refresh
 _RET_: visit | _u_: unmark   | _S_: save        | _s_: sort
  _j_:    v   | _*_: specific | _a_: all actions | _/_: filter
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
"
  ("j" ibuffer-forward-line)
  ("RET" ibuffer-visit-buffer :color blue)
  ("k" ibuffer-backward-line)

  ("m" ibuffer-mark-forward)
  ("u" ibuffer-unmark-forward)
  ("*" hydra-ibuffer-mark/body :color blue)

  ("D" ibuffer-do-delete)
  ("S" ibuffer-do-save)
  ("a" hydra-ibuffer-action/body :color blue)

  ("g" ibuffer-update)
  ("s" hydra-ibuffer-sort/body :color blue)
  ("/" hydra-ibuffer-filter/body :color blue)

  ("o" ibuffer-visit-buffer-other-window "other window" :color blue)
  ("q" quit-window "quit ibuffer" :color blue)
  ("." nil "toggle hydra" :color blue))

(defhydra hydra-ibuffer-mark (:color teal :columns 5
                              :after-exit (hydra-ibuffer-main/body))
  "Mark"
  ("*" ibuffer-unmark-all "unmark all")
  ("M" ibuffer-mark-by-mode "mode")
  ("m" ibuffer-mark-modified-buffers "modified")
  ("u" ibuffer-mark-unsaved-buffers "unsaved")
  ("s" ibuffer-mark-special-buffers "special")
  ("r" ibuffer-mark-read-only-buffers "read-only")
  ("/" ibuffer-mark-dired-buffers "dired")
  ("e" ibuffer-mark-dissociated-buffers "dissociated")
  ("h" ibuffer-mark-help-buffers "help")
  ("z" ibuffer-mark-compressed-file-buffers "compressed")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-action (:color teal :columns 4
                                :after-exit
                                (if (eq major-mode 'ibuffer-mode)
                                    (hydra-ibuffer-main/body)))
  "Action"
  ("A" ibuffer-do-view "view")
  ("E" ibuffer-do-eval "eval")
  ("F" ibuffer-do-shell-command-file "shell-command-file")
  ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
  ("H" ibuffer-do-view-other-frame "view-other-frame")
  ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
  ("M" ibuffer-do-toggle-modified "toggle-modified")
  ("O" ibuffer-do-occur "occur")
  ("P" ibuffer-do-print "print")
  ("Q" ibuffer-do-query-replace "query-replace")
  ("R" ibuffer-do-rename-uniquely "rename-uniquely")
  ("T" ibuffer-do-toggle-read-only "toggle-read-only")
  ("U" ibuffer-do-replace-regexp "replace-regexp")
  ("V" ibuffer-do-revert "revert")
  ("W" ibuffer-do-view-and-eval "view-and-eval")
  ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
  ("b" nil "back"))

(defhydra hydra-ibuffer-sort (:color amaranth :columns 3)
  "Sort"
  ("i" ibuffer-invert-sorting "invert")
  ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
  ("v" ibuffer-do-sort-by-recency "recently used")
  ("s" ibuffer-do-sort-by-size "size")
  ("f" ibuffer-do-sort-by-filename/process "filename")
  ("m" ibuffer-do-sort-by-major-mode "mode")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-filter (:color amaranth :columns 4)
  "Filter"
  ("m" ibuffer-filter-by-used-mode "mode")
  ("M" ibuffer-filter-by-derived-mode "derived mode")
  ("n" ibuffer-filter-by-name "name")
  ("c" ibuffer-filter-by-content "content")
  ("e" ibuffer-filter-by-predicate "predicate")
  ("f" ibuffer-filter-by-filename "filename")
  (">" ibuffer-filter-by-size-gt "size")
  ("<" ibuffer-filter-by-size-lt "size")
  ("/" ibuffer-filter-disable "disable")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-window ()
   "
Movement^^        ^Split^         ^Switch^		^Resize^
----------------------------------------------------------------
_h_ ←       	_v_ertical    	_b_uffer		_q_ X←
_j_ ↓        	_x_ horizontal	_f_ind files	_w_ X↓
_k_ ↑        	_z_ undo      	_a_ce 1		_e_ X↑
_l_ →        	_Z_ reset      	_s_wap		_r_ X→
_F_ollow		_D_lt Other   	_S_ave		max_i_mize
_SPC_ cancel	_o_nly this   	_d_elete	
"
   ("h" windmove-left )
   ("j" windmove-down )
   ("k" windmove-up )
   ("l" windmove-right )
   ("q" hydra-move-splitter-left)
   ("w" hydra-move-splitter-down)
   ("e" hydra-move-splitter-up)
   ("r" hydra-move-splitter-right)
   ("b" helm-mini)
   ("f" helm-find-files)
   ("F" follow-mode)
   ("a" (lambda ()
          (interactive)
          (ace-window 1)
          (add-hook 'ace-window-end-once-hook
                    'hydra-window/body))
       )
   ("v" (lambda ()
          (interactive)
          (split-window-right)
          (windmove-right))
       )
   ("x" (lambda ()
          (interactive)
          (split-window-below)
          (windmove-down))
       )
   ("s" (lambda ()
          (interactive)
          (ace-window 4)
          (add-hook 'ace-window-end-once-hook
                    'hydra-window/body)))
   ("S" save-buffer)
   ("d" delete-window)
   ("D" (lambda ()
          (interactive)
          (ace-window 16)
          (add-hook 'ace-window-end-once-hook
                    'hydra-window/body))
       )
   ("o" delete-other-windows)
   ("i" ace-maximize-window)
   ("z" (progn
          (winner-undo)
          (setq this-command 'winner-undo))
   )
   ("Z" winner-redo)
   ("SPC" nil)
   )

(use-package general :ensure t
  :config
  (progn
    (general-evil-setup)
    (general-define-key
        :states '(normal visual insert emacs)
	:prefix "SPC"
	:non-normal-prefix "M-SPC"
	"SPC" '(counsel-M-x :which-key "M-x")
	"p" '(:ignore t :which-key "Project")
	"ps" '(projectile-switch-project :which-key "Switch")
	"pt" '(treemacs-projectile  :which-key "Tree")
	"b" '(:ignore t :which-key "Buffer")
	"bk" '(dw/kill-this-buffer :which-key "Kill")
	"bb" '(ibuffer :which-key "List")
	"w" '(hydra-window/body :which-key "Window"))
    (general-define-key
     :states '(normal visual emacs)
     :prefix "SPC"
     :keymaps 'clojure-mode-map
     "m" '(:ignore t :which-key "Clojure")
     "m'" '(cider-jack-in :which-key "Jack-in")
     "mR" '(hydra-cljr-help-menu/body :which-key "Refactor")
     "md" '(cider-hydra-doc/body :which-key "Doc")
     "mr" '(cider-hydra-repl/body :which-key "Repl")
     "mt" '(cider-hydra-test/body :which-key "Test")
     "me" '(cider-hydra-eval/body :which-key "Eval"))))

(add-hook 'ibuffer-hook #'hydra-ibuffer-main/body)
(add-hook 'markdown-mode-hook 'dw/enable-olivetti-mode)

(setq-default fill-column 80)

;;;; example of setting env var named “path”, by appending a new path to existing path
;;(setenv "PATH"
;;  (concat
;;   "C:/Program Files/Git/usr/bin" ";"
;;   (getenv "PATH")))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (cider-hydra clojure-mode evil-collection all-the-icons-dired ibuffer-sidebar treemacs nord-theme use-package))))

