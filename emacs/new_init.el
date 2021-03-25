(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(require 'package)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
			 ("elpa" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
 (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Auto generated stuff, see my config below
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-box-doc-enable nil)
 '(company-box-scrollbar nil)
 '(evil-want-keybinding nil)
 '(lsp-enable-snippet nil)
 '(org-directory "~/code
/org")
 '(package-selected-packages
   '(toc-org evil-org yasnippet company-tabnine undo-tree nlinum lua-mode typescript-mode web-mode json-mode exec-path-from-shell all-the-icons-dired dired-single evil-magit magit visual-fill-column org-bullets org-mode yasnippet-snippets treemacs-all-the-icons treemacs-projectile lsp-treemacs projectile treemacs-evil python-mode company-lsp ivy-rich eglot lsp-jedi elpy company-box company lsp-mode hydra evil-collection general which-key rainbow-delimiters doom-themes doom-modeline counsel ivy use-package evil))
 '(projectile-mode t nil (projectile)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "Roboto Mono" :height 130))))
 '(org-ellipsis ((t (:foreground "dark gray" :underline nil))))
 '(variable-pitch ((t (:family "Roboto" :height 150)))))

(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
;; Inhibit the splash screen and other default ui elements
(setq inhibit-startup-message t)
(setq initial-scratch-message "; Hi, welcome to emacs :)")
(setq inhibit-startup-echo-area-message "alex")
(set-fringe-mode 0)
;; enable replacing buffer in dired.
(put 'dired-find-alternate-file 'disabled nil)
;; disable question about following symlinks
(setq vc-follow-symlinks nil)
;; stop autosave files being created
(setq auto-save-default nil)
(setq make-backup-files nil)

(use-package term
  :config
  (setq explicit-shell-file-name "bash"))

(setq scroll-conservatively 101)

(set-face-attribute 'default nil :font "Roboto Mono" :height 130) ;

(use-package nlinum
  :config
  (setq nlinum-format "%3d "))

(add-hook 'prog-mode-hook (lambda () (nlinum-mode t)))

(setq-default truncate-lines t)
(set-display-table-slot standard-display-table 'truncation 32)

(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (setq visual-line-mode t))))

(use-package all-the-icons)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 30)))
(use-package doom-themes)
(load-theme 'doom-one t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package counsel
  :bind (("M-x" .  counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.6))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package general
  :after evil
  :config
  (general-create-definer alex/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (alex/leader-keys
   "f" '(:ignore t :which-key "files")
   "ff" '(counsel-find-file :which-key "quick-find")
   "ft" '(treemacs :which-key "toggle treemacs")
   "fs" '(lsp-treemacs-symbols :which-key "toggle lsp symbols")
   "fp" '(projectile-switch-project :which-key "open project")
   "fd" '(dired-jump :which-key "dired-mode"))
  (alex/leader-keys
    "a" '(:ignore t :which-key "actions")
    "at" '(counsel-load-theme :which-key "load-theme")
    "ae" '(eval-buffer :which-key "eval-buffer"))
  (alex/leader-keys
    "b" '(:ignore t :which-key "buffers")
    "bk" '(kill-this-buffer :which-key "kill this buffer")
    "bK" '(kill-buffer :which-key "kill any buffer")
    "bi" '(ibuffer :which-key "ibuffer")
    "bs" '(counsel-ibuffer :which-key "switch"))
  (alex/leader-keys
    "w" '(:ignore t :which-key "windows")
    "wh" '(evil-window-left :which-key "focus left")
    "wj" '(evil-window-down :which-key "focus down")
    "wk" '(evil-window-up :which-key "focus up")
    "wl" '(evil-window-right :which-key "focus right")
    "wq" '(kill-buffer-and-window :which-key "kill")
    "wf" '(delete-other-windows :which-key "focus this"))
  (alex/leader-keys
    "c" '(:ignore t :which-key "clipboard")
    "cc" '(clipboard-kill-ring-save :which-key "copy")
    "ck" '(clipboard-kill-ring :which-key "cut")
    "cp" '(clipboard-yank :which-key "paste"))
  (alex/leader-keys
    "o" '(:ignore t :which-key "org mode")
    ;"os" '(org-schedule :which-key "schedule")
    ;"od" '(org-deadline :which-key "deadline")
    ;"oa" '(org-agenda :which-key "agenda")
    "ob" '(org-babel-tangle :which-key "export blocks")
    "ol" '(org-store-link :which-key "store link")
    "oi" '(org-insert-last-stored-link :which-key "insert link")
    "or" '(org-mode-restart :which-key "reload"))
  (alex/leader-keys
    "s" '(swiper :which-key "search this file")
    "m" '(magit-status :which-key "magit")
    "j" '(counsel-ibuffer :which-key "switch buffer")
    "k" '(counsel-buffer :which-key "switch buffer"))
  (alex/leader-keys
    "t" '(term :which-key "term"))
  (alex/leader-keys
    "m" '(magit-status :which-key "magit"))
)
;; This package is useful if you want to make a quick menu
(use-package hydra)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(global-set-key (kbd "C-u") 'tab-to-tab-stop)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(require 'evil)
;;(setq x-select-enable-clipboard nil)
;;(setq interprogram-cut-function nil)
;;(setq interprogram-paste-function nil)
(setq save-interprogram-paste-before-kill t)
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-fine-undo 'fine)  
  ;(setq evil-want-C-u-scroll t); Use this option if you want C-u to scroll. I do not.
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-tree)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "C-j") 'counsel-ibuffer)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-set-initial-state 'term-mode 'normal))

(use-package evil-collection
  :after evil
  :ensure t
  :custom
  (evil-collection-want-unimpaired-p t)
  (evil-collection-company-use-tng t)
  (evil-collection-calendar-want-org-bindings t)
  :config
  (evil-collection-init))

(use-package undo-tree
  :after evil
  :config
  (global-undo-tree-mode))

(defun alex/evil-write ()
    (interactive)  
    (save-buffer)
    (kill-this-buffer))
(evil-ex-define-cmd "wq" 'alex/evil-write)
(evil-ex-define-cmd "q" 'kill-this-buffer)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/code")
    (setq projectile-project-search-path '("~/code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))
(use-package dired-single)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(defun alex/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1) ;; If you want fancy variable width fonts.
  (visual-line-mode 1))

(defun alex/org-font-setup ()
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Roboto" :weight 'regular :height 150))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(require 'org)

(setq org-cycle-separator-lines 2)

(use-package org
  :hook (org-mode . alex/org-mode-setup)
  :config
  (setq org-ellipsis " ")
  ;;(setq org-ellipsis " ⤵")
  ;;(setq org-ellipsis " ")
  (setq org-indent-indentation-per-level 2)
  (setq org-hide-emphasis-markers t)
  (setq org-agenda-files '("~/code/org/agenda.org"))
  (alex/org-font-setup))

(use-package toc-org)
(add-hook 'org-mode-hook 'toc-org-mode)

(with-eval-after-load 'org
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python")))

;; This package allows to define custom bullet points like doom emacs.
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun alex/org-mode-visual-fill ()
  (setq visual-fill-column-width 180
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . alex/org-mode-visual-fill))

(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-ui-doc-mode 0)
(setq lsp-diagnostics-provider :none)
(setq lsp-signature-auto-activate nil)
(setq lsp-signature-render-documentation nil)
(setq lsp-modeline-code-actions-enable nil)
(setq lsp-modeline-diagnostics-enable nil)
(setq lsp-log-io nil)
(setq lsp-restart 'auto-restart)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (web-mode . lsp-deferred)
  :config
  (lsp-enable-which-key-integration t))
(require 'lsp-mode)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :config
  (setq company-selection-wrap-around t)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  ; this line is for tab and go completion
  (company-tng-configure-default))
(with-eval-after-load 'company
  ; also use my prefered keys for selection
  (define-key company-active-map (kbd "C-j") #'company-select-next)
  (define-key company-active-map (kbd "C-k") #'company-select-previous))

(use-package company-box ; This package adds some icons in company mode.
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

;(use-package company-tabnine :ensure t)
;(require 'company-tabnine)
;(add-to-list 'company-backends #'company-tabnine)
;(add-hook 'prog-mode-hook (lambda () (company-mode t)))
;;(add-hook 'prog-mode-hook (lambda () (company-tabnine t)))

(use-package lsp-jedi
  :ensure t)

(add-hook 'python-mode-hook 'lsp-deferred)
(add-hook 'c++-mode-hook 'lsp-deferred)
(add-hook 'c-mode-hook 'lsp-deferred)

(use-package web-mode
  :mode "\\.js\\'"
  :hook (web-mode . lsp-deferred))

; lua mode for configuring awesome window manager
(use-package lua-mode
  :hook (lua-mode-hook . lua-mode))

(use-package lsp-treemacs
  :after lsp)
(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)
(use-package treemacs-all-the-icons
  :after treemacs)
(use-package treemacs-evil
  :after treemacs)

(add-hook 'treemacs-mode-hook (lambda () (treemacs-load-theme "all-the-icons")))
