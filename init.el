;; Auto generated stuff, see my config below
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-box-doc-enable nil)
 '(evil-want-keybinding nil)
 '(lsp-enable-snippet nil)
 '(package-selected-packages
   '(nlinum lua-mode typescript-mode web-mode json-mode exec-path-from-shell all-the-icons-dired dired-single evil-magit magit visual-fill-column org-bullets org-mode yasnippet-snippets treemacs-all-the-icons treemacs-projectile lsp-treemacs projectile treemacs-evil python-mode company-lsp ivy-rich eglot lsp-jedi elpy company-box company lsp-mode hydra evil-collection general which-key rainbow-delimiters doom-themes doom-modeline counsel ivy use-package evil))
 '(projectile-mode t nil (projectile)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; =============================================================================================================================
;; This section is all about stopping the silly default behaviour of emacs.
;;(server-start) ;; needed for daemon mode
;; disable gui elements.

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
;; Better term behaviour

(use-package term
  :config
  (setq explicit-shell-file-name "bash"))
;; Font and theme settings
(set-face-attribute 'default nil :font "Roboto Mono" :height 130)
(load-theme 'doom-one t)

;; display line numbers, also using nlinum in order to control width.
(use-package nlinum
  :config
  (setq nlinum-format "%3d "))
;;(column-number-mode)
;;(global-display-line-numbers-mode t) ;; this will enable line numbers globaly.
(add-hook 'prog-mode-hook (lambda () (nlinum-mode t)))

;; disable line wrapping and and improve scrolling.
(setq-default truncate-lines t)
(setq scroll-conservatively 101)
;; Enable line wrapping in certain modes.
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (setq visual-line-mode t))))

;; =============================================================================================================================
;;  Install doom emacs themes and modelines ===================================================================================
;; use: M-x all-the-icons-install-fonts to make sure that the icons get installed.

(use-package all-the-icons)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))
(use-package doom-themes)
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; =============================================================================================================================
;; Package sources: ===============================================================================

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

;; =============================================================================================================================
;; Counsel, Ivy and other general utilities. ====================================================================

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

;; use system shell path

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))


;; =============================================================================================================================
;; Create an accelerator key like doom emacs ===================================================================================

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
    "os" '(org-schedule :which-key "schedule")
    "od" '(org-deadline :which-key "deadline")
    "oa" '(org-agenda :which-key "agenda")
    "ol" '(org-agenda-list :which-key "agenda list")
    "or" '(org-mode-restart :which-key "reload"))
  (alex/leader-keys
    "s" '(swiper :which-key "search this file")
    "m" '(magit-status :which-key "magit")
    "j" '(next-buffer :which-key "next buffer")
    "k" '(previous-buffer :which-key "previous buffer"))

  (alex/leader-keys
    "t" '(term :which-key "term"))

  (alex/leader-keys
    "m" '(magit-status :which-key "magit"))
)
;; This package is useful if you want to make a quick menu
(use-package hydra)

;; General key bindings

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(global-set-key (kbd "C-u") 'tab-to-tab-stop)

;;(setq indent-line-function 'insert-tab)


;; =============================================================================================================================
;; Evil Vim emulation :) =======================================================================================================

(require 'evil)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;;(setq x-select-enable-clipboard nil)
(use-package evil
  :init
  (setq evil-want-integration t)
  ;;(setq evil-want-keybinding nil) ;; Should be set above.
  (setq evil-want-fine-undo t)  
  ;;(setq evil-want-C-u-scroll t)
  ;;:hook (evil-mode . alex/evil-hook)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (evil-set-initial-state 'term-mode 'emacs))

(use-package evil-collection
  :after evil
  :ensure t
  :custom
  (evil-collection-company-use-tng nil)
  (evil-collection-calendar-want-org-bindings t)
  :config
  (evil-collection-init))

;; =========================================================================================================================
;;  Set up projectile for managing my projects =============================================================================

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/vc_projects")
    (setq projectile-project-search-path '("~/vc_projects")))
  (setq projectile-switch-project-action #'projectile-dired))

;; =========================================================================================================================
;;  Use magit to interact with git projects =============================================================================

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; =========================================================================================================================
;;  Set up dires mode ======================================================================================================

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


;; =========================================================================================================================
;;  Set up Org mode ========================================================================================================

(defun alex/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 0) ;; If you want fancy variable width fonts.
  (visual-line-mode 1))

;; This section is copied from the internet. It sets up different font faces and uses a variable width font.
(defun alex/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Mononoki Nerd Font" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . alex/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-indent-indentation-per-level 2)
  (setq org-hide-emphasis-markers t)
  (setq org-agenda-files '("~/vc_projects/org/Agenda.org"))
  (alex/org-font-setup))
;; This package allows to define custom bullet points like doom emacs.
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))
;;(add-hook 'org-mode-hook 'org-indent-mode)

(defun alex/org-mode-visual-fill ()
  (setq visual-fill-column-width 180
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . alex/org-mode-visual-fill))
;; =========================================================================================================================
;; LSP language server setup ===============================================================================================

;; Disable certain lsp settings.

(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-ui-doc-mode 0)
(setq lsp-diagnostics-provider :none)
(setq lsp-signature-auto-activate nil)
(setq lsp-signature-render-documentation nil)
(setq lsp-modeline-code-actions-enable nil)
(setq lsp-modeline-diagnostics-enable nil)
(setq lsp-log-io nil)
(setq lsp-restart 'auto-restart)


;; Basic lsp congig

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (web-mode . lsp-deferred)
  :config
  (lsp-enable-which-key-integration t))
(require 'lsp-mode)

;; Set up company mode when lsp-mode is active

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
      ("<tab>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-j") #'company-select-next)
  (define-key company-active-map (kbd "C-k") #'company-select-previous))

(use-package company-box ;; This package adds some icons in company mode.
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

;; Yasnippets

(use-package yasnippet)
(use-package yasnippet-snippets)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

;; Treemacs file explorer
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

;; Json & Web setup

 (use-package json-mode)
 (use-package web-mode
   :mode
   ("\\.js\\'" . web-mode)
   ("\\.jsx\\'" . web-mode)
   ("\\.ts\\'" . web-mode)
   ("\\.tsx\\'" . web-mode)
   ("\\.html\\'" . web-mode)
   :commands web-mode)

;; lua mode for configuring awesome

(use-package lua-mode
  :hook (lua-mode-hook . lua-mode))


;; Put the above config into practice for python mode

;(use-package python-mode
 ; :ensure nil
  ;:hook (python-mode . lsp-deferred))
(add-hook 'python-mode-hook 'lsp-deferred)
;; C and C++
(add-hook 'c++-mode-hook 'lsp-deferred)
;; javascript

