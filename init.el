;; Auto generated stuff, see my config below
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(python-mode company-lsp ivy-rich eglot lsp-jedi elpy company-box company lsp-mode hydra evil-collection general which-key rainbow-delimiters doom-themes doom-modeline counsel ivy use-package evil)))
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
(setq initial-scratch-message ";; Hello again Alex.")
(setq inhibit-startup-echo-area-message "alex")
(set-fringe-mode 0)
;; enable replacing buffer in dired.
(put 'dired-find-alternate-file 'disabled nil)
;; stop autosave files being created
(setq auto-save-default nil)
(setq make-backup-files nil)

;; Font and theme settings
(set-face-attribute 'default nil :font "Source Code Pro" :height 130)
(load-theme 'doom-spacegrey t)
;; display line numbers.
(column-number-mode)
;;(global-display-line-numbers-mode t) ;; this will enable line numbers globaly.
(add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode t)))
;; disable line wrapping and and improve scrolling.
(setq-default truncate-lines t)
(setq scroll-conservatively 101)
;; Enable line wrapping in certain modes.
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (setq visual-line-mode t))))

;; =============================================================================================================================

;; This part of the configuration is for packages which are being added
;; Package sources:

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

;; Packages:
(use-package counsel)

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
;; Install the doom emacs themes modelines and icons :) 
;; use: M-x all-the-icons-install-fonts to make sure that the icons get installed.
(use-package all-the-icons)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))
(use-package doom-themes)
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.6))

;;used with the settings below, you can see more info when using M-x etc...
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; counsel is already installed, this is just configuration, see above.

(use-package counsel
  :bind (("M-x" .  counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil))

;; =============================================================================================================================
;; Create an accelerator key like doom emacs

(use-package general
  :after evil
  :config
  (general-create-definer alex/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (alex/leader-keys
   "f" '(:ignore t :which-key "files")
   "ff" '(counsel-find-file :which-key "quick-find"))
  (alex/leader-keys
    "a" '(:ignore t :which-key "actions")
    "ae" '(eval-buffer :which-key "eval-buffer"))
  (alex/leader-keys
    "b" '(:ignore t :which-key "buffers")
    "bk" '(kill-buffer :which-key "kill")
    "bs" '(counsel-switch-buffer :which-key "switch"))



  )

;; =============================================================================================================================
;; Evil Vim emulation :)

(require 'evil)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
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
;; This package is useful if you want to make a quick menu
(use-package hydra)

;; =============================================================================================================================
;; LSP language server setup
;(use-package lsp-ui)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))
(require 'lsp-mode)

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-j") #'company-select-next)
  (define-key company-active-map (kbd "C-k") #'company-select-previous))

(use-package company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode))
(use-package python-mode
  :ensure nil
  :hook (python-mode . lsp-deferred)
  :custom
  (python-shell-interpreter))
