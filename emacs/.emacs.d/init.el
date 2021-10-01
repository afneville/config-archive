(defun alex/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'alex/display-startup-time)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-want-keybinding nil)
 '(org-agenda-files nil)
 '(org-startup-folded t)
 '(package-selected-packages
   '(org-bullets visual-fill-column visual-fill spacemacs-theme dired-single undo-tree evil-collection use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "RobotoMono Nerd Font" :height 130))))
 '(variable-pitch ((t (:family "ETBembo" :height 150)))))

(load-theme 'spacemacs-light t)
(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
;; Inhibit the splash screen and other default ui elements
(setq inhibit-startup-message t)
(setq initial-scratch-message "; Hi, welcome to emacs :)")
(setq inhibit-startup-echo-area-message "alex")
(set-fringe-mode 0)

(setq scroll-conservatively 101)
(setq scroll-margin 1)

(defun alex/font-faces ()
  (set-face-attribute 'default nil :font "RobotoMono Nerd Font" :height 130)
  (set-face-attribute 'fixed-pitch nil :font "RobotoMono Nerd Font" :height 130)
  (set-face-attribute 'variable-pitch nil :font "ETBembo" :weight 'regular :height 150))

(defun alex/daemon-setup (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (cursor-color . "dodger blue")
                             (horizontal-scroll-bars . nil))))

(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (setq visual-line-mode t))))

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

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(global-set-key (kbd "C-u") 'tab-to-tab-stop)

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
    "ae" '(eval-buffer :which-key "eval-buffer")
    "al" '(load-file :which-key "load-file"))
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
    "le" '(org-edit-latex-fragment :whichkey "edit src")
    "lp" '(org-latex-preview :whichkey "toggle preview"))
  (alex/leader-keys
    "o" '(:ignore t :which-key "org mode")
    "oi" '(org-toggle-inline-images :whichkey "toggle images")
    "or" '(org-mode-restart :which-key "reload"))
    ;"os" '(org-schedule :which-key "schedule")
    ;"od" '(org-deadline :which-key "deadline")
    ;"oa" '(org-agenda :which-key "agenda")
    ; "ob" '(org-babel-tangle :which-key "export blocks")
    ; "ol" '(org-store-link :which-key "store link")
    ; "oi" '(org-insert-last-stored-link :which-key "insert link")
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

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))
(use-package dired-single)

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
    (set-face-attribute (car face) nil :font "ETBembo" :weight 'regular :height 150))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-property-value nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-document-info-keyword nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))
         ; 'org-document-info-keyword
(require 'org)
(setq header-line-format " ")
(setq line-spacing 0.1)

(defun my-set-margins ()
  "Set margins in current buffer."
  (setq left-margin-width 24)
  (setq right-margin-width 24))

(add-hook 'text-mode-hook 'my-set-margins)
(setq org-cycle-separator-lines 2)
(setq org-startup-indented t
      ; org-bullets-bullet-list '(" ") ;; no bullets, needs org-bullets package
      ; org-ellipsis "  " ;; folding symbol
      org-ellipsis " … " ;; folding symbol
      org-pretty-entities t
      org-hide-emphasis-markers t
      ;; show actually italicized text instead of /italicized text/
      org-agenda-block-separator ""
      org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t)

(setq org-image-actual-width (/ (display-pixel-width) 3))
(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
(use-package org
  :hook (org-mode . alex/org-mode-setup)
  :config
  ;(setq org-ellipsis " ")
  ;;(setq org-ellipsis " ⤵")
  (setq org-indent-indentation-per-level 2)
  (setq org-hide-emphasis-markers t)
  (setq org-agenda-files '("~/agenda.org"))
  (alex/org-font-setup))

(use-package org-bullets
    :config
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))
(put 'dired-find-alternate-file 'disabled nil)
