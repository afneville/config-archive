;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq

 doom-font (font-spec :family "Roboto Mono" :weight 'regular :size 16 :height 1.0)
 doom-variable-pitch-font (font-spec :family "Robto" :size 18)
 doom-theme 'doom-nord

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.

 display-line-numbers-type nil

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.

 user-full-name "Alexander Neville"
 user-mail-address "alexander.neville@icloud.com"

)



(setq

 neo-theme 'icons
 ispell-program-name "aspell"
 ispell-local-dictionary "british-ise"
 org-directory "~/notes"

 auto-save-default nil
 make-backup-files nil

 mouse-wheel-scroll-amount '(3 ((shift) . 1)) ;; one line at a time
 mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
 mouse-wheel-follow-mouse 't ;; scroll window under mouse
 doom-modeline-height 35
 doom-modeline-enable-word-count nil
 doom-modeline-buffer-encoding nil
 org-cycle-separator-lines 2
 org-startup-folded t
 which-key-idle-delay 0.4
 confirm-kill-emacs nil
 blink-cursor-mode 1

)

;; Disable line highlighting, except in treemacs buffers
(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)
(add-hook 'treemacs-mode-hook (lambda () (setq hl-line-mode 1)))
;; Remove bracket completion
;;(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

(add-hook 'doom-first-buffer-hook #'blink-cursor-mode)
;; Add some margins to text and prog mode buffers


;; (defun alex/set-up-text-mode ()
;;   (setq left-margin-width 10)
;;   (setq right-margin-width 10))

;; (defun alex/set-up-prog-mode ()
;;   (setq left-margin-width 2)
;;   (setq right-margin-width 2))

;; (add-hook 'text-mode-hook 'alex/set-up-text-mode)
;; (add-hook 'prog-mode-hook 'alex/set-up-prog-mode)

(custom-set-faces!
  '(font-lock-comment-face :slant italic))

;; Some keybindings i like
(map! :leader
      :desc "switch buffer"
      "b s" #'counsel-ibuffer
      :desc "toggle line highlighting (current buffer)"
      "t h" #'hl-line-mode
      :desc "toggle line highlighting"
      "t H" #'global-hl-line-mode
      :desc "toggle treemacs"
      "f t" #'treemacs
      :desc "Insert line above"
      "i k"   #'+evil/insert-newline-above
      :desc "Insert line below"
      "i j"   #'+evil/insert-newline-below)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(defun alex/org-mode-setup ()
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             ;; (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "."))))))
  (org-indent-mode)
  (setq left-margin-width 10)
  (setq right-margin-width 10)
  ;(variable-pitch-mode 1) ;; If you want fancy variable width fonts.
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . alex/org-mode-setup)
  :config
  (setq org-indent-indentation-per-level 2)
  (setq org-hide-emphasis-markers t)
  (setq org-agenda-files '("~/notes/agenda.org")))

(use-package org-bullets
 :after org
 :hook (org-mode . org-bullets-mode)
 :custom
 (org-bullets-bullet-list '( "●" "✿" "○" "●" "●" "●")))
(setq org-ellipsis "↴")
(use-package! mixed-pitch
   :hook (org-mode . mixed-pitch-mode)
   :config
   (setq mixed-pitch-set-heigth t)
   (set-face-attribute 'variable-pitch nil :height 130))

(after! org

  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  ;(custom-set-faces!
  ;  '(org-document-title :height 1.3)
  ;  '(org-level-1 :inherit outline-1 :weight semi-bold :height 1.3)
  ;  '(org-level-2 :inherit outline-2 :weight semi-bold :height 1.2)
  ;  '(org-level-3 :inherit outline-3 :weight semi-bold :height 1.1)
  ;  '(org-level-4 :inherit outline-4 :weight semi-bold)
  ;  '(org-level-5 :inherit outline-5 :weight semi-bold)
  ;  '(org-level-6 :inherit outline-6 :weight semi-bold)
  ;  '(org-level-7 :inherit outline-7 :weight semi-bold)
  ;  '(org-level-8 :inherit outline-8 :weight semi-bold)
  ;  ;; Ensure that anything that should be fixed-pitch in org buffers appears that way.
  ;  '(org-block nil :foreground nil :inherit 'fixed-pitch)
  ;  '(org-code nil   :inherit '(shadow fixed-pitch))
  ;  '(org-table nil   :inherit '(shadow fixed-pitch))
  ;  '(org-verbatim nil :inherit '(shadow fixed-pitch))
  ;  '(org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  ;  '(org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  ;  '(org-checkbox nil :inherit 'fixed-pitch)))
  )

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/code")
    (setq projectile-project-search-path '("~/code")))
  (setq projectile-switch-project-action #'projectile-dired))

(require 'lsp-mode)

(setq

 lsp-headerline-breadcrumb-enable nil lsp-ui-doc-mode 0
 lsp-diagnostics-provider :none
 lsp-signature-auto-activate nil
 lsp-signature-render-documentation nil
 lsp-modeline-code-actions-enable nil
 lsp-modeline-diagnostics-enable nil
 lsp-log-io nil
 lsp-restart 'auto-restart
 lsp-enable-snippet nil
 company-lsp-enable-snippet nil

)

;; (use-package lsp-mode
;;   :commands (lsp lsp-deferred)
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook
;;   (web-mode . lsp-deferred)
;;   :config
;;   (lsp-enable-which-key-integration t))

(use-package company
  :after lsp-mode
  :hook
  (lsp-mode . company-mode)
  ;; (lsp-mode . company-tng-mode)
  :config
  (setq company-selection-wrap-around t)
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  ;; this line is for tab and go completion
  (company-tng-configure-default))


(with-eval-after-load 'company
   ;; also use my prefered keys for selection
   (define-key company-active-map (kbd "C-j") #'company-select-next)
   (define-key company-active-map (kbd "C-k") #'company-select-previous))

;; (add-hook 'python-mode-hook 'lsp-deferred)
(add-hook 'html-mode-hook 'web-mode)

;(after! persp-mode
;  (setq persp-emacsclient-init-frame-behaviour-override "main"))
