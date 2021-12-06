;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Alexander Neville"
      user-mail-address "alexander.neville@icloud.com")

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
(setq doom-font (font-spec :family "JetbrainsMono Nerd Font" :size 13 :weight 'regular)
      doom-variable-pitch-font (font-spec :family "Ubuntu" :size 15))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


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
;;
;; PERSONAL CONFIG


(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(use-package evil
  :config
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
)


(setq
 display-line-numbers-type nil
 ispell-program-name "aspell"
 ispell-local-dictionary "british-ise"
 org-directory "~/notes"
 auto-save-default nil
 make-backup-files nil
 mouse-wheel-scroll-amount '(3 ((shift) . 1)) ;; one line at a time
 mouse-wheel-progressive-speed nil ;; don't accelerate scrolling
 mouse-wheel-follow-mouse 't ;; scroll window under mouse
 ;; neo-theme 'icons
 ;; doom-fallback-buffer "*dashboard*"
 ;; doom-modeline-height 15
 doom-modeline-enable-word-count nil
 doom-modeline-buffer-encoding nil
 which-key-idle-delay 0.4
 confirm-kill-emacs nil
 blink-cursor-mode 1
)

(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)
(add-hook 'treemacs-mode-hook (lambda () (setq hl-line-mode 1)))
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)
(add-hook 'doom-first-buffer-hook #'blink-cursor-mode)

;;
;; ORG CONFIG
;;

(defun alex/org-mode-setup ()
  (font-lock-add-keywords 'org-mode
                            '(("^ *\\([-]\\) "
                               (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (org-indent-mode)
  (setq left-margin-width 10)
  (setq right-margin-width 10)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(setq org-directory "~/org/")
(after! org (setq org-hide-emphasis-markers t))
(after! org (setq org-insert-heading-respect-content nil))
(after! org (setq org-cycle-separator-lines 2))
(after! org (setq org-startup-folded t))

(add-hook! org-mode :append
           #'visual-line-mode
           #'org-appear-mode
           #'variable-pitch-mode)

(after! org

  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (custom-set-faces!
   '(org-document-title :height 1.3)
   '(org-level-1 :inherit outline-1 :weight bold :height 1.2)
   '(org-level-2 :inherit outline-2 :weight bold :height 1.15)
   '(org-level-3 :inherit outline-3 :weight semi-bold :height 1.1)
   '(org-level-4 :inherit outline-4 :weight semi-bold)
   '(org-level-5 :inherit outline-5 :weight semi-bold)
   '(org-level-6 :inherit outline-6 :weight semi-bold)
   '(org-level-7 :inherit outline-7 :weight semi-bold)
   '(org-level-8 :inherit outline-8 :weight semi-bold)
   ;; Ensure that anything that should be fixed-pitch in org buffers appears that way.
   '(org-block nil :foreground nil :inherit 'fixed-pitch)
   '(org-code nil   :inherit '(shadow fixed-pitch))
   '(org-table nil   :inherit '(shadow fixed-pitch))
   '(org-verbatim nil :inherit '(shadow fixed-pitch))
   '(org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
   '(org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
   '(org-checkbox nil :inherit 'fixed-pitch)))

(add-hook 'org-mode-hook #'alex/org-mode-setup)

(use-package mixed-pitch
   :hook (org-mode . mixed-pitch-mode))

(use-package org-bullets
 :after org
 :hook (org-mode . org-bullets-mode)
 :custom
 (org-bullets-bullet-list '( "●" "●" "●" "●" "●" "●")))
