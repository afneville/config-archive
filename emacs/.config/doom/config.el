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
(setq doom-font (font-spec :family "Jetbrains Mono Nerd Font" :size 14 :weight 'regular :height 1.0)
      ;; doom-variable-pitch-font (font-spec :family "CMU Typewriter Text" :size 16 :height 1.0))
      doom-variable-pitch-font (font-spec :family "Noto Sans" :size 16 :height 1.0))
      ;; doom-variable-pitch-font (font-spec :family "ETBembo" :size 16 :height 1.1))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dark+)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)


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


(remove-hook 'doom-first-buffer-hook #'global-hl-line-mode)
;;(add-hook 'treemacs-mode-hook (lambda () (setq hl-line-mode 1)))
(remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)
(add-hook 'doom-first-buffer-hook #'blink-cursor-mode)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(use-package evil
  :config
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
)

(map! :leader
      :desc "Insert line above"
      "i k"   #'+evil/insert-newline-above
      :desc "Insert line below"
      "i j"   #'+evil/insert-newline-below
      :desc "Preview Latex"
      "o l"   #'org-latex-preview)

;; ORG
 (setq org-latex-default-packages-alist
                 '(("hidelinks" "hyperref" nil)))
(setq org-image-actual-width (/ (display-pixel-width) 5))
(after! org (plist-put org-format-latex-options :scale 1.5))
(setq
 ispell-program-name "aspell"
 ispell-local-dictionary "british-ise"
 ispell-personal-dictionary "~/british-ise.pws"
)
(setq org-highlight-latex-and-related '(latex script entities))

(defun alex/org-mode-setup ()
  (font-lock-add-keywords 'org-mode
                            '(("^ *\\([-]\\) "
                               (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (org-indent-mode)
  ;; (setq left-margin-width 20)
  ;; (setq right-margin-width 10)
  ;; (variable-pitch-mode 1)
  (visual-line-mode 1))

(setq org-directory "~/org/")
(after! org (setq org-hide-emphasis-markers t))
(after! org (setq org-insert-heading-respect-content nil))
(after! org (setq org-cycle-separator-lines 2))
(after! org (setq org-startup-folded t))

(add-hook! org-mode :append
           #'visual-line-mode
           #'visual-fill-column-mode)
           ;; #'visual-fill-column-mode
           ;; #'variable-pitch-mode)

(after! org

  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (custom-set-faces!
   '(org-document-title :height 1.3)
   '(org-level-1 :inherit outline-1 :weight semi-bold :height 1.3)
   '(org-level-2 :inherit outline-2 :weight semi-bold :height 1.1)
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

;; (use-package mixed-pitch
;;   :hook (org-mode . mixed-pitch-mode)
;;   :config
;;   (setq mixed-pitch-set-height t)
;;   (set-face-attribute 'variable-pitch nil :height 1.0))

;; (use-package org-bullets
;;  :after org
;;  :hook (org-mode . org-bullets-mode)
;;  :custom
;;  (org-bullets-bullet-list '( "●" "●" "●" "●" "●" "●")))

(defun alex/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . alex/org-mode-visual-fill))
