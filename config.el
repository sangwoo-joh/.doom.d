;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Sangwoo Joh"
      user-mail-address "work.sangwoo.joh@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one-light)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
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
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Kernel settings
(defalias 'yes-or-no-p 'y-or-n-p)

(setq require-final-newline t)
(setq x-alt-keysym 'meta)
(setq tramp-default-method "sshx")
(setq password-cache-expiry nil)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(set-language-environment-input-method "Korean")

(set-language-environment "Korean")

(display-time)

(prefer-coding-system 'utf-8)

(set-default-coding-systems 'utf-8)

(set-terminal-coding-system 'utf-8)

(when window-system
  (global-unset-key (kbd "C-z")))

(after! switch-window
  (setq switch-window-shortcut-style 'qwerty))

;; key bindings - for macos
(when IS-MAC
  (defvar mac-option-modifier)
  (defvar mac-command-modifier)

  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta))

(setq doom-font (font-spec :family "Ubuntu Mono derivative Powerline" :size 18))

;;
;; kernel functions
;;
(defun kernel/select-next-window ()
  "SELECT NEXT WINDOW."
  (interactive)
  (select-window (next-window (selected-window))))

(defun kernel/select-previous-window ()
  "SELECT PREVIOUS WINDOW."
  (interactive)
  (select-window (previous-window (selected-window))))

(defun kernel/unfill-paragraph ()  ; by Stefan Monnier (foo at acm.org)
  "TAKE A MULTI-LINE PARAGRAPH AND MAKE IT INTO A SINGLE LINE OF TEXT."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))


;;
;; kernel key maps
;;
;; How to bind keys -- from doomemacs discourse (https://discourse.doomemacs.org/t/how-to-re-bind-keys/56)
;;
;; 1. Global keys
;; (map! "C-x C-r" #'git-gutter:revert-hunk
;;       "C-x C-b" #'ibuffer
;;       "C-x C-l" #'+lookup/file)
;; ;; or
;; (map! :prefix "C-x"
;;       "C-r" #'git-gutter:revert-hunk
;;       "C-b" #'ibuffer
;;       "C-l" #'+lookup/file)
;;
;; 2. Mode or buffer-local keys
;; (map! :after python
;;       :map python-mode-map
;;       "C-x C-r" #'python-shell-send-region)
;; ;; or
;; (after! python
;;   (map! :map python-mode-map "C-x C-r" #'python-shell-send-region))
;;
(map! ;; Global
      "<C-tab>" #'kernel/select-next-window
      "<C-S-tab>" #'kernel/select-previous-window
      "<C-iso-lefttab>" #'kernel/select-previous-window
      "C-x C-k" #'kill-this-buffer
      "C-x C-n" #'next-buffer
      "C-x C-p" #'previous-buffer
      "C-c ;" #'comment-region
      "C-c :" #'uncomment-region
      "M-Q" #'kernel/unfill-paragraph
      ;; multiple-cursors
      "C->" #'mc/mark-next-like-this
      "C-<" #'mc/mark-previous-like-this
      ;; centered-window
      "C-M-l" #'centered-window-mode
      ;; switch-window
      "C-x o" #'switch-window
      "C-x 1" #'switch-window-then-maximize
      "C-x 2" #'switch-window-then-split-below
      "C-x 3" #'switch-window-then-split-right
      "C-x 0" #'switch-window-then-delete
      "C-x 4 d" #'switch-window-then-dired
      "C-x 4 f" #'switch-window-then-find-file
      "C-x 4 0" #'switch-window-then-kill-buffer
      ;; fzf
      "C-x C-r" #'fzf
      ;; swiper
      "C-c C-a" #'swiper-thing-at-point
      ;; iedit
      "C-c C-e" #'iedit-mode
      ;; undo-tree
      "C-z" #'undo-tree-visualize)

(map! :after copilot
      :map copilot-completion-map
      "<tab>" #'copilot-accept-completion
      "TAB" #'copilot-accept-completion)

(after! eyebrowse
  (eyebrowse-mode t)
  (map! :map eyebrowse-mode-map
        "M-0" #'eyebrowse-switch-to-window-config-0
        "M-1" #'eyebrowse-switch-to-window-config-1
        "M-2" #'eyebrowse-switch-to-window-config-2
        "M-3" #'eyebrowse-switch-to-window-config-3
        "M-4" #'eyebrowse-switch-to-window-config-4
        "M-5" #'eyebrowse-switch-to-window-config-5
        "M-6" #'eyebrowse-switch-to-window-config-6
        "M-7" #'eyebrowse-switch-to-window-config-7
        "M-8" #'eyebrowse-switch-to-window-config-8
        "M-9" #'eyebrowse-switch-to-window-config-9))

;;
;; kernel hooks
;;
(add-hook! 'prog-mode-hook #'copilot-mode)
