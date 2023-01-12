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
(setq doom-font (font-spec :family "Ubuntu Mono derivative Powerline" :size 18))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(set-language-environment-input-method "Korean")

(set-language-environment "Korean")

(set-fontset-font "fontset-default" 'hangul "D2Coding")

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

(elpy-enable)

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

(defun kernel/increment-number-at-point ()
  "Increase number at cursor."
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun kernel/decrement-number-at-point ()
  "Decrease number at cursor."
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1- (string-to-number (match-string 0))))))


(defun kernel/timestamp ()
  "PUT TIMESTAMP"
  (interactive)
  (insert (format-time-string "%Y-%02m-%02d %02H:%02M:%02S")))

(defun kernel/leetcode-problem-link (problem)
  "CREATE LEETCODE PROBLEM LINK"
  (interactive)
  (format "https://leetcode.com/problems/%s/" problem))

(defun kernel/md/save-with-timestamp ()
  "Save markdown with timestamp"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* ((section-header "^-\\{3,\\}$")
           (section-last-update-key "^last_update[ ]*:[ ]*")
           (section-beginning (re-search-forward section-header))
           (section-end (re-search-forward section-header))
           (section-string (buffer-substring-no-properties section-beginning section-end)))
      (goto-char section-beginning)
      (if (string-match-p section-last-update-key section-string)
          ;; if there is already last_update section, then kill the outdated date.
          (progn
            (re-search-forward section-last-update-key)
            (kill-line))
        ;; otherwise, insert new last_update section
        (goto-char section-end)
        (forward-line -1)
        (goto-char (line-end-position))
        (electric-newline-and-maybe-indent)
        (insert "last_update: "))
      (kernel/timestamp))))

(defun kernel/md/add-save-with-timestamp-hook ()
  "Add markdown save with timestamp hook"
  (interactive)
  (add-hook 'before-save-hook #'kernel/md/save-with-timestamp nil 'local))

(defun kernel/md/delete-save-with-timestamp-hook ()
  "Delete markdown save with timestamp hook"
  (interactive)
  (remove-hook 'before-save-hook #'kernel/md/save-with-timestamp 'local))

(defun kernel/org/save-with-timestamp ()
  "Save org file with timestamp."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* ((section-last-update-key-re "^#\\+last_update[ ]*:[ ]*")
           (section-last-update-key "#+last_update: "))
      (if (re-search-forward section-last-update-key-re nil t)
          (kill-line)
        ;; else
        (message "No last_update key found.")
        ;; insert last_update into the first line
        (goto-char (point-min))
        (electric-newline-and-maybe-indent)
        (forward-line -1)
        (insert section-last-update-key))
      (kernel/timestamp))))

(defun kernel/org/add-save-with-timestamp-hook ()
  "Add org timestamp hook."
  (interactive)
  (add-hook 'before-save-hook #'kernel/org/save-with-timestamp nil 'local))

(defun kernel/org/delete-save-with-timestamp-hook ()
  "Delete org timestamp hook."
  (interactive)
  (remove-hook 'before-save-hook #'kernel/org/save-with-timestamp 'local))

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
      "C-c +" #'kernel/increment-number-at-point
      "C-c -" #'kernel/decrement-number-at-point
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
      ;;
      ;; iedit
      ;; After iedit-mode is on (by C-C C-e),
      ;; <tab>, S-<tab>, M->, M-<: navigation
      ;; M-;: toggle
      ;; M-N: numbering
      ;; M-R: replace
      ;; M-D: delete
      ;;
      "C-c C-e" #'iedit-mode
      ;; undo-tree
      "C-z" #'undo-tree-visualize
      ;; workspace
      "M-1" #'+workspace/switch-to-0
      "M-2" #'+workspace/switch-to-1
      "M-3" #'+workspace/switch-to-2
      "M-4" #'+workspace/switch-to-3
      "M-5" #'+workspace/switch-to-4
      "M-6" #'+workspace/switch-to-5
      "M-7" #'+workspace/switch-to-6
      "M-8" #'+workspace/switch-to-7
      "M-9" #'+workspace/switch-to-8)


(map! :after copilot
      :map copilot-completion-map
      "<tab>" #'copilot-accept-completion
      "TAB" #'copilot-accept-completion)

;;
;; pipenv
;; C-c C-p: enter pipenv shell
;;        a: activate
;;        d: deactivate
;;

;;
;; magit
;; C-c v: enters version control management
;;       B: blame
;;       F: fetch
;;       S: stage file
;;       U: unstage file
;;       t: git timemachine
;;       x: magit file delete
;;

;;
;; wgrep and wgrep-ag
;; Usage: grep some keywords, and C-c C-p in result buffer.
;; After editing, just save with C-x C-s as usual file in the buffer.
;; Others:
;; C-c C-e: Apply changes to file buffers
;; C-c C-u: All changes are unmarked and ignored
;; C-c C-d: Mark as delete to current line
;; C-c C-r: Remove the changes in the region
;; C-c C-p: Toggle read-only area
;; C-c C-k: Discard all changes and exit
;; C-c C-q: Exit wgrep mode
;;

;;
;; kernel hooks
;;
(add-hook! 'prog-mode-hook #'copilot-mode)
(add-hook! 'markdown-mode-hook #'kernel/md/add-save-with-timestamp-hook)
(add-hook! 'org-mode-hook #'kernel/org/add-save-with-timestamp-hook)

(after! python
  (setq pipenv-projectile-after-switch-function
        #'pipenv-projectile-after-switch-extended)
  (setq elpy-rpc-timeout nil)
  (setq elpy-rpc-python-command "python3")
  (setq elpy-rpc-backend "jedi")
  (add-hook! 'python-mode-hook #'pipenv-mode #'elpy-mode))

;;
;; custom faces
;;
(custom-set-faces!
  ;;
  ;; Magit
  ;;
  '(magit-branch :inherit font-lock-constant-face :bold t)
  '(magit-diffstat-added :inherit font-lock-type-face)
  '(magit-diffstat-removed :inherit font-lock-variable-name-face)
  '(magit-hash :foreground "fg-alt")
  '(magit-log-author :foreground "fg-alt")
  '(magit-process-ng :inherit font-lock-warning-face :bold t)
  '(magit-process-ok :inherit font-lock-function-name-face :bold t)
  '(magit-section-heading :inherit font-lock-keyword-face :bold t)
  ;;
  ;; OCaml
  ;;
  '(tuareg-font-lock-governing-face :inherit font-lock-string-face :bold t)
  '(tuareg-font-lock-multistage-face :inherit font-lock-comment-face :bold t)
  '(tuareg-font-lock-line-number-face :foreground "base3")
  '(tuareg-font-lock-operator-face :inherit font-lock-string-face)
  '(tuareg-font-lock-module-face :inherit font-lock-type-face :bold t)
  '(tuareg-font-lock-constructor-face :inherit font-lock-constant-face :bold t)
  '(tuareg-font-lock-error-face :inherit font-lock-string-face :background "red" :bold t)
  '(tuareg-font-lock-interactive-output-face :foreground "base3")
  '(tuareg-font-lock-interactive-error-face :inherit font-lock-warning-face)
  ;;
  ;; Web
  ;;
  '(web-mode-builtin-face :inherit font-lock-builtin-face)
  '(web-mode-comment-face :inherit font-lock-comment-face)
  '(web-mode-constant-face :inherit font-lock-constant-face)
  '(web-mode-doctype-face :inherit font-lock-comment-face)
  '(web-mode-function-name-face :inherit font-lock-function-name-face)
  '(web-mode-css-selector-face :inherit font-lock-type-face)
  '(web-mode-html-attr-name-face :inherit font-lock-type-face)
  '(web-mode-html-attr-value-face :inherit font-lock-function-name-face)
  '(web-mode-html-tag-bracket-face :inherit 'default)
  '(web-mode-html-tag-face :inherit font-lock-keyword-face :weight 'bold)
  '(web-mode-keyword-face :inherit font-lock-keyword-face)
  '(web-mode-preprocessor-face :foreground "orange")
  '(web-mode-string-face :inherit font-lock-string-face)
  '(web-mode-type-face :inherit font-lock-type-face)
  '(web-mode-warning-face :inherit font-lock-warning-face))

;;
;; org
;;
(after! org
  (setq org-startup-indented t
        org-format-latex-options (plist-put org-format-latex-options :scale 1.75)
        org-ellipsis " ▾"
        org-hide-emphasis-markers t ;; Hide markup characters
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-window-setup 'current-window
        org-pretty-entities t
        org-fontify-whole-heading-line t
        org-fontify-quote-and-verse-blocks t
        org-fontify-done-headline t
        org-fontify-todo-headline t
        org-edit-src-content-indentation 2
        org-src-preserve-indentation t
        org-hide-block-startup nil
        org-cycle-separator-lines 2
        org-hide-leading-stars t
        org-export-backends '(ascii html icalendar latex md odt)
        org-export-with-toc t
        org-highlight-latex-and-related '(native)
        org-agenda-search-view-always-boolean t
        org-agenda-timegrid-use-ampm t
        org-agenda-time-grid
        '((daily today require-timed remove-match)
          (900 930 1000 1030 1200 1230 1400 1430 1600 1630 1700 1730 1800 1830 2000 2200)
          "......." "-----------------")
        org-agenda-current-time-string "⏰ ----------------"
        org-log-done 'time
        org-log-into-drawer t
        org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "|" "DONE(d)" "CANCELLED(c)" "DEFERRED(f)" "SOMEDAY(s)")
          (sequence "BACKLOG(b)" "ACTIVE(a)" "REVIEW(r)" "HOLD(h)" "|" "CANCELLED(c)")))
  (dolist
      (face '((org-document-title . 1.5)
              (org-level-1 . 1.75)
              (org-level-2 . 1.5)
              (org-level-3 . 1.25)))
    (set-face-attribute (car face) nil
                        :font "Sans Serif"
                        :foreground (face-foreground 'default nil 'default)
                        :inherit 'default
                        :underline nil
                        :weight 'bold
                        :height (cdr face))))
