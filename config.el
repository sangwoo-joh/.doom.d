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
(setq doom-theme 'doom-dark+)

(defun kernel/switch-theme (theme)
  "Change theme and set fringe color to nil."
  (interactive
   (list
    (intern (completing-read
             "Load custom theme: "
             (mapcar #'symbol-name
                     (custom-available-themes))))))
  (unless (custom-theme-name-valid-p theme)
    (error "Invalid theme name `%s'" theme))
  (if (eq theme doom-theme)
      (message "Already using %s theme" theme)
    (progn
      (message "Switching to %s theme" theme)
      (setq doom-theme theme)
      (load-theme doom-theme t)
      (set-face-attribute 'fringe nil :background nil :inherit 'default))))

;; HACK: for some reason, setting fringe color after loading a theme doesn't work.
;; So I use this hook to set fringe color after loading a theme.

(defvar after-enable-theme-hook nil
   "Normal hook run after enabling a theme.")

(defun run-after-enable-theme-hook (&rest _args)
   "Run `after-enable-theme-hook'."
   (run-hooks 'after-enable-theme-hook))

(advice-add 'enable-theme :after #'run-after-enable-theme-hook)

(add-hook! 'after-enable-theme-hook
  (set-face-attribute 'fringe nil :background (face-background 'default) :foreground (face-foreground 'default)))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/org/")


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

(setq company-idle-delay nil)

;; show battery
(unless (string-match-p "battery unknown" (battery))
  (display-battery-mode 1))

;; fullscreen when starting emacs
(if (eq initial-window-system 'x)
    (toggle-frame-maximized)
  (toggle-frame-fullscreen))

(setq require-final-newline t)
(setq x-alt-keysym 'meta)
(setq tramp-default-method "sshx")
(setq password-cache-expiry nil)

;; Font settings
(when IS-LINUX
  (setq doom-font (font-spec :family "Ubuntu Mono derivative Powerline" :size 18))
  (add-hook! 'after-setting-font-hook
    (set-fontset-font "fontset-default" 'hangul (font-spec :family "D2Coding" :size 18))))

(when IS-MAC
  (setq doom-font (font-spec :family "Menlo" :size 18))
  (add-hook! 'after-setting-font-hook
    (set-fontset-font "fontset-default" 'hangul (font-spec :family "Menlo" :size 18))))


(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(set-language-environment-input-method "Korean")

(set-language-environment "Korean")

(display-time)

(prefer-coding-system 'utf-8)

(set-default-coding-systems 'utf-8)

(set-terminal-coding-system 'utf-8)

(setq locale-coding-system 'utf-8)

(set-keyboard-coding-system 'utf-8)

(set-selection-coding-system 'utf-8)

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

(after! centaur-tabs
  (setq centaur-tabs-style "wave"))

(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(server-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; my kernel functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

(defun kernel/md/get-document-filename (problem)
  "Get proper filename for document.
   This checks whether title.md or title.org exists.
   And returns the existing file path."
  (interactive)
  (let* ((md-file (concat problem ".md"))
         (org-file (concat problem ".org")))
    (cond
     ((file-exists-p org-file) org-file)
     ((file-exists-p md-file) md-file)
     ((file-exists-p problem) problem)
     (t (error "File not found: %s" problem)))))

(defun kernel/ps/get-markdown-leetcode-problem-name ()
  "Get leetcode problem name from current line's markdown link format."
  (interactive)
  (save-excursion
    (goto-char (line-end-position))
    (re-search-backward "\\[.+\\](\\(.+\\))")
    (match-string-no-properties 1)))

(defun kernel/ps/md-goto-leetcode-problem ()
  "Go to leetcode problem."
  (interactive)
  (let* ((problem (kernel/ps/get-markdown-leetcode-problem-name)))
    (browse-url (concat "https://leetcode.com/problems/" problem))))

(defun kernel/ps/org-open-link ()
  "Open doom link in browser"
  (interactive)
  (save-excursion
    (goto-char (line-end-position))
    (re-search-backward "\\[\\[\\(.+\\)\\]\\[.+\\]\\]")
    (browse-url (match-string 1))))

(defun kernel/ps/md-goto-leetcode-document ()
  "Goto leetcode document.
   For backward compatibility, it searches for .org and .md files.
   If both files exist, it will open the .org file.
   If none of them exist, it will just report an error.
   Link text regexp: \[.*\](.*)"
  (interactive)
  (let* ((problem (kernel/ps/get-markdown-leetcode-problem-name))
         (filename (kernel/md/get-document-filename problem)))

    (xref-push-marker-stack)
    (find-file filename)))

(defun kernel/ps/md-create-leetcode-document (url)
  "Create leetcode document from the problem's link."
  (interactive "sURL: ")
  (unless (string-match "^https://leetcode.com/problems/\\([^/.]+\\)/?" url)
    (error "Invalid URL: %s" url))
  (let* ((problem (match-string-no-properties 1 url))
         (title (capitalize (replace-regexp-in-string "-" " " problem))))
    (insert (format "[%s](%s)" title problem))
    (save-buffer)
    ;; 1. If problem.md or problem.org exists, do the same thing as kernel/ps/md-goto-leetcode-document
    ;; 2. Otherwise, create a new file problem.md from .template.md and set the title.
    (condition-case nil
        (kernel/ps/md-goto-leetcode-document)
      (error
       (let* ((template ".template.md")
              (filename (concat problem ".md"))
              (md-title (format "# [%s](%s)" title url)))
         (copy-file template filename)
         (xref-push-marker-stack)
         (find-file filename)
         (goto-char (point-min))
         (re-search-forward "^title:")
         (insert " ")
         (insert title)
         (goto-char (point-max))
         (electric-newline-and-maybe-indent)
         (electric-newline-and-maybe-indent)
         (insert md-title)
         (save-buffer))))))

(defun kernel/ps/org-insert-leetcode-link (url)
  "Insert leetcode link into org file."
  (interactive "sURL: ")
  (unless (string-match "^https://leetcode.com/problems/\\([^/.]+\\)/?" url)
    (error "Invalid URL: %s" url))
  (let* ((problem (match-string 1 url))
         (title (capitalize (replace-regexp-in-string "-" " " problem))))
    (insert (format "[[%s][%s]]" url title))))

(defun +workspace/switch-to-9 ()
  "Switch to workspace 9."
  (interactive)
  (+workspace/switch-to 9))

(defun kernel/copy-all ()
  "Copy all."
  (interactive)
  (save-excursion
    (kill-ring-save (point-min) (point-max)))
  (message "Copied the whole current buffer."))

(defun kernel/count-marked-items ()
  "Run ls -la | wc -l in dired mode"
  (interactive)
  (dired-unmark-all-marks)
  (dired-toggle-marks)
  (message "Total items: %d" (length (dired-get-marked-files)))
  (dired-toggle-marks))

;; Selecting all buffer is already mapped to C-x h (mark-whole-buffer)

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
(map! "<C-tab>" #'kernel/select-next-window
      "<C-S-tab>" #'kernel/select-previous-window
      "<C-iso-lefttab>" #'kernel/select-previous-window
      "C-<next>" #'centaur-tabs-forward
      "C-<prior>" #'centaur-tabs-backward
      "C-<home>" #'centaur-tabs-select-beg-tab
      "C-<end>" #'centaur-tabs-select-end-tab
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
      "C-x w" #'switch-window-then-swap-buffer
      ;; fzf
      "C-x C-r" #'fzf
      ;; neotree
      "C-x C-o" #'neotree-projectile-action
      ;; "C-c C-a" #'kernel/copy-all
      ;; swiper
      "C-c C-s" #'swiper-thing-at-point
      ;; iedit
      "C-c C-i" #'iedit-mode
      ;; After iedit-mode is on (by C-C C-e),
      ;; <tab>, S-<tab>, M->, M-<: navigation
      ;; M-;: toggle
      ;; M-N: numbering
      ;; M-R: replace
      ;; M-D: delete
      ;;
      "C-c <tab>" #'company-complete
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
      "M-9" #'+workspace/switch-to-8
      "M-0" #'+workspace/switch-to-9)

(map! :after copilot
      :map copilot-completion-map
      "<tab>" #'copilot-accept-completion
      "TAB" #'copilot-accept-completion)

(map! :after dired
      :map dired-mode-map
      "b" #'dired-up-directory
      "W" #'kernel/count-marked-items)

(map! :map markdown-mode-map
      "C-c C-c C-l" #'kernel/ps/md-create-leetcode-document
      "C-c C-c C-o" #'kernel/ps/md-goto-leetcode-problem
      "M-." #'kernel/ps/md-goto-leetcode-document)

(map! :map tuareg-mode-map
      "C-c C-f" #'ocamlformat
      "C-c C-o" #'merlin-eldoc-jump-to-next-occurrence)

(add-hook! tuareg-mode (merlin-mode))

(map! :map (c++-mode-map c-mode-map)
      "M-." #'rtags-find-symbol-at-point
      "M-," #'xref-go-back
      "C-," #'xref-go-forward
      "C-c C-t" #'rtags-symbol-type)

;; ibuffer
;; % n: mark buffers by their name, using a regexp
;; % m: mark buffers by their major mode, using a regexp
;; % f: mark buffers by their filename, using a regexp
;; % g: mark buffers by their group, using a regexp
;; % L: mark all locked buffers
;; * M: mark buffers by major mode
;; * u: mark all "unsaved" buffers
;; * m: mark all modified buffers
;; * s: mark all buffers whose name begins and ends with '*'
;; * e: mark all buffers which have an associated file, but that file doesn't currently exist
;; * /: mark buffers in dired-mode
;;

;;
;; pipenv
;; C-c C-p: enter pipenv shell
;;        a: activate
;;        d: deactivate
;;

(after! magit
  (setq auth-sources '("~/.authinfo"))
  (add-to-list 'forge-alist
               '("github.sec.samsung.net"
                 "github.sec.samsung.net/api/v3"
                 "github.sec.samsung.net"
                 forge-github-repository)))

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
;; Usage: grep some keywords:
;; C-c C-p: Enter wgrep mode
;; C-x C-s: Save editing
;; C-c C-e: Apply changes to file buffers
;; C-c C-u: All changes are unmarked and ignored
;; C-c C-d: Mark as delete to current line
;; C-c C-r: Remove the changes in the region
;; C-c C-p: Toggle read-only area
;; C-c C-k: Discard all changes and exit
;; C-c C-q: Exit wgrep mode
;;

;;
;; ein
;;
;; Open an .ipynb file, press C-c C-o, or,
;; M-x ein:run launches a jupyter process from emacs, or,
;; M-x ein:login to a running jupyter server, or,

;;
;; kernel hooks
;;
(add-hook! 'prog-mode-hook #'copilot-mode)

(defun nicer-md ()
  (progn
    (display-line-numbers-mode -1)
    (olivetti-mode 1)
    (setq markdown-header-scaling t)
    (markdown-toggle-markup-hiding)
    (markdown-toggle-fontify-code-blocks-natively)
    (highlight-indent-guides-mode -1)))

(add-hook! 'markdown-mode-hook #'kernel/md/add-save-with-timestamp-hook #'nicer-md)

(after! python
  (setq python-shell-interpreter "python3"
        python-shell-interpreter-args "-i --simple-prompt")
  (remove-hook! 'python-mode-hook #'anaconda-mode))

(after! pipenv
  (setq pipenv-projectile-after-switch-function
        #'pipenv-projectile-after-switch-extended))

(defun kernel/unify-web-mode-spacing ()
  "Stole from https://github.com/trev-dev/emacs"
  (setq web-mode-markup-indent-offset tab-width)
  (setq web-mode-css-indent-offset tab-width)
  (setq web-mode-code-indent-offset tab-width)
  (setq web-mode-style-padding tab-width)
  (setq web-mode-script-padding tab-width)
  (setq web-mode-indent-style 2))

(after! web-mode
  (add-hook! 'web-mode-hook #'kernel/unify-web-mode-spacing))

(after! typescript-mode
  (setq typescript-indent-level 2))
;;
;; custom faces
;;
(custom-set-faces!
  '(line-number :foreground "#8e8e8e")
  '(line-number-current-line :foreground "yellow")
  ;;
  ;; Magit
  ;;
  '(magit-branch-current :inherit font-lock-constant-face :bold t)
  '(magit-diffstat-added :inherit font-lock-type-face)
  '(magit-diffstat-removed :inherit font-lock-variable-name-face)
  '(magit-hash :foreground "fg-alt")
  '(magit-log-author :foreground "fg-alt")
  '(magit-process-ng :inherit font-lock-warning-face :bold t)
  '(magit-process-ok :inherit font-lock-function-name-face :bold t)
  '(magit-section-heading :inherit font-lock-keyword-face :bold t)
  ;;
  ;; Tree-Sitter
  ;;
  '(tree-sitter-hl-face:function :inherit font-lock-function-name-face)
  '(tree-sitter-hl-face:type :inherit font-lock-type-face :bold t)
  '(tree-sitter-hl-face:constructor :inherit tree-sitter-hl-face:type)
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
  '(web-mode-html-tag-face :inherit font-lock-keyword-face :bold t)
  '(web-mode-keyword-face :inherit font-lock-keyword-face)
  '(web-mode-preprocessor-face :foreground "orange")
  '(web-mode-string-face :inherit font-lock-string-face)
  '(web-mode-type-face :inherit font-lock-type-face)
  '(web-mode-warning-face :inherit font-lock-warning-face))

;;
;; org settings - from Academic-Doom-Emacs-Config
;;

(use-package! org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode)
  :config
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0)))

(use-package! org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :config (setq
           org-appear-autolinks t
           org-appear-autoentities t
           org-appear-autosubmarkers t ))

(use-package! olivetti
  :after org
  :hook (org-mode . olivetti-mode)
  :config (setq olivetti-min-body-width 50
                olivetti-body-width 80
                olivetti-style 'fancy
                olivetti-margin-width 12))

;;; Ugly org hooks
(defun nicer-org ()
  (progn
    (+org-pretty-mode 1)
    (display-line-numbers-mode -1)
    (highlight-indent-guides-mode nil)))

(add-hook! 'org-mode-hook  #'nicer-org)

(after! org
  (custom-set-faces!
    '((org-block) :background nil))
  (defface redd
    '((((class color) (min-colors 88) (background light))
       :foreground "red"))
    "Red."
    :group 'basic-faces)
  (custom-set-faces!
    '(org-level-1 :height 1.3 :weight extrabold :slant normal)
    '(org-level-2 :height 1.2 :weight bold :slant normal)
    '(org-level-3 :height 1.1 :weight regular :slant normal)
    '(org-document-title
      :family "Roboto"
      :height 250
      :weight medium)))

(after! org
  (setq org-emphasis-alist
        '(("*" (bold))
          ("/" italic)
          ("_" underline)
          ("=" redd)
          ("~" code)
          ("+" (:strike-through t))))
  (setq org-ellipsis " ▾ ")
  (appendq! +ligatures-extra-symbols
            `(:checkbox      "☐"
              :pending       "◼"
              :checkedbox    "☑"
              :list_property "∷"
              :em_dash       "—"
              :ellipses      "…"
              :arrow_right   "→"
              :arrow_left    "←"
              :title         nil
              :subtitle      "𝙩"
              :author        "𝘼"
              :date          "𝘿"
              :property      ""
              :options       "⌥"
              :startup       "⏻"
              :macro         "𝓜"
              :html_head     "🅷"
              :html          "🅗"
              :latex_class   "🄻"
              :latex_header  "🅻"
              :beamer_header "🅑"
              :latex         "🅛"
              :attr_latex    "🄛"
              :attr_html     "🄗"
              :attr_org      "⒪"
              :begin_quote   "❝"
              :end_quote     "❞"
              :caption       "☰"
              :header        "›"
              :results       "🠶"
              :begin_export  "⏩"
              :end_export    "⏪"
              :properties    ""
              :end           "∎"
              :priority_a   ,(propertize "⚑" 'face 'all-the-icons-red)
              :priority_b   ,(propertize "⬆" 'face 'all-the-icons-orange)
              :priority_c   ,(propertize "■" 'face 'all-the-icons-yellow)
              :priority_d   ,(propertize "⬇" 'face 'all-the-icons-green)
              :priority_e   ,(propertize "❓" 'face 'all-the-icons-blue)
              :roam_tags nil
              :filetags nil))
  (set-ligatures! 'org-mode
    :merge t
    :checkbox      "[ ]"
    :pending       "[-]"
    :checkedbox    "[X]"
    :list_property "::"
    :em_dash       "---"
    :ellipsis      "..."
    :arrow_right   "->"
    :arrow_left    "<-"
    :title         "#+title:"
    :subtitle      "#+subtitle:"
    :author        "#+author:"
    :date          "#+date:"
    :property      "#+property:"
    :options       "#+options:"
    :startup       "#+startup:"
    :macro         "#+macro:"
    :html_head     "#+html_head:"
    :html          "#+html:"
    :latex_class   "#+latex_class:"
    :latex_header  "#+latex_header:"
    :beamer_header "#+beamer_header:"
    :latex         "#+latex:"
    :attr_latex    "#+attr_latex:"
    :attr_html     "#+attr_html:"
    :attr_org      "#+attr_org:"
    :begin_quote   "#+begin_quote"
    :end_quote     "#+end_quote"
    :caption       "#+caption:"
    :header        "#+header:"
    :begin_export  "#+begin_export"
    :end_export    "#+end_export"
    :results       "#+RESULTS:"
    :property      ":PROPERTIES:"
    :end           ":END:"
    :priority_a    "[#A]"
    :priority_b    "[#B]"
    :priority_c    "[#C]"
    :priority_d    "[#D]"
    :priority_e    "[#E]"
    :roam_tags     "#+roam_tags:"
    :filetags      "#+filetags:")
  (plist-put +ligatures-extra-symbols :name "⁍"))

(with-eval-after-load 'org
  (plist-put org-format-latex-options :background 'default))

;; experimental - org-roam-v2
(use-package! org-roam
  :after org
  :config
  (setq org-roam-v2-ack t)
  (setq org-roam-directory "~/.roam")
  (org-roam-db-autosync-enable)
  (setq org-roam-completion-everywhere t))

(defun nicer-roam ()
  (progn
    (setq-local oliveitti-body-width 44)
    (olivetti-mode 1)
    (centered-window-mode -1)
    (set-face-background 'magit-section-highlight (face-background 'default))))

(after! org-roam
  (add-hook! 'org-roam-mode-hook #'nicer-roam))

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t)
  (setq org-roam-ui-open-on-start nil)
  (setq org-roam-ui-follow t)
  (setq org-roam-ui-update-on-save t))
