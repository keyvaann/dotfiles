(message "1. Requires successfully loaded.")

(defun kill-and-join-forward (&optional arg)
  "If at end of line, join with following; otherwise kill line.
   Deletes whitespace at join.
   http://www.emacswiki.org/emacs/AutoIndentation"
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (progn (forward-char 1)
             (just-one-space 0)
             (backward-char 1)
             (kill-line arg))
    (kill-line arg)))

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad.
   http://whattheemacsd.com/buffer-defuns.el-01.html"
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun iwb ()
  "Perform a bunch of operations on the whitespace content of a buffer.
   Including indent-buffer, which should not be called automatically on save.
   http://whattheemacsd.com/buffer-defuns.el-01.html"
  (interactive)
  (cleanup-buffer-safe)
  (indent-region (point-min) (point-max)))

(defun skip-to-next-blank-line ()
  (interactive)
  (let ((inhibit-changing-match-data t))
    (skip-syntax-forward " >")
    (unless (search-forward-regexp "^\\s *$" nil t)
      (goto-char (point-max)))))

(defun skip-to-previous-blank-line ()
  (interactive)
  (let ((inhibit-changing-match-data t))
    (skip-syntax-backward " >")
    (unless (search-backward-regexp "^\\s *$" nil t)
      (goto-char (point-min)))))

;; Original idea from
;; http://www.opensubscriber.com/message/emacs-devel@gnu.org/10971693.html
(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
        If no region is selected and current line is not blank and we are not at the end of the line,
        then comment current line.
        Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(defun point-in-comment ()
  "Determine if the point is inside a comment"
  (interactive)
  (let ((syn (syntax-ppss)))
    (and (nth 8 syn)
         (not (nth 3 syn)))))

(defun end-of-code-or-line+ (arg)
  "Move to the end of code. If already there, move to the end of line,
  that is after the possible comment. If at the end of line, move to the
  end of code.
  Comments are recognized in any mode that sets syntax-ppss properly."
  (interactive "P")
  (let ((eoc (save-excursion
               (move-end-of-line arg)
               (while (point-in-comment)
                 (backward-char))
               (skip-chars-backward " \t")
               (point))))
    (cond ((= (point) eoc)
           (move-end-of-line arg))
          (t
           (move-end-of-line arg)
           (while (point-in-comment)
             (backward-char))
           (skip-chars-backward " \t")))))

(defun toggle-window-split ()
  "Annoyed when Emacs opens the window below instead at the side?
  http://whattheemacsd.com/buffer-defuns.el-03.html
  toggles between horizontal and vertical layout of two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

;; Mark whole line
(defun mark-line (&optional arg)
  "Marks a line"
  (interactive "p")
  (beginning-of-line)
  (push-mark (point) nil t)
  (end-of-line))

(message "2. Functions successfully defined.")

(add-hook 'text-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (auto-fill-mode 1)
            (flyspell-mode))
          )

;; enable flyspell-mode in markdown-mode
(add-hook 'markdown-mode-hook 'flyspell-mode)

;; Reuse Dired Buffers, http://emacsblog.org/2007/02/25/quick-tip-reuse-dired-buffers/
(put 'dired-find-alternate-file 'disabled nil)

;; Disable nuisance enabled commands
(put 'overwrite-mode 'disabled t)

(message "3. Hooks and puts successfully defined.")

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(setq
 backup-by-copying-when-linked t  ; Copy linked files, don't rename.
 clean-buffer-list-delay-general 2
 clean-buffer-list-delay-special (* 6 3600)
 desktop-base-file-name "emacs-desktop"
 desktop-base-lock-name "emacs-desktop-lock"
 desktop-recover-location user-emacs-directory
 enable-recursive-minibuffers t         ;; enable multiple minibuffers:
 global-auto-revert-non-file-buffers t
 ido-ignore-buffers '("\\` " "^\\*ESS\\*" "^\\*Buffer" "^\\*epc con 3\\*$"
                      "^\\*.*Completions\\*$" "^\\*Ediff" "^\\*tramp" "^\\*cvs-"
                      "_region_" " output\\*$" "^TAGS$" "^\*Ido" "^\*GNU Emacs")
 ido-ignore-directories '("\\__pycache__/" "\\.prv/" "\\`CVS/" "\\`\\.\\./" "\\`\\./")
 ido-ignore-files '("\\`auto/" "\\.prv/" "_region_" "\\`CVS/" "\\`#" "\\`.#"
                    "\\`\\.\\./" "\\`\\./")
 ido-max-prospects 8
 inhibit-eol-conversion t
 ispell-list-command "--list"
 line-move-ignore-invisible nil
 max-lisp-eval-depth 1000
 mouse-wheel-follow-mouse 't
 mouse-wheel-scroll-amount '(1 ((shift) . 1))
 next-line-add-newlines t            ;; add a new line when going to the next line
 read-file-name-completion-ignore-case 't ; Ignore case when completing file names
 require-final-newline 't
 save-interprogram-paste-before-kill t
 show-paren-style 'mixed ; Highlight text between parens
 track-eol t                         ; stay at end-of-line when moving vertically
 tramp-auto-save-directory (expand-file-name "autosaves/" user-emacs-directory)
 vc-follow-symlinks t  ;; follow symlinks and don't ask
 )

(message "4. Variables successfully defined.")

(global-set-key (kbd "<f1>") 'ispell-word)
(global-set-key (kbd "C-'") 'kill-this-buffer)
(global-set-key (kbd "C-c a") 'list-matching-lines)
(global-set-key (kbd "C-e") 'end-of-code-or-line+)
(global-set-key (kbd "C-k") 'kill-and-join-forward)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-x C-c") 'delete-frame) ;; the mnemonic is C-x REALLY QUIT
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal) ;; I don't need to kill emacs that easily
(global-set-key (kbd "C-x t c") 'highlight-changes-mode)
(global-set-key (kbd "M-;") 'comment-dwim-line)
;; Here's one keybinding I could not live without.
;; http://whattheemacsd.com/key-bindings.el-03.html
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1))) ; joins the following line onto this one.
(global-set-key (kbd "C-z") 'repeat)
;; Defining some useful keybindings
(global-set-key (kbd "C-c l") 'mark-line)
;;
(global-set-key "\M-9" 'backward-sexp)
(global-set-key "\M-0" 'forward-sexp)
(global-set-key "\M-1" 'delete-other-windows)

(message "5. Key bindings successfully defined.")

;; always need a scrach
(run-with-idle-timer 1 t
                     '(lambda () (get-buffer-create "*scratch*")))

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-flet ((process-list ())) ad-do-it))

(eval-after-load "web-mode"
  '(progn
     (define-key web-mode-map
       [remap forward-paragraph] 'skip-to-next-blank-line)

     (define-key web-mode-map
       [remap backward-paragraph] 'skip-to-previous-blank-line)))

(message "6. Other stuff successfully defined.")

(abbrev-mode t)
(midnight-delay-set 'midnight-delay 16200) ;; (eq (* 4.5 60 60) "4:30am")
(temp-buffer-resize-mode 1)
;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)
;; Remove text in active region if inserting text
(delete-selection-mode 1)

(setq prelude-theme 'monokai)
(scroll-bar-mode -1)
(message "7. Config file has successfully loaded.")

(global-subword-mode +1)
;; (eval-after-load "vc" '(remove-hook 'find-file-hooks 'vc-find-file-hook))

;; http://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html
(define-key ctl-x-map "\C-i" 'endless/ispell-word-then-abbrev)

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word'. Then create an abbrev for the correction made.
With prefix P, create local abbrev. Otherwise it will be global."
  (interactive "P")
  (let ((bef (downcase (or (thing-at-point 'word) ""))) aft)
    (call-interactively 'ispell-word)
    (setq aft (downcase (or (thing-at-point 'word) "")))
    (unless (string= aft bef)
      (message "\"%s\" now expands to \"%s\" %sally"
               bef aft (if p "loc" "glob"))
      (define-abbrev
        (if p local-abbrev-table global-abbrev-table)
        bef aft))))

(setq save-abbrevs t)
(setq-default abbrev-mode t)

;; http://endlessparentheses.com/meta-binds-part-2-a-peeve-with-paragraphs.html
(global-set-key "\M-a" 'endless/backward-paragraph)
(global-set-key "\M-e" 'endless/forward-paragraph)

(defun endless/forward-paragraph (&optional n)
  "Advance just past next blank line."
  (interactive "p")
  (let ((m (use-region-p))
        (para-commands
         '(endless/forward-paragraph endless/backward-paragraph)))
    ;; Only push mark if it's not active and we're not repeating.
    (or m
        (not (member this-command para-commands))
        (member last-command para-commands)
        (push-mark))
    ;; The actual movement.
    (dotimes (_ (abs n))
      (if (> n 0)
          (skip-chars-forward "\n[:blank:]")
        (skip-chars-backward "\n[:blank:]"))
      (if (search-forward-regexp
           "\n[[:blank:]]*\n[[:blank:]]*" nil t (cl-signum n))
          (goto-char (match-end 0))
        (goto-char (if (> n 0) (point-max) (point-min)))))
    ;; If mark wasn't active, I like to indent the line too.
    (unless m
      ;; This looks redundant, but it's surprisingly necessary.
      (back-to-indentation))))

(defun endless/backward-paragraph (&optional n)
  "Go back up to previous blank line."
  (interactive "p")
  (endless/forward-paragraph (- n)))

;; http://www.masteringemacs.org/articles/2010/12/22/fixing-mark-commands-transient-mark-mode/
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
(global-set-key (kbd "C-`") 'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-`") 'jump-to-mark)

;; http://wenshanren.org/?p=351
(defun python-add-breakpoint ()
  "Add a break point"
  (interactive)
  (newline-and-indent)
  (insert "import ipdb; ipdb.set_trace()")
  (highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()"))
;; (define-key python-mode-map (kbd "C-c C-b") 'python-add-breakpoint)

(defun python-interactive ()
  "Enter the interactive Python environment"
  (interactive)
  (progn
    (insert "from IPython import embed; embed()")
    (move-end-of-line 1)
    (comint-send-input)))
;; (global-set-key (kbd "C-c i") 'python-interactive)

(define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)
(define-key sp-keymap (kbd "M-k") 'sp-kill-hybrid-sexp)
(define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
(define-key sp-keymap (kbd "C-<left_bracket>") 'sp-select-previous-thing)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)

(require 'highlight-symbol)
(highlight-symbol-nav-mode)
(add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))
(add-hook 'org-mode-hook (lambda () (highlight-symbol-mode)))
(setq highlight-symbol-idle-delay 0.2
      highlight-symbol-on-navigation-p t)
(global-set-key (kbd "M-n") 'highlight-symbol-next)
(global-set-key (kbd "M-p") 'highlight-symbol-prev)

(defun sacha/vsplit-last-buffer (prefix)
  "Split the window vertically and display the previous buffer."
  (interactive "p")
  (split-window-vertically)
  (other-window 1 nil)
  (unless prefix
    (switch-to-next-buffer)))
(defun sacha/hsplit-last-buffer (prefix)
  "Split the window horizontally and display the previous buffer."
  (interactive "p")
  (split-window-horizontally)
  (other-window 1 nil)
  (unless prefix (switch-to-next-buffer)))
(global-set-key (kbd "C-x 2") 'sacha/vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'sacha/hsplit-last-buffer)

;; https://github.com/sachac/.emacs.d/blob/gh-pages/Sacha.org#pop-to-mark
(global-set-key (kbd "C-x p") 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))
(setq helm-buffers-fuzzy-matching t) ; fuzzy matching buffer names when non--nil

(autoload 'helm-company "helm-company") ;; Not necessary if using ELPA package
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))

(setq langtool-language-tool-jar "~/.emacs.d/LanguageTool-2.7/languagetool-commandline.jar")
