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
(global-set-key (kbd "C-x <f1>") 'ispell)
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal) ;; I don't need to kill emacs that easily
(global-set-key (kbd "C-x t c") 'highlight-changes-mode)
(global-set-key (kbd "M-;") 'comment-dwim-line)
;; Here's one keybinding I could not live without.
;; http://whattheemacsd.com/key-bindings.el-03.html
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1))) ; joins the following line onto this one.
(global-set-key (kbd "C-z") 'repeat)
;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
;; Defining some useful keybindings
(global-set-key (kbd "C-c l") 'mark-line)
;;
(global-set-key "\M-9" 'backward-sexp)
(global-set-key "\M-0" 'forward-sexp)
(global-set-key "\M-1" 'delete-other-windows)

(define-key ido-file-dir-completion-map
  [remap set-mark-command]  'ido-restrict-to-matches)

(message "5. Key bindings successfully defined.")

;; always need a scrach
(run-with-idle-timer 1 t
                     '(lambda () (get-buffer-create "*scratch*")))

(add-to-list 'clean-buffer-list-kill-regexps
             '("\\`\\*Customize .*\\*\\'"
               "\\`\\*\\(Wo\\)?Man .*\\*\\'"))

;; Hyphen on Space
;; modify smex so that typing a space will insert a hyphen ‘-’ like in normal M-x
;; http://www.emacswiki.org/emacs/Smex
(defadvice smex (around space-inserts-hyphen activate compile)
        (let ((ido-cannot-complete-command
               `(lambda ()
                  (interactive)
                  (if (string= " " (this-command-keys))
                      (insert ?-)
                    (funcall ,ido-cannot-complete-command)))))
          ad-do-it))

;;; Filters ido-matches setting acronynm matches in front of the results
;; http://www.emacswiki.org/emacs/Smex
(defadvice ido-set-matches-1 (after ido-acronym-matches activate)
  (if (> (length ido-text) 1)
      (let ((regex (concat "^" (mapconcat 'char-to-string ido-text "[^-]*-")))
            (acronym-matches (list))
            (remove-regexes '("-menu-")))
        ;; Creating the list of the results to be set as first
        (dolist (item items)
          (if (string-match (concat regex "[^-]*$") item) ;; strict match
              (add-to-list 'acronym-matches item)
            (if (string-match regex item) ;; appending relaxed match
                (add-to-list 'acronym-matches item t))))

        ;; Filtering ad-return-value
        (dolist (to_remove remove-regexes)
          (setq ad-return-value
                (delete-if (lambda (item)
                             (string-match to_remove item))
                           ad-return-value)))

        ;; Creating resulting list
        (setq ad-return-value
              (append acronym-matches
                      ad-return-value))

        (delete-dups ad-return-value)
        (reverse ad-return-value))))

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-flet ((process-list ())) ad-do-it))

;; Keep region when undoing in region
;; http://whattheemacsd.com/my-misc.el-02.html
(defadvice undo-tree-undo (around keep-region activate)
  (if (use-region-p)
      (let ((m (set-marker (make-marker) (mark)))
            (p (set-marker (make-marker) (point))))
        ad-do-it
        (goto-char p)
        (set-mark m)
        (set-marker p nil)
        (set-marker m nil))
    ad-do-it))

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

(load-theme 'monokai t)
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


;; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, subtree, or defun, whichever applies
first.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode) (org-narrow-to-subtree))
        (t (narrow-to-defun))))

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
    (insert "!import code; code.interact(local=vars())")
    (move-end-of-line 1)
    (comint-send-input)))
;; (global-set-key (kbd "C-c i") 'python-interactive)

(define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)
(define-key sp-keymap (kbd "M-k") 'sp-kill-hybrid-sexp)
(define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
(define-key sp-keymap (kbd "C-<left_bracket>") 'sp-select-previous-thing)


(setq last-kbd-macro
   "\C-u\C-u\353")


(defun elpy-nav-forward-block ()
  "Move to the next line indented like point.
This will skip over lines and statements with different
indentation levels."
  (interactive)
  (let ((indent (current-column))
        (start (point)))
    (when (/= (% indent python-indent-offset)
              0)
      (setq indent (* (1+ (/ indent python-indent-offset))
                      python-indent-offset)))
    (python-nav-forward-statement)
    (while (and (< indent (current-indentation))
                (not (eobp)))
      (python-nav-forward-statement))
    (when (< (current-indentation)
             indent)
      (goto-char start))))

(defun elpy-nav-backward-block ()
  "Move to the previous line indented like point.
This will skip over lines and statements with different
indentation levels."
  (interactive)
  (let ((indent (current-column))
        (start (point)))
    (when (/= (% indent python-indent-offset)
              0)
      (setq indent (* (1+ (/ indent python-indent-offset))
                      python-indent-offset)))
    (python-nav-backward-statement)
    (while (and (< indent (current-indentation))
                (not (bobp)))
      (python-nav-backward-statement))
    (when (< (current-indentation)
             indent)
      (goto-char start))))

(defun elpy-nav-forward-indent ()
  "Move forward to the next indent level, or over the next word."
  (interactive)
  (if (< (current-column) (current-indentation))
      (let* ((current (current-column))
             (next (* (1+ (/ current python-indent-offset))
                      python-indent-offset)))
        (goto-char (+ (point-at-bol)
                      next)))
    (let ((eol (point-at-eol)))
      (forward-word)
      (when (> (point) eol)
        (goto-char (point-at-bol))))))

(defun elpy-nav-backward-indent ()
  "Move backward to the previous indent level, or over the previous word."
  (interactive)
  (if (and (<= (current-column) (current-indentation))
           (/= (current-column) 0))
      (let* ((current (current-column))
             (next (* (1- (/ current python-indent-offset))
                      python-indent-offset)))
        (goto-char (+ (point-at-bol)
                      next)))
    (backward-word)))

(defun elpy-nav--iblock (direction skip)
  "Move point forward, skipping lines indented more than the current one.
DIRECTION should be 1 or -1 for forward or backward.
SKIP should be #'> to skip lines with larger indentation or #'<
to skip lines with smaller indentation."
  (let ((start-indentation (current-indentation)))
    (python-nav-forward-statement direction)
    (while (and (not (eobp))
                (not (bobp))
                (or (looking-at "^\\s-*$")
                    (funcall skip
                             (current-indentation)
                             start-indentation)))
      (python-nav-forward-statement direction))))

(defun elpy-nav-move-iblock-down (&optional beg end)
  "Move the current indentation block below the next one.
With an active region, move that instead of the current block.
An indentation block is a block indented further than the current
one."
  (interactive "r")
  (let ((use-region (use-region-p))
        (startm (make-marker))
        (starti nil)
        (midm (make-marker))
        (midi nil)
        (endm (make-marker))
        (deactivate-mark nil))
    (save-excursion
      (when use-region
        (goto-char beg))
      (set-marker startm (line-beginning-position))
      (setq starti (current-indentation))
      (if use-region
          (progn
            (goto-char end)
            (when (> (current-column)
                     0)
              (forward-line 1)))
        (elpy-nav--iblock 1 #'>))
      (set-marker midm (line-beginning-position))
      (setq midi (current-indentation))
      (elpy-nav--iblock 1 #'>)
      (goto-char (line-beginning-position))
      (when (<= (current-indentation)
                starti)
        (when (/= (skip-chars-backward "[:space:]\n") 0)
          (forward-line 1)))
      (when (and (= midm (point))
                 (/= (point)
                     (line-end-position))
                 (= (line-end-position)
                    (point-max)))
        (goto-char (point-max))
        (insert "\n"))
      (set-marker endm (line-beginning-position)))
    (when (and (/= startm midm)
               (/= midm endm)
               (/= startm endm)
               (= starti midi))
      (goto-char endm)
      (insert (buffer-substring startm midm))
      (when use-region
        (set-mark (point)))
      (delete-region startm midm)
      (goto-char endm)
      (back-to-indentation))))

(defun elpy-nav-move-iblock-up (&optional beg end)
  "Move the current indentation block below the next one.
With an active region, move that instead of the current block.
An indentation block is a block indented further than the current
one."
  (interactive "r")
  (let ((use-region (use-region-p))
        (startm (make-marker))
        (starti nil)
        (midm (make-marker))
        (midi nil)
        (endm (make-marker))
        (deactivate-mark nil))
    (save-excursion
      (when use-region
        (goto-char beg))
      (set-marker startm (line-beginning-position))
      (setq starti (current-indentation))
      (if use-region
          (progn
            (goto-char end)
            (when (> (current-column)
                     0)
              (forward-line 1)))
        (elpy-nav--iblock 1 #'>)
        (cond
         ((and (save-excursion
                 (goto-char (line-end-position))
                 (and (> (current-column) 0)
                      (= (point-max) (point)))))
          (goto-char (line-end-position))
          (insert "\n"))
         ((< (current-indentation)
             starti)
          (when (/= (skip-chars-backward "[:space:]\n") 0)
            (forward-line 1)))))
      (set-marker midm (line-beginning-position))
      (goto-char startm)
      (elpy-nav--iblock -1 #'>)
      (goto-char (line-beginning-position))
      (set-marker endm (line-beginning-position))
      (setq midi (current-indentation)))
    (when (and (/= startm midm)
               (/= midm endm)
               (/= startm endm)
               (= starti midi))
      (goto-char endm)
      (insert (buffer-substring startm midm))
      (when use-region
        (set-mark (point)))
      (delete-region startm midm)
      (goto-char endm)
      (back-to-indentation))))

(defun elpy-nav-move-iblock-left ()
  "Dedent the current indentation block, or the active region."
  (interactive)
  (let (beg end)
    (if (use-region-p)
        (setq beg (region-beginning)
              end (region-end))
      (save-excursion
        (setq beg (line-beginning-position))
        (elpy-nav--iblock 1 #'>)
        (setq end (line-beginning-position))))
    (python-indent-shift-left beg end)))

(defun elpy-nav-move-iblock-right ()
  "Indent the current indentation block, or the active region."
  (interactive)
  (let (beg end)
    (if (use-region-p)
        (setq beg (region-beginning)
              end (region-end))
      (save-excursion
        (setq beg (line-beginning-position))
        (elpy-nav--iblock 1 #'>)
        (setq end (line-beginning-position))))
    (python-indent-shift-right beg end)))

(global-set-key (kbd "<s-down>") 'elpy-nav-forward-block)
(global-set-key (kbd "<s-up>") 'elpy-nav-backward-block)
(global-set-key (kbd "<s-left>") 'elpy-nav-backward-indent)
(global-set-key (kbd "<s-right>") 'elpy-nav-forward-indent)
(global-set-key (kbd "<s-M-down>") 'elpy-nav-move-iblock-down)
(global-set-key (kbd "<s-M-up>") 'elpy-nav-move-iblock-up)
(global-set-key (kbd "<s-M-left>") 'elpy-nav-move-iblock-left)
(global-set-key (kbd "<s-M-right>") 'elpy-nav-move-iblock-right)

(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes 'html-mode)
(add-to-list 'aggressive-indent-excluded-modes 'python-mode)
