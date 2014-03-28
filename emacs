(add-to-list 'load-path "/home/mrgee/.emacs.d")

(setq developer-name "Keyvan Hedayati")
(setq developer-email "k1.hedayati93@gmail.com")
(setq kuso-workspace "~/git/")

(setq el-get-dir "/home/mrgee/.emacs.d/")
(setq el-get-git-install-url "http://github.com/KusoIDE/el-get.git")

(add-to-list 'load-path "/home/mrgee/.emacs.d/el-get/")

(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/KusoIDE/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))
    )
  )

;; Uncomment this line if you want to debug an error
(toggle-debug-on-error)

(setq kuso:el-get-packages
      '(kuso-base
        kuso-python)
        ;;kuso-ruby)
      )

(el-get 'sync kuso:el-get-packages)
;; ===================================================================


(message "0. KusoIDE successfully loaded.")

(setq k1/el-get-packages
  '(
    desktop-recover
    idomenu
    pylookup
    smex
    undo-tree
    )
)

(el-get 'sync k1/el-get-packages)

(require 'bs)                           ; a better show-buffer C-x C-b
(require 'desktop)
(require 'desktop-recover)
(require 'find-dired)
(require 'midnight)
(require 'recentf)
(require 'saveplace)                    ; http://whattheemacsd.com/init.el-03.html
(require 'undo-tree)                    ; http://www.dr-qubit.org/undo-tree/undo-tree.el
(require 'uniquify)

(message "1. Requires successfully loaded.")

(defun save-macro (name)
  "save a macro. Take a name as argument
     and save the last defined macro under
     this name at the end of your .emacs"
  (interactive "SName of the macro :")  ; ask for the name of the macro
  (kmacro-name-last-macro name)         ; use this name for the macro
  (find-file user-init-file)            ; open ~/.emacs or other user init file
  (goto-char (point-max))               ; go to the end of the .emacs
  (newline)                             ; insert a newline
  (insert-kbd-macro name)               ; copy the macro
  (newline)                             ; insert a newline
  (switch-to-buffer nil))               ; return to the initial buffer

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let* ((file-assoc-list
          (mapcar (lambda (x)
                    (cons (file-name-nondirectory x)
                          x))
                  recentf-list))
         (filename-list
          (remove-duplicates (mapcar #'car file-assoc-list)
                             :test #'string=))
         (filename (ido-completing-read "Choose recent file: "
                                        filename-list
                                        nil
                                        t)))
    (when filename
      (find-file (cdr (assoc filename
                             file-assoc-list))))))

(defun python-auto-fill-comments-only ()
  "Autofill inside of comments"
  (auto-fill-mode 1)
  (set (make-local-variable 'fill-nobreak-predicate)
       (lambda ()
         (not (python-in-string/comment)))))

(defun djcb-find-file-as-root ()
  "Like `ido-find-file, but automatically edit the file with
   root-privileges (using tramp/sudo), if the file is not writable by
   user."
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))

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

(defun ergoemacs-forward-open-bracket (&optional number)
  "Move cursor to the next occurrence of left bracket or quotation mark.
   With prefix NUMBER, move forward to the next NUMBER left bracket or quotation mark.
   With a negative prefix NUMBER, move backward to the previous NUMBER left bracket or quotation mark."
  (interactive "p")
  (if (and number
           (> 0 number))
      (ergoemacs-backward-open-bracket (- 0 number))
    (forward-char 1)
    (search-forward-regexp
     (eval-when-compile
       (regexp-opt
        '("(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«"))) nil t number)
    (backward-char 1)))

(defun ergoemacs-backward-open-bracket (&optional number)
  "Move cursor to the previous occurrence of left bracket or quotation mark.
   With prefix argument NUMBER, move backward NUMBER open brackets.
   With a negative prefix NUMBER, move forward NUMBER open brackets."
  (interactive "p")
  (if (and number
           (> 0 number))
      (ergoemacs-forward-open-bracket (- 0 number))
    (search-backward-regexp
     (eval-when-compile
       (regexp-opt
        '("(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«"))) nil t number)))

(defun ergoemacs-forward-close-bracket (&optional number)
  "Move cursor to the next occurrence of right bracket or quotation mark.
   With a prefix argument NUMBER, move forward NUMBER closed bracket.
   With a negative prefix argument NUMBER, move backward NUMBER closed brackets."
  (interactive "p")
  (if (and number
           (> 0 number))
      (ergoemacs-backward-close-bracket (- 0 number))
    (search-forward-regexp
     (eval-when-compile
       (regexp-opt '(")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»"))) nil t number)))

(defun ergoemacs-backward-close-bracket (&optional number)
  "Move cursor to the previous occurrence of right bracket or quotation mark.
   With a prefix argument NUMBER, move backward NUMBER closed brackets.
   With a negative prefix argument NUMBER, move forward NUMBER closed brackets."
  (interactive "p")
  (if (and number
           (> 0 number))
      (ergoemacs-forward-close-bracket (- 0 number))
    (backward-char 1)
    (search-backward-regexp
     (eval-when-compile
       (regexp-opt '(")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»"))) nil t number)
    (forward-char 1)))

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

(defun edit-dot-emacs ()
  "Edits the user's .emacs file"
  (interactive)
  (progn
    (find-file (or user-init-file "~/.emacs"))
    (or (eq major-mode (quote emacs-lisp-mode)) (emacs-lisp-mode))))

(defun reload-dot-emacs()
  "Reload .emacs on the fly"
  (interactive)
  (if(bufferp (get-file-buffer ".emacs"))
      (save-buffer(get-buffer ".emacs")))
  (load-file "~/.emacs")
  (message ".emacs reloaded successfully"))

(defun dos2unix ()
  "convert a buffer from dos ^M end of lines to unix end of lines"
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t)))

;; versa vice
(defun unix2dos ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t)
    (replace-match "\r\n")))

(defun undo-kill-buffer (arg)
  "Re-open the last buffer killed.  With ARG, re-open the nth buffer."
  (interactive "p")
  (let ((recently-killed-list (copy-sequence recentf-list))
        (buffer-files-list
         (delq nil (mapcar (lambda (buf)
                             (when (buffer-file-name buf)
                               (expand-file-name (buffer-file-name buf)))) (buffer-list)))))
    (mapc
     (lambda (buf-file)
       (setq recently-killed-list
             (delq buf-file recently-killed-list)))
     buffer-files-list)
    (find-file
     (if arg (nth arg recently-killed-list)
       (car recently-killed-list)))))

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

(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer.
   http://whattheemacsd.com/file-defuns.el-02.html"
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting.
   http://whattheemacsd.com/file-defuns.el-01.html"
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun search-keybind (regexp &optional nlines)
  "Occur search the full list of keybinds & their commands."
  (interactive (occur-read-primary-args))
  (save-excursion
    (describe-bindings)
    (set-buffer "*Help*")
    (occur regexp nlines)
    (delete-windows-on "*Help*")))

(defun diff-buffer-with-associated-file ()
  "View the differences between BUFFER and its associated file.
This requires the external program \"diff\" to be in your `exec-path'.
Returns nil if no differences found, 't otherwise."
  (interactive)
  (let ((buf-filename buffer-file-name)
        (buffer (current-buffer)))
    (unless buf-filename
      (error "Buffer %s has no associated file" buffer))
    (let ((diff-buf (get-buffer-create
                     (concat "*Assoc file diff: "
                             (buffer-name)
                             "*"))))
      (with-current-buffer diff-buf
        (setq buffer-read-only nil)
        (erase-buffer))
      (let ((tempfile (make-temp-file "buffer-to-file-diff-")))
        (unwind-protect
            (progn
              (with-current-buffer buffer
                (write-region (point-min) (point-max) tempfile nil 'nomessage))
              (if (zerop
                   (apply #'call-process "diff" nil diff-buf nil
                          (append
                           (when (and (boundp 'ediff-custom-diff-options)
                                      (stringp ediff-custom-diff-options))
                             (list ediff-custom-diff-options))
                           (list buf-filename tempfile))))
                  (progn
                    (message "No differences found")
                    nil)
                (progn
                  (with-current-buffer diff-buf
                    (goto-char (point-min))
                    (if (fboundp 'diff-mode)
                        (diff-mode)
                      (fundamental-mode)))
                  (display-buffer diff-buf)
                  t)))
          (when (file-exists-p tempfile)
            (delete-file tempfile)))))))

;; tidy up diffs when closing the file
(defun kill-associated-diff-buf ()
  (let ((buf (get-buffer (concat "*Assoc file diff: "
                                 (buffer-name)
                                 "*"))))
    (when (bufferp buf)
      (kill-buffer buf))))

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

(defun move-line-down ()
  "When programming I tend to shuffle lines around a lot.
   http://whattheemacsd.com/editing-defuns.el-02.html"
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  "When programming I tend to shuffle lines around a lot.
   http://whattheemacsd.com/editing-defuns.el-02.html"
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

(defun de-context-kill (arg)
  "Kill buffer, taking gnuclient into account. I use the following code (some of it modified from ibuffer.el) to display a diff of the buffer with its associated file when I try and kill a buffer that has unsaved changes. If I decide I don’t want the buffer anyway, hitting c-u <killkey> will kill the file unconditionally and also tidy up the diff buffer. http://www.emacswiki.org/emacs/KillKey"
  (interactive "p")
  (when (and (buffer-modified-p)
             buffer-file-name
             (not (string-match "\\*.*\\*" (buffer-name)))
             ;; erc buffers will be automatically saved
             (not (eq major-mode 'erc-mode))
             (= 1 arg))
    (let ((differences 't))
      (when (file-exists-p buffer-file-name)
        (setq differences (diff-buffer-with-associated-file)))
      (error (if differences
                 "Buffer has unsaved changes"
               "Buffer has unsaved changes, but no differences wrt. the file"))))
  (if (and (boundp 'gnuserv-minor-mode)
           gnuserv-minor-mode)
      (gnuserv-edit)
    (set-buffer-modified-p nil)
    (kill-buffer (current-buffer))))


(defun back-to-indentation-or-beginning ()
  "This function switches the point to before the first non-space character, or if the point is already there it goes to the beginning of the line. "
  (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))

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

(defun rotate-windows ()
  "Ever open a file in the wrong window?
   This snippet flips a two-window frame, so that left is right, or up is down.
   http://whattheemacsd.com/buffer-defuns.el-02.html
   Rotate your windows"
  (interactive)
  (cond ((not (> (count-windows)1))
         (message "You can't rotate a single window!"))
        (t
         (setq i 1)
         (setq numWindows (count-windows))
         (while  (< i numWindows)
           (let* (
                  (w1 (elt (window-list) i))
                  (w2 (elt (window-list) (+ (% i numWindows) 1)))

                  (b1 (window-buffer w1))
                  (b2 (window-buffer w2))

                  (s1 (window-start w1))
                  (s2 (window-start w2))
                  )
             (set-window-buffer w1  b2)
             (set-window-buffer w2 b1)
             (set-window-start w1 s2)
             (set-window-start w2 s1)
             (setq i (1+ i)))))))

(message "2. Functions successfully defined.")

;;Place all backup copies of files in a common location
(make-directory "~/.emacs.d/autosaves/" t)
(make-directory "~/.emacs.d/backups/" t)

(add-hook 'term-mode-hook
          (lambda()
            (yas-minor-mode -1)
            (autopair-mode -1)))

(add-hook 'python-mode-hook
          (lambda ()
            (python-auto-fill-comments-only)))

(add-hook 'text-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (auto-fill-mode 1)))

;; Various superfluous white-space. Just say no.
(add-hook 'before-save-hook 'cleanup-buffer-safe)

;; make scripts executable on save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(add-hook 'kill-buffer-hook 'kill-associated-diff-buf)

;; y-or-n-p and Function Aliases, http://emacsblog.org/2007/02/03/newbie-tip-1/
(fset 'yes-or-no-p 'y-or-n-p)

;; Thus, ‘M-w’ with no selection copies the current line, ‘C-w’ kills it entirely, and ‘C-a M-w C-y’ duplicates it.
;; http://www.emacswiki.org/emacs/?action=browse;oldid=SlickCopy;id=WholeLineOrRegion
(put 'kill-ring-save 'interactive-form
     '(interactive
       (if (use-region-p)
           (list (region-beginning) (region-end))
         (list (line-beginning-position) (line-beginning-position 2)))))

(put 'kill-region 'interactive-form
     '(interactive
       (if (use-region-p)
           (list (region-beginning) (region-end))
         (list (line-beginning-position) (line-beginning-position 2)))))


;; Reuse Dired Buffers, http://emacsblog.org/2007/02/25/quick-tip-reuse-dired-buffers/
(put 'dired-find-alternate-file 'disabled nil)

;; Disable nuisance enabled commands
(put 'overwrite-mode 'disabled t)

(message "3. Hooks and puts successfully defined.")

(setq url-proxy-services
   '(("no_proxy" . "^\\(localhost\\|10.*\\)")
     ("socks" . "127.0.0.1:9050")))

;; ensure utf-8
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(setq-default
 indent-tabs-mode nil     ; disable tab indent
 ispell-program-name "aspell"
 tab-width 4
 indicate-empty-lines 't
 frame-title-format (list "%b - %f - Emacs") ;; Set the name of current path/file in title bar:
 save-place t
 )

(setq
 auto-revert-interval 2
 auto-revert-verbose nil
 auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t)))
 backup-by-copying-when-linked t  ; Copy linked files, don't rename.
 backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/")))
 bs-default-sort-name "by name"                              ; bs.el
 bs-must-always-show-regexp "\\(^\\*scratch\\*\\|^\\*SQL\\)" ; bs.el
 clean-buffer-list-delay-general 2
 clean-buffer-list-delay-special (* 6 3600)
 coding-system-for-read 'utf-8
 coding-system-for-write 'utf-8
 default-major-mode 'text-mode
 delete-old-versions t            ; Ask to delete excess backup versions?
 desktop-base-file-name "emacs-desktop"
 desktop-base-lock-name "emacs-desktop-lock"
 desktop-recover-location user-emacs-directory
 dired-recursive-deletes 'top ;; dired-recursive-deletes, http://emacsblog.org/2007/02/08/quick-tip-dired-recursive-deletes/
 echo-keystrokes 0.1                 ;; See what you are typing.
 enable-recursive-minibuffers t         ;; enable multiple minibuffers:
 file-name-coding-system 'utf-8
 fill-column 79
 find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld")
 global-auto-revert-non-file-buffers t
 ibuffer-always-show-last-buffer :nomini
 ido-enable-flex-matching t
 ido-everywhere t
 ido-save-directory-list-file (expand-file-name "ido.last" user-emacs-directory)
 ido-ignore-buffers '("\\` " "^\\*ESS\\*" "^\\*Buffer" "^\\*epc con 3\\*$"
                      "^\\*.*Completions\\*$" "^\\*Ediff" "^\\*tramp" "^\\*cvs-"
                      "_region_" " output\\*$" "^TAGS$" "^\*Ido" "^\*GNU Emacs")
 ido-ignore-directories '("\\__pycache__/" "\\.prv/" "\\`CVS/" "\\`\\.\\./" "\\`\\./")
 ido-ignore-files '("\\`auto/" "\\.prv/" "_region_" "\\`CVS/" "\\`#" "\\`.#"
                    "\\`\\.\\./" "\\`\\./")
 ido-max-prospects 8
 imenu-auto-rescan t
 inhibit-eol-conversion t
 initial-scratch-message ""   ;; scratch should be scratch
 ispell-list-command "--list"
 kept-new-versions 16             ; Number of newest versions to keep
 kept-old-versions 2              ; Number of oldest versions to keep
 kill-whole-line t
 line-move-ignore-invisible nil
 locale-coding-system 'utf-8
 max-lisp-eval-depth 1000
 mouse-wheel-follow-mouse 't
 mouse-wheel-scroll-amount '(1 ((shift) . 1))
 next-line-add-newlines t            ;; add a new line when going to the next line
 py-block-comment-prefix "#"
 ;; py-imenu-create-index-p t
 ;; py-imenu-show-method-args-p t
 py-tab-indents-region-p t
 read-file-name-completion-ignore-case 't ; Ignore case when completing file names
 recentf-auto-cleanup 'never  ;; disable before we start recentf!
 recentf-max-saved-items 200
 recentf-save-file (expand-file-name "recentf" user-emacs-directory)
 redisplay-dont-pause t
 require-final-newline 't
 py-closing-list-dedents-bos t
 py-electric-colon-greedy-p t
 py-empty-line-closes-p t
 py-indent-honors-inline-comment t
 py-shell-name "ipython"
 py-split-windows-on-execute-p nil
 py-start-run-ipython-shell t
 py-tab-shifts-region-p t
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
 save-interprogram-paste-before-kill t
 save-place-file (expand-file-name "saved-places" user-emacs-directory)
 scroll-conservatively 100000             ; avoid recentering point when scrolling
 scroll-margin 3
 scroll-preserve-screen-position t ; Preserve point when scrolling
 scroll-step 3
 show-paren-style 'mixed ; Highlight text between parens
 smex-history-length 10
 smex-save-file (expand-file-name "smex-items" user-emacs-directory)
 tab-stop-list (quote (2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50 52 54 56 58 60))
 tooltip-use-echo-area t
 track-eol t                         ; stay at end-of-line when moving vertically
 tramp-auto-save-directory (expand-file-name "autosaves/" user-emacs-directory)
 uniquify-after-kill-buffer-p t ; rename after killing uniquified
 uniquify-buffer-name-style 'reverse
 uniquify-ignore-buffers-re "^\\*" ; don't muck with special buffers
 uniquify-separator "/"
 vc-follow-symlinks t  ;; follow symlinks and don't ask
 vc-make-backup-files t
 version-control t                ; Use version numbers for backups
 )

(message "4. Variables successfully defined.")

(global-set-key (kbd "<f1>") 'ispell-word)
(global-set-key (kbd "C-'") 'de-context-kill)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-u") 'search-keybind)
;; There are lots of neat ways of moving around quickly in a buffer.
;; http://whattheemacsd.com/key-bindings.el-02.html
(global-set-key (kbd "C-S-b") (lambda () (interactive) (ignore-errors (backward-char 10))))
(global-set-key (kbd "C-S-f") (lambda () (interactive) (ignore-errors (forward-char 10))))
(global-set-key (kbd "C-S-n") (lambda () (interactive) (ignore-errors (next-line 10))))
(global-set-key (kbd "C-S-p") (lambda () (interactive) (ignore-errors (previous-line 10))))
(global-set-key (kbd "C-S-<down>") 'move-line-down)
(global-set-key (kbd "C-S-<up>") 'move-line-up)
(global-set-key (kbd "C-a") 'back-to-indentation-or-beginning)
(global-set-key (kbd "C-c a") 'list-matching-lines)
(global-set-key (kbd "C-c d") 'diff-buffer-with-associated-file)
(global-set-key (kbd "C-c x") 'smex)    ;; If you want to be able to M-x without meta (phones, etc)
(global-set-key (kbd "C-e") 'end-of-code-or-line+)
(global-set-key (kbd "C-h a") 'apropos)    ;The generic apropos (of any symbol) is MUCH more useful than apropos-command
(global-set-key (kbd "C-k") 'kill-and-join-forward)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-S-w") 'kill-region)
(global-set-key (kbd "C-x C-b") 'bs-show)
(global-set-key (kbd "C-x C-c") 'delete-frame) ;; the mnemonic is C-x REALLY QUIT
(global-set-key (kbd "C-x C-e") 'rename-current-buffer-file)
(global-set-key (kbd "C-x C-i") 'idomenu) ; Jump to a definition in the current file. (Protip: this is awesome.)
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)
(global-set-key (kbd "C-x C-r") 'ido-recentf-open) ;; get rid of `find-file-read-only' and replace it with something more useful.
(global-set-key (kbd "C-x F") 'djcb-find-file-as-root)
(global-set-key (kbd "C-x M-f")     'region-to-file)
(global-set-key (kbd "C-x <f1>") 'ispell)
(global-set-key (kbd "C-x k") 'de-context-kill)
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal) ;; I don't need to kill emacs that easily
(global-set-key (kbd "C-x t c") 'highlight-changes-mode)
(global-set-key (kbd "C-x t k") 'flyspell-prog-mode)
(global-set-key (kbd "C-x t l") 'flyspell-mode)
(global-set-key (kbd "M-;") 'comment-dwim-line)
;; Here's one keybinding I could not live without.
;; http://whattheemacsd.com/key-bindings.el-03.html
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1))) ; joins the following line onto this one.
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-z") 'repeat)

;; Add occur to isearch, http://emacsblog.org/2007/02/27/quick-tip-add-occur-to-isearch/
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

(define-key ido-file-dir-completion-map
  [remap set-mark-command]  'ido-restrict-to-matches)

(define-key global-map [(shift next)] 'scroll-up-in-place)
(define-key global-map [(shift prior)] 'scroll-down-in-place)

;; In dired, M-> and M-< never take me where I want to go.
;; http://whattheemacsd.com/setup-dired.el-02.html
(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)
(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

(message "5. Key bindings successfully defined.")

;; always need a scrach
(run-with-idle-timer 1 t
                     '(lambda () (get-buffer-create "*scratch*")))

(add-to-list 'clean-buffer-list-kill-regexps
             '("\\`\\*Customize .*\\*\\'"
               "\\`\\*\\(Wo\\)?Man .*\\*\\'"))

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

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings)) ; shift+Arrow moves to other window

(eval-after-load "web-mode"
  '(progn
     (define-key web-mode-map
       [remap forward-paragraph] 'skip-to-next-blank-line)

     (define-key web-mode-map
       [remap backward-paragraph] 'skip-to-previous-blank-line)))

(message "6. Other stuff successfully defined.")

(abbrev-mode t)
(global-auto-revert-mode 1)             ; revert buffers automatically when underlying files are changed externally
(global-hl-line-mode t)
(global-undo-tree-mode)
(ido-mode 1)
(midnight-delay-set 'midnight-delay 16200) ;; (eq (* 4.5 60 60) "4:30am")
(recentf-mode t)         ;; enable recent files mode.
(savehist-mode 1)
(subword-mode 1)
(temp-buffer-resize-mode 1)
(tooltip-mode -1)
(winner-mode 1)                         ; restore window configuration, C-c left, C-c right

(message "7. Config file has successfully loaded.")

;; (desktop-recover-interactive)
