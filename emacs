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
;(toggle-debug-on-error)

(setq kuso:el-get-packages
      '(kuso-base
        kuso-python
        kuso-ruby)
      )

(el-get 'sync kuso:el-get-packages)
;; ===================================================================

(let ((default-directory "~/.emacs.d/my-packages/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'find-dired)
(require 'saveplace)
(require 'uniquify)
(require 'recentf)
;;(require 'multiple-cursors)
(require 'bs)                           ; a better show-buffer C-x C-b
;;(require 'bm)                           ; Bookmark Mania, http://emacsblog.org/2007/03/22/bookmark-mania/
;(require 'redo+)
;(require 'discover)

;(global-discover-mode 1)
(ido-mode 1)
(subword-mode 1)
(global-hl-line-mode t)
(abbrev-mode t)
(winner-mode 1)                         ; restore window configuration, C-c left, C-c right
(global-auto-revert-mode 1)             ; revert buffers automatically when underlying files are changed externally
(tooltip-mode -1)
;(size-indication-mode 1) ; if there is size information associated with text, change the text size to reflect it
(recentf-mode t)         ;; enable recent files mode.
;; (desktop-save-mode 1)
;(browse-kill-ring-default-keybindings)

(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

                                        ;Reload .emacs on the fly
(defun reload-dot-emacs()
  (interactive)
  (if(bufferp (get-file-buffer ".emacs"))
      (save-buffer(get-buffer ".emacs")))
  (load-file "~/.emacs")
  (message ".emacs reloaded successfully"))

;; Autofill inside of comments
(defun python-auto-fill-comments-only ()
  (auto-fill-mode 1)
  (set (make-local-variable 'fill-nobreak-predicate)
       (lambda ()
         (not (python-in-string/comment)))))

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
   This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(defun djcb-find-file-as-root ()
  "Like `ido-find-file, but automatically edit the file with
   root-privileges (using tramp/sudo), if the file is not writable by
   user."
  (interactive)
  (let ((file (ido-read-file-name "Edit as root: ")))
    (unless (file-writable-p file)
      (setq file (concat "/sudo:root@localhost:" file)))
    (find-file file)))

(defun start-or-end-macro (arg)
  (interactive "P")
  (if defining-kbd-macro
      (if arg
          (end-kbd-macro arg)
        (end-kbd-macro))
    (start-kbd-macro arg)))

(defun kill-and-join-forward (&optional arg)
  "If at end of line, join with following; otherwise kill line.
   Deletes whitespace at join.
   http://www.emacswiki.org/emacs/AutoIndentation"
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (progn
        (delete-indentation t)
        (if (looking-at " $")
            (delete-char 1)))
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

(defun ergoemacs-forward-block ()
  "Move cursor forward to the beginning of next text block.
   A text block is separated by 2 empty lines (or line with just whitespace).
   In most major modes, this is similar to `forward-paragraph', but this command's behavior is the same regardless of syntax table."
  (interactive)
  (if (search-forward-regexp "\n[[:blank:]\n]*\n+" nil "NOERROR")
      (progn (backward-char))
    (progn (goto-char (point-max)) )))

(defun ergoemacs-backward-block ()
  "Move cursor backward to previous text block.
   See: `ergoemacs-forward-block'"
  (interactive)
  (if (search-backward-regexp "\n[\t\n ]*\n+" nil "NOERROR")
      (progn
        (skip-chars-backward "\n\t ")
        (forward-char 1)
        )
    (progn (goto-char (point-min)) )))

(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun scroll-up-in-place (n)
  (interactive "p")
  (scroll-up n))

(defun scroll-down-in-place (n)
  (interactive "p")
  (scroll-down n))

(defun edit-dot-emacs ()
  "Edits the user's .emacs file"
  (interactive)
  (progn
    (find-file (or user-init-file "~/.emacs"))
    (or (eq major-mode (quote emacs-lisp-mode)) (emacs-lisp-mode))))

;convert a buffer from dos ^M end of lines to unix end of lines
(defun dos2unix ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t)
    (replace-match "")))

;versa vice
(defun unix2dos ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t)
    (replace-match "\r\n")))


;; ensure utf-8
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq-default indent-tabs-mode nil)     ; disable tab indent
(setq-default tab-width 4)
(setq-default indicate-empty-lines 't)
(setq-default frame-title-format (list "%f - Emacs")) ;; Set the name of current path/file in title bar:
;(setq-default bm-buffer-persistence t)  ;; make bookmarks persistent as default
(setq-default save-place t)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

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

;; (add-hook 'text-mode-hook (lambda () (abbrev-mode 1)))

;; make scripts executable on save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; ;; Loading the repository from file when on start up.
;; (add-hook' after-init-hook 'bm-repository-load)

;; ;; Restoring bookmarks when on file find.
;; (add-hook 'find-file-hooks 'bm-buffer-restore)

;; ;; Saving bookmark data on killing a buffer
;; (add-hook 'kill-buffer-hook 'bm-buffer-save)

;; ;; Saving the repository to file when on exit.
;; ;; kill-buffer-hook is not called when emacs is killed, so we
;; ;; must save all bookmarks first.
;; (add-hook 'kill-emacs-hook '(lambda nil
;;                               (bm-buffer-save-all)
;;                               (bm-repository-save)))


;; y-or-n-p and Function Aliases, http://emacsblog.org/2007/02/03/newbie-tip-1/
(fset 'yes-or-no-p 'y-or-n-p)

;; (define-globalized-minor-mode
;;   global-fci-mode fci-mode (lambda () (fci-mode 1)))
;; (global-fci-mode t)

; Thus, ‘M-w’ with no selection copies the current line, ‘C-w’ kills it entirely, and ‘C-a M-w C-y’ duplicates it.
; http://www.emacswiki.org/emacs/?action=browse;oldid=SlickCopy;id=WholeLineOrRegion
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

;; (setq url-proxy-services
;;       '(("no_proxy" . "^\\(localhost\\|10.*\\)")
;;         ("socks" . "127.0.0.1:9050")))

(custom-set-variables
 '(auto-revert-interval 2)
 '(smex-history-length 10)
 '(ibuffer-always-show-last-buffer :nomini)
 '(imenu-auto-rescan t)
 '(track-eol t)                         ; stay at end-of-line when moving vertically
 '(line-move-ignore-invisible nil)
 '(py-block-comment-prefix "#")
 '(py-imenu-create-index-p t)
 '(py-imenu-show-method-args-p t)
 '(py-tab-indents-region-p t)
 '(bs-must-always-show-regexp "\\(^\\*scratch\\*\\|^\\*SQL\\)") ; bs.el
 '(bs-default-sort-name "by name")                              ; bs.el
 '(recentf-max-saved-items 100)
 '(find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))
 '(next-line-add-newlines t)            ;; add a new line when going to the next line
 '(redisplay-dont-pause t)
 '(scroll-margin 3)
 '(scroll-step 3)
 '(scroll-conservatively 3)             ; avoid recentering point when scrolling
 '(mouse-wheel-follow-mouse 't)
 '(kill-whole-line t)
 '(mouse-wheel-scroll-amount '(1 ((shift) . 1)))
 '(dired-recursive-deletes 'top) ;; dired-recursive-deletes, http://emacsblog.org/2007/02/08/quick-tip-dired-recursive-deletes/
 '(initial-scratch-message "")   ;; scratch should be scratch
 '(tooltip-use-echo-area t)
 '(uniquify-buffer-name-style 'reverse)
 '(uniquify-separator "/")
 ;'(default-major-mode 'text-mode) ;; set default mode
 ;'(initial-major-mode 'text-mode) ;; set default mode
 '(uniquify-after-kill-buffer-p t) ; rename after killing uniquified
 '(uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
 '(fill-column 79)
 '(inhibit-eol-conversion t)
 '(locale-coding-system 'utf-8)
 '(file-name-coding-system 'utf-8)
 '(coding-system-for-read 'utf-8)
 '(coding-system-for-write 'utf-8)
 '(ido-enable-flex-matching t)
 '(ido-everywhere t)
 '(vc-follow-symlinks t)  ;; follow symlinks and don't ask
;; '(bm-restore-repository-on-load t)
 '(minibuffer-max-depth nil)         ;; enable multiple minibuffers:
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(version-control t)                ; Use version numbers for backups
 '(kept-new-versions 16)             ; Number of newest versions to keep
 '(kept-old-versions 2)              ; Number of oldest versions to keep
 '(delete-old-versions t)            ; Ask to delete excess backup versions?
 '(backup-by-copying-when-linked t)  ; Copy linked files, don't rename.
 '(scroll-preserve-screen-position t); Preserve point when scrolling
 '(save-interprogram-paste-before-kill t)
 '(tab-stop-list (quote (2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50 52 54 56 58 60)))
;;‬ '(words-include-escapes t)
 '(echo-keystrokes 0.1))                 ;; See what you are typing.

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-flet ((process-list ())) ad-do-it))

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; bind Caps-Lock to M-x
;; http://sachachua.com/wp/2008/08/04/emacs-caps-lock-as-m-x/
;; of course, this disables normal Caps-Lock for *all* apps...
;(if (eq window-system 'x) (shell-command "xmodmap -e 'clear Lock' -e 'keycode 66 = F13'"))

;; How did I live without this?
(global-set-key (kbd "C-'") 'other-window)
(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c x") 'smex)    ;; If you want to be able to M-x without meta (phones, etc)
;; (global-set-key (kbd "C-x C-b" 'bs-show)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-x C-i") 'imenu) ; Jump to a definition in the current file. (Protip: this is awesome.)
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal) ;; I don't need to kill emacs that easily
(global-set-key (kbd "C-x C-c") 'delete-frame) ;; the mnemonic is C-x REALLY QUIT
(global-set-key (kbd "C-h a") 'apropos)    ;The generic apropos (of any symbol) is MUCH more useful than apropos-command
(global-set-key (kbd "<f5>") 'comment-region)   ; comment
(global-set-key (kbd "<f6>") 'set-mark-command) ; mark
(global-set-key (kbd "<f7>") 'kill-ring-save)   ; copy
(global-set-key (kbd "<f8>") 'yank)             ; paste
(global-set-key (kbd "<f9>") 'kill-region)      ; delete
(global-set-key (kbd "C-x F") 'djcb-find-file-as-root)
(global-set-key (kbd "C-x C-r") 'ido-recentf-open) ;; get rid of `find-file-read-only' and replace it with something more useful.
(global-set-key (kbd "<f11>") 'toggle-fullscreen)
(global-set-key (kbd "<C-k>") 'kill-and-join-forward)
(global-set-key (kbd "<C-/>") 'undo)
(global-set-key (kbd "<C-.>") 'redo)
(global-set-key (kbd "<f13>") 'god-local-mode)
;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; (global-set-key (kbd "<M-f1>") 'bm-toggle)
;; (global-set-key (kbd "<f1>")   'bm-next)
;; (global-set-key (kbd "<S-f1>") 'bm-previous)
(global-set-key (kbd "<C-\>") 'kill-this-buffer) ; C-\ kills current buffer.
(global-set-key (kbd "<C-x home>")     'mark-buffer-before-point)
(global-set-key (kbd "<C-x end>")      'mark-buffer-after-point)
(global-set-key (kbd "<C-x M-f>")     'region-to-file)
(global-set-key (kbd "M-+") 'text-scale-increase) ;; Font size
(global-set-key (kbd "M--") 'text-scale-decrease) ;; Font size
(global-set-key (kbd "<home>") 'ergoemacs-backward-open-bracket)
(global-set-key (kbd "<end>") 'ergoemacs-forward-close-bracket)
(global-set-key (kbd "<prior>") 'ergoemacs-backward-block)
(global-set-key (kbd "<next>") 'ergoemacs-forward-block)

;; Add occur to isearch, http://emacsblog.org/2007/02/27/quick-tip-add-occur-to-isearch/
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

(define-key global-map [(shift next)] 'scroll-up-in-place)
(define-key global-map [(shift prior)] 'scroll-down-in-place)

;; always need a scrach
(run-with-idle-timer 1 t
    '(lambda () (get-buffer-create "*scratch*")))

(defun recode-region (start end &optional coding-system)
  "Replace the region with a recoded text."
  (interactive "r\n\zCoding System (cp1256): ")
  (setq coding-system (or coding-system 'cp1256-dos))
  (let ((buffer-read-only nil)
	    (text (buffer-substring start end)))
    (delete-region start end)
    (insert (decode-coding-string (string-make-unibyte text) coding-system))
    (save-buffer)
    )
  )
