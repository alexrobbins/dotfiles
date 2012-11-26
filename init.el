;; Drop menu bar
(menu-bar-mode 0)

;; turn off emacs startup message
(setq inhibit-startup-message t)
;; do not wrap lines
(setq-default truncate-lines t)

;; tab width as two, using spaces
(setq default-tab-width 2)
(setq-default indent-tabs-mode nil)

;; add all subdirs of ~/.emacs.d to your load-path
(dolist (f (file-expand-wildcards "~/.emacs.d/*"))
  (add-to-list 'load-path f))

(require 'package)
(add-to-list 'package-archives
                          '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; load color-theme
(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
(color-theme-euphoria)

(add-to-list 'default-frame-alist '(alpha 85 75))

;; rainbow parentheses
(require 'highlight-parentheses)
(add-hook 'clojure-mode-hook '(lambda () (highlight-parentheses-mode 1)))
(setq hl-paren-colors
      '("orange1" "yellow1" "greenyellow" "green1"
        "springgreen1" "cyan1" "slateblue1" "magenta1" "purple"))

;; magic, haven't broken this down yet
(defmacro defclojureface (name color desc &optional others)
  `(defface ,name '((((class color)) (:foreground ,color ,@others))) ,desc :group 'faces))

(defclojureface clojure-braces       "#49b2c7"   "Clojure braces")
(defclojureface clojure-brackets     "SteelBlue" "Clojure brackets")
(defclojureface clojure-keyword      "khaki"     "Clojure keywords")
(defclojureface clojure-namespace    "#c476f1"   "Clojure namespace")
(defclojureface clojure-java-call    "#4bcf68"   "Clojure Java calls")
(defclojureface clojure-special      "#b8bb00"   "Clojure special")
(defclojureface clojure-double-quote "#b8bb00"   "Clojure special" (:background "unspecified"))

(defun tweak-clojure-syntax ()
  (mapcar (lambda (x) (font-lock-add-keywords nil x))
          '((("#?['`]*(\\|)"       . 'clojure-parens))
            (("#?\\^?{\\|}"        . 'clojure-brackets))
            (("\\[\\|\\]"          . 'clojure-braces))
            ((":\\w+"              . 'clojure-keyword))
            (("#?\""               0 'clojure-double-quote prepend))
            (("nil\\|true\\|false\\|%[1-9]?" . 'clojure-special))
            (("(\\(\\.[^ \n)]*\\|[^ \n)]+\\.\\|new\\)\\([ )\n]\\|$\\)" 1 'clojure-java-call)))))

(add-hook 'clojure-mode-hook 'tweak-clojure-syntax)
(add-hook 'clojure-mode-hook 'paredit-mode)

(setq nrepl-popup-stacktraces nil)
;;(add-to-list 'same-window-buffer-names "*nrepl*") 
(add-hook 'nrepl-mode-hook 'paredit-mode)

;; Needed to define ctrl arrow keys from terminal.app
(define-key input-decode-map "\e[5C" [C-right])
(define-key input-decode-map "\e[5D" [C-left])
(define-key input-decode-map "\e[8C" [C-M-right])
(define-key input-decode-map "\e[8D" [C-M-left])

(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)

;;macros
(global-set-key (kbd "C-,")        'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "C-.")        'kmacro-end-or-call-macro)
(global-set-key (kbd "<C-return>") 'apply-macro-to-region-lines)

(add-hook 'before-save-hook 'whitespace-cleanup)

(setq frame-title-format '("%f"))

(defun smart-line-beginning ()
  "Move point to the beginning of text
on the current line; if that is already
the current position of point, then move
it to the beginning of the line."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))

(global-set-key "\C-a" 'smart-line-beginning)

(setq make-backup-files nil)

(put 'scroll-left 'disabled nil)

(global-set-key (kbd "C-c c") 'toggle-truncate-lines)
(global-set-key (kbd "C-c ;") 'comment-or-uncomment-region)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:height 140 :family "Monaco")))))

;; enable awesome file prompting
(when (> emacs-major-version 21)
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point t
        ido-max-prospects 10))

;; display pretty lambdas
(font-lock-add-keywords 'emacs-lisp-mode
    '(("(\\(lambda\\)\\>" (0 (prog1 ()
                               (compose-region (match-beginning 1)
                                               (match-end 1)
                                               ?λ))))))

;; turn off scroll-bars
(scroll-bar-mode -1)

(defun squeeze-whitespace ()
  "Squeeze white space (including new lines) between objects around point.
Leave one space or none, according to the context."
  (interactive "*")
  (skip-chars-backward " \t\r\n\f")
  (set-mark (point))
  (skip-chars-forward " \t\r\n\f")
  (kill-region (point) (mark))
  (insert ?\s)
  (fixup-whitespace))

(defun insert-line-numbers (beg end &optional start-line)
  "Insert line numbers into buffer."
  (interactive "r")
  (save-excursion
    (let ((max (count-lines beg end))
          (line (or start-line 1))
          (counter 1))
      (goto-char beg)
      (while (<= counter max)
        (insert (format "%0d	" line))
        (beginning-of-line 2)
        (incf line)
        (incf counter)))))

(defun insert-line-numbers+ ()
  "Insert line numbers into buffer."
  (interactive)
  (if mark-active
      (insert-line-numbers (region-beginning) (region-end) (read-number "Start line: "))
    (insert-line-numbers (point-min) (point-max))))

(defun strip-blank-lines ()
  "Strip blank lines in region.
   If no region strip all blank lines in current buffer."
  (interactive)
  (strip-regular-expression-string "^[ \t]*\n"))

(defun strip-line-numbers ()
  "Strip line numbers in region.
   If no region strip all the line numbers in current buffer."
  (interactive)
  (strip-regular-expression-string "^[0-9]+[ \t]?"))

(defun strip-regular-expression-string (regex)
  "Strip all strings that match regex in region.
   If no region strip current buffer."
  (interactive)
  (let ((begin (point-min))
        (end (point-max)))
    (if mark-active
        (setq begin (region-beginning)
              end (region-end)))
    (save-excursion
      (goto-char end)
      (while (and (> (point) begin)
                  (re-search-backward regex nil t))
        (replace-match "" t t)))))

(setq font-lock-verbose nil)

;; show column numbers
(setq column-number-mode t)

;; evil mode
(require 'evil)
(evil-mode 1)
(defun my-move-key (keymap-from keymap-to key)
       "Moves key binding from one keymap to another, deleting from the old location. "
            (define-key keymap-to key (lookup-key keymap-from key))
                 (define-key keymap-from key nil)
                      )
   (my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
   (my-move-key evil-motion-state-map evil-normal-state-map " ")

(show-paren-mode)
