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
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(setq org-startup-folded "content")
(setq org-startup-indented "indent")

;; load color-theme
(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
(color-theme-euphoria)

(add-to-list 'default-frame-alist '(alpha 85 75))

(add-hook 'clojure-mode-hook 'paredit-mode)

;; Needed to define ctrl arrow keys from terminal.app
(define-key input-decode-map "\e[5C" [C-right])
(define-key input-decode-map "\e[5D" [C-left])
(define-key input-decode-map "\e[3C" [C-M-right])
(define-key input-decode-map "\e[3D" [C-M-left])
(define-key input-decode-map "\e[8C" [M-right])
(define-key input-decode-map "\e[8D" [M-left])

(add-hook 'before-save-hook 'whitespace-cleanup)

(setq make-backup-files nil)

(put 'scroll-left 'disabled nil)

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
(define-key evil-motion-state-map (kbd "TAB") nil) ; So tab works in org mode
(define-key evil-normal-state-map (kbd "TAB") nil) ; So tab works in org mode
(define-key evil-normal-state-map (kbd "M-.") nil)

(require 'cider)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)

(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)
