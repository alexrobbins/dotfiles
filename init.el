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
;; (dolist (f (file-expand-wildcards "~/.emacs.d/*"))
;;   (add-to-list 'load-path f))

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(setq package-check-signature nil)
(package-initialize)

(require 'org-install)
(require 'org)
(setq org-startup-folded "content")
(setq org-startup-indented "indent")
(setq org-return-follows-link t)
(setq org-hide-leading-stars t)
(define-key global-map "\C-cl" 'org-store-link)

(color-theme-initialize)
(color-theme-solarized)

(add-to-list 'default-frame-alist '(alpha 85 75))

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
(when (> emacs-major-version 20)
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

(require 'smartparens-config)
(global-set-key (kbd "C-<right>") 'sp-forward-slurp-sexp)
(global-set-key (kbd "C-<left>") 'sp-forward-barf-sexp)
(setq sp-highlight-wrap-overlay nil)
(setq sp-highlight-pair-overlay nil)
(setq sp-highlight-wrap-tag-overlay nil)

 
(add-hook 'prog-mode-hook #'which-key-mode)
(add-hook 'prog-mode-hook #'smartparens-strict-mode)

(add-hook 'cider-repl-mode-hook #'smartparens-strict-mode)
(add-hook 'cider-mode-hook #'eldoc-mode)
(setq cider-repl-display-help-banner nil)

(global-company-mode)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'aggressive-indent-mode)

(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 2
                                  tab-width 2
                                  indent-tabs-mode nil)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" default)))
 '(frame-background-mode (quote dark)))
