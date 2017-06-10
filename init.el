;;; *** EMACS CONFIGURATION ***
;;; Name:  VAEmacs 1.0
;;; By:    Viet Anh Nguyen 2017
;;; Email: vietanh@vietanhdev.com


;;; MAXIMIZE WINDOWS ON START
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; WELCOME SCREEN
(setq initial-scratch-message "

              “Talk is cheap. Show me the code.” 
                               ― Linus Torvalds ")
(setq inhibit-startup-message t)

;;;;; PACKAGE MANAGER
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)


;;;;; EMACS UI

; fontset
(set-face-attribute 'default nil :font "DejaVu Sans Mono")
(set-frame-font "DejaVu Sans Mono" nil t)
(set-face-attribute 'default (selected-frame) :height 100)

; turn off menubar, toolbar, scollbar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

; load theme
(load-theme 'paganini t)

; display line number
(global-linum-mode t)


;;;;; SHORTCUT KEYS
(global-set-key (kbd "C-c j") 'goto-line) 
(global-set-key [f5] 'compile)

; MANAGE BUFFER
(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define-global "jj" 'switch-to-previous-buffer)
(key-chord-define-global "kk" 'next-buffer)

;;;;; MOVE BETWEEN WINDOWS
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)


;;;;; AUTOCOMPLETE
; start yasnippet with emacs
(require 'yasnippet)
(yas-global-mode 1)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(require 'company-yasnippet)

;;;;; AUTO PAIR QUOTES, BRACES ...
(require 'autopair)
(autopair-global-mode 1)
(setq autopair-autowrap t)

;;;;; MULTIPLE CURSORS
(require 'multiple-cursors)
(global-set-key (kbd "C-x c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(global-unset-key (kbd "C-S-<down-mouse-1>"))
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)


;;;;; ZOOM IN/OUT
(global-set-key (kbd "C-x C-+") 'text-scale-increase)
(global-set-key (kbd "C-x C--") 'text-scale-decrease)
(global-set-key [C-mouse-4] '(lambda () (interactive) (text-scale-increase 1)))
(global-set-key [C-mouse-5] '(lambda () (interactive) (text-scale-decrease 1)))

;;;;; NO MESSAGE BUFFER
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;;;;; AUTOCOMPLETE EVERYTHING
(require 'ido)
(ido-mode t)

;;;;; FILE TREE VIEW
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

