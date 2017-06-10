;;; *** EMACS CONFIGURATION ***
;;; Name:  VAEmacs 1.0
;;; By:    Viet Anh Nguyen 2017
;;; Email: vietanh@vietanhdev.com

;;;;; PACKAGE MANAGER
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)


;;; MAXIMIZE WINDOWS ON START
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; WELCOME SCREEN
(setq initial-scratch-message "
              “Talk is cheap. Show me the code.” 
                               ― Linus Torvalds ")
(setq inhibit-startup-message t)

;;;;; EMACS UI

; fontset
(set-face-attribute 'default nil :font "DejaVu Sans Mono 16")
(set-frame-font "DejaVu Sans Mono 16" nil t)
(set-face-attribute 'default (selected-frame) :height 160)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
; turn off menubar, toolbar, scollbar
;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
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


;;;;; RESIZE BUFFER WINDOWS
(global-set-key (kbd "C-s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-s-<down>") 'shrink-window)
(global-set-key (kbd "C-s-<up>") 'enlarge-window)


;;;;; AUTOCOMPLETE
; start yasnippet with emacs
(require 'yasnippet)
(yas-global-mode 1)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(require 'company-yasnippet)

(require 'helm-config)
(helm-mode 1)

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


;;;;; SMOOTH SCOLL
(require 'sublimity)
(require 'sublimity-scroll)
(require 'sublimity-attractive)
(sublimity-mode 1)


;;;;; MARKDOWN MODE
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;;;; YAML MODE
(add-hook 'yaml-mode-hook
        (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

