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


;;;;; HELM FOR AUTOCOMPLETE ANYTHING
(require 'helm-mode)
(require 'helm-config)
;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

; autoheight for the search windows
(helm-autoresize-mode 1)
(setq helm-autoresize-max-height 40)

; use Helm for searching commands
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

; show kill-ring (deleted items)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

; open helm-mini
(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

; use Helm to find files
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)


; replace isearch
;; C-s in a buffer: open helm-swoop with empty search field
(global-set-key (kbd "C-s") 'helm-swoop)
(with-eval-after-load 'helm-swoop
    (setq helm-swoop-pre-input-function
        (lambda () nil)))

;; C-s in helm-swoop with empty search field: activate previous search.
;; C-s in helm-swoop with non-empty search field: go to next match.
(with-eval-after-load 'helm-swoop
    (define-key helm-swoop-map (kbd "C-s") 'tl/helm-swoop-C-s))

(defun tl/helm-swoop-C-s ()
    (interactive)
    (if (boundp 'helm-swoop-pattern)
            (if (equal helm-swoop-pattern "")
                    (previous-history-element 1)
                (helm-next-line))
    (helm-next-line)
    ))

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
;(require 'ido)
;(ido-mode t)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (helm-swoop counsel ivy yasnippet use-package sublimity paganini-theme neotree multiple-cursors markdown-mode key-chord iedit company autopair))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
