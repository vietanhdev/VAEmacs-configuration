"
*** EMACS CONFIGURATION ***
 Name:  VAEmacs 0.1 2017.06.11
 By:    Viet Anh Nguyen
 Email: vietanh@vietanhdev.com

*** Notice:
- For Vietnamese input method:
    use C-\ > type 'vietnamese-telex' or the method you prefer. > Use C-\ to toggle input method.
"

;;; Decrease the number of garbage collection invocations
(setq gc-cons-threshold 10000000)
(add-hook 'emacs-startup-hook 'my/set-gc-threshold)
(defun my/set-gc-threshold ()
  "Reset `gc-cons-threshold' to its default value."
  (setq gc-cons-threshold 800000))


;;;;; MAXIMIZE WINDOWS ON START
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;;;; PACKAGE MANAGER
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)


(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;;;;; NOTICE: uncomment following line to download needed packages automatically
(setq use-package-always-ensure t)


(setq initial-scratch-message "


~~~

######## ##     ##    ###     ######   ######
##       ###   ###   ## ##   ##    ## ##    ##
##       #### ####  ##   ##  ##       ##
######   ## ### ## ##     ## ##        ######
##       ##     ## ######### ##             ##
##       ##     ## ##     ## ##    ## ##    ##
######## ##     ## ##     ##  ######   ######

GNU EMACS EDITOR - CONFIGURATION BY VIET-ANH NGUYEN   https://vietanhdev.com 
  
 ~~~

#### Some packages inside:
  - Theme: monokai
  - Autocomplete: ivy, company, yasnippet, autopair
  - Realtime error checking: flycheck
  - UX: neotree, all-the-icons

#### For Vietnamese input method:

  - Use C-\\ and type ‘vietnamese-telex‘ / ‘vietnamese-viqr‘ / ‘vietnamese-vni‘
  - Use C-\\ to toggle input method.

")

(setq inhibit-startup-message t)
(setq initial-major-mode 'markdown-mode)


;;;;; EMACS UI

;;; confirm y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;;; fontset
(set-face-attribute 'default nil :font "DejaVu Sans Mono 16")
(set-frame-font "DejaVu Sans Mono 16" nil t)
(set-face-attribute 'default (selected-frame) :height 160)

;;; turn off menubar, toolbar, scollbar
;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;; load theme
(use-package monokai-theme
  :config (load-theme 'monokai t))

;;; load icons
(use-package all-the-icons)


;;; display line number when programming
(add-hook 'prog-mode-hook 'linum-mode)
(setq linum-format "%4d \u2502 ")

;; zoom in/ zoom out
(global-set-key (kbd "C-x C-+") 'text-scale-increase)
(global-set-key (kbd "C-x C--") 'text-scale-decrease)
(global-set-key [C-mouse-4] '(lambda () (interactive) (text-scale-increase 1)))
(global-set-key [C-mouse-5] '(lambda () (interactive) (text-scale-decrease 1)))


;;; dir. tree view
(use-package neotree
 :config (progn
	  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))
 :bind ("C-x n o" . neotree-toggle))


;;;;; SHORTCUT KEYS
(global-set-key (kbd "C-c j") 'goto-line) 
(global-set-key [f5] 'compile)

;;;;; MANAGE BUFFER
(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;;; switch buffer
(use-package key-chord
  :config
  (progn
    (key-chord-mode 1)
    (key-chord-define-global "jj" 'switch-to-previous-buffer)
    (key-chord-define-global "kk" 'next-buffer)))

;;; mover between windows
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;;; resize windows
(global-set-key (kbd "C-s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-s-<down>") 'shrink-window)
(global-set-key (kbd "C-s-<up>") 'enlarge-window)

;;;;; AUTOCOMPLETE

(use-package emmet-mode
  :defer t
  :init
  (add-hook 'css-mode-hook 'emmet-mode)
  (add-hook 'sgml-mode-hook 'emmet-mode)
  :config
  (setq-default emmet-move-cursor-between-quote t)
  :bind ("<C-return>" . emmet-expand-yas)
  )

(use-package yasnippet
  :defer t
  :init
  (yas-global-mode 1))

;;; autocomplete code
(use-package company
  :config (global-company-mode t))

(use-package counsel
  :config (ivy-mode 1)
  :init
  (progn
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    ;(global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-find-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))
  )

;;;;; AUTO PAIR QUOTES, BRACES ...
(use-package autopair
  :config
  (progn
    (autopair-global-mode 1)
    (setq autopair-autowrap t)))

;;;;; MULTIPLE CURSORS
(use-package multiple-cursors
  :bind (("C-x c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
	 ("C-S-<down-mouse-1>" . mc/add-cursor-on-click)
       )
)


;;;;; FLYCHECK  - REALTIME ERROR CHECKING
(use-package flycheck
  :config
  (global-flycheck-mode)
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  )



;;;;; FOR WEB DEVELOPMENT

;;;;; RAINBOW MODE - COLOR FOR HTML/CSS
(use-package rainbow-mode
  :init
  (add-hook 'html-mode-hook 'rainbow-mode)
  (add-hook 'css-mode-hook 'rainbow-mode))

;;;;; MARKDOWN MODE
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config (setq markdown-command "multimarkdown"))

;;;;; YAML MODE
(add-hook 'yaml-mode-hook
        (lambda ()
	  (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;;;;; WEB-MODE

;;; js2-mode
(use-package js2-mode)
(use-package web-mode
  :init
  (progn
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

  
  (setq web-mode-enable-auto-pairing t
     web-mode-enable-css-colorization t
     web-mode-enable-current-element-highlight t)
  ))


;;;;; BEAUTIFY
(use-package web-beautify
   :init
  (progn
     (eval-after-load 'js2-mode
       '(define-key js2-mode-map (kbd "C-c b") 'web-beautify-js))
 
     (eval-after-load 'js
       '(define-key js-mode-map (kbd "C-c b") 'web-beautify-js))
     
     (eval-after-load 'json-mode
       '(define-key json-mode-map (kbd "C-c b") 'web-beautify-js))
     
     (eval-after-load 'sgml-mode
       '(define-key html-mode-map (kbd "C-c b") 'web-beautify-html))
     
     (eval-after-load 'web-mode
       '(define-key web-mode-map (kbd "C-c b") 'web-beautify-html))
   
     (eval-after-load 'css-mode
       '(define-key css-mode-map (kbd "C-c b") 'web-beautify-css))
     )
  )

;;;;; skewer-mode
(use-package skewer-mode)

;;;;; Ruby
(use-package rvm
  :init (rvm-use-default) ;; use rvm's default ruby for the current Emacs session
  )

(use-package robe)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (robe rvm skewer-mode web-beautify web-mode js2-mode markdown-mode rainbow-mode flycheck multiple-cursors autopair counsel company yasnippet emmet-mode key-chord neotree all-the-icons monokai-theme use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
