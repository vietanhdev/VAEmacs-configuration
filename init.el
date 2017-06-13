
"
*** EMACS CONFIGURATION ***
 Name:  VAEmacs 0.1 2017.06.11
 By:    Viet Anh Nguyen
 Email: vietanh@vietanhdev.com

*** Notice:
- For Vietnamese input method:
    use C-\ > type 'vietnamese-telex' or the method you prefer. > Use C-\ to toggle input method.
"

;;;;; MAXIMIZE WINDOWS ON START
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;;;; PACKAGE MANAGER
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;;; install use-package if not installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;;;;; NOTICE: uncomment following line to download needed packages automatically
;(setq use-package-always-ensure t)


(setq initial-scratch-message "


~~~

######## ##     ##    ###     ######   ######
##       ###   ###   ## ##   ##    ## ##    ##
##       #### ####  ##   ##  ##       ##
######   ## ### ## ##     ## ##        ######
##       ##     ## ######### ##             ##
##       ##     ## ##     ## ##    ## ##    ##
######## ##     ## ##     ##  ######   ###### 

EMACS EDITOR - CONFIGURATION BY VIET-ANH NGUYEN
https://vietanhdev.com

~~~


#### Some packages inside:

- Theme: monokai
- Autocomplete: helm, company, yasnippet, autopair
- Realtime error checking: flycheck
- UX: sumlimity, neotree, all-the-icons

#### For Vietnamese input method:

- Use C-\\ and type ‘vietnamese-telex‘ / ‘vietnamese-viqr‘ / ‘vietnamese-vni‘
- Use C-\\ to toggle input method.

")

(setq inhibit-startup-message t)
(setq initial-major-mode 'markdown-mode)


;;;;; EMACS UI

;;; confirm y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;;; smooth-scoll and attractive mode
(use-package sublimity
  :config (progn
	    (use-package sublimity-scroll)
	    (use-package sublimity-attractive))
  :init (sublimity-mode 1))

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
  :init (load-theme 'monokai t))

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
 :init (progn
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
  :init
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

;;; snippets
(use-package yasnippet
  :defer t
  :init (yas-global-mode 1))

;;; autocomplete code
(use-package company
  :defer t
  :init (global-company-mode t))


;;; Helm autocomplete framework for autocomplete everything
(use-package helm
  :defer t
  :diminish helm-mode
  :init
  (progn
    (use-package helm)
    (use-package helm-config)
    (setq helm-yas-display-key-on-candidate t
	  helm-autoresize-mode 1
	  helm-autoresize-max-height 40
	  helm-M-x-fuzzy-match t ;; optional fuzzy matching for helm-M-x
	  helm-buffers-fuzzy-matching t
	  helm-recentf-fuzzy-match    t
	  helm-semantic-fuzzy-match t
	  helm-imenu-fuzzy-match    t )
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
    (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

    ;;; for helm swoop
    (use-package helm-swoop)
    (setq helm-swoop-pre-input-function
        (lambda () nil))

    ;;; C-s in helm-swoop with empty search field: activate previous search.
    ;;; C-s in helm-swoop with non-empty search field: go to next match.
    (with-eval-after-load 'helm-swoop
        (define-key helm-swoop-map (kbd "C-s") 'tl/helm-swoop-C-s))

    (defun tl/helm-swoop-C-s ()
        (interactive)
        (if (boundp 'helm-swoop-pattern)
            (if (equal helm-swoop-pattern "")
                    (previous-history-element 1)
                (helm-next-line))
        (helm-next-line)
	)))

    ;;; backspace two times to return to previous folder
    (defun fu/helm-find-files-navigate-back (orig-fun &rest args)
      (if (= (length helm-pattern) (length (helm-find-files-initial-input)))
        (helm-find-files-up-one-level 1)
        (apply orig-fun args)))
        (advice-add 'helm-ff-delete-char-backward :around #'fu/helm-find-files-navigate-back)

  
  :bind (("C-c h" . helm-command-prefix)
	 ("C-i" . helm-execute-persistent-action)
	 ("C-z" . helm-select-action)
	 ("M-x" . helm-M-x) ; use Helm for searching commands
	 ("M-y" . helm-show-kill-ring) ; show kill-ring (deleted items)
	 ("C-x b" . helm-mini) ; manage buffers
	 ("C-x C-f" . helm-find-files) ; open files
	 ("C-s" . helm-swoop)
  )
)


;;;;; AUTO PAIR QUOTES, BRACES ...
(use-package autopair
  :init
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
  :ensure t
  :init
  (global-flycheck-mode)
  (setq flycheck-check-syntax-automatically '(mode-enabled save))
)

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
