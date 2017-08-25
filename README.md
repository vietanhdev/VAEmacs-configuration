~~~
######## ##     ##    ###     ######   ######
##       ###   ###   ## ##   ##    ## ##    ##
##       #### ####  ##   ##  ##       ##
######   ## ### ## ##     ## ##        ######
##       ##     ## ######### ##             ##
##       ##     ## ##     ## ##    ## ##    ##
######## ##     ## ##     ##  ######   ######
~~~


# EMACS CONFIGURATION BY VIET-ANH NGUYEN

[https://vietanhdev.com](https://vietanhdev.com)

## About this configuration's components

- Theme: monokai
- Default font: DejaVu Sans Mono 16px. Please install this font if not available.
- Packages:
  + Load from local: smooth-scroll
  + Load using Elpa: use-package, diminish, bind-key, monokai-theme, all-the-icons, neotree (directory view), key-chord, autopair, multiple-cursors, magit (git integration), company, yasnippet, counsel (swiper), flycheck, markdown-mode, emmet-mode, rainbow-mode, js2-mode, web-mode, web-beautify, ng2-mode, rvm, robe, anaconda-mode.

## Installation:

### 1. Install the configuration file

~~~ bash
rm -r ~/.emacs.d
rm ~/.emacs
git clone https://github.com/vietanhdev/VAEmacs-configuration.git ~/.emacs.d
~~~

### 2. Install fonts

- Install font "DejaVu Sans Mono" yourself.
- Open Emacs, wait for all packages to be installed, then install icon fonts:

~~~
M-x all-the-icons-install-fonts
~~~


### 3. Finish

- Restart Emacs for all functions to work correctly.

- After all, Comment this line by putting `;` at the beginning of the line. This helps stop checking for the existence of packages upon startup and shoten startup time.

~~~elisp
(setq use-package-always-ensure t)
~~~


## For Vietnamese input method:

- Use C-\ and type ‘vietnamese-telex‘ / ‘vietnamese-viqr‘ / ‘vietnamese-vni‘ .
- Use C-\ to toggle input method.

## Screenshot

![Emacs](screenshot/screenshot.png)
