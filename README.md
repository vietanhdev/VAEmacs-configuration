~~~
######## ##     ##    ###     ######   ######
##       ###   ###   ## ##   ##    ## ##    ##
##       #### ####  ##   ##  ##       ##
######   ## ### ## ##     ## ##        ######
##       ##     ## ######### ##             ##
##       ##     ## ##     ## ##    ## ##    ##
######## ##     ## ##     ##  ######   ######
~~~


# EMACS EDITOR - CONFIGURATION BY VIET-ANH NGUYEN

[https://vietanhdev.com](https://vietanhdev.com)

#### Some packages inside:

- Theme: monokai
- Autocomplete: ivy, company, yasnippet, autopair
- Realtime error checking: flycheck
- UI: neotree, all-the-icons

#### Installation:

~~~ bash
rm -r ~/.emacs.d
rm ~/.emacs
git clone https://github.com/vietanhdev/VAEmacs-configuration.git ~/.emacs.d
~~~

In the first time Emacs starts, it will download all the neccessary packages to complete the configuration. Please restart Emacs after the installation.

#### For Vietnamese input method:

- Use C-\ and type ‘vietnamese-telex‘ / ‘vietnamese-viqr‘ / ‘vietnamese-vni‘ .
- Use C-\ to toggle input method.

#### Screenshot

![Emacs](screenshot/screenshot.png)
