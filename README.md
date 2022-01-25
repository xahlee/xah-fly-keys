xah-fly-keys
===================

A modal keybinding for emacs (like vim), but based on command frequency and ergonomics.

This is the most efficient editing system in the universe.

home page at
http://ergoemacs.org/misc/ergoemacs_vi_mode.html

2020-04-18 News: Key Engine Rewrite
===================

Major key engine rewrite by Dan Langlois (https://github.com/DanLanglois) and Will Dey (https://github.com/wi11dey) .

The old stable version is available at
http://ergoemacs.org/misc/i/xah-fly-keys_old_2020-04-18.el

QWERTY layout
-------------------
![xah-fly-keys qwerty layout](xah_fly_keys_qwerty_layout_2020-04-18_4fgyk.png)

Documentation
-------------------

Add the following to `.emacs` after installing manually or via MELPA:
```elisp
(require 'xah-fly-keys)
(xah-fly-keys-set-layout "qwerty") ; required
```

The following keyboard layouts are supported:

* "azerty"
* "azerty-be"
* "colemak"
* "colemak-mod-dh"
* "colemak-mod-dh-new"
* "dvorak"
* "programer-dvorak"
* "qwerty"
* "qwerty-abnt"
* "qwertz"
* "workman"
* "norman"
* ["neo2"](https://neo-layout.org/)
* "koy"
* "adnw"
* "pt-nativo"
* "carpalx-qgmlwy"
* "carpalx-qgmlwb"
* "carpalx-qfmlwy"

Full Documentation
-------------------

http://ergoemacs.org/misc/ergoemacs_vi_mode.html

Been working on this since 2013, and since 2007 on ergoemacs-mode.

Put in 5 bucks in my patreon.
https://www.patreon.com/xahlee

or https://paypal.com
pay to xah@xahlee.org

Thanks.
