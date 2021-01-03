.. image:: http://melpa.org/packages/browse-at-remote-badge.svg
   :target: http://melpa.org/#/browse-at-remote

browse-at-remote.el
===================

This package is easiest way to open particular link on *github*/*gitlab*/*bitbucket*/*stash*/*git.savannah.gnu.org*/*sourcehut* from Emacs. It supports various kind of emacs buffer, like:

- file buffer
- dired buffer
- magit-mode buffers representing code
- vc-annotate mode (use get there by pressing ``C-x v g`` by default)

Installation:
-------------

Add ``browse-at-remote`` to your Cask file:::

  (depends-on "browse-at-remote")

Manual
******

Simply add this package to your emacs path, and add to ``.emacs``,::

  (require 'browse-at-remote)

Active keybindings for ``browse-at-remote`` function:::

  (global-set-key (kbd "C-c g g") 'browse-at-remote)

GNU Guix
********

Run ``guix package -i emacs-browse-at-remote`` then load ``browse-at-remote`` from your Emacs init.

Customization
-------------

Remote types
************

By default `browse-at-remote` knows how to work with popular remote types (github/gitlab..). Knowledge how to work with certain remote-type comes from mapping `browse-at-remote-remote-type-domains`. It defines that `github.com` should be treat in github manner, `bitbucket.org` in bitbucket manner and so on.
In your development you may have some specific git-url, and `browse-at-remote` will before confuse which remote-type map to your domain.

Two solution available:

1. In that case you can to customize that. (`M-x customize ... browse-at-remote-remote-type-domains`). For now our package supports next remote-types:


   - bitbucket.com
   - gitlab.com
   - github.com
   - Stash
   - git.savannah.gnu.org
   - gist.github.com
   - Phabricator
   - git.sr.ht
   - pagure.io
   - vs-ssh.visualstudio.com


2. Set specific remote-type directly in git repo. For example, if your repository is hosted on GitHub enterprise, you should add following setting to its config::

     git config --add browseAtRemote.type "github"

   or for private Stash repository use command::

     git config --add browseAtRemote.type "stash"

Excluding line number if no region is selected
**********************************************

By default `browse-at-remote` add line number when region is not selected in file attached buffer. If you don't like that and what to see no line information URL, it's possible to disable that by adding:::

  (setq browse-at-remote-add-line-number-if-no-region-selected nil)

Or setting via UI with `M-x customize`.


Adding new remote type
----------------------

You can your own remote if you need - PRs are welcome! Please see good examples here: gnu-savannah-remote_, or stash-remote_.


Usage:
------

1. Call function from emacs buffer::

     M-x browse-at-remote

   or::

     M-x bar-browse

   .. image:: http://i.imgur.com/rmAky8e.png

   or just call ``C-c g g`` if you've already added binding before. You can use
   this command in dired buffers too.

2. Target page at github/bitbucket will be opened using your default browser:


   .. image:: http://i.imgur.com/wBW9Gov.png
      alt: screenshot of page at github

   or same here is folder view at bitbucket:

   .. image:: http://i.imgur.com/XuzLhcR.png
      alt: screenshot page tree at bibucket

3. Opening github commit's page at *magit-commit-mode*, *magit-log-mode*:

   .. image:: http://i.imgur.com/NzlIHYr.png
      alt: screenshot of *magit-log-mode*

4. Open last commit which added target line:

   .. image:: http://i.imgur.com/lpmOAz2.png
      alt: screen of *vc-annotate-mode*

   - Press `C-x v g` to call standard vc-annotate
   - Call `browse-at-remote` on target line


Contributors:
-------------

- `@rmuslimov`_
- `@env0der`_
- `@ben`_
- `@duff`_
- `@Wilfred`_
- `@yauhen-l`_
- `@ieure`_
- `@wigust`_
- `@CyberShadow`_
- `@kuba-orlik`_
- `@jwhitbeck`_
- `@microamp`_
- `@FrostyX`_
- `@legendary-mich`_

Changelog:
--------

0.14.0
******
New remote type added **Pagure** by `@FrostyX`_.
New configuration option `browse-at-remote-add-line-number-if-no-region-selected` allowing add or not line number when target page open and region initially is not selected.

0.13.0
******
New remote type added **Sourcehut** by `@microamp`_.

0.12.0
******
New remote type added **Phabricator** by `@kuba-orlik`_.

0.11.0
******
New remote type added **gist.github.com** by `@CyberShadow`_.

0.10.0
******
New remote type added **git.savannah.gnu.org** by `@wigust`_.

0.9.0
*****
Minor fixes, added Stash (bitbucket support) by `@yauhen-l`_.

0.8.0
*****
Drop clojure-style function namings. Add abbrev methods like `bar-browse` and `bar-to-clipoboard` (where `bar` is browse-at-remote abbrev.)

0.7.0
*****
Major refactorings by `@ieure`_. Main function renamed to `browse-at-remote/browse`. (renamed in 0.8.0 to `bar-browse`)

0.6.0
*****
Added support of Gitlab by `@env0der`_. Thanks!

0.5.0
*****
Added support of Github Enterprice. Special thanks for `@env0der`_ for this feature.

0.4.0
*****
Function `browse-at-remote/to-clipboard` were added (renamed in 0.8.0 to `bar-to-clibpoard`)

TODO:
-----

- Add mercurial support


.. _`@rmuslimov`: https://github.com/rmuslimov
.. _`@env0der`: https://github.com/env0der
.. _`@Wilfred`: https://github.com/Wilfred
.. _`@ben`: https://github.com/ben
.. _`@duff`: https://github.com/duff
.. _`@ieure`: https://github.com/ieure
.. _`@yauhen-l`: https://github.com/yauhen-l
.. _`@wigust`: https://github.com/wigust
.. _`@CyberShadow`: https://github.com/CyberShadow
.. _`@kuba-orlik`: https://github.com/kuba-orlik
.. _`@jwhitbeck`: https://github.com/jwhitbeck
.. _`@microamp`: https://github.com/microamp
.. _`@FrostyX`: https://github.com/FrostyX
.. _`@legendary-mich`: https://github.com/legendary-mich
.. _stash-remote: https://github.com/rmuslimov/browse-at-remote/pull/34/files
.. _gnu-savannah-remote: https://github.com/rmuslimov/browse-at-remote/pull/46/files
