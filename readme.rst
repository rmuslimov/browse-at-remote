.. image:: http://melpa.org/packages/browse-at-remote-badge.svg
   :target: http://melpa.org/#/browse-at-remote

browse-at-remote.el
===================

I'm tired to walk through Github/Bitbucket -> find required file, select particular branch and stand on required line. This package provides simple function may be called from emacs buffer, and opens target page.

Works only for git-repos and Github/Bitbucket hosted repositories.

p.s. This is openatgithub.el project reincornation.

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

If your repository is hosted on GitHub enterprise, you should add following setting to it's config:::

  git config --add browseAtRemote.type "github"


Usage:
------

1. Call function from emacs buffer::

     M-x browse-at-remote

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

- `@env0der`_
- `@ben`_
- `@duff`_


Changelog:
--------

0.6.0
*****
- Added support of Gitlab by `@env0der`_. Thanks!

0.5.0
*****

- Added support of Github Enterprice. Special thanks for `@env0der`_ for this feature.

0.4.0
*****

- Function `browse-at-remote/to-clipboard` were added

TODO:
-----

- Add mercurial support


.. _`@env0der`: https://github.com/env0der
.. _`@ben`: https://github.com/ben
.. _`@duff`: https://github.com/duff
