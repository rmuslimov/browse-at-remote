browse-at-remote.el
========================

I'm tired to walk through Github/Bitbucket -> find required file, select particular branch and stand on required line. This package provides simple function may be called from emacs buffer, and opens target page.

Works only for git-repos and Github/Bitbucket hosted repositories.

p.s. This is openatgithub.el project reincornation.

Installation:
-------------

Simply add this package to your emacs path, and add to ``.emacs``,::

  (require 'browse-at-remote)

Active keybindings for ``browse-at-remote`` function:::

  (global-set-key (kbd "C-c g g") 'browse-at-remote)


Usage:
------

1. Call function from emacs buffer::

     M-x browse-at-remote

   .. image:: http://i.imgur.com/RvKPgy8.png

   or just call ``C-c g g`` if you've already added binding before. You can use
   this command in dired buffers too.

2. Target page at github/bitbucket will be opened using your default browser:


   .. image:: http://i.imgur.com/ZPqK3nw.png
      alt: screenshot of page at github

   or same here is folder view at bitbucket:

   .. image:: http://i.imgur.com/XuzLhcR.png
      alt: screenshot page tree at bibucket

TODO:
-----

- Add mercurial support
