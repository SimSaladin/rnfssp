rnfssp: Media sharing web application
=====================================

**NOTE: Absolutely not bugfree, but good enough for testing, if you really want
to...**

`rnfssp` is a web application built on top of Yesod. Features include

* Maintainers' blog.
* Media shares.
* Imageboard-style forum for users.
* Real-time chat.

Installation
------------

### Requirements

- PostgreSQL setup, and a user/db for rnfssp
- *nix system with a recent (>=7.4) GHC and cabal-install.

### Procedure

- Clone the repo.
- Run `cabal install` in the root.
- Setup files `config/postgresql.yml` and `config/settings.yml`
- run the binary `rnfssp Production`


Some known bugs
---------------

* The mpd section fails with 500 if mpd is not started
* section filepaths (at least Film,SingleSourc?) need to be absolute.
  Symbolic links don't work.

Technical information
---------------------

### Icons

by Fontello, http://fontello.com. Configuration in the repo.

### Of interest

* http://www.catswhocode.com/blog/10-useful-tools-to-simplify-css3-development

git should not track changes to configs/postgresql.yml, as
> git update-index --assume-unchanged config/postgresql.yml
