# dirtree.el -- Directory tree views for Emacs

## Installation

```elisp
;; Add dirtree and dependencies to load path
(add-to-list 'load-path "PATH_TO/dirtree")
;; Compile .el files (optional)
(byte-recompile-directory "PATH_TO/dirtree" 0)
;; Autoload dirtree
(autoload 'dirtree "dirtree" "Add directory to tree view" t)
```

## Git submodule installation

In your emacs configuration repository execute the following commands

```sh
git submodule add git@github.com:rtircher/dirtree.git .emacs.d/vendor/dirtree
```

In your `.emacs` file add the following

```elisp
(add-to-list 'load-path "~/.emacs.d/vendor/dirtree")
(autoload 'dirtree "dirtree" "Add directory to tree view" t)
```

You can also precompile the elisps files located in the vendor directory by adding the following line to your `.emacs` config file

```elisp
(byte-recompile-directory "~/.emacs.d/vendor" 0)
```

# Sources extracted from

* http://www.emacswiki.org/emacs/dirtree.el
* http://www.emacswiki.org/emacs/tree-mode.el
* http://www.emacswiki.org/emacs/windata.el
