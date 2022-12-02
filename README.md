# org-imagine
Org-imagine is a visualization decorator for org-mode element, its purpose is to make inserting images in emacs org-mode easier, programmatic, and more fun 

<img src="./org-imagine2.gif" alt="Cover" width="80%"/>

## Install
``` elisp
(use-package org-imagine
  :load-path "~/.emacs.d/site-lisp/org-imagine/")
```


# Usage

Here are two examples for explaining how to use org-imagine.


## Use case: preview PPT link
- before executing org-imagine-view:
```elisp
#+IMAGINE: pptsnap.py -p 2
this is my [[file:~/keyboards.pptx][custom keybinding]]
```

- put cursor on `#+IMAGINE` or below and M-x org-imagine-view

  - when detecting a command without `%s-escape`, org-imagine will add `-l=link` and `-d=org-imagine-cache-dir` options 
  so after parsing, the real command is `pptsnap.py -p 2 -l="[[file:~/keyboards.pptx]]" -d=./.org-imagine`

  - pptsnap.py is an executable python script (command-line interface) in `org-imagine-dir`, it uses `LibreOffice` and `pdftoppm`(or `pyMupdf`) to generate the preview of the second page of `~/keyboards.pptx` in `org-imagine-cache-dir` with naming convention  `filename-last_modified-hash.png`

- after executing org-imagine-view:

```elisp
#+IMAGINE: pptsnap.py -p 2
this is my [[file:~/keyboards.pptx][custom keybinding]]
[[file:./.org-imagine/keyboards-20220903232619-25822.png]]
```


## Use case: Random Cover image generator

Downloads and preview random images from picsum:
```
#+IMAGINE: wget -O %{%o.png} %f
[[https://picsum.photos/1366/768/\?random]]
#+ATTR_HTML: :width 800 :align center
```

- put cursor on `#+IMAGINE` or below and M-x org-imagine-view, then

```org
#+IMAGINE: wget -O %{%o.png} %f
[[https://picsum.photos/1366/768/\?random]]
#+ATTR_HTML: :width 800 :align center
[[file:./.org-imagine/N-T-7bbbd1174f.png]]
```


`%o`, `%{}` and `%f` is just a kind of special [%-escapes](https://orgmode.org/manual/Template-expansion.html#FOOT86) allow dynamic insertion/substitution of content

- `%o` will be substituted by an image name in the form of  `filename-last_modified-hash`
- `%{}` is an anchor to tell org-imagine that the content within it will be used as an image path which will be inserted, allowing user-specified image path, e.g. `#+IMAGINE: wget -O %{/tmp/cover.png} %f` 
- `%f` will be substituted by the first org link below the `#+IMAGINE` comment line, currently support link formats:
  - `[[file:path_of_file::page]]` extract `path_of_file`
  - `[[pdf:file.pdf]]` extract `file.pdf`
  - `[[id:xxxxx]]` extract the file path of org id `xxxxx`
  - `[[http://url_of_image.jpg]]` extract `http://url_of_image.jpg`
  - `[[https://url_of_image.png]]` extract `https://url_of_image.png`


other templates:

- `%l`: substituted by the content of the next line



## Customization 

- `org-imagine-view-dir`: where user-defined visualiser locate, default is "/path/to/org-imagine/view/"

``` elisp
(use-package org-imagine
  :load-path "~/.emacs.d/site-lisp/org-imagine/"
  :config
  (setq org-imagine-cache-dir "./.org-imagine")
  )
```


Using `org-imagine-clear-cache`(require projectile) to clear unlinked image files (like garbage-collection)
