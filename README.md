# org-imagine
An org element visualization decorator

(currently only support `file:` or `pdf:` link type)

<img src="./org-imagine2.gif" alt="Cover" width="100%"/>

## Usage

before:
```elisp
#+IMAGINE: pptsnap.py -p 2
this is my [[file:~/keyboards.pptx][custom keybinding]]
```

- put cursor on `#+IMAGINE` or below

- M-x org-imagine-view

- pptsnap.py is just a executable python script (command-line interface) in `./view/`, using `libreoffice` and `pdftoppm`(or `pyMupdf`) to generate the preview of the second page of `~/keyboards.pptx` in `org-imagine-cache-dir` with naming convention  `filename-lastmodified-hash.png`

- org-imagine then inserts the image link below and call `org-redisplay-inline-images` to show image

after:
```elisp
#+IMAGINE: pptsnap.py -p 2
this is my [[file:~/keyboards.pptx][custom keybinding]]
[[file:./.org-imagine/keyboards-20220903232619-25822.png]]
```


Replace pptsnap.py with other executable in `org-imagine-view-dir`. (e.g. pdfsnap.py for pdf file links, timemap.py for gtd/journal file links)

You can also write your own visualizer script in `org-imagine-view-dir`, make sure it is an executable

Using `org-imagine-clear-cache`(require projectile) to clear unlinked image files (like garbage-collection)


## Customization 

``` elisp
(use-package org-imagine
  :load-path "~/.emacs.d/site-lisp/org-imagine/"
  :config
  (setq org-imagine-view-dir "./view/"
        org-imagine-cache-dir "./.org-imagine")
  )
```
