# org-imagine
An org element visualization decorator

(currently only support `file:` or `pdf:` link type)

## Usage

before:
```elisp
#+IMAGINE: pptsnap.py -p 2
this is my [[file:~/keyboards.pptx][custom keybinding]]
```

- put cursor on `#+IMAGINE` or below

- M-x org-imagine-view

- pptsnap.py (require `libreoffice`, `pdftoppm`) will generate the preview of the second page of `~/keyboards.pptx` in `org-imagine-cache-dir` with naming convention  `filename-lastmodified-hash.png`

- org-imagine then inserts the image link below and call `org-redisplay-inline-images` to show image

after:
```elisp
#+IMAGINE: pptsnap.py -p 2
this is my [[file:~/keyboards.pptx][custom keybinding]]
[[file:./.org-imagine/keyboards-20220903232619-25822.png]]
```


Replace pptsnap.py with other executable in `org-imagine-view-dir`. (e.g. pdfsnap.py, pptsnap.py)

You can also write your own visualizer script in `org-imagine-view-dir`, make sure it is executable

`org-imagine-clear-cache` to clear unlinked image files (like garbage-collection)


## Customization 

``` elisp
(use-package org-imagine
  :load-path "~/.emacs.d/site-lisp/org-imagine/"
  :config
  (setq org-imagine-view-dir "./view/"
        org-imagine-cache-dir "./.org-imagine")
  )
```
