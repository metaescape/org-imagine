;;; org-imagine.el ---  an org element visualization decorator -*- lexical-binding:t -*-
(require 'org-element)

(defgroup org-imagine nil
  "Insert a image related to an org element."
  :group 'org
  :prefix "org-imagine-"
  :link '(url-link :tag "Github" "https://github.com/metaescape/org-imagine.git")
  )

(defvar org-imagine-dir
  (file-name-directory (locate-library "org-imagine")))

(defvar org-imagine-view-dir
  (concat org-imagine-dir "view/"))

(defvar org-imagine-cache-dir "./.org-imagine")

(defun org-imagine-clear-cache (&optional dir)
  "clear cache files that not mentioned by files in current project."
  (interactive)
  (let ((root (projectile-project-root (buffer-file-name)))
        (cache-dir (if dir dir org-imagine-cache-dir)))
    (dolist (imgpath (directory-files-recursively cache-dir ""))
      (let* ((imgname (file-name-nondirectory imgpath))
             (cmd (format "grep -r %s %s" imgname root))
             (ret (shell-command cmd)))
        (when (not (eq ret 0))
          (delete-file imgpath))))))

(defun org-imagine-view ()
  "search `'#+IMAGINE:`' backward, parse arguments, 
generate image and insert image link in the next line"
  (interactive)
  (unless (file-directory-p org-imagine-cache-dir)
    (make-directory org-imagine-cache-dir))
  
  (save-restriction
    (end-of-line)
    (let* ((marker (move-marker (make-marker) (point)))
           (regexp "^[ \t]*#\\+IMAGINE:"))
      (when (re-search-backward regexp nil t)
        (let* ((args (split-string (thing-at-point 'line t)))
               (link (org-imagine-get-link-below))
               (viewer (concat org-imagine-view-dir (cadr args)))
               (str-args (mapconcat 'identity (cddr args) " "))
               (cmd (format "%s -l=\"%s\" -d=\"%s\" %s"
                            viewer
                            link
                            org-imagine-cache-dir
                            str-args))
               (img-path (shell-command-to-string cmd))
               )
          (org-imagine-insert-below img-path)
          )
        (goto-char marker)
        (move-marker marker nil) ; point nowhere for GC
        t))))


(defun org-imagine-insert-below (filepath)
  (next-line) (end-of-line)
  (insert (format "\n[[file:%s]]" filepath))
  (org-redisplay-inline-images))


(defun org-imagine-get-link-below ()
  (save-excursion
    (beginning-of-line)
    (let ((regexp "\\[\\["))
      (when (re-search-forward regexp nil t)
        (extract-org-link)))))


(defun extract-org-link ()
  "Extract the link location at point."
  (interactive)
  (when (org-in-regexp org-bracket-link-regexp 1)
    (org-link-unescape (org-match-string-no-properties 1))))


(provide 'org-imagine)
