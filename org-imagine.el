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

;;;###autoload
(defun org-imagine-clear-cache (&optional dir)
  "clear cache files that not mentioned by files in current project."
  (interactive)
  (require 'projectile)
  (let ((root (projectile-project-root (buffer-file-name)))
        (cache-dir (if dir dir org-imagine-cache-dir)))
    (dolist (imgpath (directory-files-recursively cache-dir ""))
      (let* ((imgname (file-name-nondirectory imgpath))
             (cmd (format "grep -r %s %s" imgname root))
             (ret (shell-command cmd)))
        (when (not (eq ret 0))
          (delete-file imgpath))))))

(defun org-imagine--get-cmd (imagine-line)
  (let ((reg "^[ \t]*#\\+IMAGINE:[ \t]*"))
    (replace-regexp-in-string reg "" imagine-line)))

(defun org-imagine--get-line-at-point ()
  "current line without trailing newline"
  (replace-regexp-in-string "\n$" "" (thing-at-point 'line t)))

;;;###autoload
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
        (let* ((imagine-line (org-imagine--get-line-at-point))
               (cmd (org-imagine--get-cmd imagine-line))
               (cmd (org-imagine--expand-viewer cmd))
               (cmd-content (org-imagine--fill-cmd-input cmd))
               (cmd (car cmd-content))
               (content (cadr cmd-content))
               (path (org-imagine--get-output-path content cmd))
               (cmd-outpath (org-imagine--fill-cmd-output cmd path))
               (final-cmd (car cmd-outpath))
               (img-path (cadr cmd-outpath))
               (cmd-out-path (shell-command-to-string final-cmd))
               (img-path (if img-path img-path cmd-out-path)))
          (if current-prefix-arg
              (message final-cmd))
          (org-imagine--insert-below img-path)
          )
        (goto-char marker)
        (move-marker marker nil) ; point nowhere for GC
        t))))


(defun org-imagine--expand-viewer (cmd)
  "when viewer is in `org-imagine-view-dir` e.g. pptsnap.py 
then convert it to /full/path/to/pptsnap.py"
  (let* ((args (split-string cmd))
         (bin (car args))
         (viewer (concat
                  (file-name-as-directory org-imagine-view-dir)
                  bin)))
    (if (file-exists-p viewer)
        (replace-regexp-in-string (concat "^" bin) viewer cmd)
      cmd)))

(defun org-imagine--insert-below (filepath)
  (unless (string-blank-p (org-imagine--get-line-below))
    (next-line))
  (end-of-line)
  (insert (format "\n[[file:%s]]" filepath))
  (org-redisplay-inline-images))


(defun org-imagine-get-link-below ()
  (save-excursion
    (beginning-of-line)
    (let ((regexp "\\[\\["))
      (when (re-search-forward regexp nil t)
        (org-imagine-extract-org-link)))))


(defun org-imagine-extract-org-link ()
  "Extract the link location at point."
  (interactive)
  (when (org-in-regexp org-bracket-link-regexp 1)
    (org-link-unescape (org-match-string-no-properties 1))))


(defun org-imagine--fill-cmd-input (cmd)
  "template-expansion for %f, %l"
  (let* ((template-content (org-imagine--get-input-content cmd))
         (template (car template-content))
         (content (cadr template-content))
         cmd-input target-path)
    (if template
        (setq cmd-input (replace-regexp-in-string template content cmd))
      (setq cmd-input (concat cmd (format " -l \"%s\"" content))))
    (list cmd-input content)))


(defun org-imagine--fill-cmd-output (cmd target-path)
  "template-expansion for %o and %{}"
  (let (cmd-output)
    (if (or (string-match-p "%o" cmd) (string-match-p "%{.*}" cmd))
        (setq cmd-output (replace-regexp-in-string "%o" target-path cmd))
      (setq cmd-output
            (if (string-blank-p (org-imagine--get-line-below))
                cmd
              (concat cmd (format " -d %s" org-imagine-cache-dir)))))
    (let ((output-path
           (org-imagine-get-user-specified-target cmd-output target-path)))
      (when output-path
        (setq cmd-output (replace-regexp-in-string "%{.*}" output-path cmd-output))
        (when (path-no-ext? output-path)
          (setq output-path (concat output-path ".png"))))
      (list cmd-output output-path))))

(defun path-no-ext? (path)
  "check if `path` has no extension"
  (equal (f-no-ext path) path)
  )


(defun org-imagine--get-last-modifed (file)
  "Returns the last modified date of a FILE."
  (interactive)
  (format-time-string "%Y%m%d%H%M%S"
                      (nth 5 (file-attributes file))))

(defun org-imagine--get-hash (string n)
  (substring (secure-hash 'sha256 string) 0 n))

(defun org-imagine--get-output-path (path cmd)
  "generate output image name without extension"
  (let*
      ((filename
        (if (file-exists-p path)
            (file-name-sans-extension (file-name-nondirectory path))
          "a"))
       (modified
        (if (file-exists-p path)
            (org-imagine--get-last-modifed path)
          "b"))
       (hash
        (if (file-exists-p path)
            (org-imagine--get-hash cmd 5)
          (org-imagine--get-hash cmd 10))))
    (format "%s/%s-%s-%s"
            org-imagine-cache-dir
            filename
            modified
            hash)))

(defun org-imagine--get-line-below ()
  (save-excursion
    (beginning-of-line)
    (next-line)
    (org-imagine--get-line-at-point)))

(defun org-imagine--get-input-content (cmd)
  "extract org element based on template type, 
e.g. %f will drive org-imagine to extrat file path in the next line"
  (cond
   ((string-match-p "%f" cmd)
    (list "%f" (org-imagine--extract-path-from-link
                (org-imagine-get-link-below))))
   ((string-match-p "%l" cmd)
    (list "%l" (org-imagine--get-line-below)))
   (t
    (if (string-blank-p (org-imagine--get-line-below))
        (list "%f" "") ;; signal: don't add new args
      (list nil (org-imagine--extract-path-from-link
                 (org-imagine-get-link-below)))))))

(defun org-imagine-get-user-specified-target (imagine-line &optional default)
  "extract content in  %{ }"
  (when (string-match-p "%{.*}" imagine-line)
    (string-match "%{.*}" imagine-line)
    (let* ((out (substring (match-string 0 imagine-line) 2 -1)))
      (if (and (equal out "") default)
          default
        out))))

(defun org-imagine--extract-path-from-link (link)
  "get pure path from org link, e.g [[file:~/abc.org::12]] return ~/abc.org
also convert org-id to file path"
  (let ((path 
         (cond ((string-prefix-p "id:" link)
                (org-id-find-id-file
                 (concat "" (substring link (length "id:")))))
               ((string-prefix-p "pdf:" link)
                (org-imagine--path-trim-trail "pdf:" link))
               ((string-prefix-p "file:" link)
                (org-imagine--path-trim-trail "file:" link))
               (t (org-imagine--path-trim-trail "" link)))))
    (when (file-exists-p path)
      path)))

(defun org-imagine--path-trim-trail (prefix link)
  "retain substring before `::`"
  (replace-regexp-in-string
   "::.*$"
   "" 
   (concat "" (substring link (length prefix)))))

(provide 'org-imagine)
