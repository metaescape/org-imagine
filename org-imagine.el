;;; org-imagine.el ---  an org element visualization decorator -*- lexical-binding:t -*-
(require 'org-element)
(require 'f)

(defgroup org-imagine nil
  "Insert a image related to an org element."
  :group 'org
  :prefix "org-imagine-"
  :link '(url-link :tag "Github" "https://github.com/metaescape/org-imagine.git")
  )


(defvar org-imagine-dir
  (file-name-directory (locate-library "org-imagine")))


(defvar org-imagine-view-dir (concat org-imagine-dir "view/"))


(defvar org-imagine-cache-dir "./.org-imagine")


(defvar org-imagine-is-overwrite t
  "when non nil, existing file link will be overwrite by a new link
after execute org-image-view
")


;;;###autoload
(defun org-imagine-clear-cache (&optional dir)
  "clear cache files that not mentioned by files in current project."
  (interactive)
  (let ((root (car (last (project-current))))
        (cache-dir (if dir dir org-imagine-cache-dir)))
    (dolist (imgpath (directory-files-recursively cache-dir ""))
      (let* ((imgname (file-name-nondirectory imgpath))
             (cmd (format "grep -r %s %s" imgname root))
             (ret (shell-command cmd)))
        (when (not (eq ret 0))
          (message (format "delete %s" imgpath))
          (delete-file imgpath))))))

;;;###autoload
(defun org-imagine-view ()
  "search `'#+IMAGINE:`' backward, parse arguments, 
  generate image/src-block and insert image link or src block below"
  (interactive)
  (unless (file-directory-p org-imagine-cache-dir)
    (make-directory org-imagine-cache-dir))
  (end-of-line)
  (let ((regexp "^[ \t]*#\\+IMAGINE:"))
    (when (re-search-backward regexp nil t)
      (if (not-quote-link-followed-imagine)
          (org-imagine--view-image)
        (org-imagine--view-src-block)))))

(defun not-quote-link-followed-imagine ()
  "Check if the current line matches specific format after '#+IMAGINE:'."
  (save-excursion
    (let ((current-line (thing-at-point 'line t)))
      (not (string-match "^[ \t]*#\\+IMAGINE:[ \t]+\"" current-line)))))

(defun org-imagine--view-image ()
  (save-excursion
    (let*
        ((marker (move-marker (make-marker) (point)))
         (cmd-outpath (org-imagine--read-and-parse-cmd))
         (final-cmd (car cmd-outpath))
         (img-path (cadr cmd-outpath))
         (nothing (org-imagine--insert-below "\n: imagining ..."))
         (proc
          (start-process-shell-command
           "org-imagine-view"
           nil
           final-cmd)))

      (message final-cmd)
      (set-process-filter
       proc
       (lambda (proc cmd-out-path)
         (when (not img-path)
           (org-imagine--insert-image marker cmd-out-path))))
      (set-process-sentinel
       proc
       (lambda (proc event)
         (when (and (equal event "finished\n") img-path)
           (org-imagine--insert-image marker img-path))))
      t)))

(defun org-imagine--view-src-block ()
  "Open a link, extract content if it's a Python file, then insert it into a Python source block."
  (let* ((src-out (extract-block-from-target-file)))
    (insert-src-out-into-python-src-block src-out)))

(defun insert-src-out-into-python-src-block (src-out)
  "Insert SRC-OUT into a new Python source block below the current line with alignment."
  (let ((start (point))
        (indentation (current-indentation)))
    (end-of-line)
    ;; insert code with proper indentation
    (insert (replace-regexp-in-string "^" (make-string indentation ?\s) src-out))
    (goto-char start)
    (forward-line)))

(defun org-imagine--insert-image (marker img-path)
  (save-excursion
    (goto-char marker)
    (move-marker marker nil) ; point nowhere for GC
    (org-imagine--insert-below (format "\n[[file:%s]]" img-path))
    (org-redisplay-inline-images)))

(defun org-imagine--read-and-parse-cmd ()
  (let* ((imagine-line (org-imagine--get-line-at-point))
         (cmd (org-imagine--get-cmd imagine-line))
         (cmd (org-imagine--expand-viewer cmd))
         (cmd-content (org-imagine--fill-cmd-input cmd))
         (cmd (car cmd-content))
         (content (cadr cmd-content))
         (path (org-imagine--get-output-path content cmd))
         (cmd-and-outpath (org-imagine--fill-cmd-output cmd path))
         (final-cmd (car cmd-and-outpath)))
    (if current-prefix-arg
        (message final-cmd))
    cmd-and-outpath))

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

(defun org-imagine--fill-cmd-input (cmd)
  "template-expansion for %f, %l"
  (let* ((template-content (org-imagine--get-input-content cmd))
         (template (car template-content))
         (content (cadr template-content)))
    (if (equal template "-l") 
        (list (concat cmd (format " -l \"%s\"" content)) content)
      (list (replace-regexp-in-string template content cmd) content))))

(defun org-imagine--get-cmd (imagine-line)
  (let ((reg "^[ \t]*#\\+IMAGINE:[ \t]*"))
    (replace-regexp-in-string reg "" imagine-line)))

(defun org-imagine--get-line-at-point ()
  "current line without trailing newline"
  (replace-regexp-in-string "\n$" "" (thing-at-point 'line t)))

(defun org-imagine--insert-below (content) 
  (save-excursion
    (next-logical-line)
    (next-logical-line)
    (while (org-imagine--on-attr-comment)
      (next-logical-line))
    (org-imagine--maybe-remove-current-link)
    (insert content)))

(defun org-imagine--on-attr-comment (&optional line)
  (let ((regexp "#\\+") ;; #+ATTR_ #+CAPTION #+NAME
        (line (if line line (thing-at-point 'line t))))
    (string-match-p regexp line)))

(defun org-imagine--maybe-remove-current-link ()
  (beginning-of-line)
  (let ((img-regexp "\\[\\[file")
        (placeholder-regexp ": imagining")
        (line (thing-at-point 'line t)))
    (when (or
           (string-match-p placeholder-regexp line)
           (and org-imagine-is-overwrite
                (string-match-p img-regexp line)))
      (kill-whole-line))
    (previous-line)
    (end-of-line)))

(defun org-imagine--get-link-below ()
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

(defun org-imagine--has-output-placeholder (cmd)
  (or (string-match-p "%o" cmd) (string-match-p "%{.*}" cmd)))

(defun org-imagine--fill-cmd-output (cmd target-path)
  "template-expansion for %o and %{}"
  (let (cmd-output)
    (if (org-imagine--has-output-placeholder cmd)
        (setq cmd-output (replace-regexp-in-string "%o" target-path cmd))
      ;; add default -d option to snap cmd
      (setq cmd-output (concat cmd (format " -d %s" org-imagine-cache-dir))))
    (let ((output-path
           (org-imagine-get-user-specified-target cmd-output target-path)))
      (when output-path
        (setq cmd-output (replace-regexp-in-string "%{.*}" output-path cmd-output))
        (when (path-no-ext? output-path)
          (setq output-path (concat output-path ".png"))))
      (list cmd-output output-path))))

(defun org-imagine--on-attr-comment-or-blank (&optional line)
  (let ((target-line (if line line (org-imagine--get-line-at-point))))
    (or (string-blank-p target-line)
        (org-imagine--on-attr-comment target-line))))

(defun path-no-ext? (path)
  "check if `path` has no extension"
  (equal (f-no-ext path) path))

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
          "N"))
       (modified
        (if (file-exists-p path)
            (org-imagine--get-last-modifed path)
          "T"))
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
    (next-logical-line)
    (org-imagine--get-line-at-point)))

(defun org-imagine--get-input-content (cmd)
  "extract org element based on template type, 
e.g. %f will drive org-imagine to extract file path in the next line"
  (let ((next-logical-line (org-imagine--get-line-below)))
    (cond
     ((string-match-p "%f" cmd)
      (list "%f" (org-imagine--extract-path-from-link
                  (org-imagine--get-link-below))))
     ((string-match-p "%l" cmd) (list "%l" next-logical-line))
     ((org-imagine--on-attr-comment-or-blank next-logical-line) (list "" ""))
     ;; snap command need -l
     (t (list "-l" (org-imagine--extract-path-from-link
                    (org-imagine--get-link-below)))))))

(defun org-imagine-get-user-specified-target (imagine-line &optional default)
  "extract content in  %{ }"
  (when (string-match-p "%{.*}" imagine-line)
    (string-match "%{.*}" imagine-line)
    (let* ((out (substring (match-string 0 imagine-line) 2 -1)))
      (if (and (equal out "") default)
          default
        out))))

(defun org-imagine--extract-path-from-link (link)
  "Get pure path from org link, e.g., [[file:~/abc.org::12]] return ~/abc.org.
Also convert org-id to file path. Error out if the link is invalid or file does not exist."
  (let ((path 
         (cond ((string-prefix-p "id:" link)
                (let ((id-file (org-id-find-id-file
                                (substring link (length "id:")))))
                  (unless id-file
                    (error "Invalid link: ID not found"))
                  id-file))
               ((string-prefix-p "pdf:" link)
                (org-imagine--path-trim-tail "pdf:" link))
               ((string-prefix-p "file:" link)
                (org-imagine--path-trim-tail "file:" link))
               ((string-prefix-p "http" link)
                link)
               (t (org-imagine--path-trim-tail "" link)))))

    (unless (or (string-prefix-p "http" path)
                (and (stringp path) (file-exists-p path)))
      (error "Link path does not exist: %s" path))
    path))

(defun org-imagine--path-trim-tail (prefix link)
  "retain substring before `::`"
  (replace-regexp-in-string
   "::.*$"
   "" 
   (concat "" (substring link (length prefix)))))

(defun org-edit-special-advice-for-org-imagine (original-function &optional arg)
  (let ((element (org-element-at-point)))
    (pcase (org-element-type element)
      (`keyword
       (if (member (org-element-property :key element)
		           '("IMAGINE"))
           (let ((value (org-element-property :value element)))
             (unless (org-string-nw-p value) (user-error "No file to edit"))
             (let* ((file (and (string-match "\\`\"\\(.*?\\)\"\\|\\S-+" value)
			                   (or (match-string 1 value)
			                       (match-string 0 value))))
                    )
	           (when (org-file-url-p file)
	             (user-error "Files located with a URL cannot be edited"))
	           (org-link-open-from-string-with-abbrev-list (format "[[%s]]" file))))
         (funcall original-function arg)))
      (_ (funcall original-function arg)))))

(advice-add 'org-edit-special :around #'org-edit-special-advice-for-org-imagine)

(defun extract-python-block (args)
  "Extract the content of the Python block at the current point based on indentation.
If :only-contents is t, remove docstrings from the block."
  (save-excursion
    (let ((initial-indentation (current-indentation))
          (origin (point))
          (min-indentation (current-indentation))
          start)
      ;; Move up to find the start of the block
      (forward-line -1)
      (while (and (not (bobp))  ; Check if it's the beginning of the buffer
                  (not (looking-at "^\\s-*$"))
                  (looking-at "^@") ; only include lines of decorator
                  (= (current-indentation) initial-indentation))
        (forward-line -1))
      (when (not (bobp)) (forward-line 1))
      (setq start (point))
      ;; Now move down like the original code
      (goto-char origin)
      (forward-line 1)
      (while (and (not (eobp))
                  (or (and (> (current-indentation) 0) 
                           (> (current-indentation) initial-indentation))
                      (looking-at "^$")))
        (setq min-indentation (min min-indentation (current-indentation)))
        (forward-line 1))      ;; Extract the block content
      (let ((content (buffer-substring-no-properties start (point))))
        ;; If :only-contents is t, remove docstrings
        (when (and (assoc :only-contents args)
                   (string-equal "t" (cdr (assoc :only-contents args))))
          ;; Remove docstrings following class or def declarations
          ;; https://emacs.stackexchange.com/questions/58001/search-forward-regexp-with-back-reference
          ;; as . does not match newlines, the .*? should be \\(.\\|\n\\)*?
          ;; if you needed it to match with arbitrary multi-line TEXT. 
          ;; backup "\\(\"\"\"\\|'''\\)\\(.\\|\n\\)*?\\1\n?" this will remove \n after the second '''
          (setq content
                (replace-regexp-in-string "\\(\"\"\"\\|'''\\)\\(.\\|\n\\)*?\\1" "" content nil t)))
        (with-temp-buffer
          (insert content)
          (goto-char (point-min))
          (while (not (eobp))
            (delete-char min-indentation)
            (forward-line 1))
          (buffer-string))))))


(defun extract-elisp-s-expression (args)
  "Extract the smallest s-expression at the current point."
  (save-excursion
    (condition-case nil
        (let (start end)
          ;; Check if the cursor is on a parenthesis
          (if (or (looking-at "(") (looking-back ")" 1))
              (setq start (point))  ; If on a parenthesis, start from here
            (backward-up-list)      ; Otherwise, move to the start of the current s-expression
            (setq start (point)))
          (forward-sexp)           ; Move to the end of the current s-expression
          (setq end (point))
          (concat (buffer-substring-no-properties start end) "\n"))  ; Extract the s-expression as a string
      (error nil))))  ; Return nil if no valid s-expression is found

(defvar org-imagine--mode-extract-function-map
  '((python-mode . extract-python-block)
    (emacs-lisp-mode . extract-elisp-s-expression)))

(defun extract-block-for-mode (args)
  "extract code base one major-mode。"
  (let ((extract-func (cdr (assoc major-mode org-imagine--mode-extract-function-map))))
    (when extract-func
      (funcall extract-func args))))

(defun extract-block-content-at-point (options)
  "Extract the content at point and wrap it in an Org source block."
  (let ((extracted-content (extract-block-for-mode options))
        (original-mode (replace-regexp-in-string
                        "-mode\\'"
                        ""
                        (symbol-name major-mode))))  ; Capture the original mode before creating a new buffer
    (when extracted-content
      (let ((temp-buffer (generate-new-buffer "*org-imagine-temp*")))
        (with-current-buffer temp-buffer
          (insert extracted-content)
          (goto-char (point-min))
          (delete-blank-lines)
          ;; Use the captured mode for setting up the source block
          (goto-char (point-max))
          (delete-blank-lines)
          ;; Ensure no trailing empty lines
          (goto-char (point-max))
          (setq extracted-content
                (concat
                 "\n#+BEGIN_SRC " original-mode "\n"
                 (buffer-string)
                 "#+END_SRC\n")))
        (kill-buffer temp-buffer))
      extracted-content)))

(defun parse-option-string (option-string)
  "Parse an option string with a file and key-value pairs."
  (when (string-match "\\`\"\\(.*?\\)\"\\(.*\\)" option-string)
    (let ((file (match-string 1 option-string))
          (options-str (match-string 2 option-string)))
      (let ((pairs (split-string (string-trim options-str) ":"))
            options)
        (dolist (pair pairs options)
          ;; ignore empty
          (unless (string-match-p "^\\s-*$" pair)
            (let* ((key-value (split-string (string-trim pair)))
                   (key (intern (concat ":" (car key-value))))
                   (value (or (cadr key-value) "t")))
              ;; 添加到结果列表
              (push (cons key value) options))))
        `((:file . ,file) ,@options)))))
      

(defun extract-block-from-target-file ()
  "Extract the class or function content from a src file at the point."
  (let*
      ((element (org-element-at-point))
       (value (org-element-property :value element))
       (original-window (selected-window))
       (extracted-content ""))
    (unless (org-string-nw-p value) (user-error "No file to edit"))
    (let* ((parsed-options (parse-option-string value))
           (file (cdr (assoc ':file parsed-options))))
	  (when (org-file-url-p file)
	    (user-error "Files located with a URL cannot be edited"))
	  (org-link-open-from-string-with-abbrev-list (format "[[%s]]" file))

      (setq extracted-content (extract-block-content-at-point parsed-options))
      (delete-window)

      (select-window original-window)
      extracted-content)))

(defun org-link-open-from-string-with-abbrev-list (s &optional arg)
  "Open a link in the string S, as if it was in Org mode.
Optional argument is passed to `org-open-file' when S is
a \"file\" link. "
  (let ((original-abbrev-alist org-link-abbrev-alist-local))
    (pcase (with-temp-buffer
	         (let ((org-inhibit-startup nil))
	           (insert s)
	           (org-mode)
	           (goto-char (point-min))
               (setq-local org-link-abbrev-alist-local original-abbrev-alist)
	           (org-element-link-parser)))
      (`nil (user-error "No valid link in %S" s))
      (link (org-link-open link arg)))))

(provide 'org-imagine)
