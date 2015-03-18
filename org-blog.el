;;; org-blog.el --- Blog like sitemap for org-publish

;;; Commentary:

;; This program contains a function 'org-blog-export' which can be
;; used by 'org-publish.el' instead of org-publish-org-sitemap. When
;; used, it will use variable 'org-blog-entry-format' to format an
;; entry in the sitemap file. The sitemap file may then be used as an
;; index to the blog.

;;; Code:

(defcustom org-blog-export-keywords nil
  "Set to 't' to export a list of blog entries for each keyword"
  :group 'org-blog
  :type 'boolean)

(defcustom org-blog-export-dates nil
  "Set to 't' to export a list of blog entries for each year"
  :group 'org-blog
  :type 'boolean)


(defcustom org-blog-entry-format "* %t

=by= /%a/ - =%d=

:PROPERTIES:
:HTML_CONTAINER_CLASS: blogentry
:END:
%c...

[[%l][Read more]]

"
  "Format for the entries on the blog frontpage"
  :group 'org-blog
  :type 'string)

(defun org-blog-export (project &optional blog-filename)
  "Create a sitemap of pages in set defined by PROJECT.
Optionally set the filename of the sitemap with SITEMAP-FILENAME.
Default for SITEMAP-FILENAME is 'sitemap.org'."
  (let* ((project-plist (cdr project))
         (dir (file-name-as-directory
               (plist-get project-plist :base-directory)))
         (exclude-regexp (plist-get project-plist :exclude))
         (files (nreverse (org-publish-get-base-files project exclude-regexp)))
         (blog-filename (concat dir (or blog-filename "index.org")))
         (blog-title (or (plist-get project-plist :blog-title)
                         (concat "Blog " (car project))))
         (blog-entry-format (or (plist-get project-plist
                                           :blog-entry-format)
                                org-blog-entry-format))
         (blog-encoding (or (plist-get project-plist :blog-encoding)
                            'utf-8))
         (visiting (find-buffer-visiting blog-filename))
         (blog-insert-first (plist-get project-plist :blog-insert-first))
         (export-keywords (or (plist-get project-plist :blog-export-keywords)
                              org-blog-export-keywords))
         (export-dates (or (plist-get project-plist :blog-export-dates)
                           org-blog-export-dates)))
    (with-current-buffer (setq blog-buffer
                               (or visiting (find-file blog-filename)))
      (erase-buffer)
      (setq save-buffer-coding-system blog-encoding)
      (insert (concat "#+TITLE: " blog-title "\n\n"))
      (if blog-insert-first
          (insert blog-insert-first))
      (while (setq file (pop files))
        (unless (equal (file-truename blog-filename)
                       (file-truename file))
          (let* ((link (file-relative-name file dir))
                 (entry (org-blog-format-file-entry
                         blog-entry-format
                         file link project-plist))
                 (entry-list (org-blog-format-file-entry
                              "  + [[%l][%t]]\n" file link
                              project-plist))
                 (keywords (org-blog-find-keywords file)))
            (insert entry)
            (if export-keywords
                (save-excursion
                  (let ((headlineh1 (org-find-exact-headline-in-buffer
                                     "Keywords"  (current-buffer) t)))
                    (if headlineh1
                        (progn
                          (goto-char headlineh1)
                          (forward-line 4))
                      (goto-char (point-max))
                      (insert "* Keywords\n"
                              ":PROPERTIES:\n:HTML_CONTAINER_CLASS: keywords\n"
                              ":END:\n"))
                    (mapc (lambda (keyword)
                            (save-excursion
                              (let ((headlineh2 (org-find-exact-headline-in-buffer
                                                 keyword (current-buffer) t)))
                                (if (not headlineh2)
                                    (insert "\n** " keyword "\n" entry-list)
                                  (goto-char headlineh2)
                                  (forward-line 1)
                                  (insert entry-list)))))
                          (split-string keywords ", ")))))
            (if export-dates
                (save-excursion
                  (let* ((date (format-time-string "%Y" (org-publish-find-date
                                                         file)))
                         (headlineh1 (org-find-exact-headline-in-buffer
                                      "Year"  (current-buffer) t))
                         (headlineh2 (org-find-exact-headline-in-buffer
                                      date (current-buffer) t)))
                    (if headlineh1
                        (progn
                          (goto-char headlineh1)
                          (forward-line 4))
                      ;; No "Year" headline, insert it.
                      (goto-char (point-max))
                      (insert "* Year\n"
                              ":PROPERTIES:\n:HTML_CONTAINER_CLASS: year\n"
                              ":END:\n"))
                    ;; At this point we are at headlineh1
                    (if (not headlineh2)
                        ;; No headline matching the current year, insert it.
                        (insert "\n** " date "\n" entry-list)
                      (goto-char headlineh2)
                      (forward-line 1)
                      (insert entry-list))))))))
      (save-buffer))
    (or visiting (kill-buffer blog-buffer))))

(defun org-blog-format-file-entry (fmt file link project-plist)
  (format-spec fmt
               `((?t . ,(org-publish-find-title file t))
                 (?d . ,(format-time-string org-publish-sitemap-date-format
                                            (org-publish-find-date file)))
                 (?a . ,(or (plist-get project-plist :author) user-full-name))
                 (?c . ,(org-blog-find-content-lines
                         file (or (plist-get project-plist
                                             :blog-content-lines) 5)))
                 (?l . ,(concat "file:" link))
                 (?k . ,(org-blog-find-keywords file)))))

(defun org-blog-find-keywords (file &optional reset)
  "Find the keywords of FILE in project"
  (or
   (and (not reset) (org-publish-cache-get-file-property file :keywords nil t)
        (let* ((visiting (find-buffer-visiting file))
               (buffer (or visiting (find-file-noselect file)))
               keywords)
          (with-current-buffer buffer
            (let* ((opt-plist (org-combine-plists (org-default-export-plist)
                                                  (org-infile-export-plist))))
              (setq keywords (plist-get opt-plist :keywords))))
          (unless visiting
            (kill-buffer buffer))
          (org-publish-cache-set-file-property file :keywords keywords)
          keywords))))

(defun org-blog-find-content-lines (file n)
  "Find and return the n first lines of FILE. Default is 5. It
   will discard lines starting with #, and it will demote
   outlines."
  (let ((visiting (find-buffer-visiting file))
        (lines (make-string 0 0)))
    (save-excursion
      (switch-to-buffer (or visiting (find-file-noselect file nil t)))
      (goto-char (point-min))

      (while (and (> n 0) (char-after))
        (beginning-of-line)
        (if (not (char-equal (char-after) ?#))
            (let ((line (buffer-substring (line-beginning-position)
                                          (line-end-position))))
              (setq n (1- n))
              (setq lines (concat lines
                                  (if (char-equal (string-to-char line) ?*)
                                      "*"
                                    "  ")
                                  line "\n"))))
        (forward-line 1))
      (unless visiting
        (kill-buffer (current-buffer)))
      lines)))

(provide 'org-blog)
