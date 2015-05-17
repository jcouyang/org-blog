;;; org-blog.el --- Blog like sitemap for org-publish

;; Version: 20150318.1637
;; X-Original-Version: 0.1
;; URL: git@github.com:jcouyang/org-blog.git

;;; Commentary:

;; This program contains a function 'org-blog-export' which can be
;; used by 'org-publish.el' instead of org-publish-org-sitemap. When
;; used, it will use variable 'org-blog-entry-format' to format an
;; entry in the sitemap file. The sitemap file may then be used as an
;; index to the blog.

;;; Code:
(require 'dash)
(defcustom org-blog-export-keywords nil
  "Set to 't' to export a list of blog entries for each keyword"
  :group 'org-blog
  :type 'boolean)

(defcustom org-blog-export-dates nil
  "Set to 't' to export a list of blog entries for each year"
  :group 'org-blog
  :type 'boolean)


(defcustom org-blog-entry-format "* [[%l][%t]]

=by= /%a/ - =%d=

%c...

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
         (blog-archive (plist-get project-plist :blog-archive))
         (exclude-regexp (plist-get project-plist :exclude))
         
         (blog-filename (concat dir (or blog-filename "index.org")))
         (index-filename (concat dir "theindex.org"))
         (archive-filename (concat dir (or blog-archive "archive.org")))
         (blog-title (or (plist-get project-plist :blog-title)
                         (concat "Blog " (car project))))
         (blog-entry-format (or (plist-get project-plist
                                           :blog-entry-format)
                                org-blog-entry-format))
         (blog-encoding (or (plist-get project-plist :blog-encoding)
                            'utf-8))
         (visiting (find-buffer-visiting blog-filename))
         (blog-insert-first (plist-get project-plist :blog-insert-first))
         (export-dates (or (plist-get project-plist :blog-export-dates)
                           org-blog-export-dates)))

    (let ((files (sort (-remove (lambda (file)
                                  (--any? (equal it file)
                                          (list (file-truename blog-filename)
                                                (file-truename archive-filename)
                                                (file-truename index-filename))))
                                (org-publish-get-base-files project exclude-regexp))
                       'org-compare-files-timestamp)))
      (with-current-buffer (setq archive-buffer
                                 (or visiting (find-file archive-filename)))
        
        (erase-buffer)
        (setq save-buffer-coding-system blog-encoding)
        (insert (concat "#+TITLE: " blog-title "\n\n"))
        (insert "#+OPTIONS: toc:nil\n")
        (if blog-insert-first
            (insert blog-insert-first))
        (mapc
         (lambda (file)
           (let* ((link (file-relative-name file dir))
                  (entry (org-blog-format-file-entry
                          blog-entry-format
                          file link project-plist))
                  (entry-list (org-blog-format-file-entry
                               "  + [[%l][%t]]\n" file link
                               project-plist)))
             (save-excursion
               (let* ((date (format-time-string "%b %Y" (org-publish-find-date
                                                         file)))
                      (headlineh1 (org-find-exact-headline-in-buffer
                                   "Archive"  (current-buffer) t))
                      (headlineh2 (org-find-exact-headline-in-buffer
                                   date (current-buffer) t)))
                 (if headlineh1
                     (progn
                       (goto-char headlineh1)
                       (goto-char (point-max)))
                   ;; No "Year" headline, insert it.
                   (insert "* Archive\n"
                           ":PROPERTIES:\n:HTML_CONTAINER_CLASS: year\n"
                           ":END:\n"))
                 ;; At this point we are at headlineh1
                 (if (not headlineh2)
                     ;; No headline matching the current year, insert it.
                     (insert "\n** " date "\n" entry-list)
                   (goto-char headlineh2)
                   (forward-line 1)
                   (insert entry-list))))))
         files)
        (save-buffer))
      (with-current-buffer (setq blog-buffer
                                 (or visiting (find-file blog-filename)))
        
        (erase-buffer)
        (setq save-buffer-coding-system blog-encoding)
        (insert (concat "#+TITLE: " blog-title "\n\n"))
        (insert "#+OPTIONS: toc:nil\n")
        (if blog-insert-first
            (insert blog-insert-first))
        (mapc
         (lambda (file)
           (let* ((link (file-relative-name file dir))
                  (entry (org-blog-format-file-entry
                          blog-entry-format
                          file link project-plist))
                  (entry-list (org-blog-format-file-entry
                               "  + [[%l][%t]]\n" file link
                               project-plist))
                  )
             (insert entry)))
         files)
        (save-buffer))
      (or visiting (kill-buffer blog-buffer))
      (or visiting (kill-buffer archive-buffer)))
    ))
(defun org-compare-files-timestamp (a b)
  (time-less-p (org-publish-find-date b) (org-publish-find-date a))
  )
(defun org-blog-format-file-entry (fmt file link project-plist)
  (let* ((hl-home (file-name-as-directory (plist-get project-plist :html-link-home))))
    (format-spec fmt
                      `((?t . ,(org-publish-find-title file t))
                        (?d . ,(format-time-string org-publish-sitemap-date-format
                                                   (org-publish-find-date file)))
                        (?D . ,(format-time-string "<%Y-%m-%d %a>" (org-publish-find-date file)))
                        (?a . ,(or (plist-get project-plist :author) user-full-name))
                        (?c . ,(org-blog-find-content-lines
                                file (or (plist-get project-plist
                                                    :blog-content-lines) 5)))
                        (?l . ,(concat "file:" link))
                        (?L . ,(replace-regexp-in-string "\.org" "\.html" link))
                        (?p . ,(org-blog-find-description file t))))
    ))

(defun org-blog-find-description (file &optional reset)
  "Find the title of FILE in project."
  (or
   (and (not reset) (org-publish-cache-get-file-property file :description nil t))
   (let* ((org-inhibit-startup t)
          (visiting (find-buffer-visiting file))
          (buffer (or visiting (find-file-noselect file))))
     (with-current-buffer buffer
       (let ((title
              (let ((property
                     (plist-get
                      ;; protect local variables in open buffers
                      (if visiting
                          (org-export-with-buffer-copy (org-export-get-environment))
                        (org-export-get-environment))
                      :description)))
                (if property
                    (org-no-properties (org-element-interpret-data property))
                  ""))))
                (unless visiting (kill-buffer buffer))
                (org-publish-cache-set-file-property file :description title)
                title)))))


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

;;; org-blog.el ends here
