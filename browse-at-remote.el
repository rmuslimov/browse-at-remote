;;; browse-at-remote.el --- Open page at github/bitbucket from emacs buffers

;; Copyright © 2015 Rustem Muslimov
;;
;; Author:     Rustem Muslimov <r.muslimov@gmail.com>
;; Version:    0.3.0
;; Keywords:   github, bitbucket, convenience
;; Package-Requires: ((f "0.17.2") (s "1.9.0"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Easily open target page on github (or bitbucket) from Emacs.
;; by calling `browse-at-remote` function. Support dired buffers and opens
;; them in tree mode at destination.

;;; Code:

(require 'f)
(require 's)

(defun browse-at-remote/parse-git-prefixed (origin)
  "Extract domain and slug for origin like git@..."
  (cdr (s-match "git@\\([a-z\.]+\\):\\([a-z\.\-]+/[a-z0-9\.\-]+\\).git" origin)))

(defun browse-at-remote/parse-https-prefixed (origin)
  "Extract domain and slug from origin like https://...."
  (let ((matches (s-match "https://\\(?:[a-z]+@\\)?\\([a-z0-9\.-]+\\)/\\([a-z0-9\-]+/[a-z0-9\.\-]+\\)" origin)))
    (list (nth 1 matches)
          (file-name-sans-extension (nth 2 matches)))))

(defun browse-at-remote/get-url-from-origin (origin)
  "Extract browseable repo url from origin definition"
  (let* ((parsed
          (cond
           ((s-starts-with? "git" origin) (browse-at-remote/parse-git-prefixed origin))
           ((s-starts-with? "https" origin) (browse-at-remote/parse-https-prefixed origin))))
         (domain (car parsed))
         (slug (nth 1 parsed)))
    (cons domain (format "https://%s/%s" domain slug))))

(defun browse-at-remote/get-origin ()
  "Get origin from current repo"
  (with-temp-buffer
    (vc-git--call t "config" "--get" "remote.origin.url")
    (s-replace "\n" "" (buffer-string))))

(defun browse-at-remote/format-as-github (repo-url location filename &optional linestart lineend)
  "URL formatted for github"
  (cond
   ((and linestart lineend)
    (format "%s/blob/%s/%s#L%d-%d" repo-url location filename linestart lineend))
   (linestart (format "%s/blob/%s/%s#L%d" repo-url location filename linestart))
   (t (format "%s/tree/%s/%s" repo-url location filename))))

(defun browse-at-remote/format-as-bitbucket (repo-url location filename &optional linestart lineend)
  "URL formatted for bitbucket"
  (cond
   (linestart (format "%s/src/%s/%s#cl-%d" repo-url location filename linestart))
   (t (format "%s/src/%s/%s" repo-url location filename))))

(defun browse-at-remote/view-particular-commit-at-github (commithash)
  "Open commit page at github"
  (let ((repo (cdr (browse-at-remote/get-url-from-origin (browse-at-remote/get-origin)))))
    (browse-url
     (cond
      ((s-prefix? "https://github.com" repo) (format "%s/commit/%s" repo commithash))
      ((s-prefix? "https://bitbucket.org" repo) (format "%s/commits/%s" repo commithash)))
     )))

(defun browse-at-remote-at-place (filename &optional start end)
  (let* ((branch (vc-git-working-revision filename))
         (relname (f-relative filename (f-expand (vc-git-root filename))))
         (target-repo (browse-at-remote/get-url-from-origin (browse-at-remote/get-origin)))
         (domain (car target-repo))
         (repo-url (cdr target-repo))
         (url-format
          (pcase domain
            (`"bitbucket.org" `browse-at-remote/format-as-bitbucket)
            (`"github.com" `browse-at-remote/format-as-github))))

    (browse-url (funcall url-format repo-url branch relname
                         (if start (line-number-at-pos start))
                         (if end (line-number-at-pos end)))
                )))

;;;###autoload
(defun browse-at-remote()
  "Main function for interactive calls"
  (interactive)
  (cond
   ;; dired-mode
   ((eq major-mode 'dired-mode) (browse-at-remote-at-place (dired-current-directory)))

   ;; magit-log-mode
   ((or (eq major-mode 'magit-log-mode) (eq major-mode 'vc-annotate-mode))
    (browse-at-remote/view-particular-commit-at-github
     (save-excursion
       (beginning-of-line)
       (search-forward " ")
       (buffer-substring-no-properties (line-beginning-position) (- (point) 1)))))

   ;; magit-commit-mode
   ((eq major-mode 'magit-commit-mode)
    (save-excursion
      (beginning-of-buffer)
      (let* ((first-line
              (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
             (commithash (car (s-split " " first-line)))
           )
        (browse-at-remote/view-particular-commit-at-github commithash))
      ))

   ;; now assume that we're inside of file-attached buffer
   ((not (use-region-p)) (browse-at-remote-at-place (buffer-file-name) (point)))
   ((let ((point-begin (min (region-beginning) (region-end)))
          (point-end (max (region-beginning) (region-end))))
      (browse-at-remote-at-place
       (buffer-file-name) point-begin
       (if (eq (char-before point-end) ?\n) (- point-end 1) point-end))
      ))))

(provide 'browse-at-remote)

;;; browse-at-remote.el ends here
