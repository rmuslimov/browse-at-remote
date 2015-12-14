;;; browse-at-remote.el --- Open github/gitlab/bitbucket page from Emacs -*- lexical-binding:t -*-

;; Copyright © 2015 Rustem Muslimov
;;
;; Author:     Rustem Muslimov <r.muslimov@gmail.com>
;; Version:    0.7.0
;; Keywords:   github, gitlab, bitbucket, convenience
;; Package-Requires: ((f "0.17.2") (s "1.9.0") (cl-lib "0.5"))

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

;; Easily open target page on github/gitlab (or bitbucket) from Emacs.
;; by calling `browse-at-remote` function. Support dired buffers and opens
;; them in tree mode at destination.

;;; Code:

(require 'f)
(require 's)
(require 'cl-lib)
(require 'vc-git)

(defgroup browse-at-remote nil
  "Open target on github/gitlab/bitbucket"
  :prefix "browse-at-remote/"
  :group 'applications)

(defcustom browse-at-remote/remote-type-domains
  '(("bitbucket.org" ."bitbucket")
    ("github.com" . "github")
    ("gitlab.com" . "gitlab"))
  "Alist of domain patterns to remote types.

   For remote URLs matching these domain patterns, "
  :type '(alist :key-type (string :tag "Domain")
                :value-type (choice
                             (const :tag "GitHub" "github")
                             (const :tag "GitLab" "gitlab")
                             (const :tag "BitBucket" "bitbucket")))
  :group 'browse-at-remote)

(defun browse-at-remote/parse-git-prefixed (origin)
  "Extract domain and slug from ORIGIN like git@..."
  (cdr (s-match "git@\\([a-z.]+\\):\\([a-z0-9_.-]+/[a-z0-9_.-]+?\\)\\(?:\.git\\)?$" origin)))

(defun browse-at-remote/parse-https-prefixed (origin)
  "Extract domain and slug from ORIGIN like https://.... or http://...."
  (let ((matches (s-match "https?://\\(?:[a-z]+@\\)?\\([a-z0-9.-]+\\)/\\([a-z0-9_-]+/[a-z0-9_.-]+\\)" origin)))
    (list (nth 1 matches)
          (file-name-sans-extension (nth 2 matches)))))

(defun browse-at-remote/get-url-from-origin (origin)
  "Extract browseable repo url from ORIGIN."
  (let* ((parsed
          (cond
           ((s-starts-with? "git" origin) (browse-at-remote/parse-git-prefixed origin))
           ((s-starts-with? "http" origin) (browse-at-remote/parse-https-prefixed origin))))
         (proto
          (if (s-starts-with? "http:" origin) "http" "https"))
         (domain (car parsed))
         (slug (nth 1 parsed)))
    (cons domain (format "%s://%s/%s" proto domain slug))))

(defun browse-at-remote/get-origin ()
  "Get origin from current repo.

   Looks for a remote named \"origin\"; if this doesn't exist, returns
   the first remote from the list of all known remotes."
  (let ((remotes (browse-at-remote/get-remotes)))
    (browse-at-remote/get-remote-url (if (memq "origin" remotes)
        "origin"
      (car remotes)))))

(defun browse-at-remote/get-remote-url (remote)
  "Get URL of REMOTE from current repo."
  (with-temp-buffer
    (vc-git--call t "ls-remote" "--get-url" remote)
    (s-replace "\n" "" (buffer-string))))

(defun browse-at-remote/get-remotes ()
  "Get a list of known remotes."
  (with-temp-buffer
    (vc-git--call t "remote")
    (s-split "\\W+" (s-trim (buffer-string)))))

(defun browse-at-remote/get-remote-type-from-config ()
  "Get remote type from current repo."
  (with-temp-buffer
    (vc-git--call t "config" "--get" "browseAtRemote.type")
    (s-replace "\n" "" (buffer-string))))

(defun browse-at-remote/get-remote-type (target-repo)
  (or
   (let* ((domain (car target-repo))
         (remote-type-from-config (browse-at-remote/get-remote-type-from-config)))
    (if (member remote-type-from-config '("github" "bitbucket" "gitlab"))
        remote-type-from-config
      (cl-loop for pt in browse-at-remote/remote-type-domains
               when (string= (car pt) domain)
               return (cdr pt))))

   (error (format "Sorry, not sure what to do with repo `%s'" target-repo))))

(defun browse-at-remote/get-formatter (formatter-type remote-type)
  "Get formatter function for given FORMATTER-TYPE (region-url or commit-url) and REMOTE-TYPE (github or bitbucket)"
  (let ((formatter (intern (format "browse-at-remote/format-%s-as-%s" formatter-type remote-type))))
    (if (fboundp formatter)
        formatter
      nil)))

(defun browse-at-remote/format-region-url-as-github (repo-url location filename &optional linestart lineend)
  "URL formatted for github."
  (cond
   ((and linestart lineend)
    (format "%s/blob/%s/%s#L%d-L%d" repo-url location filename linestart lineend))
   (linestart (format "%s/blob/%s/%s#L%d" repo-url location filename linestart))
   (t (format "%s/tree/%s/%s" repo-url location filename))))

(defun browse-at-remote/format-commit-url-as-github (repo-url commithash)
  "Commit URL formatted for github"
  (format "%s/commit/%s" repo-url commithash))

(defun browse-at-remote/format-region-url-as-bitbucket (repo-url location filename &optional linestart _lineend)
  "URL formatted for bitbucket"
  (cond
   (linestart (format "%s/src/%s/%s#cl-%d" repo-url location filename linestart))
   (t (format "%s/src/%s/%s" repo-url location filename))))

(defun browse-at-remote/format-commit-url-as-bitbucket (repo-url commithash)
  "Commit URL formatted for bitbucket"
  (format "%s/commits/%s" repo-url commithash))

(defun browse-at-remote/format-region-url-as-gitlab (repo-url location filename &optional linestart lineend)
  "URL formatted for gitlab.
The only difference from github is format of region: L1-2 instead of L1-L2"
  (cond
   ((and linestart lineend)
    (format "%s/blob/%s/%s#L%d-%d" repo-url location filename linestart lineend))
   (linestart (format "%s/blob/%s/%s#L%d" repo-url location filename linestart))
   (t (format "%s/tree/%s/%s" repo-url location filename))))

(defun browse-at-remote/format-commit-url-as-gitlab (repo-url commithash)
  "Commit URL formatted for gitlab.
Currently the same as for github."
  (format "%s/commit/%s" repo-url commithash))

(defun browse-at-remote/commit-url (commithash)
  "Return the URL to browse COMMITHASH."
  (let* ((target-repo (browse-at-remote/get-url-from-origin (browse-at-remote/get-origin)))
         (repo-url (cdr target-repo))
         (remote-type (browse-at-remote/get-remote-type target-repo))
         (clear-commithash (s-chop-prefixes '("^") commithash))
         (url-formatter (browse-at-remote/get-formatter 'commit-url remote-type)))
    (unless url-formatter
      (error (format "Origin repo parsing failed: %s" repo-url)))
    (funcall url-formatter repo-url clear-commithash)))

(defun browse-at-remote/file-url (filename &optional start end)
  "Return the URL to browse FILENAME from lines START to END. "
  (let* ((branch (vc-git-working-revision filename))
         (relname (f-relative filename (f-expand (vc-git-root filename))))
         (target-repo (browse-at-remote/get-url-from-origin (browse-at-remote/get-origin)))
         (remote-type (browse-at-remote/get-remote-type target-repo))
         (repo-url (cdr target-repo))
         (url-formatter (browse-at-remote/get-formatter 'region-url remote-type))
         (start-line (when start (line-number-at-pos start)))
         (end-line (when end (line-number-at-pos end))))
    (unless url-formatter
      (error (format "Origin repo parsing failed: %s" repo-url)))

    (funcall url-formatter repo-url branch relname
             (if start-line start-line)
             (if (and end-line (not (equal start-line end-line))) end-line))))

(defun browse-at-remote/get-url ()
  "Main method, returns URL to browse."

  (cond
   ;; dired-mode
   ((eq major-mode 'dired-mode)
    (browse-at-remote/file-url (dired-current-directory)))

   ;; magit-status-mode
   ((eq major-mode 'magit-status-mode)
    (browse-at-remote/file-url default-directory))

   ;; magit-log-mode
   ((or (eq major-mode 'magit-log-mode) (eq major-mode 'vc-annotate-mode))
    (browse-at-remote/commit-url
     (save-excursion
       (save-restriction
         (widen)
         (goto-char (point-min))
         (search-forward " ")
         (buffer-substring-no-properties (line-beginning-position) (- (point) 1))))))

   ;; magit-commit-mode
   ((eq major-mode 'magit-commit-mode)
    (save-excursion
      (goto-char (point-min))
      (let* ((first-line
              (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
             (commithash (car (s-split " " first-line))))
        (browse-at-remote/commit-url commithash))))

   ;; We're inside of file-attached buffer with active region
   ((and buffer-file-name (use-region-p))
    (let ((point-begin (min (region-beginning) (region-end)))
          (point-end (max (region-beginning) (region-end))))
      (browse-at-remote/file-url
       buffer-file-name point-begin
       (if (eq (char-before point-end) ?\n) (- point-end 1) point-end))))

   ;; We're inside of file-attached buffer without region
   (buffer-file-name
    (browse-at-remote/file-url (buffer-file-name) (point)))

   (t (error "Sorry, I'm not sure what to do with this."))))

;;;###autoload
(defun browse-at-remote/browse ()
  "Browse the current file with `browse-url'."
  (interactive)
  (browse-url (browse-at-remote/get-url)))

;;;###autoload
(defun browse-at-remote/kill ()
  "Add the URL of the current file to the kill ring.

Works like `browse-at-remote/browse', but puts the address in the
kill ring instead of opening it with `browse-url'."
  (interactive)
  (kill-new (browse-at-remote/get-url)))

;;;###autoload
(defalias 'browse-at-remote 'browse-at-remote/browse
  "Browse the current file with `browse-url'.")
(make-obsolete 'browse-at-remote 'browse-at-remote/browse "0.7.0")

;;;###autoload
(defalias 'browse-at-remote/to-clipboard 'browse-at-remote/kill
  "Add the URL of the current file to the kill ring.

Works like `browse-at-remote/browse', but puts the address in the
kill ring instead of opening it with `browse-url'.")

(make-obsolete 'browse-at-remote/to-clipboard 'browse-at-remote/kill "0.7.0")

(provide 'browse-at-remote)

;;; browse-at-remote.el ends here
