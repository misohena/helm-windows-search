;;; helm-locate-windows-search.el --- helm-locate for Windows Search  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  AKIYAMA Kouhei

;; Author: AKIYAMA Kouhei <misohena@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Override helm-locate-init function for Windows Search
;;
;; Usage:
;;
;;   (autoload 'helm-locate-windows-search-setup "helm-locate-windows-search")
;;   (with-eval-after-load "helm-locate" (helm-locate-windows-search-setup))
;;

;;; Code:


(defcustom helm-locate-adoquery-command nil
  "Path to adoquery.exe (See https://github.com/misohena/adoquery )"
  :type 'string :group 'helm-locate)

(defun helm-locate-set-adoquery-command ()
  "Setup `helm-locate-adoquery-command' if not already defined."
  (unless helm-locate-adoquery-command
    (let* ((el-path (or load-file-name
                        (locate-library "helm-locate-windows-search")
                        (if (and (boundp 'helm-windows-search-adoquery-command) helm-windows-search-adoquery-command) helm-windows-search-adoquery-command)))
           (filename "adoquery.exe")
           (exe-path (expand-file-name filename (file-name-directory el-path))))
      (setq helm-locate-adoquery-command
            (if (file-exists-p exe-path) exe-path filename)))))

(helm-locate-set-adoquery-command)


;; test: (helm-locate-parse-locate-command-line "-b -c -d AA -dBB --database CC --database=DD -n8 -l 8 --limit=8 word1 -n 8 word2")
(defun helm-locate-parse-locate-command-line (locate-cmdline)
  (let* ((args (helm-mm-split-pattern locate-cmdline))
         (options-with-argument '("d" "database" "n" "l" "limit")) ;;regexp?
         options words unresolved-opt)
    (loop for arg in args
          if unresolved-opt do (progn (push (cons unresolved-opt arg) options) (setq unresolved-opt nil))
          else if (string-match
                   "^-\\([a-zA-Z0-9]\\)\\(.+\\)?\\|^--\\([a-zA-Z0-9-]+\\)\\(=\\(.*\\)\\)?" arg)
            do (let ((opt-name (or (match-string 1 arg) (match-string 3 arg)))
                     (opt-value (or (match-string 2 arg) (match-string 5 arg))))
                 (if (and (member opt-name options-with-argument) (null opt-value))
                     (setq unresolved-opt opt-name)
                   (push (cons opt-name opt-value) options)))
          else do (push arg words))

    ;; Replace \s- to space in words
    (setq words
          (mapcar
           (lambda (word)
             (if (string-match-p "\\\\s-" word)
                 (format "\"%s\"" (replace-regexp-in-string "\\\\s-" " " word))
               word))
           words))
    ;; Return cons word and options
    (cons (nreverse words) (nreverse options))))


(defun helm-locate-make-windows-search-sql (locate-cmdline &optional scope)
  (let* ((words-options (helm-locate-parse-locate-command-line locate-cmdline))
         (words (car words-options))
         (options (cdr words-options))
         (basename-only (or (assoc "b" options) (assoc "basename" options)))
         (limit (string-to-number
                 (or (cdr (find-if (lambda (opt) (member (car opt) '("l" "limit" "n"))) options))
                     "100"))))
    ;; not supported:
    ;;  - -i : Windows Search always ignores case (https://docs.microsoft.com/en-us/windows/desktop/search/-search-sql-casesensitivityinsearches)
    (concat
     "SELECT"
     (format " TOP %s" limit)
     " System.ItemFolderPathDisplay, System.FileName"
     " FROM SystemIndex"
     " WHERE "
     (if scope (format "SCOPE='file:%s' AND " scope)) ;;https://docs.microsoft.com/en-us/windows/desktop/search/-search-sql-folderdepth
     (mapconcat (lambda (word)
                  ;; Escape single quote
                  (setq word (replace-regexp-in-string "'" "''" word))
                  ;; Exact match ?
                  (let ((pred (if (string-match "^\\\\\\(.+\\)" word)
                                  (format "= '%s'" (match-string 1 word))
                                (format "LIKE '%%%s%%'" word))))

                    (concat
                     (if (not basename-only)
                         (format "(System.ItemFolderPathDisplay %s) AND " pred))
                     (format "(System.FileName %s)" pred))))
                words " AND "))))

(defun helm-locate-make-windows-search-args (locate-cmdline &optional scope)
  (list
   "/conn" "Provider=Search.CollatorDSO;Extended Properties='Application=Windows';" ;;Connection String
   "/format" "%1%\\\\%2%" ;; directory/filename
   "/header" "" ;; no header
   "/query" (helm-locate-make-windows-search-sql locate-cmdline scope)))


(defun helm-locate-init-for-windows-search ()
  "Replacement of helm-locate-init function for Windows Search."

  (let* ((args (helm-locate-make-windows-search-args (replace-regexp-in-string "\\`[^ ]+ " "" (format helm-locate-command "" helm-pattern))))
         ;; cmd is used for debug only
         (cmd (concat helm-locate-adoquery-command " "
                      (mapconcat 'identity args " "))))

    (helm-log "Starting helm-locate process")
    (helm-log "Command line used was:\n\n%s"
              (concat ">>> " (propertize cmd 'face 'font-lock-comment-face) "\n\n"))
    (prog1
        (apply #'start-process
               (append (list "windows-search-process" helm-buffer helm-locate-adoquery-command)
                       args))

      ;; Same as helm-locate-init(helm-locate.el) function below
      (set-process-sentinel
       (get-buffer-process helm-buffer)
       (lambda (process event)
         (let* ((err (process-exit-status process))
                (noresult (= err 1)))
           (cond (noresult
                  (with-helm-buffer
                    (unless (cdr helm-sources)
                      (insert (concat "* Exit with code 1, no result found,"
                                      " command line was:\n\n "
                                      cmd)))))
                 ((string= event "finished\n")
                  (when (and helm-locate-fuzzy-match
                             (not (string-match-p "\\s-" helm-pattern)))
                    (helm-redisplay-buffer))
                  (with-helm-window
                    (setq mode-line-format
                          '(" " mode-line-buffer-identification " "
                            (:eval (format "L%s" (helm-candidate-number-at-point))) " "
                            (:eval (propertize
                                    (format "[Locate process finished - (%s results)]"
                                            (max (1- (count-lines
                                                      (point-min) (point-max)))
                                                 0))
                                    'face 'helm-locate-finish))))
                    (force-mode-line-update)))
                 (t
                  (helm-log "Error: Locate %s"
                            (replace-regexp-in-string "\n" "" event))))))))))


;;;###autoload
(defun helm-locate-windows-search-setup ()
  (setq helm-locate-command "locate %s %s")
  (fset 'helm-locate-init #'helm-locate-init-for-windows-search))
;;  ;;(advice-add #'helm-locate-init :override #'helm-locate-init-for-windows-search))


(provide 'helm-locate-windows-search)
;;; helm-locate-windows-search.el ends here
