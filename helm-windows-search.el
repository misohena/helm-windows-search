;;; helm-windows-search.el ---                     -*- lexical-binding: t; -*-
;;; helm interface for windows search.

;; Copyright (C) 2018  AKIYAMA Kouhei <misohena@gmail.com>

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

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'helm-types)
(require 'helm-help)


(defgroup helm-windows-search nil
  "Windows Search for Helm."
  :group 'helm)

(defcustom helm-windows-search-adoquery-command nil
  "Path to adoquery.exe (See https://github.com/misohena/adoquery )"
  :type 'string :group 'helm-windows-search)

(defun helm-windows-search-set-command ()
  "Setup `helm-windows-search-adoquery-command' if not already defined."
  (unless helm-windows-search-adoquery-command
    (let* ((el-path (or load-file-name (locate-library "helm-windows-search")))
           (filename "adoquery.exe")
           (exe-path (expand-file-name filename (file-name-directory el-path))))
      (setq helm-windows-search-adoquery-command
            (if (file-exists-p exe-path) exe-path filename)))))
(helm-windows-search-set-command)


(defvar helm-windows-search-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-generic-files-map)
    (define-key map (kbd "DEL") 'helm-delete-backward-no-update)
    map))


(defface helm-windows-search-finish
    '((t (:foreground "Green")))
  "Face used in mode line when windows-search process is finish."
  :group 'helm-windows-search)



;;;
;;; Query
;;;

;; tokenize pattern string
;;
;; " aaa  bbb  ccc " => ("aaa" "bbb" "ccc")
;; " \"aaa bbb\" " => ("\"aaa bbb\"")
;; " \"aaa bbb"    => ("\"aaa bbb\"")  ;;EndOfString => "
;;
;; Escape white space:
;; "aaa\\ bbb ccc" => ("aaa bbb" "ccc") ;;escape space in word
;; "\"aaa\\ bbb\" ccc" => ("\"aaa bbb\"" "ccc") ;;escape space in phrase
;; "\"aaa bbb\" ccc"   => ("\"aaa bbb\"" "ccc")
;;
;; Escape double quotation mark:
;; " aaa \\\"bbb ccc" => ("aaa" "\"bbb" "ccc")
;; " \"aaa \\\"bbb\" ccc" => ("\"aaa \"bbb\"" "ccc") ;;@todo
;;
;; WHITESPACE ::=   | \f | \t | \n | \r | \v
;; ESCAPESEQ ::= \ ( " | WHITESPACE )
;; quoted ::= " ( ESCAPESEQ | \ | [^\"] )* ( " | EOS )
;; word ::= ( ESCAPESEQ | \ | [^\ WHITESPACE] )*
;; spaces ::= WHITESPACE*
;; pattern ::= spaces ((quoted | word) spaces)*
;;
(defun helm-windows-search-split-pattern (pattern)
  (let* ((pos 0) (len (length pattern))
         (state 'spaces)
         char-list
         args
         (start (lambda (next-state) (setq state next-state)))
         (add (lambda (char) (push char char-list)))
         (end (lambda () (push (apply #'string (nreverse char-list)) args) (setq char-list nil) (setq state 'spaces)))
         curr-char next-char)
    (while (< pos len)
      (setq curr-char (elt pattern pos))
      (setq next-char (if (< (1+ pos) len ) (elt pattern (1+ pos))))

      (case state
        ;; Skip white spaces
        (spaces
         (case curr-char
           ((?  ?\f ?\t ?\n ?\r ?\v) (incf pos 1))
           (?\" (funcall start 'quoted) (funcall add curr-char) (incf pos 1))
           (otherwise (funcall start 'word))))

        ;; Quoted phrase
        (quoted
         (case curr-char
           (?\" (funcall add curr-char) (incf pos 1) (funcall end))
           (?\\
            (case next-char
              ((?\" ?  ?\f ?\t ?\n ?\r ?\v) (funcall add next-char) (incf pos 2))
              (otherwise (funcall add curr-char) (incf pos 1))))
           (otherwise (funcall add curr-char) (incf pos 1))))

        ;; Word
        (word
         (case curr-char
           ((?  ?\f ?\t ?\n ?\r ?\v) (funcall end))
           (?\\
            (case next-char
              ((?\" ?  ?\f ?\t ?\n ?\r ?\v) (funcall add next-char) (incf pos 2))
              (otherwise (funcall add curr-char) (incf pos 1))))
           (otherwise (funcall add curr-char) (incf pos 1))))))

    ;; End Of String
    (case state
      (quoted (funcall add ?\") (funcall end))
      (word (funcall end)))

    (nreverse args)))

;; Query Date

(defun helm-windows-search-make-date-range (dayname year month day)
"Calculate lower & upper bound of date.

ex:
 (nil 2018 10 19) => ((enc 2018 10 19) . (enc 2018 10 20))
 (nil 2018 10 nil) => ((enc 2018 10 1) . (enc 2018 11 1))
 (nil 2018 nil nil) => ((enc 2018 1 1) . (enc 2019 1 1))

 *(enc y m d) = (encode-time 0 0 0 d m y)"
  (if (stringp year) (setq year (string-to-number year)))
  (if (stringp month) (setq month (string-to-number month)))
  (if (stringp day) (setq day (string-to-number day)))

  (if (and (stringp dayname) (string= dayname "today"))
      (let ((now (decode-time (current-time))))
        (setq year (nth 5 now))
        (setq month (nth 4 now))
        (setq day (nth 3 now))))

  (let ((range
         (cond
          (day (list year month day  year month (1+ day)))
          (month (list year month 1  year (1+ month) 1))
          (year (list year 1 1  (1+ year) 1 1)))))
    (if range
        ;; convert to time & normalize date (2018/13/32 => (23635 3440))
        (cons
         (encode-time 0 0 0 (nth 2 range) (nth 1 range) (nth 0 range))
         (encode-time 0 0 0 (nth 5 range) (nth 4 range) (nth 3 range))))))

(defun helm-windows-search-match-string (n str matched-data)
  (let ((from (nth (* n 2) matched-data))
        (to (nth (1+ (* n 2)) matched-data)))
    (if (and from to) (substring str from to))))

(defvar helm-windows-search-query-date-regexp
  (let* ((date-regexp "\\(\\(today\\)\\|\\(\\([0-9][0-9][0-9][0-9]\\)\\([/-]\\([0-9][0-9]?\\)\\([/-]\\([0-9][0-9]?\\)\\)?\\)?\\)\\)")
         (expr-regexp
          (format "\\(\\(\\(<\\|<=\\|=\\|>\\|>=\\)?%s\\)\\|\\(%s\\.\\.%s\\)\\) *$"
                  date-regexp date-regexp date-regexp)))
    (concat "^date:" expr-regexp)))

(defun helm-windows-search-parse-date (word &optional matched-data)
  "Parse date query string.

ex:
 date:today
 date:2018-12-19    => ((\">=\" (enc 2018 12 19)) (\"<\" (enc 2018 12 20)))
 date:2018-12       => ((\">=\" (enc 2018 12 1)) (\"<\" (enc 2019 1 1)))
 date:2018          => ((\">=\" (enc 2018 1 1)) (\"<\" (enc 2019 1 1)))
 date:>=2018-12-19  => ((\">=\" (enc 2018 12 19)))
 date:>2018-12-19   => ((\">=\" (enc 2018 12 20)))
 date:<today
 date:2018-11-3..2019-12-23  => ((\">=\" (enc 2018 11 3)) (\"<\" (enc 2019 12 24)))
 date:2018-11..2019          => ((\">=\" (enc 2018 11 1)) (\"<\" (enc 2020 1 1)))
 date:2018-11..today

 * (enc y m d) = (encode-time 0 0 0 d m y)"
  (if (null matched-data)
      (setq matched-data (if (string-match helm-windows-search-query-date-regexp word) (match-data))))

  (if matched-data
      (let ((unary-op (helm-windows-search-match-string 2 word matched-data))
            (op (helm-windows-search-match-string 3 word matched-data)) ;; < <= = > >= or nil
            (op-dayname (helm-windows-search-match-string 5 word matched-data))
            (op-year (helm-windows-search-match-string 7 word matched-data))
            (op-month (helm-windows-search-match-string 9 word matched-data))
            (op-day (helm-windows-search-match-string 11 word matched-data))
            (binary-op (helm-windows-search-match-string 12 word matched-data))
            (start-dayname (helm-windows-search-match-string 14 word matched-data))
            (start-year (helm-windows-search-match-string 16 word matched-data))
            (start-month (helm-windows-search-match-string 18 word matched-data))
            (start-day (helm-windows-search-match-string 20 word matched-data))
            (end-dayname (helm-windows-search-match-string 22 word matched-data))
            (end-year (helm-windows-search-match-string 24 word matched-data))
            (end-month (helm-windows-search-match-string 26 word matched-data))
            (end-day (helm-windows-search-match-string 28 word matched-data)))

        (cond
         (unary-op
          (let ((date-range (helm-windows-search-make-date-range op-dayname op-year op-month op-day)))
            (cond
             ((or (null op) (string= op "="))
              (list (cons ">=" (car date-range)) (cons "<" (cdr date-range))))
             ((member op '("<" ">="))
              (list (cons op (car date-range))))
             ((member op '(">" "<="))
              (list (cons op (cdr date-range)))))))
         (binary-op
          (let ((start-date-range (helm-windows-search-make-date-range start-dayname start-year start-month start-day))
                (end-date-range (helm-windows-search-make-date-range end-dayname end-year end-month end-day)))
            (list (cons ">=" (car start-date-range)) (cons "<" (cdr end-date-range)))))))))

;;

(defun helm-windows-search-escape-single-quote (value)
  ;; https://docs.microsoft.com/en-us/windows/desktop/search/-search-sql-literals
  (replace-regexp-in-string "'" "''" value nil t))

(defun helm-windows-search-escape-like (value)
  ;; https://docs.microsoft.com/en-us/windows/desktop/search/-search-sql-like
  (replace-regexp-in-string "[%_[]" "[\\&]" value))

(defvar helm-windows-search-query-sql-map
  `(
    ;; https://docs.microsoft.com/ja-jp/windows/desktop/lwef/-search-2x-wds-aqsreference#properties-by-file-kind
    ;; ( regexp sql-generator )
    ("^\\(filename\\|file\\):\\(.+\\)" (lambda (word) (format "(System.FileName Like '%%%s%%')" (helm-windows-search-escape-single-quote (match-string 2 word)))))
    ("^\\(fileext\\|ext\\):\\.?\\(.+\\)" (lambda (word) (format "(System.FileExtension = '.%s')" (helm-windows-search-escape-single-quote (match-string 2 word)))))
    ("^kind:\\(.+\\)" (lambda (word) (format "(System.Kind = '%s')" (helm-windows-search-escape-single-quote (match-string 1 word))))) ;;https://docs.microsoft.com/ja-jp/windows/desktop/properties/props-system-kind
    ("^author:\\(.+\\)" (lambda (word) (format "(System.Author = '%s')" (helm-windows-search-escape-single-quote (match-string 1 word)))))
    ("^title:\\(.+\\)" (lambda (word) (format "(System.Title Like '%%%s%%')" (helm-windows-search-escape-single-quote (match-string 1 word)))))
    ("^contents:\\(.+\\)" (lambda (word) (format "FREETEXT('%s')" (helm-windows-search-escape-single-quote (match-string 1 word)))))
    ("^size:\\(<\\|<=\\|=\\|>\\|>=\\|!=\\)?\\([0-9]+\\)" (lambda (word) (format "(System.Size %s %s)" (or (match-string 1 word) "=") (match-string 2 word))))
    (,helm-windows-search-query-date-regexp
     (lambda (word)
       (let ((date-conds (helm-windows-search-parse-date word (match-data))))
         (if date-conds
             (mapconcat (lambda (op-date) (format "(System.DateModified %s '%s')"
                                                  (car op-date)
                                                  (format-time-string "%Y-%m-%d %T" (cdr op-date) "UTC0")))
                        date-conds
                        " AND ")))))
    (t (lambda (word)
         (concat
          "("
          "(System.FileName Like '%" (helm-windows-search-escape-single-quote (helm-windows-search-escape-like word)) "%')"
          " OR "
          "(System.ItemFolderPathDisplay Like '%" (helm-windows-search-escape-single-quote (helm-windows-search-escape-like word)) "%')"
          " OR "
          "FREETEXT('" (helm-windows-search-escape-single-quote word) "')"
          ")")))))



(defun helm-windows-search-make-sql (pattern &optional scope)
  ;; https://docs.microsoft.com/en-us/windows/desktop/search/-search-sql-windowssearch-entry
  (let* ((words (helm-windows-search-split-pattern pattern))
         (where
          (concat
           (if scope (format "SCOPE='file:%s' AND " scope)) ;;https://docs.microsoft.com/en-us/windows/desktop/search/-search-sql-folderdepth
           (mapconcat
            (lambda (word)
              (loop for query-type in helm-windows-search-query-sql-map
                    do (let ((regexp (nth 0 query-type))
                             (generator (nth 1 query-type)))
                         (if (or (eq regexp t)
                                 (string-match regexp word))
                             (return (funcall generator word))))))
            words " AND ")
           )))

    (concat
     "SELECT"
     " TOP 100"
     " System.ItemFolderPathDisplay, System.FileName" ;;%1% %2%
     " FROM SystemIndex"
     " WHERE " where)))



;;;
;;; Execute
;;;

(defun helm-windows-search-make-command-args (pattern &optional scope)
  (list
   "/conn" "Provider=Search.CollatorDSO;Extended Properties='Application=Windows';" ;;Connection String
   "/format" "%1%\\\\%2%" ;; directory/filename
   "/header" "" ;; no header
   "/query" (helm-windows-search-make-sql pattern scope)))

(defun helm-windows-search-source-init ()
  (require 'helm-for-files))

(defun helm-windows-search-candidates-process ()
  "Initialize async Windows Search process for `helm-source-windows-search'."
  (let* ((args (helm-windows-search-make-command-args helm-pattern))
         (cmd (concat helm-windows-search-adoquery-command " "
                      (mapconcat 'identity args " "))))
    (helm-log "Starting helm-windows-search process")
    (helm-log "Command line used was:\n\n%s"
              (concat ">>> " (propertize cmd 'face 'font-lock-comment-face) "\n\n"))
    (prog1
        (apply #'start-process
               (append (list "windows-search-process" helm-buffer helm-windows-search-adoquery-command)
                       args))
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
                  (with-helm-window
                    (setq mode-line-format
                          '(" " mode-line-buffer-identification " "
                            (:eval (format "L%s" (helm-candidate-number-at-point))) " "
                            (:eval (propertize
                                    (format "[Windows Search process finished - (%s results)]"
                                            (max (1- (count-lines
                                                      (point-min) (point-max)))
                                                 0))
                                    'face 'helm-windows-search-finish))))
                    (force-mode-line-update)))
                 (t
                  (helm-log "Error: windows-search %s"
                            (replace-regexp-in-string "\n" "" event))))))))))

(defvar helm-windows-search-file-name-history nil)

(defclass helm-windows-search-override-inheritor (helm-type-file) ())

(defclass helm-windows-search-source (helm-source-async helm-windows-search-override-inheritor)
  ((init :initform 'helm-windows-search-source-init)
   (candidates-process :initform 'helm-windows-search-candidates-process)
   (requires-pattern :initform 3)
   (history :initform 'helm-windows-search-file-name-history)
   (persistent-action :initform 'helm-ff-kill-or-find-buffer-fname)
   (candidate-number-limit :initform 9999)
   (group :initform 'helm-windows-search)))

;; Override helm-type-file class keymap.
(defmethod helm--setup-source :after ((source helm-windows-search-override-inheritor))
  (setf (slot-value source 'keymap) helm-windows-search-map))

(defvar helm-source-windows-search
  (helm-make-source "Windows Search" 'helm-windows-search-source))


(defun helm-windows-search-1 (&optional initial-input default)
  "Run Windows Search."
  (require 'helm-mode)
  ;;(setq helm-windows-search-file-name-history (mapcar 'helm-basename file-name-history))
  (helm :sources 'helm-source-windows-search
        :buffer "*helm windows-search*"
        :ff-transformer-show-only-basename nil
        :input initial-input
        :default default
        :history 'helm-windows-search-file-name-history))

;;;###autoload
(defun helm-windows-search ()
  "Preconfigured `helm' for Windows Search."
  (interactive)
  (setq helm-ff-default-directory default-directory)
  (helm-windows-search-1 nil (thing-at-point 'filename)))

(provide 'helm-windows-search)
;;; helm-windows-search.el ends here
