; -*- lexical-binding: t -*-



(defgroup intef nil
  "Tag computer files."
  :group 'tools)

(defvar intef-buffer nil
  "Intef buffer, if any.")

(defcustom intef-database-filename "/home/lvds/Projects/personal/file-tagging/test.lisp"
  "Filename of database currently open.
`nil' if no database is open."
  :type '(file)
  :group 'intef)

(defvar intef-database nil
  "Database  currently open in hash-table format.
`nil' if no database is open.")

(defvar intef-marked-rows nil
  "Set of marked rows in Intef buffer")

;; database operations


(defun intef--read-database-file ()
  "Returns hash-table generated from alist in`intef-database-filename'.

The database file should be a lisp data file containing an alist from
filenames to alists.  The latter alists are mappings from tags to
values.  Filenames will be the hash-table keys and the alists will be
values.

A filename contains one or more tags, and each tag associated to a
filename contains zero or more values.  Filenames, tags and values are
all strings.

`intef-database' value is set to be the database."
  (or
   intef-database
   (setq intef-database
         (with-temp-buffer
           (let ((table (make-hash-table :test 'equal)))
             (cl-loop for (filename . assocs) in
                      (condition-case nil
                          (read (prog1 (point-marker)
                                  (insert-file-contents intef-database-filename)))
                        (end-of-file nil))
                      do (puthash filename assocs table))
             table)))))

(defun intef--write-database-file ()
  "Writes `intef-database' to `intef-database-filename' in alist format.
Each key of `intef-database' is a filename and its respective value is
an alist from tag to values. For more information on
`intef-database-filename's format read documentation of
`intef--read-database-file'."
  (if intef-database
      (with-temp-file intef-database-filename
        (let ((buf (current-buffer)))
          (erase-buffer)
          (insert ?\()
          (cl-loop for k being the hash-keys of intef-database
                   using (hash-values v)
                   do
                   (insert ?\()
                   (prin1 k buf)
                   (insert ?.)
                   (prin1 v buf)
                   (insert ?\)))
          (insert ?\))))))

(defun intef--tag-file-with-value (filename tag value)
  "Tags `FILENAME' to `TAG' with `VALUE'."
  (let ((filename (expand-file-name filename)))
    (if-let* ((assocs (gethash filename intef-database)))
        (if-let* ((tag-values (assoc tag assocs 'string-equal)))
            (unless (member value tag-values)
              (setcdr tag-values (cons value (cdr tag-values))))
          (setcdr assocs (cons (list tag value) (cdr assocs))))
      (puthash filename (list (list tag value)) intef-database))))

(defun intef--remove-tag-value (filename tag value)
  "Removes `VALUE' from `TAG' of `FILENAME'."
  (if-let* ((tag-values
             (assoc tag (gethash filename intef-database)
                    'string-equal)))
      (setcdr tag-values (delete value (cdr tag-values)))))

(defun intef--remove-tag (filename tag)
  "Untags `TAG' from `FILENAME'.
Note that `TAG's values in `FILENAME', if present, will be lost."
  (if-let* ((assocs (gethash filename intef-database)))
      (puthash
       filename (assoc-delete-all tag assocs 'string-equal)
       intef-database)))

(defun intef--remove-filename (filename)
  "Removes `FILENAME' from database.
Note that all tags and values associated with `FILENAME' will be lost."
  (remhash filename intef-database))

(defun intef--rename-tag-value (filename tag old-value new-value)
  "Renames `OLD-VALUE' to `NEW-VALUE' of `TAG' of `FILENAME'."
  (if-let* ((assocs (gethash filename intef-database))
            (tag-values (assoc tag assocs 'string-equal))
            (stored-value (member old-value (cdr tag-values))))
      (setcar stored-value new-value)))

(defun intef--rename-tag (filename old-tag new-tag)
  "Renames `OLD-TAG' to `NEW-TAG' of `FILENAME'."
  (if-let* ((assocs (gethash filename intef-database))
            (tag-values (assoc old-tag assocs 'string-equal)))
      (setcar tag-values new-tag))) ;; test this one

(defun intef--rename-filename (old-filename new-filename)
  "Renames `OLD-FILENAME' to `NEW-FILENAME'."
  (if-let* ((assocs (gethash filename intef-database)))
      (progn
        (puthash new-filename assocs intef-database)
        (remhash old-filename hash-table)))) ;; also test
    

;; rendering, sorting and filtering


(defun intef--render (hash-table sort-fn filter-fn)
  (setq buffer-read-only nil)
  (erase-buffer)
  (cl-loop
   for file being the hash-keys of hash-table
   using (hash-values tags) do
   (let ((file-start (point)))
     (cl-loop
      for (tag . values) in tags do
      (let ((tag-start (point)))
        (cl-loop
         for value in values do
         (let ((value-start (point))
               (row (list file tag value)))
           (add-text-properties
            (point)
            (progn
              (insert (if (member row intef-marked-rows) 42 ? )
                      ? )
              (point))
            '(column mark))

           (add-text-properties
            (point)
            (progn (insert file ? ) (point))
            '(column file))

           (add-text-properties
            (1- (point))
            (point)
            '(sep t))
           
           (add-text-properties
            (point)
            (progn (insert tag ? ) (point))
            '(column tag))
           (add-text-properties
            (1- (point))
            (point)
            '(sep t))
           
           (add-text-properties
            (point)
            (progn (insert value ?\n) (point))
            '(column value))
           (add-text-properties
            (1- (point))
            (point)
            '(sep t))
           
           (add-text-properties
            value-start (point)
            `(value ,value))))
        (add-text-properties
         tag-start (point)
         `(tag ,tag))))
     (add-text-properties
      file-start (point)
      `(file ,file))))
  (setq buffer-read-only t))

;; mode and setup


(defun intef--revert-buffer-function (file-name read-auto-save)
  (intef--render intef-database nil nil))

(defun intef--kill-buffer-hook ()
  (intef--write-database-file)
  (setq intef-database nil
        intef-buffer nil
        intef-marked-rows nil))

(defun intef--setup (sort-fn filter-fn &optional re-render)
  (when (and re-render intef-buffer) (kill-buffer intef-buffer))
  (if intef-buffer (switch-to-buffer intef-buffer)
    (let ((buff (get-buffer-create "*Intef*")))
      (switch-to-buffer buff)
      (intef-mode)
      (setq intef-buffer buff)
      (intef--read-database-file)
      (setq-local revert-buffer-function
                  #'intef--revert-buffer-function)
      (add-hook 'kill-buffer-hook #'intef--kill-buffer-hook nil t)
      (intef--render intef-database sort-fn filter-fn))))

(define-derived-mode intef-mode special-mode "Intef"
  :interactive nil)

(defun intef ()
  (interactive)
  (intef--setup nil nil))

;; commands


(defun intef-tag-file (filename tag value)
  (interactive
   (list (expand-file-name (read-file-name "file: " nil (buffer-file-name)))
         (read-string (format-prompt "tag" "\"untagged\"") nil nil "untagged")
         (read-string "value: ")))
  (intef--read-database-file)
  (intef--tag-file-with-value filename tag value))
  

(defun intef-tag-buffer-file (filename tag value)
  (interactive
   (let ((filename (buffer-file-name)))
     (if filename
         (list filename
               (read-string (format-prompt "tag" "\"untagged\"") nil nil "untagged")
               (read-string "value: "))
       (list nil nil nil))))
  (if filename
      (progn
        (intef--read-database-file)
        (intef--tag-file-with-value filename tag value))
    (message "Buffer is not associated to a file, could not tag it.")))

(defun intef-tag-files-dired (filenames tag value)
  (interactive
   (let ((files (dired-get-marked-files)))
     (if files
         (list files
               (read-string (format-prompt "tag" "\"untagged\"") nil nil "untagged")
               (read-string "value: "))
       (list nil nil nil))))
  (if filenames
      (progn
        (intef--read-database-file)
        (cl-loop
         for file in filenames do
         (intef--tag-file-with-value file tag value)))
    (message "Please mark some files before tagging them.")))

(defun intef-save-and-exit ()
  (interactive)
  (if intef-buffer
      (kill-buffer intef-buffer)
    (intef--kill-buffer-hook)))

(defun intef-buffer-mark-rows ()
  (interactive)
  (setq buffer-read-only nil)
  (save-excursion
    (if (region-active-p)
        (let* ((reg-beg (region-beginning))
               (reg-end (region-end))
               (beg (progn
                      (goto-char reg-beg)
                      (forward-line 0)
                      (point)))
               (end (progn
                      (goto-char reg-end)
                      (forward-line 0)
                      (point))))
          (goto-char beg)
          (while (< (point) end)
            (let ((row
                   (list
                    (get-text-property (point) 'file)
                    (get-text-property (point) 'tag)
                    (get-text-property (point) 'value))))
              (unless (member row intef-marked-rows)
                (setq intef-marked-rows
                      (cons row intef-marked-rows))
                (delete-char 1)
                (insert 42)))
            (forward-line 1)))
      (let ((row
             (list
              (get-text-property (point) 'file)
              (get-text-property (point) 'tag)
              (get-text-property (point) 'value))))
        (forward-line 0)
        (unless (member row intef-marked-rows)
          (setq intef-marked-rows
                (cons row intef-marked-rows))
          (delete-char 1)
          (insert 42)))))
  (setq buffer-read-only t))

(defun intef-buffer-files-tag (filenames tag value)
  "Tags selected files")

(defun intef-buffer-sort-custom (sort-fn))

(defun intef-buffer-filter-custom (filter-fn)
  (interactive
   (list
    (eval-minibuffer "Sort function: " "(lambda (file tag value) t)")))
  (intef--setup nil filter-fn t))

(provide 'intef)
