; -*- lexical-binding: t -*-



(defvar intef-buffer nil
  "Intef buffer, if any.")
;; should be custom
(defvar intef-database-filename nil
  "Filename of database currently open.
`nil' if no database is open.")
(defvar intef-database nil
  "Hash table of database currently open.
`nil' if no database is open.")

;; database operations


(defun intef--read-database-file (filename)
  "Returns hash-table generated from alist in `FILENAME'.

The database file should be a text file containing
an alist from filenames to alists. The latter alists
are mappings from tags to values. Filenames will be
the hash-table keys and the alists will be values.

A filename contains one or more tags, and each tag
associated to a filename contains zero or more values.
Filenames, tags and values are all strings."
  (with-temp-buffer
    (let ((table (make-hash-table :test 'equal)))
      (cl-loop for (filename . assocs) in
               (read (prog1 (point-marker)
                       (insert-file-contents filename)))
               do (puthash filename assocs table))
      table)))

(defun intef--write-database-file (filename hash-table)
  "Writes `HASH-TABLE' to `FILENAME' in alist format.
Each key of `HASH-TABLE' is a filename and its respective
value is an alist from tags to values. For more information
on `FILENAME's format read documentation of
`intef--read-database-file'."
  (with-temp-file filename
    (let ((buf (current-buffer)))
      (insert ?\()
      (cl-loop for k being the hash-keys of hash-table
               using (hash-values v)
               do
               (insert ?\()
               (prin1 k buf)
               (insert ?.)
               (prin1 v buf)
               (insert ?\)))
      (insert ?\)))))

(defun intef--tag-file-with-value (filename tag value hash-table)
  "Tags `FILENAME' to `TAG' with `VALUE' in `HASH-TABLE'."
  (let ((filename (expand-file-name filename)))
    (if-let* ((assocs (gethash filename hash-table)))
        (if-let* ((tag-values (assoc tag assocs 'string-equal)))
            (unless (member value tag-values)
              (setcdr tag-values (cons value (cdr tag-values))))
          (setcdr assocs (cons (list tag value) (cdr assocs))))
      (puthash filename (list (list tag value)) hash-table))))

(defun intef--remove-tag-value (filename tag value hash-table)
  "Removes `VALUE' from `TAG' of `FILENAME' in `HASH-TABLE'."
  (if-let* ((tag-values
             (assoc tag (gethash filename hash-table)
                    'string-equal)))
      (setcdr tag-values (delete value (cdr tag-values)))))

(defun intef--remove-tag (filename tag hash-table)
  "Untags `TAG' from `FILENAME' in `HASH-TABLE'.
Note that `TAG's values in `FILENAME', if present, will
be lost."
  (if-let* ((assocs (gethash filename hash-table)))
      (puthash
       filename (assoc-delete-all tag assocs 'string-equal)
       hash-table)))

(defun intef--remove-filename (filename hash-table)
  "Removes `FILENAME' from `HASH-TABLE'.
Note that all tags and values associated with `FILENAME'
will be lost."
  (remhash filename hash-table))

(defun intef--rename-tag-value (filename tag old-value new-value hash-table)
  "Renames `OLD-VALUE' to `NEW-VALUE' of `TAG' of `FILENAME' in `HASH-TABLE'."
  (if-let* ((assocs (gethash filename hash-table))
            (tag-values (assoc tag assocs 'string-equal))
            (stored-value (member old-value (cdr tag-values))))
      (setcar stored-value new-value)))
          

(defun intef--rename-tag (filename old-tag new-tag hash-table)
  "Renames `OLD-TAG' to `NEW-TAG' of `FILENAME' in `HASH-TABLE'."
  (if-let* ((assocs (gethash filename hash-table))
            (tag-values (assoc old-tag assocs 'string-equal)))
      (setcar tag-values new-tag))) ;; test this one

(defun intef--rename-filename (old-filename new-filename hash-table)
  "Renames `OLD-FILENAME' to `NEW-FILENAME' in `HASH-TABLE'."
  (if-let* ((assocs (gethash filename hash-table)))
      (progn
        (puthash new-filename assocs hash-table)
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
         (let ((value-start (point)))
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
  (intef--write-database-file intef-database-filename
                              intef-database))

(defun intef--setup (filename buffer-name sort-fn filter-fn)
  (let ((buff (get-buffer-create buffer-name)))
    (switch-to-buffer buff)
    (intef-mode)
    (setq intef-buffer buff)
    (setq intef-database-filename filename)
    (setq intef-database (intef--read-database-file filename))
    (setq-local revert-buffer-function
                #'intef--revert-buffer-function)
    (add-hook 'kill-buffer-hook #'intef--kill-buffer-hook nil t)
    (intef--render intef-database sort-fn filter-fn)))

(define-derived-mode intef-mode special-mode "Intef"
  :interactive nil)

(defun intef (filename buffer-name sort-fn filter-fn)
  (interactive
   (list "/home/lvds/Projects/personal/file-tagging/test.lisp"
         "*Intef*"
         nil nil))
  (intef--setup filename buffer-name sort-fn filter-fn))

;; commands


(defun intef-tag-file (filename tag value)
  (interactive
   (list (expand-file-name (read-file-name "file: " nil (buffer-file-name)))
         (read-string (format-prompt "tag" "\"untagged\"") nil nil "untagged")
         (read-string "value: ")))
    (intef--tag-file-with-value filename tag value intef-database))
  

(defun intef-tag-buffer-file (filename tag value)
  (interactive
   (let ((filename (buffer-file-name)))
     (if filename
         (list filename
               (read-string (format-prompt "tag" "\"untagged\"") nil nil "untagged")
               (read-string "value: "))
       (list nil nil nil))))
  (if filename
      (intef--tag-file-with-value filename tag value intef-database)
    (message "Buffer is not associated to a file, could not tag it.")))

(defun intef-tag-files-dired (filenames tag value)
  (interactive
   (let ((files (dired-get-marked-files)))
     (if files
         (list files
               (read-string (format-prompt "tag" "\"untagged\"") nil nil "untagged")
               (read-string "value: "))
       (list nil nil nil))))
  (if files
      (cl-loop
       for file in files do
       (intef--tag-file-with-value file tag value intef-database))
    (message "Please mark some files before tagging them.")))


(defun intef-buffer-files-tag (filenames tag value)
  "Tags selected files")

(defun intef-buffer-sort-custom (sort-fn))

(defun intef-buffer-filter-custom (filter-fn)
  (interactive
   (list
    (eval-minibuffer "Sort function: " "(lambda (file tag value) t)")))
  (if intef-buffer
      (progn
        (switch-to-buffer intef-buffer)
        (intef intef-database-filename (buffer-name) nil filter-fn))
    (message "Could not filter, no Intef buffer is opened.")))
