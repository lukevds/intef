; -*- lexical-binding: t -*-

(defcustom intef-database-file
  (expand-file-name (concat user-emacs-directory "intef.lisp"))
  "Location on storage for Intef's default database file.")

(defvar-local intef-buffer-database-file
  nil
  "Database file location on storage.")

(defvar-local intef-db
  nil
  "Intef's database.
Format is an association list of association lists.
The outer alist associates filenames to tags and the
inner alists associate tags to values.")

(defvar-local intef-marked-rows
  nil
  "Intef's marked rows.")

(defun intef-read-sexp-db (filename)
  (with-temp-buffer
    (let ((point (point-marker)))
      (insert-file-contents filename)
      (condition-case nil
          (read point)
        (end-of-file nil)))))

(defun intef-write-sexp-db (db filename)
  (with-temp-file filename
    (print db (current-buffer))))

(defun intef-merge-sexp-dbs-values (source target)
  (append
   source
   (cl-loop
    for value in target
    if (not (member value source))
      collect value
    end)))

(defun intef-merge-sexp-dbs-tags (source target)
  (let ((res source))
    (cl-loop
     for ttagdata in target do
     (let ((stagdata (assoc (car ttagdata) res)))
       (if stagdata
           (setcdr stagdata
                   (intef-merge-sexp-dbs-values
                    (cdr stagdata)
                    (cdr ttagdata)))
         (setq res (cons ttagdata res)))))
  res))

(defun intef-merge-sexp-dbs (source target)
  (let ((res source))
    (cl-loop
     for tfiledata in target do
     (let ((sfiledata (assoc (car tfiledata) res)))
       (if sfiledata
           (setcdr sfiledata
                   (intef-merge-sexp-dbs-tags
                    (cdr sfiledata)
                    (cdr tfiledata)))
         (setq res (cons tfiledata res)))))
    res))

(defun intef-save-to-db (data)
  (setq intef-db
        (intef-merge-sexp-dbs
         intef-db data)))

(defun intef-dired-save-to-db (data)
  (save-window-excursion
    (intef intef-database-file)
    (intef-save-to-db data)))


(defun intef-save-buffer ()
  (interactive)
  (message "Saving file %s..." intef-buffer-database-file)
  (intef-write-sexp-db intef-db intef-buffer-database-file)
  (message "Wrote %s" intef-buffer-database-file)
  (set-buffer-modified-p nil)
  t)

(defun intef-stale-function (&optional _noconfirm)
  (buffer-modified-p))

(defun intef-revert-buffer (&optional _ignore-auto _noconfirm)
  (setq intef-marked-rows nil)
  (setq buffer-read-only nil)
  (let ((point (point)))
    (erase-buffer)
    (intef-render intef-db)
    (goto-char point))
  (setq buffer-read-only t)
  (set-buffer-modified-p nil))

(cl-defun intef-format-cell (str &optional (width 40))
  (let ((n (length str)))
    (cond
     ((> n width) (concat "…" (substring str (- n (1- width)) n)))
     ((< n width) (string-pad str width))
     (t str))))

(defun intef-render (db)
  (if db
      (progn
        (insert "  " (intef-format-cell "file" 60))
        (insert  " ⯁ ")
        (insert (intef-format-cell "tag" 20))
        (insert  " ⯁ ")
        (insert (intef-format-cell "value") ?\n)
        (cl-loop
         for filedata in db do
         (let ((filename (car filedata))
               (tags (cdr filedata)))
           (cl-loop
            for tagdata in tags do
            (let ((tagname (car tagdata))
                  (values (cdr tagdata)))
              (cl-loop
               for value in values do ; if filename has some tag, it always has at least the value "ε"
               (let ((beg (point)))
                 (insert "  " (intef-format-cell filename 60))
                 (insert  " ⯁ ")
                 (insert (intef-format-cell tagname 20))
                 (insert  " ⯁ ")
                 (insert (intef-format-cell value) ?\n)
                 (add-text-properties
                  beg (point)
                  (list 'data
                        (cons filedata
                              (cons tagdata
                                    value))))))))))
        (goto-char (point-min)))
    (insert "No tags to be shown.\n")))

(defun intef (filename)
  (interactive (list intef-database-file))
  (let* ((filename intef-database-file))
    (let* ((intef-filename (file-name-nondirectory filename))
           (intef-buffer-name (concat "intef: " intef-filename))
           (buf-exists-p (get-buffer intef-buffer-name))
           (buf (get-buffer-create intef-buffer-name)))
      (switch-to-buffer buf)
      (if (not buf-exists-p)
          (progn
            (let ((db (intef-read-sexp-db filename)))
              (intef-render db)
              (intef-mode)
              (set-buffer-modified-p nil)
              (setq intef-buffer-database-file filename)
              (setq intef-db db)
              (setq-local revert-buffer-function 'intef-revert-buffer)
              (setq-local buffer-stale-function 'intef-stale-function)))))))

(defvar intef-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "p" #'previous-line)
    (keymap-set map "n" #'next-line)
    (keymap-set map "b" #'backward-char)
    (keymap-set map "f" #'forward-char)
    (keymap-set map "m" #'intef-mark-rows)
    (keymap-set map "u" #'intef-unmark-rows)
    (keymap-set map "<remap> <save-buffer>" #'intef-save-buffer)
    (keymap-set map "t" #'intef-tag-rows)
    (keymap-set map "M-t" #'intef-tag-rows-same-tag)
    (keymap-set map "d" #'intef-remove-tag-value)
    map))

(define-derived-mode intef-mode special-mode "Intef"
  "intef"
  :interactive nil)

(defun delq-first (elt list)
  (let ((original list))
    (if (eq (car list) elt)
        (setq original (cdr list))
      (while list
        (if (eq (cadr list) elt)
            (progn
              (setcdr list (cddr list))
              (setq list nil))
          (setq list (cdr list)))))
      original))

(defun intef-mark-rows (beg &optional end)
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point) (point))))
  (save-excursion
    (goto-char beg)
    (forward-line
     (if (get-text-property (point) 'data) 0 1))
    (setq buffer-read-only nil)
    (let ((lines-left-to-move 0))
      (while (and (<= (point) end) (zerop lines-left-to-move))
        (let ((data (get-text-property (point) 'data)))
          (unless (memq data intef-marked-rows)
            (setq intef-marked-rows (cons data intef-marked-rows))
            (forward-char 1)
            (insert 42)
            (delete-char 1)))
        (setq lines-left-to-move
              (forward-line 1))))
    (setq buffer-read-only t))
  (if (= beg end) (next-line) (deactivate-mark))
  (set-buffer-modified-p nil))

(defun intef-unmark-rows (beg &optional end)
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point) (point))))
  (save-excursion
    (goto-char beg)
    (forward-line
     (if (get-text-property (point) 'data) 0 1))
    (setq buffer-read-only nil)
    (let ((lines-left-to-move 0))
      (while (and (<= (point) end) (zerop lines-left-to-move))
        (let ((data (get-text-property (point) 'data)))
          (when (memq data intef-marked-rows)
            (setq intef-marked-rows (delq-first data intef-marked-rows))
            (forward-char 1)
            (insert 32)
            (delete-char 1)))
        (setq lines-left-to-move
              (forward-line 1))))
    (setq buffer-read-only t))
  (if (= beg end) (next-line) (deactivate-mark))
  (set-buffer-modified-p nil))



(defun intef-tag-rows (rows tag value)
  (interactive
   (list (or intef-marked-rows
             (list (get-text-property (point) 'data)))
         (read-string (format-prompt "tag" " \"ε\"") nil nil "ε")
         (read-string "value: " nil nil "ε")))
  (if rows
      (progn
        (intef-save-to-db
         (cl-loop
          for row in rows collect
          (let ((filename (caar row)))
            (cons filename (list (cons tag (list value)))))))
        (set-buffer-modified-p t))
    (message "Could not tag files.")))

(defun intef-tag-rows-same-tag (rows value)
  (interactive
   (list
    (or intef-marked-rows
        (list (get-text-property (point) 'data)))
    (read-string "value: ")))
  (when rows
    (intef-save-to-db
     (cl-loop
      for row in rows collect
      (let ((filename (caar row))
            (tag (caadr row)))
        (cons filename (list (cons tag (list value)))))))
    (set-buffer-modified-p t)))

(defun intef-remove-tag-value (rows)
  (interactive
   (list
    (or intef-marked-rows
        (list (get-text-property (point) 'data)))))
  (when rows
    (cl-loop
     for (data tag-data . value) in rows do
     (if (= (length (cdr tag-data)) 1)
         (setf (cdr data) (delq-first tag-data (cdr data)))
       (setf (cdr tag-data) (delq-first value (cdr tag-data)))))
    (set-buffer-modified-p t)))



(defvar intef-dired-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-c C-c" #'intef-dired-tag-file)
    map))

(defvar-local intef-dired-mode nil)

(defun intef-dired-mode (&optional arg)
  (interactive (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg) 'toggle)))
  (if (eq major-mode 'dired-mode)
    (let ((last-message (current-message)))
      (setq intef-dired-mode
            (cond
             ((eq arg 'toggle) (not intef-dired-mode))
             ((and (numberp arg) (< arg 1)) nil)
             (t t)))
      (when (boundp 'local-minor-modes)
        (setq local-minor-modes (delq 'intef-dired-mode local-minor-modes))
        (when intef-dired-mode
          (push 'intef-dired-mode local-minor-modes)))
      (run-hooks 'intef-dired-mode-hook (if intef-dired-mode 'intef-dired-mode-on-hook 'intef-dired-mode-off-hook))
      (if (called-interactively-p 'any)
          (unless (and (current-message) (not (equal last-message (current-message))))
            (let ((local " in current buffer"))
              (message "%s %sabled%s" "Intef-Dired mode" (if intef-dired-mode "en" "dis") local)))))
    (message "cannot use intef-dired-mode outside of Dired")))

(defvar intef-dired-mode-hook nil)
(put 'intef-dired-mode-hook 'custom-type 'hook)
(put 'intef-dired-mode-hook 'standard-value (list nil))

;; (setq minor-mode-map-alist (assq-delete-all 'intef-dired-mode minor-mode-map-alist))
(add-to-list 'minor-mode-map-alist (cons 'intef-dired-mode intef-dired-mode-map))

(defun intef-dired-tag-file (filenames tag value)
  (interactive
   (let ((files (dired-get-marked-files)))
     (if files
         (list files
               (read-string (format-prompt "tag" " \"ε\"") nil nil "ε")
               (read-string "value: " nil nil "ε"))
       (list nil nil nil))))
  (if filenames
      (intef-dired-save-to-db
       (cl-loop
        for filename in filenames collect
        (cons filename (list (cons tag (list value))))))
    (message "Please mark some files before tagging them.")))


;; next:
;; mark by regexp
;; invert tag
;; completion
;; intef-global-minor-mode: tags current file

;; discussion
;; create interface for db?
;; export db, export filter
;; at what point should I stop to tag my files?
;; configure file visit, so it's easy to work with multiple dbs
;; automatically intef when ext is .intef.lisp, else invoke it manually. support other formats in the future
