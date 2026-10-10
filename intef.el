; -*- lexical-binding: t -*-

(defvar intef-db 'unloaded
  "intef tagged files")

(defvar intef-file (expand-file-name (concat user-emacs-directory "intef.lisp"))
  "storage location for intef database")

(defvar intef-marked-rows nil
  "marked rows in intef buffer")

(defun intef-read-db ()
  (setq intef-db
        (if (file-exists-p intef-file)
            (with-temp-buffer
              (let ((point (point-marker)))
                (insert-file-contents intef-file)
                (condition-case nil
                    (read point)
                  (end-of-file nil))))
          (progn (with-temp-file intef-file) nil))))

(defun intef-maybe-read-db ()
  (when (eq intef-db 'unloaded) (intef-read-db))
  intef-db)

(defun intef-write-db ()
  (with-temp-file intef-file
    (print intef-db (current-buffer)))
  (setq intef-db 'unloaded)
  t)

(cl-defun intef-format-cell (str &optional (width 40))
  (let ((n (length str)))
    (cond
     ((> n width) (concat "…" (substring str (- n (1- width)) n)))
     ((< n width) (string-pad str width))
     (t str))))

(defun intef ()
  (interactive)
  (let* ((buf-exists-p (get-buffer "*Intef*"))
         (buf (get-buffer-create "*Intef*")))
    (switch-to-buffer buf)
    (if (not buf-exists-p)
        (progn
          (if intef-db
              (progn
                (insert "  " (intef-format-cell "file" 60))
                (insert  " ⯁ ")
                (insert (intef-format-cell "tag" 20))
                (insert  " ⯁ ")
                (insert (intef-format-cell "value") ?\n)
                (intef-read-db)
                (cl-loop
                 for filedata in intef-db do
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
            (insert "you have not tagged any files yet"))
          (intef-mode)))))

;; rename to intef-merge, two arguments instead of one
(defun intef-tag-multiple (data) ;; make looping code more uniform
  (intef-maybe-read-db)
  (cl-loop for (filename . tags) in data do
           (let ((filename-data (assoc-string filename intef-db)))
             (if filename-data
                 (cl-loop for tag in tags do
                          (let* ((tagname (car tag))
                                 (values (or (cdr tag) '("ε")))
                                 (tag-data (assoc-string tagname (cdr filename-data))))
                            (if tag-data
                                (cl-loop for value in values do
                                         (unless (member value (cdr tag-data))
                                           (setf (cdr tag-data) (cons value (cdr tag-data)))))
                              (setf (cdr filename-data)
                                    (cons (cons tagname values) (cdr filename-data))))))
               (setq intef-db (cons (cons filename tags) intef-db))))))

(defun intef-tag-files (filenames tag value)
  (when filenames
    (intef-tag-multiple
     (cl-loop for filename in filenames collect
              (cons filename (list (cons tag (list value))))))
    t))

(defun intef-dired-tag-file (filenames tag value)
  (interactive
   (let ((files (dired-get-marked-files)))
     (if files
         (list files
               (read-string (format-prompt "tag" " \"ε\"") nil nil "ε")
               (read-string "value: " nil nil "ε"))
       (list nil nil nil))))
  (unless (intef-tag-files filenames tag value)
    (message "Please mark some files before tagging them.")))

(defvar intef-dired-mode-map ;; active when commiting in magit? why? not using setq-local
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
  (if (= beg end) (next-line) (deactivate-mark)))

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
  (if (= beg end) (next-line) (deactivate-mark)))

(defun intef-add-value-to-tag (rows value)
  (interactive
   (list
    (or intef-marked-rows
        (list (get-text-property (point) 'data)))
    (read-string "value: ")))
  (cl-loop
   for (_ tag-data . _) in rows do
   (unless (member value (cdr tag-data))
     (setf (cdr tag-data) (cons value (cdr tag-data))))))

(defun intef-remove-value-from-tag (rows)
  (interactive
   (list
    (or intef-marked-rows
        (list (get-text-property (point) 'data)))))
  (cl-loop
   for (data tag-data . value) in rows do
   (if (= (length (cdr tag-data)) 1)
       (setf (cdr data) (delq-first tag-data (cdr data)))
     (setf (cdr tag-data) (delq-first value (cdr tag-data))))))

(defun intef-tag-file (filenames tag value)
  (interactive
   (list (or intef-marked-rows
             (list (get-text-property (point) 'data)))
         (read-string (format-prompt "tag" " \"ε\"") nil nil "ε")
         (read-string "value: " nil nil "ε")))
  (unless (intef-tag-files filenames tag value)
    (message "Could not tag files.")))

;; save file

(defvar intef-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "p" #'previous-line)
    (keymap-set map "n" #'next-line)
    (keymap-set map "b" #'backward-char)
    (keymap-set map "f" #'forward-char)
    (keymap-set map "m" #'intef-mark-rows)
    (keymap-set map "u" #'intef-unmark-rows)
    map))

(define-derived-mode intef-mode special-mode "Intef"
  "intef"
  :interactive nil)

;; next:
;; associate to a file (the db), revert buffer, save buffer
;; mark by regexp
;; invert tag
;; completion
;; intef-global-minor-mode: tags current file

;; discussion
;; create interface for db?
;; export db, export filter
;; at what point should I stop to tag my files?
;; configure file visit, so it's easy to work with multiple dbs
