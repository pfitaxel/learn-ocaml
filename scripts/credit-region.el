;;; credit-region.el --- Helper elisp script -*- lexical-binding:t; -*-
;; Author: Erik Martin-Dorel, 2023
;; License: MIT

(defconst credit-region-command "credit-region.sh")

; (defvar credit-region-log-buffer nil)
; 
; (defun credit-region-log-buffer ()
;   "Return the value of variable `credit-region-log-buffer'.
; Call `get-buffer-create' if need be, to ensure it is a live buffer."
;   (unless (buffer-live-p credit-region-log-buffer)
;     (setq credit-region-log-buffer (get-buffer-create "*credit-region-log*")))
;   credit-region-log-buffer)

(defun credit-region-update-exec-path (default-dir)
  "Propose to add a directory, e.g. DEFAULT-DIR, to `exec-path'."
  (if noninteractive
      (if default-dir
          (add-to-list 'exec-path default-dir)
        (error "No directory selected (in credit-region-update-exec-path)"))
    (let ((dir (condition-case _sig
                   (read-directory-name
                    (format "Add scripts folder containing %s: " credit-region-command)
                    (or default-dir "~/")
                    (or default-dir "~/")
                    t "")
                 (quit ""))))
      (if (and dir (not (string-equal "" dir)))
          (add-to-list 'exec-path dir)))))

(cl-defun credit-region-make-process-wrapper (&rest args &key command &allow-other-keys)
  "Call `make-process' after checking the program is in `exec-path'."
  (if (and (listp command) (not (string-equal credit-region-command (car command))))
           (error "\"%s\": command not supported" (car command)))
  (if (executable-find (car command))
      (apply #'make-process args)
    ;; (credit-region-update-exec-path nil)
    (credit-region-update-exec-path (file-name-directory (buffer-file-name)))
    (if (executable-find (car command))
        (apply #'make-process args)
      (error "\"%s\" not found in the exec-path" (car command)))))

(defun credit-region-error-handler (buffer-ok buffer-err callback-ok callback-err proc string)
  "Get text from BUFFER and pass it to the CALLBACK.
To be used as a `make-process' sentinel, using args PROC and STRING."
  (let ((stdout (if (not buffer-ok) "" (set-buffer buffer-ok) (buffer-string)))
        (stderr (if (not buffer-err) "" (set-buffer buffer-err) (buffer-string))))
    (when buffer-ok (kill-buffer buffer-ok))
    (when buffer-err (kill-buffer buffer-err))
    (if (or (string-equal string "finished\n")
            ;; (string-match "credit-region" (process-name proc))
            )
        (funcall callback-ok stdout stderr)
      (funcall callback-err stdout stderr))))

(defun credit-region-main (&optional raw)
  "The main function!"
  (interactive "P")
  (let* ((file (buffer-file-name))
         (base (file-name-nondirectory file))
         (lbeg 0)
         (lend 0)
         (run nil))
    (if (use-region-p)
        (setq lbeg (line-number-at-pos (region-beginning))
              lend (line-number-at-pos (- (region-end) 1))
              run t)
      (when (y-or-n-p (format "No region active. Get credits for the whole %s?" base))
        (setq lbeg 1
              lend ""
              run t)))
    (if run
        (let ((stdout (generate-new-buffer "credit-region-out"))
              (stderr (generate-new-buffer "credit-region-err")))
          (credit-region-make-process-wrapper
           :name "credit-region"
           :command (list credit-region-command file (number-to-string lbeg)
                          (if (equal "" lend) "" (number-to-string lend))
                          (if raw "true" ""))
           :stderr stderr
           :buffer stdout
           :sentinel (apply-partially
                      #'credit-region-error-handler
                      stdout
                      stderr
                      (lambda (std err)
                        (kill-new std)
                        (message "Co-authored-by: ... copied to the clipboard!\nStats%s:\n%s"
                                 (if raw "" " for oauth-moodle-dev") err))
                      (lambda (std err) (message-box "ERROR: %s\nOutput (if any): %s" err std)))))
      (message "Operation cancelled."))))

(global-set-key (kbd "<f9>") #'credit-region-main)
