;; Elisp helper functions

(defun build-and-run-example ()
  "Find build.sh and run.sh from within the project and execute it."
  (interactive)
  (let ((run-directory (concat (locate-dominating-file default-directory ".git")
			       "scripts/examples/")))
    (compile (concat "cd "
			   run-directory
			   " && "
			   run-directory
			   "build.sh && "
			   run-directory
			   "run.sh draw_text"))
    ))

(bind-key "C-x m" `build-and-run-example)
