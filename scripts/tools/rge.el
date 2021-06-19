;; Elisp helper functions

(setq last-rge-script-directory nil)

(defun build-and-run-example (arg)
  "Find build.sh and run.sh from within the project and execute it.
Call with universar ARG (Ctrl+U) to skip building stage."
  (interactive "P")
  (let ((can-only-run (equal current-prefix-arg '(4)))
	(script-dir (if (string= (file-name-extension buffer-file-name)
				 "scm")
			(progn
			  (setq last-rge-script-directory (file-name-directory buffer-file-name))
			  last-rge-script-directory)
		      last-rge-script-directory))
	(makefile-dir (locate-dominating-file default-directory "Makefile")))
    (compile (concat (when (not can-only-run)
		       (concat "cd "
			       makefile-dir
			       " && make build && "))
		     "cd "
		     script-dir
		     " && make -C "
		     makefile-dir
		     " run MAIN_FILE="
		     script-dir
		     "main.scm OUTPUT_FILE="
		     script-dir
		     "output.rbl && cd ../ && "
		     script-dir
		     "/output.rbl"))
    ))

(bind-key "C-x m" `build-and-run-example)
