(defun run-perl_checker ()
  (interactive)
  (save-some-buffers 1)
  (compile (concat "perl_checker --restrict-to-files " (buffer-file-name (current-buffer)))))

(let ((hook '(lambda () 
	       (local-set-key [(control return)] 'run-perl_checker)
	       )))
    (add-hook 'perl-mode-hook hook)
    (add-hook 'cperl-mode-hook hook))
