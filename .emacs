(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(setq custom-file "~/.emacs.d/etc/custom.el")
(when (not (file-exists-p custom-file))
  (with-temp-buffer (write-file custom-file)))
(load custom-file :noerror)

(use-package monokai-theme
  :ensure t)

(use-package slime
  :ensure t
  :config (progn
	    (setq inferior-lisp-program "/usr/local/opt/sbcl/bin/sbcl")
	    (setq slime-contribs '(slime-fancy)))
  :defer t)

(use-package org
  :ensure t
  :config (progn
	    (define-key global-map "\C-cl" 'org-store-link)
	    (define-key global-map "\C-ca" 'org-agenda)
	    (setq org-log-done t)
	    (setq org-agenda-files '("~/org"))
	    (setq org-agenda-window-setup 'current-window)
	    (setq org-default-notes-file (concat org-directory "/notes.org"))
	    (define-key global-map "\C-cc" 'org-capture)))

(use-package evil
  :ensure t
  :config (progn
	    ;; Esc anything
	    (define-key evil-normal-state-map [escape] 'keyboard-quit)
	    (define-key evil-visual-state-map [escape] 'keyboard-quit)
	    (define-key minibuffer-local-map [escape] 'keyboard-escape-quit)
	    (define-key minibuffer-local-ns-map [escape] 'keyboard-escape-quit)
	    (define-key minibuffer-local-completion-map [escape] 'keyboard-escape-quit)
	    (define-key minibuffer-local-must-match-map [escape] 'keyboard-escape-quit)
	    (define-key minibuffer-local-isearch-map [escape] 'keyboard-escape-quit)

	    ;; j/k on visual lines
	    (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
	    (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

	    ;; Keysmash escape
	    (define-key evil-insert-state-map "k" #'cofi/maybe-exit-kj)
	    (evil-define-command cofi/maybe-exit-kj ()
	      :repeat change
	      (interactive)
	      (let ((modified (buffer-modified-p)))
		(insert "k")
		(let ((evt (read-event (format "Insert %c to exit insert state" ?j)
				       nil 0.5)))
		  (cond
		   ((null evt) (message ""))
		   ((and (integerp evt) (char-equal evt ?j))
		    (delete-char -1)
		    (set-buffer-modified-p modified)
		    (push 'escape unread-command-events))
		   (t (setq unread-command-events (append unread-command-events
							  (list evt))))))))
	    (define-key evil-insert-state-map "j" #'cofi/maybe-exit-jk)
	    (evil-define-command cofi/maybe-exit-jk ()
	      :repeat change
	      (interactive)
	      (let ((modified (buffer-modified-p)))
		(insert "j")
		(let ((evt (read-event (format "Insert %c to exit insert state" ?k)
				       nil 0.5)))
		  (cond
		   ((null evt) (message ""))
		   ((and (integerp evt) (char-equal evt ?k))
		    (delete-char -1)
		    (set-buffer-modified-p modified)
		    (push 'escape unread-command-events))
		   (t (setq unread-command-events (append unread-command-events
							  (list evt)))))))))
  :init (evil-mode 1))

;; Whitespace cleanup
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Line numbering
(global-linum-mode t)
