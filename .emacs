(package-initialize)

(setq custom-file "~/.emacs.d/etc/custom.el")
(when (not (file-exists-p custom-file))
  (with-temp-buffer (write-file custom-file)))
(load custom-file :noerror)

(unless (require 'quelpa nil t)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

(setq quelpa-stable-p t)

(quelpa
 '(quelpa-use-package
   :fetcher github
   :repo "quelpa/quelpa-use-package"
   :stable nil))
(require 'quelpa-use-package)

(use-package monokai-theme
  :quelpa t)

(use-package linum-relative
  :quelpa t
  :init (setq linum-relative-current-symbol "")
  :config (progn
	    (global-linum-mode nil)
	    (linum-relative-on)))

(use-package slime
  :quelpa t
  :config (progn
	    (setq inferior-lisp-program "/usr/local/opt/sbcl/bin/sbcl")
	    (setq slime-contribs '(slime-fancy)))
  )

(use-package org
  :quelpa t
  :config (progn
	    (define-key global-map "\C-cl" 'org-store-link)
	    (define-key global-map "\C-ca" 'org-agenda)
	    (setq org-log-done t)
	    (setq org-agenda-files '("~/org"))
	    (setq org-agenda-window-setup 'current-window)
	    (setq org-default-notes-file (concat org-directory "/notes.org"))
	    (define-key global-map "\C-cc" 'org-capture)))

(use-package evil
  :quelpa (:stable nil)
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
	    (use-package key-chord
	      :quelpa t
	      :config (progn
			(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
			(key-chord-define evil-insert-state-map "kj" 'evil-normal-state))
	      :init (key-chord-mode 1))

	    (use-package evil-surround
	      :quelpa t
	      :init (global-evil-surround-mode 1))

	    ;; Don't print state to echo area
	    (setq evil-insert-state-message nil)
	    (setq evil-visual-state-message nil)
	    (setq evil-visual-line-message nil))
  :init (progn
	  (use-package evil-leader
	    :quelpa t
	    :config (evil-leader/set-leader "<SPC>")
	    :init (global-evil-leader-mode))

	  (evil-mode 1)))

;; Misc
(add-hook 'before-save-hook 'whitespace-cleanup)
(setq vc-follow-symlinks t)
(setq inhibit-startup-screen t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
