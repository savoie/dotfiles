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

(use-package linum-relative
  :ensure t
  :init (setq linum-relative-current-symbol "")
  :config (progn
	    (global-linum-mode nil)
	    (linum-relative-on)))

(use-package slime
  :ensure t
  :config (progn
	    (setq inferior-lisp-program "/usr/local/opt/sbcl/bin/sbcl")
	    (setq slime-contribs '(slime-fancy)))
  )

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
	    (use-package key-chord
	      :ensure t
	      :config (progn
			(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
			(key-chord-define evil-insert-state-map "kj" 'evil-normal-state))
	      :init (key-chord-mode 1))

	    (use-package evil-surround
	      :ensure t
	      :init (global-evil-surround-mode 1))

	    ;; Don't print state to echo area
	    (setq evil-insert-state-message nil)
	    (setq evil-visual-state-message nil)
	    (setq evil-visual-line-message nil))
  :init (evil-mode 1))

;; Misc
(add-hook 'before-save-hook 'whitespace-cleanup)
(setq vc-follow-symlinks t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
