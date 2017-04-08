(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)

(setq custom-file "~/.emacs.d/etc/custom.el")
(when (not (file-exists-p custom-file))
  (with-temp-buffer (write-file custom-file)))
(load custom-file :noerror)

(unless (require 'quelpa nil t)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

(quelpa
 '(quelpa-use-package
   :fetcher github
   :repo "quelpa/quelpa-use-package"
   :stable nil))
(require 'quelpa-use-package)

(use-package monokai-theme
  :ensure t
  :pin melpa-stable)

(use-package linum-relative
  :ensure t
  :pin melpa-stable
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
  :pin melpa-stable
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
	      :pin melpa-stable
	      :config (progn
			(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
			(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
			(key-chord-mode 1)))

	    (use-package evil-surround
	      :ensure t
	      :pin melpa-stable
	      :config (global-evil-surround-mode 1))

	    (use-package evil-org
	      :quelpa (evil-org
		       :fetcher github
		       :repo "somelauw/evil-org-improved"
		       :stable nil)
	      :config (evil-org-set-key-theme
		       '(textobjects insert additional shift leader)))

	    ;; Don't print state to echo area
	    (setq evil-insert-state-message nil)
	    (setq evil-visual-state-message nil)
	    (setq evil-visual-line-message nil))
  :init (progn
	  (use-package evil-leader
	    :ensure t
	    :pin melpa-stable
	    :config (evil-leader/set-leader "<SPC>")
	    :init (global-evil-leader-mode))

	  (evil-mode 1)))

;; Misc
(add-hook 'before-save-hook 'whitespace-cleanup)
(setq vc-follow-symlinks t)
(setq inhibit-startup-screen t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
