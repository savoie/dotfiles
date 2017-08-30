;; initialize frame appearance
(setq inhibit-startup-screen t)
(set-frame-parameter nil 'internal-border-width 35)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

;; package management
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

(use-package xresources-theme
  :ensure t
  :pin melpa
  :config (let ((x-resource-class "XTerm"))
	    (load-theme 'xresources)))

(set-default-font "FantasqueSansMono-12")
(setq-default letter-spacing 5)
(setq-default line-spacing 2)

(use-package linum-relative
  :ensure t
  :pin melpa-stable
  :config (progn
	    (setq linum-relative-current-symbol "")
	    (set-face-bold 'linum-relative-current-face nil)
	    (set-face-background 'linum-relative-current-face
				 (face-attribute 'default :background))
	    (set-face-foreground 'linum-relative-current-face
				 (face-attribute 'default :foreground))
	    (global-linum-mode nil)
	    (linum-relative-on)))

(use-package company
  :ensure t
  :config (use-package company-quickhelp
	    :ensure t))

(use-package slime
  :ensure t
  :config (progn
	    (setq inferior-lisp-program "/usr/local/opt/sbcl/bin/sbcl")
	    (setq slime-contribs '(slime-fancy))))

(require 'org)
(require 'ox-beamer)
(setq org-log-done t)
(setq org-agenda-files '("~/org"))
(setq org-agenda-window-setup 'current-window)
(setq org-default-notes-file (concat org-directory "/notes.org"))

(setq org-agenda-default-appointment-duration 15)
(setq org-icalendar-combined-agenda-file "~/org/agenda.ics")
(setq org-icalendar-include-todo '(all))
(setq org-icalendar-use-scheduled '(event-if-todo event-if-not-todo))
(setq org-icalendar-use-deadline '(event-if-todo event-if-not-todo))

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
		       '(textobjects insert leader)))

	    ;; Don't print state to echo area
	    (setq evil-insert-state-message nil)
	    (setq evil-visual-state-message nil)
	    (setq evil-visual-line-message nil))

  :init (progn
	  (use-package evil-leader
	    :ensure t
	    :pin melpa-stable
	    :config (progn
		      (evil-leader/set-leader "<SPC>")
		      (evil-leader/set-key
			"a" 'org-agenda
			"c" 'org-capture)
		      (global-evil-leader-mode)))

	  (evil-mode 1)))

;; misc
(add-hook 'before-save-hook 'whitespace-cleanup)
(setq vc-follow-symlinks t)
(setq mode-line-in-non-selected-windows nil)
(set-face-attribute 'mode-line nil :box nil)
