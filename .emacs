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

(use-package company
  :ensure t
  :config (use-package company-quickhelp
	    :ensure t))

(use-package slime
  :ensure t
  :config (progn
	    (setq inferior-lisp-program "/usr/local/opt/sbcl/bin/sbcl")
	    (setq slime-contribs '(slime-fancy))))

(use-package tuareg
  :quelpa (tuareg
	   :fetcher github
	   :repo "ocaml/tuareg"
	   :stable nil)
  ;; from https://ocaml.org/learn/tutorials/get_up_and_running.html
  :config (progn
	    ;; Add opam emacs directory to the load-path
	    (setq opam-share
		  (substring
		   (shell-command-to-string "opam config var share 2> /dev/null")
		   0 -1))
	    (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))

	    ;; Add the opam lisp dir to the emacs load path
	    (add-to-list
	     'load-path
	     (replace-regexp-in-string
	      "\n" "/share/emacs/site-lisp"
	      (shell-command-to-string "opam config var prefix")))
	    ;; Automatically load utop.el
	    (autoload 'utop "utop" "Toplevel for OCaml" t)
	    (setq utop-command "opam config exec -- utop -emacs")

	    (add-hook
	     'tuareg-mode-hook
	     (lambda ()
	       ;; Load merlin-mode
	       (require 'merlin)
	       ;; Start merlin on ocaml files
	       (add-hook 'tuareg-mode-hook 'merlin-mode t)
	       (add-hook 'caml-mode-hook 'merlin-mode t)
	       ;; Enable auto-complete
	       (setq merlin-use-auto-complete-mode 'easy)
	       ;; Use opam switch to lookup ocamlmerlin binary
	       (setq merlin-command 'opam)
	       (company-mode)
	       (require 'ocp-indent)
	       (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
	       (autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
	       (autoload 'merlin-mode "merlin" "Merlin mode" t)
	       (utop-minor-mode)
	       (company-quickhelp-mode)
	       ;; Important to note that setq-local is a macro and it needs to be
	       ;; separate calls, not like setq
	       (setq-local merlin-completion-with-doc t)
	       (setq-local indent-tabs-mode nil)
	       (setq-local show-trailing-whitespace t)
	       (setq-local indent-line-function 'ocp-indent-line)
	       (setq-local indent-region-function 'ocp-indent-region)
	       (merlin-mode)))

	    (add-hook 'utop-mode-hook (lambda ()
					(set-process-query-on-exit-flag
					 (get-process "utop") nil)))))


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

;; Misc
(add-hook 'before-save-hook 'whitespace-cleanup)
(setq vc-follow-symlinks t)
(setq inhibit-startup-screen t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
