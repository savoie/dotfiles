;;; package management
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/"))
(package-initialize)

(setq custom-file "~/.emacs.d/etc/custom.el")
(when (not (file-exists-p custom-file))
  (progn (let ((dir (file-name-directory custom-file)))
           (unless (file-exists-p dir)
             (make-directory dir t)))
         (with-temp-buffer (write-file custom-file))))
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

(setq use-package-always-ensure t)

(defun load-init-file (file-name)
  (load-file (concat user-emacs-directory file-name)))

;;; appearance
(setq inhibit-startup-screen t)
(setq default-frame-alist
      '((fullscreen . maximized)
        (font . "FantasqueSansMono-12")
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (internal-border-width . 35)))
(setq-default line-spacing 2)
(menu-bar-mode -1)
(tool-bar-mode -1)

(setq display-line-numbers-type 'visual)
(global-display-line-numbers-mode)

(use-package noflet)

(use-package rich-minority
  :config (if (not rich-minority-mode)
            (rich-minority-mode 1)))

(use-package ewal)
(use-package ewal-spacemacs-themes
  :config (load-theme 'ewal-spacemacs-modern t)
  (enable-theme 'ewal-spacemacs-modern))

(set-face-attribute 'mode-line nil :background nil)
;(set-face-attribute 'line-number nil
;                    :foreground (plist-get base16-wal-colors :base02)
;                    :background (face-attribute 'default :background))

;; (use-package base16-theme
;;   :pin melpa-stable
;;   :init (add-to-list 'custom-theme-load-path "~/.cache/wal/"))

;; (defun refresh-theme ()
;;   (load-theme 'base16-wal t)
;;   (set-face-attribute 'fringe nil
;;                       :background nil)
;;   (set-face-attribute 'ivy-current-match nil
;;                       :background (plist-get base16-wal-colors :base02)
;;                       :foreground (face-attribute 'default :background))
;;   (set-face-attribute 'company-tooltip nil
;;                       :foreground (plist-get base16-wal-colors :base02)
;;                       :background (face-attribute 'default :foreground))
;;   (set-face-attribute 'company-tooltip-common nil
;;                       :foreground (face-attribute 'default :background))
;;   (set-face-attribute 'line-number-current-line nil
;;                       :foreground (plist-get base16-wal-colors :base03)
;;                       :background (face-attribute 'default :background))
;;   (set-face-attribute 'org-column nil
;;                       :background (face-attribute 'default :background)))

;;; tools

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(setq smerge-command-prefix (kbd "C-c v"))
(setq ediff-window-setup-function #'ediff-setup-windows-plain)

(use-package smex)

(use-package ivy
  :pin elpa
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "")
  (setq ivy-height 5)
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order))))

(use-package company
  :config
  (use-package company-quickhelp)
  (global-company-mode 1)
  (setq company-minimum-prefix-length 3))

(use-package rainbow-mode
  :config (rainbow-mode))

(use-package slime
  :config (progn
            (setq inferior-lisp-program "/usr/bin/sbcl")
            (setq slime-contribs '(slime-fancy))))

(use-package sml-mode
  :config (setq sml-program-name "~/sml/bin/sml"))

(use-package haskell-mode)

(load-init-file "org-init.el")

;; python-mode
(use-package flymake)
(use-package elpy
  :config
  (elpy-enable)
  (use-package company-jedi)
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi)
    (hs-minor-mode 1)
    (flymake-mode 1))
  (add-hook 'python-mode-hook 'my/python-mode-hook)
  (setq elpy-rpc-backend "jedi")
  (add-to-list 'rm-whitelist " Elpy"))

;; c++-mode
(load-init-file "google-c-style.el")
(add-hook 'c++-mode-common-hook 'google-set-c-style)

(use-package php-mode
  :ensure)

;; tabs
;; https://dougie.io/emacs/indentation/
(setq-default default-tab-width 2)
(setq-default tab-width 2)

(defun disable-tabs ()
  (interactive)
  (setq indent-tabs-mode nil)
  (setq evil-indent-tabs-mode nil))

(defun enable-tabs  ()
  (interactive)
  (setq indent-tabs-mode t)
  (setq evil-indent-tabs-mode t))

(setq-default evil-indent-convert-tabs t)
(setq-default evil-shift-width tab-width)

;;; disable tabs in all modes by default
(add-hook 'prog-mode-hook 'disable-tabs)

(use-package evil
  :config (progn
            ;; Esc anything
            (define-key evil-normal-state-map [escape] 'keyboard-quit)
            (define-key evil-visual-state-map [escape] 'keyboard-quit)
            (define-key minibuffer-local-map [escape] 'keyboard-escape-quit)
            (define-key minibuffer-local-ns-map [escape] 'keyboard-escape-quit)
            (define-key minibuffer-local-completion-map [escape] 'keyboard-escape-quit)
            (define-key minibuffer-local-must-match-map [escape] 'keyboard-escape-quit)
            (define-key minibuffer-local-isearch-map [escape] 'keyboard-escape-quit)
            (define-key minibuffer-local-isearch-map [escape] 'keyboard-escape-quit)
            (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)

            ;; Smex
            (define-key evil-motion-state-map ";" 'counsel-M-x)


            ;; j/k on visual lines
            (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
            (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

            ;; Keysmash escape
            (use-package key-chord
              :pin melpa-stable
              :config (progn
                        (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
                        (key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
                        (key-chord-mode 1)))

            (use-package evil-surround
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
            :pin melpa-stable
            :config (progn
                      (evil-leader/set-leader "<SPC>")
                      (evil-leader/set-key
                        "a" 'org-agenda-list
                        "t" 'org-todo-list
                        "c" 'counsel-org-capture)
                      (global-evil-leader-mode)))

          (evil-mode 1)))

(use-package exec-path-from-shell
  :config (exec-path-from-shell-copy-env "PATH"))

;;; backup files (https://www.emacswiki.org/emacs/BackupDirectory#toc2)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; misc
(add-hook 'before-save-hook 'whitespace-cleanup)
(setq vc-follow-symlinks t)
(setq mode-line-in-non-selected-windows nil)
(setq minibuffer-message-timeout 0)
(setq-default indent-tabs-mode nil)
(setq x-select-enable-clipboard t)
(setq help-window-select t)

;; don't close window on buffer delete
(defun suppress-window-delete (orig-fun &rest args)
    (noflet ((delete-window nil)) (apply orig-fun args)))
(advice-add 'evil-delete-buffer :around #'suppress-window-delete)

;; notify statusbar to refresh emacs-related elements
(defun notify-panel (&rest args)
  (call-process-shell-command "echo \"E\" > \"$PANEL_FIFO\"&"))
