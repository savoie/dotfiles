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

;;; appearance
(setq inhibit-startup-screen t)
(setq default-frame-alist
      '((fullscreen . maximized)
        (font . "FantasqueSansMono-12")
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (internal-border-width . 35)))
(menu-bar-mode -1)
(tool-bar-mode -1)

(use-package base16-theme
  :ensure t
  :pin melpa-stable
  :init (add-to-list 'custom-theme-load-path "~/.cache/wal/"))

(defun refresh-theme ()
  (load-theme 'base16-wal t)
  (set-face-attribute 'fringe nil
                      :background nil)
  (set-face-attribute 'ivy-current-match nil
                      :background (plist-get base16-wal-colors :base02)
                      :foreground (face-attribute 'default :background))
  (set-face-attribute 'company-tooltip nil
                      :foreground (plist-get base16-wal-colors :base02)
                      :background (face-attribute 'default :foreground))
  (set-face-attribute 'company-tooltip-common nil
                      :foreground (face-attribute 'default :background))
  (set-face-attribute 'mode-line nil
                      :background nil)
  (set-face-attribute 'linum nil
                      :background (face-attribute 'default :background)
                      :foreground (face-attribute 'default :foreground))
  (set-face-attribute 'linum-relative-current-face nil
                      :background (face-attribute 'default :background)
                      :foreground (face-attribute 'default :foreground)))

(setq-default letter-spacing 5)
(setq-default line-spacing 2)

;;; tools

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(use-package smex
  :ensure t)

(use-package ivy
  :ensure t
  :pin elpa
  :diminish ivy-mode
  :diminish counsel-mode
  :config
  (ivy-mode t)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "")
  (setq ivy-height 5)
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order))))

(use-package company
  :ensure t
  :diminish company-mode
  :config
  (use-package company-quickhelp :ensure t)
  (global-company-mode 1)
  (setq company-minimum-prefix-length 3)
  (global-set-key "\t" 'company-complete-common))

(use-package linum-relative
  :ensure t
  :pin melpa-stable
  :config (progn
            (setq linum-relative-current-symbol "")
            (set-face-bold 'linum-relative-current-face nil)
            (global-linum-mode nil)
            (linum-relative-on)))

(use-package rainbow-mode
  :ensure t
  :diminish rainbow-mode
  :config (rainbow-mode))

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
(setq org-todo-keywords
      '((sequence "TODO" "BLOCK" "|" "DONE" "DELEG")))
(setq org-global-properties
      '(("Effort_ALL" .
         "0:05 0:15 0:30 0:45 1:00 2:00 6:00 10:00")))
(setq org-agenda-custom-commands
      '(("x" "Testing"
         ((org-agenda-list)
          (org-agenda-filter-apply org-agenda-tag-filter 'yesterday)))
        ("p" "Prioritized tasks"
         ((tags-todo "yesterday")
          (tags-todo "today")
          (tags-todo "soon")
          (tags-todo "later")
          (tags-todo "5min&yesterday")
          (tags-todo "5min&today")
          (tags-todo "30min&yesterday")
          (tags-todo "30min&today")
          (tags-todo "hours&yesterday")
          (tags-todo "hours&today")
          (tags-todo "days&yesterday")
          (tags-todo "days&today")
          (tags-todo "5min&soon")
          (tags-todo "5min&later")
          (tags-todo "30min&soon")
          (tags-todo "hours&soon")
          (tags-todo "days&soon")
          (tags-todo "30min&later")
          (tags-todo "hours&later")
          (tags-todo "days&later"))
         ((org-agenda-sorting-strategy '(todo-state-up deadline-up))
          (org-agenda-overriding-columns-format "%DEADLINE %50ITEM %TODO %CLOCKSUM %EFFORT")
          (org-agenda-view-columns-initially t)))))

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
            (define-key minibuffer-local-isearch-map [escape] 'keyboard-escape-quit)
            (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)

            ;; Smex
            (define-key evil-motion-state-map ";" 'counsel-M-x)


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

;;; backup files (https://www.emacswiki.org/emacs/BackupDirectory#toc2)
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))

;;; misc
(add-hook 'before-save-hook 'whitespace-cleanup)
(setq vc-follow-symlinks t)
(setq mode-line-in-non-selected-windows nil)
(setq minibuffer-message-timeout 0)
(refresh-theme)
(diminish 'undo-tree-mode)
(setq-default indent-tabs-mode nil)
(setq x-select-enable-clipboard t)

;; set up new-frame hooks, see
;; https://www.emacswiki.org/emacs/SettingFrameColorsForEmacsClient
(require 'server)
(defadvice server-create-window-system-frame
    (after set-window-system-frame-colors ())
  (refresh-theme))
(ad-activate 'server-create-window-system-frame)
(add-hook 'after-make-frame-functions (lambda (&rest frame) (refresh-theme)))
