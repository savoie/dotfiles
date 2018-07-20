(require 'org)

(setq org-log-done t)
(setq org-agenda-files '("~/org"))
(setq org-agenda-window-setup 'current-window)
(setq org-todo-keywords
      '((sequence "TODO" "BLOCK" "|" "DONE" "DELEG")))
(setq org-global-properties
      '(("Effort_ALL" .
         "0:05 0:15 0:30 0:45 1:00 2:00 6:00 10:00")))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/todo.org" "unsorted")
         "* TODO %?\n  %i" :kill-buffer t)
        ("c" "Config Todo" entry (file "~/org/config.org")
         "* TODO %?\n  %i" :kill-buffer t)
        ("n" "Note" entry (file "~/org/notes.org")
         "* %?\n  %i" :kill-buffer t)))
(setq org-enforce-todo-dependencies t)
(setq org-agenda-dim-blocked-tasks 'invisible)


; close previously-unopened buffers on agenda quit
(defun my-org-agenda-mode-hook ()
    (local-set-key (kbd "q") 'org-agenda-exit))
(add-hook 'org-agenda-mode-hook 'my-org-agenda-mode-hook)
(setq org-columns-default-format "%50ITEM %5CLOCKSUM %6EFFORT %DEADLINE")

(use-package org-super-agenda
  :config
  ; https://github.com/alphapapa/org-super-agenda/pull/38
  (org-super-agenda--defgroup category
    "Group items that match any of the given categories.
Argument may be a string or list of strings."
    :section-name (concat "Items categorized as: " (s-join " OR " args))
    :test (cl-member (org-super-agenda--when-with-marker-buffer
                       (org-super-agenda--get-marker item)
                       (org-get-category))
                     args :test 'string=))
  (org-super-agenda-mode 1)
  (setq org-super-agenda-groups
        '((:name "director emails"
                 :and (:category "directors" :todo "CHECKIN"))
          (:name "unfiled" :not (:category "todo") :order 1)
          (:and (:effort< "0:29" :deadline past))
          (:and (:effort< "0:29" :deadline today))
          (:and (:effort< "0:59" :deadline past))
          (:and (:effort< "0:59" :deadline today))
          (:deadline past)
          (:deadline today)
          (:and (:effort< "0:29" :deadline future))
          (:and (:effort< "0:59" :deadline future))
          (:deadline future)
          (:name "deadline not set" :anything))))
