(require 'org)
(setq org-log-done t)
(setq org-agenda-files '("~/org"))
(setq org-agenda-window-setup 'current-window)
(setq org-todo-keywords
      '((sequence "TODO" "BLOCK" "|" "DONE" "DELEG")))
(setq org-global-properties
      '(("Effort_ALL" .
         "0:05 0:15 0:30 0:45 1:00 2:00 6:00 10:00")))
