(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

(package-initialize)
;; update the package metadata is the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(blink-cursor-mode -1)

(setq ring-bell-function 'ignore)

(setq inhibit-startup-screen t)

(use-package autorevert
  :ensure f
  :diminish t
  :hook
  (dired-mode . auto-revert-mode)
  :config
  (global-auto-revert-mode +1)
  :custom
  (auto-revert-verbose nil))

(setq custom-file "~/.emacs.d/custom.el")

(defalias 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode 1)

(setq sentence-end-double-space nil)
(setq-default tab-width 2)
(setq-default js-indent-level 2)
(setq-default indent-tabs-mode nil)

(setq-default truncate-lines t)

(defun jethro/truncate-lines-hook ()
  (setq truncate-lines nil))

(add-hook 'text-mode-hook 'jethro/truncate-lines-hook)
(setq create-lockfiles nil)

(use-package tao-theme
  :init
  (load-theme 'tao-yang t))

(use-package rainbow-delimiters
  :defer 5
  :hook
  (prog-mode . rainbow-delimiters-mode)
  :config
  (rainbow-delimiters-mode +1)
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :inherit 'error))

(use-package counsel
  :hook
  (after-init . ivy-mode)
  :diminish ivy-mode
  :bind
  (("C-c C-r" . ivy-resume)
   ("M-x" . counsel-M-x)
   ("C-c i" . counsel-imenu)
   ("C-x b" . ivy-switch-buffer)
   ("C-x B" . ivy-switch-buffer-other-window)
   ("C-x k" . kill-buffer)
   ("C-x C-f" . counsel-find-file)
   ("C-x l" . counsel-locate)
   ("C-c j" . counsel-git)
   ("M-y" . counsel-yank-pop)
   :map help-map
   ("f" . counsel-describe-function)
   ("v" . counsel-describe-variable)
   ("l" . counsel-info-lookup-symbol)
   :map ivy-minibuffer-map
   ("C-o" . ivy-occur)
   ("<return>" . ivy-alt-done)
   ("M-<return>" . ivy-immediate-done)
   :map read-expression-map
   ("C-r" . counsel-minibuffer-history))
  :custom
  (counsel-find-file-at-point t)
  (ivy-use-virtual-buffers t)
  (ivy-display-style 'fancy)
  (ivy-use-selectable-prompt t)
  (ivy-re-builders-alist
   '((ivy-switch-buffer . ivy--regex-plus)
     (swiper . ivy--regex-plus)
     (t . ivy--regex-fuzzy)))
  :config
  (ivy-set-actions
   t
   '(("I" insert "insert")))
  (ivy-set-occur 'ivy-switch-buffer 'ivy-switch-buffer-occur))

(use-package swiper
  :bind
  (("C-s" . swiper-isearch)
   ("C-r" . swiper-isearch)
   ("C-c C-s" . counsel-grep-or-swiper)
   :map swiper-map
   ("M-q" . swiper-query-replace)
   ("C-l". swiper-recenter-top-bottom)
   ("C-'" . swiper-avy))
  :custom
  (counsel-grep-swiper-limit 20000)
  (counsel-rg-base-command
   "rg -i -M 120 --no-heading --line-number --color never %s .")
  (counsel-grep-base-command
   "rg -i -M 120 --no-heading --line-number --color never '%s' %s"))

(use-package ws-butler
  :diminish 'ws-butler-mode
  :hook
  (prog-mode . ws-butler-mode))

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package easy-kill
  :bind*
  (([remap kill-ring-save] . easy-kill)))

(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :config
  (global-aggressive-indent-mode +1)
  :custom
  (aggressive-indent-excluded-modes
   '(bibtex-mode
     cider-repl-mode
     c-mode
     c++-mode
     coffee-mode
     comint-mode
     conf-mode
     Custom-mode
     diff-mode
     doc-view-mode
     dos-mode
     erc-mode
     jabber-chat-mode
     haml-mode
     intero-mode
     haskell-mode
     interative-haskell-mode
     haskell-interactive-mode
     image-mode
     makefile-mode
     makefile-gmake-mode
     minibuffer-inactive-mode
     nix-mode
     netcmd-mode
     python-mode
     sass-mode
     slim-mode
     special-mode
     shell-mode
     snippet-mode
     eshell-mode
     tabulated-list-mode
     term-mode
     TeX-output-mode
     text-mode
     yaml-mode)))

(bind-key "M-/" 'hippie-expand)

(setq hippie-expand-try-functions-list
      '(yas-hippie-try-expand
        try-expand-all-abbrevs
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-from-kill
        try-expand-dabbrev-all-buffers
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(global-set-key [remap fill-paragraph]
                #'endless/fill-or-unfill)

(use-package dtrt-indent
  :diminish t
  :config
  (dtrt-indent-mode +1))

(use-package org
  :ensure org-plus-contrib
  :mode ("\\.org\\'" . org-mode)
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c b" . org-iswitchb)
   ("C-c c" . org-capture))
  :bind
  (:map org-mode-map
        ("M-n" . outline-next-visible-heading)
        ("M-p" . outline-previous-visible-heading))
  :custom
  (org-src-window-setup 'current-window)
  (org-return-follows-link t)
  (org-agenda-diary-file "~/storage/shared/org/diary.org")
  (org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))
  (org-use-speed-commands t)
  (org-catch-invisible-edits 'show)
  :custom-face
  (org-document-title ((t (:weight bold :height 1.5))))
  (org-done ((t (:strike-through t :weight bold))))
  (org-headline-done ((t (:strike-through t))))
  (org-level-1 ((t (:height 1.3))))
  (org-level-2 ((t (:height 1.2))))
  (org-level-3 ((t (:height 1.1))))
  (org-image-actual-width (/ (display-pixel-width) 2))
  :custom
  (org-structure-template-alist '(("a" . "export ascii")
                                  ("c" . "center")
                                  ("C" . "comment")
                                  ("e" . "example")
                                  ("E" . "export")
                                  ("h" . "export html")
                                  ("l" . "export latex")
                                  ("q" . "quote")
                                  ("s" . "src")
                                  ("v" . "verse")
                                  ("el" . "src emacs-lisp")
                                  ("d" . "definition")
                                  ("t" . "theorem")))
  :config
  (require 'org-habit)
  (require 'org-tempo)
  (setq org-startup-indented t
        org-hide-emphasis-markers t
        org-pretty-entities nil))

(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries 'org-archive-subtree "/DONE" 'file))

(setq jethro/org-agenda-directory "~/storage/shared/org/gtd/")
(setq org-agenda-files
      (find-lisp-find-files jethro/org-agenda-directory "\.org$"))

(setq org-capture-templates
      `(("i" "inbox" entry (file "~/.org/gtd/inbox.org")
         "* TODO %?")
        ("p" "paper" entry (file "~/.org/papers/papers.org")
         "* TODO %(jethro/trim-citation-title \"%:title\")\n%a" :immediate-finish t)
        ("e" "email" entry (file+headline "~/.org/gtd/emails.org" "Emails")
         "* TODO [#A] Reply: %a :@home:@school:" :immediate-finish t)
        ("l" "link" entry (file "~/.org/gtd/inbox.org")
         "* TODO %(org-cliplink-capture)" :immediate-finish t)
        ("z" "elfeed-link" entry (file "~/.org/gtd/inbox.org")
         "* TODO %a\n" :immediate-finish t)
        ("w" "Weekly Review" entry (file+olp+datetree "~/.org/gtd/reviews.org")
         (file "~/.org/gtd/templates/weekly_review.org"))
        ("s" "Snippet" entry (file "~/.org/deft/capture.org")
         "* Snippet %<%Y-%m-%d %H:%M>\n%?")))

(require 'org-agenda)
(setq jethro/org-agenda-inbox-view
      `("i" "Inbox" todo ""
        ((org-agenda-files '("~/.org/gtd/inbox.org")))))

(add-to-list 'org-agenda-custom-commands `,jethro/org-agenda-inbox-view)

(setq jethro/org-agenda-someday-view
      `("s" "Someday" todo ""
        ((org-agenda-files '("~/.org/gtd/someday.org")))))

(add-to-list 'org-agenda-custom-commands `,jethro/org-agenda-someday-view)

(setq jethro/org-agenda-reading-view
      `("r" "Reading" todo ""
        ((org-agenda-files '("~/.org/gtd/reading.org")))))

(add-to-list 'org-agenda-custom-commands `,jethro/org-agenda-reading-view)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-log-state-notes-insert-after-drawers nil)

(setq org-tag-alist (quote (("@errand" . ?e)
                            ("@office" . ?o)
                            ("@home" . ?h)
                            ("@school" . ?s)
                            (:newline)
                            ("WAITING" . ?w)
                            ("HOLD" . ?H)
                            ("CANCELLED" . ?c))))

(setq org-fast-tag-selection-single-key nil)

;; https://github.com/syl20bnr/spacemacs/issues/3094
(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-refile-targets '(("next.org" :level . 0)
                           ("someday.org" :level . 0)
                           ("reading.org" :level . 1)
                           ("projects.org" :maxlevel . 1)))

(defvar jethro/org-agenda-bulk-process-key ?f
  "Default key for bulk processing inbox items.")

(defun jethro/org-process-inbox ()
  "Called in org-agenda-mode, processes all inbox items."
  (interactive)
  (org-agenda-bulk-mark-regexp "inbox:")
  (jethro/bulk-process-entries))

(defun jethro/org-agenda-process-inbox-item ()
  "Process a single item in the org-agenda."
  (org-with-wide-buffer
   (org-agenda-set-tags)
   (org-agenda-priority)
   (org-agenda-set-effort)
   (org-agenda-refile nil nil t)))

(defun jethro/bulk-process-entries ()
  (if (not (null org-agenda-bulk-marked-entries))
      (let ((entries (reverse org-agenda-bulk-marked-entries))
            (processed 0)
            (skipped 0))
        (dolist (e entries)
          (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e)))
            (if (not pos)
                (progn (message "Skipping removed entry at %s" e)
                       (cl-incf skipped))
              (goto-char pos)
              (let (org-loop-over-headlines-in-active-region) (funcall 'jethro/org-agenda-process-inbox-item))
              ;; `post-command-hook' is not run yet.  We make sure any
              ;; pending log note is processed.
              (when (or (memq 'org-add-log-note (default-value 'post-command-hook))
                        (memq 'org-add-log-note post-command-hook))
                (org-add-log-note))
              (cl-incf processed))))
        (org-agenda-redo)
        (unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
        (message "Acted on %d entries%s%s"
                 processed
                 (if (= skipped 0)
                     ""
                   (format ", skipped %d (disappeared before their turn)"
                           skipped))
                 (if (not org-agenda-persistent-marks) "" " (kept marked)")))
    ))



(defun jethro/org-inbox-capture ()
  (interactive)
  "Capture a task in agenda mode."
  (org-capture nil "i"))

(setq org-agenda-bulk-custom-functions `((,jethro/org-agenda-bulk-process-key jethro/org-agenda-process-inbox-item)))

(define-key org-agenda-mode-map "i" 'org-agenda-clock-in)
(define-key org-agenda-mode-map "r" 'jethro/org-process-inbox)
(define-key org-agenda-mode-map "R" 'org-agenda-refile)
(define-key org-agenda-mode-map "c" 'jethro/org-inbox-capture)

(defun jethro/set-todo-state-next ()
  "Visit each parent task and change NEXT states to TODO"
  (org-todo "NEXT"))

(add-hook 'org-clock-in-hook 'jethro/set-todo-state-next 'append)

(use-package org-clock-convenience
  :bind (:map org-agenda-mode-map
              ("<S-up>" . org-clock-convenience-timestamp-up)
              ("<S-down>" . org-clock-convenience-timestamp-down)
              ("o" . org-clock-convenience-fill-gap)
              ("e" . org-clock-convenience-fill-gap-both)))

(setq org-agenda-block-separator nil)
(setq org-agenda-start-with-log-mode t)

(setq jethro/org-agenda-todo-view
      `(" " "Agenda"
        ((agenda ""
                 ((org-agenda-span 'day)
                  (org-deadline-warning-days 365)))
         (todo "TODO"
               ((org-agenda-overriding-header "To Refile")
                (org-agenda-files '("~/.org/gtd/inbox.org"))))
         (todo "TODO"
               ((org-agenda-overriding-header "Emails")
                (org-agenda-files '("~/.org/gtd/emails.org"))))
         (todo "NEXT"
               ((org-agenda-overriding-header "In Progress")
                (org-agenda-files '("~/.org/gtd/someday.org"
                                    "~/.org/gtd/projects.org"
                                    "~/.org/gtd/next.org"))
                ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))
                ))
         (todo "TODO"
               ((org-agenda-overriding-header "Projects")
                (org-agenda-files '("~/.org/gtd/projects.org"))
                ;; (org-agenda-skip-function #'jethro/org-agenda-skip-all-siblings-but-first)
                ))
         (todo "TODO"
               ((org-agenda-overriding-header "One-off Tasks")
                (org-agenda-files '("~/.org/gtd/next.org"))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
         nil)))

(add-to-list 'org-agenda-custom-commands `,jethro/org-agenda-todo-view)

(defun jethro/org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (or (org-current-is-todo)
                (not (org-get-scheduled-time (point))))
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-current-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))

(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))

(defun jethro/switch-to-agenda ()
  (interactive)
  (org-agenda nil " ")
  (delete-other-windows))

(bind-key "<f1>" 'jethro/switch-to-agenda)

(setq org-columns-default-format "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")

(use-package org-pomodoro
  :after org
  :bind
  (:map org-agenda-mode-map
        (("I" . org-pomodoro)))
  :custom
  (org-pomodoro-format "%s"))

(use-package org-cliplink
  :bind
  ("C-c C" . 'jethro/org-capture-link)
  :config
  (defun jethro/org-capture-link ()
    "Captures a link, and stores it in inbox."
    (interactive)
    (org-capture nil "l")))

(use-package deft
  :after org
  :bind
  (("C-c n" . deft))
  :custom
  (deft-default-extension "org")
  (deft-directory "~/storage/shared/org/braindump/org")
  (deft-use-filename-as-title t))
