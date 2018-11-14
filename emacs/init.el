;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(load "package")
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
;;(package-refresh-contents)

(add-hook 'coq-mode-hook #'company-coq-mode)

(setq ido-enable-flex-matching t
      ido-everywhere t)
(setq ido-use-filename-at-point 'guess
      ido-use-url-at-point nil)
(setq dionysos-backend 'vlc
      dionysos-notify-p t
      dionysos-volume-cmd 'pamixer)
(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
(add-hook 'netlogo-mode-hook 'rainbow-identifiers-mode)


;; set key for agenda
(global-set-key (kbd "C-c a") 'org-agenda)

;;set priority range from A to C with default A
(setq org-highest-priority ?A)
(setq org-lowest-priority ?C)
(setq org-default-priority ?A)

;;set colours for priorities
(setq org-priority-faces '((?A . (:foreground "#F0DFAF" :weight bold))
                           (?B . (:foreground "LightSteelBlue"))
                           (?C . (:foreground "OliveDrab"))))

;;open agenda in current window
(setq org-agenda-window-setup (quote current-window))

(setq-default org-catch-invisible-edits 'smart)

;;capture todo items using C-c c t
(define-key global-map (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(("t" "todo" entry (file+headline "/Users/bjm/todo.org" "Tasks")
         "* TODO [#A] %?")))

(setq org-capture-templates
      '(("t" "todo" entry (file+headline "~/todo.org" "Tasks")
	 "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp
            (org-read-date nil t \"+0d\"))\n")))


;;open agenda in current window
(setq org-agenda-window-setup (quote current-window))
;;warn me of any deadlines in next 7 days
(setq org-deadline-warning-days 7)
;;show me tasks scheduled or due in next fortnight
(setq org-agenda-span (quote fortnight))
;;don't show tasks as scheduled if they are already shown as a deadline
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
;;don't give awarning colour to tasks with impending deadlines
;;if they are scheduled to be done
(setq org-agenda-skip-deadline-prewarning-if-scheduled (quote pre-scheduled))
;;don't show tasks that are scheduled or have deadlines in the
;;normal todo list
(setq org-agenda-todo-ignore-deadlines (quote all))
(setq org-agenda-todo-ignore-scheduled (quote all))
;;sort tasks in order of when they are due and then by priority
(setq org-agenda-sorting-strategy
  (quote
   ((agenda deadline-up priority-down)
    (todo priority-down category-keep)
    (tags priority-down category-keep)
    (search category-keep))))

(add-hook 'scala-mode-hook   'hs-minor-mode)
(add-hook 'netlogo-mode-hook 'hs-minor-mode)

(ido-mode 1)
(display-time-mode 1)

(use-package netlogo-mode
  :ensure nil
  :load-path "~/.config/local/emacs.d/lisp"
  :mode ("\\.nls$\\|\\.nlogo$" . netlogo-mode))

(use-package blog-admin
  :init
  (progn
    ;; your config
    (setq blog-admin-backend-type 'hexo)
    (setq blog-admin-backend-path "~/codefalling.com")
    (setq blog-admin-backend-new-post-in-drafts t)
    (setq blog-admin-backend-new-post-with-same-name-dir t)))
(add-hook 'blog-admin-backend-after-new-post-hook 'find-file)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(define-key key-translation-map (kbd "<f9> p") (kbd "φ"))
(define-key key-translation-map (kbd "<f9> p") (kbd "☃"))

(use-package org-bullets
  :ensure t
  :init
  (setq org-bullets-font-name "Inconsolata-12")
  (setq org-bullets-bullet-list
        '("◉" "◎" "⚫" "☃" "○" "►" "◇"))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


(require 'org)
(setq org-agenda-include-diary t)
(setq org-agenda-files '("~/org"
                         "~/org/blog"
			 "~/org/work/dates"))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)
(setq org-tag-alist '((:startgroup . nil)
                      ("@work" . ?w) ("@home" . ?h)
                      ("@tennisclub" . ?t)
                      (:endgroup . nil)
                      ("laptop" . ?l) ("pc" . ?p)))
(setq org-log-done t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))


(load "~/.emacs.d/lisp/PG/generic/proof-site")

(add-hook 'scala-mode-hook
      (lambda () (local-set-key (kbd "RET") 'newline)))

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C->") 'mc/skip-to-next-like-this)
(global-set-key (kbd "C-c C-<") 'mc/skip-to-previous-like-this)

;;(require 'phi-search)
;;(global-set-key (kbd "C-s") 'phi-search)
;;(global-set-key (kbd "C-r") 'phi-search-backward)

;; (setq ido-create-new-buffer 'always)
(setq org-todo-keywords
 '((sequence
    "TODO(t)"
    "TOBLOG(b)"
    "STARTED(s)"
    "WAITING(w@/!)"
    "SOMEDAY(.)" "|" "DONE(x!)" "CANCELLED(c@)")
   (sequence "LEARN" "TRY" "TEACH" "|" "COMPLETE(x)")
   (sequence "TOSKETCH" "SKETCHED" "|" "POSTED")
   (sequence "TOBUY" "TOSHRINK" "TOCUT"  "TOSEW" "|" "DONE(x)")
   (sequence "TODELEGATE(-)" "DELEGATED(d)" "|" "COMPLETE(x)")))

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "green" :weight bold))
        ("DONE" . (:foreground "cyan" :weight bold))
        ("WAITING" . (:foreground "red" :weight bold))
        ("SOMEDAY" . (:foreground "gray" :weight bold))))

(setq org-log-done 'time)

(defun find-config ()
  "Edit config.org"
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "C-c I") 'find-config)
(global-set-key (kbd "C-c M U") 'dionysos-fs-list)

(setq user-full-name "Charly Resendiz"
      user-mail-address "cbrsendiz1@gmail.com")

(setenv "PATH" (concat "/usr/local/bin:/opt/local/bin:/usr/bin:/bin:/home/charly/.cabal/bin" (getenv "PATH")))
(setenv "GOPATH" (concat (getenv "HOME") "/src/golang"))
(setq exec-path (append exec-path '("/usr/local/bin")))
(add-to-list 'exec-path (concat (getenv "GOPATH") "/bin"))
(add-to-list 'exec-path "usr/bin")
(require 'cl)

(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode
      x-select-enable-clipboard t
      tab-width 2 indent-tabs-mode nil
      ring-bell-function 'ignore
      )
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode 1)
(delete-selection-mode t)
(transient-mark-mode t)

(setq make-backup-files nil)
(setq column-number-mode t)
(fset 'yes-or-no-p 'y-or-n-p)

(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(show-paren-mode t)

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))


(defun cleanup-region (beg end)
  "Remove tmux artifacts from region."
  (interactive "r")
  (dolist (re '("\\\\│\·*\n" "\W*│\·*"))
    (replace-regexp re "" nil beg global-set-key (kbd "C-x M-t") 'cleanup-global-set-key (kbd "C-c n") 'cleanup-buffer)))

(setq-default show-trailing-whitespace t)


(setq package-archive-enable-alist '(("melpa" magit f)))

(setq org-log-done t)
(add-hook 'org-mode-hook
          (lambda ()
            (flyspell-mode)))
(add-hook 'org-mode-hook
          (lambda ()
            (writegood-mode)))
(setq indent-tabs-mode nil)

(defvar charly/packages '( auto-complete
                           autopair
                           flycheck
   			   dionysos
			   magit
                           company-coq

                           haml-mode
			   blog-admin
			   hideshow
			   fold-dwim
                           multiple-cursors
			   ensime
			   org
                           org-bullets
			   paredit
			   autotetris-mode
			   smex
			   markdown-mode
			   scala-mode
			   sbt-mode
			   multiple-cursors
			   rainbow-identifiers
			   flyspell
			   writegood-mode
			   ;; themes
			   dracula-theme
   			   solarized-theme
			   browse-kill-ring
			   restclient)
  "Default packages")

(use-package ensime
  :ensure t
  :pin melpa)

(use-package sbt-mode
  :pin melpa)

(use-package scala-mode
  :pin melpa)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))
(add-hook 'prog-mode-hook 'rainbow-identifiers-mode)
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))


(defun charly/packages-installed-p ()
  (loop for pkg in charly/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(setq custom-file (make-temp-file "emacs-custom"))

(unless (charly/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg charly/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; (load-theme 'solarized-dark)
(load "~/startup-theme")
