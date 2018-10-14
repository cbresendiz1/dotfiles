(load "package")
(package-initialize)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(defvar charly/packages '(auto-complete
			   autopair
                           use-package
			   flycheck
			   haml-mode
			   multiple-cursors
			   ensime
			   intero
			   org
			   dionysos
			   paredit
			   smex
			   solarized-theme
			   markdown-mode
			   scala-mode
			   rainbow-identifiers
			   flyspell
			   writegood-mode
			   dracula-theme
			   restclient)
  "Default packages")

(load "~/.emacs.d/userinfo.el")
(load "~/.emacs.d/pathConfig.el")
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

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
(menu-bar-mode -1)
(delete-selection-mode t)
(transient-mark-mode t)

(defun charly/packages-installed-p ()
  (loop for pkg in charly/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))


(unless (charly/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg charly/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

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




(setq dionysos-backend 'vlc
      dionysos-notify-p t
      dionysos-volume-cmd 'pamixer)
(setq ido-use-filename-at-point 'guess)
(setq ido-use-url-at-point nil)
(display-time-mode 1)

(setq org-todo-keywords
 '((sequence
    "TODO(t)"  ; next action
    "TOBLOG(b)"  ; next action
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


(setq
  ensime-sbt-command "/usr/bin/sbt"
  sbt:program-name "/usr/bin/sbt")


(setq make-backup-files nil)
(setq column-number-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "C-c m c") 'mc/edit-lines)

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
    (replace-regexp re "" nil beg end)))

(global-set-key (kbd "C-x M-t") 'cleanup-region)
(global-set-key (kbd "C-c n") 'cleanup-buffer)

(setq-default show-trailing-whitespace t)


(setq package-archive-enable-alist '(("melpa" magit f)))

(setq org-log-done t
      org-todo-keywords '((sequence "TODO" "INPROGRESS" "DONE"))
      org-todo-keyword-faces '(("INPROGRESS" . (:foreground "blue" :weight bold))))
(add-hook 'org-mode-hook
          (lambda ()
            (flyspell-mode)))
(add-hook 'org-mode-hook
          (lambda ()
            (writegood-mode)))

(setq custom-file (make-temp-file "emacs-custom"))

;; (require 'solarized)
(load "~/.emacs.d/.startup-theme")
