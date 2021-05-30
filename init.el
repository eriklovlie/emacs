(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
(url-retrieve-synchronously
"https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; From https://oremacs.com/2015/01/17/setting-up-ediff/
(defmacro csetq (variable value)
  `(funcall (or (get ',variable 'custom-set)
                'set-default)
            ',variable ,value))

(use-package guix)

(use-package geiser)

(use-package undo-tree
  :config (progn (global-undo-tree-mode)))

;; NOTE: I don't want to use ivy since I prefer selectrum and friends.
;; However I do like some functions in counsel. I can pull in counsel
;; without enabling (ivy-mode) or (counsel-mode) and it seems to work
;; fine.
(use-package counsel)

(use-package selectrum
  :config (selectrum-mode +1))

(use-package prescient)
(use-package company-prescient)
(use-package selectrum-prescient
  :config
  (progn
    (company-prescient-mode +1)
    (selectrum-prescient-mode +1)
    (prescient-persist-mode +1)))

(use-package ctrlf
  :config (ctrlf-mode +1))

(use-package deadgrep
  :bind ("<f5>" . deadgrep))

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch))
  :config
  (progn
    ;; Disabled submodule section because it slows things down quite a bit.
    ;;(magit-add-section-hook 'magit-status-sections-hook 'magit-insert-modules nil t nil)
    ;; Ensure we can write commit messages in org-mode!
    (setq git-commit-major-mode 'org-mode)))

(use-package ace-window
  :bind ("M-o" . ace-window))

(use-package beacon
  :config
  (beacon-mode 1))

(use-package company
  :init (global-company-mode)
  :config
  (progn
    ;; Use Company for completion
    (bind-key [remap completion-at-point] #'company-complete company-mode-map)

    ;; Disable silly completions
    (push (apply-partially #'cl-remove-if
                         (lambda (c)
                         (or (string-match-p "[^\x00-\x7F]+" c) ;; remove non-ansi strings
                             (string-match-p "^[0-9]+" c))))    ;; remove strings starting with numbers
          company-transformers)))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)
  (define-key company-active-map (kbd "C-j") #'company-complete))

(use-package smartparens
  :config
  (progn
    (show-smartparens-global-mode t)
    (define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
    (define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)
    (define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
    (define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
    (define-key smartparens-mode-map (kbd "C-S-d") 'sp-beginning-of-sexp)
    (define-key smartparens-mode-map (kbd "C-S-a") 'sp-end-of-sexp)
    (define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)
    (define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
    (define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)
    (define-key smartparens-mode-map (kbd "C-M-n") 'sp-forward-hybrid-sexp)
    (define-key smartparens-mode-map (kbd "C-M-p") 'sp-backward-hybrid-sexp)
    (define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
    (define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)
    (define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
    (define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)
    (define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
    (define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
    (define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
    (define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-backward-barf-sexp)
    (define-key smartparens-mode-map (kbd "M-D") 'sp-splice-sexp)
    (define-key smartparens-mode-map (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
    (define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
    (define-key smartparens-mode-map (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)
    (define-key smartparens-mode-map (kbd "C-]") 'sp-select-next-thing-exchange)
    (define-key smartparens-mode-map (kbd "C-<left_bracket>") 'sp-select-previous-thing)
    (define-key smartparens-mode-map (kbd "C-M-]") 'sp-select-next-thing)
    (define-key smartparens-mode-map (kbd "M-F") 'sp-forward-symbol)
    (define-key smartparens-mode-map (kbd "M-B") 'sp-backward-symbol)
    (define-key smartparens-mode-map (kbd "C-\"") 'sp-change-inner)
    (define-key smartparens-mode-map (kbd "M-i") 'sp-change-enclosing)))

(add-hook 'prog-mode-hook 'turn-on-smartparens-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-mode)

(use-package monokai-theme
  :config
  (load-theme 'monokai t))

(use-package tuareg)

(use-package ocp-indent
  :after tuareg)

(use-package merlin
  :after tuareg
  :config
  (add-hook 'tuareg-mode-hook 'merlin-mode t))

(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$"
  :config (csetq scala-indent:step 4))

(use-package htmlize)

(use-package org
  :after htmlize
  :config
  (progn
    (global-set-key (kbd "<f1>") 'org-clock-in-last)
    (global-set-key (kbd "<f2>") 'org-clock-out)
    (global-set-key (kbd "<f12>") 'org-agenda)
    (global-set-key (kbd "C-c c") 'org-capture)
    (global-set-key (kbd "C-c s") 'org-insert-structure-template)
    (global-set-key (kbd "C-c l") 'org-store-link)

    (setq org-agenda-window-setup (quote current-window))
    (setq org-directory (quote "~/org"))
    (setq org-agenda-files (quote ("~/org")))
    (setq org-default-notes-file (concat org-directory "/refile.org"))
    (setq org-adapt-indentation nil)
    (setq org-completion-use-ido t)
    ;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
    (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                     (org-agenda-files :maxlevel . 9))))
    ;; Use full outline paths for refile targets - we file directly with IDO
    (setq org-refile-use-outline-path t)
    ;; Targets complete directly with IDO
    (setq org-outline-path-complete-in-steps nil)
    ;; Allow refile to create parent tasks with confirmation
    (setq org-refile-allow-creating-parent-nodes (quote confirm))
    ;;set priority range from A to C with default A
    (setq org-highest-priority ?A)
    (setq org-lowest-priority ?C)
    (setq org-default-priority ?A)
    ;;set colours for priorities
    (setq org-priority-faces '((?A . (:foreground "#F0DFAF" :weight bold))
                               (?B . (:foreground "LightSteelBlue"))
                               (?C . (:foreground "OliveDrab"))))
    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
             "* TODO [#A] %?")
            ("j" "Journal" entry (file+datetree "~/org/journal.org")
             "* %U\n  %?")
            ("i" "Idea" entry (file+headline "~/org/idea.org" "Ideas")
             "* %?\n  %U")
            ("r" "Read" entry (file+headline "~/org/read.org" "Read")
             "* %?\n  %U")))
    (setq org-log-done 'time)
    (setq org-log-into-drawer t)
    (setq org-babel-python-command "python3")
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)
       (shell . t)))))

(use-package org-roam
  :after org
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/org/ideas")
  :bind (:map org-roam-mode-map
              (("C-c z l" . org-roam)
               ("C-c z f" . org-roam-find-file)
               ("C-c z g" . org-roam-show-graph))
              :map org-mode-map
              (("C-c z i" . org-roam-insert))))

(use-package deft
  :bind ("<f8>" . deft)
  :commands (deft)
  :config
  (setq deft-auto-save-interval 0)
  (setq deft-default-extension "org")
  (setq deft-directory "~/org/ideas"))

;; ===============================================================
;; Misc
;; ===============================================================

;; Macbook "command" key should be meta because it's next to "space" where alt normally is
(setq ns-command-modifier (quote meta))

;; Show matching parens
(show-paren-mode 1)

;; Enable nice shortcuts for windmove
(windmove-default-keybindings)

;; Remove trailing whitespace from code
(add-hook 'prog-mode-hook (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

;; Enable C-c o for switching between header and implementation.
(add-hook 'c-mode-common-hook
          (lambda() (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

;; https://www.masteringemacs.org/article/my-emacs-keybindings
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<S-C-left>") 'shrink-window-horizontally)
(global-set-key (kbd "<S-C-down>") 'shrink-window)
(global-set-key (kbd "<S-C-right>") (lambda () (interactive) (shrink-window-horizontally -1)))
(global-set-key (kbd "<S-C-up>") (lambda () (interactive) (shrink-window -1)))
(global-set-key (kbd "<S-C-up>") (lambda () (interactive) (shrink-window -1)))
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)
(global-set-key (kbd "C-c n") 'esel/narrow-or-widen-dwim)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Enable C-x C-j to jump to current file in a dired buffer.
(require 'dired-x)

;; Column numbers are nice.
(column-number-mode)

;; Highlight current line
(global-hl-line-mode)

;; Auto-revert files changed by an external program.
(global-auto-revert-mode 1)

;; Default c style is linux with 4 spaces for indentation.
(setq c-default-style "linux"
      c-basic-offset 4)

;; Enable dir-locals for tramp files.
(setq enable-remote-dir-locals t)

;; Smooth-ish scrolling.
(setq scroll-conservatively 10000)

(setq-default indent-tabs-mode nil)

(setq-default history-length 1000)
(savehist-mode t)

;; Use magit, disable builtin vc mode to prevent perf issues
(setq vc-handled-backends nil)

;; REVISIT not entirely confident about this...
;; Prevent compilation mode and grep mode from opening a strange amount of windows.
(setq split-height-threshold nil)
(setq split-width-threshold nil)

(setq compilation-scroll-output 'first-error)

;; Make dired open in the same window when using RET or ^
(put 'dired-find-alternate-file 'disabled nil) ; disables warning
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file) ; was dired-advertised-find-file
(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))  ; was dired-up-directory

;; Large json files can be really slow unless we use text-mode.
(add-to-list 'auto-mode-alist '("\\.json\\'" . text-mode))

;; Handy function for reverting all buffers (e.g. when switching git branches)
;; Source: http://www.emacswiki.org/emacs/RevertBuffer#toc2
(defun esel/revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files.") )

;; https://www.masteringemacs.org/article/find-files-faster-recent-files-package
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(defun esel/now ()
  "Insert string for the current time formatted like '[HH:MM]'."
  (interactive)
  (insert (format-time-string "[%H:%M] ")))

(defun esel/kill-dired-buffers ()
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

(defun esel/sync-gpu-submodules ()
  (interactive)
  (magit-shell-command-topdir "git submodule sync && git submodule update --init --force"))

(defun esel/narrow-or-widen-dwim (p)
    "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or defun,
whichever applies first. Narrowing to org-src-block actually
calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
    (interactive "P")
    (declare (interactive-only))
    (cond ((and (buffer-narrowed-p) (not p)) (widen))
          ((region-active-p)
           (narrow-to-region (region-beginning)
                             (region-end)))
          ((derived-mode-p 'org-mode)
           ;; `org-edit-src-code' is not a real narrowing
           ;; command. Remove this first conditional if
           ;; you don't want it.
           (cond ((ignore-errors (org-edit-src-code) t)
                  (delete-other-windows))
                 ((ignore-errors (org-narrow-to-block) t))
                 (t (org-narrow-to-subtree))))
          ((derived-mode-p 'latex-mode)
           (LaTeX-narrow-to-environment))
                  (t (narrow-to-defun))))

;; ediff usability enhancements, from https://oremacs.com/2015/01/17/setting-up-ediff/
(csetq ediff-window-setup-function 'ediff-setup-windows-plain)
(csetq ediff-split-window-function 'split-window-horizontally)
(csetq ediff-diff-options "-w")

(winner-mode)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

(csetq magit-diff-use-overlays nil)
(csetq magit-log-margin-show-committer-date t)
(csetq org-confirm-babel-evaluate nil)
(csetq send-mail-function 'sendmail-send-it)
(csetq sentence-end-double-space nil)
(csetq org-duration-format 'h:mm)

(setq ivy-use-virtual-buffers t)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-c b") 'counsel-bookmark)
(global-set-key (kbd "C-c g") 'counsel-git)

;; System locale to use for formatting time values.
; Make sure that the weekdays in the time stamps of your Org mode
; files and in the agenda appear in English.
(setq system-time-locale "C")

(ignore-errors
  (push (format "%s/share/emacs/site-lisp"
                (string-trim (shell-command-to-string "opam config var prefix 2> /dev/null")))
        load-path))

;; Multi-file search-and-replace from dired should use the function
;; that respects emacs variables.
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "Q") 'dired-do-query-replace-regexp))

(defun esel/dont-kill-emacs()
  "Disable C-x C-c binding execute kill-emacs."
  (interactive)
  (error (substitute-command-keys "To exit emacs: \\[kill-emacs]")))
(global-set-key (kbd "C-x C-c") 'esel/dont-kill-emacs)

;; ============================================================
;; Automatic stuff below.
;; ============================================================

