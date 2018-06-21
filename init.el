;; Require Emacs' package functionality
(require 'package)

;; Add the Melpa repository to the list of package sources
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; Initialise the package system.
(package-initialize)

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)

;; EL: it is _important_ that the custom-set-variables stuff is before we start loading themes.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(desktop-save-mode t)
 '(safe-local-variable-values (quote ((c-default-style . "linux")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package ace-window
  :ensure t
  :pin melpa-stable
  :bind ("M-o" . ace-window))

(use-package zenburn-theme
  :ensure t)

(use-package undo-tree
  :ensure t
  :config (progn (global-undo-tree-mode)))

(use-package ido
  :ensure t
  :config
  (progn
    (setq ido-use-filename-at-point nil)
    (setq ido-enable-flex-matching t)
    (setq ido-everywhere t)
    (ido-mode 1)))

(use-package ggtags
  :ensure t
  :config
  (progn
    (add-hook
     'c-mode-common-hook
     (lambda ()
       (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
         (ggtags-mode 1))))))

(use-package smart-mode-line
  :ensure t
  :config
  (progn (sml/setup)))

;; Only enable the powerline eye-candy when in graphics mode
(when (display-graphic-p)
  (use-package smart-mode-line-powerline-theme
    :ensure t))

(use-package projectile
  :ensure t
  :pin melpa-stable
  :config (progn (projectile-global-mode)))

(use-package org
  :ensure t
  :bind ("M-q" . org-fill-paragraph)
  :pin org
  :config
  (progn
    (global-set-key (kbd "<f12>") 'org-agenda)
    (global-set-key (kbd "C-c c") 'org-capture)
    (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
    (setq org-agenda-window-setup (quote current-window))
    (setq org-directory (quote "~/org/agenda"))
    (setq org-agenda-files (quote ("~/org/agenda")))
    (setq org-default-notes-file (concat org-directory "/refile.org"))
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
          '(("t" "todo" entry (file+headline "" "Tasks")
             "* TODO [#A] %?")))
    (setq org-log-done 'time)
    ))

(use-package fill-column-indicator
  :ensure t
  :config
  (progn (setq fci-rule-color "gray")))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package s
  :pin melpa-stable
  :ensure t)

(use-package company
  :ensure t
  :defer t
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
      company-transformers)
  )
)

;; Macbook "command" key should be meta because it's next to "space" where alt normally is
(setq ns-command-modifier (quote meta))

;; Show matching parens
(show-paren-mode 1)

;; Enable nice shortcuts for windmove
(windmove-default-keybindings)

;; Remove trailing whitespace from c/c++ code
;;(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'c-mode-hook (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
(add-hook 'python-mode-hook (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))
(add-hook 'tuareg-mode-hook (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

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

;; Org-mode tables are best without truncation, but prose looks best with.
(global-set-key (kbd "<f1>") 'toggle-truncate-lines)
;; Insert current time
(global-set-key (kbd "<f2>") 'esel/now)

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

;; REVISIT not entirely confident about this...
;; Prevent compilation mode and grep mode from opening a strange amount of windows.
(setq split-height-threshold nil)
(setq split-width-threshold nil)

;; Ocaml configuration.
 (push
  (format "%s/share/emacs/site-lisp" (s-trim (shell-command-to-string "opam config var prefix 2> /dev/null")))
  load-path)
(require 'ocp-indent)
(autoload 'merlin-mode "merlin" "Merlin mode" t)
(add-hook 'tuareg-mode-hook 'merlin-mode)
(add-hook 'caml-mode-hook 'merlin-mode)

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
