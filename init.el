;; Require Emacs' package functionality
(require 'package)

;; Add the Melpa repository to the list of package sources
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

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
 '(custom-safe-themes
   (quote
    ("6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default)))
 '(desktop-save-mode t)
 '(safe-local-variable-values (quote ((c-default-style . "linux")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package graphene
  :ensure t)

(use-package magit
  :ensure t)

(use-package zenburn-theme
  :ensure t)

(use-package undo-tree
  :ensure t
  :config (progn (global-undo-tree-mode)))

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

(use-package recentf
  :ensure t
  :config
  (progn (recentf-mode 1)
         (setq recentf-max-menu-items 1000)))

(use-package savehist
  :ensure t
  :config
  (progn (savehist-mode 1)))

(use-package saveplace
  :ensure t
  :config
  (progn (setq-default save-place t)))

(use-package projectile
  :ensure t
  :pin melpa-stable
  :config (progn (projectile-global-mode)))

(use-package fill-column-indicator
  :ensure t
  :config
  (progn (setq fci-rule-color "gray")))

;; Enable nice shortcuts for windmove
(windmove-default-keybindings)

;; Remove trailing whitespace from c/c++ code
;;(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'c-mode-hook
          (lambda () (add-to-list
                      'write-file-functions
                      'delete-trailing-whitespace)))

;; Enable C-c o for switching between header and implementation.
(add-hook 'c-mode-common-hook
          (lambda() (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

;; Column numbers are nice.
(column-number-mode)

;; Highlight current line
(global-hl-line-mode)

;; Default c style is linux with 4 spaces for indentation.
(setq c-default-style "linux"
      c-basic-offset 4)
