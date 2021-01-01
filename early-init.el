;;; early-init.el --- Spacemacs Early Init File -*- no-byte-compile: t -*-
;;
;; Copyright (c) 2020 Sylvain Benner & Contributors
;;
;; Author: Miciah Dashiel Butler Masters <miciah.masters@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Before Emacs 27, the init file was responsible for initializing the package
;; manager by calling `package-initialize'. Emacs 27 changed the default
;; behavior: It now calls `package-initialize' before loading the init file.
;; This behavior would prevent Spacemacs's own package initialization from
;; running. However, Emacs 27 also loads the "early init" file (this file)
;; before it initializes the package manager, and Spacemacs can use this early
;; init file to prevent Emacs from initializing the package manager. (See
;; <http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=24acb31c04b4048b85311d794e600ecd7ce60d3b>.)
;;
;; Earlier Emacs versions do not load the early init file and do not initialize
;; the package manager before loading the init file, so this file is neither
;; needed nor loaded on those versions.
(setq package-enable-at-startup nil)

(load "~/.emacs.d/init-straight.el")

;; These are needed after removing copies from core/libs.
(use-package dash :defer t)
(use-package ht :defer t)
(use-package ido-vertical-mode :defer t)
(use-package load-env-vars :defer t)
(use-package mocker :defer t)
(use-package package-build :defer t)
(use-package page-break-lines :defer t)
(use-package quelpa :defer t)
(use-package spinner :defer t)
