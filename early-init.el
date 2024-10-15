;;; early-init.el --- Spacemacs Early Init File -*- no-byte-compile: t -*-
;;
;; Copyright (c) 2020-2024 Sylvain Benner & Contributors
;;
;; Author: Miciah Dashiel Butler Masters <miciah.masters@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;; Under some conditions several spacemacs|use-package-add-hook calls have no
;; impact, e.g., within treemacs/pre-init-winum.  Such conditions may include
;; if straight.el is used rather than package.el to install packages.
(setq use-package-inject-hooks t)

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

(load (concat (file-name-directory load-file-name)
              "core/core-early-funcs")
      nil (not init-file-debug))

;; Unfortunately the hooks below prevent users from customizing gui elements
;; within dotspacemacs/user-config function. Thus the hooks are commented out.
;; These should not be needed in any case since gui elements are turned off
;; within spacemacs/init function.  Original comment follows next.
;;
;; Remove GUI elements soon after GUI being initialized to avoid some possible
;; grapical glitches. This has to be done use these hooks, see
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html
;;
;; (add-hook 'window-setup-hook 'spacemacs/toggle-gui-elements-off)
;; (add-hook 'tty-setup-hook 'spacemacs/toggle-gui-elements-off)

;; Setup to use straight.el package manager rather than the default package.el.
(let* ((default-directory (file-name-directory load-file-name))
       (init-file (expand-file-name "~/org/kimr/dot-emacs/init-straight.el")))
  ;; Install packages under ./.local/straight directory.
  (setq straight-base-dir (expand-file-name ".local"))

  ;; ../init-straight.el is the setup file that sets emacs up to use straight.el
  ;; rather than pacakge.el. This file is in parent directory rather than
  ;; current directory, because it is used for several other startup
  ;; configurations.
  (if (file-exists-p init-file)
      (load init-file)
    (error "Error: %s does not exist" init-file))

  ;; A profile is collection of package names and git version numbers. Thus a
  ;; profile identifies a very accurate state of installed packages. You can
  ;; create profiles by "freezing" current versions by calling
  ;; `straight-freeze-versions`.
  (setq straight-profiles
        '((nil . "default.el")
          ;; You can any any number of additional profiles.
          (2021-04-10 . "2021-04-10.el")
          ))

  ;; You set set which profile to use.
  ;; (setq straight-current-profile '2021-04-10)

  ;; This is to prevent emacs wasting few seconds on startup contacting package
  ;; archives which won't be needed anyways.
  (define-advice configuration-layer/retrieve-package-archives
      (:around (orig-func &rest args) do-not-retrieve-package-archives))

  ;; (straight-use-package '(eaf :type git :host github
  ;;                             :repo "emacs-eaf/emacs-application-framework"
  ;;                             :files ("*.el" "core/*.el" "extension/*.el")))
  )

;; This seems to require program called "delta" which needs to be installed.
'(use-package magit-delta
  :ensure t
  :hook (magit-mode . magit-delta-mode))
