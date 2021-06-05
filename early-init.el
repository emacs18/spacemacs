;;; early-init.el --- Spacemacs Early Init File -*- no-byte-compile: t -*-
;;
;; Copyright (c) 2020 Sylvain Benner & Contributors
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
              "core/core-early-funcs.el")
      nil (not init-file-debug))

;; Remove GUI elements early to avoid some possible grapical glitches.
(spacemacs/removes-gui-elements)

;; Setup to use straight.el package manager rather than the default package.el.
(let* ((default-directory (file-name-directory load-file-name))
       (init-file (expand-file-name "../init-straight.el")))
  ;; Install packages under ./.local/straight directory.
  (setq straight-base-dir (expand-file-name ".local"))

  ;; ../init-straight.el is the setup file that sets emacs up to use straight.el
  ;; rather than pacakge.el. This file is in parent directory rather than
  ;; current directory, because it is used for several other startup
  ;; configurations.
  (if (file-exists-p init-file)
      (load init-file)
    (error "Error: %s does not exist" init-file))

  ;; Normally straight.el does not need to know your user names on github, gitlab,
  ;; etc. However you may need to specify it if you forked a package, and you fork
  ;; is at github or other sites.
  (setq straight-host-usernames
        '((github . "emacs18")
          (gitlab . "gitlabUser")
          (bitbucket . "bitbucketUser")))

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

  ;; If recipes are to be over-ridden, then it should be done very early on before
  ;; the default recipe is used, e.g., right about here.  Following is an example
  ;; to use my own forked package with a bug fix.
  ;;
  (straight-override-recipe
   '(hook-helpers :type git :host github :repo "emacs-straight/hook-helpers"
                  :fork (:host github :repo "emacs18/hook-helpers")
                  :branch "site" :files ("*" (:exclude ".git"))))

  ;; My company intranet is limited in that many sites are not reachable such as
  ;; https://depp.brause.cc/eyebrowse.git which is the default URL of eyebrowse
  ;; package. Hence use github mirror instead.
  (straight-override-recipe '(eyebrowse :host github :repo "emacsmirror/eyebrowse"))
  (straight-override-recipe '(paredit :host github :repo "emacsmirror/paredit"))
  (straight-override-recipe '(highlight-parentheses :host github :branch "master" :repo "emacsmirror/highlight-parentheses"))

  ;; This is to prevent emacs wasting few seconds on startup contacting package
  ;; archives which won't be needed anyways.
  (defadvice configuration-layer/retrieve-package-archives
      (around do-not-retrieve-package-archives activate)
    "Disable this function to speed up emacs startup time.")

  ;; These are needed to avoid start-up error after all installed packages are
  ;; removed.
  (straight-use-package 'bind-map)
  (straight-use-package 'evil)
  (straight-use-package 'which-key)
  (straight-use-package 'window-purpose)
  (straight-use-package 'helm-org-ql)
  (straight-use-package 'ox-gfm)
  (straight-use-package 'modus-vivendi-theme)
  (straight-use-package 'modus-operandi-theme)
  (straight-use-package 'org-re-reveal)
  (straight-use-package 'package-build)
  (straight-use-package 'pretty-hydra)
  (straight-use-package 'yasnippet-snippets)

  ;; Optional packages
  (straight-use-package '(devdocs-browser :type git :host github :repo "blahgeek/emacs-devdocs-browser"))
  )
