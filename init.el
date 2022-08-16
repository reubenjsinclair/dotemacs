(setq gc-cons-threshold (* 50 1000 1000))
  (defun rjs/display-startup-time ()
    (message "Emacs loaded in %s with %d garbage collections."
             (format "%.2f seconds"
                     (float-time
                      (time-subtract after-init-time before-init-time)))
             gcs-done))

  (add-hook 'emacs-startup-hook #'rjs/display-startup-time)

(require 'package)

(setq package-archives '(
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package no-littering)
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
(setq undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo/" user-emacs-directory))))

(defvar rjs/default-font-size 105)

  (desktop-save-mode 1)

  (setq find-function-C-source-directory "/home/reuben/.emacs.d/build/emacs/src")
  (setq use-short-answers t)

  (setq inhibit-startup-message t)
  (setq initial-scratch-message "")
  (set-frame-parameter (selected-frame) 'alpha '(80 . 80))
  (add-to-list 'default-frame-alist '(alpha . (80 . 80)))
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (blink-cursor-mode 0)

  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (set-fringe-mode 10)

  (menu-bar-mode -1)

  (which-function-mode 1)
  (winner-mode 1)
(setq scroll-conservatively 100)
  (setq visible-bell t)
  (setq evil-want-keybinding nil)

  (global-set-key [remap dabbrev-expand] 'hippie-expand)

(use-package doom-themes)
(load-theme 'doom-one t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom((doom-modeline-height 25)))

(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(org-mode-hook
                treemacs-mode
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


(use-package all-the-icons)

(set-face-attribute 'default nil :font "CaskaydiaCove Nerd Font Mono" :height rjs/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "CaskaydiaCove Nerd Font Mono" :height 105)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Comfortaa" :height 150 :weight 'regular)

(use-package dash
  :config (eval-after-load "dash" '(dash-enable-font-lock)))
(use-package s)
(use-package f)

(use-package general
    :after evil
    :config
    (general-evil-setup t)
    (general-create-definer rjs/leader-keys
      :keymaps '(normal insert visual emacs)
      :prefix "SPC"
      :global-prefix "C-SPC")

    (rjs/leader-keys
      "t" '(:ignore t :which-key "toggles")
      "tt" '(counsel-load-theme :which-key "choose theme")
))

(setq evil-want-keybinding nil)
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-shift-width 2)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))


(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package anzu
  :after evil
  :config
  (global-anzu-mode t)
  )

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-snipe
  :after evil
  :config
  (evil-snipe-mode t))

(use-package evil-surround
  :after evil
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-exchange
  :after evil
  :config
  (evil-exchange-install)
  )

(use-package evil-vimish-fold
  :ensure
  :after evil
  :config
  (global-evil-vimish-fold-mode t))

(use-package undo-tree
  :ensure t
  :after evil
  :diminish
  :config
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1))

(use-package which-key
  :defer 0
  :init
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

(use-package swiper
  :after ivy)
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (setq ivy-initial-inputs-alist nil)) ;; no carot on search

(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode t))
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-n" . ivy-next-line)
         ("C-e" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-n" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-n" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :config
  (ivy-rich-mode 1))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package hydra
  :defer t)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("n" text-scale-increase "in")
  ("e" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(rjs/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package vterm
  :commands vterm
  :config
  (setq vterm-shell "zsh")
  (setq vterm-max-scrollback 10000))

;;     (defun rjs/eshell-paste ()
;;   (interactive)
;; (eshell-bol)
;;    (insert " ")
;;   (evil-paste-after)
;;   )

(defun rjs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-normalize-keymaps)


  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :hook (eshell-first-time-mode . rjs/configure-eshell)
  :config
  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))
  (eshell-git-prompt-use-theme 'powerline)
  )

(defun rjs/open-eshell ()
  (interactive)
  (if (get-buffer "*eshell*")
      (progn
        (message "hi")
        (eshell 't)
        )
    (eshell)
    )
  )

(defun eshell/clear ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; (eshell-send-input)
    )
  )

(use-package define-word
  :defer t)

(use-package minimap)

(use-package dashboard)

(use-package switch-window
  :ensure t
  :config
    (setq switch-window-input-style 'minibuffer)
    (setq switch-window-increase 4)
    (setq switch-window-threshold 2)
    (setq switch-window-shortcut-style 'qwerty)
    (setq switch-window-qwerty-shortcuts
        '("a" "r" "s" "t" "n" "e" "i" "o"))
  :bind
    ([remap other-window] . switch-window))

(rjs/leader-keys
  "SPC" '(counsel-find-file :which-key "find file")
  "q" '(:ignore t :which-key "quit")
  "qq" '(rjs/suspend :which-key "quit")
  "z" '(zone :which-key "zone")
  )

(general-define-key "C-S-n" 'counsel-switch-buffer)
(define-key evil-normal-state-map (kbd "U") 'dired-single-magic-buffer)
(define-key evil-normal-state-map (kbd "Q") 'quick-calc)

(rjs/leader-keys
  "b" '(:ignore t :which-key "buffer")
  "bc" '(kill-buffer :which-key "close")
  "bk" '(kill-current-buffer :which-key "kill")
  "bK" '(rjs/nuke-all-buffers :which-key "kill")
  "bb" '(counsel-switch-buffer :which-key "switch")
  "bs" '(save-buffer :which-key "save")
  "b[" '(evil-prev-buffer :which-key "back")
  "b]" '(evil-next-buffer :which-key "forward")
  )

(rjs/leader-keys
  "w" '(:ignore t :which-key "window")
  "ww" '(switch-window :which-key "switch")
  "wb" '(winner-undo :which-key "undo")
  "wf" '(winner-redo :which-key "redo")
  "wo" '(delete-other-windows :which-key "delete others")
  "wc" '(evil-window-delete :which-key "close")
  "wv" '(evil-window-vsplit :which-key "vsplit")
  "ws" '(evil-window-split :which-key "split")
  "w=" '(balance-windows :which-key "balance")
  "wn" '(evil-window-below :which-key "select below")
  "we" '(evil-window-up :which-key "select above")
  "wi" '(evil-window-right :which-key "select right")
  "wm" '(evil-window-left :which-key "select left")
  "wN" '(windmove-swap-states-down :which-key "move below")
  "wE" '(windmove-swap-states-up :which-key "move above")
  "wI" '(windmove-swap-states-right :which-key "move right")
  "wM" '(windmove-swap-states-left :which-key "move left")
  "wr" '(hydra-window-resize/body :which-key "resize")
  )

(defhydra hydra-window-resize (:timeout 4)
  "resize window"
  ("n" evil-window-decrease-height "decrease height")
  ("e" evil-window-increase-height "increase height")
  ("i" evil-window-increase-width "increase width")
  ("m" evil-window-decrease-width "decrease width")
  ("f" nil "finished" :exit t))

(rjs/leader-keys
  "g" '(:ignore t :which-key "magit")
  "gg" '(magit-status :which-key "status")
  "gs" '(magit-status :which-key "status")
  "gz" '(magit-stash :which-key "stash")
  "gf" '(magit-pull :which-key "pull")
  "gp" '(magit-push :which-key "push")
  "gc" '(magit-commit :which-key "commit")
  )

(rjs/leader-keys
  "p" '(:ignore t :which-key "projectile")
  "pp" '(projectile-switch-project :which-key "switch project")
  "pf" '(projectile--find-file :which-key "find file")
  "pg" '(projectile-ripgrep :which-key "search project")
  )

(rjs/leader-keys
  "l" '(:ignore t :which-key "lsp")
  "la" '(lsp-execute-code-action :which-key "apply action")
  "lg" '(:ignore t :which-key "goto")
  "lgd" '(lsp-find-definition :which-key "definition")
  "lgr" '(lsp-find-references :which-key "references")
  "ltl" '(lsp-mode :which-key "toggle lsp")
  "lt" '(:ignore t :which-key "toggles")
  "lth" '(lsp-headerline-breadcrumb-mode :which-key "toggle headerline")
  "lr" '(lsp-rename :which-key "rename symbol")
  "lf" '(format-all-buffer :which-key "format all")
  "tl" '(lsp-mode :which-key "toggle lsp")
  )

(rjs/leader-keys
  "j" '(:ignore t :which-key "jump")
  "jj" '(avy-goto-char-2 :which-key "jump to char")
  "jw" '(avy-goto-word-1 :which-key "jump to word")
  "jl" '(avy-goto-line :which-key "jump to line")
  )

(rjs/leader-keys
  "o" '(:ignore t :which-key "open")
  "ot" '(rjs/open-eshell :which-key "eshell")
  "oc" '(rjs/edit-config :which-key "config")
  "od" '(dired-jump :which-key "dired")
  "oq" '((lambda () (interactive) (start-process "qutebrowser" nil "qutebrowser")) :which-key "qutebrowser")
  )

(rjs/leader-keys
  "f" '(:ignore t :which-key "format")
  "fb" '(rjs/format-code-block :which-key "format src block")
  "ff" '(format-all-buffer :which-key "format file")
  )

(rjs/leader-keys
  :states 'normal
  :keymaps 'org-mode-map
  ";" '((lambda () (interactive) (message "hi")) :which-key "say hi")
  )

(general-define-key
 :keymaps 'eshell-mode-map
 "<C-escape>" 'rjs/insert-sudo
 )

(setq browse-url-browser-function 'browse-url-generic)
(setq browse-url-generic-program "qutebrowser")

(defun rjs/search (n m)
  (interactive)
  (browse-url (format n (read-from-minibuffer (format "Search %s: " m)))))

(defun rjs/stack-search ()
  (interactive)
  (rjs/search "https://stackoverflow.com/search?q=%s" "Stack"))

(rjs/leader-keys
  "s" '(:ignore t :which-key "search")
  "ss" '((lambda () (interactive) (rjs/search "https://duckduckgo.com/?q=%s" "DuckDuckGo"))
         :which-key "Google")
  "sw" '(define-word
          :which-key "dictionary")
  "sc" '((lambda () (interactive) (rjs/search "https://www.ultimate-guitar.com/search.php?value=%s" "Chords"))
         :which-key "Chords")
  "st" '((lambda () (interactive) (rjs/search "https://stackoverflow.com/search?q=%s" "Stack") )
         :which-key "stack")
  )

(rjs/leader-keys
  "tn" '(rjs/toggle-notifications :which-key "notifications")
  )

(require 'notifications)

(rjs/leader-keys
  "y" '(:ignore t :which-key "yas")
  "yy" '(company-yasnippet :which-key "yas")
  "yc" '(yas-expand :which-key "complete")
  )

(rjs/leader-keys
  "r" '(:ignore t :which-key "roam")
  "rt" '(org-roam-buffer-toggle :which-key "toggle")
  "rf" '(org-roam-node-find :which-key "find")
  "ri" '(org-roam-node-insert :which-key "insert")
  "rc" '(completion-at-point :which-key "completion")
  )

(use-package sudo-edit
  :ensure t
  :bind
    ("s-e" . sudo-edit))

(rjs/leader-keys
  :keymap '(emacs-lisp-mode-map org-mode-map)
    "le" '(eval-last-sexp :which-key "run")
  )
(message "hi")

(rjs/leader-keys python-mode-map
:states 'normal
  "le" '(python-shell-send-buffer :which-key "run")
    )

    (general-define-key
     :keymaps 'python-mode-map
     "C-x C-e" '(python-shell-send-buffer :which-key "eval buffer")
     )

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :config
  (add-hook 'dired-mode 'undo-tree-mode)
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-find-file))
(use-package dired-single
  :after dired)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(defun rjs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))


  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Comfortaa" :weight 'regular :height (cdr face)))


  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))



(defun rjs/org-mode-setup ()
  (org-indent-mode)
  (setq evil-auto-indent nil)
  (setq org-todo-keywords '((sequence "TODO" "NEXT" "WAIT" "|" "DONE" "CANCELLED")))
  (setq org-todo-keyword-faces '(("NEXT" . "DarkOrange") ("WAIT" . "turquoise") ("CANCELLED" . "gray32")))
  (setq org-log-done 'time)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . rjs/org-mode-setup)
  :commands (org-capture org-agenda)
  :config
  (message "Org mode loaded")
  (setq org-ellipsis " ▾")
  (setq org-startup-folded t)
  (setq org-agenda-files
        '("~/org/Tasks.org"))
  '("~/org/Birthdays.org")
  (rjs/org-font-setup))


(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun rjs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . rjs/org-mode-visual-fill))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))
  (setq org-confirm-babel-evaluate nil)

  (push '("conf-unix" . conf-unix) org-src-lang-modes)
  )

(with-eval-after-load 'org
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  )

(defun rjs/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name "~/.emacs.d/"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda()(add-hook 'after-save-hook #'rjs/org-babel-tangle-config)))

(defun rjs/format-code-block ()
  "Format babel code block"
  (interactive)
  (org-edit-special)
  (format-all-buffer)
  (org-edit-src-exit)
  )

(defun rjs/run-python-code-block ()
  "Format babel code block"
  (interactive)
  (org-edit-special)
  (python-shell-send-buffer)
  (org-edit-src-exit)
  )

(use-package org-modern
  :after org
  :config
  (add-hook 'org-mode-hook #'org-modern-mode)
  )

(use-package org-roam
    :after org
    :config
    (org-roam-setup)
  (setq org-roam-directory (file-truename "~/org-roam"))
  (org-roam-db-autosync-mode)
(setq org-roam-completion-everywhere t)
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:10}" 'face 'org-tag)))
    )

(defun rjs/lsp-mode-setup ()
    (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symboles))
    (lsp-headerline-breadcrumb-mode))
  (use-package lsp-mode
    :commands (lsp lsp-deferred)
    :hook (lsp-mode . rjs/lsp-mode-setup)
    :init
    (setq lsp-keymap-prefix "C-c l")
    :config
    (lsp-enable-which-key-integration t))
  (use-package lsp-ui
    :after lsp
    :hook (lsp-mode . lsp-ui-mode)
    :custom
    (lsp-ui-doc-position 'bottom)
    (lsp-ui-doc-show-with-cursor t)
    (lsp-ui-sideline-show-code-actions t)
    (lsp-ui-doc-enable t))

  (use-package lsp-treemacs
    :after lsp)

  (use-package lsp-ivy
    :after lsp)

  (add-hook 'prog-mode-hook 'lsp-deferred)
  (add-hook 'prog-mode-hook 'format-all-mode)
  (add-hook 'format-all-mode-hook 'format-all-ensure-formatter)
(remove-hook 'elisp-mode 'lsp-deferred)

(use-package typescript-mode
:after lsp
    :mode "\\.ts\\'"
    :hook (typescript-mode . lsp-deferred)
    :config
    (setq typescript-indent-level 2))

(add-hook 'js-mode 'lsp-deferred)

(use-package rjsx-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
  )

(setq lsp-rust-server 'rls)
(setq lsp-rust-all-features t)
(use-package cargo)
(add-hook 'rust-mode-hook 'cargo-minor-mode)

(use-package haskell-mode
  :hook (haskell-mode . lsp-deferred)
  :after lsp)
(use-package lsp-haskell
  :after lsp
  :config
  (setq lsp-haskell-server-path "/home/reuben/.ghcup/hls/1.7.0.0/bin/haskell-language-server-wrapper")
  (add-hook 'haskell-mode-hook #'lsp)
  (add-hook 'haskell-literate-mode-hook #'lsp)
  )

(defvar python-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Movement
    (define-key map [remap backward-sentence] 'python-nav-backward-block)
    (define-key map [remap forward-sentence] 'python-nav-forward-block)
    (define-key map [remap backward-up-list] 'python-nav-backward-up-list)
    (define-key map [remap mark-defun] 'python-mark-defun)
    (define-key map "\C-c\C-j" 'imenu)
    ;; Indent specific
    (define-key map "\177" 'python-indent-dedent-line-backspace)
    (define-key map (kbd "<backtab>") 'python-indent-dedent-line)
    (define-key map "\C-c<" 'python-indent-shift-left)
    (define-key map "\C-c>" 'python-indent-shift-right)
    ;; Skeletons
    (define-key map "\C-c\C-tc" 'python-skeleton-class)
    (define-key map "\C-c\C-td" 'python-skeleton-def)
    (define-key map "\C-c\C-tf" 'python-skeleton-for)
    (define-key map "\C-c\C-ti" 'python-skeleton-if)
    (define-key map "\C-c\C-tm" 'python-skeleton-import)
    (define-key map "\C-c\C-tt" 'python-skeleton-try)
    (define-key map "\C-c\C-tw" 'python-skeleton-while)
    ;; Shell interaction
    (define-key map "\C-c\C-p" 'run-python)
    (define-key map "\C-c\C-s" 'python-shell-send-string)
    (define-key map "\C-c\C-e" 'python-shell-send-statement)
    (define-key map "\C-c\C-r" 'python-shell-send-region)
    (define-key map "\C-\M-x" 'python-shell-send-defun)
    (define-key map "\C-c\C-c" 'python-shell-send-buffer)
    (define-key map "\C-c\C-l" 'python-shell-send-file)
    (define-key map "\C-c\C-z" 'python-shell-switch-to-shell)
    ;; Some util commands
    (define-key map "\C-c\C-v" 'python-check)
    (define-key map "\C-c\C-f" 'python-eldoc-at-point)
    (define-key map "\C-c\C-d" 'python-describe-at-point)
    ;; Utilities
    (substitute-key-definition 'complete-symbol 'completion-at-point
                               map global-map)
    (easy-menu-define python-menu map "Python Mode menu"
      '("Python"
        :help "Python-specific Features"
        ["Shift region left" python-indent-shift-left :active mark-active
         :help "Shift region left by a single indentation step"]
        ["Shift region right" python-indent-shift-right :active mark-active
         :help "Shift region right by a single indentation step"]
        "-"
        ["Start of def/class" beginning-of-defun
         :help "Go to start of outermost definition around point"]
        ["End of def/class" end-of-defun
         :help "Go to end of definition around point"]
        ["Mark def/class" mark-defun
         :help "Mark outermost definition around point"]
        ["Jump to def/class" imenu
         :help "Jump to a class or function definition"]
        "--"
        ("Skeletons")
        "---"
        ["Start interpreter" run-python
         :help "Run inferior Python process in a separate buffer"]
        ["Switch to shell" python-shell-switch-to-shell
         :help "Switch to running inferior Python process"]
        ["Eval string" python-shell-send-string
         :help "Eval string in inferior Python session"]
        ["Eval buffer" python-shell-send-buffer
         :help "Eval buffer in inferior Python session"]
        ["Eval statement" python-shell-send-statement
         :help "Eval statement in inferior Python session"]
        ["Eval region" python-shell-send-region
         :help "Eval region in inferior Python session"]
        ["Eval defun" python-shell-send-defun
         :help "Eval defun in inferior Python session"]
        ["Eval file" python-shell-send-file
         :help "Eval file in inferior Python session"]
        ["Debugger" pdb :help "Run pdb under GUD"]
        "----"
        ["Check file" python-check
         :help "Check file for errors"]
        ["Help on symbol" python-eldoc-at-point
         :help "Get help on symbol at point"]
        ["Complete symbol" completion-at-point
         :help "Complete symbol before point"]))
    map)
  "Keymap for `python-mode'.")

(require 'python)
  ;; (use-package python-mode
  ;;   :ensure nil
  ;;   :custom
  ;;   (python-shell-interpreter "python3")
  ;;   )

(setq lsp-dart-sdk-dir "/home/reuben/snap/flutter/common/flutter")
(setq lsp-flutter-dart-sdk-dir "/home/reuben/snap/flutter/common/flutter")
(add-hook 'dart-mode-hook 'lsp)

(use-package elisp-slime-nav)

(use-package company
    :after lsp-mode
    :hook (lsp-mode . company-mode)
    :bind (:map company-active-map
                ("<tab>" . company-complete-selection)
                ("C-e" . company-select-previous-or-abort))
    (:map lsp-mode-map
          ("<tab>" . company-indent-or-complete-common))
    :custom
    (company-minimum-prefix-length 1)
    (setq company-tooltip-align-annotations t)
    (setq company-show-quick-access 'left)
    (setq lsp-completion-provider :none)
    (setq company-backends '((company-capf  company-yasnippet company-sourcekit)))
    (company-idle-delay 0.0))

  (use-package company-box
:disabled
    :hook (company-mode . company-box-mode))

(use-package flycheck
  :after lsp
  :config
  (global-flycheck-mode)
  )

(use-package dap-mode
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  ;; :config
  ;; (dap-ui-mode 1)
  :commands dap-debug
  :config
  ;; Set up Node debugging
  (require 'dap-node)
  (dap-node-setup) ;; Automatically installs Node debug adapter if needed

  ;; Bind `C-c l d` to `dap-hydra` for easy access
  (general-define-key
   :keymaps 'lsp-mode-map
   :prefix lsp-keymap-prefix
   "d" '(dap-hydra t :wk "debugger")
   )
  )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package avy
  :commands (avy-goto-char avy-goto-word-0 avy-goto-line)
  :config
  (setq avy-keys '(?a ?r ?s ?t ?g ?m ?n ?e ?i ?o))
  )

(use-package magit
  :commands (magit-status magit-pull)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package projectile
    :diminish projectile-mode
    :config (projectile-mode)
    :custom ((projectile-completion-system 'ivy))
    :bind-keymap
    ("C-c p" . projectile-command-map))

  (use-package counsel-projectile
:after projectile
    :config (counsel-projectile-mode))

(defun rjs/formatting ()
  (interactive)
  (if (region-active-p)
      (format-all-region (point) (mark))
    (format-all-buffer)))
(use-package format-all
  :config
  (define-key evil-normal-state-map "=" 'rjs/formatting)
  )

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all)
  (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)
  )
(use-package yasnippet-snippets
  :after yasnippet)

(let ((ligatures `((?-  . ,(regexp-opt '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->")))
                   (?/  . ,(regexp-opt '("/**" "/*" "///" "/=" "/==" "/>" "//")))
                   (?*  . ,(regexp-opt '("*>" "***" "*/")))
                   (?<  . ,(regexp-opt '("<-" "<<-" "<=>" "<=" "<|" "<||" "<|||::=" "<|>" "<:" "<>" "<-<"
                                         "<<<" "<==" "<<=" "<=<" "<==>" "<-|" "<<" "<~>" "<=|" "<~~" "<~"
                                         "<$>" "<$" "<+>" "<+" "</>" "</" "<*" "<*>" "<->" "<!--")))
                   (?:  . ,(regexp-opt '(":>" ":<" ":::" "::" ":?" ":?>" ":=")))
                   (?=  . ,(regexp-opt '("=>>" "==>" "=/=" "=!=" "=>" "===" "=:=" "==")))
                   (?!  . ,(regexp-opt '("!==" "!!" "!=")))
                   (?>  . ,(regexp-opt '(">]" ">:" ">>-" ">>=" ">=>" ">>>" ">-" ">=")))
                   (?&  . ,(regexp-opt '("&&&" "&&")))
                   (?|  . ,(regexp-opt '("|||>" "||>" "|>" "|]" "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||")))
                   (?.  . ,(regexp-opt '(".." ".?" ".=" ".-" "..<" "...")))
                   (?+  . ,(regexp-opt '("+++" "+>" "++")))
                   (?\[ . ,(regexp-opt '("[||]" "[<" "[|")))
                   (?\{ . ,(regexp-opt '("{|")))
                   (?\? . ,(regexp-opt '("??" "?." "?=" "?:")))
                   (?#  . ,(regexp-opt '("####" "###" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" "##")))
                   (?\; . ,(regexp-opt '(";;")))
                   (?_  . ,(regexp-opt '("_|_" "__")))
                   (?\\ . ,(regexp-opt '("\\" "\\/")))
                   (?~  . ,(regexp-opt '("~~" "~~>" "~>" "~=" "~-" "~@")))
                   (?$  . ,(regexp-opt '("$>")))
                   (?^  . ,(regexp-opt '("^=")))
                   (?\] . ,(regexp-opt '("]#"))))))
  (dolist (char-regexp ligatures)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))

(use-package paredit)

(defun rjs/insert-sudo ()
  "Insert sudo before command in eshell"
  (interactive)
  (save-excursion
    (eshell-bol)
    (if (eolp)
        (progn
          (eshell-previous-input 1)
          (setq upper-command 't)
          )
      )
    (eshell-bol)
    (unless (string= (thing-at-point 'word) "sudo")
      (insert "sudo ")
      )
    )
  (if (eq upper-command 't)
      (end-of-line)
    )
  )

(defun rjs/suspend ()
  (interactive)
  (if (yes-or-no-p "Suspend? ")
      (start-process-shell-command "suspend" nil "systemctl suspend")
    )
  )

(defun rjs/disable-screen-timeout ()
  (interactive)
      (start-process-shell-command "xset -dpms" nil "xset -dpms")
      (start-process-shell-command "xset s off" nil "xset s off")
  )


(defun rjs/enable-screen-timeout ()
  (interactive)
      (start-process-shell-command "xset +dpms" nil "xset +dpms")
      (start-process-shell-command "xset s on" nil "xset s on")
  )

(defun rjs/nuke-all-buffers ()
    (interactive)
    (if (yes-or-no-p "Kill all buffers? ")
        (progn
(mapc 'kill-buffer (delq (get-buffer "*eshell*") (delq (get-buffer "*scratch*") (buffer-list))))
          )
      )
    )

(defun rjs/edit-config ()
  (interactive)
  (find-file "~/.emacs.d/Emacs.org")
  )

(defun rjs/source-config ()
  (interactive)
  (load-file "~/.emacs.d/init.el")
  )

(setq gc-cons-threshold (* 2 1000 1000))
