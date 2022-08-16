(defun rjs/exwm-update-class ()
    (exwm-workspace-rename-buffer exwm-class-name))
  (defun rjs/run-in-background (command)
    (let ((command-parts (split-string command "[ ]+")))
      (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

  (defun rjs/set-wallpaper ()
    (interactive)
    (start-process-shell-command
     "feh" nil  "feh --bg-scale ~/important/wallpaper.jpg"))
  (defun rjs/start-picom ()
    (interactive)
    (start-process-shell-command
     "picom" nil  "picom"))
  (defun rjs/start-autorandr ()
    (interactive)
    (start-process-shell-command
     "autorandr" nil  "autorandr"))

  (defun rjs/exwm-init-hook ()
    ;; Make workspace 1 be the one where we land at startup
    (exwm-workspace-switch-create 1)


    (rjs/run-in-background "nm-applet")
    (rjs/run-in-background "pasystray")
    (rjs/run-in-background "blueman-applet")
    (rjs/run-in-background "dunst")

    ;; Show battery status in the mode line
    (display-battery-mode 1)

    ;; Show the time and date in modeline
    (setq display-time-day-and-date nil)
    (setq display-time-format "%H:%M")
    (setq display-time-next-load-average nil)
    (display-time-mode 1))
  ;; Also take a look at display-time-format and format-time-string

  ;; Launch apps that will run in the background

  (defun rjs/exwm-update-title()
    (pcase exwm-class-name
      ("Vivaldi-stable" (exwm-workspace-rename-buffer (format "Vivaldi: %s" exwm-title)))
      ("qutebrowser" (exwm-workspace-rename-buffer (format "Qutebrowser: %s" exwm-title)))
))

  (defun rjs/configure-window-by-class ()
    (interactive)
    (pcase exwm-class-name
      ;; ("Vivaldi-stable" (exwm-workspace-move-window 2))
      )
    )

  (use-package exwm
    :config
    ;; Set the default number of workspaces
    (setq exwm-workspace-number 10)

    ;;Windows follow around
    (setq exwm-layout-show-all-buffers t)
    (setq exwm-workspace-show-all-buffers t)


    ;; When window "class" updates, use it to set the buffer name
    (add-hook 'exwm-update-class-hook #'rjs/exwm-update-class)
    (add-hook 'exwm-update-title-hook #'rjs/exwm-update-title)
    (add-hook 'exwm-init-hook #'rjs/exwm-init-hook)
    (add-hook 'exwm-manage-finish-hook #'rjs/configure-window-by-class)
    ;; Set the screen resolution (update this to be the correct resolution for your screen!)
    (require 'exwm-randr)
    (exwm-randr-enable)

    ;; (start-process-shell-command "xrandr" nil "xrandr --output eDP-1 --primary --mode 1920x1080 --pos 1920x0 --rotate normal --output HDMI-1 --mode 1920x1080 --pos 0x0 --rotate normal --output DP-1 --off --output DP-2 --off")

    (setq exwm-randr-workspace-monitor-plist '(2 "HDMI-1" 1 "HDMI-1" 0 "HDMI-1"))

    (setq exwm-workspace-warp-cursor t)

    (rjs/set-wallpaper)
    (rjs/start-picom)
    (rjs/start-autorandr)
    ;; Load the system tray before exwm-init
    (require 'exwm-systemtray)
    (exwm-systemtray-enable)

    ;; These keys should always pass through to Emacs
    (setq exwm-input-prefix-keys
          '(?\C-x
            ?\C-u
            ?\C-h
            ?\M-x
            ?\M-`
            ?\M-&
            ?\M-:
            ?\C-\S-n  ;; Buffer list
            ?\C-\ ))  ;; Ctrl+Space

    ;; Ctrl+Q will enable the next key to be sent directly
    (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

    ;; Set up global key bindings.  These always work, no matter the input state!
    ;; Keep in mind that changing this list after EXWM initializes has no effect.
    (setq exwm-input-global-keys
          `(
            ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
            ([?\s-r] . exwm-reset)
            ([?\s-l] . desktop-environment-lock-screen)
            ([?\s-f] . exwm-layout-toggle-fullscreen)

            ;; Move between windows
            ([s-left] . windmove-left)
            ([s-right] . windmove-right)
            ([s-up] . windmove-up)
            ([s-down] . windmove-down)

            ;; Launch applications via shell command
            ([?\s-&] . (lambda (command)
                         (interactive (list (read-shell-command "$ ")))
                         (start-process-shell-command command nil command)))

            ;; Switch workspace
            ([?\s-w] . exwm-workspace-switch)
            ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))

            ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
            ,@(mapcar (lambda (i)
                        `(,(kbd (format "s-%d" i)) .
                          (lambda ()
                            (interactive)
                            (exwm-workspace-switch-create ,i))))
                      (number-sequence 0 9))))
    (exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app)

    (exwm-enable))

(use-package desktop-environment
    :after exwm
    :config (desktop-environment-mode)
(setq desktop-environment-update-exwm-global-keys :prefix)
  )

(defun rjs/dunstctl (cmd)
    (start-process-shell-command "dunstctl" nil (format "dunstctl %s" cmd)))

  (exwm-input-set-key (kbd "s-n") (lambda () (interactive) (rjs/dunstctl "history-pop")))
  (exwm-input-set-key (kbd "s-N") (lambda () (interactive) (rjs/dunstctl "close-all")))

  (defun rjs/toggle-notifications ()
  (interactive)
(rjs/dunstctl "set-paused toggle"))
