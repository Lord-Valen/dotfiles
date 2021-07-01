;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Lord Valen"
      user-mail-address "lord_valen@pm.me")

(set-face-attribute 'default nil
  :font "Inconsolata Nerd Font Mono"
  :height 120
  :weight 'medium)
(set-face-attribute 'variable-pitch nil
  :font "Inconsolata Nerd Font"
  :height 120
  :weight 'medium)
(set-face-attribute 'fixed-pitch nil
  :font "Inconsolata Nerd Font Mono"
  :height 120
  :weight 'medium)

;; Makes commented text and keywords italics.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

;; Adjust line spacing.
;;(setq-default line-spacing 0.12)

;; Needed if using emacsclient. Otherwise, your fonts will be smaller than expected.
(add-to-list 'default-frame-alist '(font . "Inconsolata Nerd Font Mono-12"))
;; changes certain keywords to symbols, such as lamda!
(setq global-prettify-symbols-mode t)

(setq creds "~/.doom.d/creds.el")
(setq nick "lord_valen")

(defun pass (server)
             (with-temp-buffer
               (insert-file-contents-literally creds)
               (plist-get (read (buffer-string)) :pass)))

(setq circe-network-options
      '(("Freenode" :host "chat.freenode.net" :port (6667 . 6697)
         :tls t
         :nick nick
         :sasl-username nick
         :sasl-password pass
         :channels (
                    "#philosophy"
                    "#idleRPG"
                    "#physics"
                    "#science"
                    "#emacs"
                    "#"
                    )

         )))

(setq circe-format-say "{nick:-16s}> {body}")
(setq circe-format-self-say "{nick:-16s}> {body}")
(setq circe-format-message "{nick:-16s} => {chattarget}> {body}")
(setq circe-format-self-message "{nick:-16s} => {chattarget}> {body}")

(add-hook 'circe-chat-mode-hook 'my-circe-prompt)
(defun my-circe-prompt ()
  (lui-set-prompt
   (concat (propertize (concat (buffer-name) ">")
                       'face 'circe-prompt-face)
           " ")))

(setq circe-reduce-lurker-spam t)

(setq
 org-css "file:///e:/emacs/documents/org-css/css/org.css")
(setq
 org-preamble (format
               "#+TITLE:\n#+AUTHOR:Lord Valen\n/This file is best viewed in [[https://www.gnu.org/software/emacs/][emacs]]!/"
               org-css))

(add-hook 'find-file-hook
          (lambda ()
            (if
                (string=
                 (substring
                  (buffer-name)
                  (if (> (length (buffer-name)) 3) (- (length (buffer-name)) 3) 0)
                  nil)
                 "org")
                (if
                    (=
                     (buffer-size)
                     0)
                    ((lambda ()
                       (insert org-preamble)

                                        ; navigate point to end of #+TITLE:, doesnt work when launching from gitbash for some reason, point just moves right back down after doom does something
                       (goto-line 1)
                       (forward-word)
                       (forward-char)))))))
