;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Lord Valen"
      user-mail-address "lord_valen@pm.me"
      )

(use-package! evil-colemak-basics
  :init
  (setq evil-colemak-basics-layout-mod `mod-dh)
  :config
  (global-evil-colemak-basics-mode)
  )

(set-face-attribute 'default nil
  :font "Inconsolata Nerd Font Mono"
  :height 120
  :weight 'medium
  )
(set-face-attribute 'variable-pitch nil
  :font "Inconsolata Nerd Font"
  :height 120
  :weight 'medium
  )
(set-face-attribute 'fixed-pitch nil
  :font "Inconsolata Nerd Font Mono"
  :height 120
  :weight 'medium
  )

;; Makes commented text and keywords italics.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic
  )
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic
  )

;; Adjust line spacing.
;;(setq-default line-spacing 0.12)

;; Needed if using emacsclient. Otherwise, your fonts will be smaller than expected.
(add-to-list 'default-frame-alist '(font . "Inconsolata Nerd Font Mono-12"))
;; changes certain keywords to symbols, such as lamda!
(setq global-prettify-symbols-mode t)

(after! org

(setq org-directory "~/documents/org/")
(setq org-agenda-files '("~/documents/org/agenda.org"))

(setq
 org-css "file:///e:/emacs/documents/org-css/css/org.css")
(setq
 org-preamble (format
               "#+TITLE:\n#+AUTHOR:Lord Valen\n/This file is best viewed in [[https://www.gnu.org/software/emacs/][emacs]]!/"
               org-css)
 )

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
                       (forward-char)
                       )
                     )
                  )
              )
            )
          )

(setq org-export-headline-levels 5)
(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))

;; Import ox-latex to get org-latex-classes and other funcitonality
;; for exporting to LaTeX from org
(use-package! ox-latex
  :init
  ;; code here will run immediately
  :config
  ;; code here will run after the package is loaded
  (setq org-latex-pdf-process
        '("pdflatex -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "pdflatex -interaction nonstopmode -output-directory %o %f"
          "pdflatex -interaction nonstopmode -output-directory %o %f")
        )
  (setq org-latex-with-hyperref nil) ;; stop org adding hypersetup{author..} to latex export
  ;; (setq org-latex-prefer-user-labels t)

  ;; deleted unwanted file extensions after latexMK
  (setq org-latex-logfiles-extensions
        (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl" "xmpi" "run.xml" "bcf" "acn" "acr" "alg" "glg" "gls" "ist"))
        )

  (unless (boundp 'org-latex-classes)
    (setq org-latex-classes nil)
    )
  )

)

(setq org-roam-directory (file-truename "~/documents/org/org-roam"))

(defun episteme:ensure-org-id ()
  (interactive)
  (when (s-starts-with? org-roam-directory (buffer-file-name))
    (save-excursion
      (beginning-of-buffer)
      (org-id-get-create)
      )
    )
  )

(add-hook 'org-mode-hook
    (lambda () (add-hook 'before-save-hook 'episteme:ensure-org-id nil t))
    )

(setq format-on-save-enabled-modes
      '(not emacs-lisp-mode
            sql-mode
            tex-mode
            latex-mode
        ))

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
         )
        )
      )

(setq circe-format-say "{nick:-16s}> {body}")
(setq circe-format-self-say "{nick:-16s}> {body}")
(setq circe-format-message "{nick:-16s} => {chattarget}> {body}")
(setq circe-format-self-message "{nick:-16s} => {chattarget}> {body}")

(add-hook 'circe-chat-mode-hook 'my-circe-prompt)
(defun my-circe-prompt ()
  (lui-set-prompt
   (concat (propertize (concat (buffer-name) ">")
                       'face 'circe-prompt-face)
           " ")
   )
  )

(setq circe-reduce-lurker-spam t)
