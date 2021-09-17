;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Lord Valen"
      user-mail-address "lord_valen@pm.me"
      )

(defmacro :hook (hook-name &rest body)
  "A simple wrapper around `add-hook'"
  (declare (indent defun))
  (let* ((hook-name (format "%s-hook" (symbol-name hook-name)))
         (hook-sym (intern hook-name))
         (first (car body))
         (local (eq :local first))
         (body (if local (cdr body) body))
         (first (car body))
         (body (if (consp first)
                   (if (eq (car first) 'quote)
                       first
                     `(lambda () ,@body))
                 `',first)))
    `(add-hook ',hook-sym ,body nil ,local)))

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

(after! org-roam
(setq org-roam-directory (file-truename "~/org-roam"))

(cl-defmethod org-roam-node-slug ((node org-roam-node))
  (let ((title (org-roam-node-title node)))
    (cl-flet* ((nonspacing-mark-p (char)
                                  (memq char org-roam-slug-trim-chars))
               (strip-nonspacing-marks (s)
                                       (ucs-normalize-NFC-string
                                        (apply #'string (seq-remove #'nonspacing-mark-p
                                                                    (ucs-normalize-NFD-string s)))))
               (cl-replace (title pair)
                           (replace-regexp-in-string (car pair) (cdr pair) title)))
      (let* ((pairs `(("[^[:alnum:][:digit:]]" . "-")
                      ("--*" . "-")
                      ("^-" . "")
                      ("-$" . "")))
             (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
        (downcase slug)))))
)

(defun episteme:ensure-org-id ()
  (interactive)
  (when (s-starts-with? org-roam-directory (buffer-file-name))
    (save-excursion
      (beginning-of-buffer)
      (org-id-get-create)
      )
    )
  )

(:hook org-mode
    (add-hook 'before-save-hook 'episteme:ensure-org-id nil t))

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
