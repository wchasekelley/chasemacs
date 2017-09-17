(set-face-attribute 'default nil :height 100)
;; Packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Ruby
(add-hook 'ruby-mode-hook 'robe-mode)

(eval-after-load 'company
  '(push 'company-robe company-backends))

(add-hook 'robe-mode-hook 'ac-robe-setup)

;; Higlight the current line number in linum mode
(require 'hlinum)
(hlinum-activate)

;; Shortcuts
(global-set-key (kbd "M-;") 'ace-window)
(global-set-key (kbd "S-<f1>")
  (lambda ()
    (interactive)
    (dired "~/")))

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

(global-ede-mode t)

;; Hide dired details
(setq diredp-hide-details-initially-flag t)

(require 'ido)
(ido-mode t)

(require 'ace-jump-mode)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(allout-mode)

(require 'powerline)
(powerline-default-theme)
(server-start)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'yasnippet)
(yas-global-mode 1)

(defun reset-python ()
  "Close the current python buffer and start a new one"
  (interactive)
  (delete-process (get-process "Python"))
  (kill-buffer "*Python*")
  (run-python)
  (switch-to-buffer (get-buffer "*Python*")))

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define-global "jc" 'org-paste-cpp)
(key-chord-define-global "sx" 'eval-last-sexp)
(key-chord-define-global "5t" 'revert-buffer)
(key-chord-define-global "lp" 'ace-jump-mode)
(key-chord-define-global "zx" 'eval-buffer)
(key-chord-define-global "xf" 'find-file)
(key-chord-define-global "fp" 'ace-window)
(key-chord-define-global ",."     "<>\C-b")
(key-chord-define-global "a;" 'previous-buffer)
(key-chord-define-global "a'" 'next-buffer)
(key-chord-define-global "hj"     'undo)
(key-chord-define-global [?h ?j]  'undo)  ; the same
(key-chord-define-global "jk"     'dabbrev-expand)
(key-chord-define-global "cv"     'reindent-then-newline-and-indent)
(key-chord-define-global "4r"     "$$\C-b")
(key-chord-define-global "{}"     "{}\C-b")
(key-chord-define-global "[]"     "[]\C-b")
(key-chord-define-global "cx"     'org-toggle-latex-fragment)
(key-chord-define-global "dk" 'other-window)
(key-chord-define-global "-=" 'toggle-truncate-lines)
(key-chord-define-global "1q" 'reset-python)
(key-chord-define-global "mc" 'smart-compile)
(key-chord-define-global "pd" 'realgud:pdb)
(key-chord-define-global "ws" 'whitespace-mode)
(key-chord-define-global "mf" 'menu-bar-mode)
(key-chord-define-global "ml" 'linum-mode)
(key-chord-define-global "tz" 'org-time-stamp-inactive)
(key-chord-define-global "lj" 'learnjap/show-learnjap-info)

(server-start)

;; TODO: Move this to its own file
;; https://github.com/thesoftwarebin/the-emacs-software-bin/tree/master/learnjap
(defvar learnjap/kanjitable (make-hash-table :test 'equal))
(defvar learnjap/kanatable  (make-hash-table :test 'equal))

(load-file "//home//chase//.emacs.d//learnjap-kanji-table.el")
(load-file "//home//chase//.emacs.d//learnjap-kana-table.el")

(defun learnjap/get-properties-kanji (code)
  (gethash code learnjap/kanjitable))

(defun learnjap/kanji-properties-to-string (i)
  (format
   "%s\nOn reading: %s\nKun reading: %s\nDefinition: %s\nStrokes: %s"
   (plist-get i :utf8)
   (mapconcat 'identity (plist-get i :onreadings) ", ")
   (mapconcat 'identity (plist-get i :kunreadings) ", ")
   (plist-get i :definition)
   (plist-get i :strokecount)))

(defun learnjap/get-properties-kana (code)
  (gethash code learnjap/kanatable))

(defun learnjap/kana-properties-to-string (i)
  (format
   "%s\n%s\nReading: %s"
   (plist-get i :utf8)
   (plist-get i :symbol-type)
   (plist-get i :reading)))

(defun learnjap/code-to-string (code)
  (let* (
         (prop 
          (or 
           (learnjap/get-properties-kanji code) 
           (learnjap/get-properties-kana code))))
    (cond ((null prop)                            (format "%c\n(no learnjap info available)" code))
          ((null (plist-get prop :reading))       (learnjap/kanji-properties-to-string prop))
          ((not (null (plist-get prop :reading))) (learnjap/kana-properties-to-string prop)))))

(defvar learnjap/info-buffer-name "*learnjap-info*")

(defun learnjap/show-learnjap-info (bufpoint)
  "Shows kanji information for the kanji at cursor point BUFPOINT.
Information is shown in a buffer named \"*learnjap-info*\" (if it does
not exist, it's created and shown at the bottom).
Usage example:
- M-x learnjap/show-learnjap-info RET
- or add a shortcut key with `local-set-key'; example:
  (local-set-key (kbd \"C-c C-c\") 'learnjap/show-learnjap-info)
DONE: add hiragana and katakana
TODO: create a minor mode for learnjap
"
  (interactive "d")
  (let
      (
       (charnum        (string-to-char (buffer-substring-no-properties bufpoint (1+ bufpoint))))
       (big-jap-char-height (* (face-attribute 'default :height) 6)))

    (when
        (null (get-buffer learnjap/info-buffer-name))
      (split-window-below 10)
      (other-window 1)
      (generate-new-buffer learnjap/info-buffer-name)
      (set-window-buffer nil learnjap/info-buffer-name)
      (other-window -1))

    (with-current-buffer (get-buffer learnjap/info-buffer-name)
      (erase-buffer)
      (insert (learnjap/code-to-string charnum))
      (add-text-properties 1 2  (list 'face (list :height big-jap-char-height))))))

(provide 'learnjap)
(local-set-key (kbd "C-c C-c") 'learnjap/show-learnjap-info)

;; End Jap

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#cccccc" "#f2777a" "#99cc99" "#ffcc66" "#6699cc" "#cc99cc" "#66cccc" "#515151"))
 '(custom-enabled-themes (quote (badger)))
 '(custom-safe-themes
   (quote
    ("9b402e9e8f62024b2e7f516465b63a4927028a7055392290600b776e4a5b9905" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(default-input-method "japanese")
 '(ede-project-directories
   (quote
    ("/tmp/myproject/include" "/tmp/myproject/src" "/tmp/myproject")))
 '(fci-rule-color "#515151")
 '(gdb-many-windows t)
 '(gradle-executable-path "/usr/bin/gradle14")
 '(org-format-latex-options
   (quote
    (:foreground default :background default :scale 1.5 :html-foreground "Black" :html-background "Transparent" :html-scale 2.0 :matchers
                 ("begin" "$1" "$" "$$" "\\(" "\\["))))
 '(package-selected-packages
   (quote
    (quack gnuplot gnuplot-mode elein paredit paredit-menu paren-face ac-cider cider hlinum django-manage django-mode django-snippets company company-emacs-eclim gradle-mode eclim htmlize w3m w32-browser smali-mode realgud badger-theme haskell-mode flylisp flycheck flymake-cppcheck smart-compile flymake-vala vala-mode vala-snippets flymake-yaml yaml-mode w3 gh-md markdown-mode markdown-mode+ markdown-preview-eww markdown-preview-mode sr-speedbar ein elpy auctex magit yasnippet rtags robe python-info powerline org-bullets multiple-cursors key-chord flymake-cursor expand-region color-theme-solarized color-theme-sanityinc-tomorrow cmake-mode cmake-ide better-defaults auto-complete ace-jump-mode)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f2777a")
     (40 . "#f99157")
     (60 . "#ffcc66")
     (80 . "#99cc99")
     (100 . "#66cccc")
     (120 . "#6699cc")
     (140 . "#cc99cc")
     (160 . "#f2777a")
     (180 . "#f99157")
     (200 . "#ffcc66")
     (220 . "#99cc99")
     (240 . "#66cccc")
     (260 . "#6699cc")
     (280 . "#cc99cc")
     (300 . "#f2777a")
     (320 . "#f99157")
     (340 . "#ffcc66")
     (360 . "#99cc99"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((t nil)))
 '(flymake-warnline ((t nil))))
