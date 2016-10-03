                                        ; Allan Zhang's .emacs file

                                        ;;;; Native emacs settings

                                        ;User Details
(setq user-full-name "Allan Zhang")

                                        ; Start server to open files on double click
(server-start)

                                        ; Makes all yes or no prompts to y or n
(defalias 'yes-or-no-p 'y-or-n-p)

                                        ; Allows me to see several lines below/above when scrolling
(setq scroll-margin 5)

                                        ; Reduces size of kill ring to make it faster with helm
(setq kill-ring-max 20)

                                        ; Disable scroll bars. UGLY!!!
(scroll-bar-mode -1)

                                        ; Makes it so the cursor is easier to see
(blink-cursor-mode 0)

                                        ; I like replacing highlighted things tyvm
(delete-selection-mode 1)

(global-visual-line-mode t)
(setq delete-by-moving-to-trash t)
(setq sentence-end-double-space nil)
(setq tab-always-indent t)
(tool-bar-mode -1)

                                        ; Comint stuff for good interpreter settings
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)

					; Changing backup directory to avoid clutter
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

                                        ; Initializing Package essentials
                                        ; Getting additional package repos
(load "package")
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(setq package-enable-at-startup nil)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(require 'use-package)
(require 'diminish)

;; Automatic downloading and installing of packages in this file loaded with use-package
(setq use-package-always-ensure t)

;;;;;;;;; UI Settings
                                        ; Line numbers
(use-package linum-relative
  :diminish linum-relative-mode
  :init
  (setq linum-relative-current-symbol "")
  )
(linum-relative-global-mode)

					; Color theme
(setq color-theme-is-cumulative t)
(setq color-theme-is-global t)

                                        ; font
(set-face-attribute 'default nil :font "Source Code Pro Light 8")

                                        ; Theme
(use-package material-theme)
(load-theme 'material t)

                                        ; Offset the number by two spaces to work around some weird fringe glitch. See: http://stackoverflow.com/questions/4920210/what-causes-this-graphical-error-in-emacs-with-linum-mode-on-os-x
(setq linum-format "  %d ")

                                        ; Synchronizing and installing packages
                                        ; If package present, should have no effect
                                        ; Doesn't seem to work. Will keep here as reference http://emacs.stackexchange.com/questions/408/synchronize-packages-between-different-machines

					; Evil mode and various configurations for it
(use-package evil
  :init
  (setq evil-disable-insert-state-bindings t)

  ;; Changing color of cursor states so I know what mode I'm in
  (setq evil-emacs-state-cursor '("red" box))
  (setq evil-normal-state-cursor '("green" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("red" bar))
  (setq evil-replace-state-cursor '("red" bar))
  (setq evil-operator-state-cursor '("red" hollow))

  :config
  (evil-mode 1)

  ;; Changing default modes for some major modes
  (cl-loop for (mode . state) in '((shell-mode . insert)
                                   (help-mode . emacs)
                                   (dired-mode . emacs)
                                   (wdired-mode . normal)
                                   (text-mode . insert)
                                   (org-mode . normal))
           do (evil-set-initial-state mode state))

  (define-key evil-normal-state-map "0" 'evil-first-non-blank)
  (define-key evil-normal-state-map "^" 'evil-digit-argument-or-evil-beginning-of-line)
  )

;; Changing some key bindings in insert mode for convenience
;; I think this is obsoleted by evil-disable-insert-state-bindings?
;; (define-key evil-insert-state-map "\C-y" 'yank)
;; (define-key evil-insert-state-map "\C-e" 'end-of-line)
;; (define-key evil-insert-state-map "\C-a" 'beginning-of-line)

;; Adding key chord so I don't have to hit escape
(use-package key-chord)
(key-chord-mode 1)
(key-chord-define evil-normal-state-map "jk" 'evil-force-normal-state)
(key-chord-define evil-visual-state-map "jk" 'evil-change-to-previous-state)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-replace-state-map "jk" 'evil-normal-state)

;; Additional packages
(use-package evil-commentary
  :diminish evil-commentary-mode
  :config
  (evil-commentary-mode)
  )

(use-package evil-anzu
  :defer t 
  )

(use-package evil-easymotion
  :init
  (evilem-default-keybindings "SPC")
  )

(use-package evil-smartparens
  :diminish evil-smartparens-mode
  :init
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
  )

(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
  )

(use-package evil-quickscope
  :init
  (add-hook 'prog-mode-hook 'turn-on-evil-quickscope-always-mode)
  (add-hook 'ado-mode-hook 'turn-on-evil-quickscope-always-mode)
  )

(use-package evil-snipe
  :diminish evil-snipe-mode
  :diminish evil-snipe-override-mode
  :diminish evil-snipe-local-mode
  :init
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
  (setq evil-snipe-scope 'line
        evil-spillover-scope 'visible
        evil-snipe-repeat-scope 'visible
        evil-snipe-smart-case t)

  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1)
  )

(use-package vimish-fold)
(use-package evil-vimish-fold
  :diminish evil-vimish-fold-mode)
(evil-vimish-fold-mode 1)

                                        ; Rainbow Delimiters
(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )
(rainbow-delimiters-mode 1)

                                        ; ranger mode for better directory stuff
(use-package ranger
  :config
  (ranger-override-dired-mode t)
  (setq ranger-dont-show-binary t)
  )
					; IDO mode for better filename completion
                                        ; Possibly succeeded by helm, but whatever

(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

(defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)

                                        ; Projectile
(use-package projectile
  :init
  (setq projectile-completion-system 'helm)
  (setq projectile-indexing-method 'alien)
  )
(projectile-global-mode)

                                        ; Helm mode. Superceeds ido mode above?
(use-package helm
  :diminish helm-mode

  :init
  (require 'helm-config)

  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))

  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t)

  ;; Configuring helm to replace my life
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match    t)

  (setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

  (setq helm-buffer-max-length 60)

  :config
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

  (require 'helm-mode)
  (helm-mode 1)

  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         )
  )

(use-package helm-projectile
  :init
  ;; Helm projectile
  (helm-projectile-on)

  :config
  (setq projectile-completion-system 'helm)
  )

;; Helm swoop. Replacing helm-occur.
(use-package helm-swoop
  :init
  (global-set-key (kbd "C-c o") 'helm-swoop)
  (setq helm-swoop-use-fuzzy-match t)
  (define-key evil-motion-state-map (kbd "C-s") 'helm-swoop-from-evil-search)
  )

;; Changing helm buffer candidate size
;; I think this isn't useful?
;; (defun helm-buffer-face-mode ()
;;   "Helm buffer face"
;;   (interactive)
;;   (with-helm-buffer
;;     (setq line-spacing 2)
;;     (buffer-face-set '(:family "Courier New" :height 75))))

;; (add-hook 'helm-after-initialize-hook 'helm-buffer-face-mode)

					; Flycheck global configs
;; (setq-default flycheck-flake8-maximum-line-length 500)

(use-package ace-window
  :init
  (global-set-key (kbd "C-x o") 'ace-window)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  )

					; Aggressive Indent Mode
(use-package aggressive-indent
  :init
  (global-aggressive-indent-mode 1)
  )

					; Smartparens
(use-package smartparens
  :diminish smartparens-mode
  :init
  (smartparens-global-mode t)
  (require 'smartparens-config)

  ;; Stata additions for smartparens
  (sp-local-pair 'ado-mode "`" "'") ;; Defining locals
  (sp-local-pair 'ado-mode "`\"" "\"'") ;; Escapes in Stata
  )

					; Flycheck
;; Disabled because it slows things down and crashes my stuff
;; (add-hook 'after-init-hook #'global-flycheck-mode)

					; Yas (Snippet and template support)
(use-package yasnippet
  :config
  ;; Only enable yas snippet in certain modes
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'ado-mode-hook 'yas-minor-mode)

  (setq yas-wrap-around-region t)

  (setq yas-snippet-dirs
        (quote
         ("c:/HOME/.emacs.d/elpa/yasnippet-0.8.0/snippets" "c:/HOME/.emacs.d/elpa/elpy-20150830.510/snippets/" "C:/HOME/.emacs.d/yasnippet-snippets" "~/.emacs.d/snippets")))
  )

					;Changing key to ;;;; Depreciated!!! Might not need it anymore if company does not use tab for auto-complete
;; (define-key yas-minor-mode-map (kbd "<tab>") nil)
;; (define-key yas-minor-mode-map (kbd "TAB") nil)
;; (define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand)

					; ESS for R and stuff
(use-package ess
  :init
  (setq ess-eval-visibly (quote nowait))
  (setq ess-smart-S-assign-key "")
  (setq ess-swv-pdflatex-commands (quote ("pdflatex")))
  (setq ess-swv-processor (quote knitr))
  )
(require 'ess)

                                        ; ado mode for Stata
(add-to-list 'load-path "~/.emacs.d/ado-mode-1.14.1.0/lisp")
(use-package ado-mode
  :ensure nil)

					; Org-Mode and RefTex set up
(use-package org
  :init
  (defun org-mode-reftex-setup ()
    (load-library "reftex")
    (and (buffer-file-name) (file-exists-p (buffer-file-name))
         (progn
					;enable auto-revert-mode to update reftex when bibtex file changes on disk
           (global-auto-revert-mode t)
           (reftex-parse-all)

                                        ; Add a custom reftex cite format to insert links
           (reftex-set-cite-format "** [[papers:%l][%2a %y]]: %t \n")
           )
         )
    (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
    )

  (add-hook 'org-mode-hook 'org-mode-reftex-setup)

  (setq org-link-abbrev-alist '(("papers" . "C:\\Users\\Allan Zhang\\Dropbox\\School Work\\Economic Papers\\%s.pdf")))

					; Suggested hotkeys. See documentation
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb)
  )

                                        ; Magit
(use-package magit
  :init
  (global-set-key (kbd "C-x g") 'magit-status)
  )

					; Recentf - For easily getting recent files
(use-package recentf
  :config
  (global-set-key (kbd "C-x C-r") 'ido-recentf-open) ;; get rid of `find-file-read-only' and replace it with something more useful.
  (setq recentf-max-saved-items 50) ; 50 files ought to be enough.
  ;; Regex to ignore some files
  (add-to-list 'recentf-exclude "\\.el\\'")
  (add-to-list 'recentf-exclude "\\.log\\'")
  (add-to-list 'recentf-exclude "\\.out\\'")
  (add-to-list 'recentf-exclude "\\.aux\\'")

  (defun ido-recentf-open ()
    "Use `ido-completing-read' to \\[find-file] a recent file"
    (interactive)
    (if (find-file (ido-completing-read "Find recent file: " recentf-list))
        (message "Opening file...")
      (message "Aborting")))
  )
(recentf-mode t) ;; enable recent files mode.

'(recentf-auto-cleanup (quote mode))
					; Python
(setq python-check-command "flake8")
(setq python-indent-guess-indent-offset nil)
(setq python-shell-exec-path (quote ("C:\\Python27\\ArcGIS10.1")))
(setq python-shell-extra-pythonpaths nil)
(setq python-shell-interpreter
      "C:\\Users\\azhang1\\AppData\\Local\\Programs\\Python\\Python35\\python.exe")
(setq python-shell-interpreter-args
      "-i C:\\Users\\azhang1\\AppData\\Local\\Programs\\Python\\Python35\\Scripts\\ipython.exe")

					; Elpy for Python
(use-package elpy
  :config
  (setq elpy-modules
        (quote
         (elpy-module-eldoc elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-sane-defaults)))
  (setq elpy-rpc-backend nil)
  (setq elpy-rpc-python-command "pythonw")

  (define-key elpy-mode-map (kbd "C-c C-c") 'elpy-shell-send-region-or-buffer)
  )
(elpy-enable)

                                        ; ycmd Config for use with company mode
(use-package ycmd
  :diminish ycmd-mode
  :config
  (set-variable 'ycmd-server-command '("python" "-u" "C:\\HOME\\ycmd-master\\ycmd\\ycmd"))

  ;; Adding ycmd completion to other modes
  (add-to-list 'ycmd-file-type-map '(ado-mode "generic"))
  )

(use-package company
  :diminish company-mode 
  :config
  (add-hook 'prog-mode-hook 'company-mode)
  (add-hook 'ado-mode-hook 'company-mode)

  (global-set-key (kbd "<C-tab>") 'company-complete)

  ;; Setting hooks for company mode
  (add-hook 'ado-mode-hook (lambda () set (make-local-variable 'company-backends) '(company-dabbrev)))

  (setq company-auto-complete nil)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case nil)
  (setq company-dabbrev-other-buffers t)
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  ;; Add yasnippet support for all company backends
  ;; https://github.com/syl20bnr/spacemacs/pull/179
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  )

(use-package company-ycmd
  :init
  (add-to-list 'company-backends 'company-ycmd)
  )
(company-ycmd-setup)

(add-hook 'after-init-hook #'global-ycmd-mode)

					; Company Mode Configs
;; Fixes conflicts with tab usage between company and yasnippets
;; From: http://emacs.stackexchange.com/questions/7908/how-to-make-yasnippet-and-company-work-nicer
(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (cond
   ((minibufferp)
    (minibuffer-complete))
   (t
    (indent-for-tab-command)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (progn
              (company-manual-begin)
              (if (null company-candidates)
                  (progn
                    (company-abort)
                    (indent-for-tab-command)))))))))

(defun tab-complete-or-next-field ()
  (interactive)
  (if (or (not yas/minor-mode)
          (null (do-yas-expand)))
      (if (check-expansion)
          (progn
            (company-manual-begin)
            (if (null company-candidates)
                (progn
                  (company-abort)
                  (yas-next-field))))
        (yas-next-field))))

(defun expand-snippet-or-complete-selection ()
  (interactive)
  (if (or (not yas/minor-mode)
          (null (do-yas-expand))
          (company-abort))
      (company-complete-selection)))

(defun abort-company-or-yas ()
  (interactive)
  (if (null company-candidates)
      (yas-abort-snippet)
    (company-abort)))

;; (global-set-key [tab] 'tab-indent-or-complete)
;; (global-set-key (kbd "TAB") 'tab-indent-or-complete)
(global-set-key [(control return)] 'company-complete-common)

(define-key company-active-map [tab] 'expand-snippet-or-complete-selection)
(define-key company-active-map (kbd "TAB") 'expand-snippet-or-complete-selection)

(define-key yas-minor-mode-map [tab] 'tab-indent-or-complete)
(define-key yas-minor-mode-map (kbd "TAB") 'tab-indent-or-complete)

(define-key yas-keymap [tab] 'tab-complete-or-next-field)
(define-key yas-keymap (kbd "TAB") 'tab-complete-or-next-field)
(define-key yas-keymap [(control tab)] 'yas-next-field)
(define-key yas-keymap (kbd "C-g") 'abort-company-or-yas)

                                        ; AUCTeX Config
;; (use-package auctex
;;   :init
;;   (setq TeX-auto-save t) ; Enable parse on save
;;   (setq TeX-parse-self t) ; Enable parse on load
;;   )

;; (use-package company-auctex)
;; (company-auctex-init)

                                        ; Powerline stuff
(use-package spaceline
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  (spaceline-helm-mode)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (setq powerline-default-separator 'arrow)
  (spaceline-toggle-buffer-size)
  (spaceline-compile)
  )

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CUSTOM STUFF. DON'T TOUCH ;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-electric-left-right-brace nil)
 '(LaTeX-indent-level 4)
 '(LaTeX-item-indent 0)
 '(TeX-arg-right-insert-p t)
 '(TeX-command-list
   (quote
    (("TeX" "%(PDF)%(tex) %(file-line-error) %(extraopts) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (plain-tex-mode texinfo-mode ams-tex-mode)
      :help "Run plain TeX")
     ("LaTeX" "%'%l%(mode)%' %t" TeX-run-TeX nil
      (latex-mode doctex-mode)
      :help "Run LaTeX")
     ("Makeinfo" "makeinfo %(extraopts) %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with Info output")
     ("Makeinfo HTML" "makeinfo %(extraopts) --html %t" TeX-run-compile nil
      (texinfo-mode)
      :help "Run Makeinfo with HTML output")
     ("AmSTeX" "%(PDF)amstex %(extraopts) %`%S%(PDFout)%(mode)%' %t" TeX-run-TeX nil
      (ams-tex-mode)
      :help "Run AMSTeX")
     ("ConTeXt" "texexec --once --texutil %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt once")
     ("ConTeXt Full" "texexec %(extraopts) %(execopts)%t" TeX-run-TeX nil
      (context-mode)
      :help "Run ConTeXt until completion")
     ("BibTeX" "bibtex %s" TeX-run-BibTeX nil t :help "Run BibTeX")
     ("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber")
     ("View" "%V" TeX-run-discard-or-function t t :help "Run Viewer")
     ("Print" "%p" TeX-run-command t t :help "Print the file")
     ("Queue" "%q" TeX-run-background nil t :help "View the printer queue" :visible TeX-queue-command)
     ("File" "%(o?)dvips %d -o %f " TeX-run-command t t :help "Generate PostScript file")
     ("Index" "makeindex %s" TeX-run-command nil t :help "Create index file")
     ("Xindy" "texindy %s" TeX-run-command nil t :help "Run xindy to create index file")
     ("Check" "lacheck %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for correctness")
     ("ChkTeX" "chktex -v6 %s" TeX-run-compile nil
      (latex-mode)
      :help "Check LaTeX file for common mistakes")
     ("Spell" "(TeX-ispell-document \"\")" TeX-run-function nil t :help "Spell-check the document")
     ("Clean" "TeX-clean" TeX-run-function nil t :help "Delete generated intermediate files")
     ("Clean All" "(TeX-clean t)" TeX-run-function nil t :help "Delete generated intermediate and output files")
     ("Other" "" TeX-run-command t t :help "Run an arbitrary command"))))
 '(TeX-source-correlate-mode t)
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(auto-image-file-mode t)
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (wombat)))
 '(custom-safe-themes
   (quote
    ("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "4e262566c3d57706c70e403d440146a5440de056dfaeb3062f004da1711d83fc" default)))
 '(highlight-changes-colors ("#FD5FF0" "#AE81FF"))
 '(highlight-tail-colors
   (("#49483E" . 0)
    ("#67930F" . 20)
    ("#349B8D" . 30)
    ("#21889B" . 50)
    ("#968B26" . 60)
    ("#A45E0A" . 70)
    ("#A41F99" . 85)
    ("#49483E" . 100)))
 '(indent-tabs-mode nil)
 '(monokai-high-contrast-mode-line t)
 '(org-agenda-files
   (quote
    ("c:/Users/Allan Zhang/Dropbox/School Work/Economic Papers/EconomicPapers.org")))
 '(org-startup-indented t)
 '(preview-default-document-pt 12)
 '(preview-gs-command "c:\\Program Files (x86)\\gs\\gs8.71\\bin\\gswin32c.exe")
 '(preview-gs-options
   (quote
    ("-q" "-dNOPAUSE" "-DNOPLATFONTS" "-dPrinted" "-dTextAlphaBits=4" "-dGraphicsAlphaBits=4")))
 '(preview-scale-function 1.5)
 '(reftex-cite-punctuation (quote (", " " \\& " " et al.")))
 '(show-smartparens-global-mode t)
 '(weechat-color-list
   (unspecified "#272822" "#49483E" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0"))
 '(whitespace-action nil)
 '(whitspaceespace-display-mappings
   (quote
    ((space-mark 32
                 [183]
                 [46])
     (space-mark 160
                 [164]
                 [95])
     (tab-mark 9
               [187 9]
               [92 9])))))

;; TODO
;; Commenting out for now as I have my own custom stuff, but will return to see what these options are later
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:family "Courier New" :foundry "outline" :slant normal :weight normal :height 78 :width normal))))
;;  '(aw-leading-char-face ((t (:foreground "red" :weight bold :height 2.0))))
;;  '(whitespace-empty ((t nil)))
;;  '(whitespace-hspace ((t (:background "gray12" :foreground "#969896"))))
;;  '(whitespace-indentation ((t nil)))
;;  '(whitespace-line ((t nil)))
;;  '(whitespace-space ((t (:background "gray12" :foreground "#969896"))))
;;  '(whitespace-space-after-tab ((t nil)))
;;  '(whitespace-space-before-tab ((t nil)))
;;  '(whitespace-tab ((t nil)))
;;  '(whitespace-trailing ((t nil))))
