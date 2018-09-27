(defconst FONT_SIZE 10)
(set-face-attribute 'default nil :family "Menlo" :height 140)
(set-fontset-font (frame-parameter nil 'font)
                  'japanese-jisx0208
                  (font-spec :family "Hiragino Kaku Gothic ProN"))
(add-to-list 'face-font-rescale-alist
             '(".*Hiragino Kaku Gothic ProN.*" . 1.2))

(cd "~/")

;;;;;;;;;;;;;;;;;;;;;;;; load path
(add-to-list 'load-path "~/.emacs.d/vendor/")
;;;;;;;;;;;;;;;;;;;;;;;; load path

;;;;;;;;;;;;;;;;;;;;;;;; MELPA
(defvar my-favorite-package-list
  '(exec-path-from-shell
    atom-one-dark-theme
    linum
    highlight-indentation
    key-chord
    elscreen
    smart-newline
    auto-complete
    smartparens
    company
    flyspell
    js2-mode
    scala-mode
    alchemist
    ac-alchemist
    flycheck-mix
    org
    markdown-mode
    w3m
    pdf-tools
    rust-mode
    racer
    flycheck-rust
    company-racer)
  "packages to be installed")
;;;;;;;;;;;;;;;;;;;;;;;; MELPA

;;;;;;;;;;;;;;;;;;;;;;;; packageの自動install
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(dolist (pkg my-favorite-package-list)
  (unless (package-installed-p pkg)
    (package-install pkg)))
;;;;;;;;;;;;;;;;;;;;;;;; packageの自動install

;;;;;;;;;;;;;;;;;;;;;;;; exec path
(require 'exec-path-from-shell)
  (when (memq window-system '(mac ns x))
(exec-path-from-shell-initialize))
;;;;;;;;;;;;;;;;;;;;;;;;


;;; バックアップファイルを作らない
(setq make-backup-files nil)
(setq auto-save-default nil)

;;;;;;;;;;;;;;;;;;;;;;;; 画面分割
;; "C-=" (C-S--), "C-|"で画面分割
(global-set-key (kbd "C-=") 'split-window-vertically)
(global-set-key (kbd "C-|") 'split-window-horizontally)

;; "C-S-hjkl"でウィンドウ移動
(global-set-key (kbd "C-S-h") 'windmove-left)
(global-set-key (kbd "C-S-j") 'windmove-down)
(global-set-key (kbd "C-S-k") 'windmove-up)
(global-set-key (kbd "C-S-l") 'windmove-right)

;;;;;;;;; save
(setq auto-save-default nil)

;;;;;;;;;;;;;;;;;;;;;;;; 見た目の変更
;; Color-theme:
(load-theme 'atom-one-dark t)

;; 行番号
(require 'linum)
(global-linum-mode)

;; メニューバー、ツールバー、スクロールバーを消す, Emacs23以降
(tool-bar-mode 0)
(scroll-bar-mode 0)
(horizontal-scroll-bar-mode 0)
(menu-bar-mode 0)

;; カーソルの色
(set-cursor-color 'orange)

;; 現在行をハイライト
(global-hl-line-mode t)

;; 行の折り返し
(require 'key-chord)
(key-chord-define-global "kl" 'toggle-truncate-lines)

;; 対応する括弧を表示させる
(show-paren-mode t)

;; 行番号・桁番号を表示
(line-number-mode t)
(column-number-mode t)

;; タブ、全角スペース、行末のスペースを見えるように (コメントを外すと改行が見えるように)
;;; インデント時にタブを使わないでスペースを使う
(setq-default tab-width 2 indent-tabs-mode nil)

;;(defface my-face-r-1 '((t (:background "gray15"))) nil)
(defface my-face-b-1 '((t (:background "gray"))) nil)
(defface my-face-b-2 '((t (:background "gray20"))) nil)
(defface my-face-u-1 '((t (:foreground "SteelBlue" :underline t))) nil)
;;(defvar my-face-r-1 'my-face-r-1)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)

(add-hook 'font-lock-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("[ \t]+$" 0 my-face-u-1 append)
               ("\t" 0 'my-face-b-2 append)
               ("　" 0 my-face-b-1 append)
               ))))

;; Font
(add-to-list 'face-font-rescale-alist '("MeiryoKe_PGothic" . 1.09))
(create-fontset-from-ascii-font
 "Arial:weight=normal:slant=normal" nil "variable")
(set-fontset-font
 "fontset-variable" 'japanese-jisx0208 (font-spec :family "MeiryoKe_PGothic"))
(set-face-attribute 'variable-pitch nil :fontset "fontset-variable")
;;;;;;;;;;;;;;;;;;;;;;;; 見た目の変更

;;;;;;;;;;;;;;;;;;;;;;;; elscreenの設定
(elscreen-start)
(global-set-key [C-S-right] 'elscreen-next)
(global-set-key (kbd "C->") 'elscreen-next)
(global-set-key [C-S-left]  'elscreen-previous)
(global-set-key (kbd "C-<") 'elscreen-previous)

;;;;;;;;;;;;;;;;;;;;;;;; カーソル移動
;; 自動インデント
(require 'smart-newline)
(global-set-key (kbd "C-m") 'smart-newline)

;; "C-h"をbackspaceに (これで<C-backspace>が反応しなくなるので、bindしなおす)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "<C-backspace>") 'backward-kill-word)

;; "C-a"で「行頭」と「インデントを飛ばした行頭」を行き来する
(defun u-move-beginning-of-line ()
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))
(global-set-key (kbd "C-a") 'u-move-beginning-of-line)

;; 物理行単位で移動
(setq line-move-visual nil)
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;; 補完
;; auto-complete
(require 'auto-complete-config)
(global-auto-complete-mode t)

;; 括弧の自動補完
(require 'smartparens-config)
(smartparens-global-mode t)
(show-smartparens-global-mode t)

;; company-mode
(require 'company)
(global-company-mode)
;;;;;;;;;;;;;;;;;;;;;;;; 補完
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company web-mode smartparens smart-newline projectile key-chord highlight-indentation elscreen auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;; CUA
(cua-mode t)
(setq cua-enable-cua-keys nil)                         ;; C-cやC-vの乗っ取りを阻止
(define-key cua-global-keymap (kbd "C-S-SPC") 'ignore) ;; C-S-SPCを空ける(日本語モード => 戻す)
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;; flyspell
(setq-default ispell-program-name "/usr/local/bin/aspell")
(eval-after-load "ispell"
  '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

(require 'flyspell)
(ispell-change-dictionary "american")
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;; プログラミング支援
;; Ruby
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.cap$" . ruby-mode))

;; JavaScript
(require 'js2-mode)
(require 'flow-js2-mode)
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))
(add-hook 'js2-mode-hook 'flow-js2-mode)

;; Erlang and Elixir
(require 'elixir-mode)
(require 'alchemist)
(require 'flycheck-mix)
(flycheck-mix-setup)
(require 'ac-alchemist)
(add-hook 'elixir-mode-hook 'ac-alchemist-setup)

;; Java
(add-hook 'java-mode-hook
  (lambda ()
    (setq indent-tabs-mode nil)
    (setq c-basic-offset 2)))

;; Scala
(require 'scala-mode)

;; Rust
;;(add-to-list 'exec-path (expand-file-name "~/.cargo/bin"))
(require 'company-racer)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-racer))
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'rust-mode-hook #'flycheck-rust-setup)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; org-mode
(require 'org)
;; 見出しをインデントする
(setq org-startup-indented t)
;; インデントの幅を設定
(setq org-indent-indentation-per-level 2)
;; 見出しの初期状態（見出しだけ表示）
(setq org-startup-folded 'content)

;; markdown mode
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md$"       . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . gfm-mode))
(setq markdown-command "/usr/local/bin/multimarkdown")
;; save時にHTML変換したファイルを保存し、w3mで表示
(require 'w3m)
(define-key markdown-mode-map (kbd "\C-c \C-c \C-v")
  (lambda ()
    (interactive)
    (setq html-file-name (concat (file-name-sans-extension (buffer-file-name)) ".html"))
    (markdown-export html-file-name)
    (if (one-window-p) (split-window))
    (other-window 1)
    (w3m-find-file html-file-name)))

;; rainbow-delimiters を使うための設定
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; 括弧の色を強調する設定
(require 'cl-lib)
(require 'color)
(defun rainbow-delimiters-using-stronger-colors ()
  (interactive)
  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count
   do
   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
    (cl-callf color-saturate-name (face-foreground face) 30))))
(add-hook 'emacs-startup-hook 'rainbow-delimiters-using-stronger-colors)
;;;;;;;;;;;;;;;;;;;;;;;;


(pdf-tools-install)
