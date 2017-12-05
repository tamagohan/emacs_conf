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
  '(highlight-indentation
    key-chord
    elscreen
    smart-newline
    auto-complete
    smartparens
    company
    js2-mode
    alchemist
    ac-alchemist
    flycheck-mix
    org)
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

;;;;;;;;;;;;;;;;;;;;;;;; Color-theme:
(load-theme 'wombat t)

;;;;;;;;;;;;;;;;;;;;;;;; 見た目の変更
;; メニューバー、ツールバー、スクロールバーを消す, Emacs23以降
(tool-bar-mode 0)
(scroll-bar-mode 0)
(horizontal-scroll-bar-mode 0)
(menu-bar-mode 0)

;; カーソルの色
(set-cursor-color 'orange)

;; 現在行をハイライト
(global-hl-line-mode t)
(defface my-hl-line-face
  '((((class color) (background dark))  ; カラーかつ, 背景が dark ならば
     (:background "DarkSlateBlue" t))   ; 背景を黒に.
    (((class color) (background light)) ; カラーかつ, 背景が light でも
     (:background "DarkSlateBlue" t))   ; 背景を黒に.
    (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)

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

;;;;;;;;;;;;;;;;;;;;;;;; プログラミング支援
;; Ruby
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.cap$" . ruby-mode))

;; JavaScript
(require 'js2-mode)
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))


;; Erlang and Elixir
(require 'elixir-mode)
(require 'alchemist)
(require 'flycheck-mix)
(flycheck-mix-setup)
(require 'ac-alchemist)
(add-hook 'elixir-mode-hook 'ac-alchemist-setup)

;;;;;;;;;;;;;;;;;;;;;;;; org-mode
(require 'org)
