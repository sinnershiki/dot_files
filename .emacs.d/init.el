;;---------------------------------------------------
;;
;; El-get設定
;;---------------------------------------------------

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;;(package-initialize)

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; el-get https://github.com/dimitri/el-get
(add-to-list 'load-path (locate-user-emacs-file "el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; el-getでダウンロードしたパッケージは ~/.emacs.d/elisp に入るようにする
(setq el-get-dir (locate-user-emacs-file "elisp"))

;;---------------------------------------------------
;;
;; Emacs全般の設定
;;---------------------------------------------------
;; system-type predicates
(defun x->bool (elt) (not (not elt)))
(setq darwin-p  (eq system-type 'darwin)
      ns-p      (eq window-system 'ns)
      carbon-p  (eq window-system 'mac)
      linux-p   (eq system-type 'gnu/linux)
      colinux-p (when linux-p
                  (let ((file "/proc/modules"))
                    (and
                     (file-readable-p file)
                     (x->bool
                      (with-temp-buffer
                        (insert-file-contents file)
                        (goto-char (point-min))
                        (re-search-forward "^cofuse\.+" nil t))))))
      cygwin-p  (eq system-type 'cygwin)
      nt-p      (eq system-type 'windows-nt)
      meadow-p  (featurep 'meadow)
      windows-p (or cygwin-p nt-p meadow-p))

;; 起動時に出てくるメッセージを消す
(setq inhibit-startup-message t)

;; 行、列の表示
(line-number-mode t)
(column-number-mode t)
(global-linum-mode t)

;; C-h to Backspace
(global-set-key "\C-h" 'delete-backward-char)

;; C-c C-c to comment-or-uncomment-region
(global-set-key "\C-c\C-c" 'comment-or-uncomment-region)

;; タブ幅を設定
(setq-default tab-width 4 indent-tabs-mode nil)

;; ビープを無効に
(setq ring-bell-function 'ignore)

;; 折り返し
(setq truncate-partial-width-windows nil)

;; シンボリックリンクのリンク先を直接開く
(setq vc-follow-symlinks t)

;; ファイルの自動リロード
(global-auto-revert-mode 1)

;; ツールバー・メニューバーの非表示
(menu-bar-mode -1)
(tool-bar-mode -1)

;; スクロールを良い感じにする
(defun scroll-down-with-lines ()
  "" (interactive) (scroll-down 3))
(defun scroll-up-with-lines ()
  "" (interactive) (scroll-up 3))
(global-set-key [wheel-up] 'scroll-down-with-lines)
(global-set-key [wheel-down] 'scroll-up-with-lines)
(global-set-key [double-wheel-up] 'scroll-down-with-lines)
(global-set-key [double-wheel-down] 'scroll-up-with-lines)
(global-set-key [triple-wheel-up] 'scroll-down-with-lines)
(global-set-key [triple-wheel-down] 'scroll-up-with-lines)

;; 保存するタイミングでdelete-trailing-whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; リージョン削除
(delete-selection-mode t)

;; オートセーブやバックアップファイルを~/.emacs.d/backupsに入れる
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))

;;----------------------------------------------------
;;
;; Text and Font
;;----------------------------------------------------
;; 背景色と文字色
(setq default-frame-alist
      (append
       '((foreground-color . "ivory")
         (background-color . "black")
         (cursor-color . "deep pink")
         (alpha . (80 55 nil nil)))
       default-frame-alist))


;; フォントを指定
;; linux
(when linux-p
  (set-face-attribute 'default nil
                      :family "Ricty"
                      :height 130))
;; mac
(when darwin-p
  (set-face-attribute 'default nil
                      :family "Ricty"
                      :height 180))

;;---------------------------------------------------
;;
;; 機能拡張
;;---------------------------------------------------
;;画面を 2 分割したときの 上下を入れ替える swap screen
(defun swap-screen()
  "Swap two screen,leaving cursor at current window."
  (interactive)
  (let ((thiswin (selected-window))
        (nextbuf (window-buffer (next-window))))
    (set-window-buffer (next-window) (window-buffer))
    (set-window-buffer thiswin nextbuf)))
(defun swap-screen-with-cursor()
  "Swap two screen,with cursor in same buffer."
  (interactive)
  (let ((thiswin (selected-window))
        (thisbuf (window-buffer)))
    (other-window 1)
    (set-window-buffer thiswin (window-buffer))
    (set-window-buffer (selected-window) thisbuf)))
(global-set-key [f2] 'swap-screen)
(global-set-key [S-f2] 'swap-screen-with-cursor)

;; 対応する括弧をハイライト
(show-paren-mode t)
(setq show-paren-style 'expression)
(set-face-background 'show-paren-match-face "gray15")
(set-face-foreground 'show-paren-match-face "white")

;; 矢印キーで画面移動
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <right>") 'windmove-right)

(put 'upcase-region 'disabled nil)

;;---------------------------------------------------
;;
;; el-getによるPackage管理
;;---------------------------------------------------

;; 便利系
;;---------------------------------------------------
;; sequential-command
(el-get-bundle sequential-command)
(global-set-key "\C-a" 'seq-home)
(global-set-key "\C-e" 'seq-end)
(when (require 'org nil t)
  (define-key org-mode-map "\C-a" 'org-seq-home)
  (define-key org-mode-map "\C-e" 'org-seq-end))
(define-key esc-map "u" 'seq-upcase-backward-word)
(define-key esc-map "c" 'seq-capitalize-backward-word)
(define-key esc-map "l" 'seq-downcase-backward-word)

;; google transelate
(el-get-bundle google-translate)
(require 'google-translate-default-ui)
(defvar google-translate-english-chars "[:ascii:]’“”–"
  "これらの文字が含まれているときは英語とみなす")
(defun google-translate-enja-or-jaen (&optional string)
  "regionか、現在のセンテンスを言語自動判別でGoogle翻訳する。"
  (interactive)
  (setq string
        (cond ((stringp string) string)
              (current-prefix-arg
               (read-string "Google Translate: "))
              ((use-region-p)
               (buffer-substring (region-beginning) (region-end)))
              (t
               (save-excursion
                 (let (s)
                   (forward-char 1)
                   (backward-sentence)
                   (setq s (point))
                   (forward-sentence)
                   (buffer-substring s (point)))))))
  (let* ((asciip (string-match
                  (format "\\`[%s]+\\'" google-translate-english-chars)
                  string)))
    (run-at-time 0.1 nil 'deactivate-mark)
    (google-translate-translate
     (if asciip "en" "ja")
     (if asciip "ja" "en")
     string)))
(global-set-key (kbd "C-c t") 'google-translate-enja-or-jaen)

;; anzu設定
(el-get-bundle anzu)
(global-anzu-mode +1)
(global-set-key (kbd "C-x q") 'anzu-query-replace)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-search-threshold 1000)
 '(package-selected-packages
   (quote
    (sequential-command nil pkg-info let-alist git-commit ess-R-data-view csv-mode css-mode)))
 '(safe-local-variable-values
   (quote
    ((eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
           (add-hook
            (quote write-contents-functions)
            (lambda nil
              (delete-trailing-whitespace)
              nil))
           (require
            (quote whitespace))
           "Sometimes the mode needs to be toggled off and on."
           (whitespace-mode 0)
           (whitespace-mode 1))
     (whitespace-line-column . 80)
     (whitespace-style face tabs trailing lines-tail)))))

;; autocomplete
(el-get-bundle auto-complete)
(ac-config-default)

;; yasnippet
(el-get-bundle yasnippet)
(yas-global-mode 1)
(setq yas-snippet-dirs
      '("~/.emacs.d/mysnippets"
        "~/.emacs.d/snippets"
        ))
;; 既存スニペットを挿入する
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
;; 新規スニペットを作成するバッファを用意する
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
;; 既存スニペットを閲覧・編集する
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)

;; flycheck
(el-get-bundle flycheck)
(global-flycheck-mode)
(global-set-key (kbd "C-c C-n") 'flycheck-next-error)
(global-set-key (kbd "C-c C-p") 'flycheck-previous-error)

;; git
(el-get-bundle git-gutter)
(el-get-bundle git-gutter-fringe)
(el-get-bundle magit)
(global-set-key (kbd "C-c m") 'magit-status)

;; twittering mode
(el-get-bundle twittering-mode)
(setq twittering-use-master-password t)

;; tramp
(el-get-bundle tramp)
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))

;; window
;; Then use C-S-<left>, C-S-<right>, C-S-<up>, and C-S-<down> to move window edges.
(el-get-bundle windsize)
(windsize-default-keybindings)

;; neotree
(el-get-bundle neotree)
(global-set-key (kbd "C-x n") 'neotree-toggle)
;; 隠しファイルをデフォルトで表示
(setq-default neo-show-hidden-files t)
(setq neo-smart-open t)

;; editorconfig
(el-get-bundle editorconfig)
(editorconfig-mode 1)

;; quickrun
(el-get-bundle quickrun)
(global-set-key (kbd "<f5>") 'quickrun)
(global-set-key (kbd "C-<f5>") 'quickrun-with-arg)
(global-set-key (kbd "M-<f5>") 'quickrun-compile-only)

;; undoまわり
;; undo tree
(el-get-bundle undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)
;; undohist
(el-get-bundle undohist)
(require 'undohist)
(undohist-initialize)

;; volatile-highlights
(el-get-bundle volatile-highlights)
(volatile-highlights-mode t)
(vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
(vhl/install-extension 'undo-tree)

;; projectile
;; Enable projectile-mode, open a file in one of your projects and type a command such as C-c p f.
(el-get-bundle projectile)

;; hlinum linum-modeのハイライト
(el-get-bundle tom-tan/hlinum-mode)
(hlinum-activate)

;; powerline
(el-get-bundle powerline)
(powerline-default-theme)
(set-face-attribute 'mode-line nil
                    :foreground "#fff"
                    :background "#0000ff"
                    :box nil)

(set-face-attribute 'powerline-active1 nil
                    :foreground "#fff"
                    :background "#7777ff"
                    :inherit 'mode-line)

(set-face-attribute 'powerline-active2 nil
                    :foreground "#000"
                    :background "#aaaaff"
                    :inherit 'mode-line)

;; smooth-scroll
(el-get-bundle smooth-scroll)
(smooth-scroll-mode t)

;; smartparens
(el-get-bundle smartparens)
(smartparens-global-mode t)

;; org-tree-slide
(el-get-bundle org-tree-slide)
(setq org-tree-slide-heading-emphasis t)
(define-key global-map (kbd "<f4>") 'org-tree-slide-mode)

;; popwin
(el-get-bundle popwin)
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(setq popwin:popup-window-position 'bottom)
(push '("*quickrun*") popwin:special-display-config)
(push '("*Google Translate*") popwin:special-display-config)
;;(push '("*YaTeX-typesetting*") popwin:special-display-config)

;; 言語系
;;---------------------------------------------------
;; ruby-mode
(el-get-bundle ruby-mode)
(add-to-list 'auto-mode-alist '("\\.rb$latex " . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
;; 括弧の後のインデントが深くなるのを防ぐ
(setq ruby-deep-indent-paren-style nil)
;; マジックコメントの自動挿入を停止
(setq ruby-insert-encoding-magic-comment nil)
(el-get-bundle ruby-electric)
(add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))
(setq ruby-electric-expand-delimiters-list nil)
;; ruby-block.el --- highlight matching block
(el-get-bundle ruby-block)
(require 'ruby-block)
(add-hook 'ruby-mode-hook '(lambda () (ruby-block-mode t)))
(setq ruby-block-highlight-toggle t)
;; inf-ruby
(el-get-bundle inf-ruby)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

;; rails関係
(el-get-bundle rake)
(el-get-bundle rails-el)

;; rhtml-mode
(el-get-bundle rhtml-mode)
(add-to-list 'auto-mode-alist '("\\.erb$" . rhtml-mode))

;; haml-mode
(el-get-bundle haml-mode)

;; haskell-mode
(el-get-bundle haskell-mode)
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs$" . literate-haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))

;; js-mode
(el-get-bundle js2-mode)
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq-default c-basic-offset 2)

;; css-mode
;;(el-get-bundle css-mode)
;;(setq css-indent-level 2)

;; coffee-mode
(el-get-bundle coffee-mode)
(defun coffee-custom ()
  "coffee-mode-hook"
  (and (set (make-local-variable 'tab-width) 2)
       (set (make-local-variable 'coffee-tab-width) 2))
  )
(add-hook 'coffee-mode-hook
  '(lambda() (coffee-custom)))

;; python
(el-get-bundle python-mode)

;; elixir mode
(el-get-bundle pkg-info)
(el-get-bundle elixir-lang/emacs-elixir)

;; sh-mode
(setq-default sh-indentation 2
              sh-indent-for-case-label 0
              sh-indent-for-case-alt '+)

;; markdown-mode
(el-get-bundle markdown-mode)

;; yatex-mode
;; run yatex mode when open .tex file
(el-get-bundle yatex)
(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq tex-command "platex")
(setq dviprint-command-format "dvipdfmx %s")
;; use utf-8 on yatex mode
(setq YaTeX-kanji-code 4)

;; ファイル系（csvや設定ファイル等）
;;---------------------------------------------------
;; yaml-mode
(el-get-bundle yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; csv-mode
(el-get-bundle csv-mode)

;; nginx-mode
(el-get-bundle nginx-mode)

;; json-mode
(el-get-bundle json-mode)
(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
