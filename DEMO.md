# Copilot Chat Agent Demo

このドキュメントは、copilot-chat-agent機能のデモと使用方法を説明します。

## セットアップ

1. 依存関係がインストールされていることを確認:
```bash
emacs --batch --eval "(progn (require 'package) (add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\")) (package-initialize) (package-refresh-contents) (package-install 'aio) (package-install 'request) (package-install 'transient) (package-install 'polymode) (package-install 'markdown-mode) (package-install 'shell-maker))"
```

2. agent機能をロード:
```elisp
;; .emacsまたはinit.elに追加
(add-to-list 'load-path "/path/to/copilot-chat-agent.el")
(require 'copilot-chat)
(require 'copilot-chat-agent)

;; Agent機能を有効化
(copilot-chat-agent-mode-enable)

;; 自動実行レベルを設定（推奨: 'read-only または 'safe）
(setq copilot-chat-agent-auto-run-level 'read-only)
```

## 使用例

### 基本的な使用方法

1. Copilot Chatを開始:
```
M-x copilot-chat-display
```

2. シェルコマンドを含む質問をする例:
```
"このマシンのグローバルIPアドレスをコマンド実行して調べて"
```

3. Copilotが以下のような応答をした場合:
```
*** このマシンのグローバルIPアドレスを調べる方法

以下のコマンドを実行することで、このマシンのグローバルIPアドレスを確認できます。

#+BEGIN_SRC shell
curl ifconfig.me
#+END_SRC

または、以下のコマンドでも同様の結果を得ることができます。

#+BEGIN_SRC shell
curl -s https://api.ipify.org
#+END_SRC
```

4. Agent機能が有効で`auto-run-level`が`read-only`に設定されている場合:
   - `curl ifconfig.me`が自動実行される
   - `curl -s https://api.ipify.org`が自動実行される
   - 実行結果がメッセージで表示される

### 安全性レベル

- **none**: 自動実行なし（すべて手動確認）
- **read-only**: 読み取り専用コマンドのみ（`ls`, `cat`, `curl`, `git status`など）
- **safe**: 安全なコマンド（`mkdir`, `touch`, `git add`など）
- **write**: ファイル変更を伴うコマンド（注意が必要）
- **all**: すべてのコマンド（危険、非推奨）

### 設定の変更

Transientメニューから設定を変更:
```
M-x copilot-chat-transient
```
→ 'A'キーを押してAgentメニューを開く

または直接コマンドで:
```elisp
;; Agent機能の有効/無効切り替え
(copilot-chat-agent-mode-toggle)

;; 自動実行レベルの変更
(copilot-chat-agent-set-auto-run-level 'safe)

;; 統計情報の表示
(copilot-chat-agent-statistics)

;; デバッグ情報の表示
(copilot-chat-agent-debug-info)
```

## デモ実行

テスト用のデモスクリプトを実行:
```bash
emacs --load agent-setup.el
```

その後、以下のコマンドでテストを実行:
```
M-x test-agent-with-sample-response
```

## トラブルシューティング

### よくある問題

1. **Agent機能が動作しない**
   - `(copilot-chat-agent-mode-p)`で有効化されているか確認
   - 依存関係が正しくインストールされているか確認

2. **コマンドが抽出されない**
   - org-modeフォーマット（`#+BEGIN_SRC shell`）が正しく使われているか確認
   - コードブロックが正しく閉じられているか（`#+END_SRC`）確認

3. **コマンドが自動実行されない**
   - `copilot-chat-agent-auto-run-level`の設定を確認
   - コマンドの分類（`copilot-chat-agent--classify-command`）を確認

### デバッグ

デバッグ情報を表示:
```elisp
(copilot-chat-agent-debug-info)
```

コマンド抽出のテスト:
```elisp
(copilot-chat-agent--extract-commands "#+BEGIN_SRC shell\nls -la\n#+END_SRC")
```

## 注意事項

- **セキュリティ**: agent機能は便利ですが、実行するコマンドを理解してから使用してください
- **auto-run-level**: 初回使用時は`read-only`または`safe`レベルから始めることを推奨
- **確認**: 危険なコマンドは常にユーザー確認が必要です
- **ログ**: すべての実行コマンドはログに記録されます

## サポートされているコマンド分類

### Read-only（読み取り専用）
- `ls`, `cat`, `head`, `tail`, `grep`, `find`
- `git status`, `git log`, `git diff`
- `curl`, `ps`, `df`, `whoami`など

### Safe（安全）
- `mkdir`, `touch`, `cp`, `ln`
- `git add`, `git commit`
- `npm install`, `make build`など

### Write（書き込み）
- `mv`, `chmod`, `git rm`
- `npm uninstall`など

### Dangerous（危険）
- `rm`, `sudo`コマンド
- ネットワークアクセス（`wget`, `ssh`など）

### Forbidden（禁止）
- `rm -rf /`, `dd`などのシステム破壊コマンド