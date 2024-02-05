# GitHubとTortoiseGitとの連携

## Gitの初期設定

**Git Bash**を起動させて、下記のコマンドを打ち込みます。

連携に関連するので、Git用のユーザー名とメールアドレスは最初に登録します。

なお、他の項目に関しては後から設定変更が可能です。

```GitBash:GitBash
$ git config --global user.name "登録させるユーザー名(英数字)"
$ git config --global user.email "登録させるメールアドレス"
```



## SSH認証キーの作成・GitHubでの認証

### SSH認証キーの作成など

GitとGitHubを連携するために必要なステップです。GitからGitHubにアクセスする機会の方が多いため、SSH認証を利用します。

**Git Bash**を再度使用します。

```GitBash:GitBash
$ ssh-keygen -t rsa -b 4096 -C "登録したメールアドレス"
```
コマンド入力後は、以下を聞いてきます。

鍵を保存するディレクトリと ファイル名

`.ssh`フォルダーを作成するディレクトリは、デフォルトで「ユーザー・ディレクトリー(C:/Users/ユーザー名)です。特にこだわりが無ければ、分かりやすいのでデフォルトのままで良いでしょう。

ファイル名は、デフォルトで「id_rsa」になります。

Github用であることを明示したい場合は「id_rsa_github」としておけばよい。

最後にパスフレーズを聞かれます。任意ですが、登録を推奨します。

完成したキーは、(_githubを付けていれば)以下のようになっている。

　　/c/Users/user/.ssh/id_rsa_github　---　秘密鍵

　　/c/Users/user/.ssh/id_rsa_github.pub　---　公開鍵

最後にSSHの`config`ファイルを編集します。

GitHubへSSH接続する際に、どの秘密鍵を指定するかを管理するのが目的です。
　`.ssh`フォルダーの配下に`config`ファイルを作成します。

```GitBash:GitBash
Host GitHub
HostName github.com
User git
IdentityFile ~/.ssh/id_rsa_github
```
　`Host`はSSH接続の識別名なので任意でOKですが、分かりやすく「github」にしました。
　`HostName`は「github.com」、`User`は「git」で固定しておきます。
　どうも、Githubヘルプ曰く「リモートURLへの接続を含むすべての接続は、"git"ユーザーとして作成されなければなりません。GitHubのユーザー名で接続しようとすると失敗します。」とのことです。
　`IdentityFile`で、秘密鍵(id_rsa_github)のパスとファイル名を指定します。

## GitHubでの認証

　ここまで準備出来たら、いよいよGitとGitHubの連携を行います。
　保存した公開鍵(id_rsa_github.pub)をエディタで開き、中の文字列をコピーします。
　その際、改行や余分な文字列が入らないようにご注意ください。
　GitHubの右上にあるアイコンを押すとアカウントのメニューが登場しますので、その中から`Settings`をクリックします。
　遷移先の画面でSSH and GPG keysをクリックするとSSHキーの登録画面に移ります。
　右上の`New SSH Key`を選択してください。
　`Title`は任意ですので、必要に応じてPC名などを入れておきましょう。
　`Key`にコピーしておいた公開鍵を貼り付けたら、`Add SHH Key`を押して登録します。
　登録したら、下記のコマンドを打ち込んで設定に問題がないか確認します。

```GitBash:GitBash
$ ssh github
```
　パスフレーズを聞かれたら、SSH認証キー作成時に登録したものを入力します。
　以下のような表示になれば接続成功です。お疲れ様でした！

```GitBash:GitBash
$ ssh github
PTY allocation request failed on channel 0
Hi User-name! You've successfully authenticated, but GitHub does not provide shell access.
Connection to github.com closed.
```

にGitを操作する分にはコマンド等を調べて案外何とかなるのですが、
　そもそもの環境構築を行う機会はなかなか無いと思いました。
　そのため、自分で検索しながら実際に手を動かすといい勉強になりました。
　様々な記事を参考にさせて頂きましたが、いつか独力で書けるレベルに到達したいものです。

　ちなみに、Git(ローカル)のソース管理はVSCodeを使うと簡単です。
　先述の案件では、VSCodeでローカルコミットしてTortoiseGitでAWS CodeCommitにプッシュして…としていました。今回は、AWS CodeCommitがGitHubに変わった形です。
　TortoiseGitを組合せると、GUIでアレコレ出来るので便利でした。

最後に、以下の記事も参考にさせて頂きました、ありがとうございました。
　[GitとGithubを連携してソースのバージョン管理を行う方法](https://pagain.hatenablog.com/entry/2019/07/04/230845#Githubとの連携)
　[今日からはじめるGitHub 〜 初心者がGitをインストールして、プルリクできるようになるまでを解説](https://employment.en-japan.com/engineerhub/entry/2017/01/31/110000#%E7%92%B0%E5%A2%83%E3%81%AE%E6%A7%8B%E7%AF%892-SSH%E3%81%AE%E9%8D%B5%E3%82%92%E5%8F%96%E5%BE%97%E3%81%99%E3%82%8B)

