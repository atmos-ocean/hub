#０. GitHubアカウントの作成
　プロジェクト/リポジトリの有無は今回は問いません。
　・作成済であれば、「１. Gitのインストール」に進んでください。
　・未作成であれば、[こちら](https://github.com/)から作成が可能です。
　　手順は以下の通りです。

　取得したいユーザー名・メールアドレス・設定したいパスワードを入力
　　↓
　「Sign up for GitHub」をクリックし、無料版か有料版かの選択画面に遷移
　　↓
　「Unlimited public repositories for free.」を選択し、「Continue」をクリック
　　↓
　アンケート画面は回答が任意ですので、ご自由にどうぞ。
　　↓
　ユーザー登録が完了するので、メールによるアカウント認証を行えば完了です。

#１. Gitのインストール
##　exeファイルのダウンロード
　[インストール用exeファイルをダウンロードする](https://git-scm.com/downloads)
　自身のOSによってページ内の踏むリンクが変わります。ご注意ください。
　　・Mac OS
　　・Windows
　また、32bitか64bitかビット数も要確認です。

　インストールに関しては、より詳細な記事を書いて下さっているこの方を参考にさせて頂きました。
　[Windows10にGitをインストールして初期設定する](https://qiita.com/taketakekaho/items/75161e1273dca98cb4e1)

##　Gitの初期設定
　Git Bashを起動させて、下記のコマンドを打ち込みます。
　連携に関連するので、Git用のユーザー名とメールアドレスは最初に登録します。
　なお、他の項目に関しては後から設定変更が可能です。

```GitBash:GitBash
$ git config --global user.name "登録させるユーザー名(英数字)"
$ git config --global user.email "登録させるメールアドレス"
```

#２. TortoiseGitのインストール
　※Mac OSユーザーの方は「３. 公開鍵の作成」に進んでください。
　　**Windowsユーザー かつ GUIでGitを操作したい方向け**のツールです。
##　exeファイルのダウンロード
　[インストール用exeファイルをダウンロードする](https://tortoisegit.org/)

　インストールに関しては、より詳細な記事を書いて下さっているこの方を参考にさせて頂きました。
　記事内の「TortoiseGitのセットアップ」を読んで頂ければ大丈夫です。
　[TortoiseGitのセットアップ](https://qiita.com/SkyLaptor/items/6347f38c8c010f4d5bd2#git-for-windows%E3%81%AE%E3%82%BB%E3%83%83%E3%83%88%E3%82%A2%E3%83%83%E3%83%97)

#３. SSH認証キーの作成・GitHubでの認証
##SSH認証キーの作成など
　GitとGitHubを連携するために必要(というより最重要)なステップです。
　GitからGitHubにアクセスする機会の方が多いため、SSH認証を利用します。
　(GitHubからGitへのアクセスはクローンとプルで大体どうにかなるはず)
　Git Bashを再度使用します。

```GitBash:GitBash
$ ssh-keygen -t rsa -b 4096 -C "登録したメールアドレス"
```
　コマンド入力後は、以下の2つを聞いてきます。
　　① 鍵を保存するディレクトリ
　　② ファイル名
　`.ssh`フォルダーを作成するディレクトリは、デフォルトで「ユーザー・ディレクトリー(C:/Users/ユーザー名)です。
　特にこだわりが無ければ、分かりやすいのでデフォルトのままで良いでしょう。
　ファイル名は、デフォルトで「id_rsa」になります。
　後の指定を考慮すれば、デフォルトのままの方が楽です。
　Github用であることを明示したい場合は「_github」と付ければ良いと思います。
　最後にパスフレーズを聞かれます。任意ですが、登録を推奨します。

　完成したキーは、(_githubを付けていれば)以下のようになっていると思います。
　　/c/Users/user/.ssh/id_rsa_github　---　秘密鍵
　　/c/Users/user/.ssh/id_rsa_github.pub　---　公開鍵

　最後にSSHの`config`ファイルを編集します。
　GitHubへSSH接続する際に、どの秘密鍵を指定するかを管理するのが目的です。
　`.ssh`フォルダーの配下に`config`ファイルを作成します。

```GitBash:GitBash
$ Host GitHub
$ HostName github.com
$ User git
$ IdentityFile ~/.ssh/id_rsa_github
```
　`Host`はSSH接続の識別名なので任意でOKですが、分かりやすく「github」にしました。
　`HostName`は「github.com」、`User`は「git」で固定しておきます。
　どうも、Githubヘルプ曰く「リモートURLへの接続を含むすべての接続は、"git"ユーザーとして作成されなければなりません。GitHubのユーザー名で接続しようとすると失敗します。」とのことです。
　`IdentityFile`で、秘密鍵(id_rsa_github)のパスとファイル名を指定します。

##GitHubでの認証
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

