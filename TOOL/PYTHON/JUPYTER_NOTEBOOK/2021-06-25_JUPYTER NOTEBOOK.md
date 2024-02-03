# JUPYTER NOTEBOOK

[[_TOC_]]



## JUPYTERをサーバー上で起動させる

### WINDOWSのコマンドプロンプトを使ってサーバーに接続

#### WINDOWSコマンドプロンプト起動

Windows キー＋Rキーを押して、「ファイル名を指定して実行」というウィンドウを開く



「名前」欄に

```
cmd
```

と入力する。

### コマンドプロンプト上でssh起動

Microsoft Windows [Version 10.0.19042.1052]

```bash
ssh -L 8888:localhost:8888 -Y am@133.67.98.41
```

**上記コマンドの意味**: 自分が使っているパソコン (localhost)から，サーバー (IPアドレス133.67.98.41)にポート番号8888を使って接続する。localhostで用いるポート番号も8888とする。 

am@133.67.98.197's password:

サーバで使っているパスワード入力

Last login: Fri Jun 25 11:32:16 2021 from 133.67.237.16

サーバに接続した。



これ以降以降，コマンドはwindowsプロンプトから打ち込むが，**ssh接続しているので，サーバー上で操作を行っていることに注意する**



#### anaconda起動

am@p5820
/work03/am
2021-06-25_19-39

```bash
$ conda activate
```

anacondaとはpython用の実行環境を用意するツールのこと

(base)
am@p5820
/work03/am
2021-06-25_19-39

(base)とはanacondaのデフォルトの環境のことを意味する。

#### jupyter notebook起動

```bash
$ jupyter notebook
```

[I 19:39:27.331 NotebookApp] JupyterLab extension loaded from /work03/am/anaconda3/lib/python3.8/site-packages/jupyterlab
[I 19:39:27.331 NotebookApp] JupyterLab application directory is /work03/am/anaconda3/share/jupyter/lab
[I 19:39:27.344 NotebookApp] ローカルディレクトリからノートブックをサーブ: /work03/am
[I 19:39:27.344 NotebookApp] Jupyter Notebook 6.1.4 is running at:
[I 19:39:27.344 NotebookApp] http://localhost:8888/
[I 19:39:27.345 NotebookApp] サーバを停止し全てのカーネルをシャットダウンするには Control-C を使って下さい(確認をスキ ップするには2回)。



### jupyter notebookでの作業

これ以降，**自分が使っているパソコンのブラウザを通してサーバを操作する**ことになる



#### ブラウザ起動

自分が使っているパソコン上で，ブラウザ (edge, chrome, firefox等のアプリ)を開く

アドレスバーに下記を入力

```bash
http://localhost:8888/
```

#### jupyter notebookへのログイン

ログイン名とパスワードを入力する



## jupyter notebook使用法

https://sites.google.com/site/tsubasakohyama/%E6%B0%97%E8%B1%A1%E3%83%87%E3%83%BC%E3%82%BF%E8%A7%A3%E6%9E%90



## PythonのTips

### ゼロからのPython入門講座

https://www.python.jp/train/index.html

### 作図の設定

#### 座標軸の設定

https://www.yutaka-note.com/entry/matplotlib_axis

### 文字と数式の書式設定

#### Jupyter Notebook で数式を美しく書く

https://qiita.com/namoshika/items/63db972bfd1030f8264a

#### Texによる数式表現8～論理記号

https://atatat.hatenablog.com/entry/2020/05/08/205704

#### 行列環境のエトセトラをまとめる

https://qiita.com/Yarakashi_Kikohshi/items/d488d3b2f98f37354b8a

#### ベクトル書きたい

https://qiita.com/Yarakashi_Kikohshi/items/324e07b0162cfb618dc9

#### テンソル書きたい

https://qiita.com/Yarakashi_Kikohshi/items/cc2e204e408940813335

#### tenomoto

https://github.com/tenomoto

#### 括弧の大きさを指定

https://medemanabu.net/latex/bracket/

#### 数式の括弧の大きさを自動調整したい

http://latextips.seesaa.net/article/450090699.html







# 参考：gitlabにnotebookをアップロードする

## SSH公開鍵

https://tusukuru.hatenablog.com/entry/2018/08/29/021651

### 公開鍵の作成

am@p5820
/work03/am
2021-06-25_19-52

```
$ ssh-keygen -t rsa -C infoaofd@gmail.com
```

Generating public/private rsa key pair.
Enter file in which to save the key (/work03/am/.ssh/id_rsa):
Enter passphrase (empty for no passphrase):
Enter same passphrase again:
Your identification has been saved in /work03/am/.ssh/id_rsa.
Your public key has been saved in /work03/am/.ssh/id_rsa.pub.

/work03/am
2021-06-25_19-56

```
$ ll  ~/.ssh/
```

合計 12K
-rw-------. 1 am 1.7K 2021-06-25 19:54 id_rsa
-rw-r--r--. 1 am  400 2021-06-25 19:54 id_rsa.pub
-rw-r--r--. 1 am  756 2021-06-25 19:51 known_hosts



### gitlabに公開鍵を登録する

https://tusukuru.hatenablog.com/entry/2018/08/29/021651

**１：鍵を作成する**

**２：作成した鍵をコピーする**

**３：gitlabに公開鍵を登録する**



#### １：鍵を作成する。

鍵を作成する。 (メールアドレスはgitアカウントのメールアドレスです)

$ ssh-keygen -t rsa -C mail@mail.com
Enter file in which to save the key (/home/vagrant/.ssh/id_rsa):
Enter passphrase (empty for no passphrase):
Enter same passphrase again:
※ssh-keygenコマンドについて
ssh-keygenは認証用の鍵を作成するコマンドです

ssh-keygenの後ろの、-t rsa というオプションは、RSA暗号というタイプの暗号の鍵を生成するという意味合いです。

※keygen実行後の３つの質問について。
ssh-kegen実行時に何かを聞かれるので、Enterを３回押せば先へ進めます。
※３つの質問の意味
１ 鍵ファイルを保存するフォルダはどこか (Enter file in which to save the key)
２パスフレーズを入力してください (Enter passphrase:)
３パスフレーズを再度入力してください (Enter same passphrase again:)
（※パスフレーズとはパスワードのようなものです。パスワードよりも長大な値を設定できます。 ）

#### ２：作成した鍵をコピーする

鍵ファイル id_rsa とid_rsaとid_rsa.pubができていることを確認  

$ ls ~/.ssh/
id_rsa id_rsa.pub
id_rsaが秘密鍵
id_rsa.pubが公開鍵
→この公開鍵をgithubに登録し直します。

id_rsa.pubを開く  

$ less ~/.ssh/id_rsa.pub
↓こんな文字列が書かれているかと思います。鍵の情報です。

ssh-rsa xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx mail@mail.com
id_rsa.pubの中の文字列 ssh-rsa〜xxxxxの終わりまでをコピーします。 （mail@mail.comは不要です）

#### ３：githubで鍵を登録する

ユーザー設定->SSH鍵

https://gitlab.com/-/profile/keys

キーの欄に，鍵の情報を張り付ける



## git

### clone

am@p5820
/work03/am
2021-06-25_20-07

```
$ git clone git@gitlab.com:infoaofd/lab.git
```

Cloning into 'lab'...
remote: Enumerating objects: 756, done.
remote: Counting objects: 100% (187/187), done.
remote: Compressing objects: 100% (144/144), done.
remote: Total 756 (delta 47), reused 149 (delta 32), pack-reused 569
Receiving objects: 100% (756/756), 6.02 MiB | 3.23 MiB/s, done.
Resolving deltas: 100% (281/281), done.

am@p5820
/work03/am/lab/JUPYTER
2021-06-25_20-09

```bash
$ cp -a ../../jupyter_files/2021-06-25_TEST/math_sample.ipynb .

`../../jupyter_files/2021-06-25_TEST/math_sample.ipynb' -> `./math_sample.ipynb'
```

### set-up

(base)
am@p5820
/work03/am/lab/JUPYTER
2021-06-25_20-14

```
$ git commit -m "a"
```

[master 1d1ed13] a
 Committer: am <am@p5820.bio.mie-u.ac.jp>
Your name and email address were configured automatically based
on your username and hostname. Please check that they are accurate.
You can suppress this message by setting them explicitly:

    git config --global user.name "Your Name"
    git config --global user.email you@example.com

After doing this, you may fix the identity used for this commit with:

    git commit --amend --reset-author

 1 file changed, 169 insertions(+)
 create mode 100644 JUPYTER/math_sample.ipynb

(base)
am@p5820
/work03/am/lab/JUPYTER
2021-06-25_20-14

```
$ git config --global user.name infoaofd
```

(base)
am@p5820
/work03/am/lab/JUPYTER
2021-06-25_20-15

```bash
$ git config --global user.email infoaofd@gmail.com
```

### add

(base)
am@p5820
/work03/am/lab/JUPYTER
2021-06-25_20-13

```
$ git add math_sample.ipynb
```

### commit

(base)
am@p5820
/work03/am/lab/JUPYTER
2021-06-25_20-16

```
$ git commit -m "a"
```

On branch master

Your branch is ahead of 'origin/master' by 1 commit.

(use "git push" to publish your local commits)
nothing to commit, working directory clean

### push

/work03/am/lab/JUPYTER
2021-06-25_20-26

```
$ git push
```

Counting objects: 7, done.
Delta compression using up to 8 threads.
Compressing objects: 100% (4/4), done.
Writing objects: 100% (4/4), 369 bytes | 0 bytes/s, done.
Total 4 (delta 2), reused 0 (delta 0)
To git@gitlab.com:infoaofd/lab.git
   dea0ef3..3736cad  master -> master



## 備考

Permission denied (publickey). fatal: Could not read from remote repository. Please make sure you have the correct access rights and the repository exists. が出た時の解決方法

https://tusukuru.hatenablog.com/entry/2018/08/29/021651

fatal: Could not read from remote repository. の落とし穴

https://hayato8810noblog.hatenablog.com/entry/2019/04/21/002950

SSH認証キーをGitLabに登録・設定手順 覚書

https://qiita.com/redamoon/items/07e445d1fce360cb5fa3

Git初心者のメモ

https://qiita.com/sahara/items/f6ab19fec79027379820

Git/gitlabで共同作業をするための最小限の知識

https://doss.eidos.ic.i.u-tokyo.ac.jp/html/git.html#topology

ユーザー設定 SSH 鍵

https://gitlab.com/-/profile/keys

