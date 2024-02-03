# Jupyter Book

## 概要

Jupyterを使って，電子書籍 (htmlやpdf)を作成する

## 情報

https://jupyterbook.org/en/stable/start/overview.html  

https://qiita.com/magolors/items/620860558661b527f267

## インストール

```
/work03/am
2022-08-07_16-59
$ conda install -c conda-forge jupyter-book
```

```bash
$ vi $HOME/.bashrc
```

```bash
$ grep .local $HOME/.bashrc
export PATH=/work03/$(whoami)/.local/bin:${PATH}:.
```

```bash
$ source $HOME/.bashrc
```



## テスト

/work03/am/00.JB/2022-08-07_TEST_JB
2022-08-07_17-19

```
$ cdac
```

```bash
$ jb create test_book
```

```bash
$ ls
0.README.TXT  test_book/
```

```bash
$ cd test_book/
```

```bash
$ jb build .

Finished generating HTML for book.
Your book's HTML pages are here:
    _build/html/
You can look at your book by opening this file in a browser:
    _build/html/index.html
Or paste this line directly into your browser bar:
    file:///work03/am/00.JB/2022-08-07_TEST_JB/test_book/_build/html/index.html   
```

```bash
/work03/am/00.JB/2022-08-07_TEST_JB/test_book
2022-08-07_17-22
$ ls _build/html/
_images/                index.html               objects.inv
_sources/               intro.html               search.html
_sphinx_design_static/  markdown-notebooks.html  searchindex.js
_static/                markdown.html
genindex.html           notebooks.html
```



## リモートサーバー上のファイルをブラウザで閲覧

https://www-he.scphys.kyoto-u.ac.jp/member/n-kota/dokuwiki/doku.php?id=ja:network:file_browsing



1. リモートサーバー上に仮想サーバーを設立
2. クライアント側でポートフォワードして、クライアントのポート(YYYY)を、仮想サーバーで指定したポート(XXXX)に対してつなげる。
3. ローカルのブラウザでlocalhost:YYYY に接続する

1の仮想サーバー設立は、pythonでできる。 ターミナルで、

```bash
### python2 系なら
$ python -m SimpleHTTPServer XXXXX
```

```bash
### python3 系なら
$ python -m http.server XXXXX
```



### ポートフォワード

WINDOWS アクセサリ→コマンドプロンプト

```bash
C:\Users\boofo>ssh -L YYYY:localhost:YYYY -Y xx@XXX.XX.XX.XXX
```

xx@XXX.XX.XX.XXX's password:



3.ローカルのブラウザでlocalhost:YYYY に接続

```bash
http://localhost:YYYY/
```

トラブルシューティング
$ python3 -m http.server YYYY
OSError: [Errno 98] Address already in use

```bash
$ lsof -i :8888
$ kill -KILL 61395
$ python3 -m http.server 8888
```



備考

2のポートフォワードの設定

ローカルのPC側の./ssh/config で

```bash
HOST hoge
     LocalForward YYYYY aaa.bbb.ccc.ddd:XXXXX
```

 aaa.bbb.ccc.dddは、hogeとして接続する先からみたリモートサーバーのIP or hostname.   

もしリモートサーバー=hogeなら、localhost 

上のようにして 

```bash
$ ssh hoge
```

とすれば毎回いちいち `$ ssh -L ...` のようにしなくて済む。



## PDF作成

/work03/am/00.JB/2022-08-07_TEST_JB/test_book
2022-08-07_17-55

```bash
$ jb build . --builder pdfhtml

Finished generating PDF via HTML for book. Your PDF is here:
    _build/pdf/book.pdf   
```



#### 数式のレンダリングの設定

By default MathJax version 2 is currently used. If you are using a lot of math, you may want to try using version 3, which claims to improve load speeds by 60 - 80%:

**_config.yml**

```
sphinx:
  config:
    mathjax_path: https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js
```

See the [Sphinx documentation](https://www.sphinx-doc.org/en/master/usage/extensions/math.html#module-sphinx.ext.mathjax) for details.
