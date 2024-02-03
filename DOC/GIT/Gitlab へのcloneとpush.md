# Gitlab へのcloneとpush

https://qiita.com/Taylor-U/items/0aba74d498281ffb0a7b

https://doss.eidos.ic.i.u-tokyo.ac.jp/html/git.html

https://atmarkit.itmedia.co.jp/ait/articles/2005/21/news023.html

**暗号鍵を作成**　※コマンドを実行するといくつかy/nが聞かれるが、今回はすべてに`Enter`を押す。

$ ssh-keygen -t rsa -f id_rsa_gitlab    #id_rsa_gitlabをファイル名として暗号鍵を作成



am@p5820
/work03/am
2021-10-14_20-56
$ mv id* .ssh/
`id_rsa_gitlab' -> `.ssh/id_rsa_gitlab'
`id_rsa_gitlab.pub' -> `.ssh/id_rsa_gitlab.pub'
(base)



am@p5820
/work03/am
2021-10-14_20-57
$ cat ~/.ssh/id_rsa_gitlab.pub



### 2. Gitlabに作成した公開鍵を登録する

1. [https://gitlab.com](https://gitlab.com/) にログインし、画面右上のアカウントアイコンから「setting」を開く

   https://gitlab.com/-/profile/preferences

   

2. 画面左のサイドメニューから「SSH Keys」を開く

3. `key`欄に先ほど控えておいた公開鍵の内容をペーストする

4. `Add key`をクリックし鍵を登録する



### 3. ローカルマシンにリポジトリをクローンする

お待ちかねのリポジトリのクローン手順だ。
まずはGitlab上で操作する。

1. Gitlabトップページの画面左上の「projects」からクローンしたいリポジトリを開く
2. 画面右上の「clone」をクリックし「Clone with HTTPS」欄の内容をクリップボードにコピーする
3. ターミナルから以下コマンドを実行しリポジトリをクローンする
   ※下記のコマンドのhttps://から.gitまではクリップボードの内容をそのまま貼り付ける、

am@p5820
/work03/am
2021-10-14_21-04

```
$ mv lab lab_BAK_$(myymdh)
```

`lab' -> `lab_BAK_2021-10-14_21'

(base)
am@p5820
/work03/am
2021-10-14_21-05
$ git clone git@gitlab.com:infoaofd/lab.git
Cloning into 'lab'...
remote: Enumerating objects: 1341, done.
remote: Counting objects: 100% (428/428), done.
remote: Compressing objects: 100% (230/230), done.
remote: Total 1341 (delta 197), reused 378 (delta 162), pack-reused 913
Receiving objects: 100% (1341/1341), 37.85 MiB | 9.72 MiB/s, done.
Resolving deltas: 100% (564/564), done.



am@p5820
/work03/am/lab/JUPYTER
2021-10-14_21-11
$ ls

```
2021.KANKYOU_KAISEKI/  DANA01/  math_sample.ipynb
```

(base)
am@p5820
/work03/am/lab/JUPYTER
2021-10-14_21-11

```
$ git add 2021.KANKYOU_KAISEKI/
```



am@p5820
/work03/am/lab/JUPYTER
2021-10-14_21-15

```
$ git commit -m "JUPYTER"

```

[master 04adf52] JUPYTER
 21 files changed, 3442 insertions(+)
 create mode 100644 JUPYTER/2021.KANKYOU_KAISEKI/2021-10-11_STANDARD_DEVIATION/.ipynb_checkpoints/2021-10-11_STANDARD_DEVIATION-checkpoint.ipynb
 create mode 100644 JUPYTER/2021.KANKYOU_KAISEKI/2021-10-11_STANDARD_DEVIATION/2021-10-11_STANDARD_DEVIATION.ipynb
 create mode 100644 JUPYTER/2021.KANKYOU_KAISEKI/2021-10-14_CENTRAL_LIMIT_THEOREM/.ipynb_checkpoints/2021-10-14_CENTRAL_LIMIT_THEOREM-checkpoint.ipynb
 create mode 100644 JUPYTER/2021.KANKYOU_KAISEKI/2021-10-14_CENTRAL_LIMIT_THEOREM/2021-10-14_CENTRAL_LIMIT_THEOREM.ipynb
 create mode 100644 JUPYTER/2021.KANKYOU_KAISEKI/2021-10-14_MIE_TOKYO_SD/.ipynb_checkpoints/2021-10-14_MIE_TOKYO_SD-checkpoint.ipynb
 create mode 100644 JUPYTER/2021.KANKYOU_KAISEKI/2021-10-14_MIE_TOKYO_SD/2021-10-14_MIE_TOKYO_SD.ipynb
 create mode 100644 JUPYTER/2021.KANKYOU_KAISEKI/2021-10-14_MIE_TOKYO_SD/HST_TOKYO_NOM_ANO.png
 create mode 100644 JUPYTER/2021.KANKYOU_KAISEKI/2021-10-14_MIE_TOKYO_SD/HST_TOKYO_RAW.png
 create mode 100644 JUPYTER/2021.KANKYOU_KAISEKI/2021-10-14_MIE_TOKYO_SD/HST_TSU_NOM_ANO.png
 create mode 100644 JUPYTER/2021.KANKYOU_KAISEKI/2021-10-14_MIE_TOKYO_SD/HST_TSU_RAW.png
 create mode 100644 JUPYTER/2021.KANKYOU_KAISEKI/2021-10-14_MIE_TOKYO_SD/TSR_TSU_ANO.png
 create mode 100644 JUPYTER/2021.KANKYOU_KAISEKI/2021-10-14_MIE_TOKYO_SD/TSR_TSU_NOM_ANO.png
 create mode 100644 JUPYTER/2021.KANKYOU_KAISEKI/2021-10-14_MIE_TOKYO_SD/TSR_TSU_RAW.png
 create mode 100644 JUPYTER/2021.KANKYOU_KAISEKI/2021-10-14_MIE_TOKYO_SD/TSR_TSU_SQ.png
 create mode 100644 JUPYTER/2021.KANKYOU_KAISEKI/2021-10-14_MIE_TOKYO_SD/TSR_TSU_TOKYO_ANO.png
 create mode 100644 JUPYTER/2021.KANKYOU_KAISEKI/2021-10-14_MIE_TOKYO_SD/TSR_TSU_TOKYO_NOM_ANO.png
 create mode 100644 JUPYTER/2021.KANKYOU_KAISEKI/2021-10-14_MIE_TOKYO_SD/TSR_TSU_TOKYO_RAW.png
 create mode 100644 JUPYTER/2021.KANKYOU_KAISEKI/2021-10-14_MIE_TOKYO_SD/T_TSU_TOKYO_JAN_5TH_PENTAD.csv
 create mode 100644 JUPYTER/2021.KANKYOU_KAISEKI/2021-10-14_MIE_TOKYO_SD/T_TSU_TOKYO_JAN_5TH_PENTAD_ORG.csv
 create mode 100644 JUPYTER/2021.KANKYOU_KAISEKI/2021-10-14_NORMALIZATION_SAMPLE/.ipynb_checkpoints/2021-10-14_NORMALIZATION_SAMPLE-checkpoint.ipynb
 create mode 100644 JUPYTER/2021.KANKYOU_KAISEKI/2021-10-14_NORMALIZATION_SAMPLE/2021-10-14_NORMALIZATION_SAMPLE.ipynb
(base)
am@p5820
/work03/am/lab/JUPYTER
2021-10-14_21-15

```
$ git push
```

warning: push.default is unset; its implicit value is changing in
Git 2.0 from 'matching' to 'simple'. To squelch this message
and maintain the current behavior after the default changes, use:

  git config --global push.default matching

To squelch this message and adopt the new behavior now, use:

  git config --global push.default simple

See 'git help config' and search for 'push.default' for further information.
(the 'simple' mode was introduced in Git 1.7.11. Use the similar mode
'current' instead of 'simple' if you sometimes use older versions of Git)

Counting objects: 31, done.
Delta compression using up to 8 threads.
Compressing objects: 100% (29/29), done.
Writing objects: 100% (29/29), 1.49 MiB | 0 bytes/s, done.
Total 29 (delta 1), reused 0 (delta 0)
To git@gitlab.com:infoaofd/lab.git
   5bb64bc..04adf52  master -> master