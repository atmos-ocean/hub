# samba-client／cifs-utilsを使ってWindowsの共有フォルダをマウントする

## 環境
|＃|環境|概要|
|:--|:--|:--|
|1|IP|192.168.33.10|
|2|Vagrant|2.0.3|
|3|Oracle VM VirtualBox|5.2.8|
|4|CentOS|6.9|

## Windowsに共有フォルダを作成する

Sambaを使ってWindowsの共有フォルダをマウントし、CentOS環境とWindows環境でファイルを共有する場合でも、
特にWindowsの共有フォルダ作成は、通常と変わりありません。

気を付ける点としては、

* 共有フォルダへのアクセス権設定用のWindowsアカウントを個別に作成する。
* 対象Windowsアカウントに共有フォルダへのアクセス権を設定する。
* 詳細な共有の設定により、対象Windowsアカウントに対して必要に応じてフルコントロールを与える。
* Windows標準のファイアウォールの設定で、  
セキュリティが強化されたWindowsDefenderファイアウォール＞受信の規則＞ファイルとプリンターの共有（SMB受信）  
の規則が有効化されていること。  
*※別のウィルス対策ソフトによってファイアウォールの設定がされている場合、samba-clientを使ってマウントする際に利用されるポート（標準：445）を開放するような設定が必要。*

## samba-client,cifs-utilsのインストール

```shell
yum install -y samba-client cifs-utils
```

## CentOS側にWindowsの共有フォルダをマウントした際の共有用のディレクトリを作成する

```shell
mkdir /usr/local/src/share
```

## mountコマンドを実行してWindowsの共有フォルダをマウント

ここでは以下の条件に対しマウントする

* Windows側環境
 * 共有フォルダは「//192.168.0.95/share」
 * アクセス許可をしているアカウント「share / sharepass」
* CentOS側環境
 * マウントする際のCentOS側のディレクトリ「/usr/local/src/share」

```shell
mount.cifs -o "username=share,password=sharepass" //192.168.0.95/share /usr/local/src/share
```

このコマンドの実行の結果、特にコンソールへ何も表示されなければマウント完了です。
※Windows7環境の共有フォルダで検証した際は、上記手順で問題なくマウントできましたが、
　Windows10環境の共有フォルダで検証すると、上記手順では正常にマウントできず、以下のエラーが発生しました。

```
mount error(112): Host is down
Refer to the mount.cifs(8) manual page (e.g. man mount.cifs)
```

後続の [Windows10環境へのマウントがうまくいかない件](#Windows10環境へのマウントがうまくいかない件) を参照

## マウントできているか確認

mount コマンドを実行することで、マウントの情報が確認できます。

```shell
mount
～～（省略）～～
//192.168.0.95/share on /usr/local/src/share type cifs (rw)
```

## CentOS側を再起動したあともマウントされるように/etc/fstabを修正

```shell
vim /etc/fstab
```

/etc/fstabの最終行に以下の行を追加する。

```
//192.168.0.95/share /usr/local/src/share cifs username=share,password=sharepass 0 2
```

この状態であれば、CentOS側を再起動してもファイル共有はされた状態となる。

## Windows10環境へのマウントがうまくいかない件

CentOS6.9環境でyum installした状態でmount.cifsのバージョンを確認すると以下の状態だった。

```shell
mount.cifs -V
mount.cifs version: 4.8.1
```

では、CentOS7.4環境ではどうなるか検証してみた。

```shell
mount.cifs -V
mount.cifs version: 6.2
```

だいぶバージョンが違うので、もしかしてと思い、Windows10環境へのマウントを試みたところ、正常にマウントできた。
ちなみに、マウントした時のコマンドは以下の通り。

```shell
mount.cifs -o "username=share,password=sharepass,vers=2.0" //192.168.0.170/share /usr/local/src/share
```

注意点として、他のサイトでも紹介されていた通り、**「vers=2.0」というオプションを付けないと、正常にマウントできない**ようです。

ちなみに「vers=3.0」というオプションにして実行してみたところ、これも正常にマウントできました。

なので、結論として、CentOS6.9の標準リポジトリを利用してyumでセットアップしたsamba-client及びcifs-utilsを利用すると、Windows10環境の共有フォルダはマウントできないということらしいです。

