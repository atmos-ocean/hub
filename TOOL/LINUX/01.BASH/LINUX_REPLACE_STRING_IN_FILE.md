## sed ファイルに中に含まれる文字列の置換

Sed は `-i` (または `--in-place`) というオプションをつけることで、ファイルに中に含まれる文字列の置換ができる



```bash
$ cat sample.txt
STRINGS=AAA
$ sed -i -e 's/AAA/BBB/' sample.txt
$ cat sample.txt
STRINGS=BBB
```