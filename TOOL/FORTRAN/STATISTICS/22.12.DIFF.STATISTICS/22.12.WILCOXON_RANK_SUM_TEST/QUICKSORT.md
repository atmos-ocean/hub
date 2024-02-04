---
title: 初めてのアルゴリズム vol.1
tags: Fortran90
author: B3LYP
slide: false
---
相も変わらずNo.182が解けないので少し試行錯誤

考えてたことをまとめるために色々書き出してみる

# 問題
No.182が解けない理由として、**効率的なソートができない** が一番の原因だと思う

問題の特徴として、

1. 数字が規則だったり、不規則だったりと安定していない
2. 愚直に数字の比較をしてもLTEやMLEしてしまう

大きく分けてこの二つが挙げられる

1.に対しての解決策として、ソートを利用するか、ハッシュテーブルを利用するのどちらか

ハッシュテーブルを利用すれば多分2.はそこまでややこしい処理をしなくても良い

が、ハッシュ関数って言うんですかね、そういう関数がFortranには用意されていない(ライブラリを除くと)

つまり、使いたければ自分で組む必要がある

一つ一つ整理する

# ソート
まずはソートの概念について

決められた規則、あるいは決めた規則を基に並び替えを行うこと、と認識している

そのため、ソートも立派なアルゴリズムとなる

結局は並べ方をどう工夫するか、という話

```bubble.f90
implicit none
integer(8) n,j,i,a
integer(8),allocatable :: x(:)
read(*,*) n
allocate (x(1:n))
x=0
a=0
read(*,*) (x(i),i=1,n)
do i=1,n-1
    do j=i+1,n !j=2,nではx(2)>x(2)やx(3)>x(2)の比較を行うことになる
        if (x(i) > x(j)) then
            a=x(i)
            x(i)=x(j)
            x(j)=a
        end if
    end do
end do

write(*,*) x
end
```
交換法と言われるらしい

一枚目を順に他のカードと比較して、小さい方と交換して残りを比較する。

全部と比較が終われば、一番小さいカードが分かるのでそれを先頭にして、

また次へと言う感じ

交換法というよりソート版総当たり法と言う方がしっくりくる

```quicksort.f90
!-------モジュール
module quicksort_function
    implicit none
   contains
    !再帰型副関数
    recursive function qsort(x) result(quick)
        integer(8), allocatable :: quick(:)
        integer(8), intent(in)  :: x(:)
        if (size(x) > 1) then
            quick = [qsort(pack(x(2:),x(2:)<x(1))),x(1),qsort(pack(x(2:),x(2:)>=x(1)))]
        else
            quick = x
        end if
    end function qsort
end module quicksort_function

!------主プログラム
program Sort
    use quicksort_function
    implicit none
    integer(8) m,n,i,a
    integer(8),allocatable :: x(:)
    integer(8),allocatable :: y(:)
    read(*,*) n
    allocate (x(1:n))
    allocate (y(1:n))
    read(*,*) (x(m),m=1,n)
    y=qsort(x)
    write(*,*) y
end program Sort
```
すごい速いソート

@cure_honey 氏のを~~丸パク~~げふんしました

[Fortran で QuickSort](https://qiita.com/cure_honey/items/29944920e8aff5651e41)

イマイチ中身を理解できていないので、解体する

```under.f90
if (size(x) > 1) then
      quick = [qsort(
                     pack(x(2:),x(2:)<x(1)) !---pack(配列,条件)で一次元配列に整形 配列:xのインデックスが2以上 条件:x(1)を下回る物　で配列に整形
                    ),
                     x(1),
               qsort(
                     pack(x(2:),x(2:)>=x(1))!---条件:x(1)を上回る物　で配列に整形
                    )
              ]
else
      quick = x
end if
```
文字で書いてもわけわかんないので、図に変換

![sort.jpg](https://qiita-image-store.s3.ap-northeast-1.amazonaws.com/0/293790/fdf8a672-bf5c-b255-1f5f-0e61e8874a2d.jpeg)

こういうことだな

x(1)が固定されるから、きれいにソートされるわけだ・・・

難点としてはある程度綺麗に揃っている配列に対してはあまりよろしくなさそう・・・

だから、x(1)を `mid=(n/2)`として全部x(mid)に書き換えれば良いのかな？

```mid.f90
mid=n/2
if (size(x) > 1) then
    quick = [qsort(pack(x(:mid),x(:mid)<x(mid))),x(mid),qsort(pack(x(mid:),x(mid:)>=x(mid)))]
else
    quick = x
end if
```
で、挿入ソートと言うにはお粗末なもどき

```inspro.f90
implicit none
integer(8) n,i,a
integer(8),allocatable :: x(:),Y(:)
integer(8),allocatable :: result(:)
read(*,*) n
allocate (x(1:n))
allocate (Y(1:n))
read(*,*) (x(i),i=1,n)
do i=1,n
    result = minloc(x(i:))
    a = result(1)
    Y(i) = x(a)
end do
write(*,*) Y
end
```
何がきついって `minloc()` の返り値が配列と言うこと

なんでやねん！

ヒープソートとかマージソートを考えたのものの、全然わかんなかった件について

```marge
1.二人組を作っていく
2.二人組にバブルソートを適用して並べ替える
3.結合する際に同じインデックスで比較を行う
4.小さい方を先に配列に入れれば自動的に整列される
```
という認識で良いのかな、マージソート

ヒープは全く理解できなかった件について

木と言うデータ構造がまず分かっていないので残当

# 実際にやってみる

```muripo.f90
!-------モジュール
module quicksort_function
    implicit none
   contains
    !再帰型副関数
    recursive function qsort(x) result(quick)
        integer(4), allocatable :: quick(:)
        integer(4), intent(in)  :: x(:)
        if (size(x) > 1) then
            quick = [qsort(pack(x(2:),x(2:)<x(1))),x(1),qsort(pack(x(2:),x(2:)>=x(1)))]
        else
            quick = x
        end if
    end function qsort
end module quicksort_function

!------主プログラム
program Sinki
    use quicksort_function
    implicit none
    integer(4) n,i,a
    integer(4),allocatable :: x(:)
    integer(4),allocatable :: y(:)
    read(*,*) n
    allocate (x(1:n))
    allocate (y(1:n))
    read(*,*) x(1:n)
    y=qsort(x)
    do i=1,n
        if (y(i-1)/=y(i).and.y(i)/=y(i+1)) then
            a=a+1
        end if
    end do
    write(*,*) a
end program Sinki
```
結果的にはMLEでダメだったよ

学んだこと

1.配列の削除はよりメモリを使うという事実
2.倍精度にしたら上手くソートできない

削除したらその分メモリが解放されてハピハピだと思ってました()

メモリーを大量に使う要因として`pack()`による配列の生成が山ほど行われるからかな、と予想

むむむ・・・

他にFortranで解いている人の解答を見るとその人もクイックソートを利用してた件について

そんなこんなでちょっとハッシュテーブルの利用も考えてみたんですが、

意味不明でした

配列に配列を入れ子にしている感じなんですかね

それなら配列で良くない？になるので多分そういうことじゃないと思う・・・

値が1000超えたら別の配列に・・・とかすればそれっぽくなるんでしょうか？

うーん、Pythonのディクショナリーでこの問題が解ければいいんだと思いますが、

解き方が不明という有様です

うーん、むりぽ

以上!

