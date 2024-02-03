# fortranで配列の平均値計算を高速化
https://qiita.com/ykatsu111/items/f8d421b315e27c6a7588

## 普通の書き方（昔ながらの書き方）
```
real(4) :: data(N)
```
のように，N個のデータが入った配列の平均値を求めるには，昔ながらの書き方だと，
```
function mean(data) result(out)
    real(4), intent(in) :: data(:)
    real(4) :: out
    integer(4) :: i

    out = 0
    do i = 1, size(data)
        out = out + data(i)
    end do
    out = out / size(data)
end function mean
```
こんな物が真っ先に浮かぶだろう．
もしもdataに欠損値があるのなら，
```
function mean(data, undef) result(out)
real(4), intent(in) :: data(:), undef
real(4) :: out
integer(4) :: i, N

out = 0
N = 0
do i = 1, size(data)
if(data(i) /= undef) then
out = out + data(i)
N = N + 1
end if
end do
out = out / N
end function mean
```
こんな風に書ける．
しかし，これだと，いちいちdataのメモリにアクセスするためあまり高速ではない．
fortranプログラムの高速化は，いかに組み込み関数や配列演算を駆使するかというのが基本らしいので，ちょこっとメモ．

## 欠損値のなしのデータの場合
```
function mean(data) result(out)
real(4), intent(in) :: data(:)
real(4) :: out

out = sum(data) / size(data)
end program mean
```
これは，簡単ですね．

## 欠損値ありのデータの場合
```
function mean(data, undef) result(out)
    real(4), intent(in) :: data(:), undef
    real(4) :: out

out = sum( data, mask = data /= undef ) / count( mask = data /= undef )

end function mean
```