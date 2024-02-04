! https://bellcurve.jp/statistics/course/26101.html
! https://qiita.com/kenkenvw/items/d7d5930bef3cc923c061
! 以下は、FortranでWilcoxon Rank Sum
! Testを実行するプログラムの基本的な例です。
! このプログラムは、2つの独立したサンプルからなるデータを入力として受け取り、
! それらのサンプルの中央値が等しいかどうかを検定します。
! Wilcoxon Rank Sum
! Testは、非対称なデータや外れ値にも強い統計的手法の一つです。

program WILCOXON_RANK_SUM
implicit none

! INPUT DATA
! https://waidai-csc.jp/updata/2019/05/ed6ffbab7b7dd8644913ee0a31e5b477.pdf
integer, parameter :: n1 = 34
integer, parameter :: n2 = 66
real(8), dimension(n1) :: sample1 = [22, 11, 13, 13, 21, 19, 0, 3,&
0, 16, 16, 18, 22, 27, 22, 23, 3, 18, 44, 45, 17, 27, 31, 25, 19, &
30, 16, 52, 376, 19, 27, 68, 18, 0]

real(8), dimension(n2) :: sample2 = [23, 15, 116, 87, 18, 95, 52, &
17, 87, 22, 67, 12, 14, 33, 238, 33, 18, 16, 32, 24, 103, 20, 25, &
21, 88, 55, 0, 29, 106, 34, 98, 48, 55, 0, 14, 22, 190, 32, 66,   &
35, 45, 19, 413, 16, 0, 33, 37, 13, 17, 102, 33, 25, 36, 12, 0,  &
57, 174, 40, 83, 21, 18, 8, 47, 31, 0, 155]

! 変数
real(8), dimension(n1+n2) :: combined_sample
integer:: rank(n1+n2), IFLAG(n1+n2)
real(8):: rank_avg(n1+n2)
real(8):: dnx,dny,U,avg_U,var_U,z
real(8) :: rank_sum1, rank_sum2, rank_sum
real(8) :: p_value
integer i,n,count
! コンバインドされたサンプルを作成
n=n1+n2
combined_sample(1:n1) = sample1
combined_sample(n1+1:n1+n2) = sample2

!do n=1,n1+n2
!print *,n,sngl(combined_sample(n))
!end do

! ランクを計算
call rankdata(combined_sample, rank_avg, rank, IFLAG)

print *
print '(A)',"MMMMM INPUT DATA AND RANK"
do i=1,n1+n2
print '(i3,2x,f5.0,f7.2)',i,sngl(combined_sample(i)),rank_avg(i)
end do

! ランク和を計算
rank_sum1 = SUM(rank_avg(1:n1))
rank_sum2 = SUM(rank_avg(n1+1:n1+n2))

if(n1<=n2)then
dnx=dble(n1); dny=dble(n2)
rank_sum=rank_sum1
else
dnx=dble(n2); dny=dble(n1)
rank_sum=rank_sum2
end if

U=(dnx*dny)+(dnx*(dnx+1.0)/2.d0)-(rank_sum)
avg_U=(dnx*dny/2.d0)
var_U=(dnx*dny)*(dnx+dny+1.d0)/12.d0
z=abs(U-avg_U)/sqrt(var_U)

! p値を計算
p_value = (1.d0-normcdf(z))*2.d0

print *
print '(A)',"MMMMM RESULT"
print '(A,f10.2)',"rank_sum=",rank_sum
print '(A,f10.2)',"dnx     =",dnx
print '(A,f10.2)',"dny     =",dny
print '(A,f10.2)',"U       =",U
print '(A,f10.2)',"avg_U   =",avg_U
print '(A,f10.2)',"var_U   =",var_U
print '(A,f10.2)',"z       =",z
print '(A,f10.4)',"p_value =",p_value

contains

! ランク付けのサブルーチン
subroutine rankdata(data,rank_avg,rank,IFLAG)
real(8), intent(inout) :: data(:),rank_avg(:)
integer, intent(inout) :: rank(:),IFLAG(:)
integer :: i, j, n, COUNT
REAL(8)::SUM
    
n = size(data)

do i = 1, n
  rank(i) = 1
  do j = 1, n
    if (data(j) < data(i) .or. (data(j) == data(i) .and. j < i)) then
      rank(i) = rank(i) + 1
    endif
  end do
end do

!同着の場合は平均とする
rank_avg=dble(rank)
IFLAG=0
do i = 1, n
  COUNT=0;SUM=0.0
  do j = 1, n
    if ( data(j) == data(i) ) then
      COUNT = COUNT + 1
      SUM=SUM+dble(rank(j))
      IFLAG(J)=1
    endif
  end do
  if(IFLAG(i)/=0)then
    rank_avg(i)=SUM/dble(COUNT)
  end if
end do

end subroutine rankdata



real(8) function normcdf(x)
    ! 標準正規分布の累積分布関数
    real(8), intent(in) :: x
    real(8) :: t, ans
    real(8), parameter :: b0 = 0.2316419, b1 = 0.319381530, &
    b2 = -0.356563782
    real(8), parameter :: b3 = 1.781477937, b4 = -1.821255978, &
    b5 = 1.330274429
    real(8),parameter::pi=3.141592653589793

    t = 1.0 / (1.0 + b0 * abs(x))
    ans = 1.0 - (b1 * t + b2 * t**2 + b3 * t**3 + b4 * t**4 + b5 *&
    t**5) * exp(-x**2 / 2.0) / sqrt(2.0 * pi)

   normcdf = ans
end function normcdf

end program WILCOXON_RANK_SUM
