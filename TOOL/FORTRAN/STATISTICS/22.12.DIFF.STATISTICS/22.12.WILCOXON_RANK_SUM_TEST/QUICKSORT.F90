program QS_SAMPLE
implicit none

! パラメータ
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
integer :: rank(n1+n2)
real(8) :: u_statistic
real(8) :: p_value
integer n, first,last

! コンバインドされたサンプルを作成
combined_sample(1:n1) = sample1
combined_sample(n1+1:n1+n2) = sample2

do n=1,n1+n2
print *,n,sngl(combined_sample(n))
end do
print *

first=1;last=n1+n2
call quicksort(combined_sample,first,last)

do n=1,n1+n2
print *,n,sngl(combined_sample(n))
end do

stop



contains
recursive subroutine quicksort(a, first, last)
! quicksort.f -*-f90-*-
! Author: t-nissie
! License: GPLv3
! Gist: https://gist.github.com/t-nissie/479f0f16966925fa29ea
!!
  implicit none
  real*8  a(*), x, t
  integer first, last
  integer i, j

  x = a( (first+last) / 2 )
  i = first
  j = last
  do
     do while (a(i) < x)
        i=i+1
     end do
     do while (x < a(j))
        j=j-1
     end do
     if (i >= j) exit
     t = a(i);  a(i) = a(j);  a(j) = t
     i=i+1
     j=j-1
  end do
  if (first < i-1) call quicksort(a, first, i-1)
  if (j+1 < last)  call quicksort(a, j+1, last)
end subroutine quicksort

end program QS_SAMPLE
