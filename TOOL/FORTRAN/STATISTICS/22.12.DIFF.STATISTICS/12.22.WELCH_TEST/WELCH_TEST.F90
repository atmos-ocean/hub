subroutine welch_ttest(n1, x1, n2, x2, t, df, p)
implicit none
integer :: n1, n2
double precision :: x1(n1), x2(n2)
double precision :: m1, m2, v1, v2, t, df, p
double precision :: dbetai
    
m1 = sum(x1) / n1
m2 = sum(x2) / n2
v1 = sum((x1 - m1)**2) / (n1 - 1)
v2 = sum((x2 - m2)**2) / (n2 - 1)
t = (m1 - m2) / sqrt(v1 / n1 + v2 / n2)

df = (v1 / n1 + v2 / n2)**2 / &
     (v1**2 / (n1**2 * (n1 - 1)) + v2**2 / (n2**2 * (n2 - 1)))

PRINT '(2f10.3)',m1,m2
PRINT '(2f10.3)',v1,v2
PRINT '(f10.5,f10.2)',t,df

p = dbetai(df / (t**2 + df), 0.5d0 * df, 0.5d0)

end subroutine

program pvalue
implicit none
double precision :: x(10) = [13.8, 10.2, 4.6, 10.0, 4.2, 16.1, 14.4, 4.9, 7.7, 11.4]
double precision :: y(11) = [3.3, 2.6, 4.0, 4.7, 1.9, 2.9, 4.7, 5.3, 4.3, 3.0, 2.0]
!double precision :: x(4) = [3d0, 4d0, 1d0, 2.1d0]
!double precision :: y(3) = [490.2d0, 340.0d0, 433.9d0]

double precision :: t, df, p

call welch_ttest(10, x, 11, y, t, df, p)
!call welch_ttest(4, x, 3, y, t, df, p)

PRINT *
PRINT '(A,F10.2)','t =', t
PRINT '(A,F10.2)','df=', df
PRINT '(A,F10.7)','p= ', p
end program

! Rによるウェルチのt検定
!
!状況			適用すべきt検定
!データに対応がある．	対応のあるt検定
!データに対応がなく，2群間に等分散性が仮定できる．	スチューデントのt検定
!データに対応がなく，2群間に等分散性が仮定できない．	ウェルチのt検定
!
! p値の計算
! ウェルチのt検定を行うために以下のようなデータを考える．データXの値は平均値9，標準偏差3.5の正規分布に従う値で，データYの値は平均値3，標準偏差1.5の正規分布に従う値である．
!
! データX	13.8, 10.2, 4.6, 10.0, 4.2, 16.1, 14.4, 4.9, 7.7, 11.4
! データY	3.3, 2.6, 4.0, 4.7, 1.9, 2.9, 4.7, 5.3, 4.3, 3.0, 2.0
! これらのデータを以下のようなコマンドで，適当な変数，vx と vy に読み込む．
!
! $ vx=c(13.8, 10.2, 4.6, 10.0, 4.2, 16.1, 14.4, 4.9, 7.7, 11.4)
! $ vy=c(3.3, 2.6, 4.0, 4.7, 1.9, 2.9, 4.7, 5.3, 4.3, 3.0, 2.0)
! これらのデータのサンプルサイズは一致しないので，少なくとも対応はない．
! 有意水準を0.05と決める．
! 
! var.equal=Fで不等分散を仮定するので，Welchのテストとなる。
! $ t.test(x=vx,y=vy,var.equal=F,paired=F)
! これを実行した結果は以下のようになる．1行目の Welch Two 
! Sample t-test とう表記がウェルチのt検定を実行したことを
! 示している．
! 結果から，p<0.05 なので帰無仮説が棄却され，これらの2群間には
! 差があると結論する．
!
!
!        Welch Two Sample t-test
!
! data:  vx and vy
! t = 4.4264, df = 10.174, p-value = 0.001229
! alternative hypothesis: true difference in means is not equal to 0
! 95 percent confidence interval:
! 3.092228 9.331408
! sample estimates:
! mean of x mean of y 
! 9.730000  3.518182 
!
! $ WELCH_TEST.sh 
! `WELCH_TEST.EXE' を削除しました
!      9.730     3.518
!     18.487     1.328
!    4.42644     10.17
!    0.34179
!  DGAMMA.F 85: D1MACH(3)=  1.110223024625157E-016
!  
! t =      4.43
! df=     10.17
! p=  0.0012286
