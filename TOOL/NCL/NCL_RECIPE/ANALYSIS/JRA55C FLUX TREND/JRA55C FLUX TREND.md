# JRA55C FLUX TREND

## プログラム(スクリプト)の所在

/work03/am/2022.SST.TREND.ENERGY.BUDGET/22.22.JRA55C.COARSE.MON/22.12.REGRID.NCL



## やりたいこと

JRA55Cのデータを使って, 海面熱フラックスの長期変化傾向（トレンド）を計算する

計算したトレンドのデータを用いて海面水温の長期変化傾向の要因を計算する



## 使用するソフト・言語

bashスクリプト

NCL (NCAR Command language)

Python (version 3)



## 全体の構成

22.12.REGRID.NCL: 格子幅をそろえる

22.22.SELMON.NC: 各月ごとにデータをまとめる

32.12.MON.TREND.NC: フラックスのトレンドを計算する

42.22.ENEGRY.BUDGET_CUI: 海面水温の長期変化傾向の要因を計算する

（Lesins, 2012の論文の方法で計算）



[22.12.REGRID.NCL](./22.12.REGRID.NCL)