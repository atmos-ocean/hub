X = (/13.8, 10.2, 4.6, 10.0, 4.2, 16.1, 14.4, 4.9, 7.7, 11.4/)
Y = (/3.3, 2.6, 4.0, 4.7, 1.9, 2.9, 4.7, 5.3, 4.3, 3.0, 2.0/)
siglvl  = 0.05
aveX    = avg (X)         
aveY    = avg (Y)         
varX    = variance (X)    
varY    = variance (Y)    
sX      = dimsizes (X)     
sY      = dimsizes (Y)
                                                 ; Following not used; FYI only
diffXY  = aveX - aveY                            ; 4.378

iflag   = True                                   ; population variance similar
tval_opt= False                                  ; p-value only
prob = ttest(aveX,varX,sX, aveY,varY,sY, iflag, True) 

print("MMMMM WELCH T-TEST")
print("iflag="+iflag)
print("aveX = "+aveX+" aveY = "+aveY)
print("varX = "+varX+" varY = "+varY)
print("sX   = "+sX  +" sY   = "+sY)

print(prob)
print("")

if (prob(0,0).lt.siglvl) then
print("MMMMMM Difference is significant.")
end if
