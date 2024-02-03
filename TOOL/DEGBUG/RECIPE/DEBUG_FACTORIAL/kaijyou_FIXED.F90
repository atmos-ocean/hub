real,dimension(3)::x
x(1)=1; x(2)=2; x(3)=3

i=1;FACT=x(1)
print *,'i,x(i),FACT=',i,x(i),FACT

do i=2,3
  print *,'BEFORE i,x(i),FACT=',i,x(i),FACT
  FACT=FACT*x(i)
  print *,'AFTER i,x(i),FACT=',i,x(i),FACT
end do

print *
print *,'RESULT: FACT=',FACT

end

