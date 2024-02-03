real,dimension(3)::x
x(1)=1; x(2)=2; x(3)=3

x=1.0

do i=1,3
  x=x*x(i)
  print *,'i=',i
  print *,'x=',x
end do

end

