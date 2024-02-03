real,dimension(3)::x
x(1)=1; x(2)=2; x(3)=3

print *,'BEFORE LINE 5 x=1.0: x(1),x(2),x(3)=',x(1),x(2),x(3)

x=1.0
print *,'AFTER LINE 5 x=1.0: x(1),x(2),x(3)=',x(1),x(2),x(3)

do i=1,3
  x=x*x(i)
print *,'DO LOOP: i,x(1),x(2),x(3)=',i,x(1),x(2),x(3)
end do

end

