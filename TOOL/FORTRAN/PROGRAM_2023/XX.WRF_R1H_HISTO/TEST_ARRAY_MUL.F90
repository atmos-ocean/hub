real,dimension(3)::a,b,c

data a/1,2,3/
data b/0.1,0.2,0.3/

print *,'a ',(a(i),i=1,3)
print *,'b ',(b(i),i=1,3)

c=a*b

print *,'c=a*b'
print *,'c ',(c(i),i=1,3)

end

