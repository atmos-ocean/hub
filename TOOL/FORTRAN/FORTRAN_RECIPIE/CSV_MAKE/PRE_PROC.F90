
character infle*1000, ofle*1000
character header*5000, outline*5000, number*13

integer,parameter::ni=307, nj=47

real DATA(ni,nj), y(nj), x(ni-1,nj)

infle="BURI_SPARSE_DATA.csv"
ofle="BURI_SPARSE_DATA_2015.csv"

open(11,file=infle,action="read")
read(11,'(a)')header
do j=1,nj
read(11,*)(DATA(i,j),i=1,ni)
end do !i
close(11)


open(21,file=ofle)
write(21,'(a)')trim(header)
do j=1,nj

outline=""
do i=1,ni-1

is=(i-1)*13+1; ie=is+13
write(outline(is:ie),'(f12.9,a)')DATA(i,j),','

end do !i

is=(i-1)*13+1; ie=is+13
write(outline(is:ie),'(f12.9)')DATA(ni,j)

!print *,trim(outline)
write(21,'(a)')trim(outline)

end do !j

close(21)

print *
print *,'INPUT:  ',trim(infle)
print *,'OUTPUT: ',trim(ofle)
print *

stop
end

