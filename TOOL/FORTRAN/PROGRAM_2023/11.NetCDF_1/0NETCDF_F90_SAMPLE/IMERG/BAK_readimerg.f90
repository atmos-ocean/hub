program read_TRMM
! IMERGのデータを読むプログラム
! Lib -lnetcdff -lnetcdf
! テストしたデータのファイル名は
! 3B-HHR.MS.MRG.3IMERG.20210114-S023000-E025959.0150.V06B.HDF5
     use netcdf
     
     integer,parameter :: tim=181,tjm=133, tm=434,dm=8
     real,allocatable :: var3d(:,:,:),var2d(:,:)
     
     real :: lon(tim),lat(tjm)
       
     real,allocatable :: rain1(:,:),rain2(:,:),raintmp(:,:)
     real,allocatable :: avrain(:,:,:)
     real,allocatable :: arain(:,:),dayrain(:,:)
     integer datenum,jrec,irec,h,t,i,j
     integer ncid, varid, status,d, grp_ncid
     real istart,cvar
     
     real c(tim,tjm),ca
     character(len=3)  :: chour
     character(len=200) :: flname(tm),ifile
     
    
     
     allocate (rain1(tim,tjm),rain2(tim,tjm))
     allocate (avrain(tim,tjm,240))
     allocate (arain(tm,2),dayrain(dm,2))
     allocate (raintmp(tjm,tim))


   
     open(2,file='IMERG_0213.txt')
     !read(2,*)
     do t=1,tm            ! TRMM t=1 23:00Z = 23:00-23:59Z ref:https://sharaku.eorc.jaxa.jp/TRMM/faq/TRMM_faq05.html
     read(2,*) flname(t)  ! WRF t=1 0:00z =  23:00 - 0:00Z 
                          ! read 1 hour earlier data
     enddo
     close(2)
    
     ! IMERG has group in NETCDF-4 format 

     ifile='/Volumes/home2/IMERG/2021_halfhourly/'//trim(flname(1))
     status = nf90_open(ifile, nf90_nowrite, ncid)
     status = nf90_inq_grp_ncid(ncid, "Grid", grp_ncid)
     !print *,status,grp_ncid
     status = nf90_inq_varid(grp_ncid, "lon", varid)
    ! print *,status,varid
     status = nf90_get_var(grp_ncid, varid, lon, start=(/2800 /), count=(/tim/))
   !  print *, lon
     status = nf90_inq_varid(grp_ncid, "lat", varid)
     status = nf90_get_var(grp_ncid, varid, lat, start=(/775 /), count=(/tjm/))
     status = nf90_close(ncid)


     !print *,status
  !   print *,lon
    ! call sleep(100)
     avrain(:,:,:)=0.0
     d=1
     do t=1,tm-1,2
    
     ifile='/Volumes/home2/IMERG/2021_halfhourly/'//trim(flname(t))
     
     print *,ifile
     status = nf90_open(ifile, nf90_nowrite, ncid)
     status = nf90_inq_grp_ncid(ncid, "Grid", grp_ncid)
     status = nf90_inq_varid(grp_ncid, "precipitationCal", varid)

     status = nf90_get_var(grp_ncid, varid, raintmp, start=(/775,2800/), count=(/  tjm, tim/))
 
     do i=1,tim 
      do j=1,tjm
      rain1(i,j)=raintmp(j,i)*0.5 ! to mm/ 30min
   !   print *,rain(i,j)
      enddo
     enddo
     status = nf90_close(ncid)

     ifile='/Volumes/home2/IMERG/2021_halfhourly/'//trim(flname(t+1))
     
     print *,ifile
     status = nf90_open(ifile, nf90_nowrite, ncid)
     status = nf90_inq_grp_ncid(ncid, "Grid", grp_ncid)
     status = nf90_inq_varid(grp_ncid, "precipitationCal", varid)
     !print *,varid
     status = nf90_get_var(grp_ncid, varid, raintmp, start=(/775,2800/), count=(/  tjm, tim/))
    ! print *,status
    ! call sleep(100)
     do i=1,tim 
      do j=1,tjm
      rain2(i,j)=raintmp(j,i)*0.5 ! to mm/ 30min
   !   print *,rain(i,j)
      enddo
     enddo
     status = nf90_close(ncid)



    ! print *,raintmp
     do i=1,tim 
      do j=1,tjm
      avrain(i,j,d)=rain1(i,j)+rain2(i,j)
   !   print *,rain(i,j)
      enddo
     enddo

     write(chour,'(i3.3)') d !t=00z in GSMaP and IMERG means 00z-00z:59min   = t=01z in WRF
     open(2,file='0213/imerg_'//trim(chour)//'.asc')
     do i=1,tim 
       do j=1,tjm
         write(2,'(2f9.3,f10.3)') lon(i),lat(j),avrain(i,j,d)
       enddo
     enddo
     close(2)
  

     d=d+1


     enddo !t
     
   

     open(2,file='0213/total_imerg.asc')
     do i=1,tim 
       do j=1,tjm
         write(2,'(2f9.3,f11.3)') lon(i),lat(j),sum(avrain(i,j,:))/real(tm)
       enddo
     enddo
     close(2)
  
   



end program



   
