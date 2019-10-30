      subroutine read_block_gps_flag(ios,fileID,inline,itime, debugging,
     .  current_hour, sec, msec, flag, numsat)
      implicit none
      include 'local.inc'
      integer i,fileID,ios,sec,msec,numsat,itime(5),flag,
     .  current_hour
      character*80 inline
      real*8 tod, tod_save
      logical bad_point,debugging

      read(inline(1:35),'(3X,6(1X,I2.2),X,I3,6X,I1,I3)')
     +     (itime(i), i=1,5), sec, msec, flag, numsat
      if (itime(4) .ne.current_hour) then
        current_hour = itime(4)
      endif
c     seconds in the day
      tod = itime(4)*3600.0 + 60.0*itime(5) + sec
      if (tod.lt.tod_save) then
        print*,'Time is going backwards or standing still'
        print*,'Ignoring this record'
        bad_point = .true.
        tod_save = tod
      else
        bad_point = .false.
        tod_save = tod
      endif
      if (debugging) then
        print*, 'reading block ' 
        print*, inline(1:60)
      endif
      return
      end subroutine read_block_gps_flag

      subroutine read_block_gps(fileID, flag,inline,numsat,nobs,satID,
     .  prn,obs,lli)
c     19mar01 KL
c     this was originally read_block_gnss.f
c     I transfered it over from the GNSS code - hopefully to save time
      implicit none
      include 'local.inc'
      character*1 char, satID(maxsat)
      integer fileID, i, itrack, flag, nobs, numsat, sec
      integer prn(maxsat), ios, nsat
      character*80 inline, dynfmt
      real*8  obs(maxob,maxsat) 
      integer lli(maxob,maxsat)
      logical debug
c     KL remove clockerr - was not using it - perhaps making blocks crash 
c     for certain compilers
c     KL - put lli and snr into integer arrays, previously misdefined
c     kl allow up to 15 observables now
c         and 36 satellites
c     kl 18oct16, allow up to 48 satellites now
      debug = .false.
!      write(0,*)numsat,flag
      if (flag.le.1 .or. flag.eq.6) then
c       print*, 'need to read extra lines'
c       19jan09 changed to allow up to 60 satellites
        if (debug) then
          print*, 'made it past here'
        endif
C       KL 19mar01 this should not be needed in a GPS only world
c       I need to rename the satellites now
c       do i =1, numsat
c         call newSat(satID(i), prn(i),nsat)
c         prn(i) = nsat
c         if (debug) then
c           print*, i, nsat
c         endif
c       enddo
c       change to allow up to 20 observable types
        if (flag .le. 1) then
          do itrack = 1, numsat
            if (debug) then
              print*, 'track', itrack
            endif
            write(dynfmt,'("(A,I2.2,",i2,"(F14.3, I1,1x))")') nobs
!            read(fileID, fmt='(A,I2.2,20(f14.3,I1,1x))', iostat=ios)
            read(fileID, fmt=dynfmt, iostat=ios)
     +         satID(itrack),prn(itrack),
     +         (obs(i,itrack),lli(i,itrack),i=1,nobs)
          enddo
        endif
      else
        do itrack = 1, numsat
          read(fileID, fmt='(A80)', iostat=ios) inline
        enddo
      endif
      return
      end subroutine read_block_gps
