      subroutine read_block_gps_flag(fileID, itime, debugging,
     .  current_hour, sec, msec, flag, numsat)
      implicit none
      include 'local.inc'
      integer i,fileID,ios,sec,msec,numsat,itime(5),flag,current_hour
      character*80 inline
      real*8 tod, tod_save
      logical bad_point,debugging

      inline = ' '
      read(fileID,'(A80)', iostat=ios) inline
      if (ios.ne.0)then 
        return 99
      endif
      read(inline(1:32),'(5I3,X,I2,X,I3,4X,2I3)')
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
      character*80 inline, dynfmt, dynfmt2, dynfmt3,dynfmt4
      character*80 dynfmt5, anotherline
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
      if (flag.le.1 .or. flag.eq.6) then
        read(inline(33:80),'(12(A1,I2))')
     +         (char, prn(i),i=1,12)
        anotherline = inline
        read(inline(33:80),'(12(A1,2x))') (satID(i),i=1,12) 
c       print*, 'need to read extra lines'
c       19jan09 changed to allow up to 60 satellites
        if (numsat > 12 .and. numsat < 25) then
          read(fileID,'(A80)', iostat=ios) inline
          read(inline(33:80),'(12(A1,I2))') (char, prn(i),i=13,numsat) 
          read(inline(33:80),'(12(A1,2x))') (satID(i),i=13,numsat)
        elseif (numsat > 24 .and. numsat <= 36) then
          read(fileID,'(A80)', iostat=ios) inline
          read(inline(33:80),'(12(A1,I2))') (char, prn(i),i=13,24)
          read(inline(33:80),'(12(A1,2x))') (satID(i),i=13,24)
          read(fileID,'(A80)', iostat=ios) inline
          read(inline(33:80),'(12(A1,I2))') (char, prn(i),i=25,numsat) 
          read(inline(33:80),'(12(A1,2x))') (satID(i),i=25,numsat)
        elseif (numsat > 36 .and. numsat <= 48) then
          read(fileID,'(A80)', iostat=ios) inline
          read(inline(33:80),'(12(A1,I2))') (char, prn(i),i=13,24)
          read(inline(33:80),'(12(A1,2x))') (satID(i),i=13,24)

          read(fileID,'(A80)', iostat=ios) inline
          read(inline(33:80),'(12(A1,I2))') (char, prn(i),i=25,36) 
          read(inline(33:80),'(12(A1,2x))') (satID(i),i=25,36)

          read(fileID,'(A80)', iostat=ios) inline
          read(inline(33:80),'(12(A1,I2))') (char, prn(i),i=37,numsat) 
          read(inline(33:80),'(12(A1,2x))') (satID(i),i=37,numsat)
        elseif (numsat > 48 .and. numsat <= 60) then
          read(fileID,'(A80)', iostat=ios) inline
          read(inline(33:80),'(12(A1,I2))') (char, prn(i),i=13,24)
          read(inline(33:80),'(12(A1,2x))') (satID(i),i=13,24)

          read(fileID,'(A80)', iostat=ios) inline
          read(inline(33:80),'(12(A1,I2))') (char, prn(i),i=25,36) 
          read(inline(33:80),'(12(A1,2x))') (satID(i),i=25,36)

          read(fileID,'(A80)', iostat=ios) inline
          read(inline(33:80),'(12(A1,I2))') (char, prn(i),i=37,48) 
          read(inline(33:80),'(12(A1,2x))') (satID(i),i=37,48)

          read(fileID,'(A80)', iostat=ios) inline
          read(inline(33:80),'(12(A1,I2))') (char, prn(i),i=49,numsat) 
          read(inline(33:80),'(12(A1,2x))') (satID(i),i=49,numsat)
        endif
        if (debug) then
c         print*, 'made it past here'
        endif
        if (numsat > 60) then
          print*, 'I cannot read more than 60 satellites'
          print*, 'Please stop launching them!'
          call exit
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
c           for 6-10 observables
            if (nobs.gt.5 .and. nobs .le. 10) then
              write(dynfmt, fmt='(A, I3.3, A)')
     +           "(", 5, "(F14.3, I1,1x))"
              write(dynfmt2, fmt='(A, I3.3, A)')
     +           "(" , nobs-5, "(F14.3, I1,1x))"
              read(fileID, fmt=dynfmt, iostat=ios)
     +           (obs(i,itrack), lli(i,itrack), i=1,5)
              read(fileID, fmt=dynfmt2, iostat=ios)
     +             (obs(i,itrack),lli(i,itrack), i=6,nobs)
c    for more than 10 -15 observables 
            elseif (nobs.gt.10.and.nobs.le.15) then
              write(dynfmt, fmt='(A, I3.3, A)')
     +           "(", 5, "(F14.3, I1,1x))"
              write(dynfmt2, fmt='(A, I3.3, A)')
     +           "(", 5, "(F14.3, I1,1x))"
              write(dynfmt3, fmt='(A, I3.3, A)')
     +           "(" , nobs-10, "(F14.3,I1,1x))"

              read(fileID, fmt=dynfmt, iostat=ios)
     +           (obs(i,itrack),lli(i,itrack),i=1,5)
              read(fileID, fmt=dynfmt2, iostat=ios)
     +           (obs(i,itrack),lli(i,itrack),i=6,10)
              read(fileID, fmt=dynfmt3, iostat=ios)
     +           (obs(i,itrack),lli(i,itrack),i=11,nobs)
c     trying to add 15-20 observables
            elseif (nobs.gt.15.and.nobs.le.20) then
              write(dynfmt, fmt='(A, I3.3, A)')
     +           "(", 5, "(F14.3, I1,1x))"
              write(dynfmt2, fmt='(A, I3.3, A)')
     +           "(", 5, "(F14.3, I1,1x))"
              write(dynfmt3, fmt='(A, I3.3, A)')
     +           "(", 5, "(F14.3, I1,1x))"
              write(dynfmt4, fmt='(A, I3.3, A)')
     +           "(" , nobs-15, "(F14.3,I1,1x))"
              read(fileID, fmt=dynfmt, iostat=ios)
     +           (obs(i,itrack),lli(i,itrack),i=1,5)
              read(fileID, fmt=dynfmt2, iostat=ios)
     +           (obs(i,itrack),lli(i,itrack),i=6,10)
              read(fileID, fmt=dynfmt3, iostat=ios)
     +           (obs(i,itrack),lli(i,itrack),i=11,15)
              read(fileID, fmt=dynfmt4, iostat=ios)
     +           (obs(i,itrack),lli(i,itrack),i=16,nobs)
c           5 or fewer observable types
            else
              write(dynfmt, fmt='(A, I3.3, A)')
     +           "(", nobs, "(F14.3, I1,1x))"
              read(fileID, fmt=dynfmt, iostat=ios)
     +           (obs(i,itrack),lli(i,itrack),i=1,nobs)
            endif
          enddo
        endif
      else
        do itrack = 1, numsat
          read(fileID, fmt='(A80)', iostat=ios) inline
        enddo
      endif
      return
      end subroutine read_block_gps
