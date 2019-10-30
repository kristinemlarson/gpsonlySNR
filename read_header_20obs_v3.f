      subroutine read_header_20obs(fileID,rawf,xrec,yrec,zrec,
     .  iobs,nobs,iymd,station)
c     new version (19mar01) taken from my GNSS code
c     kristine larson
c     allows 20 observables
      implicit none
      include 'local.inc'
      integer  i, fileID
      character*80 rawf
      character*80 line, outline, dynfmt, dynfmt2
      logical  endofheader
      integer nobs,iobs(maxsat), iymd(3), ios
      character*3 key(maxsat)
      character*4 station
      real*8 xrec, yrec, zrec
c     returns receiver coordinates
c     station name is empty to start with
c     returned to main code
      station = '    '
      endofheader = .false.

      open(fileID,file=rawf, status='old',iostat=ios)
      if (ios.ne.0) then
        print*, 'problem opening RINEX file'
        print*, 'name:', rawf
        call exit(0)
      endif
      do while (.not.endofheader)
c     KL 18mar05, fixed bug on nobs
        read (fileID,'(a80)') line
        dynfmt='(6X,13(1X,A3))'
        if (line(1:1)=='G'.and.line(61:80).eq.'SYS / # / OBS TYPES')then
!     GPS only
          read(line(4:6), fmt='(I6)') nobs
c   KL 19jan09 allowing more lines of OBS types
c         first line has up to 9 OBS types
          if (nobs .lt. 14) then
            read(line, fmt=dynfmt) (key(i), i=1,nobs)
c         between 14-26 OBS types
          elseif (nobs.ge.14.and.nobs.le.26) then
            read(line, fmt=dynfmt) (key(i), i=1,13)
c           read the next line
            read (fileID,'(a80)') line
            write(dynfmt(5:6), fmt='(i2)') nobs-13
            read(line, fmt=dynfmt) (key(i), i=14,nobs)
c           this is more than 26 OBS types
          else
c           first line
            read(line, fmt=dynfmt) (key(i), i=1,13)

c           read the next line
            read (fileID,'(a80)') line
c           reassign this second line
            read(line, fmt=dynfmt) (key(i), i=14,26)

c           read the third line
            read (fileID,'(a80)') line
            write(dynfmt(5:6), fmt='(i2)') nobs-13
            read(line, fmt=dynfmt) (key(i), i=27,nobs)

          endif
          print*, 'NUMBER OF OBSERVABLES ', nobs
        else if (line(61:80).eq.'APPROX POSITION XYZ') then
          read(line, fmt= '(3f14.4)') xrec, yrec, zrec
           print*, 'XYZ coordinates ', xrec, yrec, zrec
          if (xrec.eq.0) then
            print*, 'I cannot compute satellite elevation angles'
            print*, 'without apriori receiver coordinates - exiting'
            call exit
          endif
        else if (line(61:77).eq.'TIME OF FIRST OBS') then
          read(line, fmt= '(3i6)') iymd(1), iymd(2), iymd(3)
          print*, 'Time of first Obs: ', iymd
        else if (line(61:71).eq.'MARKER NAME') then
          read(line(1:4), fmt= '(a4)')  station
          print*, 'Station name ', station
        endif
        if (line(61:73).eq.'END OF HEADER'.or.
     +       line(61:73).eq.'end of header'.or.
     +       line(61:73).eq.' ') endofheader = .true.
      enddo
      print*, 'FOUND END OF HEADER'
      do i = 1,maxsat
          iobs(i) = 0
      enddo
      do i = 1, nobs
          if (key(i).eq.'l1c' .or. key(i).eq.'L1C') iobs(1) = i
          if (key(i).eq.'l2p' .or. key(i).eq.'L2P') iobs(2) = i
          if (key(i).eq.'c1c' .or. key(i).eq.'C1C') iobs(3) = i
          if (key(i).eq.'c1p' .or. key(i).eq.'C1P') iobs(4) = i
          if (key(i).eq.'c2p' .or. key(i).eq.'C2P') iobs(5) = i
          if (key(i).eq.'s1c' .or. key(i).eq.'S1C') iobs(6) = i
          if (key(i).eq.'s2p' .or. key(i).eq.'S2P') iobs(7) = i
          if (key(i).eq.'s5i' .or. key(i).eq.'S5I') iobs(8) = i
!          if (key(i).eq.'s6' .or. key(i).eq.'S6') iobs(9) = i
!          if (key(i).eq.'s7' .or. key(i).eq.'S7') iobs(10) = i
!          if (key(i).eq.'s8' .or. key(i).eq.'S8') iobs(11) = i
      enddo
      end
