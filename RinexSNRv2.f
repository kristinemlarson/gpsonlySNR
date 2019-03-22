      program RinexSNRv2
      implicit none
      include 'local.inc'
      integer stderr
      parameter (stderr=6)
      character*80 inline
      character*80 rawfilename, outfilename, broadfile
      character*4 station
      character*2  prn_pickc
      character*1 satID(maxsat)
      integer  nobs, itime(5), prn(maxsat), numsat, flag, sec,
     +   msec, lli(maxob,maxsat), iprn, l2c_pts,
     .   ios, itrack, L2Ctracking, i, iobs(maxsat),  
     .  gpsweek, the_hour, prn_pick, current_hour,npts,
     .  blockIIRM(maxsat)/maxsat*0/, fileIN, fileOUT,iymd(3)
      real*8  obs(maxob,maxsat),  tod,North(3), 
     .   East(3), Up(3), azimuth, elev, staXYZ(3), tod_save, 
     .   s1,s2,xrec, yrec, zrec, tc, l1,l2, Lat,Long,Ht,
     .   bele(maxeph, maxsat, 28),grange,s5,
     .   nxrec, nyrec, nzrec
      logical eof, bad_point
      logical help, fsite, debugging
c     Kristine M. Larson
c     version 2.0 new version - uses subroutines, fixed bug in az-el
c     verison 2.1 fixes bug in selection 77 (reading the LLI value)
c     version 2.2 fixed bug in reading the comment lines, 15sep07
c     version 2.3 added S5
c     version 2.5 distribution on gitHub
c     version 2.6 added moving site velocities, station marker name
c     version 2.7 fixed bug for reading files with < 5 observations
c     version 2.8 changed minimum observables from 10 to 15
c     version 2.9 19mar01 - allow up to 20 observables, changed code
c       to read the header and the data block
c
c     common block for the GPS broadcast ephemeris
      include 'new_orbit.inc'
c     define the blockIIRM satellites
      call pickup_blockIIRM(blockIIRM)
c     set some defaults - used for input and output file IDs
      debugging = .false.
      l2c_pts = 0
      fileIN = 12
      fileOUT = 55
      tod_save = 0.0
      npts = 0
      bad_point = .false.
      help = .false.
c     read input files
      call getarg (1,rawfilename)
      call getarg (2,outfilename)
      call getarg (3,broadfile)
      call getarg (4,prn_pickc)
      write(stderr, *) '----------------------------------'
      write(stderr, *) 'Input RINEX: ', rawfilename
      write(stderr, *) 'Broadcast RINEX: ', broadfile
      write(stderr, *) 'Outputfile: ', outfilename
      if (rawfilename(1:2) .eq. '  ')  help = .true.
c
      if (help) then
         write(stderr,*)
     +   'RinexSNRv2.e inputRinex outputSNR navigation-file out-choice '
         write(stderr,*) ' '
         write(stderr,*) 'out-choice can be a specific PRN number or'
         write(stderr,*) '50 - all satellites, all data < 10 degrees'
         write(stderr,*) '66 - all satellites, all data < 30 degrees'
         write(stderr,*) '77 - L2C satellites, all data > 5 degrees'
         write(stderr,*) '88 - all satellites, all data > 5 degrees'
         write(stderr,*) '99 - all satellites, 5 < data < 30 degrees'
        call exit
      endif
c     figure out which option is being requested
      READ (prn_pickc, '(I2)') prn_pick
      write(stderr, *) 'Selection ', prn_pick


c     read the header of the RINEX file, returning station coordinates
c     19mar01 - change to 20 observables
      call read_header_20obs(fileIN,rawfilename, xrec,yrec,zrec, 
     .  iobs,nobs,iymd,station)

c    KL added 18jan15
      call moving_sites(station, iymd(1), iymd(2), iymd(3),
     .   nxrec,nyrec,nzrec,fsite)
      if (fsite) then
        print*, 'Using variable model station coordinates (meters)'
        xrec = nxrec
        yrec = nyrec
        zrec = nzrec
        print*, xrec, yrec, zrec
      endif
      if ((xrec+yrec+zrec) .eq.0) then
        print*, 'no apriori coordinates - exiting'
        call exit
      endif
      if (iobs(6) .eq. 0) then
        print*, 'no L1 SNR data - exiting '
        call exit
      endif
      if (nobs .gt. 20) then
        print*, 'This code only works for <= 20 obs types'
        call exit
      endif


c     read the broadcast ephemeris information
      call read_broadcast4 (broadfile, bele,iymd)
      call rearrange_bele(bele)

      call envTrans(xrec,yrec,zrec,staXYZ,Lat,Long,Ht,North,East,Up)
c     open output file
      open(fileOUT,file=outfilename, status='unknown')
      eof = .false.
      current_hour = 0
      print*, 'S1 location:', iobs(6)
      print*, 'S2 location:', iobs(7)
      print*, 'S5 location:', iobs(8)
c     start reading the observation records
      do while (.not.eof) 
        inline = ' '
        read(fileIN,'(A80)', iostat=ios) inline
        if (ios.ne.0) goto 99 
        read(inline(1:32),'(5I3,X,I2,X,I3,4X,2I3)')
     +     (itime(i), i=1,5), sec, msec, flag, numsat
        if (itime(4) .ne.current_hour) then
          current_hour = itime(4)
        endif
c       seconds in the day
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
c      19mar01 - expanding number of observables allowed
        call read_block_gps(fileIN, flag,inline,numsat,nobs,satID, 
     .    prn,obs,lli)
c       if flag has value 4, that means there were comment
c       lines, and those were skipped
        if (flag .ne. 4) then
          call convert_time(itime,sec, msec, gpsweek, tc)
          do itrack = 1, numsat
c           only uses GPS satellites currently, skips over other
c           constellations
            if ((satID(itrack).eq.'G').or.(satID(itrack).eq.' ')) then
              the_hour = 1+nint(1.d0*itime(4) + itime(5)/60)
              if (the_hour > 24) the_hour = 24
              iprn = prn(itrack)
c             get azimuth and elevation angle
              call get_azel(tc, iprn, staXYZ,East,North,Up, 
     .          Lat,Long,Ht,azimuth,elev,the_hour,tod,grange)
c             you can modify this to also pick up pseudoranges
c             l1 = obs(iobs(1),itrack)
c             l2 = obs(iobs(2),itrack)
c             these can be modfiied to store phas data. currently 0
              l1 = 0.0
              l2 = 0.0
              s1 = obs(iobs(6),itrack)
              s2 = obs(iobs(7),itrack)
c             check for S5
              if (iobs(8).ne.0) then
                s5 = obs(iobs(8),itrack)
              else
                s5=0
              endif
c             check to see if L2C is available in the file
              if (iobs(2) .ne. 0) then
                L2Ctracking = lli(iobs(2),itrack)
              else
                L2Ctracking = 0
              endif
              if (debugging) then
                print*, s1, s2, s5
              endif
              if (s1.eq.0.d0 .and. s2.eq.0.d0) then
c                 no data, SNR so do not print it
              else
                call write_to_file(fileOUT, prn_pick, iprn, 
     .           elev,azimuth, tod, s1,s2, bad_point,l1,l2,
     .           grange,blockIIRM, L2Ctracking,s5)
                if ((prn_pick .eq. 77).and.(s2.gt.0.d0)) then
                  l2c_pts = l2c_pts + 1 
                endif
                 
                npts = npts + 1
              endif
            else
c             print*, 'skipping non-GPS satellite'
            endif
          enddo
        endif
      enddo
99    continue
      if (npts.eq.0) then
        write(stderr,*) 
     .   'You have been misled. There are no S1/S2 data in this file'
      endif
c     close input and output files
      if (prn_pick .eq. 77) then
        print*, 'L2C points ', l2c_pts
      endif
      close(fileIN)
      close(fileOUT)
      end

