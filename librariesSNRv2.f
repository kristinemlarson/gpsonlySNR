        subroutine azel(azimuth, elev, StaXYZ, East, North,
     .                  Staup, SatPos)
        implicit none
c      author: kristine larson
c      returns azimuth and elevation angle in degrees
c      StaXYZ is the cartesian coordinates of station (meters)
c      SatPos is X, Y, Z satellite coordinates (meters)
c ----------------------------------------------------
      double precision elev, azimuth,pi
      double precision StaXYZ(3),  East(3),
     . North(3), Staup(3), sta2sat(3), Mstation, Msat,
     . sta2sat_N, sta2sat_E, UdotS, zenith,SatPos(3)

c ----------------------------------------------------

c ... find azimuth/elevation of satellite relative
c ... to the reference station (station(3))
        pi = 4.0*datan(1.d0)
        sta2sat(1) = SatPos(1) - StaXYZ(1)
        sta2sat(2) = SatPos(2) - StaXYZ(2)
        sta2sat(3) = SatPos(3) - StaXYZ(3)

c    ... azimuth of satellite from station:
c  ... get components of 'sta2sat' in ENV frame:
         sta2sat_E = East(1)*sta2sat(1) + East(2)*sta2sat(2)
     .        + East(3)*sta2sat(3)
         sta2sat_N = North(1)*sta2sat(1) + North(2)*sta2sat(2)
     .        + North(3)*sta2sat(3)

c atan2(X,Y) == tan-1(X/Y)
         azimuth = datan2(sta2sat_E, sta2sat_N)*180/pi
         if (azimuth .lt. 0) then
            azimuth = 360 + azimuth
         endif

c    ... elevation angle calculation:
c         Mstation = dsqrt(Staup(1)**2 + Staup(2)**2 +
c    .          Staup(3)**2)
         Mstation = 1 
         Msat = dsqrt(sta2sat(1)**2 + sta2sat(2)**2 +
     .          sta2sat(3)**2)
         UdotS = Staup(1)*sta2sat(1) + Staup(2)*sta2sat(2) +
     .           Staup(3)*sta2sat(3)
         zenith = dacos(UdotS/(Mstation*Msat))*180/pi
         elev = 90 - zenith

       return
       end

      subroutine bcephX(tc,isv,recf,ierr,hr)
c     broadcast ephemeris evaluation routine
c     taken from icd-gps-200, table 20-iv
c     "elements of coordinate systems"
c     all angular values must be converted to radians
c
c     returns satellite coordinates in METERS
      implicit real*8 (a-h,o-z)
      save
      integer maxsat
      parameter (maxsat=50)
      dimension recf(3)
      integer hr, izh
      include 'new_orbit.inc'
      xmu = 3.986005d+14
      ghadot = 7.2921151467d-5
      izh = 1
      pi = 3.1415926535898d0
      twopi=pi+pi
      ierr=0
c     isv and j are the PRN numbers
      j=isv
      if(j.eq.0)then
        ierr=1
        return
      end if
      a=nroota(hr,j)**2
      en0=dsqrt(xmu/a**3)
      tk=tc-ntoe(hr,j)
      if(tk.gt.302400.d0)tk=tk-604800.d0
      if(tk.lt.-302400.d0)tk=tk+604800.d0
      en=en0+ndeltan(hr,j)
      emk=nem0(hr,j)+en*tk
      emk=dmod(emk,twopi)
      emk=dmod(emk+twopi,twopi)
      ek=emk
      ekdot=en
c
c   solve kepler's equation
c
      small = 0.00000000000001
      do 100 i=1,100
         sine=dsin(ek)
         cose=dcos(ek)
         ek=emk+ne(hr,j)*sine
         if (dabs(ek-saveek).lt.small) goto 101
         ekdot=en+ne(hr,j)*cose*ekdot
         saveek = ek
100   continue

101   continue
      roote=dsqrt(1.d0-ne(hr,j)**2)
      truea=datan2(roote*sine,cose-ne(hr,j))
      dlat=truea+nper0(hr,j)
      twolat=dmod(dlat+dlat,twopi)
      sin2p=dsin(twolat)
      cos2p=dcos(twolat)
      corlat=izh*(ncuc(hr,j)*cos2p+ncus(hr,j)*sin2p)
      corr=  izh*(ncrc(hr,j)*cos2p+ncrs(hr,j)*sin2p)
      cori=  izh*(ncic(hr,j)*cos2p+ncis(hr,j)*sin2p)
      dlat=dmod(dlat+corlat,twopi)
      rk=a*(1.d0-ne(hr,j)*cose) + corr
      xik=nxi0(hr,j) + cori + nridot(hr,j)*tk
      coslat=dcos(dlat)
      sinlat=dsin(dlat)
      xk=rk*coslat
      yk=rk*sinlat
      omegak=dmod(nomega0(hr,j)+ 
     .  (nomegad(hr,j)-ghadot)*tk-ghadot*
     .  ntoe(hr,j),twopi)
      coso=dcos(omegak)
      sino=dsin(omegak)
      cosi=dcos(xik)
      sini=dsin(xik)
c ecef coordinates of satellite in meters
      recf(1)=xk*coso-yk*cosi*sino
      recf(2)=xk*sino+yk*cosi*coso
      recf(3)=yk*sini
c     end bcxyz
      end

c---------------------------------------------------------------------
      subroutine geoxyz2(alat,along,hght,x,y,z,iflag)
c
c Purpose:
c     Convert geodetic curvilinear coordinates to geocentric Cartesian
c        coordinates and vice versa
c
c Input:
c     iflag = 1  convert geodetic coordinates to cartesian coordinates
c           = 2  convert cartesian coordinates to geodetic coordinates
c
c Input/Output:
c     alat,along : geodetic latitude and longitude (radians)
c     hght       : height above the reference ellipsiod (meters)
c     x,y,z      : geocentric Cartesian coordinates (meters)
c
c Notes:
c     Currently assumes the WGS84 reference ellipsoid;
c     Clarke 1866 ellipsoid with approximate translation parameters
c        for NAD27 are commented.
c     Cartesian to geodetic conversion uses an iterative scheme
c        valid to the millimeter level.
c
      implicit none 
      integer iflag
      real*8 alat,along,hght,x,y,z, b
      real*8 semi,finv
      real*8 twopi,f,e2,curvn,sqr,alat0,cutoff
      real*8 sinlat,coslat,sinlon,coslon

      semi = 6378137.d0
      finv = 298.257223563d0
      twopi= 8.d0*datan(1.d0)
      f= 1.d0/finv
      b = semi*(1.d0 - f)
      e2= 2.d0*f - f*f
      if( iflag.eq.1) then
         sinlat= dsin(alat)
         coslat= dcos(alat)
         sinlon= dsin(along)
         coslon= dcos(along)
         curvn= semi/(dsqrt(1.d0-e2*sinlat*sinlat))
     
         x= (curvn+hght)*coslat*coslon 
         y= (curvn+hght)*coslat*sinlon 
         z= (curvn*(1.d0-e2)+hght)*sinlat 
      else
         along= datan2(y,x)
         if( along.lt.0d0 ) along=along + twopi
c        starting value for latitude iteration
         sqr= dsqrt(x*x+y*y)
         alat0= datan2(z/sqr,1.d0-e2)
         alat= alat0
   40    sinlat= dsin(alat)
         curvn= semi/(dsqrt(1.d0-e2*sinlat*sinlat))
         alat= datan2((z+e2*curvn*sinlat),sqr)
c        iterate to millimeter level
         if( dabs(alat-alat0).lt.1.d-10) goto 30
         alat0= alat
         goto 40
   30    continue
         cutoff= 80.d0*twopi/360.d0
         if(alat.le.cutoff) then
            hght= (sqr/dcos(alat))-curvn
         else
            hght= z/dsin(alat)-curvn+e2*curvn
         endif
      endif
      return
      end
      subroutine julday (itimes, stime, utc)
c
c...coded by: j mcmillan - university of texas - july 1973
c   julday entry added by brian cuthbertson - 9/29/1979.
c
c...purpose:  to convert between calendar day and julian date (utc).
c
c...formal parameter definitions:
c
c   kalday input  (julday output):
c      utc       the julian date in (double precision) utc form.
c
c      itimes    integer array containing month, day, year, hour, minute
c      stime     floating point seconds
c
      implicit double precision (a-h,o-z)
      dimension itimes(5)
      jd = itimes(2) - 32075 +
     .     1461 * (itimes(3) + 4800 + (itimes(1)-14)/12) /4  +
     .     367 * (itimes(1) - 2 - (itimes(1)-14)/12*12) /12  -
     .     3 * ((itimes(3) + 4900 + (itimes(1)-14)/12)/100) /4
c
      utc= dfloat(jd-2400000) - 1.0d+0  +
     .     dfloat(itimes(4)) / 24.0d+0  +
     .     dfloat(itimes(5)) / 1440.0d+0  +
     .     stime / 86400.0d+0
c
      return
c
c...end kalday/julday
      end
      subroutine mjdgps(tjul,isec,nweek)
c      save
c*
cc name       : mjdgps
cc
cc      call mjdgps(tjul,second,nweek)
cc
cc purpose    : compute number of seconds past midnight of last
cc              saturday/sunday and gps week number of current
cc              date given in modified julian date
cc
cc parameters :
cc         in : tjul  : modified julian date                      r*8
cc        out : second: number of seconds past midnight of last   r*8
cc                      weekend (saturday/sunday)
cc              nweek : gps week number of current week           i*4
cc
cc sr called  : none
cc
cc author     : w. gurtner
cc              mjdgps is a member of gpslib
cc              astronomical institute, university of berne
cc              switzerland
cc##	no unit is used in this subroutine
cc
        implicit real*8 (a-h,o-z)
c
c
c  days since starting epoch of gps weeks (sunday 06-jan-80)
      deltat=tjul-44244.d0
c  current gps week
      nweek=deltat/7.d0
c  seconds past midnight of last weekend
      second=(deltat-(nweek)*7.d0)*86400.d0 
      isec = second  + 0.5d0
c
      return
      end
      subroutine read_broadcast4(filename, bele,iymd)
      implicit none
      include 'local.inc'
      character*80 filename, temp
      real*8 bele (maxeph,maxsat,28), rt1, rt2, rt3, rt4
      integer i, j, k, k1, isat, iymd(3),year4ch,
     .  iprn, file(maxsat), it1, it2, it3, it4, it5, iversion, ios
      do i = 1, maxsat
        file(i) = 0
        do j=1,maxeph 
          bele(j, i, 28) = 99
        enddo
      enddo

      open (44, file = filename, status='old',iostat=ios ) 
      if (ios.eq.0) then
      else
        print*, 'problems with this ', filename
        print*, 'perhaps it does not exist'
        call exit
      endif 
      read(44, '(i6)') iversion 
      if (iversion.eq.2.or.iversion.eq.1) then
102     read(44, '(60x, a20)') temp
        if (temp(1:13).eq.'END OF HEADER'.or.
     .   temp(1:8).eq.'        ') then
        else
          goto 102
        endif
      else
        print*, 'can only read version 1 or 2'
        call exit(1)
      endif
12    read( 44, '(i2, 5i3, f5.1, 3d19.12)',err=14,iostat=ios) iprn, it1,
     .  it2, it3, it4, it5, rt1, rt2, rt3, rt4
      if (ios.ne.0) goto 14
      file(iprn) = file(iprn) + 1
      if (file(iprn).gt.maxeph) then 
        print*, 'MORE THAN 50 ephemeris for PRN', iprn
        goto 14
      endif
      do j = 1, 6
        k1 = 4*(j-1) + 1
        read( 44, '(3x, 4d19.12)', err=14, iostat=ios) 
     .       ( bele(file(iprn), iprn , k), k = k1, k1+3)
        if (ios.ne.0) goto 14
      enddo
      if (iversion.eq.2) read(44,'(a60)') temp
c     put decimal hour into the space in broadcast 
c     element 28, i.e. hour plus minute/60
c     if the year month and day are NOT the same as the observation
c     file, put a 99 in this space
      if (it1 .lt. 80) then
        year4ch = it1 + 2000
      else
        year4ch = it1 + 1900
      endif
      if (iymd(1) .eq. year4ch .and. (iymd(2) .eq. it2 .and. 
     .   iymd(3) .eq.it3)) then
        bele(file(iprn), iprn, 28) = it4 + it5/60
      else
c       this is common, so do not print it out. but basically
c       we are going to ignore broadcast messages on the next day
c       print*, 'Ephemeris message on the wrong day' 
c       print*, it1, it2, it3, it4, it5, 'PRN ', iprn
      endif
      goto 12
14    continue
      return
      end
      subroutine rearrange_bele(bele)
      implicit none
      include 'local.inc'
      integer i, j, ihr, k, isat
      include 'new_orbit.inc'
      real*8 bele(maxeph, maxsat, 28)
      do isat = 1, maxsat
        do j=1,24
c         subtract one hour because ephem 1 goes to hour 0
          ihr = j-1
          call closest_eph(ihr, bele, isat,  k)
          naode(j,isat)=   bele(k, isat, 1)
          ncrs(j,isat)=    bele(k, isat, 2)
          ndeltan(j,isat)= bele(k, isat, 3)
          nem0(j,isat)=    bele(k, isat, 4)
          ncuc(j,isat)=    bele(k, isat, 5)
          ne(j,isat)=      bele(k, isat, 6)
          ncus(j,isat)=    bele(k, isat, 7)
          nroota(j,isat)=  bele(k, isat, 8)
          ntoe(j,isat)=    bele(k, isat, 9)
          ncic(j,isat)=    bele(k, isat, 10)
          nomega0(j,isat)= bele(k, isat, 11)
          ncis(j,isat)=    bele(k, isat, 12)
          nxi0(j,isat)=    bele(k, isat, 13)
          ncrc(j,isat)=    bele(k, isat, 14)
          nper0(j,isat)=   bele(k, isat, 15)
          nomegad(j,isat)= bele(k, isat, 16)
          nridot(j,isat)=  bele(k, isat, 17)
        enddo
      enddo
      end
      subroutine closest_eph(ihr, bele, isat, whichEphem)
      implicit none
      include 'local.inc'
      integer  itest, ihr, j,whichEphem, minval, mv,isat
      real*8 bele(maxeph, maxsat, 28)
c     want ephemeris closest to ihr
      minval = 15
      whichEphem = 1
      do j=1, maxeph
         itest = bele(j,isat,28)
         if (itest .ge.0 .or. itest .lt. 24) then
           mv =  abs(itest -ihr) 
           if (mv .lt. minval) then 
             minval = mv 
             whichEphem = j
           endif
         endif
      enddo  
      end

      subroutine read_header(fileID,rawf,xrec,yrec,zrec,iobs,nobs, 
     .  iymd,station)
      implicit none
c     Author: kristine larson
c     reads header of a RINEX file
c     KL added marker name, 18jan15, returns to main program
c     16jul15 added S5
c     note: marker name might be uppercase and filename uses lowercase
c     if you use moving site coordinates, make sure they are consistent
c     18feb22 added L5
      include 'local.inc'
      integer  i, fileID
      character*80 rawf
      character*80 line, dynfmt
      logical  endofheader 
      integer nobs,iobs(maxsat), iymd(3)
      character*2  key(maxsat)
      real*8 xrec, yrec, zrec
      character*4 station
c     default value is empty
      station = '    '
      endofheader = .false.

      open(fileID,file=rawf, status='old')
      do while (.not.endofheader)
        read (fileID,'(a80)') line
c       print*, line
        if (line(61:80).eq.'# / TYPES OF OBSERV') then
          read(line, fmt='(I6)') nobs
          if (nobs .lt. 10) then
            write(dynfmt, fmt='(A, I3.3, A)')
     +                      "(6X,", nobs, "(4X,A2))"
            read(line, fmt=dynfmt) (key(i), i=1,nobs)
          else
            write(dynfmt, fmt='(A, I3.3, A)')
     +                      "(6X,", 9, "(4X,A2))"
            read(line, fmt=dynfmt) (key(i), i=1,9)
c           read the next line
            read (fileID,'(a80)') line
            write(dynfmt, fmt='(A, I3.3, A)')
     +                      "(6X,", nobs-9, "(4X,A2))"
            read(line, fmt=dynfmt) (key(i), i=10,nobs)
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
c       added station name
        else if (line(61:71).eq.'MARKER NAME') then
          read(line(1:4), fmt= '(a4)')  station
          print*, 'Station name ', station
        else if (line(61:77).eq.'TIME OF FIRST OBS') then
          read(line, fmt= '(3i6)') iymd(1), iymd(2), iymd(3)
          print*, iymd
        endif
        if (line(61:73).eq.'END OF HEADER'.or.
     +       line(61:73).eq.'end of header'.or.
     +       line(61:73).eq.' ') endofheader = .true.
      enddo
      print*, 'FOUND END OF HEADER'
      do i = 1,maxsat
          iobs(i) = 0
      enddo
c     18feb22 added L5
      do i = 1, nobs
          if (key(i).eq.'l1' .or. key(i).eq.'L1') iobs(1) = i
          if (key(i).eq.'l2' .or. key(i).eq.'L2') iobs(2) = i
          if (key(i).eq.'c1' .or. key(i).eq.'C1') iobs(3) = i
          if (key(i).eq.'p1' .or. key(i).eq.'P1') iobs(4) = i
          if (key(i).eq.'p2' .or. key(i).eq.'P2') iobs(5) = i
          if (key(i).eq.'s1' .or. key(i).eq.'S1') iobs(6) = i
          if (key(i).eq.'s2' .or. key(i).eq.'S2') iobs(7) = i
          if (key(i).eq.'s5' .or. key(i).eq.'S5') then
            iobs(8) = i
            print*, 'found S5 in this file'
          endif
          if (key(i).eq.'l5' .or. key(i).eq.'L5') iobs(9) = i
      enddo
      end

      subroutine pickup_blockIIRM(blockIIRM)
      implicit none
      include 'local.inc'
      integer blockIIRM(maxsat)
c     define the blockIIRM satellites
c     this should be modified so that it is time dependent,
c     i.e. PRN 25 was only blockIIRM after August 2010.
      blockIIRM(1) = 1
      blockIIRM(5) = 1
      blockIIRM(7) = 1
      blockIIRM(12) = 1
      blockIIRM(17) = 1
      blockIIRM(15) = 1
      blockIIRM(29) = 1
      blockIIRM(24) = 1
      blockIIRM(31) = 1
      blockIIRM(25) = 1
      blockIIRM(27) = 1
c     launched jday 052 in 2014
      blockIIRM(30) = 1
c     launched jDAY 137 in 2014
      blockIIRM(6) = 1
c     launched August 1, 2014
      blockIIRM(9) = 1
c     launched October 29?, 2014
      blockIIRM(3) = 1
c     launched March 2015
      blockIIRM(26) = 1
c     launched July 2015 - healthy around August 12
      blockIIRM(8) = 1
c     launched oct 2015 - healthy 15dec09
      blockIIRM(10) = 1
c     launched feb 2016 - healthy in march
      blockIIRM(32) = 1

      end

      subroutine envTrans(xrec,yrec,zrec,stationXYZ, Lat,Long, 
     .  Height, North, East,Up)
c     all in meters
      implicit none
      real*8 stationXYZ(3), xrec, yrec, zrec, Lat,Long,Height
      real*8 North(3), East(3), Up(3)
      real*8 eflat, pi
      eflat = .33528919d-02
      pi = 4.0*datan(1.d0)
c     XYZ in meters
      stationXYZ(1) = xrec
      stationXYZ(2) = yrec
      stationXYZ(3) = zrec
      call geoxyz2(Lat,Long,Height,xrec,yrec,zrec,2)
      write(6,'(2F15.9,1x,f12.4)') 180*Lat/pi,180*Long/pi,Height
      Up(1) = dcos(Lat)*dcos(Long)
      Up(2) = dcos(Lat)*dsin(Long)
      Up(3) = dsin(Lat)
c ... also define local east/north for station:
      North(1) = -dsin(Lat)*dcos(Long)
      North(2) = -dsin(Lat)*dsin(Long)
      North(3) = dcos(Lat)
      East(1) = -dsin(Long)
      East(2) = dcos(Long)
      East(3) = 0
      end
      subroutine convert_time(itime,sec, msec, gpsweek, tc)
      implicit none
c     returns tc, which is time in gps seconds (real*8)
      integer itime(5), jtime(5), gpsweek, gpsseconds, msec, sec
      real*8 tjul, rho, tc
c     change 2 char to 4 char
      if (itime(1).lt.80) then
          jtime(3) = itime(1) + 2000
      else
          jtime(3) = itime(1) + 1900
      endif
c     rearrange
      jtime(1) = itime(2)
      jtime(2) = itime(3)
      jtime(4) = itime(4)
      jtime(5) = itime(5)
      rho = 1.0*sec
      call julday(jtime,rho,tjul)
      call mjdgps(tjul,gpsseconds,gpsweek)
c     gps seconds, including non integer parts
      tc = dble(gpsseconds) + msec/1000.0
      end
