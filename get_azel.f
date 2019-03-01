      subroutine get_azel(tc, prn, stationXYZ,East,North,
     .  localv, Lat,Long,Ht, azimuth,elev,ihr,tod,grange)
      implicit none
      include 'local.inc'
      integer jj, ierr,prn, ihr
      real*8 tc, SatPos(3), toffset, omeg, ghadot,
     . xnew, ynew, localv(3),tod,
     . azimuth, elev, North, East, stationXYZ(3), range2,
     . Lat,Long,Ht, grange
c     starting value
      toffset = 0.07
c     Earth Rotation
      ghadot = 7.2921151467d-5 ! rad/sec, using same one as ICD200
      do jj = 1, 4
        call bcephX(tc-toffset, prn,SatPos,ierr,ihr)
        if (ierr.ne.0) then
          print*, 'orbit error'
        endif
        omeg = -ghadot*toffset
        xnew = SatPos(1)*dcos(omeg) - SatPos(2)*dsin(omeg)
        ynew = SatPos(1)*dsin(omeg) + SatPos(2)*dcos(omeg)
        range2= (xnew - stationXYZ(1))**2 + (ynew 
     . - stationXYZ(2))**2 + (SatPos(3)-stationXYZ(3))**2
        toffset = dsqrt(range2)/c
       enddo
       SatPos(1) =  xnew
       SatPos(2) =  ynew
       grange = dsqrt(range2)
       call azel(azimuth, elev, stationXYZ, East, North,
     .        localv, SatPos)
c      write(38,'(3f20.6)') SatPos(1), SatPos(2), SatPos(3)
       end

