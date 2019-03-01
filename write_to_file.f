      subroutine write_to_file(outID, prn_pick, prn, elev, 
     . azimuth, tod, s1,s2, bad_point,l1,l2,grange,
     .  blockIIRM,L2Ctracking,s5)
      implicit none
      include 'local.inc'
      real*8 s1, s2, tod, elev, azimuth, x,y, l1, l2,w1,w2,
     .  grange, s5
      logical bad_point
      integer prn_pick,outID,L2Ctracking,prn
      integer blockIIRM(maxsat)
c     author: Kristine Larson
c     16jul15 added s5
c     this supports old option that printed out L1 and L2 phase
      w1 = c/f1 ! L1 wavelength in meters
      w2 = c/f2 !L2 wavelength in meters
c     no longer writing out reflection point. columns 5 and 6 are zero
      x = 0
      y = 0

      if (prn_pick.eq.99.and.(elev.gt.5
     .  .and.elev.lt.30)) then
          write(outID,'(i3,  2f10.4, f10.0, 2f7.2, 3f7.2)' )
     .    prn, elev, azimuth, tod, x,y, s1, s2, s5
      elseif ( prn_pick.eq.50 .and.  elev.lt.10 ) then
        write(outID,'(i3,  2f10.4, f10.0, 2f7.2, 2f7.2)' )
     .    prn, elev, azimuth, tod, 0.d0,0.d0, s1, s2
c     all data above 5 degrees
      elseif ( prn_pick.eq.88 .and.  elev.gt.5 ) then
        write(outID,'(i3,  2f10.4, f10.0, 2f7.2, 3f7.2)' )
     .    prn, elev, azimuth, tod, x,y, s1, s2,s5
c     everything < 30
      elseif (  prn_pick.eq.66 .and. elev.lt.30 ) then
        write(outID,'(i3,  2f10.4, f10.0, 2f7.2, 3f7.2)' )
     .    prn, elev, azimuth, tod, 0.d0, 0.d0, s1, s2, s5
c       L2C data only - LLI indicator has to be a zero
c       assumes if no phase data, then it is not a good snr value
      elseif ((prn_pick.eq.77).and.
     .     (blockIIRM(prn).eq.1.and.elev.ge.5))then
        if (.not.bad_point.and.(L2Ctracking.eq.0) ) then
          write(outID,'(i3,  2f10.4, f10.0, 2f7.2, 3f7.2)' )
     .        prn, elev, azimuth, tod, x,y,s1, s2,s5
        endif
c     if you only requested a single satellite
      elseif (prn.eq.prn_pick)  then
        write(outID,'(i3,  2f10.4, f10.0, 2f7.2, 3f7.2)' )
     .    prn, elev, azimuth, tod, x,y, s1, s2, s5
      endif
      end
