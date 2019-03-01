      real*8 naode,ncrs, ndeltan, nem0, ncuc,ne, ncus,
     .  nroota,ntoe,ncic, nomega0,
     .  ncis, nxi0, ncrc, nper0,nomegad,ntoc, nxclk,
     .  nsclk, nridot,nistano
c    change dimension to place for ephemeris each hour
      common/new_borbit/naode(24,maxsat),ncrs(24,maxsat),
     .  ndeltan(24,maxsat), nem0(24,maxsat), ncuc(24,maxsat),
     .  ne(24,maxsat),ncus(24,maxsat),nroota(24,maxsat),
     .  ntoe(24,maxsat),ncic(24,maxsat),nomega0(24,maxsat),
     .  ncis(24,maxsat),nxi0(24,maxsat),
     .  ncrc(24,maxsat),nper0(24,maxsat),
     .  nomegad(24,maxsat),ntoc(24,maxsat),
     .  nxclk(24,maxsat,3),nsclk(24,maxsat,3),
     .  nridot(24,maxsat), nistano(24,maxsat)
