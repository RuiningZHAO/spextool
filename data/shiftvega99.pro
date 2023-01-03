pro shiftvega99

  restore, 'lvega5.sav'
  wvin5 = wvin/1e4
  fvin5 = fvin

  restore, 'lvega99orig.sav'
  wvin99 = wvin/1e4
  fvin99 = fvin

  struc = {spec1:[[wvin5],[fvin5],[fvin5]],$
           spec2:[[wvin99],[fvin99],[fvin99]],$
           spec3:[[wvin99*(1+92/3e5)],[fvin99],[fvin99]]}
  xmc_compspec,struc,['lvega5','lvega99.sav','vega99s.sav'],XRANGE=[1.4,1.7]

  wvin99 = wvin99*(1+92/3e5)

  wvin = wvin99*1e4
  fvin = fvin99

  save,wvin,fvin,fcvin,FILENAME='lvega99.sav'

  
end
