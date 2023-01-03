pro mc_smoothpaltspec,ifile,rp,ofile,CANCEL=cancel

  spec = readfits(ifile,/SILENT,hdr)
  wave = spec[*,0]
  flux = spec[*,1]
  error = spec[*,2]
  
  x = findgen(n_elements(spec[*,0]))

  requestedfwhm = 2700./rp*3.3

  kernelfwhm = sqrt(requestedfwhm^2 - 3.3^2)

  mc_convolvespec,findgen(n_elements(flux)),flux,kernelfwhm,cflux,cerror, $
                  ERROR=error,/VARWEIGHT

  spec[*,1] = cflux
  spec[*,2] = cerror
  
  writefits,ofile,spec,hdr




end
