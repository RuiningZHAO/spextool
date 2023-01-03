;+
; NAME:
;     mc_rdpaltspeccal
;
; PURPOSE:
;     To read an Palomar Triplespec cal file.
;
; CATEGORY:
;     I/O
;
; CALLING SEQUENCE:
;     
;
;
; INPUTS:
;
;
;
; OPTIONAL INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;
;
;
; OUTPUTS:
;
;
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS:
;
;
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;
;
;
; PROCEDURE:
;
;
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
;-
pro mc_rdpaltspeccal,filename,spectrum,homeorder,dispdeg,w2pcoeffs,CANCEL=cancel

  cancel = 0

;  Check parameters

  if n_params() lt 1 then begin
     
     print, 'Syntax - mc_rdtspecwavecal,filename,spectrum,homeorder,$'
     print, '                           dispdeg,w2pcoeffs,CANCEL=cancel'
     cancel = 1
     return
     
  endif
  
  cancel = mc_cpar('mc_rdpaltspeccal',filename,'Filename',1,7,0)
  if cancel then return

  spectra = readfits(filename,hdr,/SILENT)

  homeorder  = fix(fxpar(hdr,'HOMEORDR'))  
  dispdeg    = fix(mc_fsextract(fxpar(hdr,'W2PDEG'),/INDEX))
  orders     = fix(mc_fsextract(fxpar(hdr,'ORDERS'),/INDEX))
  norders    = n_elements(orders)

;  Read w2p coefficients for each order

  w2pcoeffs = dblarr(max(dispdeg)+1,norders)
  for i = 0,norders-1 do begin
     
     key = 'W2P'+string(orders[i],format='(i2.2)')+'*'
     w2pcoeffs[0:dispdeg[i],i] = mc_fxpar(hdr,key)
     
  endfor

;  Only return the homeorder spectrum

  z = where(orders eq homeorder)
  spectrum = reform(spectra[*,0:1,z])

end
