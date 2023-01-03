;+
; NAME:
;     mc_readlinecalfile
;
; PURPOSE:
;     To read a Spextool LineCal file.
;
; CALLING SEQUENCE:
;     
;
; INPUTS:
;
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;
; OUTPUTS:
;
;
; OPTIONAL OUTPUTS:
;
;
; COMMON BLOCKS:
;
;
; RESTRICTIONS:
;
;
; DEPENDENCIES:
;
;
; PROCEDURE:
;
;
; EXAMPLES:
;
;
; MODIFICATION HISTORY:
;
;-
function mc_readlinecalfile,file,CANCEL=cancel

  cancel = 0
  
  spec = readfits(file,hdr,/SILENT)

  orders  = fix(mc_fsextract(fxpar(hdr,'ORDERS'),/INDEX))
  norders = n_elements(orders)

  ordertype = strsplit(fxpar(hdr,'ORDRTYPE'),',',/EXTRACT)
  xcororder = fxpar(hdr,'XCORORDR')
  homeorder = fxpar(hdr,'HOMEORDR')
  dispdeg   = fxpar(hdr,'DISPDEG')
  ordrdeg   = fxpar(hdr,'ORDRDEG')
  w2pdeg    = fix(strsplit(fxpar(hdr,'W2PDEG'),',',/EXTRACT))

  w2pcoeffs = dblarr(max(w2pdeg)+1,norders)

  for i = 0,norders-1 do begin
     
     key = 'W2P'+string(orders[i],format='(i2.2)')+'*'
     w2pcoeffs[0:(w2pdeg[i]),i] = mc_fxpar(hdr,key)
     
  endfor

  ncoeffs = (dispdeg+1)*(ordrdeg+1)
  p2wcoeffs = dblarr(ncoeffs)
  
  for i = 0, ncoeffs-1 do begin
     
     key = 'P2W_a'+string(i,format='(i2.2)')
     p2wcoeffs[i] = fxpar(hdr,key)
     
  endfor
  
  z = where(orders eq xcororder)
  xcorspec = spec[*,*,z]
  
  
  struc = {xcorspec:xcorspec,xcororder:xcororder,ordertype:ordertype, $
           w2pcoeffs:w2pcoeffs,homeorder:homeorder,dispdeg:dispdeg, $
           ordrdeg:ordrdeg,p2wcoeffs:p2wcoeffs,spectra:spec}

  return, struc


end
