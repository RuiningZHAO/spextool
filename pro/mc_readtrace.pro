;+
; NAME:
;     mc_readtrace
;
; PURPOSE:
;     Reads a SpeX trace file.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     mc_readtrace,filename,modename,orders,naps,fitdeg,tracecoeffs,$
;                  CANCEL=cancel
;
; INPUTS:
;     filename - A scalar string of the SpeX trace file.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     mode        - The observing mode
;     orders      - The order numbers
;     naps        - The number of apertures
;     fitdeg      - The degree of the polynomial coefficients
;     tracecoeffs - Array [fitdegree+1,naps*norders] of polynomial 
;                   coefficients of the traces of the apertures.
;                   The coefficients should be indexed starting with 
;                   the bottom order and looping through the apertures
;                   in that order.
;
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     None
;
; SIDE EFFECTS:
;     None
;
; RESTRICTIONS:
;     None
;
; PROCEDURE:
;     Easy
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2000-06-14 - Written by M. Cushing, Institute for Astronomy, UH
;-
pro mc_readtrace,filename,modename,orders,naps,fitdeg,tracecoeffs,CANCEL=cancel

  cancel = 0
  
;  Check parameters
  
  if n_params() lt 1 then begin
     
     print, 'Syntax - mc_readtrace,filename,modename,orders,naps,fitdeg,$'
     print, '                   tracecoeffs,CANCEL=cancel'
     cancel = 1
     return
     
  endif
  cancel = mc_cpar('mc_readtrace',filename,1,'Filename',7,0)
  if cancel then return
  
  mode = ' '
  line = ''
  openr, lun, filename,/GET_LUN
  
  readf, lun, line
  modename = strtrim( (strsplit(line,'=',/EXTRACT))[1],2 )
  
  readf, lun, line
  orders = ( strsplit(line,'=',/EXTRACT) )[1]
  orders = mc_fsextract(orders,/INDEX,CANCEL=cancel)
  if cancel then return
  
  readf, lun, line
  naps = strtrim( (strsplit(line,'=',/EXTRACT))[1],2 )
  
  readf, lun, line
  coeffs = (strsplit(line,'=',/EXTRACT))[1]
  tracecoeffs = float(strsplit(coeffs,' ',/EXTRACT))
  
  fitdeg = n_elements(tracecoeffs)-1
  
  while eof(lun) ne 1 do begin
     
     readf, lun, line
     coeffs = (strsplit(line,'=',/EXTRACT))[1]
     coeffs = float(strsplit(coeffs,' ',/EXTRACT))
     tracecoeffs = [[tracecoeffs],[coeffs]]
     
  endwhile
  
end




