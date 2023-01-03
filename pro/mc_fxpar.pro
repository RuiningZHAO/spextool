;+
; NAME:
;     mc_fxpar
;
; PURPOSE:
;     Extracts values from a FITS hdr using a wildcard.
;
; CATEGORY:
;     File I/O
;
; CALLING SEQUENCE:
;     result = mc_fxpar(hdr,keyword,CANCEL=cancel)
;
; INPUTS:
;     hdr     - A string array FITS header
;     keyword - The keyword to extract
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     Returns an array of the values that match keyword
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
;     Obvious
;
; EXAMPLE:
;     result = mc_fxpar(hdr,'DP*')
;
; MODIFICATION HISTORY:
;     2002 - Written by M. Cushing, Institute for Astronomy, UH
;     2008-03-06 - Renamed from mcfxpar to mc_fxpar
;-
function mc_fxpar,hdr,keyword,CANCEL=cancel

  cancel = 0
  
  if n_params() ne 2 then begin
     
     print, 'Syntax - result = mc_fxpar(hdr,keyword,CANCEL=cancel)'
     cancel = 1
     return, -1
     
  endif
  cancel = mc_cpar('mc_fxpar',hdr,1,'Hdr',7,[0,1])
  if cancel then return,-1
  cancel = mc_cpar('mc_fxpar',keyword,2,'Keyword',7,0)
  if cancel then return,-1
  
  nlines = n_elements(hdr)
  
  idx = 0
  for i = 0, nlines-1 do begin
     
     if strmid(hdr[i],8,1) eq '=' then begin
        
        key = strtrim( strmid(hdr[i],0,8),2)
        if strmatch(key,keyword) then begin
           
           val  = (idx eq 0) ? fxpar(hdr,key):[val,fxpar(hdr,key)]
           skey = (idx eq 0) ? key:[skey,key]
           idx  = idx + 1
           
        endif
        
     endif
     
  endfor
  
  return, val

end
