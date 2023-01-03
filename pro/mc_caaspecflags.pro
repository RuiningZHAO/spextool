;+
; NAME:
;     mc_caaspecflags (Check And Add Spectral Flags)
;
; PURPOSE:
;     To add a flag array to <v4.0 spextool spectral arrays
;
; CALLING SEQUENCE:
;     result = mc_caaspecflags(spec,CANCEL=cancel)
;
; INPUTS:
;     spec - A <v4.0 spextool spectral array.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem.
;
; OUTPUTS:
;     result - the same array but with flag arrays set to zero added.
;
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     None
;
; RESTRICTIONS:
;     Can only be used on <v4.0 spextool spectra array
;
; DEPENDENCIES:
;     Requires the Spextool and associated packages.
;
; PROCEDURE:
;     Just dimension checking. 
;
; EXAMPLES:
;     Duh
;
; MODIFICATION HISTORY:
;     2015-02-02 - Written by M. Cushing, University of Toledo
;-
function mc_caaspecflags,spec,CANCEL=cancel

  cancel = 0

  s = size(spec)
  if s[0] eq 2 then begin

     if s[2] eq 4 then return,spec
     
     new = fltarr(s[1],4)
     new[*,0:2] = spec
     return, new
     
  endif else begin

     if s[2] eq 4 then return,spec
     
     new = fltarr(s[1],4,s[3])
     new[*,0:2,*] = spec
     return, new
     
  endelse



end
