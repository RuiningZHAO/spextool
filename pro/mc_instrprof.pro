;+
; NAME:
;     mc_instrprof
;
; PURPOSE:
;     Computes an instrument profile using model parameters.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = mc_instrprof(x,parms,CANCEL=cancel)
;
; INPUTS:
;     x     - An input x array
;     parms - An array of parameters for the instrument profile 
;             (see below)
;    
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;     
; OUTPUTS:
;     Returns the normalized instrument profile based on the parms
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
;     This routine uses an error function to construct an instrument
;     profile.  This function is useful because it can create an
;     almost gaussian like profile for narrow slits and a flat-topped
;     profile for wider slits.
;
; EXAMPLE:
;    
; MODIFICATION HISTORY:
;     2002 - Written by M. Cushing, Institute for Astronomy, UH
;     2014-08-01 - changed name to mc_instrprof.pro.
;-
function mc_instrprof,x,parms,CANCEL=cancel

  cancel = 0

;  Check parameters

  if n_params() lt 2 then begin
     
     print, 'Syntax - result = mc_instrprof(x,parms,CANCEL=cancel)'
     cancel = 1
     return, -1
     
  endif
  cancel = mc_cpar('mc_instrprof',x,1,'X',[1,2,3,4,5],1)
  if cancel then return,-1
  cancel = mc_cpar('mc_instrprof',parms,2,'Parms',[1,2,3,4,5],1)
  if cancel then return,-1
  
  ip = errorf((x+parms[1]-parms[0])/parms[2]) - $
       errorf((x-parms[1]-parms[0])/parms[2]) 
  
  ip = ip/total(ip)
;  ip = ip/int_tabulated(x,ip)

  return, ip
  
end
