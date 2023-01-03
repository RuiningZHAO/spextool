;+
; NAME:
;     mc_imgpoly
;
; PURPOSE:
;     To evaulate a polynomial where the independent variable is an image.
;
; CALLING SEQUENCE:
;     result = mc_imgpoly(img,coeffs,CANCEL=cancel)
;
; INPUTS:
;     img   - A 2D array of size n x m.
;     coeff - A 3D array of size (n x m x ncoeff).
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem.
;
; OUTPUTS:
;     result = c_0 + c_1*img + c_2*img^2 + ...
;
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     None
;
; RESTRICTIONS:
;     None
;
; DEPENDENCIES:
;     Requires the Spextool library.
;
; PROCEDURE:
;     Just evaulates the polynomial using the technique of IDL's poly.pro.
;
; EXAMPLES:
;     eh
;
; MODIFICATION HISTORY:
;     2014-08-06 - Written by M. Cushing, University of Toledo
;-
function mc_imgpoly,img,coeffs,CANCEL=cancel

  cancel = 0

  if n_params() ne 2 then begin

     print, 'Syntax - mc_impoly,img,coeff,CANCEL=cancel'
     cancel = 1
     return, -1

  endif

  cancel = mc_cpar('mc_imgpoly',img,1,'img',[2,3,4,5],2)
  if cancel then return,-1
  cancel = mc_cpar('mc_imgpoly',coeffs,2,'coeffs',[2,3,4,5],3)
  if cancel then return,-1

  s = size(coeffs,/DIMEN)
  n = s[2]-1
  y = reform(coeffs[*, *, n])
  for i = n-1, 0, -1 do y = y*img+reform(coeffs[*, *, i])
  
  return, y

end
