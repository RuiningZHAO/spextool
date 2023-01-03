;+
; NAME:
;     mc_poly1dderiv
;
; PURPOSE:
;     To evaluate the derivative of a polynomial function.
;
; CATEGORY:
;     Fitting Data
;
; CALLING SEQUENCE:
;     result = mc_poly1dderiv(x,coeff,CANCEL=cancel)
;
; INPUTS:
;     x     - An x array
;     coeff - The polynomial coefficients
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem.
;
; OUTPUTS:
;     Returns the value of the derivative at x.
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
;     Requires the Astronomy Uers's Library and the Spextool Library.
;
; PROCEDURE:
;     Duh.
;
; EXAMPLE:
;     Later.
;
; MODIFICATION HISTORY:
;     2008-08-14 - Written by M. Cushing, Institute for Astronomy,
;                  University of Hawaii
;-
function mc_poly1dderiv,x,coeff,CANCEL=cancel

  cancel = 0

  if n_params() ne 2 then begin
     
     cancel = 1
     print, 'Syntax - result = poly1d_deriv(x,coeff,CANCEL=cancel)'
     return, -1
     
     
  endif
  cancel = mc_cpar('poly1d_deriv',x, 1, 'X',[2,3,4,5], [0,1])
  if cancel then return, -1
  cancel = mc_cpar('poly1d_deriv',coeff, 2, 'Coeff',[2,3,4,5], [0,1])
  if cancel then return, -1

  ndeg = n_elements(coeff)-1
  newcoeff = coeff[1:*]*(findgen(n_elements(coeff)-1)+1.0)
  return, poly(x,newcoeff)
  
end
