;+
; NAME:
;     mc_tabinv
;
; PURPOSE:
;     To properly deal with arrays with leading/trailing NaNs
;
; CALLING SEQUENCE:
;     mc_tabinv, XARR, X, IEFF, FAST=fast, CANCEL=cancel
;
; INPUTS:
;    XARR - the vector array to be searched, must be monotonic
;           increasing or decreasing
;    X    - the function value(s) whose effective
;           index is sought (scalar or vector)
;
; OPTIONAL INPUTS:
;    None
;
; KEYWORD PARAMETERS:
;    FAST - Set to skip checking whether XARR is monotonicitically
;           increasing in order to improve the program speed.
;
; OUTPUTS:
;    IEFF   - the effective index or indices of X in XARR
;             always floating point, same # of elements as X
;    CANCEL - Set on return if there is a problem.
;
; OPTIONAL OUTPUTS:
;    None
;
; COMMON BLOCKS:
;    None
;
; RESTRICTIONS:
;    Will only work if there are leading and trailing NaNs.
;
; DEPENDENCIES:
;    Requires the Astronomy User's Library
;    http://idlastro.gsfc.nasa.gov/homepage.html
;
; PROCEDURE:
;    Pulls out the good part of the array, and then offsets the
;    resulting index to account for the leading NaNs.
;
; EXAMPLES:
;    Let x = [NaN, NaN, 1, 2, 3, NaN, NaN]
;    mc_tabinv,x,1.5,idx
;    idx -> 2.5
;     
; MODIFICATION HISTORY:
;    2014-08-01 - Written by M. Cushing, University of Toledo
;-
pro mc_tabinv,XARR,X,IEFF,FAST=fast,CANCEL=cancel

  cancel = 0 
  
  if n_params() lt 3 then begin
     
     print, 'Syntax - mc_tabinv,XARR,X,IEFF,FAST=fast,CANCEL=cancel'
     cancel = 1
     return
     
  endif
  
  cancel = mc_cpar('mc_tabinv',xarr,'XARR',1,[2,3,4,5],1)
  if cancel then return
  cancel = mc_cpar('mc_tabinv',x,'X',2,[2,3,4,5],[0,1])
  if cancel then return

  idx = mc_nantrim(xarr,2,CANCEL=cancel)
  if cancel then return
  
  tabinv,xarr[idx],x,tmp,FAST=fast
  ieff = tmp+idx[0]
   
end
