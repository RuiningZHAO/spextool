;+
; NAME:
;     mc_mkapmask2d
;
; PURPOSE:
;     To create aperture masks in 2D
;
; CALLING SEQUENCE:
;     result = mc_mkbgmask2d(omask,spatcal,orders,positions,PSBGINFO=psbginfo,$
;                            XSBGINFO=xsbginfo,CANCEL=cancel)
;
; INPUTS:
;     omask     - A 2D order mask image.  The pixels in a given order
;                 should be set to the order number.
;     spatcal   - The spatial calibration array.
;     orders    - The orders to create masks for.
;     positions - An [naps,norders] array given the
;                 aperture positions for each order.
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;     PSBGINFO  - 2 element array [bgstart,bgwidth] giving the radius 
;                 at which to start the background and the width of 
;                 the background in units of slit.
;     XSBGINFO  - (Background Regions) Array of background regions 
;                 in arcseconds ([[1,2],[13-15]]).
;     CANCEL    - Set on return if there is a problem.
;
; OUTPUTS:
;     A 2D image with the bg pixels set to -1.
;
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     None
;
; RESTRICTIONS:
;     This only works on a unit pixel basis, in contrast to mc_mkapmask1d.
;
; DEPENDENCIES:
;     mc_cpar()
;
; PROCEDURE:
;     First the backgrounds are set to -1 without regard to the
;     apertures.  Then from appos[i]-bgstart to appos[i]+bgstart is         
;     cleared out and set to 0.
;
; EXAMPLES:
;
;
; MODIFICATION HISTORY:
;     2009-12-03 - Written by M. Cushing, NASA JPL
;-
function mc_mkbgmask2d,omask,spatcal,orders,positions,PSBGINFO=psbginfo, $
                       XSBGINFO=xsbginfo,CANCEL=cancel
  
  cancel = 0
  
  if n_params() ne 4 then begin
     
     print, 'Syntax - mc_mkbgmask2d(omask,spatcal,orders,positions,$'
     print, '                       PSBGINFO=psbginfo,XSBGINFO=xsbginfo,$'
     print, '                       CANCEL=cancel)'
     cancel = 1
     return, -1

  endif

  cancel = 0

;  Get pertinent info


  norders = n_elements(orders)
  naps    = n_elements(positions[*,0])

  s     = size(omask,/DIMEN)
  ncols = s[0]
  nrows = s[1]

  mask = intarr(ncols,nrows)

  for i = 0,norders-1 do begin
     
     if n_elements(PSBGINFO) ne 0 then begin
        
        bgstart = psbginfo[0]
        bgwidth = psbginfo[1]
        
        for j = 0,naps-1 do begin
           
           z = where((omask eq orders[i] and $
                      spatcal gt positions[j,i]+bgstart and $
                      spatcal lt positions[j,i]+bgstart+bgwidth) or $
                     (omask eq orders[i] and $
                      spatcal gt positions[j,i]-bgstart-bgwidth and $
                      spatcal lt positions[j,i]-bgstart))
           
           mask[z] = 1
           
        endfor
        
        for j =0, naps-1 do begin
           
;  Now clear out from -bgstart to bgstart around each peak.
           
           z = where((omask eq orders[i] and $
                      spatcal gt positions[j,i]-bgstart and $
                      spatcal lt positions[j,i]+bgstart))
           
           mask[z] = 0
           
        endfor
        
     endif
     
     if n_elements(XSBGINFO) ne 0 then begin
        
        s = size(XSBGINFO)
        nbg = (s[0] eq 1) ? 1:s[2]
        for j = 0, nbg-1 do begin
           
           
           z = where((omask eq orders[i] and $
                      spatcal gt xsbginfo[0,j] and $
                      spatcal lt xsbginfo[1,j]))
           
           mask[z] = 1
           
        endfor
        
     endif
     
  endfor
  
  return, mask

end
