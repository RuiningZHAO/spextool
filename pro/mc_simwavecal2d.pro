;+
; NAME:
;     mc_simwavecal2d
;
; PURPOSE:
;     To simulate a 2D wavecal file using pixels for wavelengths.
;
; CALLING SEQUENCE:
;     result = mc_simwavecal2d(ncols,nrows,edgecoeffs,xranges,slith_arc,$'
;                              CANCEL=cancel)
;
; INPUTS:
;     ncols       - The number of columns in the image.
;     nrows       - The number of rows in the image.
;     edgecoeffs  - Array [degree+1,2,norders] of polynomial coefficients 
;                   which define the edges of the orders.  array[*,0,0]
;                   are the coefficients of the bottom edge of the
;                   first order and array[*,1,0] are the coefficients 
;                   of the top edge of the first order.
;     xranges     - An array [2,norders] of pixel positions where the
;                   orders are completely on the array
;     ybuffer     - Number of pixels to move inside of the edge of
;                   array since the edges aren't infinitely sharp
;     slith_arc   - The slit height in arcseconds.      
;
; OPTIONAL INPUTS:
;     none
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem.
;
; OUTPUTS:
;     Returns a ncols,nrows,2 array where array[*,*,0] is the wavecal
;     file and array[*,*,1] is the spatcal file.  (see mc_wavecal2d).
;
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     None
;
; RESTRICTIONS:
;     
;
; DEPENDENCIES:
;     mc_cpar()
;     poly1d()
;
; PROCEDURE:
;     
;
; EXAMPLES:
;
;
; MODIFICATION HISTORY:
;     2009-12-03 - Written by M. Cushing, NASA JPL
;-
function mc_simwavecal2d,ncols,nrows,edgecoeffs,xranges,slith_arc,CANCEL=cancel

  cancel = 0
  
  if n_params() ne 5 then begin

     print, 'Syntax - mc_simwavecal2d(ncols,nrows,edgecoeffs,xranges,$'
     print, '                         slith_arc,CANCEL=cancel)'
     cancel = 1
     return, -1

  endif

  cancel = mc_cpar('mc_simwavecal2d',ncols, 1,'Ncols',[2,3],0)
  if cancel then return, -1
  cancel = mc_cpar('mc_simwavecal2d',nrows, 2,'Nrows',[2,3],0)
  if cancel then return, -1
  cancel = mc_cpar('mc_simwavecal2d',edgecoeffs, 3,'Edgecoeffs',[2,3,4,5],[2,3])
  if cancel then return, -1
  cancel = mc_cpar('mc_simwavecal2d',xranges, 4,'Xranges',[2,3,4,5],[1,2])
  if cancel then return, -1
  cancel = mc_cpar('mc_simwavecal2d',slith_arc, 5,'Slith_arc',[2,3,4,5],0)
  if cancel then return, -1

  wavecal = fltarr(ncols,nrows,/NOZERO)*!values.f_nan
  spatcal = wavecal

  s = size(edgecoeffs)
  norders = (s[0] eq 2) ? 1:s[3]
  
  for i = 0,norders-1 do begin

     start    = xranges[0,i]
     stop     = xranges[1,i]
     numwave  = fix(stop-start)+1
     x        = findgen(numwave)+start
     y        = findgen(nrows)
     pixtoarc = fltarr(stop-start+1,2)

;  Find the bottom and top of the slit
     
     botedge = poly1d(x,edgecoeffs[*,0,i])
     topedge = poly1d(x,edgecoeffs[*,1,i])
     dif     = topedge-botedge

     pixtoarc[*,1] = float(slith_arc) / (topedge-botedge) 
     pixtoarc[*,0] = -1.* (pixtoarc[*,1] * botedge)

     for j =0,stop-start do begin

        
        wavecal[x[j],botedge[j]:topedge[j]] = x[j]
        spatcal[x[j],botedge[j]:topedge[j]] = $
           poly1d(y[botedge[j]:topedge[j]],reform(pixtoarc[j,*]))

     endfor

  endfor

  return, [[[wavecal]],[[spatcal]]]

end

