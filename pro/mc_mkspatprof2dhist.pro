;+
; NAME:
;     mc_mkspatprof2d
;
; PURPOSE:
;     To construct average spatial profiles.
;
; CALLING SEQUENCE:
;     result = mc_mkspatprof2d(img,edgecoeffs,xranges,ybuffer,spatcal,$
;                              [wavecal],[wtrans],[ttrans],[atmosthresh],$
;                              BINSIZE=binsize,CANCEL=cancel)
;
; INPUTS:
;     img         - a 2D image.
;     edgecoeffs  - Array [degree+1,2,norders] of polynomial coefficients 
;                   which define the edges of the orders.  array[*,0,0]
;                   are the coefficients of the bottom edge of the
;                   first order and array[*,1,0] are the coefficients 
;                   of the top edge of the first order.
;     xranges     - An array [2,norders] of pixel positions where the
;                   orders are completely on the array
;     ybuffer     - Number of pixels to move inside of the edge of
;                   array since the edges aren't infinitely sharp
;     spatcal     - A 2D image of the same size as img that gives the
;                   spatial coordinates of each pixel.
;
; OPTIONAL INPUTS:
;     wavecal     - A 2D image of the same size as img that gives the
;                   wavelength of each pixel.
;     wtrans      - An 1D array of wavelengths for the atmospheric transmission.
;     ttrans      - A 1D array of the the atmospheric transmission.
;     atmosthresh - The transmission (0-1) below which data is ignored.
;
; KEYWORD PARAMETERS:
;     BINSIZE - Size of the histogram binsize in units of spatcal
;               with which to bin the spatial profile.  If not given,
;               then the binsize is computed automatically as,
;               
;               binsize = slith_arc/slith_pix
;
;               where each parameter is estimated from the other
;               inputs.  
;               
;               slith_arc = max(spatcal[order])
;               slith_pix = max(topslit-botslit)
;
;     CANCEL - Set on return if there is a problem.
;
; OUTPUTS:
;     Returns a structure where each value is a 2D array where
;     array[*,0] is the x values and array[*,1] is the y values.
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
;     mc_cpar (Spextool)
;     poly1d (Spextool)
;     linterp (astron)
;
; PROCEDURE:
;     Histograms the spatial coordinate values for each order and uses
;     reverse indices to keep track of which pixels fall in which
;     bin.   The profile is then created by taking the median value of
;     the img pixels in each bin.  
;
; EXAMPLES:
;     
;
; MODIFICATION HISTORY:
;     2009-12-03 - Written by M. Cushing, NASA JPL
;-
function mc_mkspatprof2d,img,edgecoeffs,xranges,ybuffer,spatcal, $
                         wavecal,wtrans,ttrans,atmosthresh,CANCEL=cancel

  cancel = 0

  if n_params() ne 5 then begin

     print, 'Syntax - mc_mkspatprof2d(img,edgecoeffs,xranges,ybuffer,$'
     print, '                         spatcal,wavecal,wtrans,ttrans,$'
     print, '                         atmosthresh,CANCEL=cancel)'
     cancel = 1
     return, -1

  endif

  cancel = mc_cpar('mc_mkspatprof2d',img, 1,'Wcinfo',[2,3,4,5],2)
  if cancel then return, -1
  cancel = mc_cpar('mc_mkspatprof2d',edgecoeffs, 2,'Edgecoeffs',[2,3,4,5],[2,3])
  if cancel then return, -1
  cancel = mc_cpar('mc_mkspatprof2d',xranges, 3,'Xranges',[2,3,4,5],[1,2])
  if cancel then return, -1
  cancel = mc_cpar('mc_mkspatprof2d',ybuffer, 4,'Ybuffer',[2,3],0)
  if cancel then return, -1
  cancel = mc_cpar('mc_mkspatprof2d',spatcal, 5,'Spatcal',[2,3,4,5],2)
  if cancel then return, -1

  if n_elements(atmosthresh) ne 0 then begin
          
     cancel = mc_cpar('mc_mkspatprof2d',wavecal,6,'Wavecal',[2,3,4,5],2)
     if cancel then return,-1
     cancel = mc_cpar('mc_mkspatprof2d',wtrans,7,'WTrans',[2,3,4,5],1)
     if cancel then return,-1
     cancel = mc_cpar('mc_mkspatprof2d',ttrans,8,'TTrans',[2,3,4,5],1)
     if cancel then return,-1
     cancel = mc_cpar('mc_mkspatprof2d',atmosthresh,9,'Atmosthresh',[2,3,4,5],0)
     if cancel then return,-1

  endif

;  Get basic info

  s = size(img,/DIMEN)
  ncols = s[0]
  nrows = s[1]
  y     = findgen(nrows)

  s = size(edgecoeffs)
  norders = (s[0] eq 2) ? 1:s[3]

  dobinsize = ~keyword_set(BINSIZE) 
  
  for i = 0,norders-1 do begin
     
     start   = xranges[0,i]
     stop    = xranges[1,i]
     numwave = fix(stop-start)+1
     x       = findgen(numwave)+start
     
;  Find the bottom and top of the slit and min slit height in pixels
     
     botedge   = poly1d(x,edgecoeffs[*,0,i])
     topedge   = poly1d(x,edgecoeffs[*,1,i])

; Create order mask (can't use one from mc_readflat because we
; need the ybuffer
     
     omask = intarr(ncols,nrows)
     for j = 0,stop-start do $
        omask[x[j],(botedge[j]+ybuffer):(topedge[j]-ybuffer)] = 1

     z = where(omask eq 1)

     if dobinsize then binsize = max(spatcal[z],/NAN)/max(topedge-botedge)

;  Histogram the spatcal array and use reverse indices to keep track
;  of which pixels fall into which bins.

     if n_elements(ATMOSPTHRESH) then begin

        linterp,wtrans,ttrans,wavecal[z],itrans
        good = where(itrans gt atmosthresh)
        z = z[good]

     endif 

     histy = histogram(spatcal[z],BINSIZE=binsize,/NAN,REVERSE=ri,$
                      LOCATIONS=histx)
     histx = histx+binsize/2.
     nbins = n_elements(histy)

;  Determine the median flux in each bin.

     prof = fltarr(nbins,/NOZERO)
     
     for j = 0,nbins-1 do begin

        idx = (ri[j] ne ri[j+1]) ? ri[ri[j]:(ri[j+1]-1)]:ri[ri[j]]

        prof[j] = median((img[z])[idx],/EVEN)

     endfor

;  Store the results in a structure

     key = 'Order'+string(i,format='(i2.2)')
     sprofiles = (i eq 0) ? create_struct(key,[[histx],[prof]]):$
                 create_struct(sprofiles,key,[[histx],[prof]])
     

  endfor

  return, sprofiles

end
