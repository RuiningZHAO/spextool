;+
; NAME:
;     mc_radprof
;
; PURPOSE:
;     To measure the radial profile of a star.
;
; CATEGORY:
;     Photometry
;
; CALLING SEQUENCE:
;     mc_radprof,img,xcen,ycen,dist,prof,XWIN=xwin,YWIN=ywin,CANCEL=cancel
;
; INPUTS:
;     img  - The 2D image.
;     xcen - The X centroid of the star.
;     ycen - The Y centroid of the star.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;
;   ***XWIN and YWIN don't seem to work...***
;
;     XWIN   - The total number of pixels around the xcen with which to
;              determine the radial profile.  Default is to use the
;              entire image.  If even, converted to odd.  
;     YWIN   - The total number of pixels around the ycen with which to
;              determine the radial profile.  Default is to use then the 
;              entire image.  If even, converted to odd.  
;     CANCEL - Set on return if there is a problem.
;
; OUTPUTS:
;     dist - A 1D array given the radial distance from xcen,ycen.
;     prof - The intensity values corresponding to each dist value.
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
;     Later
;
; EXAMPLE:
;     None
;
; MODIFICATION HISTORY:
;     2009-05-15 - Written by M. Cushing, Institute for Astronomy,
;                  Unversity of Hawaii
;-
pro mc_radprof,img,xcen,ycen,dist,prof,XWIN=xwin,YWIN=ywin,CANCEL=cancel

  cancel = 0

  if n_params() lt 3 then begin

     print, 'Syntax - mc_radprof,img,xcen,ycen,dist,prof,XWIN=xwin,$'
     print, '                    YWIN=ywin,CANCEL=cancel'
     cancel = 1
     return

  endif

  cancel = mc_cpar('mc_radprof',img,1,'Image',[2,3,4,5],2)
  if cancel then return
  cancel = mc_cpar('mc_radprof',xcen,2,'X Centroid',[2,3,4,5],0)
  if cancel then return
  cancel = mc_cpar('mc_radprof',xcen,3,'Y Centroid',[2,3,4,5],0)
  if cancel then return


  s     = size(img,/DIMEN)
  ncols = s[0]
  nrows = s[1]

  xrange = (n_elements(XWIN) ne 0) ? $
           [(xcen-xwin/2) > 0, (xcen+xwin/2) < (ncols-1)]:[0,ncols-1]

  yrange = (n_elements(YWIN) ne 0) ? $
           [(ycen-ywin/2) > 0, (ycen+ywin/2) < (nrows-1)]:[0,nrows-1]

  ncols = xrange[1]-xrange[0]+1
  nrows = yrange[1]-yrange[0]+1

  x = rebin(findgen(ncols),ncols,nrows)-xcen
  y = rebin(reform(findgen(nrows),1,nrows),ncols,nrows)-ycen

  dist = reform(sqrt(x^2 + y^2),ncols*nrows)

  prof = reform(img[xrange[0]:xrange[1],yrange[0]:yrange[1]],ncols*nrows)
  

end
