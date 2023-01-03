;+
; NAME:
;     mc_snipimgc
;
; PURPOSE:
;     To snip out a section of an image based on a xy center.
;
; CATEGORY:
;     Imaging
;
; CALLING SEQUENCE:
;     result = mc_snipimgc(img,xcen,ycen,xwidth,ywidth,[oncols],[onrows],$
;                          [oxrange],[oyrange],[oxyoffset],$
;                          BACKGROUND=background,CANCEL=cancel)
;
; INPUTS:
;     img    - A 2D image of 3D data cube.
;     xcen   - The x center of the subimage in whole pixels. 
;     ycen   - The y center of the subimage in whole pixels. 
;     xwidth - The width of the subimage (if even, the width becomes
;              width+1).
;     ywidth - The height of the subimage (if even, the height becomes 
;              height+1).
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     BACKGROUND - Set to the background value.  Useful if the
;                  subimage is not completely bounded by the main
;                  image.  Defaults to NaN.
;     CANCEL     - Set on return if there is a problem.
;
; OUTPUTS:
;     The submimage.
;
; OPTIONAL OUTPUTS:
;     oncols    - The number of columns of the subimg.
;     onrows    - The number of rows of the subimg.
;     oxrange   - A 2-element array give the xrange of the subimg.
;     oyrange   - A 2-element array give the yrange of the subimg.
;     oxyoffset - The offset of the lower left hand pixel of the
;                 clipped image from the [0,0] pixel of the subimg.
;                 Useful if the center is near the edge of the image.
;
; COMMON BLOCKS:
;     None
;
; SIDE EFFECTS:
;     None
;
; RESTRICTIONS:
;     Fractional pixels for xcen,ycen,xwidth,ywidth are converted to
;     integers using fix().
;
; PROCEDURE:
;     Duh.
;
; EXAMPLE:
;     
;
; MODIFICATION HISTORY:
;     2008-08-11 - Written by M. Cushing, Institute for Astronomy,
;                  University of Hawaii.
;-
function mc_snipimgc,img,xcen,ycen,xwidth,ywidth,oncols,onrows,oxrange, $
                     oyrange,oxyoffset,BACKGROUND=background,CANCEL=cancel

  cancel = 0
  
  if n_params() lt 5 then begin
     
     print, 'Syntax - result = mc_snipimgc(img,xcen,ycen,xwidth,ywidth,$'
     print, '                              [oncols],[onrows],[oxrange],$'
     print, '                              [oyrange],[oxyoffset],$'
     print, '                              BACKGROUND=background,CANCEL=cancel)'
     cancel = 1
     return, -1
     
  endif
  
  cancel = mc_cpar('mc_snipimgc',img,1,'Img',[1,2,3,4,5,12,13,14,15],[2,3])
  if cancel then return,-1
  cancel = mc_cpar('mc_snipimgc',xcen,2,'Xcen',[2,3,4,5,12,13,14,15],0)
  if cancel then return,-1
  cancel = mc_cpar('mc_snipimgc',ycen,3,'Ycen',[2,3,4,5,12,13,14,15],0)
  if cancel then return,-1
  cancel = mc_cpar('mc_snipimgc',xwidth,4,'Xwidth',[2,3,4,5,12,13,14,15],0)
  if cancel then return,-1
  cancel = mc_cpar('mc_snipimgc',ywidth,5,'Ywidth',[2,3,4,5,12,13,14,15],0)
  if cancel then return,-1

  mc_arrinfo,img,ndimen,dimen,CANCEL=cancel
  if cancel then return,-1
  
  bg = (n_elements(BACKGROUND) eq 0) ? !values.f_nan:background
  
  ncols = dimen[0]
  nrows = dimen[1]

  nimgs = (ndimen eq 3) ? dimen[2]:1

  half = fix([xwidth,ywidth])/2

  oxrange = [fix(xcen)-half[0],fix(xcen)+half[0]]
  oyrange = [fix(ycen)-half[1],fix(ycen)+half[1]]

  sxrange = 0 > oxrange < (ncols-1)
  syrange = 0 > oyrange < (nrows-1)

  sxsize = sxrange[1]-sxrange[0]
  sysize = syrange[1]-syrange[0]
  
  oxyoffset = [sxrange[0]-oxrange[0],syrange[0]-oyrange[0]]

  simg = fltarr(half[0]*2+1,half[1]*2+1,nimgs)+bg

  simg[oxyoffset[0]:(oxyoffset[0]+sxsize), $
       oxyoffset[1]:(oxyoffset[1]+sysize),*] = $
     img[sxrange[0]:sxrange[1],syrange[0]:syrange[1],*]

  simg = reform(temporary(simg))
  s = size(simg,/DIMEN)
  oncols = s[0]
  onrows = s[1]

  return, simg


end


