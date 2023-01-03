;+
; NAME:
;     mc_readflat
;
; PURPOSE:
;     Reads a Spextool flat field FITS image.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     mc_readflat,flatname,image,ncols,nrows,modename,ps,slith_pix,$
;                 slith_arc,slitw_pix,slitw_arc,rp,norders,orders,edgecoeffs,$
;                 xranges,rms,rotation,edgedeg,VARIANCE=variance,FLAGS=flags,$
;                 OMASK=omask,CANCEL=cancel
;
; INPUTS:
;     flatname - A string of the flat-flat name
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS: 
;     OMASK - If given, an image where each pixel value is set to the 
;             order number is returned.
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     image      - An array containing the image.
;     ncols      - Number of columns in the image.
;     nrows      - Number of rows in the image.
;     modename   - The observing mode of the flat.
;     ps         - The plate scale in arcseconds per pixel
;     slith_pix  - The approximate slit height in pixels.
;     slith_arc  - The slit height in arcsecs.
;     slitw_pix  - The slit width in pixels.
;     slitw_arc  - The slit width in arcsecs.
;     rp         - The resolving power
;     norders    - Number of orders on the array.
;     orders     - An array of the order numbers.
;     edgecoeffs - Array [degree+1,2,norders] of polynomial coefficients 
;                  which define the edges of the orders.  array[*,0,0]
;                  are the coefficients of the bottom edge of the
;                  first order and array[*,1,0] are the coefficients 
;                  of the top edge of the first order.
;     xranges    - An array [2,norders] of pixel positions where the
;                  orders are completely on the array
;     rms        - An array [norders] of rms deviations for each order
;     rotation   - The IDL rotation command (ROTATE)
;     edgedeg    - The degree of the edge coefficients
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
;     Easy
;
; EXAMPLE:
; 
; MODIFICATION HISTORY:
;     2000-04-11 - Written by M. Cushing, Institute for Astronomy, UH
;     2001-10-04 - Added xranges input and removed start and stop
;     2001-11-05 - Rotation output added.
;     2004-01-29 - Added the edgedeg output.
;                  2008-02-22 - Changed name to mc_readflat and added
;                               rp (resolving power) input
;     2009-11-09 - Added omask keyword.
;     2010-08-11 - Added ps output.
;     2014-05-28 - Modified to read new MEF file.
;     2015-01-03 - Added variance and flag keywords.
;-
pro mc_readflat,flatname,image,ncols,nrows,modename,ps,slith_pix,$
                slith_arc,slitw_pix,slitw_arc,rp,norders,orders,edgecoeffs,$
                xranges,rms,rotation,edgedeg,VARIANCE=variance,FLAGS=flags,$
                OMASK=omask,CANCEL=cancel

  cancel = 0

;  Check parameters

  if n_params() lt 1 then begin
     
     print, 'Syntax - mc_readflat,flatname,image,ncols,nrows,modename,ps,$'
     print, '                     slith_pix,slith_arc,slitw_pix,slitw_arc,rp,$'
     print, '                     norders,orders,edgecoeffs,xranges,rms,$'
     print, '                     rotation,edgedeg,VARIANCE=variance,$'
     print, '                     FLAGS=flags,OMASK=omask,CANCEL=cancel'
     cancel = 1
     return
     
  endif
  cancel = mc_cpar('mc_readflat',flatname,1,'Filename',7,0)
  if cancel then return
  
  junk = mrdfits(flatname,0,hdr,/SILENT)
  
  image    = mrdfits(flatname,1,/SILENT)
  variance = mrdfits(flatname,2,/SILENT)
  flags    = mrdfits(flatname,3,/SILENT)
  
;  Get hdr info.
  
  norders   = fxpar(hdr,'NORDERS')
  fitorder  = fxpar(hdr,'EDGEDEG')
  start     = fxpar(hdr,'START')
  stop      = fxpar(hdr,'STOP')
  ps        = fxpar(hdr,'PLTSCALE')
  slith_arc = fxpar(hdr,'SLTH_ARC')
  slith_pix = fxpar(hdr,'SLTH_PIX')
  slitw_arc = fxpar(hdr,'SLTW_ARC')
  slitw_pix = fxpar(hdr,'SLTW_PIX')
  rp        = fxpar(hdr,'RP')
  rotation  = fxpar(hdr,'ROTATION') 
  edgedeg   = fxpar(hdr,'EDGEDEG')
  modename  = strtrim(fxpar(hdr, 'MODENAME'),2)
  orders    = long( strsplit( fxpar(hdr,'ORDERS'), ',', /EXTRACT) )

;  Rotate image and get ncols and nrows

  image    = rotate(temporary(image),rotation)
  variance = rotate(temporary(variance),rotation)
  flags    = rotate(temporary(flags),rotation)
  
  s = size(image,/DIMEN)

  ncols = s[0]
  nrows = s[1]
  
  edgecoeffs = dblarr(fitorder+1,2,norders)
  rms        = fltarr(norders)
  xranges    = intarr(2,norders)
  omask      = intarr(ncols,nrows)

  for i = 0,norders-1 do begin
     
     name_T = 'ODR'+string(orders[i],FORMAT='(i2.2)')+'_T*'
     name_B = 'ODR'+string(orders[i],FORMAT='(i2.2)')+'_B*'
     
     coeff_T = fxpar(hdr,name_T)
     coeff_B = fxpar(hdr,name_B)
     
     edgecoeffs[*,1,i] = coeff_T
     edgecoeffs[*,0,i] = coeff_B
     
     name         = 'ODR'+string(orders[i],FORMAT='(i2.2)')+'_XR'
     xranges[*,i] = long( strsplit( fxpar(hdr,name), ',', /EXTRACT) )
     
     name = 'ODR'+string(orders[i],FORMAT='(i2.2)')+'RMS'
     rmss = fxpar(hdr,name)
     rms[i] = rmss

     if arg_present(omask) then begin
     
        x       = indgen(xranges[1,i]-xranges[0,i]+1)+xranges[0,i]
        botedge = poly1d(x,edgecoeffs[*,0,i])
        topedge = poly1d(x,edgecoeffs[*,1,i])
        
        for j = 0,xranges[1,i]-xranges[0,i] do begin
          
           if topedge[j] gt nrows-0.5 or botedge[j] lt -0.5 then continue
           omask[x[j],botedge[j]:topedge[j]] = orders[i]       
           
        endfor
        
     endif
     
  endfor
  


  
end



