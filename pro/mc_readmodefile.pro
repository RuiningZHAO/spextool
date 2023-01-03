;+
; NAME:
;     mc_readmodefile
;
; PURPOSE:
;     Reads an extraction mode calibration file for Spextool.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = mc_readmodefile(filename,CANCEL=cancel)
;
; INPUTS:
;     filename - The full path of a Spextool calibration file
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     result - a structure where the tag if each field is given below:
;
;              modename        - A string giving the mode name.
;              rotation        - The IDL rotation direction 
;              norders         - The number of orders in the image.
;              orders          - An array of the order numbers.
;              ps              - Plate scale in arcsec pix-1
;              slith_pix       - The slit height in pixels. 
;              slith_pix_range - The slit height range in pixels. 
;              slith_arc       - The slit height in arcseconds.
;              rppix           - The resolving power times the slit width in
;                                pixels for any given slit in this mode.  Used
;                                to scale to other slit widths.
;              step            - The step size in the dispersion direction 
;                                (see findorders) 
;              flatfrac        - The fraction value used to find the edges of
;                                the orders (see findorders.pro)
;              comwin          - The center-of-mass searching window.
;              edgedeg         - The polynomial degree of the edges of
;                                the  orders.
;              norm_nxg        - The number of grid squares in the x
;                                direction used
;                                to removed the scattered light
;              norm_nyg        - The number of grid squares in the y
;                                direction used
;                                to removed the scattered light
;              oversamp        - The over sampling factor when it straightens
;                                each order
;              ybuffer         - The number of pixels to move inside the edges
;                                of the orders since the edges are not
;                                inifitely sharp
;              fixed           - If 'Yes' then the guesspos are row
;                                numbers  of the edge of the order.
;                                If 'No',  then they are guess positions.
;              guesspos        - An (2,norders) array of positions of
;                                the  orders on the array.
;              findrange       - An (2,norders) array of xranges to search for
;                                the orders
;              type            - The type of wavelength calibration, 1D or 2D.
;              homeorder       - The order to scale all lines to for the
;                                multi-order wavelength calibration.
;              dispdeg         - The polynomial degree to fit in the dispersion
;                                dimension.
;              ordrdeg         - The polynomial degree to fit in the order
;                                dimension. 
;              linedeg         - The polynomial degree to fit the curvature of
;                                a spectral line.
;              linereg         - The number of pixels around a spectral line to
;                                fit to a gaussian.
;              ystep           - The number of pixels to step in the Y
;                                direction to trace out the position of the
;                                line.
;              ysum            - The number of rows to sum in order to increase
;                                the S/N of the emission line.
;              wxdeg           - The degree of the polynomial function to fit
;                                wavelength = f(x,y) in the X direction.
;              wydeg           - The degree of the polynomial function to fit
;                                wavelength = f(x,y) in the Y direction.
;              sxdeg           - The degree of the polynomial function to fit
;                                spatial = f(x,y) in the X direction.
;              sydeg           - The degree of the polynomial function to fit
;                                spatial = f(x,y) in the Y direction.
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
;     2000-05-10 - Written by M. Cushing, Institute for Astronomy, UH
;     2002-09-01 - Added slith_pix_range parameter
;     2003-01-23 - Removed SG stuff and added fiterpolate stuff,
;                  yubuffer
;     2007-04-02 - Added findrange output variable.
;     2007-07-13 - Added flatfrac input 
;     2007-07-14 - Added step input
;     2007-07-25 - Removed start and stop inputs.
;     2007-07-25 - Added comwin input
;     2008-02-22 - Added rppix input.
;     2009-12-16 - Added linedeg,linereg,ystep,ysum,wxdeg,wydeg,sxdeg,sydeg
;     2010-08-08 - Changed to a function, changed output to a
;                  structure, added type, homeorder, dispdeg, and
;                  ordrdeg output, and renamed to mc_readmodfile.
;-
function mc_readmodefile,filename,CANCEL=cancel

  cancel = 0
  
;  Check parameters
  
  if n_params() lt 1 then begin
     
     print, 'Syntax - result = mc_readmodefile(filename,CANCEL=cancel)'
     cancel = 1
     return, -1
     
  endif
  
  cancel = mc_cpar('mc_readmodefile',filename,1,'Filename',7,0)
  if cancel then return,-1
  
  readcol,filename,type,val,COMMENT='#',DELIMITER='=',FORMAT='A,A',/SILENT

  idx = 0
  modename = strtrim(val[0],2)
  idx = idx+1

  rotation = fix(val[idx])
  idx = idx+1

  orders = fix(mc_fsextract(val[idx],/INDEX,CANCEL=cancel))
  if cancel then return,-1
  norders = n_elements(orders)
  idx = idx+1

  ps = float(val[idx])
  idx = idx+1

  slith_pix = fix(val[idx])
  idx = idx+1

  slith_pix_range = fix(strsplit(val[idx],' ',/EXTRACT) )
  idx = idx+1

  slith_arc = float(val[idx])
  idx = idx+1

  rpppix = float(val[idx])
  idx = idx+1

  step = fix(val[idx])
  idx = idx+1

  flatfrac = float(val[idx])
  idx = idx+1

  comwin = fix(val[idx])
  idx = idx+1

  edgedeg = fix(val[idx])
  idx = idx+1

  norm_nxg = fix(val[idx])
  idx = idx+1

  norm_nyg = fix(val[idx])
  idx = idx+1

  oversamp = float(val[idx])
  idx = idx+1

  norm_ybuffer = fix(val[idx])
  idx = idx+1

  fixed = strtrim(val[idx],2)
  idx = idx+1

  guesspos = intarr(2,norders)

  for i = 0, norders-1 do begin
     
     guesspos[*,i] = fix( strsplit(val[idx],' ',/EXTRACT))
     idx = idx+1

  endfor

  findrange = intarr(2,norders)
  
  for i = 0, norders-1 do begin
     

     findrange[*,i] = fix( strsplit(val[idx],' ',/EXTRACT) )   
     idx = idx+1

  endfor

  type = strtrim(val[idx],2)
  idx = idx+1
  
  homeorder = fix(val[idx])
  idx = idx+1
  
  dispdeg = fix(val[idx])
  idx = idx+1

  ordrdeg = fix(val[idx])
  idx = idx+1

  linedeg = fix(val[idx])
  idx = idx+1

  linereg = fix(val[idx])
  idx = idx+1

  ystep = fix(val[idx])
  idx = idx+1

  ysum = fix(val[idx])
  idx = idx+1

  wxdeg = fix(val[idx])
  idx = idx+1

  wydeg = fix(val[idx])
  idx = idx+1

  sxdeg = fix(val[idx])
  idx = idx+1

  sydeg = fix(val[idx])
  idx = idx+1  

  mode = {modename:modename,rotation:rotation,orders:orders,ps:ps,$
          slith_pix:slith_pix,slith_pix_range:slith_pix_range,$
          slith_arc:slith_arc,rpppix:rpppix,step:step,flatfrac:flatfrac,$
          comwin:comwin,edgedeg:edgedeg,norm_nxg:norm_nxg,$
          norm_nyg:norm_nyg,oversamp:oversamp,norm_ybuffer:norm_ybuffer,$
          fixed:fixed,guesspos:guesspos,findrange:findrange,type:type,$
          homeorder:homeorder,dispdeg:dispdeg,ordrdeg:ordrdeg,$
          linedeg:linedeg,linereg:linereg,ystep:ystep,ysum:ysum,$
          wxdeg:wxdeg,wydeg:wydeg,sxdeg:sxdeg,sydeg:sydeg}

  return, mode


end
