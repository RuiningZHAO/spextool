;+
; NAME:
;     mc_mkspatprof2d
;
; PURPOSE:
;     To construct average spatial profiles.
;
; CALLING SEQUENCE:
;     result = mc_mkspatprof2d(img,omask,wavecal,spatcal,ds,[wtrans],$
;                              [ttrans],[atmosthresh],SPATCOEFFS=spatcoeffs,$
;                              MEDSPATCOEFFS=medspatcoeffs,UPDATE=update,$
;                              WIDGET_ID=widget_id,CANCEL=cancel)
;
; INPUTS:
;     img         - a 2D image.
;     omask       - A 2D image of the same size as img that gives the
;                   order number of each pixel.
;     wavecal     - A 2D image of the same size as img that gives the
;                   wavelength of each pixel.
;     spatcal     - A 2D image of the same size as img that gives the
;                   spatial coordinates of each pixel.
;     ds          - The spatial spacing of the resampling slit in
;                   arcsec, typically given by slith_arc/slith_pix.
;     ybuffer     - The number of pixels to ignore near the top and
;                   bottom of the slit.  
;
; OPTIONAL INPUTS:
;     wtrans      - An 1D array of wavelengths for the atmospheric transmission.
;     ttrans      - A 1D array of the the atmospheric transmission.
;     atmosthresh - The transmission (0-1) below which data is ignored.
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem.
;
; OUTPUTS: 
;     Returns a structure with norders fields containing the median
;     spatial profiles.  Each field is a 2D array where array[*,0] is 
;     contains the x values in arcseconds and array[*,1] are the
;     intensity values.  .
;
; OPTIONAL OUTPUTS:
;     SPATCOEFFS    - A structure with 2*norders elements containing the
;                     the information necessary to construct the spatial
;                     map.  The first field of the two fields is a 1D
;                     array  [nspat] containing the values of the
;                     spatial  coordinate.  The second field contains
;                     an  array [3, nspat] of polynomial cofficients.
;                     The coefficients are a function of wavelength
;                     (whether it be actually units or pixels).
;     MEDSPATCOEFFS - Same as SPATCOEFFS except that the polynomial
;                     coefficients give the median spatial profile
;                     instead of a wavelength dependent profile.
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
;     Each order is resampled onto a uniform grid.  The median
;     backgrouns is subtracted on a column by column basis.  The
;     median spatial profile is then created.  If the user passes
;     wtrans, ttrans, atmosthresh, then pixels that have atmospheric
;     transmission below atmosthresh are ignored.  The median spatial
;     profile is then used to normalize the resampling image.
;     2D polynomial coefficients are then derived on a row by row basis.     
;
; EXAMPLES:
;     NA     
;
; MODIFICATION HISTORY:
;     2009-12-03 - Written by M. Cushing, NASA JPL
;-
function mc_mkspatprof2d,img,omask,wavecal,spatcal,ds,ybuffer,wtrans,ttrans, $
                         atmosthresh,SPATCOEFFS=spatcoeffs, $
                         MEDSPATCOEFFS=medspatcoeffs,UPDATE=update, $
                         WIDGET_ID=widget_id,CANCEL=cancel

  debug = 0
  cancel = 0

  if n_params() lt 6 then begin

     print, 'Syntax - mc_mkspatprof2d(img,omask,wavecal,spatcal,ds,ybuffer,$'
     print, '                         [wtrans],[ttrans],[atmosthresh],$'
     print, '                         SPATCOEFFS=spatcoeffs,$'
     print, '                         MEDSPATCOEFFS=medspatcoeffs,$'
     print, '                         UPDATE=update,WIDGET_ID=widget_id,$'
     print, '                         CANCEL=cancel)'
     cancel = 1
     return, -1

  endif

  cancel = mc_cpar('mc_mkspatprof2d',img, 1,'Wcinfo',[2,3,4,5],2)
  if cancel then return, -1
  cancel = mc_cpar('mc_mkspatprof2d',omask, 2,'Omask',[2,3,4,5],2)
  if cancel then return, -1
  cancel = mc_cpar('mc_mkspatprof2d',wavecal, 3,'Wavecal',[2,3,4,5],2)
  if cancel then return, -1
  cancel = mc_cpar('mc_mkspatprof2d',spatcal, 4,'Spatcal',[2,3,4,5],2)
  if cancel then return, -1
  cancel = mc_cpar('mc_mkspatprof2d',ds, 5,'ds',[2,3,4,5],0)
  if cancel then return, -1
  cancel = mc_cpar('mc_mkspatprof2d',ybuffer, 6,'ybuffer',[2,3,4,5],0)
  if cancel then return, -1

  if n_elements(atmosthresh) ne 0 then begin
          
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

  if debug then begin
     
     window, /FREE
     wid = !d.window
     re = ' '
     
  endif
  
;  Create 2D x and y arrays

  x = rebin(indgen(ncols),ncols,nrows)
  y = rebin(reform(indgen(nrows),1,nrows),ncols,nrows)

  orders = (omask[uniq(omask,sort(omask))])[1:*]
  norders = n_elements(orders)

  if keyword_set(UPDATE) then begin
     
     if keyword_set(WIDGET_ID) then begin
        
        cancelbutton = (n_elements(WIDGET_ID) ne 0) ? 1:0
        progressbar = obj_new('SHOWPROGRESS',widget_id,COLOR=2,$
                              CANCELBUTTON=cancelbutton,$
                              MESSAGE='Creating Spatial Profile')
        progressbar -> start
        
     endif
     
  endif

;  Loop over each order

  for i = 0,norders-1 do begin

     z = where(omask eq orders[i])
     
     wmax = max(wavecal[z],MIN=wmin)
     smax = max(spatcal[z],MIN=smin)
     xmax = max(x[z],MIN=xmin)
     
     dw = (wmax-wmin)/(xmax-xmin)
     
;  Triangulate the wavecal and spatcal arrays and derive indices for a
;  a new grid 
     
     triangulate,wavecal[z],spatcal[z],tri

     ix = trigrid(wavecal[z],spatcal[z],x[z],tri,[dw,ds], $
                  [wmin,smin,wmax,smax],MISSING=!values.f_nan)
     
     iy = trigrid(wavecal[z],spatcal[z],y[z],tri,[dw,ds], $
                  [wmin,smin,wmax,smax],MISSING=!values.f_nan,$
                  XGRID=xgrid,YGRID=ygrid)

     nx = n_elements(xgrid)
     ny = n_elements(ygrid)

;  Resample onto new grid

     
;     a = obj_new('mcoplot')
;     a->set,ix,iy,PSYM=1,COLOR=3
;
;     ximgtool,img,RANGE='99.5%',OPLOT=a
;     re = ' '
;     read, re

     rectorder = interpolate(img,ix,iy,MISSING=!values.f_nan,CUBIC=-0.5)

;     re = ' '
;     read, re
     
;  Trim ybuffer pixels off the top and bottom

     ygrid = ygrid[ybuffer:ny-1-ybuffer]
     rectorder = rectorder[*,ybuffer:ny-1-ybuffer]
     ny = ny-ybuffer*2
     
;  Subtract background

     bg = median(rectorder,DIMEN=2)
     rectorder = temporary(rectorder)-rebin(bg,nx,ny)

     tmp = rectorder

;  Mask out atmosphere

     if n_elements(wtrans) ne 0 then begin

        mask = fltarr(nx)+1.0
        linterp,wtrans,ttrans,xgrid,rtrans,MISSING=1
        z = where(rtrans lt atmosthresh,cnt)
        if cnt ne 0 then mask[z] = !values.f_nan

        rectorder = temporary(rectorder)*rebin(mask,nx,ny)

     endif

;  Compute median spatial profile
     
     prof = median(rectorder,DIMEN=1)
     prof = prof/total(abs(prof),/NAN)

;  Store the results in a structure

     key = 'Order'+string(orders[i],FORMAT='(i2.2)')
     sprofiles = (i eq 0) ? create_struct(key,[[ygrid],[prof]]):$
                 create_struct(sprofiles,key,[[ygrid],[prof]])

;  Now create the old Spextool 1D profile coefficients for 1D
;  extraction

     ndeg = 4
     coeff  = fltarr(ndeg+1,ny)
     acoeff = fltarr(ndeg+1,ny)

;  Determine normalization factors and normalize the image
        
     meanmap   = rebin(reform(prof,1,ny),nx,ny)
     norms     = median(rectorder/meanmap,/EVEN,DIMENSION=2)
     rectorder = temporary(rectorder)/rebin(norms,nx,ny)  

;  Now fit each row with a polynomial

     for j = 0,ny-1 do begin
        
        c = robustpoly1d(xgrid,rectorder[*,j],ndeg,5,0.1,/GAUSSJ,/SILENT,$
                         CANCEL=cancel)
        if cancel then c = replicate(!values.f_nan,ndeg+1)
        
        coeff[*,j] = c

        if debug then begin

           plot,xgrid,rectorder[*,j],/XSTY,/YSTY,PSYM=1
           oplot,xgrid,poly(xgrid,c),COLOR=2
           read, re
           if re eq 'q' then debug = 0

        endif

     endfor

;  Create the structure that hold the coefficients

     key = 'Order'+string(i,format='(i2.2)')
     spatcoeffs = (i eq 0) ? create_struct(key+'Y',ygrid,key+'C',coeff):$
                  create_struct(spatcoeffs,key+'Y',ygrid,key+'C',coeff)
     
;  Also store the average profile in the same fashion
     
     acoeff[0,*] = prof 
     
     key = 'Order'+string(i,format='(i2.2)')
     medspatcoeffs = (i eq 0) ? create_struct(key+'Y',ygrid,key+'C',acoeff):$
                     create_struct(medspatcoeffs,key+'Y',ygrid,key+'C',acoeff)


;  Do the update stuff

     if keyword_set(UPDATE) then begin
        
        if keyword_set(WIDGET_ID) then begin
           
           if cancelbutton then begin
              
              cancel = progressBar->CheckCancel()
              if cancel then begin
                 
                 progressBar->Destroy
                 obj_destroy, progressbar
                 cancel = 1
                 return, -1
                 
              endif
              
           endif
           percent = (i+1)*(100./float(norders))
           progressbar->update, percent
           
        endif else begin
           
           if norders gt 1 then mc_loopprogress,i,0,norders-1
           
        endelse
        
     endif

  endfor
  
  out:
  if keyword_set(UPDATE) and keyword_set(WIDGET_ID) then begin
     
     progressbar-> destroy
     obj_destroy, progressbar
     
  endif

  if cancel then return, -1 else return, sprofiles

end
