;+
; NAME:
;     mc_bgsubspec2d
;
; PURPOSE:
;     To subtract off the background of a 2D spectra image using a bspline.
;
; CALLING SEQUENCE:
;     mc_bgsubspec2d,img,var,wavecal,spatcal,edgecoeffs,xranges,ybuffer, $
;                        NORD=nord,NPOLY=npoly,UPPER=upper,LOWER=lower, $
;                        MODIMG=modimg,BGMASK=bgmask,,UPDATE=update, $
;                        WIDGET_ID=widget_id,CANCEL=cancel
;
; INPUTS:
;     img        - A 2D spectral image.
;     var        - A 2D variance image.
;     wavecal    - A 2D image where the value of each pixel is its
;                  wavelength.
;     spatcal    - A 2D image where the value of each pixel is its
;                  position along the slit (spatial coordinate).
;     edgecoeffs - Array [degree+1,2,norders] of polynomial coefficients 
;                  which define the edges of the orders.  array[*,0,0]
;                  are the coefficients of the bottom edge of the
;                  first order and array[*,1,0] are the coefficients 
;                  of the top edge of the first order.
;     xranges    - An array [2,norders] of pixel positions of the left
;                  and right columns of the order.
;     ybuffer    - Number of pixels to move inside of the edges of
;                  the orders since they are not infinitely sharp.
;                  Useful for avoiding weird edge effects.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     NORD       - Order for spline fit, default is 4 (see
;                  bspline_iterfit.pro)
;     NPOLY      - Polynomial order to fit over 2nd variable (X2);
;                  default to 2 (see bspline_iterfit.pro).
;     BKSPACE    - The break point spacing in units of wavecal.
;     UPPER      - Upper rejection threshhold; default to 5 sigma (see
;                  bspline_iterfit.pro).
;     LOWER      - Lower rejection threshhold; default to 5 sigma (see
;                  bspline_iterfit.pro).
;     MODIMG     - If requested, the model image.
;     BGMASK     - If given, only those pixels set to 1 will be used
;                  to estimate the background.
;     UPDATE    - If set, the program will launch the Fanning
;                 showprogress widget.
;     WIDGET_ID - If given, a cancel button is added to the Fanning
;                 showprogress routine.  The widget blocks the
;                 WIDGET_ID, and checks for for a user cancel
;                 command.
;     CANCEL     - Set on return if there is a problem.
;    
; OUTPUTS:
;     A background subtracted image.
;
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     None
;
; RESTRICTIONS:
;     Later
;
; DEPENDENCIES:
;     Later
;
; PROCEDURE:
;     Fits a bspline to the 2D data and subtractions it off.
;
; EXAMPLES:
;
;
; MODIFICATION HISTORY:
;     2009-11-14 - by M. Cushing, JPL.
;-
function mc_bgsubspec2d,img,var,wavecal,spatcal,edgecoeffs,xranges,ybuffer, $
                        NORD=nord,NPOLY=npoly,BKSPACE=bkspace,UPPER=upper, $
                        LOWER=lower,MODIMG=modimg,BGMASK=bgmask,UPDATE=update, $
                        WIDGET_ID=widget_id,CANCEL=cancel
  
  if n_params() ne 7 then begin

     print, 'Syntax - mc_bgsubspec2d(img,var,wavecal,spatcal,edgecoeffs,$'
     print, '                        xranges,ybuffer,NORD=nord,NPOLY=npoly,$'
     print, '                        BKSPACE=bkspace,UPPER=upper,$'
     print, '                        LOWER=lower,MODIMG=modimg,$'
     print, '                        BGMASK=bgmask,UPDATE=update,$'
     print, '                        WIDGET_ID=widget_id,CANCEL=cancel'
     cancel = 1
     return, -1

  endif

  cancel = 0

  s = size(img,/DIMEN)
  ncols = s[0]
  nrows = s[1]
  norders = n_elements(xranges[0,*])

  oimg = img

  if arg_present(MODIMG) then begin

     domod = 1
     modimg = fltarr(ncols,nrows)*!values.f_nan

  endif else domod = 0

  if arg_present(MASKIMG) then begin

     domask = 1
     maskimg = fltarr(ncols,nrows)*!values.f_nan


  endif else domask = 0

  if keyword_set(UPDATE) then begin

     if keyword_set(WIDGET_ID) then begin

        cancelbutton = (n_elements(WIDGET_ID) ne 0) ? 1:0
        progressbar = obj_new('SHOWPROGRESS',widget_id,COLOR=2,$
                              CANCELBUTTON=cancelbutton,$
                              MESSAGE='Subtracting Background')
        progressbar -> start
     
     endif

  endif

  for i =0,norders-1 do begin


;  Find top and bottom of slit.

     npts    = (xranges[1,i]-xranges[0,i])+1
     x       = indgen(npts)+xranges[0,i]
     botedge = poly1d(x,edgecoeffs[*,0,i])
     topedge = poly1d(x,edgecoeffs[*,1,i])

;  Create order (and BG) mask

     omask = intarr(ncols,nrows)
     for j = 0,xranges[1,i]-xranges[0,i] do $
        omask[x[j],(botedge[j]+ybuffer):(topedge[j]-ybuffer)] = 1

     if keyword_set(BGMASK) then begin

        z = where(omask eq 1 and bgmask eq 1)

     endif else z = where(omask eq 1)

;  Do the bspline fit

     sset = bspline_iterfit(float(wavecal[z]),float(img[z]), $
                            X2=float(spatcal[z]),INVVAR=1./float(var[z]), $
                            NPOLY=npoly,BKSPACE=bkspace,NORD=nord,UPPER=upper, $
                            LOWER=lower,OUTMASK=outmask)
     
;  Solve for the entire slit

     z = where(omask eq 1)
     
     yfit = bspline_valu(wavecal[z],sset,X2=spatcal[z])

     if domod then modimg[z] = yfit
     oimg[z] = oimg[z]-yfit

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

  if keyword_set(UPDATE) and keyword_set(WIDGET_ID) then begin
     
     progressbar-> destroy
     obj_destroy, progressbar
     
  endif


  return, oimg

end
