;+
; NAME:
;     mc_sumextspec2d
;
; PURPOSE:
;     To perform a sum extraction in 2D.
;
; CALLING SEQUENCE:
;     result = mc_sumextspec2d(img,var,omask,orders,wavecal,spatcal,ds, $
;                              apsign,TRACECOEFFS=tracecoeffs,$
;                              APRADII=apradii,APRANGES=apranges,$
;                              UPDATE=update,WIDGET_ID=widget_id,CANCEL=cancel)
;
; INPUTS:
;     img     - A 2D image.
;     var     - A 2D variance image.
;     omask   - A 2D array where each pixel value is set to the 
;               order number.
;     orders  - An norders array 
;     wavecal - A 2D image where the value of each pixel is its
;               wavelength.
;     spatcal - A 2D image where the value of each pixel is its
;               position along the slit (spatial coordinate).
;     ds      - The sampling frequency (arcsec) in the spatial
;               dimension.
;     apsign  - Array of 1s and -1s indicating which apertures
;               are positive and which are negative (for IR pair
;               subtraction). 
;
; OPTIONAL INPUTS:
;     See keywords
;
; KEYWORD PARAMETERS:
;     TRACECOEFFS - Array [fitdegree+1,naps*norders] of polynomial 
;                   coefficients of the traces of the apertures.
;                   The coefficients should be indexed starting with 
;                   the bottom order and looping through the apertures
;                   in that order.
;     APRADII     - An [naps,norders] array giving the aperture radius
;                   for each aperture.
;     APRANGES    - An [naps*2,norders] array giving the left and
;                   right edges of the apertures.
;     UPDATE      - If set, the program will launch the Fanning
;                   showprogress widget.
;     WIDGET_ID   - If given, a cancel button is added to the Fanning
;                   showprogress routine.  The widget blocks the
;                   WIDGET_ID, and checks for for a user cancel
;                   command.
;     CANCEL      - Set on return if there is a problem.
;
; OUTPUTS:
;     A structure 
;
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     None
;
; RESTRICTIONS:
;     Lots
;
; DEPENDENCIES:
;     
;
; PROCEDURE:
;
;
; EXAMPLES:
;
;
; MODIFICATION HISTORY:
;
;-
function mc_sumextspec2d2,img,var,omask,orders,wavecal,spatcal,ds,apsign, $
                          TRACECOEFFS=tracecoeffs,APRADII=apradii, $
                          APRANGES=apranges,PSBGINFO=bginfo,XSBGINFO=xsbginfo,$
                          UPDATE=update,WIDGET_ID=widget_id,CANCEL=cancel

  cancel = 0

;  Get necessary info

  norders = n_elements(orders)

  if keyword_set(TRACECOEFFS) then naps = n_elements(APRADII[*,0])

  if keyword_set(APRANGES) then naps = n_elements(APRANGES[*,0])/2

  s = size(img,/DIMEN)
  ncols = s[0]
  nrows = s[1]

;  Create 2D x and y arrays

  x = rebin(indgen(ncols),ncols,nrows)
  y = rebin(reform(indgen(nrows),1,nrows),ncols,nrows)

  m = 0

  if keyword_set(UPDATE) then begin

     if keyword_set(WIDGET_ID) then begin

        cancelbutton = (n_elements(WIDGET_ID) ne 0) ? 1:0
        progressbar = obj_new('SHOWPROGRESS',widget_id,COLOR=2,$
                              CANCELBUTTON=cancelbutton,$
                              MESSAGE='Extracting Spectra')
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
     
;  Triangulate the wavecal and spatcal arrays

     triangulate,wavecal[z],spatcal[z],tri

;  Carve up the slit

     ix = trigrid(wavecal[z],spatcal[z],x[z],tri,[dw,ds], $
                  [wmin,smin,wmax,smax],MISSING=!values.f_nan)
     
     iy = trigrid(wavecal[z],spatcal[z],y[z],tri,[dw,ds], $
                  [wmin,smin,wmax,smax],MISSING=!values.f_nan,$
                  XGRID=xgrid,YGRID=ygrid)

;     a = obj_new('mcoplot')
;     a->set, ix,iy,PSYM=1,COLOR=3

;     ximgtool,img,OPLOT=a
;     cancel = 1
;     return, -1

     nx = n_elements(xgrid)
     ny = n_elements(ygrid)
     yy = findgen(ny)

     owave  = fltarr(nx,/NOZERO)*!values.f_nan
     ofspec = fltarr(nx,/NOZERO)*!values.f_nan
     ovspec = fltarr(nx,/NOZERO)*!values.f_nan

     slit_arc = (ygrid[1:*]+ygrid[0:ny-1])/2.

     for j = 0,naps-1 do begin
        
        for k = 10,nx-2 do begin

           owave[k]  = total(xgrid[k]+xgrid[k+1])/2.              
           apmask = intarr(ny-1)
           slit_flx = fltarr(ny-1,/NOZERO)*!values.f_nan
           slit_var = fltarr(ny-1,/NOZERO)*!values.f_nan

           
           for l = 0,ny-3 do begin

              px = [reform(ix[k,l+1:l+2]),reverse(reform(ix[k+1,l+1:l+2]))]
              py = [reform(iy[k,l+1:l+2]),reverse(reform(iy[k+1,l+1:l+2]))]
        
              inds=polyfillaa(px,py,ncols,nrows,AREAS=areas)

              if inds[0] ne -1 then begin

                 slit_flx[l] = total(img[inds]*areas)
                 slit_var[l] = total(var[inds]*areas)

              endif

           endfor

;  Subtract background

           

;           plot,slit_arc,slit_flx,/XSTY,/YSTY,TITLE=owave[k]
           bg = median(slit_flx,/EVEN)
;           plots,!x.crange,[bg,bg],COLOR=2
;           re = ' '
;           read, re

;           slit_flx = slit_flx-bg

           if n_elements(APRANGES) ne 0 then begin
              
              aprange = [apranges[j*2,i],apranges[j*2+1,i]]
              
           endif else begin
              
              cen = poly((xgrid[k]+xgrid[k+1])/2.,tracecoeffs[*,i*naps+j])
              aprange = [cen-apradii[j,i],cen+apradii[j,i]]
              
           endelse

           tabinv,slit_arc,aprange,apidx
           apmask[apidx[0]:apidx[1]] = 1
           
;  Fix endpoints to reflect fractional pixels.
     
           if apidx[0]-floor(apidx[0]) ge 0.5 then begin
              
              apmask[apidx[0]]   = 0
              apmask[apidx[0]+1] = (0.5 + round(apidx[0])-apidx[0]) 
              
           endif else begin
              
              apmask[apidx[0]] = (0.5 - (apidx[0]-floor(apidx[0]) ) ) 
              
           endelse

           if apidx[1]-floor(apidx[1]) ge 0.5 then begin
              
              apmask[apidx[1]+1] = (0.5 - (round(apidx[1])-apidx[1]) ) 
              
              
           endif else begin
              
              apmask[apidx[1]] = ( 0.5 + (apidx[1]-round(apidx[1])) ) 
              
           endelse

           z = where(apmask gt 0)


           ofspec[k] = total(slit_flx[z])
           ovspec[k] = total(slit_var[z])


;##########################

;  Don't erase 

;  polyclip the slit all at once

;           px = 0.0
;           py = 0.0
;           ri = 0
;           for m = -1,ny-3 do begin
;              
;              
;              px = [px,reform(ix[k,m+1:m+2]),reverse(reform(ix[k+1,m+1:m+2]))]
;              py = [py,reform(iy[k,m+1:m+2]),reverse(reform(iy[k+1,m+1:m+2]))]
;              ri = [ri,[0,3]+4*(m+1)]
;                            
;           endfor   
;
;           px = px[1:*]
;           py = py[1:*]
;           ri = ri[1:*]
;
;
;           inds=polyfillaa(px,py,ncols,nrows,AREAS=areas,POLY_INDICES=ri)
;
;###########################


        endfor

;  Store the results

        name = 'ORD'+string(orders[i],FORMAT='(I2.2)')+ $
               'AP'+string(j+1,FORMAT='(I2.2)')

        nonan = mc_nantrim(owave,2,CANCEL=cancel)
        if cancel then return,-1

;        mc_writespexfits,owave,apsign[j]*ofspec,'nosub.fits'
        

        array = [[owave[nonan]], $
                 [float(apsign[j])*ofspec[nonan]],[ovspec[nonan]]]

        struc = (m eq 0) ? $
                create_struct(name,array):create_struct(struc,name,array)

        m = m + 1
             

     endfor
;     
;;  Do the update stuff
;
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

  return, struc

end
