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
;                              PSBGINFO=psbginfo,XSBGINFO=xsbginfo,$
;                              BGDEG=bgdeg,UPDATE=update,WIDGET_ID=widget_id,$
;                              CANCEL=cancel)
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
;     PSBGINFO    - The background info for standard point source
;                   background definitions [bgstart,bgwidth].
;                   bgstart - The radius in arcseconds at which to start the
;                             background region definition (see mkmask_ps)
;                   bgwidth - The width of the background region in arcseconds
;                             (see mc_mkapmask1d.pro)
;     XSBGINFO    - (Background Regions) Array of background regions 
;                   in arcseconds ([[1,2],[13-15]]).
;     BGDEG       - Polynomial fit degree order of the background.  
;                   ***If omitted, then the background is not subtracted.***
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
function mc_sumextspec2d,img,var,omask,orders,wavecal,spatcal,ds,apsign, $
                         tracecoeffs,apradii,PSBGINFO=psbginfo, $
                         XSBGINFO=xsbginfo,BGDEG=bgdeg,SMAPINFO=smapinfo,$
                         PSFRADIUS=psfradius,UPDATE=update, $
                         WIDGET_ID=widget_id,CANCEL=cancel
  
  cancel = 0

;  Get necessary info

  norders = n_elements(orders)
  naps    = n_elements(apradii[*,0])

  s = size(img,/DIMEN)
  ncols = s[0]
  nrows = s[1]

  subbg = n_elements(BGDEG) 

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
     
;  Start with getting the pixels that fall within the order

     z = where(omask eq orders[i])

;  Find maximum values in various arrays for these pixels

     wmax = max(wavecal[z],MIN=wmin)
     smax = max(spatcal[z],MIN=smin)
     xmax = max(x[z],MIN=xmin)

;  Triangulate the wavecal and spatcal arrays

     triangulate,wavecal[z],spatcal[z],tri

;  Generate delta lambda based on the span of the order in pixels and
;  wavelengths. 

     dw = (wmax-wmin)/(xmax-xmin)
     
;  Carve up the slit

     ix = trigrid(wavecal[z],spatcal[z],x[z],tri,[dw,ds], $
                  [wmin,smin,wmax,smax],MISSING=!values.f_nan)
     
     iy = trigrid(wavecal[z],spatcal[z],y[z],tri,[dw,ds], $
                  [wmin,smin,wmax,smax],MISSING=!values.f_nan,$
                  XGRID=xgrid,YGRID=ygrid)

     nx = n_elements(xgrid)
     ny = n_elements(ygrid)
     
;  Trim the top and bottom few rows to get rid of NaNs

     ybuffer = 3
     ix = ix[*,ybuffer:ny-ybuffer-1]
     iy = iy[*,ybuffer:ny-ybuffer-1]
     ygrid = ygrid[ybuffer:ny-ybuffer-1]
     ny = ny-6
     
;  Now trim partial NaNs near the left and right edges due to the tilt
;  of the slit.

     ixnan = total(ix,2)
     zx = where(finite(ixnan) eq 1,xcnt)
     xgood = (xcnt eq 0) ? [0,n_elements(ixnan)-1]:[zx[0],zx[xcnt-1]]

     iynan = total(ix,1)
     zy = where(finite(iynan) eq 1,ycnt)
     ygood = (ycnt eq 0) ? [0,n_elements(iynan)-1]:[zy[0],zy[ycnt-1]]

     xgrid = xgrid[xgood[0]:xgood[1]]
     ygrid = ygrid[ygood[0]:ygood[1]]

     ix = ix[xgood[0]:xgood[1],ygood[0]:ygood[1]]
     iy = iy[xgood[0]:xgood[1],ygood[0]:ygood[1]]

;     a = obj_new('mcoplot')
;     a->set, ix,iy,PSYM=1,COLOR=3
;     ximgtool,img,OPLOT=a
;     return, -1

     nx = n_elements(xgrid)
     ny = n_elements(ygrid)

;  Create slit positions array

     slit_arc = (ygrid[1:*]+ygrid[0:ny-1])/2.

;  Start the actual extraction process

     owave = (xgrid+shift(xgrid,-1))/2.
     owave = owave[0:nx-2]

     ofspec = fltarr(nx-1,naps,/NOZERO)*!values.f_nan
     ovspec = fltarr(nx-1,naps,/NOZERO)*!values.f_nan

;  Generate the trace positions for each aperture in this order.

     trace_arc = fltarr(n_elements(owave),naps)
     
     for j = 0, naps-1 do trace_arc[*,j] = poly(owave,tracecoeffs[*,i*naps+j])

;  Now loop over each slit polygon

     for j = 0,nx-3 do begin

;  Concatinate the corners of each new pixel in the slit polygon to do
;  multiple polygons at once.  Loop for now, be creative later.
        
        px = 0
        py = 0
        
        for k = 0,ny-2 do begin

           px = [px,reform(ix[j,k:k+1]),reverse(reform(ix[j+1,k:k+1]))]
           py = [py,reform(iy[j,k:k+1]),reverse(reform(iy[j+1,k:k+1]))]

        endfor

        px = px[1:*]
        py = py[1:*]
        ri = lindgen(ny)*4

;  Polyclip the slit        

        inds = polyfillaa(px,py,ncols,nrows,AREAS=areas,POLY_INDICES=ri)

;  Reconstruct the spatial profile at this wavelength

        slit_flx = fltarr(ny-1,/NOZERO)*!values.f_nan
        slit_var = fltarr(ny-1,/NOZERO)*!values.f_nan
        
        for k =0,ny-2 do begin
           
           idx = inds[ri[k]:ri[k+1]-1]
           area = areas[ri[k]:ri[k+1]-1]

           slit_flx[k] = total(img[idx]*area)
           slit_var[k] = total(var[idx]*area)
           
        endfor

        slit_mask = mc_mkapmask1d(slit_arc,reform(trace_arc[j,*]), $
                                  reform(apradii[*,i]),PSBGINFO=psbginfo,$
                                  XSBGINFO=xsbgino,CANCEL=cancel)
        if cancel then return,-1

        if subbg then begin

           z = where(slit_mask eq -1 and slit_var ne 0.0,cnt)

           if cnt ge 2 then begin

              coeff  = robustpoly1d(slit_arc[z],slit_flx[z],bgdeg,4,0.1,$
                                    YERR=sqrt(slit_var[z]),/SILENT,$
                                    OGOODBAD=ogoodbad,COVAR=cvar,CANCEL=cancel)
              if cancel then return,-1

              bg = poly1d(slit_arc,coeff,cvar,YVAR=yvar,CANCEL=cancel)
              if cancel then return,-1

              slit_flx = temporary(slit_flx)-bg
              slit_var = temporary(slit_var)+yvar

           endif
           
        endif

;        plot,slit_arc,slit_flx,/XSTY,/YSTY,TITLE=owave[j]

        for k = 0,naps-1 do begin

;  Do standard extraction

           z = where(slit_mask gt float(k) and slit_mask le float(k+1))
;           oplot,slit_arc[z],slit_flx[z],PSYM=1,COLOR=2
           ofspec[j,k] = total(slit_flx[z]*(slit_mask[z]-float(k)))
           ovspec[j,k] = total(slit_var[z]*(slit_mask[z]-float(k))^2)
           
        endfor
;        re = ' '
;        read, re

     endfor

;     xzoomplot,owave,apsign[0]*ofspec[*,0]
;     return, -1

;  Store the results

     for j = 0,naps-1 do begin

        name = 'ORD'+string(orders[i],FORMAT='(I2.2)')+ $
               'AP'+string(j+1,FORMAT='(I2.2)')
        
        nonan = mc_nantrim(owave,2,CANCEL=cancel)
        if cancel then return,-1
        
        array = [[owave[nonan]], $
                 [float(apsign[j])*ofspec[nonan,j]],[sqrt(ovspec[nonan,j])]]

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

;  struc = 1
  return, struc

end
