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
function mc_extspec2d,img,var,omask,orders,wavecal,spatcal,ds,apsign, $
                      tracecoeffs,apradii,PSBGINFO=psbginfo,XSBGINFO=xsbginfo, $
                      BGDEG=bgdeg,SMAPINFO=smapinfo,PSFRADIUS=psfradius, $
                      UPDATE=update,WIDGET_ID=widget_id,CANCEL=cancel
  
  cancel = 0

;  Debugging flags

  range = [000,300]
  debugbgsub = 0
  debugsclprof = 0
  debugopt = 0
  debugsum = 0

;  Get necessary info

  norders = n_elements(orders)
  naps    = n_elements(apradii[*,0])

  s = size(img,/DIMEN)
  ncols = s[0]
  nrows = s[1]

  subbg  = n_elements(BGDEG) 
  optext = keyword_set(PSFRADIUS)
  fixpix = keyword_set(SMAPINFO)	

;  Create 2D x and y arrays

  x = rebin(indgen(ncols),ncols,nrows)
  y = rebin(reform(indgen(nrows),1,nrows),ncols,nrows)

  m = 0

;  Make debug window if needed

  if debugbgsub or debugsclprof or debugopt or debugsum then begin
     
     window, /FREE
     wid = !d.window
     re = ' '
     
  endif

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
     
;  Carve up the order 

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

;  Now trim to avoid the edge of the arrays     

     xmask = (ix le 2) + (ix ge (ncols-2))
     xmask = total(xmask,2)
     zx = where(xmask eq 0,xcnt)
     xgood = [zx[0],zx[xcnt-1]]

     ymask = (iy le 2) + (iy ge (nrows-2))
     ymask = total(ymask,1)
     zy = where(ymask eq 0,ycnt)
     ygood = [zy[0],zy[ycnt-1]]

     xgrid = xgrid[xgood[0]:xgood[1]]
     ygrid = ygrid[ygood[0]:ygood[1]]

     ix = ix[xgood[0]:xgood[1],ygood[0]:ygood[1]]
     iy = iy[xgood[0]:xgood[1],ygood[0]:ygood[1]]
     

     nw = n_elements(xgrid)-1
     ns = n_elements(ygrid)-1

;  Create slit positions and wavelength array

     slit_arc = (ygrid+ds/2.)[0:ns-1]
     owave    = (xgrid+dw/2.)[0:nw-1]

;  Start the extraction process

     ofspec = fltarr(nx,naps,/NOZERO)*!values.f_nan
     ovspec = fltarr(nx,naps,/NOZERO)*!values.f_nan

;  Generate the trace positions for each aperture in this order.

     trace_arc = fltarr(nw,naps)
     
     for j = 0, naps-1 do trace_arc[*,j] = poly(owave,tracecoeffs[*,i*naps+j])

;  Create spatial map if required

     if optext or fixpix then begin

        spatmap_yarc = smapinfo.(i*2)
        nspat = n_elements(spatmap_yarc)

        spatmap = fltarr(nw,nspat)

        for j = 0,nspat-1 do spatmap[*,j] = poly(owave,smapinfo.(i*2+1)[*,j])
     
     endif

;  Now loop over each slit polygon

     for j = 0,nw-1 do begin
        
;  Concatinate the corners of each new pixel in the slit polygon to do
;  multiple polygons at once.  Loop over each pixel for now, be
;  creative later.
        
        px = 0
        py = 0

;        ns = 1
        for k = 0,ns-1 do begin

           px = [px,reform(ix[j,k:k+1]),reverse(reform(ix[j+1,k:k+1]))]
           py = [py,reform(iy[j,k:k+1]),reverse(reform(iy[j+1,k:k+1]))]

        endfor

        px = px[1:*]+0.5
        py = py[1:*]+0.5
        

        ri = lindgen(ns+1)*4

;  (the +0.5 above is because J.D. should be killed for indexing
;  pixels at the lower left hand corner instead of the middle)

;  Polyclip the slit        

;        a = obj_new('mcoplot')
;        a->set,px,py,PSYM=1,COLOR=3
;        ximgtool,img,OPLOT=a,ZOOM=4,XYPOS=[2000,990]
;        re = ' '
;        read, re
;
;        if j eq 1700 then begin
;
;           cancel = 1
;           return, -1
;
;        endif

        inds = polyfillaa(px,py,ncols,nrows,AREAS=areas,POLY_INDICES=ri)

;  Reconstruct the spatial profile at this wavelength

        slit_flx = fltarr(ny,/NOZERO)*!values.f_nan
        slit_var = fltarr(ny,/NOZERO)*!values.f_nan

        for k =0,ns-1 do begin
           
           idx = inds[ri[k]:ri[k+1]-1]
           area = areas[ri[k]:ri[k+1]-1]

           slit_flx[k] = total(img[idx]*area)
           slit_var[k] = total(var[idx]*area)
           
        endfor

;  Generate the aperture and background slit mask

        slitmask = mc_mkapmask1d(slit_arc,reform(trace_arc[j,*]), $
                                 reform(apradii[*,i]),PSBGINFO=psbginfo,$
                                 XSBGINFO=xsbgino,CANCEL=cancel)
        if cancel then goto, out
        
        if optext then begin
           
           psfmask = mc_mkapmask1d(slit_arc,reform(trace_arc[j,*]), $
                                   replicate(psfradius,naps),CANCEL=cancel)
           if cancel then goto, out
           
        endif

;  Subtract the background

        if subbg then begin


           z = where(slitmask eq -1.0 and slit_var,cnt)
           if cnt ge 2 then begin

              coeff  = robustpoly1d(slit_arc[z],slit_flx[z],bgdeg,4,0.1,$
                                    YERR=sqrt(slit_var[z]),/SILENT,$
                                    OGOODBAD=ogoodbad,COVAR=cvar,CANCEL=cancel)
              if cancel then goto, out

;  Debug plotter

;              if debugbgsub then begin
              if debugbgsub and j gt range[0] and j lt range[1] then begin
                 
                 plot,slit_arc,slit_flx,/XSTY,/YSTY,PSYM=10,$
                      XTITLE='Spatial Position (arcsec)',$
                      YTITLE='Relative Flux',$
                      TITLE='Background Subtraction Fit'+string(j)+' '+$
                      string(owave[j])+' '+string(median(slit_flx))
                 oplot,slit_arc[z],slit_flx[z],PSYM=2,COLOR=2
                 oplot,slit_arc,poly(slit_arc,coeff),COLOR=6
                 read, re
                 
              endif

              bg = poly1d(slit_arc,coeff,cvar,YVAR=yvar,CANCEL=cancel)
              if cancel then goto, out

              slit_flx = temporary(slit_flx)-bg
              slit_var = temporary(slit_var)+yvar

              if debugbgsub and j gt range[0] and j lt range[1] then begin
                 
                 plot,slit_arc,slit_flx,/XSTY,/YSTY,PSYM=10,$
                      XTITLE='Spatial Position (arcsec)',$
                      YTITLE='Relative Flux',$
                      TITLE='Background Subtracted Slit'+string(j)+' '+$
                      string(owave[j])+' '+string(median(slit_flx))
                 plots,!x.crange,[0,0],LINESTYLE=1
                 read, re
                 
              endif



           endif else begin

              print, 'No background subtracted at '+strtrim(owave[j],2)+ $
                     ' microns.'

           endelse
           
        endif

;  Scale the superprofile and find bad pixels
        
        if optext or fixpix then begin
           
           sprofile = mc_sincinterp(spatmap_yarc,reform(spatmap[j,*]), $
                                    slit_arc,CANCEL=cancel)
           if cancel then goto, out
           
           scoeff = robustpoly1d(sprofile,slit_flx,1,6,0.1,/SILENT,$
                                 OGOODBAD=ogoodbad,CANCEL=cancel)
           if cancel then goto, out
           
;  Debug plotter
           
           if debugsclprof then begin

              plot, slit_arc,slit_flx,/XSTY,/YSTY,PSYM=10,$
                    XTITLE='Spatial Position (arcsec)',$
                    YTITLE='Relative Flux',$
                    TITLE='Profile Scaling'+string(k)+string(owave[j])
              
              oplot,slit_arc,poly(sprofile,scoeff),psym=10,COLOR=2
              z = where(ogoodbad eq 0,cnt)
              if cnt ne 0 then oplot,slit_arc[z],slit_flx[z],COLOR=4, $
                                     PSYM=2,SYMSIZE=2
              
              read, re
              if re eq 'q' then begin

                 cancel = 1
                 goto, out

              endif
              
           endif

        endif 

;  Fix bad pixels if requested and not optimal extraction

        if not optext and fixpix then begin

           scsprofile = poly(sprofile,scoeff)
           
           coeff = robustpoly1d(abs(sprofile),slit_var,1,6,0.1,/SILENT,CANCEL=cancel)
           scvprofile = poly(abs(sprofile),coeff)
           
           badpix = where(ogoodbad eq 0,count)
           
           if count ne 0 then begin
              
              slit_flx[badpix]  = scsprofile[badpix]
              slit_var[badpix] = scvprofile[badpix]
              
           endif
           
        endif

;  Extract spectra

        for k = 0,naps-1 do begin

           if optext then begin

              zpsf = where(psfmask gt float(k) and psfmask le float(k+1),junk)
              
;  Enforce positivity

              psprofile = (apsign[k] eq 1) ? $
                          (sprofile[zpsf]>0.0):(sprofile[zpsf]<0.0)

;  Normalize the profile

              psprofile = apsign[k]*abs(psprofile/total(psprofile,/NAN))
              sprofile[zpsf] = psprofile

;  Generate an array of estimates of the total flux

              goodbad   = ogoodbad
              zslit     = where(slitmask gt float(k) and $
                                slitmask le float(k+1) and $
                                sprofile ne 0.0 and goodbad eq 1,cnt)        
                   
              if cnt ne 0 then begin
                 
                 flux     = slit_flx[zslit]/sprofile[zslit]
                 flux_var = slit_var[zslit]/sprofile[zslit]^2
                 
                 mc_meancomb,flux,mean,mvar,DATAVAR=flux_var
                 ofspec[j,k] = mean
                 ovspec[j,k] = mvar

              endif else begin
                 
                 ofspec[j,k] = !values.f_nan
                 ovspec[j,k] = !values.f_nan
                 print, 'Warning:  Optimal extraction failed at wavelength '+$
                        strtrim(owave[j],2)+'.'
                 
              endelse

           endif else begin

;  Standard extraction

              z = where(slitmask gt float(k) and slitmask le float(k+1))

              if debugsum and j gt range[0] and j lt range[1] then begin

                 plot,slit_arc,slit_flx,/XSTY,/YSTY,PSYM=10,$
                      XTITLE='Spatial Position (arcsec)',$
                      YTITLE='Relative Flux',$
                      YRANGE=[-0.2,0.2],$
                      TITLE='Background Subtraction Fit'+string(owave[j])+' '+$
                      string(apsign[k]*total(slit_flx[z]* $
                                             (slitmask[z]-float(k))))
                 plots,!x.crange,[0,0],LINESTYLE=1
                 
                 oplot,slit_arc[z],slit_flx[z],PSYM=1,COLOR=2
                 read, re
                 
              endif

              z = where(slitmask gt float(k) and slitmask le float(k+1))
              ofspec[j,k] = apsign[k]*total(slit_flx[z]*(slitmask[z]-float(k)))
              ovspec[j,k] = total(slit_var[z]*(slitmask[z]-float(k))^2)
              
           endelse
           
        endfor

     endfor

;  Store the results

     for j = 0,naps-1 do begin

        tag = 'ORD'+string(orders[i],FORMAT='(I2.2)')+'AP'+ $
              string(j+1,FORMAT='(I2.2)')
        
        nonan = mc_nantrim(owave,2,CANCEL=cancel)
        if cancel then goto, out
        
        array = [[owave[nonan]],[ofspec[nonan,j]],[sqrt(ovspec[nonan,j])]]

        struc = (m eq 0) ? $
                create_struct(tag,array):create_struct(struc,tag,array)
        
        m = m + 1
        
     endfor

;     
;  Do the update stuff
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
  
  out:

  if keyword_set(UPDATE) and keyword_set(WIDGET_ID) then begin
     
     progressbar-> destroy
     obj_destroy, progressbar

  endif 

  if cancel then return, -1 else return, struc

end
