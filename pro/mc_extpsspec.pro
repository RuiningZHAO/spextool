;+
; NAME:
;     mc_extpsspec
;
; PURPOSE:
;     (Optimally) extracts point source spectra from a XD spectral image.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = mc_extpsspec(image,var,edgecoeffs,tracecoeffs,norders,$
;                           naps,xmin,xmax,xranges,slith_arc,apradii,$
;                           apsign,PSFWIDTH=psfwidth,$
;                           SPATCOEFFS=spatcoeffs,$
;                           BGORDER=bgorder,PSBGINFO=psbginfo,$
;                           XSBGINFO=xsbginfo,STDSPECTRA=stdspectra,$
;                           BITMASK=bitmask,BDPIXTHRESH=bdpixthresh,$
;                           BGSUBIMG=bgsubimg,UPDATE=update,$
;                           WIDGET_ID=widget_id,CANCEL=cancel)
;
; INPUTS:
;     image       - A 2-D image with spectra to be extracted
;     var         - The 2-D variance image
;     edgecoeffs  - Array [degree+1,2,norders] of polynomial coefficients 
;                   which define the edges of the orders.  array[*,0,0]
;                   are the coefficients of the bottom edge of the
;                   first order and array[*,1,0] are the coefficients 
;                   of the top edge of the first order.
;     tracecoeffs - Array [fitdegree+1,naps*norders] of polynomial 
;                   coefficients of the traces of the apertures.
;                   The coefficients should be indexed starting with 
;                   the bottom order and looping through the apertures
;                   in that order.
;     norders     - The number of orders
;     naps        - The number of apertures
;     xmin        - The minimum xrange value for ALL orders, not just
;                   those being extracted.
;     xmax        - The maximum xrange value for ALL orders, not just
;                   those being extracted.
;     xranges     - An array [2,norders] of pixel positions where the
;                   orders are completely on the array
;     slith_arc   - Slit length in arcsecs.
;     apradii     - Array of aperture radii in arcseconds
;     apsign      - Array of 1s and -1s indicating which apertures
;                   are positive and which are negative (for IR pair
;                   subtraction). 
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     SPATCOEFF - A structure will norders elements each of which will 
;                 contain an array of coefficients for each "row" of
;                 the spatial map.
;     PSBGINFO  - The background info for standard point source
;                 background definitions [bgstart,bgwidth].
;                 bgstart - The radius in arcseconds at which to start the
;                           background region definition (see mkmask_ps)
;                 bgwidth - The width of the background region in arcseconds
;                           (see mkmask_ps)
;     XSBGINFO  - (Background Regions) Array of background regions 
;                 in arcseconds ([[1,2],[13-15]]).
;     PSFWIDTH  - The radius at which the profile goes to zero.
;     BGORDER   - Polynomial fit degree order of the background.  If
;                 omitted, then the background is not subtracted.
;     BGSUBIMG  - The background subtracted image 
;     UPDATE    - If set, the program will launch the Fanning
;                 showprogress widget.
;     WIDGET_ID - If given, a cancel button is added to the Fanning
;                 showprogress routine.  The widget blocks the
;                 WIDGET_ID, and checks for for a user cancel
;                 command.
;     CANCEL    - Set on return if there is a problem
;
; OUTPUTS:
;     Returns an (stop-start+1,naps,norders) array of spectra
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
;
; MODIFICATION HISTORY:
;     2000-08-24 - Written by M. Cushing, Institute for Astronomy, UH
;     2001-10-04 - Added xranges input
;     2002-11-20 - Added optimal extraction.  Heavily modified.
;     2005-05-02 - Added the BGSUBIMG keyword
;     2005-10-10 - Modified background subtraction for BG degrees of
;                  zero to avoid bad pixel clusters when using small
;                  numbers of BG pixels
;     2007-07-16 - Removed start and stop input
;     2008-08-11 - Removed BGSTART and BGWIDTH keywords and replaced
;                  with PSBGINFO.  Also added XSBGINFO keyword.
;     2009-11-02 - Added the xmin and xmax inputs.
;     2015-01-05 - Added BITMASK keyword and removed BDPXMK keyword.

;-
function mc_extpsspec,image,var,wavecal,edgecoeffs,tracecoeffs,norders,$
                      naps,xranges,slith_arc,apradii,apsign,$
                      PSFWIDTH=psfwidth,SPATCOEFFS=spatcoeffs,BGORDER=bgorder, $
                      PSBGINFO=PSBGINFO,XSBGINFO=xsbginfo,BDPXMASK=bdpxmask,$
                      BITMASK=bitmask,BDPIXTHRESH=bdpixthresh,UPDATE=update,$
                      WIDGET_ID=widget_id,CANCEL=cancel

  cancel = 0
  
  debug = 0
  debugbgsub = 0
  debugfndbdpx = 0
  debugopt = 0
  debugxrange = [1000,1400]
  
;  Get image info
  
  s       = size(image)
  ncols   = s[1]
  nrows   = s[2]
  
  y          = findgen(nrows)
  
  optextract = (n_elements(PSFWIDTH) ne 0) ? 1:0
  fixbdpx    = (n_elements(SPATCOEFFS) ne 0 and optextract eq 0) ? 1:0

  if n_elements(BDPXMASK) eq 0 then bdpxmask = intarr(ncols,nrows)+1
  
;  Create linearity max mask 
  
  linmask = mc_bitset(bitmask,0,CANCEL=cancel)
  if cancel then return,-1

;  Set up debugging window.
  
  if debug or debugbgsub or debugfndbdpx or debugopt then begin
     
     window, /FREE
     wid = !d.window
     re = ' '
     
  endif

;  Set up Fanning update object.
  
  if keyword_set(UPDATE) then begin
     cancelbutton = (n_elements(WIDGET_ID) ne 0) ? 1:0
     progressbar = obj_new('SHOWPROGRESS',widget_id,COLOR=2,$
                           CANCELBUTTON=cancelbutton,$
                           MESSAGE='Extracting Spectra...')
     progressbar -> start
     
  endif
  
;  Start order loop

  m = 0
  
  for i = 0, norders-1 do begin

     start = xranges[0,i]
     stop = xranges[1,i]

     x = findgen(stop-start+1)+start
     
     pixtoarc = fltarr(stop-start+1,2)
     
;  Find edge of slit, traces, or pixel-to-arcsecond transformation
     
     bot = poly(x,edgecoeffs[*,0,i]) 
     top = poly(x,edgecoeffs[*,1,i]) 

     owave = wavecal[x,(bot+top)/2.]
     
     ofspec = fltarr(n_elements(owave),naps,/NOZERO)*!values.f_nan
     oespec = fltarr(n_elements(owave),naps,/NOZERO)*!values.f_nan
     obspec = fltarr(n_elements(owave),naps)
     
     trace = fltarr(n_elements(owave),naps)
     
     for j = 0, naps-1 do begin
        
        l = i*naps+j
        trace[*,j] = poly(owave,tracecoeffs[*,l])
        
     endfor
     
     pixtoarc[*,1] = float(slith_arc) / (top-bot) 
     pixtoarc[*,0] = -1.* (pixtoarc[*,1] * bot)
     
;  Create spatial map
     
     if optextract or fixbdpx then begin
        
        nspats = n_elements(spatcoeffs.(i*2))
        spatmap = fltarr(stop-start+1,nspats)
        for j = 0,nspats-1 do spatmap[*,j] = poly(owave,spatcoeffs.(i*2+1)[*,j])
        
        spaty_arc = spatcoeffs.(i*2)

     endif
     
;  Loop over each column 
     
     for j = 0,stop-start do begin
        
;  Create the slit and psf mask
        
        if x[j] lt xranges[0,i] or x[j] gt xranges[1,i] then goto, cont2
        if top[j] gt nrows-0.5 or bot[j] lt -0.5 then goto, cont2
        
        slity_pix = y[bot[j]:top[j]]
        slity_arc = poly(slity_pix,pixtoarc[j,*])
        slitz     = reform(image[x[j],bot[j]:top[j]])
        vslitz    = reform(var[x[j],bot[j]:top[j]])
        bdpxmkz   = bdpxmask[x[j],bot[j]:top[j]]
        linmkz    = linmask[x[j],bot[j]:top[j]]
        trace_arc = reform(trace[j,*])
        
        mask = mc_mkapmask1d(slity_arc,trace_arc,apradii,$
                             PSBGINFO=psbginfo,XSBGINFO=xsbginfo,CANCEL=cancel)
        if cancel then return, -1
        
        if optextract then begin

           psfmask = mc_mkapmask1d(slity_arc,trace_arc, $
                                   replicate(psfwidth,naps),CANCEL=cancel)
           if cancel then return,-1

        endif
        
        if debug and x[j] ge debugxrange[0] and x[j] le debugxrange[1] then $
           begin
           
           plot, slity_arc,slitz,/xsty,/ysty,PSYM=10, $
                 XRANGE=[0,slith_arc],TITLE='Column '+strtrim(x[j],2)
           read, re
           if re eq 'q' then debug = 0

        endif
        
;  Subtract the background from the slit if requested
        
        if n_elements(BGORDER) ne 0 then begin
        
           z = where(mask eq -1 and vslitz ne 0.0,cnt)
           if cnt ge 2 then begin
              
;  Find outliers including the bad pixels
              
              mc_moments,slitz[z],mean,vvar,/SILENT,IGOODBAD=bdpxmkz[z], $
                         OGOODBAD=ogoodbad,ROBUST=5,CANCEL=cancel
              if cancel then return,-1
              
;  Now fit the background ignoring these pixels
              
              coeff  = robustpoly1d(slity_arc[z],slitz[z],bgorder,4,0.1,$
                                    YERR=sqrt(vslitz[z]),/SILENT,$
                                    IGOODBAD=ogoodbad, $
                                    OGOODBAD=ogoodbad,COVAR=cvar)
              
              if debug and debugbgsub and x[j] ge debugxrange[0] and $
                 x[j] le debugxrange[1] then begin
                 
                 plot, slity_arc,slitz,/xsty,/ysty,PSYM=10, $
                       XRANGE=[0,slith_arc],TITLE='BG Sub Window-Column '+$
                       strtrim(x[j],2)
                 plots,slity_arc[z],slitz[z],PSYM=2,COLOR=2
                 junk = where(ogoodbad eq 0,cnt)
                 if cnt ne 0 then plots,slity_arc[z[junk]], $
                                        slitz[z[junk]],COLOR=3,SYMSIZE=2, $
                                        PSYM=4
                 oplot,slity_arc,poly(slity_arc,coeff),COLOR=6
                 read, re
                 if re eq 'q' then debug = 0
                 
              endif
              
              slitz  = temporary(slitz)-poly1d(slity_arc,coeff,cvar,YVAR=yvar)
              vslitz = temporary(vslitz)+yvar
                 
           endif
           
        endif
        
;  Scale the superprofile and find bad pixels
        
        if optextract or fixbdpx then begin

;  Do a sinc interpolation as it is better than linear.
           
           sprofile = mc_sincinterp(spaty_arc,reform(spatmap[j,*]), $
                                    slity_arc,CANCEL=cancel)
           if cancel then return,-1
           scoeff = robustpoly1d(sprofile,slitz,1,bdpixthresh,0.1,/SILENT,$
                                 OGOODBAD=ogoodbad,IGOODBAD=bdpxmkz,$
                                 CANCEL=cancel)
;  Debug plotter
           
           if debug and debugfndbdpx and x[j] gt debugxrange[0] and x[j] lt $
              debugxrange[1] then begin
              
              plot, slity_arc,slitz,/XSTY,PSYM=10,$
                    XRANGE=[0,slith_arc],/YSTY,$
                    TITLE='Bad Pixel Window, Column - '+strtrim(x[j],2)
              
              oplot,slity_arc,poly(sprofile,scoeff),psym=10,COLOR=2
              z = where(ogoodbad eq 0 or bdpxmkz eq 0,cnt)
              if cnt ne 0 then oplot,slity_arc[z],slitz[z],COLOR=4,psym=2,$
                                       SYMSIZE=2
              read, re
              if re eq 'q' then debug = 0              

           endif
           
        endif 
        
;  Fix bad pixels if requested and not optimal extraction
        
        if not optextract and fixbdpx then begin
           
           scsprofile = poly(sprofile,scoeff)
           
           coeff = robustpoly1d(abs(sprofile),vslitz,1,bdpixthresh,0.1,$
                                /SILENT,CANCEL=cancel)
           scvprofile = poly(abs(sprofile),coeff)

           badpix = where(ogoodbad eq 0 or bdpxmkz eq 0,count)

           if count ne 0 then begin
              
              slitz[badpix]  = scsprofile[badpix]
              vslitz[badpix] = scvprofile[badpix]
              
           endif

        endif
        
;  Extract the spectrum
        
        for k = 0, naps-1 do begin
           
           l = i*naps+k
           
;  Enforce positivity and renormalize.
           
           if optextract then begin
              
              zpsf = where(psfmask gt float(k) and psfmask le float(k+1),junk)

              psprofile = (apsign[k] eq 1) ? $
                          (sprofile[zpsf]>0.0):(sprofile[zpsf]<0.0)

              
              psprofile = apsign[k]*abs(psprofile/total(psprofile,/NAN))
              sprofile[zpsf] = psprofile

;  Scale data values
              
              goodbad   = ogoodbad*bdpxmkz
              zslit     = where(mask gt float(k) and mask le float(k+1) and $
                                sprofile ne 0.0 and goodbad eq 1,count)
              
              if count ne 0 then begin
                 
                 be_slitz  = slitz[zslit]/sprofile[zslit]
                 be_vslitz = vslitz[zslit]/sprofile[zslit]^2

                 junk = where(linmkz[zslit] eq 1,cnt)
                 if cnt ne 0 then obspec[j,k] = obspec[j,k] +1 
                 
              endif else begin

                 be_slitz  = slitz*0.0
                 be_vslitz = slitz/slitz
                 obspec[j,k] = obspec[j,k] + 8
                 print, 'Warning:  Optimal extraction failed at column '+$
                        strtrim(x[j],2)
                 
              endelse

;  Do optimal extraction and get flags
           
              mc_meancomb,be_slitz,mean,mvar,DATAVAR=be_vslitz
              ofspec[j,k] = mean
              oespec[j,k] = sqrt(mvar)

              
;  Debug plotter
              
              if debug and debugopt and optextract and $
                 x[j] ge debugxrange[0] and $
                 x[j] le debugxrange[1] then begin
                 
                 apmask = where(mask gt float(k) and mask le float(k+1))
                 
                 ymin = min(slitz[zslit]/sprofile[zslit],MAX=ymax)
                 plot,slity_arc[zpsf],slitz[zpsf]/sprofile[zpsf],/XSTY,$
                      /NODATA,TITLE='Optimal Extraction Window, Column '+ $
                      strtrim(x[j],2)+' Aperture '+ $
                      strtrim(k+1,2),YRANGE=[ymin,ymax]
                 oploterr,slity_arc[zslit],slitz[zslit]/sprofile[zslit],$
                          sqrt(vslitz[zslit]/sprofile[zslit]^2),6
                 read, re
                 if re eq 'q' then debug = 0                 

              endif
              
           endif else begin
           
;  Do standard extraction and get flags
           
              z = where(mask gt float(k) and mask le float(k+1))
              ofspec[j,k] = float(apsign[k])*total(slitz[z]*$
                                                       (mask[z]-float(k)))
              oespec[j,k] = sqrt(total(vslitz[z]*(mask[z]-float(k))^2))

              junk = where(linmkz[z] eq 1,cnt)
              if cnt ne 0 then obspec[j,k] = obspec[j,k] + 1

              if fixbdpx then begin
              
                 goodbad = ogoodbad*bdpxmkz
                 
                 junk = where(goodbad eq 0 and $
                              mask gt float(k) and mask le float(k+1),cnt)
                 if cnt ne 0 then obspec[j,k] = obspec[j,k] + 2

              endif
              
           endelse
           
        endfor
        
        cont2:
        
     endfor

;  Store the results

     nonan = mc_nantrim(owave,2,CANCEL=cancel)
     if cancel then return,-1
     
     for k = 0,naps-1 do begin
        
        name = 'ORD'+string(i+1,FORMAT='(I2.2)')+ $
               'AP'+string(k+1,FORMAT='(I2.2)')
        
        array = [[owave[nonan]],$
                 [ofspec[nonan,k]],$
                 [oespec[nonan,k]],$
                 [obspec[nonan,k]]]
        
        struc = (m eq 0) ? $
                create_struct(name,array):create_struct(struc,name,array)
        
        m = m + 1
        
     endfor

     
     if keyword_set(UPDATE) then begin
        
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
        
     endif
     
  endfor
  
  if debugbgsub or debugfndbdpx or debugopt then wdelete, wid
  
  done:

  if keyword_set(UPDATE) then begin
     
     progressbar-> destroy
     obj_destroy, progressbar
     
  endif
  
  return, struc
  
end
