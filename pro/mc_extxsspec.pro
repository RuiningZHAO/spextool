;+
; NAME:
;     mc_extxsspec
;
; PURPOSE:
;     Extracts extended source spectra.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = mc_extxsspec(image,var,edgecoeffs,tracecoeffs,norders,naps,$
;                           xmin,xmax,xranges,slith_arc,apradii,BGR=bgr,$
;                           BGORDER=bgorder,SPATCOEFFS=spatcoeffs,$
;                           BDPIXTHRESH=bdpixthresh,BGSUBIMG=bgsubimg,$
;                           UPDATE=update,WIDGET_ID=widget_id,CANCEL=cancel)
;
; INPUTS:
;     image       - A 2-D image with spectra to be extracted
;     var         - The variance image
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
;     slith_arc   - Slit length in arcseconds
;     apradii     - Array of aperture radii in arcseconds
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     BGR       - (Background Regions) Array of background regions 
;                 in arcseconds ([[1,2],[13-15]]).
;     BGORDER   - Polynomial fit degree order of the background.  If
;                 omitted, then the background is not subtracted.
;     BGSUBIMG  - The background subtracted image
;     UPDATE    - If set, the program will launch the Fanning
;                 showprogress widget.
;     WIDGET_ID - If given, a cancel button is added to the Fanning
;                 showprogress routine.  The widget blocks the
;                 WIDGET_ID, and checks for for a user cancel
;                 command.
;     CANCEL    - Set on return if there is a problem.
;
; OUTPUTS:
;     Returns an (stop-start+1,naps*norders) array of spectra.
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
;     2000-08-24 - written by M. Cushing, Institute for Astronomy, UH
;     2000-11-13 - Modified to accept multiple background regions.
;     2001-10-04 - Added xranges input
;     2002-11-30 - Added bad pixel fixing
;     2005-05-02 - Added the BGSUBIMG keyword
;     2007-07-16 - Removed start and stop inputs
;     2009-11-02 - Added xmin and xmax (start and stop!) inputs
;-
function mc_extxsspec,image,var,wavecal,edgecoeffs,tracecoeffs,norders,naps, $
                      xranges,slith_arc,apradii,BGR=bgr,$
                      BGORDER=bgorder,SPATCOEFFS=spatcoeffs,$
                      BDPXMASK=bdpxmask,BITMASK=bitmask, $
                      BDPIXTHRESH=bdpixthresh,ERRORPROP=errorprop, $
                      UPDATE=update,WIDGET_ID=widget_id,CANCEL=cancel
  
  debugbgsub = 0
  debugfndbdpx = 0
  debugxrange = [550,580]
  
;  Check parameters
  
;  cancel = 0
;  if n_params() lt 11 then begin
;     
;     print, 'Syntax - result = mc_extxsspec(image,var,edgecoeffs,$'
;     print, '                               tracecoeffs,norders,naps,$'
;     print, '                               xmin,xmax,xranges,slith_arc,$'
;     print, '                               apradii,BGR=bgr,BGORDER=bgorder,$'
;     print, '                               SPATCOEFFS=spatcoeffs,$'
;     print, '                               ERRORPROP=errorprop,$'
;     print, '                               BDPXMK=bdpxmk,UPDATE=update,$'
;     print, '                               WIDGET_ID=widget_id,CANCEL=cancel'
;     cancel = 1
;     return, -1
;     
;  endif
;  cancel = mc_cpar('mc_extxsspec',image,1,'Image',[2,3,4,5],2)
;  if cancel then return,-1
;  cancel = mc_cpar('mc_extxsspec',var,2,'Var',[2,3,4,5],2)
;  if cancel then return,-1
;  cancel = mc_cpar('extracspec_xs',edgecoeffs,3,'Edgecoeffs',[2,3,4,5],[2,3])
;  if cancel then return,-1
;  cancel = mc_cpar('mc_extxsspec',tracecoeffs,4,'Tracecoeffs',[2,3,4,5],[1,2])
;  if cancel then return,-1
;  cancel = mc_cpar('mc_extxsspec',norders, 5,'Norders',[2,3,4,5],0)
;  if cancel then return,-1
;  cancel = mc_cpar('mc_extxsspec',naps, 6,'Naps',[2,3,4,5],0)
;  if cancel then return,-1
;  cancel = mc_cpar('mc_extxsspec',xranges, 7,'Xranges',[2,3,4,5],[1,2])
;  if cancel then return,-1
;  cancel = mc_cpar('mc_extpsspec',xmin, 8,'Xmin',[2,3,4,5],0)
;  if cancel then return,-1
;  cancel = mc_cpar('mc_extpsspec',xmax, 9,'Xmax',[2,3,4,5],0)
;  if cancel then return,-1
;  cancel = mc_cpar('mc_extxsspec',slith_arc, 10,'Slith_arc',[2,3,4,5],0)
;  if cancel then return,-1
;  cancel = mc_cpar('mc_extxsspec',apradii, 11,'Apradii',[2,3,4,5],[0,1])
;  if cancel then return,-1
  
;  Get info
  
  s     = size(image)
  ncols = s[1]
  nrows = s[2]
  
  y       = findgen(nrows)
  
  fixbdpx = (n_elements(SPATCOEFFS) ne 0) ? 1:0
  
  if n_elements(BDPXMASK) eq 0 then bdpxmask = intarr(ncols,nrows)+1
  
  if debugbgsub or debugfndbdpx then begin
     
     window, /FREE
     wid = !d.window
     re = ' '
     
  endif

;  Create linearity max mask 
  
  linmask = mc_bitset(bitmask,0,CANCEL=cancel)
  if cancel then return,-1
  
;  Set up the Fanning showprogress object if requested.
  
  if keyword_set(UPDATE) then begin
     cancelbutton = (n_elements(WIDGET_ID) ne 0) ? 1:0
     progressbar = obj_new('SHOWPROGRESS',widget_id,COLOR=2,$
                           CANCELBUTTON=cancelbutton,$
                           MESSAGE='Extracting Spectra...')
     progressbar -> start
     
  endif

  m = 0
  
  for i = 0, norders-1 do begin
     
     start = xranges[0,i]
     stop = xranges[1,i]

     x = findgen(stop-start+1)+start

     pixtoarc  = fltarr(stop-start+1,2)
     
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
     
;  create spatial map
     
     if fixbdpx then begin
        
        nspats = n_elements(spatcoeffs.(i*2))
        spatmap = fltarr(stop-start+1,nspats)
        for j = 0,nspats-1 do spatmap[*,j] = poly(owave,spatcoeffs.(i*2+1)[*,j])
        
        spaty_arc = spatcoeffs.(i*2)
        
     endif
     
     for j = 0,stop-start do begin
        
        if x[j] lt xranges[0,i] or x[j] gt xranges[1,i] then goto, cont2
        if top[j] gt nrows-0.5 or bot[j] lt -0.5 then goto, cont2
        
        low       = 0 > bot[j]
        high      = nrows-1 < top[j]
        slity_pix = y[low:high]
        slity_arc = poly(slity_pix,pixtoarc[j,*])
        slitz     = reform(image[x[j],low:high])
        vslitz    = reform(var[x[j],low:high])
        bdpxmkz   = bdpxmask[x[j],bot[j]:top[j]]
        linmkz    = linmask[x[j],bot[j]:top[j]] 
        trace_arc = reform(trace[j,*])
        
        mask = mc_mkapmask1d(slity_arc,reform(trace_arc),apradii, $
                             XSBGINFO=bgr,CANCEL=cancel)
        if cancel then return, -1
        if n_elements(BGORDER) ne 0 then begin

           z = where(mask eq -1 and vslitz ne 0.0)
           
;  Find outliers including the bad pixels
           
           mc_moments,slitz[z],mean,vvar,/SILENT,IGOODBAD=bdpxmkz[z],$
                      OGOODBAD=ogoodbad,ROBUST=5,CANCEL=cancel
           if cancel then return,-1
           
;  Now fit the background ignoring these pixels
           
           coeff = robustpoly1d(slity_arc[z],slitz[z],bgorder,4,0.1,$
                                IGOODBAD=ogoodbad,YERR=sqrt(vslitz[z]), $
                                /SILENT,COVAR=cvar)
           
           if debugbgsub and x[j] ge debugxrange[0] and $
              x[j] le debugxrange[1] then begin
              
              plot, slity_arc,slitz,/xsty,/ysty,PSYM=10, $
                    XRANGE=[0,slith_arc],TITLE='BG Sub Window-Column '+$
                    strtrim(x[j],2)
              plots,slity_arc[z],slitz[z],PSYM=2,COLOR=2
              junk = where(ogoodbad eq 0,cnt)
              if cnt ne 0 then plots,slity_arc[z[junk]], $
                                     slitz[z[junk]],COLOR=3,SYMSIZE=2,PSYM=4
              
              oplot,slity_arc,poly(slity_arc,coeff),COLOR=6
              read, re
              
           endif
           
           if keyword_set(ERRORPROP) then begin
              
              slitz  = temporary(slitz)-poly1d(slity_arc,$
                                               coeff,cvar,YVAR=yvar)
              vslitz = temporary(vslitz)+yvar
              
           endif else begin
              
              slitz  = temporary(slitz)-poly1d(slity_arc,coeff)
              vslitz[*] = 1.0
              
           endelse
           
        endif
        
;  Scale the superprofile and fix bad pixels

        if fixbdpx then begin

           linterp,spaty_arc,reform(spatmap[j,*]),slity_arc,sprofile
           
           coeff = robustpoly1d(sprofile,slitz,1,bdpixthresh,0.1,/SILENT,$
                                OGOODBAD=ogoodbad,CANCEL=cancel)
           if cancel then goto, cont2
           
;  Debug plotter
           
           if debugfndbdpx then begin
;x[j] ge debugxrange[0] and $
;              x[j] le debugxrange[1] 
              
              mc_moments,slitz,tmean,tvvar,/SILENT,ROBUST=5,CANCEL=cancel

              plot, slity_arc,slitz,/XSTY,PSYM=10,$
                    XRANGE=[0,slith_arc],/YSTY,$
                    TITLE='Bad Pixel Window, Column - '+strtrim(x[j],2),$
                    YRANGE=[tmean-5*sqrt(tvvar),tmean+5*sqrt(tvvar)]
              
              oplot,slity_arc,poly(sprofile,coeff),psym=10,COLOR=2
              z = where(ogoodbad eq 0 or bdpxmkz eq 0,count)
              if count ne 0 then oplot,slity_arc[z],slitz[z],COLOR=4,psym=2,$
                                       SYMSIZE=2
              read, re
              
           endif
           
           scsprofile = poly(sprofile,coeff)
           
;           coeff = robustpoly1d(abs(sprofile),vslitz,1,bdpixthresh,0.1,$
;                                /SILENT,CANCEL=cancel)
;           if cancel then goto, cont2
           
;           scvprofile = poly(abs(sprofile),coeff)
           
           badpix = where(ogoodbad eq 0 or bdpxmkz eq 0,count)
           if count ne 0 then begin
              
              slitz[badpix]  = scsprofile[badpix]
;              vslitz[badpix] = scvprofile[badpix]
              vslitz[badpix] = !values.f_nan
              
           endif
           
        endif
        
        for k = 0, naps-1 do begin
           
           l = i*naps+k
           z = where(mask gt float(k) and mask le float(k+1))

           ofspec[j,k] = total(slitz[z]*(mask[z]-float(k)))
           oespec[j,k] = sqrt(total(vslitz[z]*(mask[z]-float(k))^2))

;  Identify flagged pixels

           junk = where(linmkz[z] eq 1,cnt)
           if cnt ne 0 then obspec[j,k] = obspec[j,k] + 1
           
           if fixbdpx then begin
              
              goodbad = ogoodbad*bdpxmkz
              
              junk = where(goodbad eq 0 and $
                           mask gt float(k) and mask le float(k+1),cnt)
              if cnt ne 0 then obspec[j,k] = obspec[j,k] + 2
              
           endif                     

        endfor
        cont2:
        
     endfor
     
;  Store the results

     nonan = mc_nantrim(owave,2,CANCEL=cancel)
     if cancel then return,-1
     
     for k = 0,naps-1 do begin
        
        name = 'ORD'+string(i+1,FORMAT='(I2.2)')+ $
               'AP'+string(k+1,FORMAT='(I2.2)')
        
        array = [[owave[nonan]], $
                 [ofspec[nonan,k]], $
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
  
  if debugbgsub or debugfndbdpx then wdelete, wid
  
  if keyword_set(UPDATE) then begin
     
     progressbar-> destroy
     obj_destroy, progressbar
     
  endif
  
  return, struc

end









