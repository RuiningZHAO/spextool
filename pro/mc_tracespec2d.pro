;+
; NAME:
;     mc_tracespec2d
;
; PURPOSE:
;     Traces spectra in an cross-dispersed image.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = mc_tracespec2d(image,var,edgecoeffs,norders,naps,slith_arc,$
;                             pos_arc,xranges,step,sumap,winthresh,sigthresh,$
;                             fitorder,WID=wid,OPLOT=oplot,CANCEL=cancel)
;
; INPUTS:
;     image      - 2-D spectral image
;     var        - The variance image
;     edgecoeffs - Array [degree+1,2,norders] of polynomial coefficients 
;                  which define the edges of the orders.  array[*,0,0]
;                  are the coefficients of the bottom edge of the
;                  first order and array[*,1,0] are the coefficients 
;                  of the top edge of the first order.  
;     norders    - The number of orders
;     naps       - The number of apertures
;     slith_arc  - The length of the slit in arcseconds
;     pos_arc    - An array of guess positions of each object in arcseconds
;     xranges    - An array [2,norders] of pixel positions where the
;                  orders are completely on the array
;     step       - Step size in the dispersion direction used to determine the 
;                  the trace 
;     sumap      - Number of columns to add together (MUST BE ODD
;                  AND LE 2*step)
;     winthresh  - The threshold over which an identified peak is
;                  ignored. If the difference between the guess
;                  position and the fitted position is larger than
;                  thresh, the fitted position is ignored.
;     sigthresh  - The threshold over which the fitted sigma is
;                  accepted.  This is used to ignore fits with sigmas
;                  of ~1 pixel.
;     fitorder   - polynomial fit degree 
;    
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     WID    - Window ID number to plot the results.  This will only
;              do oplot statements so the plot command must
;              have be executed before this program is called.
;     OPLOT  - A structure where each element corresponds to a single
;              aperture.  Within each element, is a [ncols,3] array
;              where [*,0] are the column numbers used for the trace,
;              [*,1] are the fitted points, and [*,2] is the goodbad
;              array from the polynomial fit.  Unfortunately, the
;              apertures are in reverse order, i.e., the go from the
;              top of the array down instead of the standard Spextool
;              format of bottom up.
;     CANCEL - Set on return if there is the problem
;
; OUTPUTS:
;     Returns an array [ncoeff+1,naps*norders] of polynomial coefficients of
;     the trace of each object
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
;     Within an order, sumap columns are added together to
;     increase the total signal.  A gaussian is then fitted around the
;     guess position.  If mu is within 5 pixels of the guess, the
;     position is stored.  The resulting positions are then fitted
;     with a polynomial of degree, fitorder.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2000-08-30 - Written by M. Cushing, Institute for Astronomy, UH
;     2008-08-17 - Changed name to mc_tracespec
;     2008-09-20 - Added OPLOT keyword.
;-
function mc_tracespec2d,image,wavecal,spatcal,edgecoeffs,xranges,appos, $
                        step,sumap,winthresh,sigthresh,fitorder,WID=wid, $
                        OPLOT=oplot,CANCEL=cancel
  
  cancel = 0

  plotline = 0
  plotpoly = 0

;  Check parameters

  if n_params() lt 11 then begin
     
     cancel = 1
     print, 'Syntax - coeff = mc_tracespec2d(image,wavecal,spatcal,edgecoeffs,$'
     print, '                                xranges,appos,step,sumap,$'
     print, '                                winthresh,sigthresh,$'
     print, '                                fitorder,WID=wid,OPLOT=oplot,$'
     print, '                                CANCEL=cancel)'
     return, -1
     
  endif
  
  cancel = mc_cpar('mc_tracespec2d',image,1,'Image',[2,3,4,5],2)
  if cancel then return,-1
  cancel = mc_cpar('mc_tracespec2d',wavecal,2,'Wavecal',[2,3,4,5],2)
  if cancel then return,-1
  cancel = mc_cpar('mc_tracespec2d',spatcal,3,'Spatcal',[2,3,4,5],2)
  if cancel then return,-1
  cancel = mc_cpar('mc_tracespec2d',edgecoeffs,4,'Edgecoeffs',[2,3,4,5],[2,3])
  if cancel then return,-1
  cancel = mc_cpar('mc_tracespec2d',xranges,5,'Xranges',[2,3,4,5],[1,2])
  if cancel then return,-1
  cancel = mc_cpar('mc_tracespec2d',appos,6,'Appos',[2,3,4,5],[1,2])
  if cancel then return,-1
  cancel = mc_cpar('mc_tracespec2d',step,7,'Step',[2,3,4,5],0)
  if cancel then return,-1
  cancel = mc_cpar('mc_tracespec2d',sumap,8,'Sumap',[2,3,4,5],0)
  if cancel then return,-1
  cancel = mc_cpar('mc_tracespec2d',winthresh,9,'Win Thresh',[2,3,4,5],0)
  if cancel then return,-1
  cancel = mc_cpar('mc_tracespec2d',sigthresh,10,'Sig Thresh',[2,3,4,5],0)
  if cancel then return,-1
  cancel = mc_cpar('mc_tracespec2d',fitorder,11,'Fitorder',[2,3,4,5],0)
  if cancel then return,-1
    
  !except = 0
           
  s = size(image,/DIMEN)
  ncols     = s[0]
  nrows     = s[1]

  norders   = n_elements(xranges[0,*])
  naps      = n_elements(appos[*,0])

  halfsumap = fix(sumap/2.)
  x         = findgen(ncols)
  y         = findgen(nrows)
  coeff     = dblarr(fitorder+1,naps*norders)

  xx = rebin(indgen(ncols),ncols,nrows)
  yy = rebin(reform(indgen(nrows),1,nrows),ncols,nrows)


  if plotline then begin

     window
     linewid = !d.window

  endif

  if plotpoly then begin

     window
     polywid = !d.window

  endif

  for i = 0, norders-1 do begin

     starts  = xranges[0,i]+step-1 
     stops   = xranges[1,i]-step+1 
     
     numstep = fix((stops-starts)/step)+1
     column  = findgen(numstep)*step + starts
     
     botedge = poly(column,edgecoeffs[*,0,i])
     topedge = poly(column,edgecoeffs[*,1,i])

;  Make order mask for plotting if need be

     if plotpoly then begin

        omask = intarr(ncols,nrows)

        for j = xranges[0,i],xranges[1,i] do begin
           
           omask[j,poly(j,edgecoeffs[*,0,i]):poly(j,edgecoeffs[*,1,i])] = 1
           
        endfor

        orderpixels = where(omask eq 1)

        minwave = min(wavecal[orderpixels],MAX=maxwave,/NAN)
        minspat = min(spatcal[orderpixels],MAX=maxspat,/NAN)

        dw = (maxwave-minwave)/(xranges[1,i]-xranges[0,i])
        nwave = findgen(xranges[1,i]-xranges[0,i]+1)*dw+minwave

     endif

     peaks = replicate(!values.f_nan,numstep,naps)
     
     for j = 0,numstep-1 do begin
        
        if botedge[j] lt -0.5 or topedge[j] gt nrows-0.5 then continue
        
        
        y_bot  = 0 > botedge[j]
        y_top  = nrows-1 < topedge[j] 

        zimg   = median(reform(image[(column[j]-halfsumap) > 0: $
                                     (column[j]+halfsumap) < (ncols-1), $
                                     round(y_bot):round(y_top)]),/EVEN,DIMEN=1 )
        
        yarc   = reform(spatcal[column[j],round(y_bot):round(y_top)])
        yidx   = reform(y[round(y_bot):round(y_top)])

        z = where(finite(yarc) eq 1)
        zimg = zimg[z]
        yarc = yarc[z]
        yidx = yidx[z]

        for k = 0, naps-1 do begin
           
           if n_elements(yarc)-3 le 0 then continue
           
           linterp,yarc,zimg,appos[k,i],guessz
           linterp,yarc,yidx,appos[k,i],guessy

           if guessy ge nrows-1 then continue
           
           junk = gaussfit(yidx,zimg,fit,NTERMS=4,ESTIMATES=[guessz,guessy,1,0])

           if fit[2] gt sigthresh and $
              abs(fit[1]-guessy) le winthresh then peaks[j,k]=fit[1]
           
           if plotline then begin
              
              wset, linewid
              plot, yidx,zimg,/XSTY,/YSTY,PSYM=10,$
                    TITLE=string(column[j])+string(abs(fit[1]-guessy))+$
                    ((finite(peaks[j,k]) eq 1) ? 'Good':'Bad')
              plots, [guessy,guessy],!y.crange,COLOR=4
              plots, [fit[1],fit[1]],!y.crange,COLOR=2
              oplot, yidx,junk,COLOR=3,PSYM=10
              re = ' '
              read, re
                            
           endif
           
        endfor

       if n_elements(WID) ne 0 then begin
           
           wset, wid                 
           for k = 0, naps-1 do if finite(fit[1])  then $
              plots, column[j],peaks[j,k],PSYM=4,COLOR=6
           
        endif

        cont2:
        
     endfor
     
;  Get the fit plot window ready if need be.

     if plotpoly then begin
        
        wset, polywid
        plot,[1],[1],XRANGE=[min(wavecal[orderpixels],MAX=max,/NAN),max],$
             YRANGE=[min(spatcal[orderpixels],MAX=max,/NAN),max],/NODATA,$
             /XSTY,/YSTY
        
     endif
     

;  Now fit the objects locations with a polynomial.
     
     for k = 0, naps-1 do begin
        
        l = naps*i+k
        
        wave = interpolate(wavecal,column,peaks[*,k],/CUBIC)
        spat = interpolate(spatcal,column,peaks[*,k],/CUBIC)

;  Fit the trace in w/s space

        output = robustpoly1d(wave,spat,fitorder,3,0.01,OGOODBAD=goodbad, $
                              /GAUSSJ,/SILENT,CANCEL=cancel)
        if cancel then return, -1
        
        coeff[*,l] = output

;  Plot it if requested.

        if plotpoly then begin

           oplot,wave,spat,PSYM=4
           oplot,nwave,poly(nwave,output),COLOR=2
           bad = where(goodbad eq 0,cnt)
           if cnt ne 0 then oplot,wave[bad],spat[bad],PSYM=4,COLOR=4

        endif

;  Store data for ximgtool

        if l eq 0 then begin

           xoplot = column
           yoplot = peaks[*,k]
           goplot = goodbad

        endif else begin

           xoplot = [xoplot,column]
           yoplot = [yoplot,peaks[*,k]]
           goplot = [goplot,goodbad]

        endelse

     endfor

     if plotpoly then begin
        
        re = ' '
        read, re
        
     endif
     
  endfor

  oplot = [[xoplot],[yoplot],[goplot]]

  junk = check_math()
  !except = 1
  return, coeff
  
end


