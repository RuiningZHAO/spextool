;+
; NAME:
;     wavecal
;
; PURPOSE:
;     Wavelength calibrates a X-dispersed spectra.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;
;
;  wrong
;
;     result = mc_xdwavecal1d(x,spectra,norders,naps,ltop,ordertypes,$
;                             pixoffset,orders,homeorder,FWHM,dispdeg,$
;                             ordrdeg,lines,types,robustthresh,[psfile],$
;                             RMS=rms,NLINES=nlines,UPDATE=update,$
;                             WIDGET_ID=widget_id,PLOTRESID=plotresid,$
;                             PLOTLINEFIND=plotlinefind,CANCEL=cancel
;
; INPUTS:
;     x            - An array of independent values for the spectra
;     spectra      - An array [nelements,naps*norders] where array[*,x]
;                    are flux values
;     norders      - The number of orders
;     naps         - The number of apertures
;     ltop         - (lambda to pixel) 1-D polynomial guess
;                    to convert wavelengths to pixel positions
;     ordertypes   - An NORDERS length array giving the type of line, 
;                    a=argon, s=sky
;     pixoffset    - Pixel offset to ltop.  Accounts for shifts from
;                    observations to the standard solution
;     orders       - An array of the orders numbers
;     homeorder    - The home order number (see below).
;     FWHM         - The FWHM of the gaussian to fit to each line. 
;     dipdeg       - The order to fit the results in the dispersion
;                    dimension.
;     ordrdeg      - The order to fit the results in the order
;                    dimension.  If a single order, set to 0.
;     lines        - An array of lines that appear on the array
;     types        - The type of line, a=argon, s=sky
;     robustthresh - The sigma threshold to identify outliers in the
;                    robust polynomial fit.
;
; OPTIONAL INPUTS:
;     psfile       - A scalar string giving the root of the QA plot
;                    name.
;
; KEYWORD PARAMETERS:
;     RMS          - Returns the rms of the robust fit(s).
;     NLINES       - An n_elements(order) array giving the number of
;                    lines identified in each order.
;     WRANGES      - An array [2,norders] of the wavelengths of the left
;                    and right columns of the order.
;     UPDATE       - If set, the program will launch the Fanning
;                    showprogress widget.
;     WIDGET_ID    - If given, a cancel button is added to the Fanning
;                    showprogress routine.  The widget blocks the
;                    WIDGET_ID, and checks for for a user cancel
;                    command.
;     PLOTRESID    - Set to plot the residuals..
;     PLOTLINEFIND - Set to plot the line finding steps.
;     CANCEL       - Set on return if there is a problem.
;
; OUTPUTS:
;     Returns the 2-D coefficients of the superfit.
;
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
;     This routine is based on the idea that all the orders of 
;     an cross-dispersed spectrum can be wavelength calibrated at
;     once.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2000-08-01 - Written by M. Cushing, Institute for Astronomy, UH
;     2001-02-20 - Heavily modified to make sure that only lines found 
;                  in all apertures are used in the fit.
;     2001-03-05 - Modified to identify the lines in a given order
;                  without knowing the wavelength range of the order.
;     2002-10-22 - Changed PLOT keyword to PLOTRESID
;     2002-12-04 - Removed PS keyword
;     2004-01-29 - Added ordertypes, and types inputs
;     2008-02-15 - Removed naps input and added NLINES keywords and
;                  changed name from wavecal to mc_wavecal1d.
;     2014-07-29 - Modifed to create .eps files and requires the user
;                  to only enter the root of the output name.
;-

function mc_xdwavecal1d,spectra,orders,w2pcoeffs,offset,lines,linetypes, $
                        ordertypes,slitw_pix,homeorder,dispdeg,ordrdeg, $
                        robustthresh,psfile,RMS=rms,NLINES=nlines, $
                        WRANGES=wranges,UPDATE=update,WIDGET_ID=widget_id, $
                        PLOT=plot,CANCEL=cancel
  
  cancel = 0

  norders = n_elements(orders)
  nlines = intarr(norders)
  
  sarray = [[0],[0],[0]]

  for i = 0, norders-1 do begin


     wspec = reform((spectra.(i))[*,0])
     fspec = reform((spectra.(i))[*,1])

;  NaN trim just to be sure

     idx = mc_nantrim(wspec,2,CANCEL=cancel)
     if cancel then return,-1

     wspec = wspec[idx]
     fspec = fspec[idx]

;  Identify the lines to be used in orders[i]
     
     goodl = where(linetypes eq strtrim(ordertypes[i],2))
     plines = lines[goodl]

;  Determine their initial x positions

     xguess = poly(plines,w2pcoeffs[*,i]) + offset        
     
;  Only try and find those that fall on the array

     z = where(xguess gt min(wspec,MAX=xmax) and xguess lt xmax,cnt)
     xguess = xguess[z]
     plines = plines[z]

;  Now prune the lines based on the resolution 


;  Now scale them to the home order for storage later

     slines = plines*orders[i]/homeorder

;  Now find the lines

     xpos = mc_findlines1d(wspec,fspec,xguess,slitw_pix/2.354,WLINES=plines, $
                           PLOT=plot,GOODBAD=goodbad,CANCEL=cancel)

     z    = where(goodbad eq 1,cnt)
     nlines[i] = cnt

     if cnt eq 0 then continue

     sarray = [sarray,[[xpos[z]],[replicate(orders[i],cnt)],[slines[z]]]]

  endfor

;  Now do the fit

  scoeffs = dblarr( (dispdeg+1)*(ordrdeg+1))
  
  sxpos  = reform(sarray[1:*,0])
  sorder = reform(sarray[1:*,1])
  slines = reform(sarray[1:*,2])
  
;  For convenience sort them.
  
  z      = sort(slines)
  sxpos  = sxpos[z]
  slines = slines[z]
  sorder = sorder[z]

;  Do the fit

  scoeffs = robustpoly2d(sxpos,sorder,slines,dispdeg,ordrdeg,robustthresh, $
                         0.01,SILENT=1,RMS=rms,OGOODBAD=ogoodbad, $
                         CANCEL=cancel)
  if cancel then return, -1

;  Record the wavelength ranges

  wranges = fltarr(2,norders)

  for i = 0,norders-1 do begin

     xmin = min(reform((spectra.(i))[*,0]),MAX=xmax)
     wranges[*,i] = poly2d([xmin,xmax],[orders[i],orders[i]], $
                           dispdeg,ordrdeg,scoeffs)/orders[i]*homeorder

  endfor

;  Report bad lines

;  bad = where(ogoodbad eq 0,cnt)
;  if cnt ne 0 then begin

;     for j = 0,cnt-1 do print, sorder[bad[j]],(slines*homeorder/sorder)[bad[j]]

;  endif

  rms = rms*10000. ; in Angstroms
  
  if n_params() eq 13 then begin

     zfit = poly2d(sxpos,sorder,dispdeg,ordrdeg,scoeffs)
     resid = (slines-zfit)*10000.
     
     z = where(ogoodbad eq 1)
     m = moment(resid[z])

     bad = where(ogoodbad eq 0,badcnt)

     mc_setpsdev,psfile+'.eps',8,10.5,FONT=14,/ENCAP

     positions = mc_gridplotpos([0.1,0.075,0.97,0.95],[1,2],0.08,/COL)

     plot,sorder,resid,PSYM=8,POSITION=positions[*,0],FONT=0,/NODATA,$
          XRANGE=[min(sorder,MAX=max)-0.5,max+0.5],/XSTY,/YSTY,$
          XTITLE='Order Number',YTITLE='Data-Model (Angstroms)',$
          TITLE='RMS = '+string(sqrt(m[1]),FORMAT='(f5.3)')+' Angstroms'

     plotsym,0,0.5,/FILL
     for i = 0,norders-1 do begin

        z = where(fix(sorder) eq orders[i],cnt)
        if cnt ne 0 then begin

           plotsym,0,0.9,/FILL
           oplot,sorder[z],resid[z],PSYM=8
           plotsym,0,0.7,/FILL
           oplot,sorder[z],resid[z],COLOR=2+i,PSYM=8
           
        endif
           
     endfor     


     if badcnt ne 0 then oplot,sorder[bad],resid[bad],PSYM=6,SYMSIZE=1.5

     plots,!x.crange,[m[0],m[0]],LINESTYLE=2
     plots,!x.crange,[m[0],m[0]]-sqrt(m[1]),LINESTYLE=1
     plots,!x.crange,[m[0],m[0]]+sqrt(m[1]),LINESTYLE=1


     
     plot,sxpos,resid,PSYM=8,POSITION=positions[*,1],FONT=0,/NODATA, $
          /NOERASE,/XSTY,/YSTY,XTITLE='Column (pixels)',$
          YTITLE='Data-Model (Angstroms)'
     

     for i = 0,norders-1 do begin

        z = where(fix(sorder) eq orders[i],cnt)

        if cnt ne 0 then begin

           plotsym,0,0.9,/FILL
           oplot,sxpos[z],resid[z],PSYM=8
           plotsym,0,0.7,/FILL
           oplot,sxpos[z],resid[z],COLOR=2+i,PSYM=8

        endif
        
     endfor
     
     plotsym,0,1
     if badcnt ne 0 then oplot,sxpos[bad],resid[bad],PSYM=6,SYMSIZE=1.5

     plots,!x.crange,[m[0],m[0]],LINESTYLE=2
     plots,!x.crange,[m[0],m[0]]-sqrt(m[1]),LINESTYLE=1
     plots,!x.crange,[m[0],m[0]]+sqrt(m[1]),LINESTYLE=1



     
     mc_setpsdev,/CLOSE
     
  endif
 
  return,scoeffs
  
end

