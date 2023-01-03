;+
; NAME:
;     mc_wavecal1d
;
; PURPOSE:
;     To wavelength calibrate coefficients for a single spectrum.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = mc_wavecal1d(xspec,yspec,xranges,wlines,xglines,FWHM,polydeg, $
;                           thresh,[psfile],RMS=rms,YFIT=yfit,NLINES=nlines, $
;                           GOODBAD=goodbad,WRANGES=wranges,SILENT=silent,$
;                           PLOTLINEFIND=plotlinefind,CANCEL=cancel)
;
; INPUTS:
;      xspec   - The x array (pixels).
;      yspec   - The y array (line emission spectrum).
;      xranges - The x values over which to search for lines, e.g.,
;                [500,1000]
;      wlines  - The wavelengths of the lines.
;      xglines - Guess positions in units of xspec for each line.
;      FWHM    - The FWHM of a line (see mc_findlines.pro).
;      polydeg - The polynomial degree to fit to the line positions.
;      thresh  - The threshold over which to ignore data points int
;                the robust polynomial fit.
;
; OPTIONAL INPUTS:
;      psfile  - If given, a plot showing the residuals is created.
;
; KEYWORD PARAMETERS:
;      RMS          - The RMS error of the fit in units of wlines.
;      COEFF        - The coefficients of the polynomial fit.
;      NLINES       - The number of good lines identified.  Fewer may
;                     make it past the robust polynomial fit.
;      GOODBAD      - An array of 1s (good) and 0s (bad) to identify which
;                     lines where identified
;      WRANGES      - An array [2] of the wavelengths of the left
;                     and right columns of the order.
;      SILENT       - Set to silence reports about bad lines.
;      PLOTLINEFIND - Set to watch the line finding.
;      CANCEL       - Set on return if there is a problem.
;
; OUTPUTS:
;      The polynomial coefficients.
;
; OPTIONAL OUTPUTS:
;      None
;
; COMMON BLOCKS:
;      None
;
; SIDE EFFECTS:
;      None
;
; RESTRICTIONS:
;      Requires the Astronomy User's Library and the Spextool package,
;      http://irtfweb.ifa.hawaii.edu/~spex/
;
; PROCEDURE:
;      Later.
;
; EXAMPLE:
;      Later.
;
; MODIFICATION HISTORY:
;      2008 - Written by M. Cushing, Institute for Astronomy,
;             University of Hawaii.
;-
function mc_wavecal1d,xspec,yspec,xranges,wlines,xglines,FWHM,polydeg, $
                      thresh,psfile,RMS=rms,COEFF=coeff,NLINES=nlines, $
                      GOODBAD=goodbad,WRANGES=wranges,SILENT=silent, $
                      PLOTLINEFIND=plotlinefind,CANCEL=cancel

  cancel = 0

  if n_params() lt 8 then begin

     print, 'Syntax - result = mc_wavecal1d(xspec,yspec,xranges,wlines,$'
     print, '                               xglines,FWHM,polydeg,thresh,$'
     print, '                               [psfile],RMS=rms,COEFF=coeff,$'
     print, '                               NLINES=nlines,GOODBAD=goodbad,$'
     print, '                               WRANGES=wranges,SILENT=silent,$'
     print, '                               PLOTLINEFIND=plotlinefind,$'
     print, '                               CANCEL=cancel)'
     cancel = 1
     return, -1

  endif

;  Cut line list first based on the xranges

  z = where(xglines gt xranges[0] and xglines lt xranges[1],cnt)
  
  xgline = xglines[z]
  wline = wlines[z]

;  Find lines

  xline = mc_findlines1d(xspec,yspec,xgline,FWHM/2.354,WLINES=wline, $
                         GOODBAD=goodbad,PLOT=plotlinefind,CANCEL=cancel)
  if cancel then return,-1

;  Store only those lines that were correctly found

  zgood = where(goodbad eq 1,nlines,COMP=zbad,NCOMP=nbad)

  if ~keyword_set(SILENT) and nbad ne 0 then begin

     print
     print, 'Centroids for the following lines (x,wave) could not' + $
            ' be determined:'
     print
     for i = 0,nbad-1 do begin
        
        print, 'X Guess='+strtrim(xgline[zbad[i]],2)+', X Fit='+$
               strtrim(xline[zbad[i]],2)+', W='+strtrim(wline[zbad[i]],2)
        
     endfor
     print
     
  endif

  xline = xline[zgood]
  wline = wline[zgood]

;  Do robust fit.

  coeff = robustpoly1d(xline,wline,polydeg,thresh,0.05,RMS=rms,/SILENT, $
                       YFIT=yfit,OGOODBAD=ogoodbad,CANCEL=cancel)
  if cancel then return, -1

  zgood = where(ogoodbad eq 1,COMP=zbad,NCOMP=nbad)

  if ~keyword_set(SILENT) and nbad ne 0 then begin

     print
     print, 'The following lines were rejected in the fit::'
     print
     for i = 0,nbad-1 do begin
        
        print, 'X pos='+strtrim(xline[zbad[i]],2)+ $
               ', W='+strtrim(wline[zbad[i]],2)
        
     endfor
     print
     
  endif

  x = xspec*!values.f_nan
  z = where(finite(yspec) eq 1)
  x[z] = xspec[z]

;  Plot the residuals if requested

  if n_params() eq 9 then begin

     resid = (wline-yfit)*10000.
     m = moment(resid[zgood])

     mc_setpsdev,psfile,8,5,FONT=14

     plotsym,0,0.5,/FILL
     plot,xline,resid,PSYM=8,FONT=0,/NODATA, $
          /XSTY,/YSTY,$
;          YRANGE=[m[0]-3*sqrt(m[1]),m[0]+3*sqrt(m[1])],$
          XTITLE='Column (pixels)', $
          YTITLE='Data-Model (Angstroms)',$
          TITLE='RMS = '+strtrim(string(sqrt(m[1]),FORMAT='(G7.4)'),2)+ $
          ' Angstroms'
     
     oplot,xline,resid,COLOR=2,PSYM=8
     plotsym,0,1
     if nbad ne 0 then oplot,xline[zbad],resid[zbad],PSYM=8

     plots,!x.crange,[m[0],m[0]],LINESTYLE=2
     plots,!x.crange,[m[0],m[0]]-sqrt(m[1]),LINESTYLE=1
     plots,!x.crange,[m[0],m[0]]+sqrt(m[1]),LINESTYLE=1

     mc_setpsdev,/CLOSE


  endif

  yfit = poly(x,coeff)
  wranges = [min(yfit,MAX=max),max]

  return, coeff

end




