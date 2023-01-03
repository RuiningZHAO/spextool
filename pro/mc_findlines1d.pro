;+
; NAME:
;     mc_findlines1d
;
; PURPOSE:
;     Identifies arc line positions given the guesses of their positions.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = mc_findlines1d(x,y,xguess,sigma,GOODBAD=goodbad,PLOT=plot,$'
;                             WLINES=wlines,CANCEL=cancel)
;
; INPUTS:
;     x      - Array of indenpendent values
;     y      - Array of dependent values (the spectrum)
;     xguess - An array of guess positions (in units of x) of the lines
;      sigma - A scalar giving the theoretical standard deviation of
;              the lines in units of x.
;    
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     GOODBAD - An array of 1s (good) and 0s (bad) to identify which
;               lines where identified
;     PLOT    - Set to plot the fits.  WLINES is the required.
;     WLINES  - Given an array of lines associated with the xguess
;               positions the program will label the plots with the
;               wavelength. 
;     CANCEL  - Set on return if there is a problem
;     
; OUTPUTS:
;     Returns an array of the line positions in units of x.
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
;     Each line is fit with a gaussian to determine the line center.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     Written 2/10/99 by M. Cushing, Institute for Astronomy, UH
;     2007-09-14 - Changed name to mc_findlines to avoid conflict with
;                  Keck/OSIRIS DRP.
;-
function mc_findlines1d,x,y,xguess,sigma,GOODBAD=goodbad,PLOT=plot, $
                        WLINES=wlines,CANCEL=cancel

  plot = keyword_set(PLOT)
  cancel = 0

;  Check parameters 

  if n_params() lt 3 then begin
     
     print, 'Syntax - result = mc_findlines1d(x,y,xguess,sigma,$'
     print, '                                 GOODBAD=goodbad,PLOT=plot,$'
     print, '                                 WLINES=wlines,CANCEL=cancel)'
     cancel = 1
     return, -1
     
  endif
  cancel = mc_cpar('mc_findlines1d',x,1,'X',[2,3,4,5],1)
  if cancel then return,-1
  cancel = mc_cpar('mc_findlines1d',y,2,'Y',[2,3,4,5],1)
  if cancel then return,-1
  cancel = mc_cpar('mc_findlines1d',sigma,3,'Sigma',[2,3,4,5],0)
  if cancel then return,-1
  
;  Get basic info

  xmax   = max(x,MIN=xmin)
  nx     = n_elements(x)
  nlines = n_elements(xguess)
  hwin   = fix(sigma*(5.))
  
; Initialize the goodbad and lines array.
  
  goodbad = intarr(nlines)
  lines   = fltarr(nlines,/NOZERO)
  y       = float(y)
  
  if keyword_set(PLOT) then begin
     
     window,/FREE,YSIZE=((get_screen_size())[1]-30) < 1000,XSIZE=600
     win_idx = !d.window
     !p.multi[2] = 2
     re = ' '
     
  endif
  
;  Set up Gaussian
  
  expr = 'p[0] + gaussian(x,p[1:3])'
  
  for i = 0, nlines-1 do begin
     
;  Extract the line to be fit, ignoring lines with NaNs (usually the edge.)
     
     tabinv, x,xguess[i],idx
     xwin  = float( x[(0 > (round(idx)-hwin)):(nx-1 < (round(idx)+hwin))] )
     ywin  =        y[(0 > (round(idx)-hwin)):(nx-1 < (round(idx)+hwin))] 
     nan   = where(finite(ywin) eq 0,count)
     if count ne 0 then continue
     
;  Fit and store the line position.

     tabinv,xwin,xguess[i],idx
     start =float( [median(ywin),ywin[round(idx)],xguess[i],sigma] )
     fit = mpfitexpr(expr,xwin,ywin,dummy,start,PARINFO=pi,/QUIET,/NOCOVAR)
     
     lines[i] = fit[2]

;  Check whether it is a good fit or not.

     tabinv,xwin,fit[2],idx
     
     if fit[2] le xguess[i]+1 and $
        fit[2] ge xguess[i]-1 and $
        fit[3] gt 0.0 and $
        fit[1] gt 0 then goodbad[i]  = 1     
     
     if keyword_set(PLOT) then begin
        
        mc_moments,y,mean,var,stddev,ROBUST=3,/SILENT,CANCEL=cancel
        if cancel then return,-1
        
        plot,x,y,/XSTY,/YSTY,TITLE='Line: '+strtrim(wlines[i],2),$
             YRANGE=[mean-5*stddev,mean+20*stddev],CHARSIZE=1.5,$
             XTITLE='Column (pixels)',YTITLE='Arbitrary Flux'
        plots,[xguess[i],xguess[i]],!y.crange,COLOR=6
        
        plot,xwin,ywin,/XSTY,PSYM=10,TITLE=(goodbad[i] eq 1) ? 'Good':'Bad',$
             CHARSIZE=1.5,XTITLE='Column (pixels)',YTITLE='Arbitrary Flux'
        oplot,xwin,poly(xwin,fit[0])+gaussian(xwin,fit[1:3]),COLOR=3,PSYM=10
        plots,[xguess[i],xguess[i]],!y.crange,COLOR=6
        plots,[fit[2],fit[2]],!y.crange,COLOR=2
        plots,[fit[2]-2,fit[2]-2],!y.crange,COLOR=2,LINESTYLE=1
        plots,[fit[2]+2,fit[2]+2],!y.crange,COLOR=2,LINESTYLE=1
        
        read, re
        
     endif
     
  endfor
  
  if keyword_set(PLOT) then begin
     
     wdelete, win_idx
     !p.multi = 0

  endif
  return, lines
  
end







