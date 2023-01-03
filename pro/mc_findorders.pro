;+
; NAME:
;     mc_findorders
;
; PURPOSE:
;     Determines the position of the order(s) in a spectral image.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     mc_findorders,image,guesspos,start,stop,step,slith_pix,degree,$
;                   edgecoeffs,xranges,FRAC=frac,COMWIDTH=comwidth,WID=wid,$
;                   PLOTGUESS=plotguess,CANCEL=cancel
;
; INPUTS:
;     image     - Flat field image oriented so that the dispersion
;                 axis is roughly aligned with the rows
;     guesspos  - An array [2,norders] of positions x,y located
;                 within each order.  The positions should be near the 
;                 center of the order and be located in a region of 
;                 large flux away from bad pixels.
;     start     - Start column
;     stop      - Stop column
;     step      - Step size in the dispersion direction 
;     slith_pix - A 2-D array giving the range of possible slit
;                 heights in pixels.  This is used to make sure the 
;                 routine doesn't include bad pixels in the fit
;     degree    - Polynomial fit degree for the edges of the orders
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     FRAC      - The fraction of the flux of the center of the slit
;                 used to identify the location of the edge of the order
;     WID       - Window ID of a window to plot the results
;     PLOTGUESS - Set to plot the guess solution positions
;     CANCEL    - Set on return if there is a problem
;
; OUTPUTS:
;     edgecoeffs - Array [degree+1,2,norders] of polynomial coefficients 
;                  which define the edges of the orders.  array[*,0,0]
;                  are the coefficients of the bottom edge of the
;                  first order and array[*,1,0] are the coefficients 
;                  of the top edge of the first order.
;     xranges    - An array [2,norders] of column numbers where the
;                  orders are completely on the array
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
;     The IDL sobel function is used to enhance the edges of the
;     orders.  Within an order, and at a particular column, the
;     positions above and below the guess position at which the flux
;     falls to 0.75 the value at the guess position is determined.  
;     Centroids are computed at these positions on the Sobeled image
;     to determine the location of the edge of the order.  As the
;     program steps away from the guess position, the new guess
;     position is estimated using the previously determined edges.
;     After the entire array has been checked, the results are fit
;     with a robust least-squares polynomial of degree degree.
;     
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2001-05-01 - Written by Michael Cushing, Institute for Astronomy, UH
;     2001-10-02 - Added xrange input keyword and changed to a procedure
;     2001-11-06 - Added the PLOTGUESS keyword
;     2002-08-20 - Added FRAC keyword.
;     2002-09-01 - Changed the slith_pix parameter to a 2-D array
;     2003-09-04 - Fixed bug found by K. Cruz.
;     2005-10-11 - Modified findorders for CORMASSTOOL.
;     2007-04-02 - Added sranges input variable
;     2007-07-25 - Removed start and stop inputs.
;                  Added frac and comwidth inputs.
;-
pro mc_findorders,image,guesspos,sranges,step,slith_pix,degree,bufpix,$
                  frac,comwidth,edgecoeffs,xranges,WID=wid,PLOTGUESS=plotguess,$
                  CANCEL=cancel
  
  cancel = 0

;  Check parameters

  if n_params() lt 9 then begin
     
     print, 'Syntax - mc_findorders,image,guesspos,sranges,step,slith_pix,$'
     print, '            degree,edgecoeffs,xranges,frac,comwin,WID=wid,$'
     print, '            PLOTGUESS=plotguess,CANCEL=cancel'
     cancel = 1
     return
     
  endif
  
  cancel = mc_cpar('mc_findorders',image,1,'Image',[2,3,4,5],2)
  if cancel then return
  cancel = mc_cpar('mc_findorders',guesspos,2,'Guesspos',[2,3,4,5],[1,2])
  if cancel then return
  cancel = mc_cpar('mc_findorders',sranges,3,'Sranges',[2,3,4,5],[1,2])
  if cancel then return
  cancel = mc_cpar('mc_findorders',step,4,'Step',[2,3,4,5],0)
  if cancel then return
  cancel = mc_cpar('mc_findorders',slith_pix,5,'Slith_pix',[2,3,4,5],1)
  if cancel then return
  cancel = mc_cpar('mc_findorders',degree,6,'Degree',[2,3,4,5],0)
  if cancel then return
  
;  Debugging variables
  
  debug      = 0
  debugorder = 0               ;  Note only 1 plotting debug part is in here...
  
  if debug then window, /FREE
  mywid = !d.window
  
  
;  Take care of keywords
  
  halfwin = round(comwidth/2.)
  if n_elements(FRAC) eq 0 then frac = 0.8
  if n_elements(WID) ne 0 then wset, wid
  if debug then wset, mywid
  
;  Get size of image and norders
  
  mc_arrinfo,guesspos,ndimen,dimen,CANCEL=cancel
  if cancel then return
  norders = (ndimen eq 1) ? 1:dimen[1]
  
  s = size(image,/DIMEN)
  ncols = s[0]
  nrows = s[1]
  
  row    = findgen(nrows)
  
  edgecoeffs = dblarr(degree+1,2,norders)
  xranges    = intarr(2,norders)
  
;  Scale, and roberts the image
  
  rimage = sobel((image*1000./max(image)))
  
;ximgtool, rimage,/AUTO,STDIMAGE=2048
;cancel = 1
;return

  for i = 0, norders-1 do begin

     start = sranges[0,i]
     stop  = sranges[1,i]
     
     starts = start+step-1  
     stops  = stop -step+1 
     scols  = findgen(fix((stops-starts)/step)+1)*step + starts
     nscols = n_elements(scols) 
     edges  = replicate(!values.f_nan,nscols,2)
     cen    = fltarr(nscols)+!values.f_nan
     
;  Set up array to store the position of the center of the order once 
;  the edges are found.
     
     cen = fltarr(nscols)+!values.f_nan
     
     if n_elements(WID) ne 0 and not debug then begin

        plotsym,0,1,/FILL
        plots,[guesspos[0,i],guesspos[0,i]],[guesspos[1,i],guesspos[1,i]],COLOR=2,PSYM=8

     endif
     
;  Use the guess position as the first few entries into the cen array
;  since we don't actually know the center position.  You fill in at
;  least degree+1 points so you can do the initial fit.
     
    tabinv,scols,guesspos[0,i],idx
    gidx = round(idx)
    cen[(gidx-degree):(gidx+degree)] = guesspos[1,i]
    
;  Now move left
    
    dotop = 1
    dobot = 1
    
    for j = 0, gidx do begin
       
       k = gidx-j
       fcol = reform( image[scols[k],*] )
       rcol = reform( rimage[scols[k],*] )
       
       coeff   = poly_fit1d(scols,cen,1 > (degree-2),/SILENT,/JUSTFIT)
       y_guess =  bufpix > poly(scols[k],coeff) < (nrows-bufpix-1)
       z_guess = fcol[y_guess]
       
       if keyword_set(PLOTGUESS) and not debug then $
          plots,[scols[k],scols[k]],[y_guess,y_guess],COLOR=2,PSYM=3,$
                SYMSIZE=1.5
       
;  Determine COM of edges
       
       if dotop then begin
          
          z = where(fcol lt frac*z_guess and row gt y_guess,cnt)
          if cnt ne 0 then begin
             
             guessyt = z[0]
             bidx    = 0 > (guessyt-halfwin)
             tidx    = (guessyt+halfwin) < (nrows-1)
             y       = row[bidx:tidx]
             z       = rcol[bidx:tidx]
             COM_top = total(y*z)/total(z)
             
          endif else COM_top = !values.f_nan
          
       endif else COM_top = !values.f_nan
       
       if dobot then begin
          
          z = where(fcol lt frac*z_guess and row lt y_guess,cnt)
          if cnt ne 0 then begin
             
             guessyb  = z[cnt-1]
             bidx    = 0 > (guessyb-halfwin)
             tidx    = (guessyb+halfwin) < (nrows-1)
             y       = row[bidx:tidx]
             z       = rcol[bidx:tidx]
             COM_bot = total(y*z)/total(z)
             
          endif else COM_bot = !values.f_nan
          
       endif else COM_bot = !values.f_nan
       
       if debug and debugorder eq i then begin
          
          plot, row,rcol,/xsty,/ysty,$
                TITLE=scols[k]
          plots,[y_guess,y_guess],!y.crange,COLOR=3
          plots, [COM_bot,COM_bot],!y.crange,color=2
          plots, [COM_top,COM_top],!y.crange,color=2
          re = ' '
          read, re
          
       endif
       
       
;  Check to make sure the slit height is reasonable
       
       if dotop and dobot and finite(COM_bot) and finite(COM_top) then begin
          
          if abs(com_bot-com_top) gt slith_pix[0] and $
             abs(com_bot-com_top) lt slith_pix[1] then begin
             
             edges[k,*] = [com_bot,com_top]
             cen[k] = (com_bot+com_top)/2.
             
          endif else goto, cont1
          
       endif else begin
          
          com_bot = !values.f_nan
          com_top = !values.f_nan
          edges[k,*] = [com_bot,com_top]
          cen[k] = y_guess
          
       endelse
       
       if n_elements(WID) ne 0 and dotop and not debug then $
          plots,[scols[k],scols[k]],[com_top,com_top],PSYM=1, COLOR=3
       
       if n_elements(WID) ne 0 and dobot and not debug then $
          plots,[scols[k],scols[k]],[com_bot,com_bot],PSYM=1, COLOR=3
       
       if com_top le bufpix or com_top ge (nrows-1-bufpix) then dotop = 0
       if com_bot le bufpix or com_bot gt (nrows-1-bufpix) then dobot = 0
       
       if not dotop and not dobot then goto, moveon1
       
       cont1:
       
    endfor
    moveon1:
    
;  Now go right
    
    dotop = 1
    dobot = 1
    
    for j = gidx+1,nscols-1 do begin
       
       k    = j
       fcol = reform( image[scols[k],*] )
       rcol = reform( rimage[scols[k],*] )
       
       coeff   = poly_fit1d(scols,cen,1 > (degree-2),/SILENT,/JUSTFIT)
       y_guess =  bufpix > poly(scols[k],coeff) < (nrows-bufpix-1)
       z_guess = fcol[y_guess]
       
       if keyword_set(PLOTGUESS) and not debug then $
          plots,[scols[k],scols[k]],[y_guess,y_guess],COLOR=2,PSYM=3,$
                SYMSIZE=1.5
       
;  Determine COM of edges
       
       if dotop then begin
          
          z = where(fcol lt frac*z_guess and row gt y_guess,cnt)
          if cnt ne 0 then begin
             
             guessy  = z[0]
             bidx    = 0 > (guessy-halfwin)
             tidx    = (guessy+halfwin) < (nrows-1)
             y       = row[bidx:tidx]
             z       = rcol[bidx:tidx]
             COM_top = total(y*z)/total(z)
             
          endif else com_top = !values.f_nan
          
       endif else com_top = !values.f_nan
       
       if dobot then begin
          
          z = where(fcol lt frac*z_guess and row lt y_guess,cnt)
          if cnt ne 0 then begin
             
             guessy  = z[cnt-1]
             bidx    = 0 > (guessy-halfwin)
             tidx    = (guessy+halfwin) < (nrows-1)
             y       = row[bidx:tidx]
             z       = rcol[bidx:tidx]
             COM_bot = total(y*z)/total(z)
             
          endif else com_bot = !values.f_nan
          
       endif else com_bot = !values.f_nan
       
;  Check to make sure the slit height is reasonable
       
       if dotop and dobot and finite(com_top) and finite(com_bot) then begin
          
          if abs(com_bot-com_top) gt slith_pix[0] and $
             abs(com_bot-com_top) lt slith_pix[1] then begin
             
             edges[k,*] = [com_bot,com_top]
             cen[k] = (com_bot+com_top)/2.
             
          endif else goto, cont2
          
       endif else begin

          com_bot = !values.f_nan
          com_top = !values.f_nan          
          edges[k,*] = [com_bot,com_top]
          cen[k] = y_guess
          
       endelse
       
       if n_elements(WID) ne 0 and dotop and not debug then $
          plots,[scols[k],scols[k]],[com_top,com_top],PSYM=1, COLOR=3
       
       if n_elements(WID) ne 0 and dobot and not debug then $
          plots,[scols[k],scols[k]],[com_bot,com_bot],PSYM=1, COLOR=3
       
       
       if com_top le bufpix or com_top ge (nrows-1-bufpix) then dotop = 0
       if com_bot le bufpix or com_bot gt (nrows-1-bufpix) then dobot = 0
       
       if not dotop and not dobot then goto, moveon2
       
       cont2:
       
    endfor
    moveon2:
    
    x = findgen(stop-start+1)+start
    
    for j = 0,1 do begin
       
       coeff = robustpoly1d(scols,edges[*,j],degree,3,0.01,$
                            OGOODBAD=goodbad,/SILENT,/GAUSSJ,CANCEL=cancel)
       if cancel then return
       edgecoeffs[*,j,i] = coeff
       
       if n_elements(WID) ne 0 and not debug then begin
          
          z = where(finite(edges[*,j]) eq 1)
          oplot, x,poly(x,coeff),color=2
          z = where(goodbad eq 0,count)
          if count ne 0 then oplot, scols[z],edges[z,j],color=4,psym=1
          
       endif
       
    endfor
    
    xranges[0,i] = sranges[0,i]
    xranges[1,i] = sranges[0,i]
    
    bot = poly(x,edgecoeffs[*,0,i])
    top = poly(x,edgecoeffs[*,1,i])
    
    z = where(top gt 0.0 and top lt nrows-1 and bot gt 0 and bot lt nrows-1)
    xranges[*,i] = [min(x[z],MAX=max),max]
    
 endfor
  
  if debug then wdelete, mywid
  
end
