;+
; NAME:
;     mc_findlines2d
;
; PURPOSE:
;     To measure the positions of lines in a spectrum and return, x,y,w,s.  
;
; CALLING SEQUENCE:
;     result = mc_findlines2d(img,edgecoeffs,xranges,orders,wlines,$
;                             w2pcoeffs,offset,wranges,slitw_pix,slith_arc,$
;                             linedeg,linewin,ysum,ystep,PLOTIMG=plotimg,$
;                             PLOTFIT=plotfit,PSFILE=psfile,WID=wid,$
;                             CANCEL=cancel)
;
; INPUTS:
;     img        - The 2D image with some kind of lines.
;     edgecoeffs - Array [degree+1,2,norders] of polynomial coefficients 
;                  which define the edges of the orders.  array[*,0,0]
;                  are the coefficients of the bottom edge of the
;                  first order and array[*,1,0] are the coefficients 
;                  of the top edge of the first order.
;     xranges    - An array [2,norders] of pixel positions of the left
;                  and right columns of the order.
;     orders     - An array [norder] of the order numbers
;     wlines     - The wavelengths of the lines.
;     w2pcoeffs  - An array of [deg,norders] of polynomial
;                  coefficients where w2pcoeffs[*,n] gives the
;                  transformation between a wavelength and the pixel
;                  position for order n.  See offset below.
;     offset     - Any offset between the pixel position given by
;                  w2pcoeffs and the actual position of the lines.
;     wranges    - An array [2,norders] of the wavelengths of the left
;                  and right columns of the order.
;     slitw_pix  - The slit width in pixels.
;     slith_arc  - The slit height in arcseconds.
;     linedeg    - The degree of the polynomial used to fit the line.
;     linewin    - The number of pixels on either side of the line
;                  center used to fit the line.
;     ystep      - The number of pixels to step in y when finding the lines.
;     ysum       - The number of rows summed in order to increase the
;                  S/N of the line.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     PLOTIMG - Set to plot the image during the line finding process.
;     PLOTFIT - Set to plot the fit to the line at each row.
;     PSFILE  - If given, the residuals of the fitting process is
;               written to a postscript file named PSFILE.
;     WID     - If given, the positions of the lines are plotted on
;               this window (e.g., ximgtool). 
;     CANCEL  - Set on return if there is a problem.
;
; OUTPUTS:
;
;
; OPTIONAL OUTPUTS:
;
;
; COMMON BLOCKS:
;
;
; RESTRICTIONS:
;
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
;
;============================================================================
;
function findlines_linefunc,x,COEFF=coeff

  return, sqrt( 1.0 + mc_poly1dderiv(x,coeff)^2)

end
;
;===========================================================================
;
function mc_findlines2d,img,edgecoeffs,xranges,orders,wlines,w2pcoeffs,offset, $
                        wranges,slitw_pix,slith_arc,linedeg,linewin,ystep, $
                        ysum,PLOTIMG=plotimg,PLOTFIT=plotfit,WID=wid, $
                        CANCEL=cancel
  
  cancel = 0

  if n_params() lt 14 then begin

     print, 'Syntax - '
     cancel = 1
     return,-1

  endif

  cancel = mc_cpar('mc_findlines2d',img,1,'Img',[2,3,4,5],2)
  if cancel then return,-1
  cancel = mc_cpar('mc_findlines2d',edgecoeffs,2,'Edgecoeffs',[2,3,4,5],[2,3])
  if cancel then return,-1
  cancel = mc_cpar('mc_findlines2d',xranges, 3,'Xranges',[2,3,4,5],[1,2])
  if cancel then return,-1
  cancel = mc_cpar('mc_findlines2d',orders, 4,'Orders',[2,3,4,5],[0,1])
  if cancel then return,-1
  cancel = mc_cpar('mc_findlines2d',wlines, 5,'Wlines',[2,3,4,5],1)
  if cancel then return,-1
  cancel = mc_cpar('mc_findlines2d',w2pcoeffs, 6,'W2pcoeffs',[2,3,4,5],[1,2])
  if cancel then return,-1
  cancel = mc_cpar('mc_findlines2d',offset, 7,'Offset',[2,3,4,5],0)
  if cancel then return,-1
  cancel = mc_cpar('mc_findlines2d',wranges,8,'Wranges',[2,3,4,5],[1,2])
  if cancel then return,-1
  cancel = mc_cpar('mc_findlines2d',slitw_pix,9,'Slitw_pix',[2,3,4,5],0)
  if cancel then return,-1
  cancel = mc_cpar('mc_findlines2d',slith_arc, 10,'Slith_arc',[2,3,4,5],0)
  if cancel then return,-1
  cancel = mc_cpar('mc_findlines2d',linedeg, 11,'Linedeg',[2,3,4,5],0)
  if cancel then return,-1
  cancel = mc_cpar('mc_findlines2d',linewin, 12,'Linewin',[2,3,4,5],0)
  if cancel then return,-1
  cancel = mc_cpar('mc_findlines2d',ystep, 13,'Ystep',[2,3,4,5],0)
  if cancel then return,-1
  cancel = mc_cpar('mc_findlines2d',ysum, 14,'Ysum',[2,3,4,5],0)
  if cancel then return,-1


;  plotimg = 0
;  plotfit = 0

  if keyword_set(WID) then wset, wid
  if keyword_set(plotfit) then window, 3

  if (keyword_set(plotimg) or keyword_set(plotfit)) and $
     keyword_set(WID) then delvarx,wid

  s = size(edgecoeffs)
  norders = (s[0] eq 2) ? 1:s[3]
  s = size(img,/DIMEN)
  ncols = s[0]
  nrows = s[1]

  warr = dblarr(ncols,nrows,/NOZERO)*!values.f_nan
  sarr = fltarr(ncols,nrows,/NOZERO)*!values.f_nan

  mkhdr,hdr,[[[warr]],[[sarr]]]

  ximg = rebin(findgen(ncols),ncols,nrows)
  yimg = rebin(reform(findgen(nrows),1,nrows),ncols,nrows)

  if keyword_set(plotfit) then window, 2
  if keyword_set(plotimg) then mc_mkct, 0, BOT=cmapbot
  
;  Loop over each order
  
  x = findgen(ncols)

  for i = 0,norders-1 do begin

     z = where(wlines gt wranges[0,i] and wlines lt wranges[1,i])
     wline = wlines[z]
     xgline = poly(wline,w2pcoeffs[*,i])+offset

     top = poly(x,edgecoeffs[*,1,i])
     bot = poly(x,edgecoeffs[*,0,i])
     del = top-bot
     mid = bot+del/2.
     
     tabinv,x,xgline,idx

     sx = 0D
     sy = 0D
     sl = 0D
     ss = 0D
     sw = 0D

     for j = 1,n_elements(idx)-1 do begin

        guessx = x[idx[j]]
        if guessx lt 4 or guessx gt ncols-5 then continue
        midslit = mid[idx[j]]
        
;  Get image for plotting purposes

        if keyword_set(plotimg) then begin
           
           xsize = 30
           ysize = del[idx[j]]+10
           
           subimg = mc_snipimgc(img,guessx,total(midslit),xsize,ysize, $
                                dncols,dnrows,xrange,yrange,CANCEL=cancel)
           
           window, 2,XSIZE=700*(xsize/float(ysize)),YSIZE=700
           mc_imgrange,subimg,0.99,min,max
           tvimage,mc_bytsclimg(subimg,0,cmapbot,MIN=min,MAX=max), $
                   POSITION=[0,0,1,1],/NOINTERP
           
           plot,[1],[1],/NODATA,/NOERASE,XSTY=5,YSTY=5, $
                XRANGE=[xrange[0]-0.5,xrange[1]+0.5], $
                YRANGE=[yrange[0]-0.5,yrange[1]+0.5],XMARGIN=[0,0], $
                YMARGIN=[0,0],COLOR=2,NOCLIP=0,POSITION=[0,0,1,1],XMINOR=1, $
                YMINOR=1

           plot2x = !x
           plot2y = !y
           plot2p = !p
           
           oplot,x,poly(x,edgecoeffs[*,1]),COLOR=3
           oplot,x,poly(x,edgecoeffs[*,0]),COLOR=3
           
           plotsym,0,0.5,/FILL
           plots,guessx,midslit,PSYM=8,COLOR=3
           
        endif

;  Now start the fitting
     
        tx = 0
        ty = 0
        
;  Move upwards from the center of the line

        for k = 0,nrows-1 do begin

;  Check to see if we are too close to the top of the slit.

           tabinv,x,guessx,topidx
           if midslit+k*ystep+2 ge top[topidx] then goto, godown
           
;  Check against no lines
           
           if k eq 5 and n_elements(tx) lt 3 then goto, noline
           
;  Plot guess

           if keyword_set(plotimg) then $
              plots,guessx,midslit+k*ystep,PSYM=6,COLOR=2,SYMSIZE=2

;  Pull out the small region around the line
          
           xsub = x[((guessx-linewin) > 0):((guessx+linewin) < (ncols-1))]
           ysub = median(img[((guessx-linewin) > 0):$
                             ((guessx+linewin) < (ncols-1)), $
                             (midslit+k*ystep-ysum):midslit+k*ystep+ysum], $
                         DIMEN=2)
           
;  Estimate the intensity of the line.
           
           tabinv,xsub,guessx,zidx
           guessz = ysub[zidx]
           
;  Fit the line with a 4 parameter gaussian 

           yfit = mpfitpeak(double(xsub),double(ysub),a,NTERMS=4,$
                            ESTIMATES=double([guessz,guessx, $
                                              slitw_pix/2.354,min(ysub)]))
           
;  Plot the fit if requested
           
           if keyword_set(plotfit) then begin
              
              wset, 3
              plot,xsub,ysub,/XSTY,/YSTY,PSYM=10, $
                   YRANGE=[min([ysub,yfit],MAX=max),max],TITLE=a[1]
              oplot,xsub,yfit,COLOR=3,PSYM=10
              plots,[guessx,guessx],!y.crange,COLOR=2
              plots,[a[1],a[1]],!y.crange,COLOR=3
              
           endif
           
;  Check to see if the fit is a good one.

;           if a[0] lt 0.0 or a[2] gt 2*slitw_pix/2.354 or $
           if a[0] lt 0.0 or $
              abs(a[1]-guessx) gt 2 then continue

           if keyword_set(plotimg) then begin
              
              wset, 2
              !x = plot2x
              !y = plot2y
              !p = plot2p
              plots,a[1],midslit+k*ystep,PSYM=2,COLOR=3
              
           endif

           if keyword_set(plotfit) or keyword_set(plotimg) then begin

              re = ' '
              read, re


           endif
           
           if keyword_set(WID) then plots,a[1],midslit+k*ystep,PSYM=6,COLOR=3
      
;  Store positions
           
           tx = [tx,a[1]]
           ty = [ty,midslit+k*ystep]
           
;  Get new guess position.  If just starting out, then simply use the
;  position determined by the fitting.  This works fine as long as the
;  line isn't really tilted.
           
           if n_elements(tx) gt 3 then begin
           
              coeff = robustpoly1d(ty[1:*],tx[1:*], $
                                   (n_elements(tx[1:*])< linedeg),4,0.1,/SILENT)
              guessx = poly(midslit+k*ystep,coeff)
              
           endif else guessx = a[1]
           
        endfor
        
        godown:
        
;  Move downwards
        
        for k =1,nrows-1 do begin
           
           coeff = poly_fit(ty[1:*],tx[1:*], linedeg)
           guessx = poly(midslit-k*ystep,coeff)
           if guessx lt 4 or guessx gt ncols-5 then continue
           
;  Are we too close to the bottom of the slit?

           tabinv,x,guessx,botidx
           if midslit-k*ystep-2 le bot[botidx] then goto, cont
           
;  Plot guess

           if keyword_set(plotimg) then $
              plots,guessx,midslit-k*ystep,PSYM=6,COLOR=2,SYMSIZE=2
           
;  Pull out the small region around the line

           xsub = x[((guessx-linewin) > 0):((guessx+linewin) < (ncols-1))]
           ysub = median(img[((guessx-linewin) > 0):$
                         ((guessx+linewin) < (ncols-1)), $
                  (midslit-k*ystep-ysum):midslit-k*ystep+ysum], $
                         DIMEN=2)

;  Estimate the intensity of the line.
        
           tabinv,xsub,guessx,zidx
           guessz = ysub[zidx]
           
;  Fit the line with a 4 parameter gaussian 

           yfit = mpfitpeak(double(xsub),double(ysub),a,PARINFO=parinfo, $
                            NTERMS=4,$
                            ESTIMATES=double([guessz,guessx, $
                                              slitw_pix/2.354,min(ysub)]))
           
;        yfit = gaussfit(xsub,ysub,a,NTERMS=3)

;  Plot the fit if requested
           
           if keyword_set(plotfit) then begin
           
              wset, 3
              plot,xsub,ysub,/XSTY,/YSTY,PSYM=10, $
                   YRANGE=[min([ysub,yfit],MAX=max),max]
              oplot,xsub,yfit,COLOR=3,PSYM=10
              plots,[guessx,guessx],!y.crange,COLOR=2
              plots,[a[1],a[1]],!y.crange,COLOR=3
              re = ' '
              read, re
              
           endif

           if keyword_set(plotfit) or keyword_set(plotimg) then begin

              re = ' '
              read, re


           endif
           
;  Check to see if the fit is a good one.
           
;           if a[0] lt 0.0 or a[2] gt 2*slitw_pix/2.354 or $
           if a[0] lt 0.0 or $
              abs(a[1]-guessx) gt 2 then continue
           
           if keyword_set(plotimg) then begin
              
              wset, 2
              !x = plot2x
              !y = plot2y
              !p = plot2p
              plots,a[1],midslit-k*ystep,PSYM=2,COLOR=3
              
           endif

           if keyword_set(WID) then plots,a[1],midslit-k*ystep,PSYM=6,COLOR=3

;  Get new positions

           tx = [tx,a[1]]
           ty = [ty,midslit-k*ystep]
           
        endfor
        
        cont:
        
;  Get intercepts of line and edges of the order.
        
        tx = tx[1:*]            ;  ignore first zero 
        ty = ty[1:*]            ;  ignore first zero 
        s = sort(tx)
        tx = tx[s]
        ty = ty[s]

;  Fit line 

        linecoeffs = robustpoly1d(ty,tx,linedeg,3.5,0.01,/SILENT, $
                                  OGOODBAD=ogoodbad,YFIT=test,CANCEL=cancel)
        if cancel then return,-1

;  Plot fit if requested.

        if keyword_set(plotimg) then begin
           
           wset, 2
           !x = plot2x
           !y = plot2y
           !p = plot2p
           
           s = sort(ty)
           togoodbad = ogoodbad[s]
           ttx = tx[s]
           tty = ty[s]
           
           oplot,poly(tty,linecoeffs),tty,COLOR=3
           z = where(togoodbad eq 0,cnt)
           if cnt ne 0 then oplot,ttx[z],tty[z],PSYM=2,COLOR=4

        endif
        
;  Top intercept
        
        xline = poly(top,linecoeffs)
        dif = x-xline
        linterp,dif,x,0,xtop
        ytop = poly([xtop],edgecoeffs[*,1,i])
        
;  Bottom intercept
        
        xline = poly(bot,linecoeffs)
        dif = x-xline
        linterp,dif,x,0,xbot
        ybot = poly([xbot],edgecoeffs[*,0,i])

;  Middle intercept
     
        xline = poly(mid,linecoeffs)
        dif = x-xline
        linterp,dif,x,0,xmid
        linterp,x,mid,xmid,ymid

;        if keyword_set(WID) then plots,xmid,ymid,PSYM=6,COLOR=2
           
;  Plot points if requested

        if keyword_set(plotimg) then begin
           
           plotsym,0,2,/FILL
           plots,xtop,ytop,PSYM=8,COLOR=3
           plots,xbot,ybot,PSYM=8,COLOR=3
           re = ' '
           read, re
           
        endif
        
;  Integrate up the line to find the total length of the line
        
        qsimp,'findlines_linefunc',ybot[0],ytop[0],linelength,COEFF=linecoeffs
        
;  Identify the fitted points that are good.
        
        z = where(ogoodbad eq 1,cnt)
        
;  Integrate from the bottom of the line to each good point to find
;  the fractional length (length/total line length).  Then store
;  x,y,xmid,length/(total line length)*slith_arc.

        tsx = 0 & tsy = 0 & tsl = 0 & tss = 0
        for k = 0,cnt-1 do begin
           
           qsimp,'findlines_linefunc',ybot[0],ty[z[k]],val,COEFF=linecoeffs
           tsx = [tsx,tx[z[k]]]
           tsy = [tsy,ty[z[k]]]
           tsl  = [tsl,xmid]
           tss = [tss,val/linelength*slith_arc]
           
        endfor

        tsx = tsx[1:*]
        tsy = tsy[1:*]
        tsl = tsl[1:*]
        tss = tss[1:*]

        s = sort(tss)
        tsx = tsx[s]
        tsy = tsy[s]
        tsl = tsl[s]
        tss = tss[s]

        sx = [sx,tsx]
        sy = [sy,tsy]
        sl = [sl,tsl]
        ss = [ss,tss]
        
        sw = [sw,replicate(wlines[j],n_elements(tsx))]
        
        noline:
        
     endfor
     
;  Store results in data structure
     
     sx = sx[1:*]
     sy = sy[1:*]
     ss = ss[1:*]
     sl = sl[1:*]
     sw = sw[1:*]

     name = 'order'+string(orders[i],FORMAT='(I2.2)')

     wcmap = (i eq 0) ? create_struct(name,{sx:sx,sy:sy,ss:ss,sl:sl,sw:sw}):$
             create_struct(wcmap, $
                           create_struct(name,{sx:sx,sy:sy,ss:ss,sl:sl,sw:sw}))
     
  endfor

  return, wcmap

end
