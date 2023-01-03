;+
; NAME:
;     mc_wavecal2d
;
; PURPOSE:
;     To wavelength calibrate a spectral image in 2D.
;
; CALLING SEQUENCE:
;     mc_wavecal2,wcinfo,wdx,wdy,sdx,sdy,oname,[psfile],CANCEL=cancel
;
; INPUTS:
;     wcinfo     - 
;     wdx        - The x degree of the 2D polynomial used to fit the
;                  xy positions of the lines as a function of
;                  wavelength.
;     wdy        - The y degree of the 2D polynomial used to fit the
;                  xy positions of the lines as a function of
;                  wavelength.
;     sdx        - The x degree of the 2D polynomial used to fit the
;                  xy positions of the lines as a function of
;                  the spatial coordinate.
;     sdy        - The y degree of the 2D polynomial used to fit the
;                  xy positions of the lines as a function of the
;                  spatial coordiate.
;     oname      - The output wavecal file name.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL  - Set on return if there is a problem.
;
; OUTPUTS:
;     Writes an [ncols,nrows,2] FITS image where the first image is
;     the wavelength calibration and the second image is the spatial 
;     calibration.  The coefficients of the fits are also included in
;     the FITS header.
;
; OPTIONAL OUTPUTS:
;     See PSFILE.
;
; COMMON BLOCKS:
;     None
;
; RESTRICTIONS:
;     None
;
; DEPENDENCIES:
;
;     
; PROCEDURE:
;
;
; EXAMPLES:
;     yeah right.
;
; MODIFICATION HISTORY:
;     2009-11-10 - Written by M. Cushing, JPL
;-
;
;===========================================================================
;
pro mc_wavecal2d,wcinfo,omask,orders,xranges,rotation,wdx,wdy,sdx,sdy, $
                 oname,psfile,WIDGET_ID=widget_id,CANCEL=cancel
  
  cancel = 0

  if n_params() lt 10 then begin

     print, 'Syntax - '
     cancel = 1
     return

  endif

;  cancel = mc_cpar('mc_wavecal2d',wcinfo, 1,'Wcinfo',8,0)
;  if cancel then return
;  cancel = mc_cpar('mc_wavecal2d',ncols, 2,'Sdx',[2,3,4,5],0)
;  if cancel then return
;  cancel = mc_cpar('mc_wavecal2d',nrows, 3,'Sdy',[2,3,4,5],0)
;  if cancel then return
;
;
;  cancel = mc_cpar('mc_wavecal2d',sdx, 2,'Sdx',[2,3,4,5],0)
;  if cancel then return
;  cancel = mc_cpar('mc_wavecal2d',sdy, 3,'Sdy',[2,3,4,5],0)
;  if cancel then return
;  cancel = mc_cpar('mc_wavecal2d',wdx, 4,'Wdx',[2,3,4,5],0)
;  if cancel then return
;  cancel = mc_cpar('mc_wavecal2d',wdy, 5,'Wdy',[2,3,4,5],0)
;  if cancel then return
;  cancel = mc_cpar('mc_wavecal2d',oname, 6,'Oname',[7],0)
;  if cancel then return
;
;  if n_params() eq 6 then begin
;
;     cancel = mc_cpar('mc_wavecal2d',psfile, 7,'Psfile',[7],0)
;     if cancel then return
;     
;  endif

  if keyword_set(WIDGET_ID) then widget_control, widget_id, SENSITIVE=0

  norders = n_elements(orders)
  s = size(omask,/DIMEN)
  ncols = s[0]
  nrows = s[1]

  warr = dblarr(ncols,nrows,/NOZERO)*!values.f_nan
  sarr = fltarr(ncols,nrows,/NOZERO)*!values.f_nan
  
  mkhdr,hdr,[[[mc_unrotate(warr,rotation)]],[[mc_unrotate(sarr,rotation)]]]
  fxaddpar,hdr,'WTYPE','2D',' Wavelength calibration type'

  fxaddpar,hdr,'WXDEG',fix(wdx), $
           ' X Polynomial degree for wavelength calibration'
  fxaddpar,hdr,'WYDEG',fix(wdy), $
           ' Y Polynomial degree for wavelength calibration'
  fxaddpar,hdr,'SXDEG',fix(sdx), $
           ' X Polynomial degree for spatial calibration'
  fxaddpar,hdr,'SYDEG',fix(sdy), $
           ' Y Polynomial degree for spatial calibration'

  fxaddpar,hdr,'NORDERS',norders,' Number of orders'
  fxaddpar,hdr,'ORDERS',strjoin(strtrim(fix(orders),2),','),' Order numbers'
  
  ximg = rebin(findgen(ncols),ncols,nrows)
  yimg = rebin(reform(findgen(nrows),1,nrows),ncols,nrows)

;  Set up plotting
     
  if n_params() eq 11 then begin

     mc_setpsdev,psfile,10.5,8,/LANDSCAPE,FONT=13
     positions = mc_gridplotpos([0.1,0.08,0.97,0.92],[2,2],0.08,/COL)
     plotsym,0,0.5,/FILL

  endif

;  Loop over each order

  wavescale = 10000.
  for i = 0,norders-1 do begin

     x = findgen(xranges[1,i]-xranges[0,i]+1)+xranges[0,i]
     
     sx = wcinfo.(i).sx
     sy = wcinfo.(i).sy
     sw = wcinfo.(i).sw
     ss = wcinfo.(i).ss
     
;  Fit the spatial coordinates versus xy

     scoeff = robustpoly2d(sx,sy,ss,sdx,sdy,4,0.01,OGOODBAD=sogoodbad, $
                           /SILENT,CANCEL=cancel)
     if cancel then goto, out
     
;  Fit the wavelengths versus xy
     
     wcoeff = robustpoly2d(sx,sy,sw,wdx,wdy,4,0.01,OGOODBAD=logoodbad, $
                           /SILENT,CANCEL=cancel)
     if cancel then goto, out 

;  Fill in the wavelength and spatial 2D output arrays

     z = where(omask eq orders[i])
     warr[z] = poly2d(ximg[z],yimg[z],wdx,wdy,wcoeff)
     sarr[z] = poly2d(ximg[z],yimg[z],sdx,sdy,scoeff)

;  Get RMS values

     sb = where(sogoodbad eq 0,COMP=sg,scnt)
     sm = moment((ss-poly2d(sx,sy,sdx,sdy,scoeff))[sg]*wavescale)
     
     fxaddpar,hdr,'SpatRMS',sqrt(sm[1]),' RMS (Spatial-Model) [mas]'

     lb = where(logoodbad eq 0,COMP=lg,lcnt)
     lm = moment((sw-poly2d(sx,sy,wdx,wdy,wcoeff))[lg]*wavescale)
     print, sqrt(lm[1])
     fxaddpar,hdr,'WaveRMS',sqrt(lm[1]),' RMS (Wavelength-Model) [pixels]'

;  Add coefficients to the hdr

     for j = 0,(wdx+1)*(wdy+1)-1 do begin
	
        name    = 'OR'+string(orders[i],FORMAT='(i2.2)') + $
                  'WC'+string(j+1,FORMAT='(i2.2)')
        comment = ' a'+string(j,FORMAT='(i2.2)')+ $
                  ' 2D wavecal coefficient for order '+ $
                  string(orders[i],FORMAT='(i2.2)')
        
        sxaddpar,hdr,name,wcoeff[j],comment

     endfor

     for j = 0,(sdx+1)*(sdy+1)-1 do begin
	
        name    = 'OR'+string(orders[i],FORMAT='(i2.2)') + $
                  'SC'+string(j+1,FORMAT='(i2.2)')
        comment = ' a'+string(j,FORMAT='(i2.2)')+ $
                  ' 2D spatial coefficient for order '+ $
                  string(orders[i],FORMAT='(i2.2)')
        
        sxaddpar,hdr,name,scoeff[j],comment

     endfor

     if n_params() eq 11 then begin

;  Plot dS versus Y

        plot,sx,(ss-poly2d(sx,sy,sdx,sdy,scoeff))*wavescale,PSYM=8,/XSTY, $
             /YSTY, $
             XRANGE=xranges[*,i],XTITLE='X (pixels)', $
             YTITLE='Spatial-Model (mas)',FONT=0,POSITION=positions[*,0], $
             TITLE='RMS = '+string(sqrt(sm[1]),FORMAT='(f5.2)')+' mas'
        
        if scnt ne 0 then $
           oplot,sx[sb],(ss-poly2d(sx,sy,sdx,sdy,scoeff))[sb]*wavescale, $
                 PSYM=8,COLOR=4
        plots,!x.crange,[0,0],COLOR=2
        plots,!x.crange,[sm[0],sm[0]],LINESTYLE=2,COLOR=2
        plots,!x.crange,[sm[0],sm[0]]-sqrt(sm[1]),LINESTYLE=1,COLOR=2
        plots,!x.crange,[sm[0],sm[0]]+sqrt(sm[1]),LINESTYLE=1,COLOR=2
        
 
;  Plot Y versus dS

        plot,(ss-poly2d(sx,sy,sdx,sdy,scoeff))*wavescale,sy,PSYM=8,/XSTY, $
             /YSTY, $
             YTITLE='Y (pixels)',XTITLE='Spatial-Model (mas)',FONT=0, $
             YRANGE=[min(sy,MAX=max),max],POSITION=positions[*,1],/NOERASE


        if scnt ne 0 then $
           oplot,(ss-poly2d(sx,sy,sdx,sdy,scoeff))[sb]*wavescale,sy[sb], $
                 PSYM=8,COLOR=4
        plots,[0,0],!y.crange,COLOR=2
        plots,[sm[0],sm[0]],LINESTYLE=2,COLOR=2
        plots,[sm[0]-sqrt(sm[1]),sm[0]-sqrt(sm[1])],!y.crange,LINESTYLE=1, $
              COLOR=2
        plots,[sm[0]+sqrt(sm[1]),sm[0]+sqrt(sm[1])],!y.crange,LINESTYLE=1, $
              COLOR=2


; Plot dW versus X

        plot,sx,(sw-poly2d(sx,sy,wdx,wdy,wcoeff))*wavescale,PSYM=8,/XSTY, $
             /YSTY, $
             XRANGE=xranges[*,i],XTITLE='X (pixels)', $
             YTITLE='Wavelength-Model (pixels)',FONT=0,$
             POSITION=positions[*,2],/NOERASE,$
             TITLE='RMS = '+string(sqrt(lm[1]),FORMAT='(f6.3)')+' pixels'

        if lcnt ne 0 then $
           oplot,sx[lb],(sw-poly2d(sx,sy,wdx,wdy,wcoeff))[lb]*wavescale, $
                 PSYM=8,COLOR=4
        plots,!x.crange,[0,0],COLOR=2


        plots,!x.crange,[lm[0],lm[0]],LINESTYLE=2,COLOR=2
        plots,!x.crange,[lm[0],lm[0]]-sqrt(lm[1]),LINESTYLE=1,COLOR=2
        plots,!x.crange,[lm[0],lm[0]]+sqrt(lm[1]),LINESTYLE=1,COLOR=2

; Plot dW versus Y

        plot,(sw-poly2d(sx,sy,wdx,wdy,wcoeff))*wavescale,sy,PSYM=8,/XSTY, $
             /YSTY, $
             YRANGE=[min(sy,MAX=max),max],YTITLE='Y (pixels)', $
             XTITLE='Wavelength-Model (pixels)',FONT=0,$
             POSITION=positions[*,3],/NOERASE

        if lcnt ne 0 then $
           oplot,((sw-poly2d(sx,sy,wdx,wdy,wcoeff))*wavescale)[lb],sy[lb], $
                 PSYM=8,COLOR=4
     
        plots,[0,0],!y.crange,COLOR=2
        
        plots,[lm[0],lm[0]],!y.crange,LINESTYLE=2,COLOR=2
        plots,[lm[0],lm[0]]-sqrt(lm[1]),!y.crange,LINESTYLE=1,COLOR=2
        plots,[lm[0],lm[0]]+sqrt(lm[1]),!y.crange,LINESTYLE=1,COLOR=2
        
        xyouts,0.5,0.95, 'Order '+string(orders[i],FORMAT='(I2.2)'), $
               ALIGNMENT=0.5,FONT=0,/NORM,CHARSIZE=2

     endif
     
  endfor

  writefits,oname,[[[mc_unrotate(warr,rotation)]],$
                   [[mc_unrotate(sarr,rotation)]]],hdr     

  out:

  if n_params() eq 11 then mc_setpsdev,/CLOSE
  if keyword_set(WIDGET_ID) then widget_control, widget_id, SENSITIVE=1


end
