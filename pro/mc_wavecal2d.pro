;+
; NAME:
;     mc_wavecal2d
;
; PURPOSE:
;     To wavelength calibrate a spectral image in 2D.
;
; CALLING SEQUENCE:
;     mc_wavecal2,wcinfo,wdx,wdy,sdx,sdy,oname,ps,psfile,SILENT=silent,$
;                 CANCEL=cancel
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
;     ps         - The plate scale in arcsec pixel-1
;     psfile     - The name of a file to display the residuals in the fit.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     SILENT  - Set to report on lines identified as bad.
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
pro mc_wavecal2d,wcinfo1d,wcinfo2d,edgecoeffs,omask,orders,xranges,rotation, $
                 wdx,wdy,sdx,sdy,oname,ps,psfile,SILENT=silent, $
                 WIDGET_ID=widget_id,OPLOT=oplot,CANCEL=cancel
  
  cancel = 0

  if n_params() lt 13 then begin

     print, 'Syntax - '
     cancel = 1
     return

  endif

  if keyword_set(WIDGET_ID) then widget_control, widget_id, SENSITIVE=0

  norders = n_elements(orders)
  s = size(omask,/DIMEN)
  ncols = s[0]
  nrows = s[1]

  warr = dblarr(ncols,nrows,/NOZERO)*!values.f_nan
  sarr = fltarr(ncols,nrows,/NOZERO)*!values.f_nan

  xdwavecal = (n_tags(wcinfo1d) gt 2) ? 1:0
  
;  Defaults for the OPLOT arrays

  pxs = 0
  pys = 0
  pxl = 0
  pyl = 0

; Add header info

  mkhdr,hdr,[[[mc_unrotate(warr,rotation)]],[[mc_unrotate(sarr,rotation)]]]
  fxaddpar,hdr,'WCTYPE','2D',' Wavelength calibration type'

  fxaddpar,hdr,'WDEG',fix(wcinfo1d.dispdeg), $
           ' X Polynomial degree for 1D wavelength calibration'

  if xdwavecal then begin

     fxaddpar,hdr,'ODEG',fix(wcinfo1d.ordrdeg), $
              ' Y Polynomial degree for 1D wavelength calibration'
     
     sxaddpar,hdr,'HOMEORDR',string(wcinfo1d.homeorder,FORMAT='(I2.2)'), $
              ' Home order for 1D wavelength calibration'
  
  endif

  fxaddpar,hdr,'WXDEG',fix(wdx), $
           ' X Polynomial degree for 2D "wavelength" distortion calibration'
  fxaddpar,hdr,'WYDEG',fix(wdy), $
           ' Y Polynomial degree for 2D "wavelength" distortion calibration'
  fxaddpar,hdr,'SXDEG',fix(sdx), $
           ' X Polynomial degree for 2D "spatial" distortion calibration'
  fxaddpar,hdr,'SYDEG',fix(sdy), $
           ' Y Polynomial degree for 2D "spatial" distortion calibration'

  fxaddpar,hdr,'NORDERS',norders,' Number of orders'
  fxaddpar,hdr,'ORDERS',strjoin(strtrim(fix(orders),2),','),' Order numbers'
  
;  Create arrays of x and y values

  ximg = rebin(findgen(ncols),ncols,nrows)
  yimg = rebin(reform(findgen(nrows),1,nrows),ncols,nrows)

;  Set up plotting
     
  mc_setpsdev,psfile,10.5,8,/LANDSCAPE,FONT=13
  positions = mc_gridplotpos([0.1,0.08,0.97,0.92],[2,2],0.08,/COL)
  plotsym,0,0.5,/FILL

;  Loop over each order

  badslines = ''
  badwlines = ''
  for i = 0,norders-1 do begin

     x = findgen(xranges[1,i]-xranges[0,i]+1)+xranges[0,i]

     oyimg = yimg-rebin(poly(findgen(ncols),edgecoeffs[*,0,i]),ncols,nrows)
    
     sx = wcinfo2d.(i).sx
     sy = wcinfo2d.(i).sy-poly(sx,edgecoeffs[*,0,i])
     sl = wcinfo2d.(i).sl
     ss = wcinfo2d.(i).ss
     sw = wcinfo2d.(i).sw

;  Fit the spatial coordinates versus xy

     scoeff = robustpoly2d(sx,sy,ss,sdx,sdy,4,0.01,OGOODBAD=sogoodbad, $
                           /SILENT,CANCEL=cancel)
     if cancel then goto, out
     
;  Fit the wavelengths versus xy
     
     wcoeff = robustpoly2d(sx,sy,sl,wdx,wdy,4,0.01,OGOODBAD=logoodbad, $
                           /SILENT,CANCEL=cancel)
     if cancel then goto, out 

;  Fill in the wavelength and spatial 2D output arrays

     z = where(omask eq orders[i],cnt)
     xcen = poly2d(ximg[z],oyimg[z],wdx,wdy,wcoeff)

     
     if xdwavecal then begin

        warr[z] = poly2d(xcen,replicate(orders[i],cnt),wcinfo1d.dispdeg,$
                         wcinfo1d.ordrdeg,wcinfo1d.coeffs)/ $
                  float(orders[i])*wcinfo1d.homeorder

     endif else warr[z] = poly(xcen,wcinfo1d.coeffs)

     sarr[z] = poly2d(ximg[z],oyimg[z],sdx,sdy,scoeff)

;  Get residuals

     sb = where(sogoodbad eq 0,COMP=sg,scnt)
     sm = moment((ss-poly2d(sx,sy,sdx,sdy,scoeff))[sg])

     lb = where(logoodbad eq 0,COMP=lg,lcnt)
     lm = moment((sl-poly2d(sx,sy,wdx,wdy,wcoeff))[lg])
     
     if ~keyword_set(SILENT) then begin

        if scnt ne 0 then $
           badslines = badslines+strjoin(strtrim(sw[sb[uniq(sw[sb])]],2),', ')
        if lcnt ne 0 then $
           badwlines = badwlines+strjoin(strtrim(sw[lb[uniq(sw[lb])]],2),', ')

     endif

;  Get OPLOT output

     if scnt ne 0 then begin

        pxs = [pxs,sx[sb]]
        pys = [pys,sy[sb]+poly(sx[sb],edgecoeffs[*,0,i])]

     endif

     if lcnt ne 0 then begin

        pxl = [pxl,sx[sl]]
        pyl = [pyl,sy[sl]+poly(sx[sl],edgecoeffs[*,0,i])]

     endif


;  Add XD 1D calibration coefficients to the header

     if xdwavecal then begin
     
        for j = 0,(wcinfo1d.dispdeg+1)*(wcinfo1d.ordrdeg+1)-1 do begin
           
           name    = '1DWC'+string(j+1,FORMAT='(i2.2)')
           comment = ' a'+string(j+1,FORMAT='(i2.2)')+ $
                     ' 1D wavecal coefficient'
           
           sxaddpar,hdr,name,(wcinfo1d.coeffs)[j],comment
        
        endfor

     endif else begin

        for j = 0,(wcinfo1d.dispdeg+1)-1 do begin
           
           name    = '1DWC'+string(j+1,FORMAT='(i2.2)')
           comment = ' a'+string(j+1,FORMAT='(i2.2)')+ $
                     ' 1D wavecal coefficient'
           
           sxaddpar,hdr,name,(wcinfo1d.coeffs)[j],comment

        endfor

     endelse
        
;  Add 2D calibration coefficients to the hdr

     for j = 0,(wdx+1)*(wdy+1)-1 do begin
	
        name    = 'OR'+string(orders[i],FORMAT='(i2.2)') + $
                  'WC'+string(j+1,FORMAT='(i2.2)')
        comment = ' a'+string(j+1,FORMAT='(i2.2)')+ $
                  ' 2D "wavecal" distortion coefficient for order '+ $
                  string(orders[i],FORMAT='(i2.2)')
        
        sxaddpar,hdr,name,wcoeff[j],comment

     endfor

     for j = 0,(sdx+1)*(sdy+1)-1 do begin
	
        name    = 'OR'+string(orders[i],FORMAT='(i2.2)') + $
                  'SC'+string(j+1,FORMAT='(i2.2)')
        comment = ' a'+string(j+1,FORMAT='(i2.2)')+ $
                  ' 2D "spatial" distortion coefficient for order '+ $
                  string(orders[i],FORMAT='(i2.2)')
        
        sxaddpar,hdr,name,scoeff[j],comment

     endfor

;  Plot dS versus Y

     plot,sx,(ss-poly2d(sx,sy,sdx,sdy,scoeff))/ps,PSYM=8,/XSTY,/YSTY, $
          XRANGE=xranges[*,i],XTITLE='X (pixels)', $
          YTITLE='Spatial-Model (pixels)',FONT=0,POSITION=positions[*,0], $
          TITLE='RMS = '+string(sqrt(sm[1]/ps),FORMAT='(f6.4)')+' pixels'
     
     if scnt ne 0 then $
        oplot,sx[sb],(ss-poly2d(sx,sy,sdx,sdy,scoeff))[sb]/ps,PSYM=8,COLOR=4

     plots,!x.crange,[0,0],COLOR=2
     plots,!x.crange,[sm[0],sm[0]]/ps,LINESTYLE=2,COLOR=2
     plots,!x.crange,([sm[0],sm[0]]-sqrt(sm[1]))/ps,LINESTYLE=1,COLOR=2
     plots,!x.crange,([sm[0],sm[0]]+sqrt(sm[1]))/ps,LINESTYLE=1,COLOR=2
     
     
;  Plot Y versus dS
     
     plot,(ss-poly2d(sx,sy,sdx,sdy,scoeff))/ps,sy,PSYM=8,/XSTY,/YSTY, $
          YTITLE='Y (pixels)',XTITLE='Spatial-Model (pixels)',FONT=0, $
          YRANGE=[min(sy,MAX=max),max],POSITION=positions[*,1],/NOERASE
     
     
     if scnt ne 0 then $
        oplot,(ss-poly2d(sx,sy,sdx,sdy,scoeff))[sb]/ps,sy[sb],PSYM=8,COLOR=4
     plots,[0,0],!y.crange,COLOR=2
     plots,[sm[0],sm[0]]/ps,LINESTYLE=2,COLOR=2
     plots,([sm[0]-sqrt(sm[1]),sm[0]-sqrt(sm[1])])/ps,!y.crange,LINESTYLE=1, $
           COLOR=2
     plots,([sm[0]+sqrt(sm[1]),sm[0]+sqrt(sm[1])])/ps,!y.crange,LINESTYLE=1, $
           COLOR=2
     
     
; Plot dW versus X
     
     plot,sx,(sl-poly2d(sx,sy,wdx,wdy,wcoeff)),PSYM=8,/XSTY,/YSTY, $
          XRANGE=xranges[*,i],XTITLE='X (pixels)', $
          YTITLE='Wavelength-Model (pixels)',FONT=0,$
          POSITION=positions[*,2],/NOERASE,$
          TITLE='RMS = '+string(sqrt(lm[1]),FORMAT='(f6.3)')+' pixels'
     
     if lcnt ne 0 then $
        oplot,sx[lb],(sl-poly2d(sx,sy,wdx,wdy,wcoeff))[lb],PSYM=8,COLOR=4
     plots,!x.crange,[0,0],COLOR=2
     
     
     plots,!x.crange,[lm[0],lm[0]],LINESTYLE=2,COLOR=2
     plots,!x.crange,[lm[0],lm[0]]-sqrt(lm[1]),LINESTYLE=1,COLOR=2
     plots,!x.crange,[lm[0],lm[0]]+sqrt(lm[1]),LINESTYLE=1,COLOR=2
     
; Plot dW versus Y
     
     plot,(sl-poly2d(sx,sy,wdx,wdy,wcoeff)),sy,PSYM=8,/XSTY,/YSTY, $
          YRANGE=[min(sy,MAX=max),max],YTITLE='Y (pixels)', $
          XTITLE='Wavelength-Model (pixels)',FONT=0,$
          POSITION=positions[*,3],/NOERASE
     
     if lcnt ne 0 then $
        oplot,((sl-poly2d(sx,sy,wdx,wdy,wcoeff)))[lb],sy[lb],PSYM=8,COLOR=4
     
     plots,[0,0],!y.crange,COLOR=2
     
     plots,[lm[0],lm[0]],!y.crange,LINESTYLE=2,COLOR=2
     plots,[lm[0],lm[0]]-sqrt(lm[1]),!y.crange,LINESTYLE=1,COLOR=2
     plots,[lm[0],lm[0]]+sqrt(lm[1]),!y.crange,LINESTYLE=1,COLOR=2
     
     xyouts,0.5,0.95, 'Order '+string(orders[i],FORMAT='(I2.2)'), $
            ALIGNMENT=0.5,FONT=0,/NORM,CHARSIZE=2
  
  endfor
  
  writefits,oname,[[[mc_unrotate(warr,rotation)]],$
                   [[mc_unrotate(sarr,rotation)]]],hdr     
  
  if ~keyword_set(SILENT) then begin
     
     print
     print, 'The following lines had points identified as bad in the ' + $
            'spatial fit:'
     print
     print, mc_splittext(badslines,80) 
     print
     print, 'The following lines had points identified as bad in the ' + $
            'wavelength fit:'
     print
     print, mc_splittext(badwlines,80)
     print
     
  endif


out:
  
  mc_setpsdev,/CLOSE
  if keyword_set(WIDGET_ID) then widget_control, widget_id, SENSITIVE=1

;  Do oplot

  dooplot = 0

  if n_elements(pxs) ne 1 then begin

     a = obj_new('mcoplot')
     a->set,pxs[1:*],pys[1:*],PSYM=1,COLOR=2
     dooplot=1

  endif

  if n_elements(pxl) ne 1 then begin

     b = obj_new('mcoplot')
     b->set,pxl[1:*],pyl[1:*],PSYM=5,COLOR=4
     dooplot = dooplot+2

  endif 

  case dooplot of

     1:  oplot = [a]
     2:  oplot = [b]
     3:  oplot = [a,b]

     else:

  endcase  

end
