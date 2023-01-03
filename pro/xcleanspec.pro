; NAME:
;     xcleanspec
;    
; PURPOSE:
;     Cleans SpeX spectra interactively.
;    
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     xcleanspec
;    
; INPUTS:
;     None
;    
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     None
;     
; OUTPUTS:
;     Writes a SpeX FITS file out with the cleaned spectra
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
;     Only works with SpeX spectra FITS format
;
; PROCEDURE:
;     Allows the user to either remove bad pixels or replace bad pixels.
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2002       - Written by M. Cushing, Institute for Astronomy, UH
;     2004-05-19 - Removed Change <R> and replaced with Gaussian smooth
;                  by slit width
;     2005-02-10 - Changed lower plot to S/N
;     2005-12-17 - Fixed bug where the yrange in the lower plot would
;                  not readjust after a smoothing.
;     2008-02-22 - Modified to read in an atmospheric transmission
;                  curve with a resolving power appropriate for the
;                  given observations.  Also added a scale factor for
;                  the atmospheric transmission curve.
;-
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xcleanspec_cleanup,base

  widget_control, base, GET_UVALUE=state, /NO_COPY
  if n_elements(state) ne 0 then begin

     ptr_free, state.r.orders
     ptr_free, state.d.hdr
     ptr_free, state.d.espec
     ptr_free, state.d.ospec
     ptr_free, state.d.pspec
     ptr_free, state.p.atrans
     ptr_free, state.p.awave

  endif

  state = 0B

end
;
;******************************************************************************
;
pro xcleanspec_loadspec,state

;  Get files.

  spemc_cfile = mc_cfld(state.w.ispectrum_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return
  spemc_cfile = mc_cfile(spemc_cfile,WIDGET_ID=state.w.xcleanspec_base, $
                         CANCEL=cancel)
  if cancel then return

  state.r.spemc_cfile = spemc_cfile

;  Read spectra.

  mc_readspec,spemc_cfile,spc,hdr,obsmode,start,stop,norders,naps,orders,$
              xunits,yunits,slith_pix,slith_arc,slitw_pix,slitw_arc,$
              rp,airmass,xtitle,ytitle,CANCEL=cancel
  if cancel then return

  spc = mc_caaspecflags(spc,CANCEL=cancel)
  if cancel then return
  
;  Store spectra 

  *state.d.ospec    = spc
  *state.d.espec    = spc
  *state.d.pspec    = spc
  *state.d.hdr      = hdr          
  *state.r.orders   = orders
  state.r.norders   = norders
  state.r.naps      = naps
  state.r.slitw_pix = slitw_pix
  state.r.start     = 0
  state.r.stop      = n_elements(spc[*,0,0])-1

;  Update the order and aperture droplists and set up first spectrum

  widget_control, state.w.order_dl, SET_VALUE=string(orders,FORMAT='(i2.2)')
  widget_control, state.w.aperture_dl, SET_VALUE=string(indgen(naps)+1,$
                                                        FORMAT='(i2.2)')

  state.p.order = orders[0]
  state.p.ap    = 1
  state.p.idx   = 0

;  Get plot info

  state.p.xtitle = xtitle
  state.p.ytitle = ytitle


  smooth = mc_robustsg(findgen(state.r.stop+1),reform(spc[*,1,0]),5,3,0.1, $
                       CANCEL=cancel)
  if cancel then return
 
  state.p.plot1xrange = [min(spc[*,0,0],MAX=max),max]
  state.p.plot1yrange = [[min(smooth[*,1,0],MAX=max,/NAN),max], $
                         [0,max(spc[*,2,0],/NAN)]]

  state.p.plot2yrange = [min(spc[0,1,0]/spc[0,2,0],MAX=max,/NAN),max]

  state.p.plot1absxrange  = state.p.plot1xrange
  state.p.plot1absyrange = state.p.plot1yrange
  state.p.plot2absyrange  = state.p.plot2yrange

  scale = (slitw_pix gt 2) ? 1.5:2.5

  widget_control, state.w.sgwin_fld[1],$
                  SET_VALUE=strtrim(string(slitw_pix*scale,FORMAT='(f4.1)'),2)

  widget_control, state.w.fwhm_fld[1],SET_VALUE=strtrim(state.r.slitw_pix,2)

;  Load the atmospheric transmission

  if rp ge 10000.0 then trp = round(rp/10000.)*10000
  if rp lt 10000.0 and rp ge 1000.0 then trp = round(rp/1000.)*1000
  if rp lt 1000.0 and rp ge 100.0 then trp = round(rp/100.)*100
  if rp lt 100.0 and rp ge 10.0 then trp = 100
  if rp eq 0 then trp=2000
  
  spec = readfits(filepath('atran'+strtrim(trp,2)+'.fits', $
                           ROOT_DIR=state.r.spextoolpath,SUBDIR='data'))
  *state.p.awave = reform(spec[*,0])
  *state.p.atrans = reform(spec[*,1])

;  Unfreeze the widget and upgrey box 3. if this is a reload.

  state.w.freeze = 0
  widget_control, state.w.plotwin1, DRAW_BUTTON_EVENTS=1
  widget_control, state.w.plotwin2, DRAW_BUTTON_EVENTS=1

  widget_control, state.w.box3_base,SENSITIVE=1

;  Plot first spectrum

  xcleanspec_plotupdate,state
  xcleanspec_setminmax,state

  state.r.smthhistory = ''

out:

end
;
;******************************************************************************
;
pro xcleanspec_plotsn,state

  z = where(finite((*state.d.pspec)[*,0,state.p.idx]) eq 1)

  wave = (*state.d.pspec)[z,0,state.p.idx]
  sn = (*state.d.pspec)[z,1,state.p.idx]/(*state.d.pspec)[z,2,state.p.idx]
  flag = (*state.d.pspec)[z,3,state.p.idx]


  plot,wave,sn,PSYM=10,/XSTY,/YSTY,$
       XRANGE=state.p.plot1xrange,YRANGE=state.p.plot2yrange,$
       XTITLE=state.p.xtitle,YTITLE='!5S/N',CHARSIZE=1.5,/NOERASE

;  Plot flags
  
  if state.p.plotreplacepixel then begin
     
     mask = mc_bitset(fix(flag),1,CANCEL=cancel)
     z = where(mask eq 1,cnt)
     plotsym,0,1.2,/FILL
     if cnt ne 0 then oplot,wave[z],sn[z],PSYM=8,COLOR=4
     
  endif
  
  if state.p.plotfixpixel then begin
     
     mask = mc_bitset(fix(flag),2,CANCEL=cancel)
     z = where(mask eq 1,cnt)
     plotsym,0,1.0,/FILL
     if cnt ne 0 then oplot,wave[z],sn[z],PSYM=8,COLOR=7
     
  endif
  
  if state.p.plotsatpixel then begin
     
     mask = mc_bitset(fix(flag),0,CANCEL=cancel)
     z = where(mask eq 1,cnt)
     plotsym,0,0.8,/FILL
     if cnt ne 0 then oplot,wave[z],sn[z],PSYM=8,COLOR=2

  endif

  
  state.p.pscale2 = !p
  state.p.xscale2 = !x
  state.p.yscale2 = !y

end
;
;******************************************************************************
;
pro xcleanspec_plotflux,state

  atmoscolor = 5

  z    = where(finite((*state.d.pspec)[*,0,state.p.idx]) eq 1)
  wave = (*state.d.pspec)[z,0,state.p.idx]
  flux = (*state.d.pspec)[z,state.p.spectype+1,state.p.idx]
  flag = (*state.d.pspec)[z,3,state.p.idx]

  lidx   = strpos(state.p.ytitle,'(')
  ridx   = strpos(state.p.ytitle,')')
  yunits = strmid(state.p.ytitle,lidx+1,ridx-lidx-1)

  ytitle = (state.p.spectype eq 0) ? state.p.ytitle:'!5Error ('+yunits+')'

  if state.p.plotatmos then begin

     scale = mc_cfld(state.w.scaleatmos_fld,4,/EMPTY,CANCEL=cancel)
     if cancel then return

     atrans = *state.p.atrans
     awave  = *state.p.awave
     atrans = (temporary(atrans)-1.0)*scale+1.0

     linterp,awave,atrans,wave,natrans
     
     plot,wave,natrans,PSYM=10,XSTY=5,YSTY=5,$
          XRANGE=state.p.plot1xrange,YRANGE=[0,1],COLOR=atmoscolor, $
          CHARSIZE=1.5,/NOERASE

     ticks = string(findgen(11)*.1,FORMAT='(f3.1)')
     
     axis,YAXIS=1,YTICKS=10,YTICKNAME='!5'+ticks,YMINOR=1,COLOR=atmoscolor,$
          CHARSIZE=1.5


     ystyle = 9

  endif else ystyle = 1

  plot,wave,flux,PSYM=10,/XSTY,YSTY=ystyle,XRANGE=state.p.plot1xrange,$
       YRANGE=state.p.plot1yrange[*,state.p.spectype],XTITLE=state.p.xtitle, $
       YTITLE=ytitle,/NOERASE, $
       CHARSIZE=1.5

;  Plot flags
  
  if state.p.plotreplacepixel then begin
     
     mask = mc_bitset(fix(flag),1,CANCEL=cancel)
     z = where(mask eq 1,cnt)
     plotsym,0,1.2,/FILL
     if cnt ne 0 then oplot,wave[z],flux[z],PSYM=8,COLOR=4
     
  endif
  
  if state.p.plotfixpixel then begin
     
     mask = mc_bitset(fix(flag),2,CANCEL=cancel)
     z = where(mask eq 1,cnt)
     plotsym,0,1.0,/FILL
     if cnt ne 0 then oplot,wave[z],flux[z],PSYM=8,COLOR=7
     
  endif
  
  if state.p.plotsatpixel then begin
     
     mask = mc_bitset(fix(flag),0,CANCEL=cancel)
     z = where(mask eq 1,cnt)
     plotsym,0,0.8,/FILL
     if cnt ne 0 then oplot,wave[z],flux[z],PSYM=8,COLOR=2

  endif
  
  if state.r.plotlines then begin

;  Label H lines if requested
     
     z = where(state.d.hlines lt state.p.plot1xrange[1] and $
               state.d.hlines gt state.p.plot1xrange[0],count)

     for i =0, count-1 do begin

        name = '!5'+(state.d.hnames)[z[i]]        
        mc_tabinv,wave,(state.d.hlines)[z[i]],idx,CANCEL=cancel
        if cancel then return
        fluxval = (finite(flux[idx]) eq 0) ? !y.crange[0]:flux[idx]

        xybot = convert_coord((state.d.hlines)[z[i]],fluxval,/DATA, $
                              /TO_NORM)
        if finite(flux[idx]) ne 0 then $
           plots,[xybot[0],xybot[0]],[xybot[1],0.85],LINESTYLE=1,COLOR=3, $
                 THICK=2,/NORM
           
        xyouts, xybot[0],0.8,name,ORIENTATION=90,/NORM,COLOR=3,$
                CHARSIZE=1.5
           
     endfor
     
  endif

  if not keyword_set(PS) then begin

     state.p.pscale1 = !p
     state.p.xscale1 = !x
     state.p.yscale1 = !y

  endif

end
;
;******************************************************************************
;
pro xcleanspec_plotupdate,state

;  Plot window 1

  wset, state.p.pixmap1_wid
  erase
  polyfill,[0,0,1,1,0],[0,1,1,0,0],COLOR=20,/NORM
  xcleanspec_plotflux,state

  wset, state.p.plotwin1_wid
  device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
                state.p.pixmap1_wid]

;  Plot window 2

  wset, state.p.pixmap2_wid
  erase
  polyfill,[0,0,1,1,0],[0,1,1,0,0],COLOR=20,/NORM
  xcleanspec_plotsn,state

  wset, state.p.plotwin2_wid
  device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,0,$
                state.p.pixmap2_wid]

end
;
;******************************************************************************
;
pro xcleanspec_setminmax,state

  widget_control,state.w.xmin_fld[1], $
                 SET_VALUE=strtrim(state.p.plot1xrange[0],2)
  widget_control,state.w.xmax_fld[1], $
                 SET_VALUE=strtrim(state.p.plot1xrange[1],2)
  widget_control,state.w.ymin1_fld[1], $
                 SET_VALUE=strtrim(state.p.plot1yrange[0,state.p.spectype],2)
  widget_control,state.w.ymax1_fld[1], $
                 SET_VALUE=strtrim(state.p.plot1yrange[1,state.p.spectype],2)

  widget_control,state.w.ymin2_fld[1], $
                 SET_VALUE=strtrim(state.p.plot2yrange[0],2)
  widget_control,state.w.ymax2_fld[1], $
                 SET_VALUE=strtrim(state.p.plot2yrange[1],2)

end
;
;******************************************************************************
;
pro xcleanspec_smoothspec,state

  spec = *state.d.espec

  if state.r.smoothtype eq 'Savitzky-Golay' then begin

     res = state.r.resperpix/state.r.slitw_pix

     sgwin = mc_cfld(state.w.sgwin_fld,4,/EMPTY,CANCEL=cancel)
     if cancel then return

     sgdeg = mc_cfld(state.w.sgdeg_fld,4,/EMPTY,CANCEL=cancel)
     if cancel then return
     
     sgdeg = mc_crange(sgdeg,sgwin,'SG Degree',/KLT,$
                    WIDGET_ID=state.w.xcleanspec_base,CANCEL=cancel)
     if cancel then return

     print, ' '
     print, 'Performing a Savitzky-Golay smoothing with a window of '+$
            strtrim(sgwin,2)
     print, ' and a degree of '+strtrim(fix(sgdeg),2)+'.'
     print, ' '

     for i = 0, state.r.norders-1 do begin
        
        for j = 0, state.r.naps-1 do begin
           
           z = where(finite((*state.d.espec)[*,1,i*state.r.naps+j]) eq 1)

           cflux  = (*state.d.espec)[z,1,i*state.r.naps+j]
           cerror = (*state.d.espec)[z,2,i*state.r.naps+j]
           
           cflux = savitzky_golay(cflux,sgwin,DEGREE=sgdeg,ERROR=cerror)

           spec[z,1,i*state.r.naps+j] = cflux
           spec[z,2,i*state.r.naps+j] = cerror
           
        endfor
        
     endfor

     if state.r.norders gt 1 or state.r.naps gt 1 then begin

        state.r.smthhistory='These spectra have been convolved with ' + $
                            'a Savitzky-Golay filter of width '+ $
                            strtrim(sgwin,2)+' pixels and degree '+ $
                            strtrim(sgdeg,2)+'.'
        
     endif else begin

        state.r.smthhistory='This spectrum has been convolved with ' + $
                            'a Savitzky-Golay filter of width '+ $
                            strtrim(sgwin,2)+' pixels and degree '+ $
                            strtrim(sgdeg,2)+'.'

     endelse

  endif else begin

     fwhm_kernel = mc_cfld(state.w.fwhm_fld,4,/EMPTY,CANCEL=cancel)
     if cancel then return

     print, ' '
     print, 'Smoothing with a Gaussian with FWHM='+$
            string(FWHM_kernel,format='(f3.1)')+'.'
     print, ' '

     for i = 0, state.r.norders-1 do begin
        
        for j = 0, state.r.naps-1 do begin
           
           z = where(finite((*state.d.espec)[*,1,i*state.r.naps+j]) eq 1)
           mc_convolvespec,findgen(state.r.stop-state.r.start+1),$
                           (*state.d.espec)[z,1,i*state.r.naps+j],FWHM_kernel, $
                           cflux,cerror, $
                           ERROR=(*state.d.espec)[z,2,i*state.r.naps+j], $
                           CANCEL=cancel
           
           spec[z,1,i*state.r.naps+j] = cflux
           spec[z,2,i*state.r.naps+j] = cerror
           
        endfor
        
     endfor

     print, state.r.norders,state.r.naps
     if state.r.norders gt 1 or state.r.naps gt 1 then begin

        state.r.smthhistory = 'These spectra have been convolved with a ' + $
                              'Gaussian of FWHM='+strcompress(FWHM_kernel,/RE)+ $
                              ' pixels.'
        
     endif else begin

        state.r.smthhistory = 'This spectrum has been convolved with a ' + $
                              'Gaussian of FWHM='+strcompress(FWHM_kernel,/RE)+ $
                              ' pixels.'

     endelse

  endelse

  *state.d.espec = spec
  *state.d.pspec = spec
  state.p.plot2yrange    = [min(spec[*,1]/spec[*,2],MAX=max,/NAN),max]
  xcleanspec_plotupdate,state
  xcleanspec_setminmax,state

  widget_control, state.w.box3_base,SENSITIVE=0

end
;
;******************************************************************************
;
pro xcleanspec_writefile,state


  file = mc_cfld(state.w.oname_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return

  hdr = *state.d.hdr
  if state.r.smthhistory ne '' then begin

     hdr = mc_addhistlabel(hdr,'Xcleanspec History',CANCEL=cancel)
     if cancel then return

     smthhistory = mc_splittext(state.r.smthhistory,70,CANCEL=cancel)
     if cancel then return
     sxaddhist,smthhistory,hdr

  endif

  writefits, state.r.path+file+'.fits',*state.d.espec,hdr
  xvspec,state.r.path+file+'.fits',/PLOTLINMAX,/PLOTFIX


end
;
;******************************************************************************
;
pro xcleanspec_zoom,state,IN=in,OUT=out

  if state.p.plotwin eq 1 then begin

     idx = state.p.spectype
     delabsx = state.p.plot1absxrange[1]-state.p.plot1absxrange[0]
     delx    = state.p.plot1xrange[1]-state.p.plot1xrange[0]
     
     delabsy = state.p.plot1absyrange[1,idx]-state.p.plot1absyrange[0,idx]
     dely    = state.p.plot1yrange[1,idx]-state.p.plot1yrange[0,idx]
     
     xcen = state.p.plot1xrange[0]+delx/2.
     ycen = state.p.plot1yrange[0,idx]+dely/2.
     
     case state.r.cursormode of 
        
        'XZoom': begin
           
           z = alog10(delabsx/delx)/alog10(2)
           if keyword_set(IN) then z = z+1 else z=z-1
           hwin = delabsx/2.^z/2.
           state.p.plot1xrange = [xcen-hwin,xcen+hwin]
           xcleanspec_plotupdate,state
           xcleanspec_setminmax,state
           
        end
        
        'YZoom': begin
           
           z = alog10(delabsy/dely)/alog10(2)
           if keyword_set(IN) then z = z+1 else z=z-1
           hwin = delabsy/2.^z/2.
           state.p.plot1yrange[*,idx] = [ycen-hwin,ycen+hwin]
           xcleanspec_plotupdate,state
           xcleanspec_setminmax,state
           
        end
        
        'Zoom': begin
           
           z = alog10(delabsx/delx)/alog10(2)
           if keyword_set(IN) then z = z+1 else z=z-1
           hwin = delabsx/2.^z/2.
           state.p.plot1xrange = [xcen-hwin,xcen+hwin]
           
           z = alog10(delabsy/dely)/alog10(2)
           if keyword_set(IN) then z = z+1 else z=z-1
           hwin = delabsy/2.^z/2.
           state.p.plot1yrange[*,idx] = [ycen-hwin,ycen+hwin]
           
           xcleanspec_plotupdate,state
           xcleanspec_setminmax,state
           
        end
        
        else:
        
     endcase

  endif else begin

     delabsy = state.p.plot2absyrange[1]-state.p.plot2absyrange[0]
     dely    = state.p.plot2yrange[1]-state.p.plot2yrange[0]
     
     ycen = state.p.plot2yrange[0]+dely/2.

     case state.r.cursormode of 
        
        'YZoom': begin
           
           z = alog10(delabsy/dely)/alog10(2)
           if keyword_set(IN) then z = z+1 else z=z-1
           hwin = delabsy/2.^z/2.
           state.p.plot2yrange = [ycen-hwin,ycen+hwin]
           xcleanspec_plotupdate,state
           xcleanspec_setminmax,state
           
        end

        else:

     endcase

  endelse

end
;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro xcleanspec_event, event

  widget_control, event.id,  GET_UVALUE = uvalue

  if uvalue eq 'Quit' then begin

     widget_control, event.top, /DESTROY
     goto, getout

  endif

  widget_control, event.top, GET_UVALUE = state, /NO_COPY

  if uvalue eq 'Help' then begin
     
     pre = (strupcase(!version.os_family) eq 'WINDOWS') ? 'start ':'open '
     
     spawn, pre+filepath('spextoolmanual.pdf', $
                         ROOT=state.r.spextoolpath,$
                         SUBDIR='manuals')
     
  endif

  if state.w.freeze then begin

     if uvalue ne 'Input Spectrum' and uvalue ne 'Load Spectrum' then goto, cont

  endif

  case uvalue of


     'Copy Input Name': begin

        widget_control, state.w.oname_fld[1], $
                        SET_VALUE=strtrim(file_basename( $
                        state.r.spemc_cfile,'.fits'),2)

     end


     'Input Spectrum': begin

        obj = dialog_pickfile(DIALOG_PARENT=state.w.xcleanspec_base,$
                              PATH=state.r.path,GET_PATH=path, $
                              /MUST_EXIST,FILTER='*.fits')
        if obj eq '' then goto, cont
        widget_control,state.w.ispectrum_fld[1],SET_VALUE =strtrim(obj,2)
        mc_setfocus,state.w.ispectrum_fld
        state.r.path = path


     end

     'Hydrogen Lines': begin

        state.r.plotlines = event.select
        if not state.w.freeze then xcleanspec_plotupdate,state

     end

     'Keyboard': begin

        case strtrim(event.ch,2) of 
           
           'a': BEGIN

              state.p.plot1absxrange = state.p.plot1xrange
              state.p.plot1absyrange=state.p.plot1yrange
              state.p.plot2absyrange=state.p.plot2yrange

           end

           'c': begin
              
              state.r.cursormode = 'None'
              state.p.reg = !values.f_nan
              xcleanspec_plotupdate,state
              
           end

           'f': begin           ; Fix

              if state.r.cursormode eq 'None' then begin

                 state.r.cursormode = 'Fix'
                 state.p.reg = !values.f_nan
                 
              endif

           end

           'i': xcleanspec_zoom,state,/IN

           'o': xcleanspec_zoom,state,/OUT

           'p': xcleanspec_plotupdate,state,/PS

           'r': begin           ; Remove

              if state.r.cursormode eq 'None' then begin
                 
                 state.r.cursormode = 'Remove'
                 state.p.reg = !values.f_nan
                 
              endif

           end
           
           's': *state.d.espec = *state.d.pspec

           'u': begin           ; Undo

              *state.d.pspec = *state.d.espec
              state.p.reg = !values.f_nan
              xcleanspec_plotupdate,state

           end

           'w': begin

              if state.p.plotwin eq 1 then begin

                 state.p.plot1xrange = state.p.plot1absxrange
                 state.p.plot1yrange = state.p.plot1absyrange

              endif else state.p.plot2yrange = state.p.plot2absyrange

              xcleanspec_plotupdate,state
              xcleanspec_setminmax,state

           end

           'x': begin 

              state.r.cursormode = 'XZoom'
              state.p.reg = !values.f_nan
              
           end

           'y': begin 
              
              state.r.cursormode = 'YZoom'
              state.p.reg = !values.f_nan

           end

           'z': begin

              state.r.cursormode = 'Zoom'
              state.p.reg = !values.f_nan
              
           end
           
           else:

        endcase

     end

     'Load Spectrum': xcleanspec_loadspec,state

     'Plot Aperture': begin

        state.p.ap = event.index+1
        idx  = (state.p.order-(*state.r.orders)[0])*state.r.naps + $
               (state.p.ap-1)
        state.p.idx = idx

        state.p.plot1xrange = [min((*state.d.espec)[*,0,idx],MAX=max,/NAN),max]
        state.p.plot1yrange = [0,max((*state.d.espec)[*,1,idx],/NAN)]
        sn = (*state.d.espec)[*,1,idx]/(*state.d.espec)[*,2,idx]
        state.p.plot2yrange = [min(sn,MAX=max,/NAN),max]


        state.p.plot1absxrange = state.p.plot1xrange
        state.p.plot1absyrange = state.p.plot1yrange
        state.p.plot2absyrange = state.p.plot2yrange
        
        xcleanspec_plotupdate,state
        xcleanspec_setminmax,state
        
     end

     'Plot Atmosphere': begin

        state.p.plotatmos = event.select
        widget_control,state.w.scaleatmos_fld[0],SENSITIVE=event.select
        xcleanspec_plotupdate,state
        

     end

     'Plot Order': begin

        state.p.order = (*state.r.orders)[event.index]
        idx           = event.index*state.r.naps + (state.p.ap-1)
        state.p.idx   = idx

        state.p.plot1xrange = [min((*state.d.espec)[*,0,idx],/NAN,MAX=max),max]
        state.p.plot1yrange = [0,max((*state.d.espec)[*,1,idx],/NAN)]
        sn = (*state.d.espec)[*,1,idx]/(*state.d.espec)[*,2,idx]
        state.p.plot2yrange = [min(sn,MAX=max,/NAN),max]

        state.p.plot1absxrange = state.p.plot1xrange
        state.p.plot1absyrange = state.p.plot1yrange
        state.p.plot2absyrange = state.p.plot2yrange
        
        xcleanspec_plotupdate,state
        xcleanspec_setminmax,state
        
     end
     
     'Plot Replaced Pixel': begin

        state.p.plotreplacepixel = ~state.p.plotreplacepixel
        widget_control, state.w.flags[1],SET_BUTTON=state.p.plotreplacepixel
        xcleanspec_plotupdate,state
        
     end
     
     'Plot Saturated Pixel': begin

        state.p.plotsatpixel = ~state.p.plotsatpixel
        widget_control, state.w.flags[0],SET_BUTTON=state.p.plotsatpixel
        xcleanspec_plotupdate,state
        
     end

     'Plot Fixed Pixel': begin

        state.p.plotfixpixel = ~state.p.plotfixpixel
        widget_control, state.w.flags[2],SET_BUTTON=state.p.plotfixpixel
        xcleanspec_plotupdate,state
        
     end

     
     'Scale Atmosphere': xcleanspec_plotupdate,state

     'Smooth Spectra': xcleanspec_smoothspec,state

     'Smooth Type': begin

        widget_control, state.w.sg_base, MAP=0
        widget_control, state.w.gs_base, MAP=0

        if event.value eq 'Savitzky-Golay' then begin

           state.r.smoothtype = event.value
           widget_control, state.w.sg_base, MAP=1

        endif
        if event.value eq 'Gaussian' then begin

           state.r.smoothtype = event.value
           widget_control, state.w.gs_base, MAP=1
           
        endif

     end

     'Spectrum Type': begin

        state.p.spectype = event.value
        xcleanspec_plotupdate,state

     end
     'Write File': begin

        xcleanspec_writefile,state

     end
     
     else:

  endcase

cont: 

  widget_control, state.w.xcleanspec_base, SET_UVALUE=state, /NO_COPY
getout:

end
;
;******************************************************************************
;
pro xcleanspec_plotwinevent1, event

  widget_control, event.top, GET_UVALUE = state, /NO_COPY
  widget_control, event.id,  GET_UVALUE = uvalue

;  Check to see if it is a TRACKING event.

  if strtrim(tag_names(event,/STRUCTURE_NAME),2) eq 'WIDGET_TRACKING' then $
     begin

     if event.enter eq 0 then widget_control, state.w.keyboard, SENSITIVE=0
     wset, state.p.plotwin1_wid
     device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
                   state.p.pixmap1_wid]

     wset, state.p.plotwin2_wid
     device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,0,$
                   state.p.pixmap2_wid]

     state.p.plotwin = 1    
     goto, cont
     
  endif

;  If not, set the keyboard focus and active window.

  widget_control, state.w.keyboard, /INPUT_FOCUS, /SENSITIVE

  !p = state.p.pscale1
  !x = state.p.xscale1
  !y = state.p.yscale1
  x  = event.x/float(state.p.plot1size[0])
  y  = event.y/float(state.p.plot1size[1])
  xy = convert_coord(x,y,/NORMAL,/TO_DATA)

  if event.type eq 1 then begin

     case state.r.cursormode of 

        'Fix': begin

           z = where(finite(state.p.reg) eq 1,count)
           if count eq 0 then begin
              
              state.p.reg[*,0] = xy[0:1] 

              wset, state.p.plotwin1_wid
              plots,[xy[0],xy[0]],(state.p.yscale1).crange,LINESTYLE=2,$
                    COLOR=7
              wset, state.p.pixmap1_wid
              plots,[xy[0],xy[0]],(state.p.yscale1).crange,LINESTYLE=2,$
                    COLOR=7
              
           endif else begin
              
              state.p.reg[*,1] = xy[0:1]
              xrange = reform(state.p.reg[0,sort(state.p.reg[0,*])])
              mc_tabinv,(*state.d.espec)[*,0,state.p.idx],xrange,idx, $
                        CANCEL=cancel
              if cancel then return
              idx = round(idx)
              
              x = [(*state.d.espec)[idx[0],0,state.p.idx],$
                   (*state.d.espec)[idx[1],0,state.p.idx]]

              y = [(*state.d.espec)[idx[0],1,state.p.idx],$
                   (*state.d.espec)[idx[1],1,state.p.idx]]

              e = [(*state.d.espec)[idx[0],2,state.p.idx],$
                   (*state.d.espec)[idx[1],2,state.p.idx]]

              if state.p.spectype eq 0 then begin

                 coeff = poly_fit1d(x,y,1,/SILENT)

                 (*state.d.pspec)[idx[0]:idx[1],1,state.p.idx] = $
                    poly((*state.d.espec)[idx[0]:idx[1],0,state.p.idx],coeff)
                 
              endif
              coeff = poly_fit1d(x,e,1,/SILENT)
              (*state.d.pspec)[idx[0]:idx[1],2,state.p.idx] = $
                 poly((*state.d.espec)[idx[0]:idx[1],0,state.p.idx],coeff)

              (*state.d.pspec)[idx[0]:idx[1],3,state.p.idx] = $
                 (*state.d.pspec)[idx[0]:idx[1],3,state.p.idx]  + 4
              
              xcleanspec_plotupdate,state
              state.r.cursormode = 'None'

           endelse

        end

        'Remove': begin

           z = where(finite(state.p.reg) eq 1,count)
           if count eq 0 then begin
              
              state.p.reg[*,0] = xy[0:1] 
              wset, state.p.plotwin1_wid
              plots,[xy[0],xy[0]],(state.p.yscale1).crange,LINESTYLE=2,$
                    COLOR=7
              wset, state.p.pixmap1_wid
              plots,[xy[0],xy[0]],(state.p.yscale1).crange,LINESTYLE=2,$
                    COLOR=7
              
           endif else begin
              
              state.p.reg[*,1] = xy[0:1]
              mc_tabinv,(*state.d.espec)[*,0,state.p.idx],$
                        reform(state.p.reg[0,*]),idx,CANCEL=cancel
              if cancel then return
              
              idx = round(idx)                
              min = idx[0] < idx[1]
              max = idx[0] > idx[1]

              idx = findgen(n_elements((*state.d.pspec)[*,0,0]))
              amin = min(idx,MAX=amax)
              
              if state.p.spectype eq 0 then  begin
                 
                 (*state.d.pspec)[((min) > amin):((max) < amax),1,$
                                  state.p.idx] = !values.f_nan
                 
              endif

              (*state.d.pspec)[((min) > amin):((max) < amax),2,$
                               state.p.idx] = !values.f_nan

              xcleanspec_plotupdate,state
              state.r.cursormode = 'None'
              
           endelse

        end

        'Zoom': begin
           
           z = where(finite(state.p.reg) eq 1,count)
           if count eq 0 then state.p.reg[*,0] = xy[0:1] else begin 
              
              state.p.reg[*,1] = xy[0:1]
              state.p.plot1xrange   = [min(state.p.reg[0,*],MAX=max),max]
              state.p.plot1yrange   = [min(state.p.reg[1,*],MAX=max),max]
              xcleanspec_plotupdate,state
              xcleanspec_setminmax,state
              state.r.cursormode   = 'None'
              state.p.reg = !values.f_nan
              
           endelse

        end

        'XZoom': begin

           z = where(finite(state.p.reg) eq 1,count)
           if count eq 0 then begin
              
              state.p.reg[*,0] = xy[0:1]
              wset, state.p.pixmap1_wid
              plots, [event.x,event.x],[0,state.p.plot1size[1]],COLOR=2,$
                     /DEVICE,LINESTYLE=2
              wset, state.p.plotwin1_wid
              device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,$
                            0,state.p.pixmap1_wid]
              wset, state.p.pixmap2_wid
              plots, [event.x,event.x],[0,state.p.plot2size[1]],COLOR=2,$
                     /DEVICE,LINESTYLE=2
              wset, state.p.plotwin2_wid
              device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,$
                            0,state.p.pixmap2_wid]
              
           endif else begin

              state.p.reg[*,1] = xy[0:1]
              state.p.plot1xrange = [min(state.p.reg[0,*],max=m),m]
              state.r.cursormode = 'None'
              state.p.reg = !values.f_nan
              xcleanspec_plotupdate,state
              xcleanspec_setminmax,state
              
           endelse


        end

        'YZoom': begin

           z = where(finite(state.p.reg) eq 1,count)
           if count eq 0 then begin
              
              state.p.reg[*,0] = xy[0:1]
              wset, state.p.pixmap1_wid
              plots, [0,state.p.plot1size[0]],[event.y,event.y],COLOR=2,$
                     /DEVICE,LINESTYLE=2

              wset, state.p.plotwin1_wid
              device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,$
                            0,state.p.pixmap1_wid]
              
           endif else begin

              state.p.reg[*,1] = xy[0:1]
              state.p.plot1yrange[*,state.p.spectype] = $
                 [min(state.p.reg[1,*],max=m),m]
              state.r.cursormode = 'None'
              state.p.reg[*] = !values.f_nan
              xcleanspec_plotupdate,state
              xcleanspec_setminmax,state
              
           endelse

        end

        else:

     endcase

  endif

;  Copy the pixmaps and draw the lines.

  wset, state.p.plotwin1_wid
  device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
                state.p.pixmap1_wid]

  wset, state.p.plotwin2_wid
  device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,0,$
                state.p.pixmap2_wid]

  wset, state.p.plotwin1_wid

  case state.r.cursormode of 

     'XZoom': begin

        wset, state.p.plotwin1_wid
        plots, [event.x,event.x],[0,state.p.plot1size[1]],COLOR=2,/DEVICE
        wset, state.p.plotwin2_wid
        plots, [event.x,event.x],[0,state.p.plot2size[1]],COLOR=2,/DEVICE

     end
     'YZoom': plots, [0,state.p.plot1size[0]],[event.y,event.y],COLOR=2,/DEVICE

     'Zoom': begin

        plots, [event.x,event.x],[0,state.p.plot1size[1]],COLOR=2,/DEVICE
        plots, [0,state.p.plot1size[0]],[event.y,event.y],COLOR=2,/DEVICE
        xy = convert_coord(event.x,event.y,/DEVICE,/TO_DATA)
        plots,[state.p.reg[0,0],state.p.reg[0,0]],[state.p.reg[1,0],xy[1]],$
              LINESTYLE=2,COLOR=2
        plots, [state.p.reg[0,0],xy[0]],[state.p.reg[1,0],state.p.reg[1,0]],$
               LINESTYLE=2,COLOR=2
        
     end

     else: begin

        plots, [event.x,event.x],[0,state.p.plot1size[1]],COLOR=2,/DEVICE
        plots, [0,state.p.plot1size[0]],[event.y,event.y],COLOR=2,/DEVICE
        wset, state.p.plotwin2_wid
        plots, [event.x,event.x],[0,state.p.plot2size[1]],COLOR=2,/DEVICE

     end

  endcase

;  Update cursor position.
  
  if not state.w.freeze then begin

     mc_tabinv, (*state.d.pspec)[*,0,state.p.idx],xy[0],idx,CANCEL=cancel
     if cancel then return
     idx = round(idx)
     label = 'Cursor X: '+strtrim(xy[0],2)+', Y:'+strtrim(xy[1],2)
     label = label+'   Spectrum X: '+$
             strtrim( (*state.d.pspec)[idx,0,state.p.idx],2)+$
             ', Y:'+strtrim( (*state.d.pspec)[idx,1,state.p.idx],2)
     widget_control,state.w.message,SET_VALUE=label

  endif

cont:

  widget_control, state.w.xcleanspec_base, SET_UVALUE=state, /NO_COPY
  
end
;
;******************************************************************************
;
pro xcleanspec_plotwinevent2, event

  widget_control, event.top, GET_UVALUE = state, /NO_COPY
  widget_control, event.id,  GET_UVALUE = uvalue

;  Check to see if it is a TRACKING event.

  if strtrim(tag_names(event,/STRUCTURE_NAME),2) eq 'WIDGET_TRACKING' then $
     begin

     if event.enter eq 0 then widget_control, state.w.keyboard, SENSITIVE=0
     wset, state.p.plotwin1_wid
     device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
                   state.p.pixmap1_wid]

     wset, state.p.plotwin2_wid
     device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,0,$
                   state.p.pixmap2_wid]

     state.p.plotwin = 2
     goto, cont
     
  endif

;  If not, set the keyboard focus and active window.

  widget_control, state.w.keyboard, /INPUT_FOCUS, /SENSITIVE
  wset, state.p.plotwin2_wid

  !p = state.p.pscale2
  !x = state.p.xscale2
  !y = state.p.yscale2
  x  = event.x/float(state.p.plot2size[0])
  y  = event.y/float(state.p.plot2size[1])
  xy = convert_coord(x,y,/NORMAL,/TO_DATA)

  if event.type eq 1 and state.r.cursormode eq 'YZoom' then begin
     
     z = where(finite(state.p.reg) eq 1,count)
     if count eq 0 then begin
        
        state.p.reg[*,0] = xy[0:1]
        wset, state.p.pixmap2_wid
        plots, [0,state.p.plot2size[0]],[event.y,event.y],COLOR=2,$
               /DEVICE,LINESTYLE=2
        
        wset, state.p.plotwin2_wid
        device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,0,$
                      state.p.pixmap2_wid]
        
     endif else begin
        
        state.p.reg[*,1] = xy[0:1]
        state.p.plot2yrange = [min(state.p.reg[1,*],MAX=m),m]
        state.r.cursormode = 'None'
        state.p.reg = !values.f_nan
        xcleanspec_plotupdate,state
        xcleanspec_setminmax,state
        
     endelse

endif	

;  Copy the pixmaps and draw the lines.

  wset, state.p.plotwin1_wid
  device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
                state.p.pixmap1_wid]

  wset, state.p.plotwin2_wid
  device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,0,$
                state.p.pixmap2_wid]

  if state.r.cursormode eq 'YZoom' then begin

     plots, [0,state.p.plot2size[0]],[event.y,event.y],COLOR=2,/DEVICE

  endif else begin

     plots, [0,state.p.plot2size[0]],[event.y,event.y],COLOR=2,/DEVICE
     plots, [event.x,event.x],[0,state.p.plot2size[1]],COLOR=2,/DEVICE

     wset, state.p.plotwin1_wid
     plots, [event.x,event.x],[0,state.p.plot1size[1]],COLOR=2,/DEVICE

  endelse

;  Update cursor position.
  
  if not state.w.freeze then begin

     mc_tabinv, (*state.d.pspec)[*,0,state.p.idx],xy[0],idx,CANCEL=cancel
     if cancel then return
     idx = round(idx)
     sn = (*state.d.pspec)[idx,1,state.p.idx]/$
          (*state.d.pspec)[idx,2,state.p.idx]


     label = 'Cursor X: '+strtrim(xy[0],2)+', Y:'+strtrim(xy[1],2)
     label = label+'   Spectrum X: '+$
             strtrim( (*state.d.pspec)[idx,0,state.p.idx],2)+$
             ', Y:'+strtrim(sn ,2)
     widget_control,state.w.message,SET_VALUE=label

  endif


cont:

  widget_control, state.w.xcleanspec_base, SET_UVALUE=state, /NO_COPY

end
;
;******************************************************************************
;
pro xcleanspec_minmax,event

  widget_control, event.top, GET_UVALUE = state, /NO_COPY
  widget_control, event.id,  GET_UVALUE = uvalue

  if state.w.freeze then goto, cont

  case uvalue of 

     'X Min': begin

        xmin = mc_cfld(state.w.xmin_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then goto, cont
        xmin2 = mc_crange(xmin,state.p.plot1xrange[1],'X Min',/KLT,$
                       WIDGET_ID=state.w.xcleanspec_base,CANCEL=cancel)
        if cancel then begin
           
           widget_control, state.w.xmin_fld[0],$
                           SET_VALUE=state.p.plot1xrange[0]
           goto, cont
           
        endif else state.p.plot1xrange[0] = xmin2

     end
     'X Max': begin

        xmax = mc_cfld(state.w.xmax_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then goto, cont
        xmax2 = mc_crange(xmax,state.p.plot1xrange[0],'X Max',/KGT,$
                       WIDGET_ID=state.w.xcleanspec_base,CANCEL=cancel)
        if cancel then begin
           
           widget_control, state.w.xmax_fld[0],$
                           SET_VALUE=state.p.plot1xrange[1]
           goto, cont
           
        endif else state.p.plot1xrange[1] = xmax2

     end
     'Y1 Min': begin

        ymin = mc_cfld(state.w.ymin1_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then goto, cont
        ymin2 = mc_crange(ymin,state.p.plot1yrange[1],'Y Min',/KLT,$
                       WIDGET_ID=state.w.xcleanspec_base,CANCEL=cancel)
        if cancel then begin
           
           widget_control,state.w.ymin1_fld[0],$
                          SET_VALUE=state.p.plot1yrange[0]
           goto, cont
           
        endif else state.p.plot1yrange[0] = ymin2
        
     end
     'Y1 Max': begin

        ymax = mc_cfld(state.w.ymax1_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then goto, cont
        ymax2 = mc_crange(ymax,state.p.plot1yrange[0],'Y Max',/KGT,$
                       WIDGET_ID=state.w.xcleanspec_base,CANCEL=cancel)
        if cancel then begin
           
           widget_control,state.w.ymax1_fld[0],$
                          SET_VALUE=state.p.plot1yrange[1]
           goto, cont
           
        endif else state.p.plot1yrange[1] = ymax2
        
     end
     'Y2 Min': begin

        ymin = mc_cfld(state.w.ymin2_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then goto, cont
        ymin2 = mc_crange(ymin,state.p.plot2yrange[1],'Y Min',/KLT,$
                       WIDGET_ID=state.w.xcleanspec_base,CANCEL=cancel)
        if cancel then begin
           
           widget_control,state.w.ymin2_fld[0],$
                          SET_VALUE=state.p.plot2yrange[0]
           goto, cont
           
        endif else state.p.plot2yrange[0] = ymin2
        
     end
     'Y2 Max': begin

        ymax = mc_cfld(state.w.ymax2_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then goto, cont
        ymax2 = mc_crange(ymax,state.p.plot2yrange[0],'Y Max',/KGT,$
                       WIDGET_ID=state.w.xcleanspec_base,CANCEL=cancel)
        if cancel then begin
           
           widget_control,state.w.ymax2_fld[0],$
                          SET_VALUE=state.p.plot2yrange[1]
           goto, cont
           
        endif else state.p.plot2yrange[1] = ymax2
        
     end
     
  endcase

  xcleanspec_plotupdate,state

cont:
  widget_control, state.w.xcleanspec_base, SET_UVALUE=state, /NO_COPY

end
;
;******************************************************************************
;
pro xcleanspec_resize, event

  widget_control, event.top, GET_UVALUE=state,/NO_COPY
  widget_control, event.id,  GET_UVALUE=uvalue

  widget_control, state.w.xcleanspec_base,TLB_GET_SIZE=size

; Resize first

  state.p.plot1size[0]=size[0]-state.p.buffer[0]
  state.p.plot1size[1]=(size[1]-state.p.buffer[1])*state.p.plot1scale

  state.p.plot2size[0]=size[0]-state.p.buffer[0]
  state.p.plot2size[1]=(size[1]-state.p.buffer[1])*state.p.plot2scale

  widget_control, state.w.plotwin1,UPDATE=0
  widget_control, state.w.plotwin2,UPDATE=0

  widget_control, state.w.plotwin1,DRAW_XSIZE=state.p.plot1size[0], $
                  DRAW_YSIZE=state.p.plot1size[1]
  polyfill,[0,0,1,1,0],[0,1,1,0,0],COLOR=20,/NORM

  widget_control, state.w.plotwin2, DRAW_XSIZE=state.p.plot2size[0], $
                  DRAW_YSIZE=state.p.plot2size[1]
  polyfill,[0,0,1,1,0],[0,1,1,0,0],COLOR=20,/NORM

  widget_control, state.w.plotwin1,UPDATE=1
  widget_control, state.w.plotwin2,UPDATE=1

  wdelete,state.p.pixmap1_wid
  window, /FREE, /PIXMAP,XSIZE=state.p.plot1size[0],YSIZE=state.p.plot1size[1]
  polyfill,[0,0,1,1,0],[0,1,1,0,0],COLOR=20,/NORM
  state.p.pixmap1_wid = !d.window

  wset, state.p.plotwin1_wid
  device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
                state.p.pixmap1_wid]

  wdelete,state.p.pixmap2_wid
  window, /FREE, /PIXMAP,XSIZE=state.p.plot2size[0],YSIZE=state.p.plot2size[1]
  polyfill,[0,0,1,1,0],[0,1,1,0,0],COLOR=20,/NORM
  state.p.pixmap2_wid = !d.window

  wset, state.p.plotwin2_wid
  device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,0,$
                state.p.pixmap2_wid]

  if not state.w.freeze then xcleanspec_plotupdate,state

  widget_control, state.w.xcleanspec_base, SET_UVALUE=state, /NO_COPY

end
;
;******************************************************************************
;
; ------------------------------Main Program-------------------------------- 
;
;******************************************************************************
;
pro xcleanspec

  if xregistered('xcleanspec') then goto, cont

;  Load color table

  mc_mkct
  device, RETAIN=2

  mc_getosinfo,dirsep,strsep,CANCEL=cancel
  if cancel then return


;  Get spextool and instrument information 
  
  mc_getspextoolinfo,spextoolpath,packagepath,instr,notspex,version, $
                     INSTRUMENT=instrument,CANCEL=cancel
  if cancel then return
  
  readcol,filepath('HI.dat',ROOT_DIR=spextoolpath,SUBDIR='data'),$
          hlines,hnames,FORMAT='D,A',COMMENT='#',DELIMITER='|',/SILENT
  
;  Build three structures which will hold important info.

  w = {aperture_dl:0L,$
       box2_base:0L,$
       box3_base:0L,$
       box4_base:0L,$
       dirsep:dirsep,$
       freeze:1,$
       fwhm_fld:[0L,0L],$
       flags:[0L,0L,0L],$
       gs_base:0L,$
       gs_fld:[0L,0L],$
       keyboard:0L,$
       ispectrum_fld:[0L,0L],$
       message:0L,$
       oname_fld:[0L,0L],$
       order_dl:0L,$
       path_fld:[0L,0L],$
       plotrow:0L,$
       plotwin1:0,$
       plotwin2:0,$
       scaleatmos_fld:[0L,0L],$
       sg_base:0L,$
       sgwin_fld:[0L,0L],$
       sgdeg_fld:[0L,0L],$
       xcleanspec_base:0L,$
       xmin_fld:[0L,0L],$
       xmax_fld:[0L,0L],$
       ymin1_fld:[0L,0L],$
       ymax1_fld:[0L,0L],$
       ymin2_fld:[0L,0L],$
       ymax2_fld:[0L,0L]}

  r = {cursormode:'None',$
       maxres:0.,$
       resperpix:0.,$
       naps:0,$
       norders:0,$
       orders:ptr_new(fltarr(2)),$
       spextoolpath:spextoolpath,$
       path:'',$
       plotlines:0,$
       slitw_pix:0.,$
       smoothtype:'Savitzky-Golay',$
       smthhistory:'',$
       spemc_cfile:'',$
       start:0L,$
       stop:0L}

  d = {hdr:ptr_new(strarr(2)),$
       hlines:hlines,$
       hnames:hnames,$
       espec:ptr_new(fltarr(2)),$
       ospec:ptr_new(fltarr(2)),$
       pspec:ptr_new(fltarr(2))}

  p = {activespec:1,$
       ap:0,$
       atrans:ptr_new(2),$
       awave:ptr_new(2),$
       buffer:[0.,0.],$
       idx:0,$
       order:0,$
       pixmap1_wid:0L,$
       pixmap2_wid:0L,$
       plotatmos:0,$
       plotfixpixel:0,$
       plotreplacepixel:0,$
       plotsatpixel:0,$
       plotwin1_wid:0L,$
       plotwin2_wid:0L,$
       plot1absxrange:[0.,0.],$
       plot1absyrange:[[0.,0.],[0.,0.]],$
       plot1scale:0.0,$
       plot1xrange:[0.,0.],$
       plot1yrange:[[0.,0.],[0.,0.]],$
       plot2scale:0.0,$
       plot1size:[670,280],$
       plot2absyrange:[0.,0.],$
       plot2yrange:[0.,0.],$
       plot2size:[670,280/1.4],$
       plotwin:1,$
       pscale1:!p,$
       xscale1:!x,$
       pscale2:!p,$
       xscale2:!x,$
       spectype:0,$
       xtitle:'',$
       yscale1:!y,$
       yscale2:!y,$
       ytitle:'',$
       reg:[[!values.f_nan,!values.f_nan],$
            [!values.f_nan,!values.f_nan]]}

  p.plot1scale = float(p.plot1size[1])/(p.plot1size[1]+p.plot2size[1])
  p.plot2scale = float(p.plot2size[1])/(p.plot1size[1]+p.plot2size[1])


;  Load the three structures in the state structure.

  state = {w:w,r:r,d:d,p:p}

title = 'Xcleanspec '+version+' for '+instr.instr
  
;  Build the widget.

  mc_getfonts,buttonfont,textfont
  
  state.w.xcleanspec_base = widget_base(TITLE=title, $
                                        /COLUMN,$
                                        /TLB_SIZE_EVENTS)

     quit_button = widget_button(state.w.xcleanspec_base,$
                                 FONT=buttonfont,$
                                 EVENT_PRO='xcleanspec_event',$
                                 VALUE='Quit',$
                                 UVALUE='Quit')
     
     state.w.keyboard = widget_text(state.w.xcleanspec_base, $
                                    /ALL_EVENTS, $
                                    SCR_XSIZE=1, $
                                    SCR_YSIZE=1, $
                                    UVALUE='Keyboard', $
                                    EVENT_PRO='xcleanspec_event',$
                                    VALUE= '')
  
     row_base = widget_base(state.w.xcleanspec_base,$
                            /ROW)

        col1_base = widget_base(row_base,$
                                EVENT_PRO='xcleanspec_event',$
                                /COLUMN)
        
           box1_base = widget_base(col1_base,$
                                   /COLUMN,$
                                   FRAME=2)
           
              label = widget_label(box1_base,$
                                   VALUE='1.  Load Spectra',$
                                   FONT=buttonfont,$
                                   /ALIGN_LEFT)
              
              row = widget_base(box1_base,$
                                /ROW,$
                                /BASE_ALIGN_CENTER)
              
                 input = widget_button(row,$
                                       FONT=buttonfont,$
                                       VALUE='Input Spectrum',$
                                       UVALUE='Input Spectrum')
                 
                 input_fld = coyote_field2(row,$
                                           LABELFONT=buttonfont,$
                                           FIELDFONT=textfont,$
                                           TITLE=':',$
                                           UVALUE='Input Spectrum Field',$
                                           XSIZE=15,$
                                           TEXTID=textid)
                 state.w.ispectrum_fld = [input_fld,textid]
                 
              load = widget_button(box1_base,$
                                   VALUE='Load Spectrum',$
                                   UVALUE='Load Spectrum',$
                                   FONT=buttonfont)
           
           state.w.box2_base = widget_base(col1_base,$
                                           /COLUMN,$
                                           FRAME=2)
           
              label = widget_label(state.w.box2_base,$
                                   VALUE='2.  Edit Spectra',$
                                   FONT=buttonfont,$
                                   /ALIGN_LEFT)

              bg = cw_bgroup(state.w.box2_base,$
                             FONT=buttonfont,$
                             ['Flux','Error'],$
                             /ROW,$
                             /RETURN_INDEX,$
                             /NO_RELEASE,$
                             /EXCLUSIVE,$
                             LABEL_LEFT='Plot Spectrum:',$
                             UVALUE='Spectrum Type',$
                             SET_VALUE=0)
              

              
              row = widget_base(state.w.box2_base,$
                                /ROW,$
                                /BASE_ALIGN_CENTER)

                 state.w.order_dl = widget_droplist(row,$
                                                    FONT=buttonfont,$
                                                    TITLE='Order:',$
                                                    VALUE='01',$
                                                    UVALUE='Plot Order')
                 
                 state.w.aperture_dl = widget_droplist(row,$
                                                       FONT=buttonfont,$
                                                       TITLE='Aperture:',$
                                                       VALUE='01',$
                                                       UVALUE='Plot Aperture')

           state.w.box3_base = widget_base(col1_base,$
                                           /COLUMN,$
                                           FRAME=2)
           
              label = widget_label(state.w.box3_base,$
                                   VALUE='3.  Smooth Spectra',$
                                   FONT=buttonfont,$
                                   /ALIGN_LEFT)
              
              smooth_bg = cw_bgroup(state.w.box3_base,$
                                    FONT=buttonfont,$
                                    ['Savitzky-Golay','Gaussian'],$
                                    /ROW,$
                                    /RETURN_NAME,$
                                    /NO_RELEASE,$
                                    /EXCLUSIVE,$
                                    LABEL_LEFT='',$
                                    UVALUE='Smooth Type',$
                                    SET_VALUE=0)
              
              row = widget_base(state.w.box3_base)

                 state.w.sg_base = widget_base(row,$
                                               /ROW,$
                                               /BASE_ALIGN_CENTER)
                 
                    window = coyote_field2(state.w.sg_base,$
                                           LABELFONT=buttonfont,$
                                           FIELDFONT=textfont,$
                                           TITLE='SG Window:',$
                                           UVALUE='SG Window',$
                                           XSIZE=4,$
                                           TEXTID=textid)
                    state.w.sgwin_fld = [window,textid] 
                    
                    window = coyote_field2(state.w.sg_base,$
                                           LABELFONT=buttonfont,$
                                           FIELDFONT=textfont,$
                                           TITLE='Degree:',$
                                           UVALUE='SG Degree',$
                                           VALUE='2',$
                                           XSIZE=3,$
                                           TEXTID=textid)
                    state.w.sgdeg_fld = [window,textid] 
                    
                 state.w.gs_base = widget_base(row,$
                                               /ROW,$
                                               /BASE_ALIGN_CENTER,$
                                               MAP=0)
                 
                    field = coyote_field2(state.w.gs_base,$
                                          LABELFONT=buttonfont,$
                                          FIELDFONT=textfont,$
                                          TITLE='FWHM=',$
                                          UVALUE='FWHM',$
                                          XSIZE=10,$
                                          TEXTID=textid)
                    state.w.fwhm_fld = [field,textid] 
                    
              smooth_button = widget_button(state.w.box3_base,$
                                            FONT=buttonfont,$
                                            VALUE='Smooth Spectra',$
                                            UVALUE='Smooth Spectra')

           state.w.box4_base = widget_base(col1_base,$
                                           /COLUMN,$
                                           FRAME=2)
           
              label = widget_label(state.w.box4_base,$
                                   VALUE='4.  Write Spectra to File',$
                                   FONT=buttonfont,$
                                   /ALIGN_LEFT)
              
              button = widget_button(state.w.box4_base,$
                                     FONT=buttonfont,$
                                     VALUE='Copy Input Name',$
                                     UVALUE='Copy Input Name')

              oname = coyote_field2(state.w.box4_base,$
                                    LABELFONT=buttonfont,$
                                    FIELDFONT=textfont,$
                                    TITLE='File Name:',$
                                    UVALUE='Object File Oname',$
                                    XSIZE=18,$
                                    TEXTID=textid)
              state.w.oname_fld = [oname,textid]
              
              write = widget_button(state.w.box4_base,$
                                    VALUE='Write File',$
                                    UVALUE='Write File',$
                                    FONT=buttonfont)
              
        col2_base = widget_base(row_base,$
                                EVENT_PRO='xcleanspec_event',$
                                /COLUMN)
        
           state.w.message = widget_text(col2_base, $
                                         YSIZE=1)
           
           state.w.plotrow = widget_base(col2_base,$
                                         /ROW,$
                                         /BASE_ALIGN_CENTER,$
                                         FRAME=2)

           button = widget_button(state.w.plotrow, $
                                  EVENT_PRO='xcleanspec_event',$
                                  VALUE='Plot Flags', $
                                  UVALUE='Flags Button',$
                                  /MENU,$
                                  FONT=buttonfont)
           
           state.w.flags[0] = widget_button(button, $
                                            VALUE='Lincor Max Pixel', $
                                            UVALUE='Plot Saturated Pixel',$
                                            FONT=buttonfont,$
                                            /CHECKED_MENU)
           widget_control, state.w.flags[0],SET_BUTTON=state.p.plotsatpixel
  
           state.w.flags[1] = widget_button(button, $
                                            VALUE='Replaced Pixel', $
                                            UVALUE='Plot Replaced Pixel',$
                                            FONT=buttonfont,$
                                            /CHECKED_MENU)
           widget_control, state.w.flags[1],SET_BUTTON=state.p.plotreplacepixel
     

           state.w.flags[2] = widget_button(button, $
                                            VALUE='Fixed Pixel', $
                                            UVALUE='Plot Fixed Pixel',$
                                            FONT=buttonfont,$
                                            /CHECKED_MENU)
           widget_control, state.w.flags[2],SET_BUTTON=state.p.plotfixpixel


           
              hlines_bg = cw_bgroup(state.w.plotrow,$
                                    ['Plot Hydrogen Lines'],$
                                    FONT=buttonfont,$
                                    UVALUE='Hydrogen Lines',$
                                    SET_VALUE=[0],$
                                    /NONEXCLUSIVE)
              
              hlines_bg = cw_bgroup(state.w.plotrow,$
                                    ['Plot Atmosphere'],$
                                    FONT=buttonfont,$
                                    UVALUE='Plot Atmosphere',$
                                    SET_VALUE=[0],$
                                    /NONEXCLUSIVE)

              fld  = coyote_field2(state.w.plotrow,$
                                   LABELFONT=buttonfont,$
                                   FIELDFONT=textfont,$
                                   TITLE='Scale:',$
                                   UVALUE='Scale Atmosphere',$
                                   XSIZE=5,$
                                   VALUE=1.0,$
                                   EVENT_PRO='xcleanspec_event',$
                                   /CR_ONLY,$
                                   TEXTID=textid)
              state.w.scaleatmos_fld = [fld,textid]
              widget_control,state.w.scaleatmos_fld[0],SENSITIVE=0

           state.w.plotwin1 = widget_draw(col2_base,$
                                          XSIZE=state.p.plot1size[0],$
                                          YSIZE=state.p.plot1size[1],$
                                          /TRACKING_EVENTS,$
                                          /MOTION_EVENTS,$
                                         EVENT_PRO='xcleanspec_plotwinevent1',$
                                          UVALUE='Plot Window 1')
           
           row_base = widget_base(col2_base,$
                                  /ROW,$
                                  FRAME=2)
           
              xmin = coyote_field2(row_base,$
                                   LABELFONT=buttonfont,$
                                   FIELDFONT=textfont,$
                                   TITLE='X Min:',$
                                   UVALUE='X Min',$
                                   XSIZE=12,$
                                   EVENT_PRO='xcleanspec_minmax',$
                                   /CR_ONLY,$
                                   TEXTID=textid)
              state.w.xmin_fld = [xmin,textid]
              
              xmax = coyote_field2(row_base,$
                                   LABELFONT=buttonfont,$
                                   FIELDFONT=textfont,$
                                   TITLE='X Max:',$
                                   UVALUE='X Max',$
                                   XSIZE=12,$
                                   EVENT_PRO='xcleanspec_minmax',$
                                   /CR_ONLY,$
                                   TEXTID=textid)
              state.w.xmax_fld = [xmax,textid]
              
              ymin = coyote_field2(row_base,$
                                   LABELFONT=buttonfont,$
                                   FIELDFONT=textfont,$
                                   TITLE='Y Min:',$
                                   UVALUE='Y1 Min',$
                                   XSIZE=12,$
                                   EVENT_PRO='xcleanspec_minmax',$
                                   /CR_ONLY,$
                                   TEXTID=textid)
              state.w.ymin1_fld = [ymin,textid]
              
              ymax = coyote_field2(row_base,$
                                   LABELFONT=buttonfont,$
                                   FIELDFONT=textfont,$
                                   TITLE='Y Max:',$
                                   UVALUE='Y1 Max',$
                                   XSIZE=12,$
                                   EVENT_PRO='xcleanspec_minmax',$
                                   /CR_ONLY,$
                                   TEXTID=textid)
              state.w.ymax1_fld = [ymax,textid]
              
           state.w.plotwin2 = widget_draw(col2_base,$
                                          XSIZE=state.p.plot2size[0],$
                                          YSIZE=state.p.plot2size[1],$
                                          /TRACKING_EVENTS,$
                                          /MOTION_EVENTS,$
                                         EVENT_PRO='xcleanspec_plotwinevent2',$
                                          UVALUE='Plot Window 2')
           
           row_base = widget_base(col2_base,$
                                  /ROW,$
                                  FRAME=2)
           
              ymin = coyote_field2(row_base,$
                                   LABELFONT=buttonfont,$
                                   FIELDFONT=textfont,$
                                   TITLE='Y Min:',$
                                   UVALUE='Y2 Min',$
                                   XSIZE=12,$
                                   EVENT_PRO='xcleanspec_minmax',$
                                   /CR_ONLY,$
                                   TEXTID=textid)
              state.w.ymin2_fld = [ymin,textid]
              
              ymax = coyote_field2(row_base,$
                                   LABELFONT=buttonfont,$
                                   FIELDFONT=textfont,$
                                   TITLE='Y Max:',$
                                   UVALUE='Y2 Max',$
                                   XSIZE=12,$
                                   EVENT_PRO='xcleanspec_minmax',$
                                   /CR_ONLY,$
                                   TEXTID=textid)
              state.w.ymax2_fld = [ymax,textid]
              
   button = widget_button(state.w.xcleanspec_base,$
                          FONT=buttonfont,$
                          EVENT_PRO='xcleanspec_event',$
                          VALUE='Help',$
                          UVALUE='Help')
   
; Get things running.  Center the widget using the Fanning routine.
  
  cgcentertlb,state.w.xcleanspec_base
  widget_control, state.w.xcleanspec_base, /REALIZE

;  Get plotwin ids
  
  widget_control, state.w.plotwin1, GET_VALUE=x
  state.p.plotwin1_wid = x 
  wset, state.p.plotwin1_wid
  polyfill,[0,0,1,1,0],[0,1,1,0,0],COLOR=20,/NORM


  widget_control, state.w.plotwin2, GET_VALUE=x
  state.p.plotwin2_wid = x
  wset, state.p.plotwin2_wid
  polyfill,[0,0,1,1,0],[0,1,1,0,0],COLOR=20,/NORM

  
;  Create pixmap windows

  window, /FREE, /PIXMAP,XSIZE=state.p.plot1size[0],YSIZE=state.p.plot1size[1]
  polyfill,[0,0,1,1,0],[0,1,1,0,0],COLOR=20,/NORM
  state.p.pixmap1_wid = !d.window

  window, /FREE, /PIXMAP,XSIZE=state.p.plot2size[0],YSIZE=state.p.plot2size[1]
  polyfill,[0,0,1,1,0],[0,1,1,0,0],COLOR=20,/NORM
  state.p.pixmap2_wid = !d.window

;  Get sizes for things.

  widget_geom = widget_info(state.w.xcleanspec_base, /GEOMETRY)
  state.p.buffer[0]=widget_geom.xsize-state.p.plot1size[0]
  state.p.buffer[1]=widget_geom.ysize-state.p.plot1size[1]-$
                    state.p.plot2size[1]

; Start the Event Loop. This will be a non-blocking program.

  XManager, 'xcleanspec', $
            state.w.xcleanspec_base, $
            CLEANUP='xcleanspec_cleanup',$
            EVENT_HANDLER='xcleanspec_resize',$
            /NO_BLOCK

; Put state variable into the user value of the top level base.

  widget_control, state.w.xcleanspec_base, SET_UVALUE=state, /NO_COPY

cont:

end
