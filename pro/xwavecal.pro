pro xwavecal_cleanup,base

  widget_control, base, GET_UVALUE=state, /NO_COPY
  if n_elements(state) ne 0 then begin

     ptr_free, state.d.x
     ptr_free, state.d.y
     ptr_free, state.d.lines
     ptr_free, state.d.types
     ptr_free, state.d.xpos
     ptr_free, state.d.w2pcoeffs
     ptr_free, state.d.p2wcoeffs
     ptr_free, state.d.wave
     ptr_free, state.d.resid

     ptr_free, state.r.fit
     ptr_free, state.p.overlay

  endif

  state = 0B

end
;
;==============================================================================
;
pro xwavecal_delete,state


  idx = findgen(n_elements(*state.d.xpos))

  z = where(idx ne state.r.linenum)

  *state.d.xpos = (*state.d.xpos)[z]
  *state.d.wave = (*state.d.wave)[z]

  widget_control, state.w.lines_dl,SET_VALUE=strtrim(*state.d.wave,2)

  xwavecal_fitdisp,state
  xwavecal_plotupdate,state

 
end
;
;==============================================================================
;
pro xwavecal_fitdisp,state

  if n_elements(*state.d.xpos) lt 2 then return

  fitdeg = (n_elements(*state.d.xpos)-1) < state.r.fitdeg
  
  *state.d.p2wcoeffs = poly_fit1d(*state.d.xpos,*state.d.wave, $
                                  fitdeg,/SILENT,CANCEL=cancel)
  if cancel then return
  
  *state.d.w2pcoeffs = poly_fit1d(*state.d.wave,*state.d.xpos, $
                                  fitdeg,/SILENT,CANCEL=cancel)
  if cancel then return
  
  
  *state.d.resid = *state.d.xpos-poly(*state.d.wave,*state.d.w2pcoeffs)

end
;
;******************************************************************************
;
pro xwavecal_fitline,state

  z = where(*state.d.x ge state.r.linereg[0] and $
            *state.d.x le state.r.linereg[1],cnt)
  
  case state.r.fittype of 
     
     0: begin
        
        tabinv,*state.d.x,state.r.linereg[0],lidx
        tabinv,*state.d.x,state.r.linereg[1],ridx
        lidx = mc_roundgt(lidx)
        ridx = mc_roundgt(ridx)
        
        *state.r.fit = double( total((*state.d.x)[lidx:ridx]* $
                                     (*state.d.y)[lidx:ridx],/DOUBLE)/$
                               total((*state.d.y)[lidx:ridx],/DOUBLE) )
        
        *state.p.overlay = !values.f_nan
        
        widget_control, state.w.xpos_fld[1],SET_VALUE=strtrim(*state.r.fit,2)
        
        
     end
     
     1: begin
        
        result = mpfitpeak(double((*state.d.x)[z]),double((*state.d.y)[z]), $
                           a,NTERMS=3)
        *state.r.fit = [a[1],a[2],a[0]]
        *state.p.overlay = [[(*state.d.x)[z]],[[result]]]
        
        widget_control, state.w.xpos_fld[1],SET_VALUE=strtrim(a[1],2)
        
     end
     
     2: begin

        result = mpfitpeak(double((*state.d.x)[z]),double((*state.d.y)[z]), $
                           a,NTERMS=4)
        *state.r.fit = [a[1],a[2],a[0],a[3]]
        *state.p.overlay = [[(*state.d.x)[z]],[[result]]]
        
        widget_control, state.w.xpos_fld[1],SET_VALUE=strtrim(a[1],2)
        
     end
     
     3: begin
        
        result = mpfitpeak(double((*state.d.x)[z]),double((*state.d.y)[z]), $
                           a,NTERMS=5)
        *state.r.fit = [a[1],a[2],a[0],a[3],a[4]]
        *state.p.overlay = [[(*state.d.x)[z]],[[result]]]
        
        widget_control, state.w.xpos_fld[1],SET_VALUE=strtrim(a[1],2)
        
     end
     
  endcase


  if n_elements(*state.d.xpos) ge 2 then begin

     lam = poly(a[1],*state.d.p2wcoeffs)
     xmin = min(*state.d.x,MAX=xmax,/NAN)
     wrange = poly([xmin,xmax],*state.d.p2wcoeffs)

     z = where(*state.d.lines gt wrange[0] and $
               *state.d.lines lt wrange[1],cnt)

     slines = (*state.d.lines)[z]
     del = abs(replicate(lam,cnt)-slines)

     min = min(del,idx)
     widget_control, state.w.wave_fld[1],SET_VALUE=strtrim(slines[idx],2)
     
  endif

  tvcrs,-0.1,0.5,/NORM

  mc_setfocus,state.w.wave_fld
  
end
;
;******************************************************************************
;
pro xwavecal_loadspec,state

;  Get files.

  spemc_cfile = mc_cfld(state.w.ispectrum_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return
  spemc_cfile = mc_cfile(spemc_cfile,WIDGET_ID=state.w.xwavecal_base, $
                         CANCEL=cancel)
  if cancel then return

  linefile = mc_cfld(state.w.ilist_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return

;  Load spectrum

  mc_readspec,spemc_cfile,spc,hdr,obsmode,start,stop,norders,naps,orders,$
              xunits,yunits,slith_pix,slith_arc,slitw_pix,slitw_arc,$
              rp,airmass,xtitle,ytitle,/SILENT,CANCEL=cancel
  if cancel then return
  
  if norders eq 1 and naps eq 1 then begin
     
     z = where(finite(spc[*,0,0]) eq 1)
     
     *state.d.x  = spc[z,0,0]
     *state.d.y  = spc[z,1,0] 
     
  endif else begin
     
     xmc_pickorder,orders,naps,porder,pap, $
                   GROUP_LEADER=state.w.xwavecal_base,CANCEL=cancel
     if cancel then return
     z = where(orders eq porder)
     
     *state.d.x  = spc[*,0,z*naps+(pap-1)]
     *state.d.y  = spc[*,1,z*naps+(pap-1)]
     
  endelse
  
  idx = mc_nantrim(*state.d.x,2)
  *state.d.x = (*state.d.x)[idx]
  *state.d.y = (*state.d.y)[idx]

;  Get plot info
  
  state.p.xtitle = xtitle
  state.p.ytitle = ytitle

  state.p.plot1xrange = [min(*state.d.x,MAX=max),max]
  state.p.plot1yrange = [0,max(*state.d.y,/NAN)]

  state.p.plot1absxrange  = state.p.plot1xrange
  state.p.plot1absyrange = state.p.plot1yrange

;  Get line list and prune if necessary

  if state.r.singlecolumn then begin

     readcol,linefile,wave,COMMENT='#',FORMAT='D',/SILENT
     type = replicate('na',n_elements(wave))

  endif else readcol,linefile,wave,type,COMMENT='#',FORMAT='D,A',/SILENT

  *state.d.lines = wave
  *state.d.types = type
  *state.d.xpos = !values.f_nan
  *state.d.wave = !values.f_nan

;  Unfreeze the widget 

  state.r.freeze = 0
  widget_control, state.w.plotwin1, DRAW_BUTTON_EVENTS=1

;  Plot first spectrum

  xwavecal_plotupdate,state

out:

end
;
;******************************************************************************
;
pro xwavecal_plot,state

  z    = where(finite(*state.d.x) eq 1)
  wave = (*state.d.x)[z]
  flux = (*state.d.y)[z]
  
  if n_elements(*state.d.xpos) lt 2 then begin

     if state.p.plottype eq 'Fit' then begin

        plot,wave,flux,PSYM=10,/XSTY,/YSTY,XRANGE=state.p.plot1xrange,$
             YRANGE=state.p.plot1yrange,XTITLE=state.p.xtitle, $
             YTITLE=state.p.ytitle,CHARSIZE=1.5
        
     endif else begin

        xyouts,0.5,0.5,'Not enough points for a fit.',/NORM,CHARSIZE=2, $
               ALIGNMENT=0.5
        return

     endelse

  endif else begin

     x = poly(*state.d.lines,*state.d.w2pcoeffs)
     z = where(x gt 0 and x lt max(wave),cnt)
     
     if state.p.plottype eq 'Fit' then begin

        lineid_plot,wave,flux,x[z],strtrim((*state.d.lines)[z],2)+' '+$
                    strtrim((*state.d.types)[z],2),/EXTEND,$
                    PSYM=10,/XSTY,/YSTY,XRANGE=state.p.plot1xrange,$
                    YRANGE=state.p.plot1yrange,XTITLE=state.p.xtitle, $
                    YTITLE=state.p.ytitle,CHARSIZE=1.5,LCHARSIZE=1.5,$
                    /TRADITIONAL

     endif else begin
        
        lineid_plot,*state.d.xpos,*state.d.resid,x[z], $
                    strtrim((*state.d.lines)[z],2)+' '+$
                    strtrim((*state.d.types)[z],2), $
                    /EXTEND,PSYM=1,/XSTY,/YSTY,XRANGE=state.p.plot1xrange,$
                    XTITLE=state.p.xtitle,YTITLE='Residual (pixels)', $
                    CHARSIZE=1.5,LCHARSIZE=1.5,/TRADITIONAL
        plotsym,0,1,/FILL
        oplot,*state.d.xpos,*state.d.resid,COLOR=2,PSYM=-8
        plots,!x.crange,[0,0],LINESTYLE=1

        xyouts,0.01,0.95,strtrim(n_elements(*state.d.xpos),2)+' points', $
               COLOR=5,ALIGNMENT=0,/NORM,CHARSIZE=2

        fitdeg = (n_elements(*state.d.xpos)-1) < state.r.fitdeg
        xyouts,0.01,0.9,strtrim(fitdeg,2)+' deg', $
               COLOR=5,ALIGNMENT=0,/NORM,CHARSIZE=2

     endelse

          
  endelse

  state.p.pscale1 = !p
  state.p.xscale1 = !x
  state.p.yscale1 = !y

; Plot found lines

  for i = 0,n_elements(*state.d.xpos)-1 do $
     plots,[(*state.d.xpos)[i],(*state.d.xpos)[i]],!y.crange, $
           COLOR=3,LINESTYLE=0
  
  if state.p.plottype eq 'Fit' then begin
        
;  Plot Line Region if necessary
     
     plots, [state.r.linereg[0],state.r.linereg[0]],!y.crange,COLOR=7, $
            LINESTYLE=2
     plots, [state.r.linereg[1],state.r.linereg[1]],!y.crange,COLOR=7, $
            LINESTYLE=2
     
;  Plot fitted line center
     
     plots,[(*state.r.fit)[0],(*state.r.fit)[0]],!y.crange,COLOR=4
     
     if n_elements(*state.p.overlay) gt 1 then begin
        
        oplot,(*state.p.overlay)[*,0],(*state.p.overlay)[*,1],COLOR=3,PSYM=10
        
     endif
     
  endif  
      
end
;
;******************************************************************************
;
pro xwavecal_plotupdate,state

;  Plot window 1

  wset, state.p.pixmap1_wid  
  erase
  xwavecal_plot,state
  xwavecal_setminmax,state

  wset, state.p.plotwin1_wid
  device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
                state.p.pixmap1_wid]

end
;
;******************************************************************************
;
pro xwavecal_setminmax,state

  widget_control,state.w.xmin_fld[1], $
                 SET_VALUE=strtrim(state.p.plot1xrange[0],2)
  widget_control,state.w.xmax_fld[1], $
                 SET_VALUE=strtrim(state.p.plot1xrange[1],2)
  widget_control,state.w.ymin1_fld[1], $
                 SET_VALUE=strtrim(state.p.plot1yrange[0],2)
  widget_control,state.w.ymax1_fld[1], $
                 SET_VALUE=strtrim(state.p.plot1yrange[1],2)

end
;
;******************************************************************************
;
pro xwavecal_store,state

  xpos = mc_cfld(state.w.xpos_fld,5,/EMPTY,CANCEL=cancel)
  if cancel then return
  wave = mc_cfld(state.w.wave_fld,5,/EMPTY,CANCEL=cancel)
  if cancel then return

  z = where(finite(*state.d.xpos) eq 1,cnt)
  if cnt eq 0 then begin

     (*state.d.xpos) = double(xpos)
     (*state.d.wave) = double(wave)


  endif else begin

     *state.d.xpos = [*state.d.xpos,double(xpos)]
     *state.d.wave = [*state.d.wave,double(wave)]

  endelse

  z = where(finite(*state.d.xpos) eq 1)
  
  (*state.d.xpos) = (*state.d.xpos)[z]
  (*state.d.wave) = (*state.d.wave)[z]

  s = sort(*state.d.xpos)
  
  (*state.d.xpos) = (*state.d.xpos)[s]
  (*state.d.wave) = (*state.d.wave)[s]

  widget_control, state.w.wave_fld[1],SET_VALUE=''
  widget_control, state.w.xpos_fld[1],SET_VALUE=''

  state.r.linereg = !values.f_nan
  *state.r.fit = !values.f_nan
  *state.p.overlay = !values.f_nan
  state.r.cursormode = 'None'

  xwavecal_fitdisp,state
  xwavecal_plotupdate,state

  widget_control, state.w.lines_dl,SET_VALUE=strtrim(*state.d.wave,2)

  widget_control, state.w.keyboard, /INPUT_FOCUS, /SENSITIVE
  widget_control, state.w.plotwin1, /INPUT_FOCUS
  tvcrs,0.5,0.5,/NORM
  
end
;
;******************************************************************************
;
pro xwavecal_writefile,state


  file = mc_cfld(state.w.oname_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return

  openw, lun, file,/GET_LUN,WIDTH=200
  
  printf, lun, mc_datetag()
  printf, lun, '# W2P Coeffs= ',*state.d.w2pcoeffs
  printf, lun, '# P2W Coeffs= ',*state.d.p2wcoeffs
  printf, lun, '#'
  mc_filltable,lun,2,$
               {l:'Wavelength',v:*state.d.wave,f:'D12.7',u:'um'},$
               {l:'X',v:*state.d.xpos,f:'D12.7',u:'pixels'}
               
  
  free_lun, lun

end
;
;******************************************************************************
;
pro xwavecal_zoom,state,IN=in,OUT=out

  if state.p.plotwin eq 1 then begin

     delabsx = state.p.plot1absxrange[1]-state.p.plot1absxrange[0]
     delx    = state.p.plot1xrange[1]-state.p.plot1xrange[0]
     
     delabsy = state.p.plot1absyrange[1]-state.p.plot1absyrange[0]
     dely    = state.p.plot1yrange[1]-state.p.plot1yrange[0]
     
     xcen = state.p.plot1xrange[0]+delx/2.
     ycen = state.p.plot1yrange[0]+dely/2.
     
     case state.r.cursormode of 
        
        'XZoom': begin
           
           z = alog10(delabsx/delx)/alog10(2)
           if keyword_set(IN) then z = z+1 else z=z-1
           hwin = delabsx/2.^z/2.
           state.p.plot1xrange = [xcen-hwin,xcen+hwin]
           xwavecal_plotupdate,state
           xwavecal_setminmax,state
           
        end
        
        'YZoom': begin
           
           z = alog10(delabsy/dely)/alog10(2)
           if keyword_set(IN) then z = z+1 else z=z-1
           hwin = delabsy/2.^z/2.
           state.p.plot1yrange = [ycen-hwin,ycen+hwin]
           xwavecal_plotupdate,state
           xwavecal_setminmax,state
           
        end
        
        'Zoom': begin
           
           z = alog10(delabsx/delx)/alog10(2)
           if keyword_set(IN) then z = z+1 else z=z-1
           hwin = delabsx/2.^z/2.
           state.p.plot1xrange = [xcen-hwin,xcen+hwin]
           
           z = alog10(delabsy/dely)/alog10(2)
           if keyword_set(IN) then z = z+1 else z=z-1
           hwin = delabsy/2.^z/2.
           state.p.plot1yrange = [ycen-hwin,ycen+hwin]
           
           xwavecal_plotupdate,state
           xwavecal_setminmax,state
           
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
           xwavecal_plotupdate,state
           xwavecal_setminmax,state
           
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
pro xwavecal_event, event

  widget_control, event.id,  GET_UVALUE = uvalue

  if uvalue eq 'Quit' then begin

     widget_control, event.top, /DESTROY
     goto, getout

  endif


  widget_control, event.top, GET_UVALUE = state, /NO_COPY

  case uvalue of

     'Delete Line': xwavecal_delete,state

     'Fit Type': begin
        
        state.r.fittype = event.index
        state.r.linereg = !values.f_nan
        *state.r.fit = !values.f_nan
        *state.p.overlay = !values.f_nan
        xwavecal_plotupdate,state
        
     end
     
     'Fit Degree': begin

        state.r.fitdeg = event.index
        xwavecal_fitdisp,state
        xwavecal_plotupdate,state

     end

     'Input Line List': begin
        
        obj = dialog_pickfile(DIALOG_PARENT=state.w.xwavecal_base,$
                              /MUST_EXIST)
        if obj eq '' then goto, cont
        widget_control,state.w.ilist_fld[1],SET_VALUE =strtrim(obj,2)
        mc_setfocus,state.w.ilist_fld
        
     end
     
     'Input Spectrum': begin
        
        obj = dialog_pickfile(DIALOG_PARENT=state.w.xwavecal_base,$
                              /MUST_EXIST,FILTER='*.fits')
        if obj eq '' then goto, cont
        widget_control,state.w.ispectrum_fld[1],SET_VALUE =strtrim(obj,2)
        mc_setfocus,state.w.ispectrum_fld
        
     end
     
     'Keyboard': begin
        
        if state.r.freeze then goto, cont
        case strtrim(event.ch,2) of 
           
           'a': BEGIN
              
              state.p.plot1absxrange = state.p.plot1xrange
              state.p.plot1absyrange=state.p.plot1yrange
              
           end
           
           'c': begin
              
              state.r.cursormode = 'None'
              state.r.linereg = !values.f_nan
              *state.r.fit = !values.f_nan
              *state.p.overlay = !values.f_nan
              xwavecal_plotupdate,state
              
           end
           
           'i': xwavecal_zoom,state,/IN
           
           'o': xwavecal_zoom,state,/OUT
           
           's': begin
              
              state.r.cursormode = 'Select'
              state.r.linereg = !values.f_nan
              *state.r.fit = !values.f_nan
              *state.p.overlay = !values.f_nan
              xwavecal_plotupdate,state
              
           end
           
           'w': begin
              
              if state.p.plotwin eq 1 then begin
                 
                 state.p.plot1xrange = state.p.plot1absxrange
                 state.p.plot1yrange = state.p.plot1absyrange
                 
              endif else state.p.plot2yrange = state.p.plot2absyrange
              
              xwavecal_plotupdate,state
              xwavecal_setminmax,state
              
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

     'Line File Columns': state.r.singlecolumn = event.value
     
     'Plot Type': begin

        state.p.plottype = event.value
        

        xwavecal_plotupdate,state
     
     end

     'Store Position': xwavecal_store, state

     'Stored Lines': state.r.linenum = event.index
     
     'Load Spectrum': xwavecal_loadspec,state

     'Wavelength Field': xwavecal_store, state
     
     'Write File': begin
        
        if state.r.freeze then goto, cont
        xwavecal_writefile,state
        
     end
     
     else:
     
  endcase
  
cont: 

  widget_control, state.w.xwavecal_base, SET_UVALUE=state, /NO_COPY

getout:

end
;
;******************************************************************************
;
pro xwavecal_plotwinevent1, event

  widget_control, event.top, GET_UVALUE = state, /NO_COPY
  widget_control, event.id,  GET_UVALUE = uvalue

  if state.p.plottype eq 'Residuals' then goto, cont

;  Check to see if it is a TRACKING event.

  if strtrim(tag_names(event,/STRUCTURE_NAME),2) eq 'WIDGET_TRACKING' then $
     begin

     if event.enter eq 0 then widget_control, state.w.keyboard, SENSITIVE=0
     wset, state.p.plotwin1_wid
     device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
                   state.p.pixmap1_wid]

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

        'Select': begin
           
           if event.type ne 1 then goto, cont     
           z = where(finite(state.r.linereg) eq 1,count)        
           if count eq 0 then begin
              
              state.r.linereg[0] = xy[0]
              xwavecal_plotupdate,state
              
           endif
           if count eq 1 then begin
              
              state.r.linereg[1] = xy[0]
              state.r.linereg = state.r.linereg[sort(state.r.linereg)]
              xwavecal_fitline,state
              xwavecal_plotupdate,state
              
           endif
           
        end
        
        'Zoom': begin
           
           z = where(finite(state.p.reg) eq 1,count)
           if count eq 0 then state.p.reg[*,0] = xy[0:1] else begin 
              
              state.p.reg[*,1] = xy[0:1]
              state.p.plot1xrange   = [min(state.p.reg[0,*],MAX=max),max]
              state.p.plot1yrange   = [min(state.p.reg[1,*],MAX=max),max]
              xwavecal_plotupdate,state
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
              
           endif else begin

              state.p.reg[*,1] = xy[0:1]
              state.p.plot1xrange = [min(state.p.reg[0,*],max=m),m]
              state.r.cursormode = 'None'
              state.p.reg = !values.f_nan
              xwavecal_plotupdate,state
              
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
              state.p.plot1yrange = [min(state.p.reg[1,*],max=m),m]
              state.r.cursormode = 'None'
              state.p.reg[*] = !values.f_nan
              xwavecal_plotupdate,state
              
           endelse

        end

        else:

     endcase

  endif

;  Copy the pixmaps and draw the lines.

  wset, state.p.plotwin1_wid
  device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
                state.p.pixmap1_wid]

  case state.r.cursormode of 

     'XZoom': begin

        wset, state.p.plotwin1_wid
        plots, [event.x,event.x],[0,state.p.plot1size[1]],COLOR=2,/DEVICE

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

     end

  endcase

;  Update cursor position.
  
  if not state.r.freeze then begin

     tabinv, *state.d.x,xy[0],idx
     idx = round(idx)
     label = 'Cursor X: '+strtrim(xy[0],2)+', Y:'+strtrim(xy[1],2)
     label = label+'   Spectrum X: '+$
             strtrim( (*state.d.x)[idx],2)+$
             ', Y:'+strtrim( (*state.d.y)[idx],2)
     widget_control,state.w.message,SET_VALUE=label

  endif

cont:


  widget_control, state.w.xwavecal_base, SET_UVALUE=state, /NO_COPY
  
end
;
;******************************************************************************
;
pro xwavecal_minmax,event

  widget_control, event.top, GET_UVALUE = state, /NO_COPY
  widget_control, event.id,  GET_UVALUE = uvalue

  if state.r.freeze then goto, cont

  case uvalue of 

     'X Min': begin

        xmin = mc_cfld(state.w.xmin_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then goto, cont
        xmin2 = mc_crange(xmin,state.p.plot1xrange[1],'X Min',/KLT,$
                       WIDGET_ID=state.w.xwavecal_base,CANCEL=cancel)
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
                       WIDGET_ID=state.w.xwavecal_base,CANCEL=cancel)
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
                       WIDGET_ID=state.w.xwavecal_base,CANCEL=cancel)
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
                       WIDGET_ID=state.w.xwavecal_base,CANCEL=cancel)
        if cancel then begin
           
           widget_control,state.w.ymax1_fld[0],$
                          SET_VALUE=state.p.plot1yrange[1]
           goto, cont
           
        endif else state.p.plot1yrange[1] = ymax2
        
     end
  endcase

  xwavecal_plotupdate,state

cont:
  widget_control, state.w.xwavecal_base, SET_UVALUE=state, /NO_COPY

end
;
;******************************************************************************
;
pro xwavecal_resize, event

  widget_control, event.top, GET_UVALUE=state,/NO_COPY
  widget_control, event.id,  GET_UVALUE=uvalue

  widget_control, state.w.xwavecal_base,TLB_GET_SIZE=size

; Resize first

  state.p.plot1size[0]=size[0]-state.p.buffer[0]
  state.p.plot1size[1]=(size[1]-state.p.buffer[1])

  widget_control, state.w.plotwin1,UPDATE=0

  widget_control, state.w.plotwin1,DRAW_XSIZE=state.p.plot1size[0]
  widget_control, state.w.plotwin1,DRAW_YSIZE=state.p.plot1size[1]

  widget_control, state.w.plotwin1,UPDATE=1

  wdelete,state.p.pixmap1_wid
  window, /FREE, /PIXMAP,XSIZE=state.p.plot1size[0],YSIZE=state.p.plot1size[1]
  state.p.pixmap1_wid = !d.window

  wset, state.p.plotwin1_wid
  device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
                state.p.pixmap1_wid]

  if not state.r.freeze then xwavecal_plotupdate,state

  widget_control, state.w.xwavecal_base, SET_UVALUE=state, /NO_COPY

end
;
;******************************************************************************
;
; ------------------------------Main Program-------------------------------- 
;
;******************************************************************************
;
pro xwavecal

  if xregistered('xwavecal') then goto, cont

;  Load color table

  mc_mkct
  device, RETAIN=2

  mc_getosinfo,dirsep,strsep,CANCEL=cancel
  if cancel then return

  packagepath = file_dirname(file_dirname(file_which('Instrument.dat')),/MARK)

;  Build three structures which will hold important info.

  w = {box3_base:0L,$
       dirsep:dirsep,$
       keyboard:0L,$
       ispectrum_fld:[0L,0L],$
       ilist_fld:[0L,0L],$
       linefile_bg:0L,$
       lines_dl:0L,$
       message:0L,$
       oname_fld:[0L,0L],$
       path_fld:[0L,0L],$
       plotwin1:0,$
       rp_fld:[0L,0L],$
       thresh_fld:[0L,0L],$
       wave_fld:[0L,0L],$
       wmin_fld:[0L,0L],$
       wmax_fld:[0L,0L],$
       xwavecal_base:0L,$
       xmin_fld:[0L,0L],$
       xmax_fld:[0L,0L],$
       xpos_fld:[0L,0L],$
       ymin1_fld:[0L,0L],$
       ymax1_fld:[0L,0L]}

  r = {cursormode:'None',$
       freeze:1,$
       fittype:2,$
       linereg:[!values.f_nan,!values.f_nan],$
       linenum:0L,$
       fit:ptr_new(!values.f_nan),$
       fitdeg:1,$
       naps:0,$
       norders:0,$
       orders:ptr_new(fltarr(2)),$
       packagepath:packagepath,$
       path:'',$
       plotlines:0,$
       singlecolumn:1,$
       slitw_pix:0.,$
       start:0L,$
       stop:0L}

  d = {w2pcoeffs:ptr_new(!values.f_nan),$
       p2wcoeffs:ptr_new(!values.f_nan),$
       idx:0,$
       lines:ptr_new(!values.f_nan),$
       types:ptr_new(!values.f_nan),$
       xpos:ptr_new(!values.f_nan),$
       wave:ptr_new(!values.f_nan),$
       x:ptr_new(!values.f_nan),$
       resid:ptr_new(!values.f_nan),$
       y:ptr_new(!values.f_nan)}


  p = {activespec:1,$
       ap:0,$
       atrans:ptr_new(2),$
       awave:ptr_new(2),$
       buffer:[0.,0.],$
       idx:0,$
       order:0,$
       overlay:ptr_new(!values.f_nan),$
       pixmap1_wid:0L,$
       plotatmos:0,$
       plotwin1_wid:0L,$
       plot1absxrange:[0.,0.],$
       plot1absyrange:[0.,0.],$
       plot1scale:0.0,$
       plot1xrange:[0.,0.],$
       plot1yrange:[0.,0.],$
       plot2yrange:[0.,0.],$
       plot1size:[670,400],$
       plotwin:1,$
       plottype:'Fit',$
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

;  Load the three structures in the state structure.

  state = {w:w,r:r,d:d,p:p}

;  Build the widget.

  mc_getfonts,buttonfont,textfont
  
  state.w.xwavecal_base = widget_base(TITLE='Xwavecal', $
                                        /COLUMN,$
                                        /TLB_SIZE_EVENTS)

     quit_button = widget_button(state.w.xwavecal_base,$
                                 FONT=buttonfont,$
                                 EVENT_PRO='xwavecal_event',$
                                 VALUE='Quit',$
                                 UVALUE='Quit')
     
     state.w.keyboard = widget_text(state.w.xwavecal_base, $
                                    /ALL_EVENTS, $
                                    SCR_XSIZE=1, $
                                    SCR_YSIZE=1, $
                                    UVALUE='Keyboard', $
                                    EVENT_PRO='xwavecal_event',$
                                    VALUE= '')
  
     row_base = widget_base(state.w.xwavecal_base,$
                            /ROW)

        col1_base = widget_base(row_base,$
                                EVENT_PRO='xwavecal_event',$
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

              row = widget_base(box1_base,$
                                /ROW,$
                                /BASE_ALIGN_CENTER)
              
                 input = widget_button(row,$
                                       FONT=buttonfont,$
                                       VALUE='Input Line List',$
                                       UVALUE='Input Line List')
                 
                 input_fld = coyote_field2(row,$
                                           LABELFONT=buttonfont,$
                                           FIELDFONT=textfont,$
                                           TITLE=':',$
                                           UVALUE='Input Line List Field',$
                                           XSIZE=15,$
                                           TEXTID=textid)
                 state.w.ilist_fld = [input_fld,textid]
                 
                 state.w.linefile_bg = cw_bgroup(box1_base,$
                                                 FONT=buttonfont,$
                                                 ['No','Yes'],$
                                                 /ROW,$
                                                 /NO_RELEASE,$
                                                 /EXCLUSIVE,$
                                                 LABEL_LEFT='Single Column:',$
                                                 SET_VALUE=1,$
                                                 UVALUE='Line File Columns')
                                                   
              load = widget_button(box1_base,$
                                   VALUE='Load Spectrum',$
                                   UVALUE='Load Spectrum',$
                                   FONT=buttonfont)
           
           box2_base = widget_base(col1_base,$
                                   /COLUMN,$
                                   FRAME=2)
           
              label = widget_label(box2_base,$
                                   VALUE='2.  Identify Lines',$
                                   FONT=buttonfont,$
                                   /ALIGN_LEFT)

                 input_fld = coyote_field2(box2_base,$
                                           LABELFONT=buttonfont,$
                                           FIELDFONT=textfont,$
                                           TITLE='Wavelength:',$
                                           UVALUE='Wavelength Field',$
                                           XSIZE=15,$
                                           /CR_ONLY,$
                                           EVENT_PRO='xwavecal_event',$
                                           TEXTID=textid)
                 state.w.wave_fld = [input_fld,textid]


                 input_fld = coyote_field2(box2_base,$
                                           LABELFONT=buttonfont,$
                                           FIELDFONT=textfont,$
                                           TITLE='X Pos:',$
                                           UVALUE='X Pos Field',$
                                           XSIZE=15,$
                                           TEXTID=textid)
                 state.w.xpos_fld = [input_fld,textid]

              button = widget_button(box2_base,$
                                     VALUE='Store Position',$
                                     UVALUE='Store Position',$
                                     FONT=buttonfont)

           box3_base = widget_base(col1_base,$
                                   /COLUMN,$
                                   FRAME=2)

              label = widget_label(box3_base,$
                                   VALUE='3.  Adjust Fit',$
                                   FONT=buttonfont,$
                                   /ALIGN_LEFT)

                 dl = widget_droplist(box3_base,$
                                      VALUE=['0','1','2','3','4','5','6', $
                                             '7','8','9'],$
                                      UVALUE='Fit Degree',$
                                      TITLE='Fit Degree: ',$
                                   FONT=buttonfont)
                 widget_control, dl,SET_DROPLIST_SELECT=1


                 row = widget_base(box3_base,$
                                   /ROW,$
                                   /BASE_ALIGN_CENTER)

                    button = widget_button(row,$
                                           VALUE='Delete',$
                                           UVALUE='Delete Line',$
                                           FONT=buttonfont)
                 
                    state.w.lines_dl = widget_droplist(row,$
                                                       VALUE=['None'],$
                                                       UVALUE='Stored Lines',$
                                                       TITLE='Stored Lines: ',$
                                                       /DYNAMIC_RESIZE,$
                                                       FONT=buttonfont)
              

           box4_base = widget_base(col1_base,$
                                   /COLUMN,$
                                   FRAME=2)
           
              label = widget_label(box4_base,$
                                   VALUE='4.  Write Spectra to File',$
                                   FONT=buttonfont,$
                                   /ALIGN_LEFT)
              
              oname = coyote_field2(box4_base,$
                                    LABELFONT=buttonfont,$
                                    FIELDFONT=textfont,$
                                    TITLE='File Name:',$
                                    UVALUE='Object File Oname',$
                                    XSIZE=18,$
                                    TEXTID=textid)
              state.w.oname_fld = [oname,textid]
              
              write = widget_button(box4_base,$
                                    VALUE='Write File',$
                                    UVALUE='Write File',$
                                    FONT=buttonfont)
              
        col2_base = widget_base(row_base,$
                                EVENT_PRO='xwavecal_event',$
                                /COLUMN)
        
           state.w.message = widget_text(col2_base, $
                                         YSIZE=1)

           row = widget_base(col2_base,$
                             /ROW,$
                            /BASE_ALIGN_CENTER)

              dl = widget_droplist(row,$
                                   VALUE=['Centroid','Gaussian',$
                                          'Gaussian+Constant','Gaussian+Line'],$
                                   UVALUE='Fit Type',$
                                   TITLE='Fit Type: ',$
                                   FONT=buttonfont)
              widget_control, dl, SET_DROPLIST_SELECT=state.r.fittype
              
              bf = cw_bgroup(row,$
                             FONT=buttonfont,$
                             LABEL_LEFT='Plot Type: ',$
                             ['Fit','Residuals'],$
                             /EXCLUSIVE,$
                             /ROW,$
                             /RETURN_NAME,$
                             /NO_RELEASE,$
                             SET_VALUE=0,$
                             UVALUE='Plot Type')
           
           state.w.plotwin1 = widget_draw(col2_base,$
                                          XSIZE=state.p.plot1size[0],$
                                          YSIZE=state.p.plot1size[1],$
                                          /TRACKING_EVENTS,$
                                          /MOTION_EVENTS,$
                                         EVENT_PRO='xwavecal_plotwinevent1',$
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
                                   EVENT_PRO='xwavecal_minmax',$
                                   /CR_ONLY,$
                                   TEXTID=textid)
              state.w.xmin_fld = [xmin,textid]
              
              xmax = coyote_field2(row_base,$
                                   LABELFONT=buttonfont,$
                                   FIELDFONT=textfont,$
                                   TITLE='X Max:',$
                                   UVALUE='X Max',$
                                   XSIZE=12,$
                                   EVENT_PRO='xwavecal_minmax',$
                                   /CR_ONLY,$
                                   TEXTID=textid)
              state.w.xmax_fld = [xmax,textid]
              
              ymin = coyote_field2(row_base,$
                                   LABELFONT=buttonfont,$
                                   FIELDFONT=textfont,$
                                   TITLE='Y Min:',$
                                   UVALUE='Y1 Min',$
                                   XSIZE=12,$
                                   EVENT_PRO='xwavecal_minmax',$
                                   /CR_ONLY,$
                                   TEXTID=textid)
              state.w.ymin1_fld = [ymin,textid]
              
              ymax = coyote_field2(row_base,$
                                   LABELFONT=buttonfont,$
                                   FIELDFONT=textfont,$
                                   TITLE='Y Max:',$
                                   UVALUE='Y1 Max',$
                                   XSIZE=12,$
                                   EVENT_PRO='xwavecal_minmax',$
                                   /CR_ONLY,$
                                   TEXTID=textid)
              state.w.ymax1_fld = [ymax,textid]
              
; Get things running.  Center the widget using the Fanning routine.
  
  cgcentertlb,state.w.xwavecal_base
  widget_control, state.w.xwavecal_base, /REALIZE

;  Get plotwin ids
  
  widget_control, state.w.plotwin1, GET_VALUE=x
  state.p.plotwin1_wid = x

;  Create pixmap windows

  window, /FREE, /PIXMAP,XSIZE=state.p.plot1size[0],YSIZE=state.p.plot1size[1]
  state.p.pixmap1_wid = !d.window

;  Get sizes for things.

  widget_geom = widget_info(state.w.xwavecal_base, /GEOMETRY)
  state.p.buffer[0]=widget_geom.xsize-state.p.plot1size[0]
  state.p.buffer[1]=widget_geom.ysize-state.p.plot1size[1]

; Start the Event Loop. This will be a non-blocking program.

  XManager, 'xwavecal', $
            state.w.xwavecal_base, $
            CLEANUP='xwavecal_cleanup',$
            EVENT_HANDLER='xwavecal_resize',$
            /NO_BLOCK

; Put state variable into the user value of the top level base.

  widget_control, state.w.xwavecal_base, SET_UVALUE=state, /NO_COPY

cont:

end
