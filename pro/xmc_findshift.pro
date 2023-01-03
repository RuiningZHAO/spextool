;+
; NAME:
;     xmc_findshift
;
; PURPOSE:
;     Finds the shift between an object and telluric spectra.
;    
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     result = xmc_findshift(objwave,objflux,telflux,awave,atrans,$
;                            XTITLE=xtitle,INITSHIFT=initshift,$
;                            PARENT=parent,CANCEL=cancel)
;
; INPUTS:
;     objwave - The object wavelength array
;     objflux - The object flux array
;     telflux - The telluric correction spectra (sampled on objwave)
;     awave   - The atmospheric transmission wavelength array
;     atrans  - The atmospheric transmission transmission array     
;    
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     XTITLE    - A string giving the x-axis title
;     INITSHIFT - If given, the telluric spectrum is shifted by
;                 INITSHIFT
;     PARENT    - If given, the parent sensitivity is set to zero
;     CANCEL    - Set on return if there is a problem
;     
; OUTPUTS:
;     Returns the shift between the two spectra
;     
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     xmc_findshift_state
;
; SIDE EFFECTS:
;     None
;
; RESTRICTIONS:
;     None
;
; PROCEDURE:
;     Minimizes RMS deviation of a region by shifting the telluric
;     spectrum relative to the object spectrum
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2002 - Written by M. Cushing
;     2003-03-25 - Removed the error inputs
;     2005-05-24 - Added a new yrange calculator for the division,
;                  removed erase procedure from plotupdate to keep
;                  from blinking the plot windows and shut of motion
;                  events during the autofind.
;     2005-06-15 - Corrected bug where region selection lines remained
;                  when the user hit 'c'
;     2005-08-04 - Changed XUNITS to XTITLE
;     2014-05-13 - Added the atmospheric transmission
;     2014-06-16 - Added the ability to 'o' and 'i' in the zoom modes.
;                  Adjusted yrange determinations to make prism data
;                  more visible.
;-
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xmc_findshift_initcommon,objwave,objflux,telflux,awave,atrans, $
                             XTITLE=xtitle,INITSHIFT=initshift

  shift = (n_elements(INITSHIFT) eq 0) ? 0.0:initshift
  xtitle = (n_elements(XTITLE) eq 0) ? '':xtitle
  
  cleanplot,/SILENT
  
  common xmc_findshift_state, state
  
;  Build three structures which will hold important info.
;  w - contains info pertaining to widget operations.
;  r - contains info pertaining to the reduction process.
;  d - contains all of the data.

  w = {keyboard:0L,$
       plotwin1:0,$
       message:0L,$
       plotwin2:0,$
       rms:0L,$
       scale_fld:[0L,0L],$
       scaleatmos_fld:[0L,0L],$
       shift_fld:[0L,0L],$
       xmc_findshift_base:0L,$
       xmin_fld:[0L,0L],$
       xmax_fld:[0L,0L],$
       ymin1_fld:[0L,0L],$
       ymax1_fld:[0L,0L],$
       ymin2_fld:[0L,0L],$
       ymax2_fld:[0L,0L]}
  
  r = {cancel:0,$
       cursormode:'None',$
       plotatmos:1L,$
       srange:[!values.f_nan,!values.f_nan],$
       shift:shift}
  
  d = {awave:awave,$
       atrans:atrans,$
       divspec:objflux,$
       dtelflux:telflux,$
       objflux:objflux,$
       objwave:objwave,$
       shtelflux:telflux,$
       telflux:telflux}
  
  p = {activespec:1,$
       buffer:[0.,0.],$
       offset:[70,55,40,30],$
       pixmap1_wid:0L,$
       pixmap2_wid:0L,$
       plotwin1_wid:0L,$
       plotwin2_wid:0L,$
       plot1absxrange:[0.,0.],$
       plot1absyrange:[0.,0.],$
       plot1xrange:[0.,0.],$
       plot1scale:0.0,$
       plot1yrange:[0.,0.],$
       plot1size:[780,260],$
       plot2absyrange:[0.,0.],$
       plot2yrange:[0.,0.],$
       plot2scale:0.0,$
       plot2size:[780,260],$
       plotwin:1,$
       position1:[0,0,1,1],$
       position2:[0,0,1,1],$
       pscale1:!p,$
       xscale1:!x,$
       pscale2:!p,$
       xscale2:!x,$
       xtitle:xtitle,$
       yscale1:!y,$
       yscale2:!y,$
       ytitle:'',$
       reg:[[!values.f_nan,!values.f_nan],$
            [!values.f_nan,!values.f_nan]]}
  
  p.plot1scale = float(p.plot1size[1])/(p.plot1size[1]+p.plot2size[1])
  p.plot2scale = float(p.plot2size[1])/(p.plot1size[1]+p.plot2size[1])
  
  p.position1 = [p.offset[0:1],p.plot1size-p.offset[2:3]]
  p.position2 = [p.offset[0:1],p.plot2size-p.offset[2:3]]
  
;  Load the three structures in the state structure.
  
  state = {w:w,r:r,d:d,p:p}
  
end
;
;******************************************************************************
;
pro xmc_findshift_autofind

  common xmc_findshift_state
  
  widget_control, state.w.plotwin1,DRAW_MOTION_EVENTS=0
  widget_control, state.w.plotwin2,DRAW_MOTION_EVENTS=0
  
  
  z = where(state.d.objwave gt state.r.srange[0] and state.d.objwave lt $
            state.r.srange[1],count)
  
  shifts = findgen(151)/50.+(state.r.shift-1.5)
  rms    = fltarr(151)
  
  for i = 0, 150 do begin
     
     state.r.shift = shifts[i]
     xmc_findshift_shiftspec
     xmc_findshift_plotupdate
     
     widget_control, state.w.shift_fld[1],SET_VALUE=strtrim(state.r.shift,2)
     mc_moments,state.d.divspec[z],mean,var,stddev,/SILENT
     rms[i] = stddev
     widget_control, state.w.rms, SET_VALUE='RMS:  '+strtrim(stddev,2)
     
  endfor
  
;  Choose best shift value and update plot and message window
  
  min = min(rms,minidx)
  
  del = 5
;window, 2
;plot,shifts,rms,/XSTY,/YSTY,xrange=[-0.5,0.5]
  
  coeff = poly_fit1d(shifts[(minidx-del):(minidx+del)],$
                     rms[(minidx-del):(minidx+del)],2,/SILENT)
  
;oplot,shifts[(minidx-del):(minidx+del)],$
;  poly(shifts[(minidx-del):(minidx+del)],coeff),color=2
  
  state.r.shift = -coeff[1]/2./coeff[2]
  
  widget_control, state.w.shift_fld[1],SET_VALUE=strtrim(state.r.shift,2)
  widget_control, state.w.rms, SET_VALUE='RMS:  '+strtrim(min,2)
  
  widget_control, state.w.plotwin1,DRAW_MOTION_EVENTS=1
  widget_control, state.w.plotwin2,DRAW_MOTION_EVENTS=1
  
  xmc_findshift_shiftspec
  xmc_findshift_plotupdate
  xmc_findshift_setminmax
  
end
;
;*****************************************************************************
;
pro xmc_findshift_plotresid

  common xmc_findshift_state

  plot,state.d.objwave,state.d.divspec,/XSTY,/YSTY,$
       XRANGE=state.p.plot1xrange,YRANGE=state.p.plot2yrange,PSYM=10,$
       XTITLE=state.p.xtitle,YTITLE='!5Arbitray Flux',$
       TITLE='!5Object x Telluric',CHARSIZE=1.5,$
       POSITION=state.p.position2,/DEVICE
  
  
  z = where(finite(state.r.srange) eq 1,count)
  if count eq 0 then goto, cont
  
  for i = 0, count-1 do plots,[state.r.srange[i],state.r.srange[i]], $
                              !y.crange,COLOR=7,LINESTYLE=2
  
  state.p.xscale2 = !x
  state.p.yscale2 = !y
  state.p.pscale2 = !p
  
cont:

end
;
;******************************************************************************
;
pro xmc_findshift_plotspec

  common xmc_findshift_state

  scale = mc_cfld(state.w.scaleatmos_fld,4,/EMPTY,CANCEL=cancel)
  if cancel then return
  
  z = where(state.d.awave lt state.p.plot1absxrange[1] and $
            state.d.awave gt state.p.plot1absxrange[0],count)
  
  wtrans = state.d.awave[z] 
  spec   = state.d.atrans[z] 
  spec   = (temporary(spec)-1.0)*scale+1.0
  
  if state.r.plotatmos then begin
     
     z = where( wtrans lt state.p.plot1xrange[1] and $
                wtrans gt state.p.plot1xrange[0],count)
     
     if count ne 0 then begin
        
        plot, wtrans,spec,COLOR=5,YRANGE=[0,1],YSTYLE=5,XSTYLE=5,$
              XRANGE=state.p.plot1xrange,PSYM=10,CHARSIZE=1.5,$
              POSITION=state.p.position1,/DEVICE
        ticks = string(findgen(11)*.1,FORMAT='(f3.1)')
        axis,YAXIS=1,YTICKS=10,YTICKNAME='!5'+ticks,YMINOR=1,COLOR=5, $
             CHARSIZE=1.5
        
     endif
     ystyle = 9
     noerase = 1
     
  endif else begin
     
     ystyle=1
     noerase = 0
     
  endelse

  plot,state.d.objwave,state.d.objflux,/XSTY,YSTY=ystyle,$
       YRANGE=state.p.plot1yrange,XRANGE=state.p.plot1xrange,PSYM=10,$
       XTITLE=state.p.xtitle,YTITLE='!5Arbitrary Flux', CHARSIZE=1.5,/NOERASE,$
       POSITION=state.p.position1,/DEVICE
  oplot,state.d.objwave,state.d.dtelflux,COLOR=3,PSYM=10
  

  xyouts,state.p.position1[0]+10,state.p.position1[3]+10, $
         'Object Spectrum',CHARSIZE=1.5,ALIGNMENT=0,/DEVICE

  xyouts,state.p.position1[2]-10,state.p.position1[3]+10, $
         '1/TelluricSpectrum',CHARSIZE=1.5,ALIGNMENT=1,COLOR=3,/DEVICE

;  Plot scale region lines

  z = where(finite(state.r.srange) eq 1,count)
  if count eq 0 then goto, cont
  
  for i = 0, count-1 do plots,[state.r.srange[i],state.r.srange[i]],!y.crange,$
                              COLOR=7,LINESTYLE=2
  
  state.p.xscale1 = !x
  state.p.yscale1 = !y
  state.p.pscale1 = !p
  
cont:

end
;
;******************************************************************************
;
pro xmc_findshift_plotupdate

  common xmc_findshift_state

;  Plot window 1

  wset, state.p.pixmap1_wid
  erase
  xmc_findshift_plotspec 
  
  wset, state.p.plotwin1_wid
  device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
                state.p.pixmap1_wid]
  
;  Plot window 2
  
  wset, state.p.pixmap2_wid
  erase
  xmc_findshift_plotresid
  
  wset, state.p.plotwin2_wid
  device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,0,$
                state.p.pixmap2_wid]
  
END
;
;******************************************************************************
;
pro xmc_findshift_setminmax

  common xmc_findshift_state
  
  widget_control,state.w.xmin_fld[1],SET_VALUE=strtrim(state.p.plot1xrange[0],2)
  widget_control,state.w.xmax_fld[1],SET_VALUE=strtrim(state.p.plot1xrange[1],2)
  widget_control,state.w.ymin1_fld[1],SET_VALUE=strtrim(state.p.plot1yrange[0],2)
  widget_control,state.w.ymax1_fld[1],SET_VALUE=strtrim(state.p.plot1yrange[1],2)
  
  widget_control,state.w.ymin2_fld[1],SET_VALUE=strtrim(state.p.plot2yrange[0],2)
  widget_control,state.w.ymax2_fld[1],SET_VALUE=strtrim(state.p.plot2yrange[1],2)

end
;
;******************************************************************************
;
pro xmc_findshift_shiftspec

  common xmc_findshift_state

;  Shift std spectrum.

  x1 = findgen(n_elements(state.d.telflux))+state.r.shift
  x2 = findgen(n_elements(state.d.telflux))

  mc_interpspec,x1,state.d.telflux,x2,y,error,CANCEL=cancel
  if cancel then return
  
  state.d.shtelflux  = y

;  Divide spectra.
  
  state.d.divspec = state.d.objflux*state.d.shtelflux
  state.d.divspec = state.d.divspec/median(state.d.divspec,/EVEN)
  
end
;
;******************************************************************************
;
pro xmc_findshift_setup

  common xmc_findshift_state

;  Normalize the object spectrum.

  smooth = median(state.d.objflux,4)
  state.d.objflux = state.d.objflux/max(smooth,/NAN)

  smooth = median(1./state.d.telflux,4)
  state.d.dtelflux = 1./state.d.telflux/max(smooth,/NAN)
  
;  Get ranges.
  
  state.p.plot1xrange = [min(state.d.objwave,/NAN,MAX=max),max]
  state.r.srange         = state.p.plot1xrange
  
  state.p.plot1yrange = [0,max([max(state.d.objflux,/NAN),$
                                max(state.d.dtelflux,/NAN)])]
  
  state.p.plot1absxrange = state.p.plot1xrange
  state.p.plot1absyrange = state.p.plot1yrange
  
  xmc_findshift_shiftspec

  mc_interpspec,state.d.awave,state.d.atrans,state.d.objwave,trans,CANCEL=cancel
  if cancel then return

  z = where(trans gt 0.6)

  state.p.plot2yrange = [min((median(state.d.divspec,4))[z],/NAN,MAX=max),max]
  state.p.plot2absyrange = state.p.plot2yrange

  xmc_findshift_plotupdate
  xmc_findshift_setminmax
  
end
;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro xmc_findshift_event, event

common xmc_findshift_state

widget_control, event.id,  GET_UVALUE=uvalue

case uvalue of

    'Accept': widget_control, event.top, /DESTROY

    'Atmosphere': begin

        state.r.plotatmos = event.select
        xmc_findshift_plotupdate

    end

    'Auto Find': xmc_findshift_autofind

    'Cancel': begin

        state.r.cancel = 1
        widget_control, event.top, /DESTROY

    end

    'Keyboard': begin

        case strtrim(event.ch,2) of 
            
            'c': begin
                
                state.r.cursormode = 'None'
                state.p.reg = !values.f_nan
                state.r.srange=!values.f_nan
                xmc_findshift_plotupdate
            
            end

            'i': xmc_findshift_zoom,/IN

            'o': xmc_findshift_zoom,/OUT

            'w': begin

                if state.p.plotwin eq 1 then begin

                    state.p.plot1xrange = state.p.plot1absxrange
                    state.p.plot1yrange = state.p.plot1absyrange
                    xmc_findshift_plotupdate
                    xmc_findshift_setminmax
                
                endif else begin

                    state.p.plot2yrange = state.p.plot2absyrange
                    xmc_findshift_plotupdate
                    xmc_findshift_setminmax

                endelse
            end

            's': begin

                state.r.cursormode = 'Select'
                state.p.reg=!values.f_nan
                state.r.srange=!values.f_nan
                xmc_findshift_plotupdate

            end

            'x': begin 

                state.r.cursormode = 'XZoom'
                state.p.reg=!values.f_nan
                
            end

            'y': begin 
                
                state.r.cursormode = 'YZoom'
                state.p.reg=!values.f_nan

            end

            'z': begin

                state.r.cursormode = 'Zoom'
                state.p.reg=!values.f_nan
                
            end
        
            else:

        endcase

    end

    'Scale Atmosphere': xmc_findshift_plotupdate

    'Shift': begin

        shift = mc_cfld(state.w.shift_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then return
        state.r.shift = shift
        xmc_findshift_shiftspec

        z = where(state.d.objwave gt state.r.srange[0] and $
                  state.d.objwave lt state.r.srange[1])
        mc_moments,(state.d.divspec)[z],mean,var,stddev,/SILENT
        widget_control, state.w.rms, SET_VALUE='RMS:  '+strtrim(stddev,2)

        xmc_findshift_plotupdate

    end

    else:

endcase

cont: 

END
;
;******************************************************************************
;
pro xmc_findshift_plotwinevent1, event

  common xmc_findshift_state
  
  widget_control, event.id,  GET_UVALUE = uvalue
  
;  Check to see if it is a TRACKING event.
  
  if strtrim(tag_names(event,/STRUCTURE_NAME),2) eq 'WIDGET_TRACKING' then begin
     
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
  wset, state.p.plotwin1_wid
  
  !p = state.p.pscale1
  !x = state.p.xscale1
  !y = state.p.yscale1
  x  = event.x/float(state.p.plot1size[0])
  y  = event.y/float(state.p.plot1size[1])
  xy = convert_coord(x,y,/NORMAL,/TO_DATA)
  
  if event.type eq 1 then begin
     
     case state.r.cursormode of 
        
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
              state.p.plot1xrange = [min(state.p.reg[0,*],MAX=m),m]
              state.r.cursormode = 'None'
              state.p.reg = !values.f_nan
              xmc_findshift_plotupdate
              xmc_findshift_setminmax
              
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
              state.p.plot1yrange = [min(state.p.reg[1,*],MAX=m),m]
              state.r.cursormode = 'None'
              state.p.reg = !values.f_nan
              xmc_findshift_plotupdate
              xmc_findshift_setminmax
              
           endelse
           
        end
        
        'Zoom': begin
           
           z = where(finite(state.p.reg) eq 1,count)
           if count eq 0 then state.p.reg[*,0] = xy[0:1] else begin 
              
              state.p.reg[*,1] = xy[0:1]
              state.p.plot1xrange   = [min(state.p.reg[0,*],MAX=max),max]
              state.p.plot1yrange   = [min(state.p.reg[1,*],MAX=max),max]
              xmc_findshift_plotupdate
              xmc_findshift_setminmax
              state.r.cursormode   = 'None'
              state.p.reg = !values.f_nan
              
           endelse
           
        end
        
        'Select': begin
           
           z = where(finite(state.r.srange) eq 1,count)
           if count eq 0 then begin
              
              state.r.srange[0] = xy[0]
              xmc_findshift_plotupdate
              
           endif else begin 
              
              state.r.srange[1] = xy[0]
              x1 = xy[0] < state.r.srange[0]
              x2 = xy[0] > state.r.srange[0]
              
              state.r.srange = [x1,x2]
              xmc_findshift_plotupdate
              state.r.cursormode = 'None'
              state.p.reg = !values.f_nan
              
              
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
  
  label = 'Cursor X: '+strtrim(xy[0],2)
  widget_control,state.w.message,SET_VALUE=label
  
cont:
    
end
;
;******************************************************************************
;
pro xmc_findshift_plotwinevent2, event

common xmc_findshift_state

widget_control, event.id,  GET_UVALUE = uvalue

;  Check to see if it is a TRACKING event.

if strtrim(tag_names(event,/STRUCTURE_NAME),2) eq 'WIDGET_TRACKING' then begin

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
          /DEVICE,LINESTYLE=1,THICK=2
        
        wset, state.p.plotwin2_wid
        device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,0,$
                      state.p.pixmap2_wid]
        
    endif else begin
        
        state.p.reg[*,1] = xy[0:1]
        state.p.plot2yrange = [min(state.p.reg[1,*],MAX=m,/NAN),m]
        state.r.cursormode = 'None'
        state.p.reg=!values.f_nan
        xmc_findshift_plotupdate
        xmc_findshift_setminmax
        
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

label = 'Cursor X: '+strtrim(xy[0],2)
widget_control,state.w.message,SET_VALUE=label

cont:

end
;
;******************************************************************************
;
pro xmc_findshift_minmax,event

common xmc_findshift_state
widget_control, event.id,  GET_UVALUE = uvalue


case uvalue of 

    'X Min': begin

        xmin = mc_cfld(state.w.xmin_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then return
        xmin2 = mc_crange(xmin,state.p.plot1xrange[1],'X Min',/KLT,$
                       WIDGET_ID=state.w.xmc_findshift_base,CANCEL=cancel)
        if cancel then begin
            
            widget_control, state.w.xmin_fld[0],$
              SET_VALUE=state.p.plot1xrange[0]
            return
            
        endif else state.p.plot1xrange[0] = xmin2

    end
    'X Max': begin

        xmax = mc_cfld(state.w.xmax_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then return
        xmax2 = mc_crange(xmax,state.p.plot1xrange[0],'X Max',/KGT,$
                       WIDGET_ID=state.w.xmc_findshift_base,CANCEL=cancel)
        if cancel then begin
            
            widget_control, state.w.xmax_fld[0],$
              SET_VALUE=state.p.plot1xrange[1]
            return
            
        endif else state.p.plot1xrange[1] = xmax2

    end
    'Y1 Min': begin

        ymin = mc_cfld(state.w.ymin1_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then return
        ymin2 = mc_crange(ymin,state.p.plot1yrange[1],'Y Min',/KLT,$
                       WIDGET_ID=state.w.xmc_findshift_base,CANCEL=cancel)
        if cancel then begin
            
            widget_control,state.w.ymin1_fld[0],$
              SET_VALUE=state.p.plot1yrange[0]
            return
            
        endif else state.p.plot1yrange[0] = ymin2
        
    end
    'Y1 Max': begin

        ymax = mc_cfld(state.w.ymax1_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then return
        ymax2 = mc_crange(ymax,state.p.plot1yrange[0],'Y Max',/KGT,$
                       WIDGET_ID=state.w.xmc_findshift_base,CANCEL=cancel)
        if cancel then begin
            
            widget_control,state.w.ymax1_fld[0],set_val=state.p.plot1yrange[1]
            return
            
        endif else state.p.plot1yrange[1] = ymax2
        
    end
    'Y2 Min': begin

        ymin = mc_cfld(state.w.ymin2_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then return
        ymin2 = mc_crange(ymin,state.p.plot2yrange[1],'Y Min',/KLT,$
                       WIDGET_ID=state.w.xmc_findshift_base,CANCEL=cancel)
        if cancel then begin
            
            widget_control,state.w.ymin2_fld[0],set_val=state.p.plot2yrange[0]
            return
            
        endif else state.p.plot2yrange[0] = ymin2
        
    end
    'Y2 Max': begin

        ymax = mc_cfld(state.w.ymax2_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then return
        ymax2 = mc_crange(ymax,state.p.plot2yrange[0],'Y Max',/KGT,$
                       WIDGET_ID=state.w.xmc_findshift_base,CANCEL=cancel)
        if cancel then begin
            
            widget_control,state.w.ymax2_fld[0],set_val=state.p.plot2yrange[1]
            return
            
        endif else state.p.plot2yrange[1] = ymax2
        
    end
    
endcase

if uvalue eq 'X Min' or uvalue eq 'X Max' then xmc_findshift_shiftspec
xmc_findshift_plotupdate
xmc_findshift_setminmax

end
;
;******************************************************************************
;
pro xmc_findshift_resize, event

  common xmc_findshift_state

  widget_control, state.w.xmc_findshift_base, TLB_GET_SIZE=size
  
;  Get new sizes

  state.p.plot1size[0]=size[0]-state.p.buffer[0]
  state.p.plot1size[1]=(size[1]-state.p.buffer[1])*state.p.plot1scale

  state.p.plot2size[0]=size[0]-state.p.buffer[0]
  state.p.plot2size[1]=(size[1]-state.p.buffer[1])*state.p.plot2scale

;  Resize windows
  
  widget_control, state.w.plotwin1,UPDATE=0
  widget_control, state.w.plotwin2,UPDATE=0
  
  widget_control, state.w.plotwin1, DRAW_XSIZE=state.p.plot1size[0]
  widget_control, state.w.plotwin1, DRAW_YSIZE=state.p.plot1size[1]
  
  widget_control, state.w.plotwin2, DRAW_XSIZE=state.p.plot2size[0]
  widget_control, state.w.plotwin2, DRAW_YSIZE=state.p.plot2size[1]
  
  widget_control, state.w.plotwin1,UPDATE=1
  widget_control, state.w.plotwin2,UPDATE=1
  
;  Redo pix maps

  wdelete,state.p.pixmap1_wid
  window, /FREE, /PIXMAP,XSIZE=state.p.plot1size[0],YSIZE=state.p.plot1size[1]
  state.p.pixmap1_wid = !d.window
  
  wdelete,state.p.pixmap2_wid
  window, /FREE, /PIXMAP,XSIZE=state.p.plot2size[0],YSIZE=state.p.plot2size[1]
  state.p.pixmap2_wid = !d.window

;  Update plot positions

  state.p.position1 = [state.p.offset[0:1],state.p.plot1size-state.p.offset[2:3]]
  state.p.position2 = [state.p.offset[0:1],state.p.plot2size-state.p.offset[2:3]]
  
  xmc_findshift_plotupdate

end
;
;******************************************************************************
;
pro xmc_findshift_zoom,IN=in,OUT=out

  common xmc_findshift_state

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
           xmc_findshift_plotupdate
           xmc_findshift_setminmax
           
        end
        
        'YZoom': begin
           
           z = alog10(delabsy/dely)/alog10(2)
           if keyword_set(IN) then z = z+1 else z=z-1
           hwin = delabsy/2.^z/2.
           state.p.plot1yrange = [ycen-hwin,ycen+hwin]
           xmc_findshift_plotupdate
           xmc_findshift_setminmax
           
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
           
           xmc_findshift_plotupdate
           xmc_findshift_setminmax
           
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
           xmc_findshift_plotupdate
           xmc_findshift_setminmax
           
        end
        
        else:
        
     endcase
     
  endelse
  
end
;
;******************************************************************************
;
; ------------------------------Main Program-------------------------------- 
;
;******************************************************************************
;
function xmc_findshift,objwave,objflux,telflux,awave,atrans,XTITLE=xtitle,$
                    INITSHIFT=initshift,PARENT=parent,CANCEL=cancel

  mc_mkct
  COMMON xmc_findshift_state
  
  if n_params() lt 5 then begin
     
     print, 'Syntax - result  = xmc_findshift(objwave,objflux,telflux,awave,$'
     print, '                              atrans,XTITLE=xtitle,$'
     print, '                              INITSHIFT=initshift,PARENT=parent,$'
     print, '                              CANCEL=cancel)'
    cancel = 1
    return,-1

 endif
  cancel = mc_cpar('xmc_findshift',objwave,1,'Objwave',[2,3,4,5],1)
  if cancel then return,-1
  cancel = mc_cpar('xmc_findshift',objflux,2,'Objflux',[2,3,4,5],1)
  if cancel then return,-1
  cancel = mc_cpar('xmc_findshift',telflux,3,'Telflux',[2,3,4,5],1)
  if cancel then return,-1
  
  if not xregistered('xmc_findshift') then begin
     
     xmc_findshift_initcommon,objwave,objflux,telflux,awave,atrans, $
                              XTITLE=xtitle,INITSHIFT=initshift
     
     if n_elements(PARENT) ne 0 then widget_control, parent,SENSITIVE=0
     
     mc_getfonts,buttonfont,textfont,CANCEL=cancel
     if cancel then return,-1
     
     state.w.xmc_findshift_base = widget_base(TITLE='Xmc_Findshift', $
                                           /COLUMN,$
                                           /TLB_SIZE_EVENTS)
     
     quit_button = widget_button(state.w.xmc_findshift_base,$
                                 FONT=buttonfont,$
                                 EVENT_PRO='xmc_findshift_event',$
                                 VALUE='Cancel',$
                                 UVALUE='Cancel')
     
     state.w.keyboard = widget_text(state.w.xmc_findshift_base, $
                                    /ALL_EVENTS, $
                                    SCR_XSIZE=1, $
                                    SCR_YSIZE=1, $
                                    UVALUE='Keyboard', $
                                    EVENT_PRO='xmc_findshift_event',$
                                    VALUE= '')
     
     row = widget_base(state.w.xmc_findshift_base,$
                       /ROW,$
                       FRAME=2,$
                       EVENT_PRO='xmc_findshift_event',$
                       /BASE_ALIGN_CENTER)
     
        auto = widget_button(row,$
                             FONT=buttonfont,$
                             VALUE='Auto Find',$
                             UVALUE='Auto Find')
        
        fld = coyote_field2(row,$
                            LABELFONT=buttonfont,$
                            FIELDFONT=textfont,$
                            TITLE='Shift:',$
                            UVALUE='Shift',$
                            VALUE=strtrim(state.r.shift,2),$
                            XSIZE=12,$
                            EVENT_PRO='xmc_findshift_event',$
                            /CR_ONLY,$
                            TEXTID=textid)
        state.w.shift_fld = [fld,textid]
        
        state.w.rms = widget_label(row,$
                                   VALUE='',$
                                   FONT=buttonfont,$
                                   /DYNAMIC_RESIZE)
        
        state.w.message = widget_label(row,$
                                       VALUE=' Cursor X :',$
                                       FONT=buttonfont,$
                                       /DYNAMIC_RESIZE)

        atmos_bg = cw_bgroup(row,$
                             ['Atmosphere'],$
                             FONT=buttonfont,$
                             UVALUE='Atmosphere',$
                             SET_VALUE=[1],$
                             /NONEXCLUSIVE)
        
        
        fld  = coyote_field2(row,$
                             LABELFONT=buttonfont,$
                             FIELDFONT=textfont,$
                             TITLE='Scale:',$
                             UVALUE='Scale Atmosphere',$
                             XSIZE=4,$
                             VALUE=1.0,$
                             EVENT_PRO='xmc_findshift_event',$
                             /CR_ONLY,$
                             TEXTID=textid)
        state.w.scaleatmos_fld = [fld,textid]
        
     state.w.plotwin1 = widget_draw(state.w.xmc_findshift_base,$
                                    XSIZE=state.p.plot1size[0],$
                                    YSIZE=state.p.plot1size[1],$
                                    /TRACKING_EVENTS,$
                                    /BUTTON_EVENTS,$
                                    /MOTION_EVENTS,$
                                    EVENT_PRO='xmc_findshift_plotwinevent1',$
                                    UVALUE='Plot Window 1')
          
     row_base = widget_base(state.w.xmc_findshift_base,$
                            /ROW,$
                            FRAME=2)
     
        xmin = coyote_field2(row_base,$
                             LABELFONT=buttonfont,$
                             FIELDFONT=textfont,$
                             TITLE='X Min:',$
                             UVALUE='X Min',$
                             XSIZE=12,$
                             EVENT_PRO='xmc_findshift_minmax',$
                             /CR_ONLY,$
                             TEXTID=textid)
        state.w.xmin_fld = [xmin,textid]
        
        xmax = coyote_field2(row_base,$
                             LABELFONT=buttonfont,$
                             FIELDFONT=textfont,$
                             TITLE='X Max:',$
                             UVALUE='X Max',$
                             XSIZE=12,$
                             EVENT_PRO='xmc_findshift_minmax',$
                             /CR_ONLY,$
                             TEXTID=textid)
        state.w.xmax_fld = [xmax,textid]
        
        ymin = coyote_field2(row_base,$
                             LABELFONT=buttonfont,$
                             FIELDFONT=textfont,$
                             TITLE='Y Min:',$
                             UVALUE='Y1 Min',$
                             XSIZE=12,$
                             EVENT_PRO='xmc_findshift_minmax',$
                             /CR_ONLY,$
                             TEXTID=textid)
        state.w.ymin1_fld = [ymin,textid]
        
        ymax = coyote_field2(row_base,$
                             LABELFONT=buttonfont,$
                             FIELDFONT=textfont,$
                             TITLE='Y Max:',$
                             UVALUE='Y1 Max',$
                             XSIZE=12,$
                             EVENT_PRO='xmc_findshift_minmax',$
                             /CR_ONLY,$
                             TEXTID=textid)
        state.w.ymax1_fld = [ymax,textid]
        
        state.w.plotwin2 = widget_draw(state.w.xmc_findshift_base,$
                                       XSIZE=state.p.plot2size[0],$
                                       YSIZE=state.p.plot2size[1],$
                                       /TRACKING_EVENTS,$
                                       /MOTION_EVENTS,$
                                       /BUTTON_EVENTS,$
                                       EVENT_PRO='xmc_findshift_plotwinevent2',$
                                       UVALUE='Plot Window 2')
        
        row_base = widget_base(state.w.xmc_findshift_base,$
                               /ROW,$
                               FRAME=2,$
                               EVENT_PRO='xmc_findshift_event')
        
           ymin = coyote_field2(row_base,$
                                LABELFONT=buttonfont,$
                                FIELDFONT=textfont,$
                                TITLE='Y Min:',$
                                UVALUE='Y2 Min',$
                                XSIZE=12,$
                                EVENT_PRO='xmc_findshift_minmax',$
                                /CR_ONLY,$
                                TEXTID=textid)
           state.w.ymin2_fld = [ymin,textid]
           
           ymax = coyote_field2(row_base,$
                                LABELFONT=buttonfont,$
                                FIELDFONT=textfont,$
                                TITLE='Y Max:',$
                                UVALUE='Y2 Max',$
                                XSIZE=12,$
                                EVENT_PRO='xmc_findshift_minmax',$
                                /CR_ONLY,$
                                TEXTID=textid)
           state.w.ymax2_fld = [ymax,textid]
           
     quit_button = widget_button(state.w.xmc_findshift_base,$
                                 FONT=buttonfont,$
                                 EVENT_PRO='xmc_findshift_event',$
                                 VALUE='Accept',$
                                 UVALUE='Accept')
     
; Get things running.  Center the widget using the Fanning routine.
     
     cgcentertlb,state.w.xmc_findshift_base
     widget_control, state.w.xmc_findshift_base, /REALIZE
     
;  Get plotwin ids
     
     widget_control, state.w.plotwin1, GET_VALUE=x
     state.p.plotwin1_wid = x
     widget_control, state.w.plotwin2, GET_VALUE=x
     state.p.plotwin2_wid = x
     
     window, /FREE, /PIXMAP,XSIZE=state.p.plot1size[0],$
             YSIZE=state.p.plot1size[1]
     state.p.pixmap1_wid = !d.window
     
     window, /FREE, /PIXMAP,XSIZE=state.p.plot2size[0],$
             YSIZE=state.p.plot2size[1]
     state.p.pixmap2_wid = !d.window
     
;  Get sizes for things.
     
     widget_geom = widget_info(state.w.xmc_findshift_base, /GEOMETRY)
     state.p.buffer[0]=widget_geom.xsize-state.p.plot1size[0]
     state.p.buffer[1]=widget_geom.ysize-state.p.plot1size[1]-$
                       state.p.plot2size[1]
     
     xmc_findshift_setup
     
; Start the Event Loop. This will be a non-blocking program.
     
     XManager, 'xmc_findshift', $
               state.w.xmc_findshift_base, $
               EVENT_HANDLER='xmc_findshift_resize'
     
     if n_elements(PARENT) ne 0 then widget_control, parent,SENSITIVE=1
     
     cancel = state.r.cancel
     shift  = state.r.shift
     state  = 0B
     
     return, shift
     
  endif

end

