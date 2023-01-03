;+
; NAME:
;     xmc_plotprofiles
;    
; PURPOSE:
;     Plots spatial profiles. 
;
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     xmc_plotprofiles,profiles,orders,doorders,slith_arc,APPOS=appos,$
;                   APRADIUS=apradius,MASK=mask,PSFAP=psfap,BGFIT=bgfit,$
;                   CLEARAPPOS=clearappos,GROUP_LEADER=group_leader,$
;                   NOUPDATE=noupdate,CANCEL=cancel
; INPUTS:
;     profiles  - An structure with norder elements where 
;                 struct.(i) = [[arcseconds],[data]]
;     orders    - An array [norders] of order numbers
;     doorders  - An array [norders] where 1 means plot APPOS and
;                 MASK while 0 means do not
;     slith_arc - Slit length in arcseconds
;    
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     APPOS        - An array [naps,norders] of aperture positions in 
;                    arcseconds
;     APRADIUS     - An array of naps aperture radii
;     MASK         - An array [norders] of structures {x_values,mask}
;                    giving the mask.  (See mkmask_ps.pro)
;     NOUPDATE     - Set to force the XRANGE to remain the same
;     CLEARAPPOS   - The clear the aperture guesses
;     GROUP_LEADER - The widget ID of an existing widget that serves
;                    as "group leader" for the newly-created widget. 
;     CANCEL   - Set on return if there is a problem
;     
; OUTPUTS:
;     None
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     xmc_plotprofiles_state
;
; SIDE EFFECTS:
;     None
;
; RESTRICTIONS:
;     None
;
; PROCEDURE:
;     The resizable widget just plots the superprofiles.  The user can 
;     change the Y range of an order by click on the Y range button
;     and typing in the new min and max values.  The x range of all 
;     the orders can be changed by typing 'x' in the plot window,
;     clicking the left mouse button on the lower X value, and then 
;     clicking the left mouse button on the upper X value.  
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     ? - Written by M. Cushing, Institute for Astronomy, UH
;     2003-06-15 - Added Output Profiles button
;     2005-08-13 - Modified to generate to xspextool.pro an event when
;                  the apertures are selected.  
;-
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xmc_plotprofiles_initcommon


;  Build the structures which will hold the important info.
;  w - contains info pertaining to widget operations.

common xmc_plotprofiles_state, state

cleanplot,/SILENT

screenSize = Get_Screen_Size()

w = {keyboard:0L,$
     max_fld:[0L,0L],$
     message:0L,$
     min_fld:[0L,0L],$
     notify:[0L,0L],$
     mode_pdm:0L,$
     order_dl:0L,$
     plotwin:0L,$
     plotbase:0L,$
     range_base:0L,$
     xmc_plotprofiles_base:0L}

p = {buffer:[0L,0L],$
     charsize:1.5,$
     cursormode:'None',$
     doorders:ptr_new(intarr(1)),$
     bgfit:-1,$
     fix:0,$
     plotmask:0,$
     naps:0,$
     noupdate:0,$
     pixpp:250.0,$
     plotwin_wid:0L,$
     plotwinsize:[650,screenSize[1]*0.75],$
     minplotwinsize:600,$
     pixmap_wid:0L,$
     ysize:0,$
     plotaps:0,$
     plotpsf:0,$
     pscale:!p,$
     ranges:ptr_new(fltarr(2)),$
     rangeorder:0,$
     reg:[[!values.f_nan,!values.f_nan],$
          [!values.f_nan,!values.f_nan]],$
     scrollsize:[650,600],$
     slith_arc:0.,$
     xrange:[0.,0.],$
     xscale:!x,$
     yscale:!y}

d = {apradii:ptr_new(2),$
     event:{ID:0L,TOP:0L,HANDLER:0L},$
     mask:ptr_new(fltarr(1)),$
     mode:0,$
     orders:ptr_new(intarr(2)),$
     oidx:0,$
     norders:0,$
     ntotaps:20,$
     appos:ptr_new(fltarr(2)),$
     profiles:ptr_new(fltarr(2,2)),$
     psfap:0.0,$
     tmpappos:ptr_new(fltarr(2)),$
     x_arcs:ptr_new(fltarr(2))}

state = {w:w,p:p,d:d}

end
;
;******************************************************************************
;
pro xmc_plotprofiles_modwinsize

  common xmc_plotprofiles_state
  
;  Modify plot window according to the number of orders
  
  widget_control, state.w.plotbase, UPDATE=0
  widget_control, state.w.plotwin, /DESTROY
  
  if state.p.plotwinsize[1] le state.p.scrollsize[1] then begin
     
     state.w.plotwin = widget_draw(state.w.plotbase,$
                                   XSIZE=state.p.plotwinsize[0],$
                                   YSIZE=state.p.plotwinsize[1],$
                                   UVALUE='Plot Window',$
                                   /TRACKING_EVENTS,$
                                   /MOTION_EVENTS,$
                                   /BUTTON_EVENTS,$
                                   EVENT_PRO='xmc_plotprofiles_plotwin_event')
     
  endif else begin
     
     state.w.plotwin = widget_draw(state.w.plotbase,$
                                   XSIZE=state.p.plotwinsize[0],$
                                   YSIZE=state.p.plotwinsize[1],$
                                   X_SCROLL_SIZE=state.p.scrollsize[0],$
                                   Y_SCROLL_SIZE=state.p.scrollsize[1],$
                                   UVALUE='Plot Window',$
                                   /SCROLL,$
                                   /TRACKING_EVENTS,$
                                   /MOTION_EVENTS,$
                                   /BUTTON_EVENTS,$
                                   EVENT_PRO='xmc_plotprofiles_plotwin_event')
     
  endelse
  
  widget_control, state.w.plotbase, UPDATE=1
  
  geom = widget_info(state.w.xmc_plotprofiles_base,/GEOMETRY)
  
  state.p.buffer[0]=geom.xsize-state.p.scrollsize[0]
  state.p.buffer[1]=geom.ysize-state.p.scrollsize[1] 
  
  wdelete,state.p.pixmap_wid
  window, /FREE, /PIXMAP,$
          XSIZE=state.p.plotwinsize[0],YSIZE=state.p.plotwinsize[1]

  state.p.pixmap_wid = !d.window
  
end
;
;******************************************************************************
;
pro xmc_plotprofiles_cleanup,event

common xmc_plotprofiles_state

ptr_free, state.p.doorders
ptr_free, state.d.mask
ptr_free, state.d.orders
ptr_free, state.d.appos
ptr_free, state.d.profiles
ptr_free, state.d.x_arcs
ptr_free, state.d.tmpappos

state = 0B

end
;
;******************************************************************************
;
pro xmc_plotprofiles_minmax

common xmc_plotprofiles_state

widget_control, state.w.min_fld[1], SET_VALUE=strtrim($
  (*state.p.ranges)[0,state.p.rangeorder],2)

widget_control, state.w.max_fld[1], SET_VALUE=strtrim($
  (*state.p.ranges)[1,state.p.rangeorder],2)

end
;
;******************************************************************************
;
pro xmc_plotprofiles_plotupdate

  common xmc_plotprofiles_state

  wset, state.p.pixmap_wid
  erase
  polyfill,[0,0,1,1,0],[0,1,1,0,0],COLOR=20,/NORM
  xmc_plotprofiles_plotprofiles
  
  state.p.pscale = !p
  state.p.xscale = !x
  state.p.yscale = !y
  
  wset, state.p.plotwin_wid
  device, COPY=[0,0,state.p.plotwinsize[0],state.p.plotwinsize[1],0,0,$
                state.p.pixmap_wid]
  
  
end
;
;******************************************************************************
;
pro xmc_plotprofiles_plotprofiles

common xmc_plotprofiles_state

!p.multi[2] = state.d.norders
!p.multi[0] = state.d.norders

charsize = state.p.charsize
if state.d.norders gt 2 then charsize = charsize*2.0

if not state.p.fix then *state.p.ranges = fltarr(2,state.d.norders)

for i = 0, state.d.norders-1 do begin

    j = state.d.norders-1-i
    prof = (*state.d.profiles).(j)

;  Get plot range.

    if not state.p.fix then begin

        ymin = min(prof[*,1],max=ymax,/NAN)
        del  = ymax-ymin
        ymin = ymin-0.1*del
        ymax = ymax+0.1*del
        (*state.p.ranges)[*,i] = [ymin,ymax]
        
    endif

    plot,prof[*,0],prof[*,1],CHARSIZE=charsize,XRANGE=state.p.xrange,$
      XTITLE='!5Slit Position (arcsec)',/XSTY,PSYM=10,$
      TITLE='!5Order '+string((*state.d.orders)[j],FORMAT='(i2.2)'),$
      YTITLE='!5Relative Flux',/YSTY,YRANGE=(*state.p.ranges)[*,i],/NODATA

    oplot, prof[*,0],prof[*,1],color=mc_pscolor(1),PSYM=10

    if state.p.plotaps then begin

        if (*state.p.doorders)[j] then begin
            
            for k = 0, state.p.naps-1 do $
              plots,[(*state.d.appos)[k,j],$
                     (*state.d.appos)[k,j]],!y.crange,COLOR=7
            
        endif

    endif

    if state.p.plotmask then begin

        mask = (*state.d.mask).(j)
        if (*state.p.doorders)[j] then begin

;  Plot apertures

           if finite((*state.d.apradii)[0]) eq 1 then begin

              z = where(mask[*,1] le 0.0)
              tmp = prof[*,1]
              tmp[z] = !values.f_nan
              oplot,prof[*,0],tmp,PSYM=10,COLOR=3

           endif

;  Plot BG regions
                
            z = where(mask[*,1] ge 0, count)
            if count eq 0 then goto, cont
            tmp = prof[*,1]
            tmp[z] = !values.f_nan
            oplot,prof[*,0],tmp,PSYM=10,COLOR=2

            cont:

;  Plot apertures 

            if finite((*state.d.apradii)[0]) eq 1 then begin

               for k = 0, state.p.naps-1 do begin
                  
                  plots,[(*state.d.appos)[k,j],(*state.d.appos)[k,j]]- $
                        (*state.d.apradii)[k],!y.crange,LINESTYLE=1,COLOR=3
                  plots,[(*state.d.appos)[k,j],(*state.d.appos)[k,j]]+ $
                        (*state.d.apradii)[k],!y.crange,LINESTYLE=1,COLOR=3

               endfor
            
            endif

        endif

     endif

    if state.p.plotpsf then begin

        if (*state.p.doorders)[j] then begin

            for k = 0, state.p.naps-1 do begin

                plots,[(*state.d.appos)[k,j]-state.d.psfap,$
                       (*state.d.appos)[k,j]-state.d.psfap],!y.crange,COLOR=4,$
                  LINESTYLE=1
                plots,[(*state.d.appos)[k,j]+state.d.psfap,$
                       (*state.d.appos)[k,j]+state.d.psfap],!y.crange,COLOR=4,$
                  LINESTYLE=1
            
            endfor

        endif

    endif

    z = where(finite(*state.d.tmpappos) eq 1,cnt)
    if cnt gt 0 then begin

       if (*state.p.doorders)[j] then begin
          
          for k = 0,state.d.ntotaps-1 do begin
             
             plots,replicate((*state.d.tmpappos)[k,j],2),!y.crange, $
                   LINESTYLE=2,COLOR=7
             
          endfor
          
       endif
       
    endif

endfor

!p.multi=0

end
;
;******************************************************************************
;
pro xmc_plotprofiles_range

  common xmc_plotprofiles_state

;  Get fonts

  mc_getfonts,buttonfont,textfont

  if not xregistered('xmc_plotprofiles_range') then begin
     
     state.w.range_base = widget_base( $
                          GROUP_LEADER=state.w.xmc_plotprofiles_base, $
                          EVENT_PRO='xmc_plotprofiles_event',$
                          /COLUMN, $
                          TITLE='Plot Range')

     value = 'Order '+strcompress( reverse((*state.d.orders)), /re)
     state.w.order_dl = widget_droplist(state.w.range_base,$
                                        FONT=buttonfont,$
                                        VALUE=value,$
                                        /DYNAMIC_RESIZE,$
                                        UVALUE='Order')  
     
     min = coyote_field2(state.w.range_base,$
                         LABELFONT=buttonfont,$
                         FIELDFONT=textfont,$
                         TITLE='Y Min:',$
                         UVALUE='Y Min',$
                         XSIZE=10,$
                         /CR_ONLY,$
                         EVENT_PRO='xmc_plotprofiles_event',$
                         textid=textid)
     state.w.min_fld = [min,textid]
     
     max = coyote_field2(state.w.range_base,$
                         LABELFONT=buttonfont,$
                         FIELDFONT=textfont,$
                         TITLE='Y Max:',$
                         UVALUE='Y Max',$
                         XSIZE=10,$
                         /CR_ONLY,$
                         EVENT_PRO='xmc_plotprofiles_event',$
                         textid=textid)
     state.w.max_fld = [max,textid]
     
     kill = widget_button(state.w.range_base,$
                          VALUE='Quit',$
                          UVALUE='Quit Range',$
                          FONT=buttonfont)
     
     widget_control, state.w.range_base, /REALIZE
     
; Start the Event Loop. This will be a non-blocking program.
     
     XManager, 'xmc_plotprofiles_range', $
               state.w.range_base, $
               /NO_BLOCK,$
               EVENT_HANDLER='xmc_plotprofiles_event'
     
     xmc_plotprofiles_minmax
     state.p.fix = 1
     
  endif else begin
     
     value = 'Order '+string( reverse((*state.d.orders)),FORMAT='(i2.2)')
     widget_control, state.w.order_dl, SET_VALUE=value
     state.p.rangeorder = 0
     xmc_plotprofiles_minmax
     
  endelse

end
;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro xmc_plotprofiles_event,event

  common xmc_plotprofiles_state
  
  widget_control, event.id,  GET_UVALUE = uvalue
  widget_control, /HOURGLASS
  
  case uvalue of 
     
     'Ap Find Mode': begin
        
        widget_control, state.w.mode_pdm+2,SET_VALUE='[  ] All Orders'
        widget_control, state.w.mode_pdm+3,SET_VALUE='[  ] Order by Order'
        
        case event.value of 
           
           1: begin
              
              state.d.mode = 0
              widget_control, state.w.mode_pdm+2,SET_VALUE='[X] All Orders'
              
           end
           
           2: begin
              
              state.d.mode = 1
              widget_control, state.w.mode_pdm+3,SET_VALUE='[X] Order by Order'
                            
           end
           
      endcase
        
     end
     
     'Order': begin
        
        state.p.rangeorder = event.index
        xmc_plotprofiles_minmax
        
     end
     
     'Keyboard': begin
        
        case strtrim(event.ch,2) of 
           
           'c': begin   
              
              state.p.cursormode = 'None'
              state.p.reg= !values.f_nan                
              xmc_plotprofiles_plotupdate                
              
           end
           
           's': begin
              
              state.p.cursormode='Select'
              if state.d.mode then (*state.d.tmpappos)[*,state.d.oidx] = $
                 !values.f_nan
              if not state.d.mode then (*state.d.tmpappos)[*] = !values.f_nan
              widget_control, state.d.event.ID, SEND_EVENT=state.d.event
              state.p.plotaps  = 0
              state.p.plotmask = 0
              state.p.plotpsf  = 0
              
              xmc_plotprofiles_plotupdate
              
           end
           
           'w': begin
              
              state.p.xrange = [0,state.p.slith_arc]
              xmc_plotprofiles_plotupdate
              
           end
           
           'x': begin 
              
              state.p.cursormode = 'XZoom'
              state.p.reg= !values.f_nan
              
           end
           
           else:
           
        endcase
        
     end
     
     'Quit': begin
        
        widget_control, event.top, /DESTROY
        !p.multi=0
        
     end
     
     'Quit Range': widget_control, event.top, /DESTROY
     
     'Y Min': begin
        
        min = mc_cfld(state.w.min_fld,4,CANCEL=cancel)
        if cancel then break
        (*state.p.ranges)[0,state.p.rangeorder] = min
        xmc_plotprofiles_plotupdate
        mc_setfocus,state.w.max_fld
        
     end
     
     'Y Max': begin
        
        max = mc_cfld(state.w.max_fld,4,CANCEL=cancel)
        if cancel then break
        (*state.p.ranges)[1,state.p.rangeorder] = max
        xmc_plotprofiles_plotupdate
        mc_setfocus,state.w.min_fld
        
     end
     
     'Y Range': xmc_plotprofiles_range
     
  endcase
  
end
;
;******************************************************************************
;
pro xmc_plotprofiles_plotwin_event,event

  common xmc_plotprofiles_state
  
  widget_control, event.id,  GET_UVALUE = uvalue
  
;  Check to see if it is a TRACKING event.
  
  if strtrim(tag_names(event,/STRUCTURE_NAME),2) eq 'WIDGET_TRACKING' then begin
     
     if event.enter eq 0 then widget_control, state.w.keyboard, SENSITIVE=0
     wset, state.p.plotwin_wid
     device, COPY=[0,0,state.p.plotwinsize[0],state.p.plotwinsize[1],0,0,$
                   state.p.pixmap_wid]

  label = ' Cursor X : '
  widget_control,state.w.message,SET_VALUE=label
     
     goto, cont
     
  endif
  
;  If not, set the keyboard focus and active window.
  
  widget_control, state.w.keyboard, /INPUT_FOCUS, /SENSITIVE
  wset, state.p.plotwin_wid
  
  !p = state.p.pscale
  !x = state.p.xscale
  !y = state.p.yscale
  x  = event.x/float(state.p.scrollsize[0])
  y  = event.y/float(state.p.scrollsize[1])
  xy = convert_coord(x,y,/NORMAL,/TO_DATA)
  
;  Find which order the cursor is over
  
  state.d.oidx = floor(event.y/float(state.p.plotwinsize[1])*state.d.norders)
  
  if event.type eq 1 then begin
     
     case state.p.cursormode of 
        
        'XZoom': begin
           
           z = where(finite(state.p.reg) eq 1,count)
           if count eq 0 then begin
              
              wset, state.p.pixmap_wid
              state.p.reg[*,0] = xy[0:1]
              plots, [event.x,event.x],$
                     [0,state.p.plotwinsize[1]],COLOR=2,/DEVICE, $
                     LINESTYLE=2
              
              wset, state.p.plotwin_wid
              device, COPY=[0,0,state.p.plotwinsize[0], $
                            state.p.plotwinsize[1],0,0,$
                            state.p.pixmap_wid]
              
           endif else begin 
              
              state.p.reg[*,1] = xy[0:1]
              state.p.xrange = [min(state.p.reg[0,*],MAX=max),max]
              xmc_plotprofiles_plotupdate
              state.p.cursormode   = 'None'
              
           endelse
           
        end
        
        'Select': begin
           
           if state.d.mode then begin
              
              z = where(finite((*state.d.tmpappos)[*,state.d.oidx]) eq 0,cnt)
              if cnt ge 1 then (*state.d.tmpappos)[z[0],state.d.oidx] = xy[0]
              
           endif else begin
              
              z = where(finite((*state.d.tmpappos)[*,state.d.oidx]) eq 0,cnt)
              if cnt ge 1 then (*state.d.tmpappos)[z[0],*] = xy[0]
              
           endelse
           xmc_plotprofiles_plotupdate
           
        end
        
        else:
        
     endcase
     
  endif
  
;  Update cursor position.
  
  label = ' Cursor X : '+strtrim(xy[0],2)+' arcsec'
  widget_control,state.w.message,SET_VALUE=label
  
;  Copy the pixmaps and draw the lines.
  
  wset, state.p.plotwin_wid
  device, COPY=[0,0,state.p.plotwinsize[0],state.p.plotwinsize[1],0,0,$
                state.p.pixmap_wid]
  
  
  if not state.d.mode or state.p.cursormode eq 'None' then begin
     
     plots, [event.x,event.x],[0,state.p.plotwinsize[1]],COLOR=2,/DEVICE
     
  endif else begin
     
     low = state.d.oidx*(state.p.plotwinsize[1]/state.d.norders)
     plots,[event.x,event.x],[0,low],COLOR=2,/DEVICE,LINESTYLE=2
     plots,[event.x,event.x],[low,low+state.p.plotwinsize[1]/state.d.norders], $
           COLOR=2,/DEVICE
     plots,[event.x,event.x],[low+state.p.plotwinsize[1]/state.d.norders,$
                              state.p.plotwinsize[1]],COLOR=2,/DEVICE, $
           LINESTYLE=2
     
  endelse
  
cont:

end
;
;******************************************************************************
;
pro xmc_plotprofiles_resize, event

  common xmc_plotprofiles_state
  
  widget_control, state.w.xmc_plotprofiles_base, TLB_GET_SIZE = size
  
  state.p.plotwinsize[0] = size[0]-state.p.buffer[0]
  state.p.scrollsize[0]  = state.p.plotwinsize[0]
  
  state.p.scrollsize[1]  = size[1]-state.p.buffer[1]
  state.p.plotwinsize[1] = state.p.scrollsize[1] > $
                           state.p.pixpp*state.d.norders
  
  xmc_plotprofiles_modwinsize
  xmc_plotprofiles_plotupdate
  
end
;
;******************************************************************************
;
; ------------------------------Main Program-------------------------------- 
;
;******************************************************************************
;
pro xmc_plotprofiles,profiles,orders,doorders,slith_arc,APPOS=appos,$
                     APRADII=apradii,MASK=mask,PSFAP=psfap,BGFIT=bgfit, $
                     CLEARAPPOS=clearappos,GROUP_LEADER=group_leader, $
                     NOTIFY=notify,GETAPPOS=getappos,NOUPDATE=noupdate, $
                     POSITION=position,CANCEL=cancel
  
  cancel = 0

  common xmc_plotprofiles_state
  
  if arg_present(GETAPPOS) ne 0 then begin
     
     getappos = *state.d.tmpappos
     return
     
  endif 
  
;  Check parameters
  
  if n_params() lt 4 then begin
     
     print, 'Syntax - xmc_plotprofiles,profiles,orders,doorders,slith_arc,$'
     print, '                       APPOS=appos,APRADII=apradii,MASK=mask,$'
     print, '                       PSFAP=psfap,BGFIT=bgfit,$'
     print, '                       CLEARAPPOS=clearappos,NOTIFY=notify,$'
     print, '                       GROUP_LEADER=group_leader,$'
     print, '                       NOUPDATE=noupdate,CANCEL=cancel '
     cancel = 1
     return
     
  endif
  cancel = mc_cpar('xmc_plotprofiles',profiles,1,'Profiles',8,[0,1])
  if cancel then return
  cancel = mc_cpar('xmc_plotprofiles',orders,2,'Orders',[2,3,4,5],[0,1])
  if cancel then return
  cancel = mc_cpar('xmc_plotprofiles',doorders,3,'Doorders',[2,3,4,5],[0,1])
  if cancel then return
  cancel = mc_cpar('xmc_plotprofiles',slith_arc,4,'Slith_arc',[2,3,4,5],0)
  if cancel then return
  
  if not xregistered('xmc_plotprofiles') then xmc_plotprofiles_initcommon
  
;  load user info
  
  state.d.norders   = n_elements(orders)
  *state.d.orders   = orders
  *state.p.doorders = doorders
  *state.d.profiles = profiles
  if not keyword_SET(NOUPDATE) then state.p.xrange = [0,slith_arc]
  state.p.slith_arc = slith_arc
  *state.d.tmpappos = fltarr(state.d.ntotaps,state.d.norders)+!values.f_nan
  
  state.p.plotaps  = 0
  state.p.plotmask = 0
  state.p.plotpsf  = 0
  
  if n_elements(NOTIFY) ne 0 then begin
     
     state.d.event.ID  = notify[0]
     state.d.event.TOP = notify[1]
     
  endif
  
  if n_elements(APPOS) ne 0 then begin
     
     state.p.naps      = (size(APPOS))[1]
     state.p.plotaps   = 1
     *state.d.appos    = appos
     
  endif 
  
  if n_elements(APRADII) ne 0 then *state.d.apradii = apradii else $
     *state.d.apradii = !values.f_nan
  
  if n_elements(MASK) ne 0 then begin
     
     state.p.plotmask = 1
     *state.d.mask = mask
     
  endif
  
  if n_elements(PSFAP) ne 0 then begin
     
     state.d.psfap = psfap
     state.p.plotpsf = 1
     
  endif
  state.p.cursormode = 'None'
  
  state.p.bgfit = (n_elements(BGFIT) ne 0) ? bgfit:-1
  
  
  if not xregistered('xmc_plotprofiles') then begin
     
     mc_getfonts,buttonfont,textfont
     
;  Get size of the display

     screenSize = Get_Screen_Size()

     state.p.plotwinsize[1] = state.p.minplotwinsize > $
                              state.p.pixpp*state.d.norders
     
     state.p.scrollsize[1] = (state.p.plotwinsize[1] gt screensize[1]) ? $
                             screensize[1]*0.75:state.p.plotwinsize[1]

;  Make the widget
     
     state.w.xmc_plotprofiles_base = widget_base(TITLE='Plot Spatial Profiles',$
                                                 /COLUMN,$
                                                 GROUP_LEADER=group_leader,$
                                                 /TLB_SIZE_EVENTS)
     
     row = widget_base(state.w.xmc_plotprofiles_base,$
                       /BASE_ALIGN_CENTER,$
                       EVENT_PRO='xmc_plotprofiles_event',$
                       FRAME=3,$
                       /ROW)
     
        desc = ['1\Aperture Mode','0\[X] All Orders','2\[  ] Order by Order']
        
        state.w.mode_pdm = cw_pdmenu(row,$
                                     FONT=buttonfont,$
                                     desc,$
                                     /RETURN_INDEX,$
                                     UVALUE='Ap Find Mode')
        
        button = widget_button(row,$
                               FONT=buttonfont,$
                               VALUE='Y Range',$
                               UVALUE='Y Range')         
        
        button = widget_button(row,$
                               FONT=buttonfont,$
                               VALUE='Done',$
                               UVALUE='Quit')
        
        state.w.message = widget_label(row,$
                                       VALUE=' Cursor X :',$
                                       FONT=buttonfont,$
                                      /DYNAMIC_RESIZE)
        
        state.w.keyboard = widget_text(state.w.xmc_plotprofiles_base, $
                                       /ALL_EVENTS,$
                                       SCR_XSIZE=1,$
                                       SCR_YSIZE=1,$
                                       UVALUE='Keyboard',$
                                       EVENT_PRO='xmc_plotprofiles_event',$
                                       VALUE='')
        
;  Get window size
   
     state.w.plotbase = widget_base(state.w.xmc_plotprofiles_base)

     if state.p.plotwinsize[1] le state.p.scrollsize[1] then begin
        
        state.w.plotwin = widget_draw(state.w.plotbase,$
                                      XSIZE=state.p.plotwinsize[0],$
                                      YSIZE=state.p.plotwinsize[1],$
                                      UVALUE='Plot Window',$
                                      /TRACKING_EVENTS,$
                                      /MOTION_EVENTS,$
                                      /BUTTON_EVENTS,$
                                   EVENT_PRO='xmc_plotprofiles_plotwin_event')
        
     endif else begin
        
        state.w.plotwin = widget_draw(state.w.plotbase,$
                                      XSIZE=state.p.plotwinsize[0],$
                                      YSIZE=state.p.plotwinsize[1],$
                                      X_SCROLL_SIZE=state.p.scrollsize[0],$
                                      Y_SCROLL_SIZE=state.p.scrollsize[1],$
                                      UVALUE='Plot Window',$
                                      /SCROLL,$
                                      /TRACKING_EVENTS,$
                                      /MOTION_EVENTS,$
                                      /BUTTON_EVENTS,$
                                   EVENT_PRO='xmc_plotprofiles_plotwin_event')
     
     endelse

     if n_elements(POSITION) eq 0 then position = [0.5,0.5]
     cgcentertlb,state.w.xmc_plotprofiles_base,position[0],position[1]
     
     widget_control, state.w.xmc_plotprofiles_base, /REALIZE
   
;  Get plotwin ids

   widget_control, state.w.plotwin, GET_VALUE=x
   state.p.plotwin_wid = x

   window, /FREE, /PIXMAP,XSIZE=state.p.plotwinsize[0],$
     YSIZE=state.p.plotwinsize[1]
   state.p.pixmap_wid = !d.window
   
; Start the Event Loop. This will be a non-blocking program.
   
   XManager, 'xmc_plotprofiles', $
     state.w.xmc_plotprofiles_base, $
     /NO_BLOCK,$
     EVENT_HANDLER='xmc_plotprofiles_resize',$
     CLEANUP='xmc_plotprofiles_cleanup'
   
   geom = widget_info(state.w.xmc_plotprofiles_base, /geometry)
   
   state.p.buffer[0] = geom.xsize-state.p.scrollsize[0]
   state.p.buffer[1] = geom.ysize-state.p.scrollsize[1]
   
endif else begin

;  Check to make sure the window is big enough

  state.p.plotwinsize[1] = state.p.plotwinsize[1] > $
                           state.p.pixpp*state.d.norders


   xmc_plotprofiles_modwinsize

endelse

xmc_plotprofiles_plotupdate


end
