;+
; NAME:
;     xzoomplot
;
; PURPOSE:
;     General purpose plotting widget.
;
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     xzoomplot,x,y,PSYM=psym,XRANGE=xrange,YRANGE=yrange,XLOG=xlog,$
;               YLOG=ylog,XTITLE=ytitle,YTITLE=xtitle,TITLE=title,$
;               POSITION=position,CANCEL=cancel      
; INPUTS:
;     x - The independent array
;     y - The dependent array
;    
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     PSYM     - Standard IDL PSYM keyword.
;     XRANGE   - The initial xrange of the plot
;     YRANGE   - The initial yrange of the plot
;     XLOG     - Set to plot the xaxis logarithmically 
;     YLOG     - Set to plot the xaxis logarithmically 
;     XTITLE   - The xtitle
;     YTITLE   - The ytitle
;     TITLE    - The title
;     POSITION - The normalized coordinates of the location of the
;                widget.  [0,0] = top left, [1,1] = bottom right.
;     CANCEL   - Set on return if there is a problem
;     
; OUTPUTS:
;     None
;     
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     xzoomplot_state
;
; SIDE EFFECTS:
;     None
;
; RESTRICTIONS:
;     None
;
; PROCEDURE:
;     Type 'h' for the help file
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2002       - Written by M. Cushing, Institute for Astronomy, UH
;     2003-06-11 - Added XRANGE and YRANGE keywords
;     2003-06-11 - Added 'a' cursor mode
;     2004-02-04 - Added XLOG and YLOG keywords
;     2008-06-10 - Added lines and labels inputs.
;     2009-04-21 - Added the WPOS keyword.
;     2014-06-19 - Changed WPOS to POSITION.
;     2014-08-05 - Added PSM keyword.
;-
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xzoomplot_startup,position
  
  common xzoomplot_state, state

  cleanplot,/SILENT

;  Load the fonts

  mc_getfonts,buttonfont,textfont
  
;  Build three structures which will hold important info.
  
  w = {charsize_fld:[0L,0L],$
;       keyboard:0L,$
       message:0L,$
       plotwin:0,$
       speccolor_dl:0,$
       thick_fld:[0L,0L],$
       title_fld:[0L,0L],$
       xtitle_fld:[0L,0L],$
       xzoomplot_base:0L,$
       xmin_fld:[0L,0L],$
       xmax_fld:[0L,0L],$
       ytitle_fld:[0L,0L],$
       ylog_bg:0L,$
       ymin_fld:[0L,0L],$
       ymax_fld:[0L,0L]}
  
  r = {fix:0,$
       remove:0,$
       cursormode:'None'}
  
  d = {oflux:ptr_new(fltarr(2)),$
       owave:ptr_new(fltarr(2))}
  
  p = {buffer:[0.,0.],$
       charsize:1.5,$
       color:3,$
       cursor:0,$
;       lines:ptr_new(2),$
;       labels:ptr_new(2),$
       pixmap_wid:0L,$
       plotwin_wid:0L,$
       plotabsxrange:[0D,0D],$
       plotabsyrange:[0D,0D],$
       plotxrange:[0D,0D],$
       plotyrange:[0D,0D],$
       plotsize:[720,450],$
       pscale:!p,$
       psym:10,$
       xaxis:0,$
       xlog:0,$
       xscale:!x,$
       xtitle:'',$
       thick:1,$
       time:!values.f_nan,$
       title:'',$
       yaxis:0,$
       ylog:0,$
       yscale:!y,$
       ytitle:'',$
       reg:[[!values.d_nan,!values.d_nan],$
            [!values.d_nan,!values.d_nan]],$
       zeroline:1L}
  
;  Load the three structures in the state structure.
  
  state = {w:w,r:r,d:d,p:p}
  
;  Build the widget.
  
  state.w.xzoomplot_base = widget_base(TITLE='Xzoomplot', $
                                       /COLUMN,$
                                       /TLB_SIZE_EVENTS)
  
     button = widget_button(state.w.xzoomplot_base,$
                            FONT=buttonfont,$
                            EVENT_PRO='xzoomplot_event',$
                            VALUE='Done',$
                            UVALUE='Done')
  
     state.w.message = widget_text(state.w.xzoomplot_base, $
                                   YSIZE=1)
     
     col_base = widget_base(state.w.xzoomplot_base,$
                            /COLUMN)
   
        state.w.plotwin = widget_draw(col_base,$
                                      XSIZE=state.p.plotsize[0],$
                                      YSIZE=state.p.plotsize[1],$
                                      /TRACKING_EVENTS,$
                                      /BUTTON_EVENTS,$
                                      /MOTION_EVENTS,$
                                      /KEYBOARD_EVENTS,$
                                      EVENT_PRO='xzoomplot_plotwin_event',$
                                      UVALUE='Plot Window 1')
        
        row_base = widget_base(col_base,$
                               FRAME=2,$
                               /ROW)
   
           fld = coyote_field2(row_base,$
                               LABELFONT=buttonfont,$
                               FIELDFONT=textfont,$
                               TITLE='X Min:',$
                               UVALUE='X Min',$
                               XSIZE=12,$
                               EVENT_PRO='xzoomplot_minmax_event',$
                               /CR_ONLY,$
                               TEXTID=textid)
           state.w.xmin_fld = [fld,textid]
      
           fld = coyote_field2(row_base,$
                               LABELFONT=buttonfont,$
                               FIELDFONT=textfont,$
                               TITLE='X Max:',$
                               UVALUE='X Max',$
                               XSIZE=12,$
                               EVENT_PRO='xzoomplot_minmax_event',$
                               /CR_ONLY,$
                               TEXTID=textid)
           state.w.xmax_fld = [fld,textid]
      
           fld = coyote_field2(row_base,$
                               LABELFONT=buttonfont,$
                               FIELDFONT=textfont,$
                               TITLE='Y Min:',$
                               UVALUE='Y Min',$
                               XSIZE=12,$
                               EVENT_PRO='xzoomplot_minmax_event',$
                               /CR_ONLY,$
                               TEXTID=textid)
           state.w.ymin_fld = [fld,textid]
    
           fld = coyote_field2(row_base,$
                               LABELFONT=buttonfont,$
                               FIELDFONT=textfont,$
                               TITLE='Y Max:',$
                               UVALUE='Y Max',$
                               XSIZE=12,$
                               EVENT_PRO='xzoomplot_minmax_event',$
                               /CR_ONLY,$
                               TEXTID=textid)
           state.w.ymax_fld = [fld,textid]

; Get things running.  Center the widget using the Fanning routine.
           
  cgcentertlb,state.w.xzoomplot_base,position[0],position[1]
  widget_control, state.w.xzoomplot_base, /REALIZE
           
;  Get plotwin ids
           
  widget_control, state.w.plotwin, GET_VALUE=x
  state.p.plotwin_wid = x
  
  window, /FREE, /PIXMAP,XSIZE=state.p.plotsize[0],YSIZE=state.p.plotsize[1]
  state.p.pixmap_wid = !d.window
  
;  Get sizes for things.
           
  widget_geom = widget_info(state.w.xzoomplot_base, /GEOMETRY)
  
  state.p.buffer[0]=widget_geom.xsize-state.p.plotsize[0]
  state.p.buffer[1]=widget_geom.ysize-state.p.plotsize[1]
  
; Start the Event Loop. This will be a non-blocking program.
  
  XManager, 'xzoomplot', $
            state.w.xzoomplot_base, $
            EVENT_HANDLER='xzoomplot_resize_event',$
            /NO_BLOCK
  
end
;
;******************************************************************************
;
pro xzoomplot_cleanup,xzoomplot_base

common xzoomplot_state

widget_control, xzoomplot_base, GET_UVALUE=state, /NO_COPY
if n_elements(state) ne 0 then begin

    ptr_free, state.d.oflux
    ptr_free, state.d.owave
;    ptr_free, state.p.lines
;    ptr_free, state.p.labels
        
endif
state = 0B

end
;
;******************************************************************************
;
pro xzoomplot_cp

common xzoomplot_state

if not xregistered('xzoomplot_cp') then begin

    mc_getfonts,buttonfont,textfont

    cp_base = widget_base(GROUP_LEADER=state.w.xzoomplot_base, $
                          /COLUMN, $
                          TITLE='Control Panel')
        
        bg = cw_bgroup(cp_base,$
                       FONT=buttonfont,$
                       ['Xlog','Ylog'],$
                       /ROW,$
                       /RETURN_NAME,$
                       /NONEXCLUSIVE,$
                       LABEL_LEFT='Axis Type:',$
                       UVALUE='Axis Type',$
                       SET_VALUE=[state.p.xlog,state.p.ylog])

        bg = cw_bgroup(cp_base,$
                       FONT=buttonfont,$
                       ['Zero line'],$
                       /ROW,$
                       /RETURN_NAME,$
                       /NONEXCLUSIVE,$
                       UVALUE='Zero Line',$
                       SET_VALUE=[state.p.zeroline])

        values = ['White','Red','Green','Blue','Yellow','Magenta',$
                  'Cyan']
        state.w.speccolor_dl = widget_droplist(cp_base,$ 
                                               FONT=buttonfont,$
                                               TITLE='Color: ',$
                                               VALUE=values,$
                                               UVALUE='Spectrum Color')
        widget_control, state.w.speccolor_dl,SET_DROPLIST_SELECT=2

        box = widget_base(cp_base,$
                          /COLUMN,$
                          /BASE_ALIGN_RIGHT)

           thick = coyote_field2(box,$
                                 LABELFONT=buttonfont,$
                                 FIELDFONT=textfont,$
                                 TITLE='Thick:',$
                                 UVALUE='Thick',$
                                 XSIZE=20,$
                                 VALUE=state.p.thick,$
                                 EVENT_PRO='xzoomplot_event',$
                                 /CR_ONLY,$
                                 TEXTID=textid)
           state.w.thick_fld = [thick,textid] 
           
           thick = coyote_field2(box,$
                                 LABELFONT=buttonfont,$
                                 FIELDFONT=textfont,$
                                 TITLE='CharSize:',$
                                 UVALUE='CharSize',$
                                 XSIZE=20,$
                                 VALUE=state.p.charsize,$
                                 EVENT_PRO='xzoomplot_event',$
                                 /CR_ONLY,$
                                 TEXTID=textid)
           state.w.charsize_fld = [thick,textid] 
           
           xlabel = coyote_field2(box,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='X Title:',$
                                  UVALUE='X Title',$
                                  XSIZE=20,$
                                  VALUE=state.p.xtitle,$
                                  EVENT_PRO='xzoomplot_event',$
                                  /CR_ONLY,$
                                  TEXTID=textid)
           state.w.xtitle_fld = [xlabel,textid]      
           
           ylabel = coyote_field2(box,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='Y Title:',$
                                  UVALUE='Y Title',$
                                  XSIZE=20,$
                                  VALUE=state.p.ytitle,$
                                  EVENT_PRO='xzoomplot_event',$
                                  /CR_ONLY,$
                                  TEXTID=textid)
           state.w.ytitle_fld = [ylabel,textid] 
           
           title = coyote_field2(box,$
                                 LABELFONT=buttonfont,$
                                 FIELDFONT=textfont,$
                                 TITLE='Title:',$
                                 UVALUE='Title',$
                                 XSIZE=20,$
                                 VALUE=state.p.title,$
                                 EVENT_PRO='xzoomplot_event',$
                                 /CR_ONLY,$
                                 TEXTID=textid)
           state.w.title_fld = [title,textid]      
           
        quit = widget_button(cp_base,$
                             VALUE='Done',$
                             FONT=buttonfont,$
                             UVALUE='Done')
         
   cgcentertlb,cp_base
   widget_control, cp_base, /REALIZE
   
; Start the Event Loop. This will be a non-blocking program.
   
   XManager, 'xcompspec_cp', $
     cp_base, $
     /NO_BLOCK,$
     EVENT_HANDLER='xzoomplot_event'
   
endif

end
;
;******************************************************************************
;
pro xzoomplot_help

common xzoomplot_state

h = [['Xzoomplot is a fully resizing widget.'],$
     [' '],$
     ['Keyboard commands:'],$
     [' '],$
     ["a - Sets the 'a'bsolute range to the current x and y range"],$
     [' '],$
     ['c - Clear mouse mode.'],$
     ['    Use to clear a zoom, fix, or remove session.'],$
     [' '],$
     ['i - To zoom IN in whatever zoom mode the cursor is currently'],$
     ['    in.'],$
     [' '],$
     ['h - To lauch the help window.'],$
     [' '],$
     ['m - To open the control panel.  The plot parameters can then '],$
     ['    be Modified.'],$
     [' '],$
     ['o - To zoom OUT in whatever zoom mode the cursor is currently'],$
     ['    in.'],$
     [' '],$
     ['q - To quit..'],$
     [' '],$
     ['w - To plot the entire spectrum'],$
     [' '],$
     ['x - Enters x zoom mode'],$
     ['    Press left mouse button at lower x value and then at upper'],$
     ['    x value.'],$
     ['y - Enters y zoom mode'],$
     ['    Press left mouse button at lower y value and then at upper'],$
     ['    y value.'],$
     ['z - Enters zoom mode'],$
     ['    Press the left mouse button in one corner of the zoom box '],$
     ['    and then move the cursor to the other corner and press the '],$
     ['    the left mouse button.'],$
     [' ']]

xmc_displaytext,h,TITLE='Xzoomplot Help File', $
                GROUP_LEADER=state.w.xzoomplot_base

end
;
;******************************************************************************
;
pro xzoomplot_plotspec

  common xzoomplot_state

  !p.multi = 0
  
  color=state.p.color
  wset, state.p.pixmap_wid

  polyfill,[0,0,1,1,0],[0,1,1,0,0],COLOR=20,/NORM

  plot,*state.d.owave,*state.d.oflux,/XSTY,/YSTY,YRANGE=state.p.plotyrange,$
       XRANGE=state.p.plotxrange,/NODATA,CHARTHICK=state.p.thick,$
       THICK=state.p.thick,PSYM=state.p.psym,XTITLE=state.p.xtitle, $
       YTITLE=state.p.ytitle,TITLE=state.p.title, $
       CHARSIZE=state.p.charsize, $
       XTHICK=state.p.thick,YTHICK=state.p.thick,XLOG=state.p.xlog, $
       YLOG=state.p.ylog,/NOERASE
  
  oplot, *state.d.owave,*state.d.oflux,COLOR=color,THICK=state.p.thick, $
         PSYM=state.p.psym
  
  if state.p.plotyrange[0] lt 0 and $
     state.p.plotyrange[1] gt 0 and $
     ~state.p.ylog and $
     state.p.zeroline then $
        plots,!x.crange,[0,0],LINESTYLE=1
  
  
  wset, state.p.plotwin_wid
  device, copy=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,0,$
                state.p.pixmap_wid]
  
  
  state.p.xscale = !x
  state.p.yscale = !y
  state.p.pscale = !p
  state.p.cursor = 1
  
cont:

end
;
;******************************************************************************
;
pro xzoomplot_setminmax

common xzoomplot_state

widget_control, state.w.xmin_fld[1],SET_VALUE=strtrim(state.p.plotxrange[0],2)
widget_control, state.w.xmax_fld[1],SET_VALUE=strtrim(state.p.plotxrange[1],2)
widget_control, state.w.ymin_fld[1],SET_VALUE=strtrim(state.p.plotyrange[0],2)
widget_control, state.w.ymax_fld[1],SET_VALUE=strtrim(state.p.plotyrange[1],2)

end
;
;******************************************************************************
;
pro xzoomplot_writedata

common xzoomplot_state

filename = dialog_pickfile(DIALOG_PARENT=state.w.xzoomplot_base,$
                           FILTER='*.dat',/WRITE,$
                           FILE='data.dat')

if filename ne '' then  begin

    z = where(*state.d.owave ge state.p.plotxrange[0] and $
              *state.d.owave le state.p.plotxrange[1],cnt)


    openw, lun, filename,/GET_LUN

    for i = 0,cnt-1 do printf, lun, $
      (*state.d.owave)[z[i]],(*state.d.oflux)[z[i]]

    free_lun, lun

endif

end
;
;******************************************************************************
;
pro xzoomplot_zoom,IN=in,OUT=out

common xzoomplot_state

delabsx = state.p.plotabsxrange[1]-state.p.plotabsxrange[0]
delx    = state.p.plotxrange[1]-state.p.plotxrange[0]

delabsy = state.p.plotabsyrange[1]-state.p.plotabsyrange[0]
dely    = state.p.plotyrange[1]-state.p.plotyrange[0]

xcen = state.p.plotxrange[0]+delx/2.
ycen = state.p.plotyrange[0]+dely/2.

case state.r.cursormode of 

    'XZoom': begin

        z = alog10(delabsx/delx)/alog10(2)
        if keyword_set(IN) then z = z+1 else z=z-1
        hwin = delabsx/2.^z/2.
        state.p.plotxrange = [xcen-hwin,xcen+hwin]
        xzoomplot_plotspec

    end

    'YZoom': begin

        z = alog10(delabsy/dely)/alog10(2)
        if keyword_set(IN) then z = z+1 else z=z-1
        hwin = delabsy/2.^z/2.
        state.p.plotyrange = [ycen-hwin,ycen+hwin]
        xzoomplot_plotspec

    end

    'Zoom': begin

        z = alog10(delabsx/delx)/alog10(2)
        if keyword_set(IN) then z = z+1 else z=z-1
        hwin = delabsx/2.^z/2.
        state.p.plotxrange = [xcen-hwin,xcen+hwin]

        z = alog10(delabsy/dely)/alog10(2)
        if keyword_set(IN) then z = z+1 else z=z-1
        hwin = delabsy/2.^z/2.
        state.p.plotyrange = [ycen-hwin,ycen+hwin]

        xzoomplot_plotspec

    end

    else:

endcase
xzoomplot_setminmax

end
;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro xzoomplot_event, event

common xzoomplot_state

widget_control, event.id,  GET_UVALUE = uvalue

case uvalue of

    'Axis Type': begin

        if event.value eq 'Xlog' then state.p.xlog = event.select
        if event.value eq 'Ylog' then state.p.ylog = event.select
        xzoomplot_plotspec

     end
    
    'CharSize': begin

        val = mc_cfld(state.w.charsize_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then return
        state.p.charsize=val
        xzoomplot_plotspec

    end

    'Keyboard': begin

    end

    'Done': widget_control, event.top, /DESTROY

    'Spectrum Color': begin

        state.p.color = event.index+1
        xzoomplot_plotspec

    end

   'Thick': begin

        label = mc_cfld(state.w.thick_fld,7,/EMPTY,CANCEL=cancel)
        if cancel then return
        state.p.thick = label
        xzoomplot_plotspec

    end

    'Title': begin

        label = mc_cfld(state.w.title_fld,7,/EMPTY,CANCEL=cancel)
        if cancel then return
        state.p.title = label
        xzoomplot_plotspec

    end

    'X Title': begin

        label = mc_cfld(state.w.xtitle_fld,7,/EMPTY,CANCEL=cancel)
        if cancel then return
        state.p.xtitle = label
        xzoomplot_plotspec

    end

    'Y Title': begin

        label = mc_cfld(state.w.ytitle_fld,7,/EMPTY,CANCEL=cancel)
        if cancel then return
        state.p.ytitle = label
        xzoomplot_plotspec

     end

    'Zero Line': begin
       
       state.p.zeroline=event.select
       xzoomplot_plotspec

    end

endcase

cont: 

end
;
;******************************************************************************
;
pro xzoomplot_plotwin_event, event

  common xzoomplot_state
  
  widget_control, event.id,  GET_UVALUE = uvalue

;  Check to see if it is a TRACKING event.

  if strtrim(tag_names(event,/STRUCTURE_NAME),2) eq 'WIDGET_TRACKING' then begin
     
     widget_control, state.w.plotwin, INPUT_FOCUS=event.enter
     wset, state.p.plotwin_wid
     device, COPY=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,0,$
                   state.p.pixmap_wid]
     return
     
     
  endif

  if strtrim(event.ch,2) ne '' then begin
  
  ;  Catch for Clemens' bug.

     if (event.type eq 2) then return
     if (event.press eq 1) then return

     case strtrim(event.ch,2) of 
        
        '?': xzoomplot_help
        
        'a': begin
           
           state.p.plotabsxrange = state.p.plotxrange
           state.p.plotabsyrange=state.p.plotyrange
           
        end
        
        'c': begin          
           
           state.r.cursormode = 'None'
           state.p.reg = !values.f_nan                
           xzoomplot_plotspec
           
        end
        
        'i': xzoomplot_zoom,/IN
        
        'h': xzoomplot_help   
        
        'm': xzoomplot_cp
        
        'o': xzoomplot_zoom,/OUT
        
        'q': begin

           widget_control, event.top, /DESTROY
           return

        end
        'r': xzoomplot_writedata
        
        'w': begin
           
           state.p.plotxrange = state.p.plotabsxrange
           state.p.plotyrange = state.p.plotabsyrange
           xzoomplot_plotspec
           xzoomplot_setminmax
           
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

  endif
  
  wset, state.p.plotwin_wid

  !p = state.p.pscale
  !x = state.p.xscale
  !y = state.p.yscale
  x  = event.x/float(state.p.plotsize[0])
  y  = event.y/float(state.p.plotsize[1])
  xy = convert_coord(x,y,/NORMAL,/TO_DATA,/DOUBLE)
  
  if event.type eq 1 then begin
     
     if state.r.cursormode eq 'None' then return
     z = where(finite(state.p.reg) eq 1,count)
     if count eq 0 then begin
        
        wset, state.p.pixmap_wid
        state.p.reg[*,0] = xy[0:1]
        case state.r.cursormode of
           
           'XZoom': plots, [event.x,event.x],$
                           [0,state.p.plotsize[1]],COLOR=2,/DEVICE,LINESTYLE=2
           
           'YZoom': plots, [0,state.p.plotsize[0]],$
                           [event.y,event.y],COLOR=2,/DEVICE,LINESTYLE=2
           
           else:
           
        endcase
        wset, state.p.plotwin_wid
        device, COPY=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,0,$
                      state.p.pixmap_wid]
        
     endif else begin 
        
        state.p.reg[*,1] = xy[0:1]
        case state.r.cursormode of 
           
           'XZoom': state.p.plotxrange = [min(state.p.reg[0,*],MAX=max),max]
           
           'YZoom': state.p.plotyrange = [min(state.p.reg[1,*],MAX=max),max]
           
           'Zoom': begin
              
              state.p.plotxrange = [min(state.p.reg[0,*],MAX=max),max]
              state.p.plotyrange = [min(state.p.reg[1,*],MAX=max),max]
              
           end
           
        endcase
        xzoomplot_plotspec
        xzoomplot_setminmax
        state.r.cursormode='None'
        
     endelse
     
  endif
  
;  Copy the pixmaps and draw the cross hair or zoom lines.
  
  wset, state.p.plotwin_wid
  device, COPY=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,0,$
                state.p.pixmap_wid]
  
  case state.r.cursormode of 
     
     'XZoom': plots, [event.x,event.x],[0,state.p.plotsize[1]],COLOR=2,/DEVICE
     
     'YZoom': plots, [0,state.p.plotsize[0]],[event.y,event.y],COLOR=2,/DEVICE
     
     'Zoom': begin
        
        plots, [event.x,event.x],[0,state.p.plotsize[1]],COLOR=2,/DEVICE
        plots, [0,state.p.plotsize[0]],[event.y,event.y],COLOR=2,/DEVICE
        xy = convert_coord(event.x,event.y,/DEVICE,/TO_DATA,/DOUBLE)
        plots,[state.p.reg[0,0],state.p.reg[0,0]],[state.p.reg[1,0],xy[1]],$
              LINESTYLE=2,COLOR=2
        plots, [state.p.reg[0,0],xy[0]],[state.p.reg[1,0],state.p.reg[1,0]],$
               LINESTYLE=2,COLOR=2
        
     end
     
     else: begin
        
        plots, [event.x,event.x],[0,state.p.plotsize[1]],COLOR=2,/DEVICE
        plots, [0,state.p.plotsize[0]],[event.y,event.y],COLOR=2,/DEVICE
        
     end
     
  endcase
  
;  Update cursor position.
  
  if state.p.cursor then begin
     
     mc_tabinv, *state.d.owave,xy[0],idx
     idx = round(idx)
     label = 'Cursor X: '+strtrim(xy[0],2)+', Y:'+strtrim(xy[1],2)
     label = label+'   Spectrum Col:  '+strtrim(idx[0],2)+'   X: '+$
             strtrim( (*state.d.owave)[idx],2)+$
             ', Y:'+strtrim( (*state.d.oflux)[idx],2)
     widget_control,state.w.message,SET_VALUE=label
     
  endif
  
end
;
;******************************************************************************
;
pro xzoomplot_minmax_event,event

common xzoomplot_state

xmin = mc_cfld(state.w.xmin_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
xmin2 = mc_crange(xmin,state.p.plotxrange[1],'X Min',/KLT,$
               WIDGET_ID=state.w.xzoomplot_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.xmin_fld[0],SET_VALUE=state.p.plotxrange[0]
    return

endif else state.p.plotxrange[0] = xmin2

xmax = mc_cfld(state.w.xmax_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
xmax2 = mc_crange(xmax,state.p.plotxrange[0],'X Max',/KGT,$
               WIDGET_ID=state.w.xzoomplot_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.xmax_fld[0],SET_VALUE=state.p.plotxrange[1]
    return

endif else state.p.plotxrange[1] = xmax2

ymin = mc_cfld(state.w.ymin_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
ymin2 = mc_crange(ymin,state.p.plotyrange[1],'Y Min',/KLT,$
               WIDGET_ID=state.w.xzoomplot_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.ymin_fld[0],SET_VALUE=state.p.plotyrange[0]
    return

endif else state.p.plotyrange[0] = ymin2

ymax = mc_cfld(state.w.ymax_fld,4,/EMPTY,CANCEL=cancel)
if cancel then return
ymax2 = mc_crange(ymax,state.p.plotyrange[0],'Y Max',/KGT,$
               WIDGET_ID=state.w.xzoomplot_base,CANCEL=cancel)
if cancel then begin

    widget_control, state.w.ymax_fld[0],SET_VALUE=state.p.plotyrange[1]
    return

endif else state.p.plotyrange[1] = ymax2

xzoomplot_plotspec

end
;
;******************************************************************************
;
pro xzoomplot_resize_event, event

common xzoomplot_state

widget_control, state.w.xzoomplot_base, TLB_GET_SIZE=size


state.p.plotsize[0]=size[0]-state.p.buffer[0]
state.p.plotsize[1]=size[1]-state.p.buffer[1]

widget_control, state.w.plotwin,UPDATE=0
widget_control, state.w.plotwin, DRAW_XSIZE=state.p.plotsize[0]
widget_control, state.w.plotwin, DRAW_YSIZE=state.p.plotsize[1]
widget_control, state.w.plotwin,UPDATE=1

wdelete,state.p.pixmap_wid
window, /FREE, /PIXMAP,XSIZE=state.p.plotsize[0],$
        YSIZE=state.p.plotsize[1]
state.p.pixmap_wid = !d.window

wset, state.p.plotwin_wid
device, COPY=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,0,$
              state.p.pixmap_wid]

xzoomplot_plotspec

state.p.time = !values.f_nan
        

end
;
;******************************************************************************
;
; ------------------------------Main Program-------------------------------- 
;
;******************************************************************************
;
pro xzoomplot,x,y,PSYM=psym,XRANGE=xrange,YRANGE=yrange,XLOG=xlog, $
              YLOG=ylog,YTITLE=ytitle,XTITLE=xtitle,TITLE=title, $
              POSITION=position,CANCEL=cancel

  cancel = 0

  mc_mkct
  common xzoomplot_state
  
  if n_params() ge 2 then begin
     
     cancel = mc_cpar('xzoomplot',x,1,'X',[2,3,4,5],1)
     if cancel then return
     cancel = mc_cpar('xzoomplot',y,2,'Y',[2,3,4,5],1)
     if cancel then return
     
     if n_elements(POSITION) eq 0 then position = [0.5,0.5]
     if not xregistered('xzoomplot') then xzoomplot_startup,position
     
     state.p.ytitle = (n_elements(YTITLE) ne 0) ? ytitle:''
     state.p.xtitle = (n_elements(XTITLE) ne 0) ? xtitle:''
     state.p.title  = (n_elements(TITLE) ne 0) ? title:''
     state.p.xlog   = keyword_set(XLOG)
     state.p.ylog   = keyword_set(YLOG)
     state.p.psym   = (n_elements(PSYM) eq 0)? 10:psym
     
;  Strip NaNs (why again?)

     good = where(finite(x) eq 1)
     *state.d.owave = x[good]
     *state.d.oflux = y[good]

;  Get plot ranges

     state.p.plotabsxrange = [min(x,MAX=xmax,/NAN),xmax]
     state.p.plotxrange    = (n_elements(XRANGE) ne 0) ? xrange:$
                             state.p.plotabsxrange

     x = findgen(n_elements(x))
     smooth = mc_robustsg(x,y,5,3,0.1,CANCEL=cancel)
     if cancel then return
     
     min = min(smooth[*,1],/NAN,MAX=max)
     del = (max-min)*0.1
     
     state.p.plotabsyrange = [min-del,max+del]
     state.p.plotyrange    = (n_elements(YRANGE) ne 0) ? yrange:$
                             state.p.plotabsyrange

     xzoomplot_setminmax             
     xzoomplot_plotspec
     
  endif else begin
     
     cancel = 1
     print, 'Syntax - xzoomplot,wave,flux,XRANGE=xrange,YRANGE=yrange,$'
     print, '                   XLOG=xlog,ylog=ylog,YTITLE=ytitle,$'
     print, '                   XTITLE=xtitle,TITLE=title,CANCEL=cancel'
     cancel = 0
     return
     
  endelse
  
end





