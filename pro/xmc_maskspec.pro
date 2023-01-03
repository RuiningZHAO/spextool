;+
; NAME:
;     xpixmask
;
; PURPOSE:
;     To mask certain regions in individual spectra in a stack
;
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     mask = xmc_mkpixmask(wave,stack,ISTACKMASK=istackmask,IPIXMASK=ipixmask,$
;                     XTITLE=xtitle,YTITLE=ytitle,GROUP_LEADER=group_leader,$
;                     CANCEL=cancel)
;
; INPUTS:
;     
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;
;
; OUTPUTS:
;
;
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS:
;
;
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;
;
;
; PROCEDURE:
;
;
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
;-
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xmc_maskspec_initcommon,wave,stack,XTITLE=xtitle,YTITLE=ytitle


  common xmc_maskspec_state, state
  
  s = size(stack,/DIMEN)
  ndat = s[0]
  nspec = s[1]
  
  mc_medcomb,stack,med
  yrange = [0.4*min(med,/NAN,MAX=max),1.2*max]
  
  ipixmask = float(stack)
  ipixmask[*] = 1.0
  
  ispecmask=intarr(nspec)+1
  
  removespec = ispecmask le 0
  plotspec   = (intarr(nspec)+1)*(removespec le 0)

  if n_elements(XTITLE) eq 0 then xtitle = ''
  if n_elements(YTITLE) eq 0 then ytitle = ''

  mc_getfonts,buttonfont,textfont
  
;  Build three structures which will hold important info.
  
  w = {all_but:0L,$
       buttonfont:buttonfont,$
       choice_base:0L,$
       idwin:0,$
       keyboard:0L,$
       message:0L,$
       plotrow:0L,$
       plotwin:0,$
       plot_bg:0L,$
       plot_base:0L,$
       mask_base:0L,$
       remove_base:0L,$
       mask_bg:0L,$
       remove_bg:0L,$
       textfont:textfont,$
       xmc_maskspec_base:0L,$
       xmin_fld:[0L,0L],$
       xmax_fld:[0L,0L],$
       ymin_fld:[0L,0L],$
       ymax_fld:[0L,0L]}
  
  r = {cancel:0,$
       choicetype:0,$
       colors:[1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,$
               1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,$
               1,2,3,4,5,6,7,8,9,10],$
       cursormode:'None',$
       lines:[0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,$
              3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,$
              5,5,5,5,5,5,5,5,5,5],$
;       maskrange:[!values.f_nan,!values.f_nan],$
       nspec:nspec,$
       pixmask:ipixmask,$
       plotspec:plotspec,$
       removespec:removespec,$
       maskspec:intarr(nspec)}
  
  d = {oflux:stack,$
       owave:wave}
  
  p = {buffer:[0.,0.],$
       cursor:0,$
       idwin_wid:0L,$
       reg:[[!values.f_nan,!values.f_nan],$
            [!values.f_nan,!values.f_nan]],$
       pixmap_wid:0L,$
       plotwin_wid:0L,$
       plotxrange:[min(wave,MAX=max),max],$
       plotyrange:yrange,$
       plotabsyrange:yrange,$
       plotabsxrange:[min(wave,MAX=max),max],$
       plotsize:[700,550],$
       pscale:!p,$
       xscale:!x,$
       xtitle:xtitle,$
       yscale:!y,$
       ytitle:ytitle}
  
  
;  Load the three structures in the state structure.
  
  state = {w:w,r:r,d:d,p:p}
  
end
;
;******************************************************************************
;
pro xmc_maskspec_plotupdate

  common xmc_maskspec_state

;  Plot spectra

  wset, state.p.pixmap_wid
  erase
  polyfill,[0,0,1,1,0],[0,1,1,0,0],COLOR=20,/NORM
  
  plot, state.d.owave,state.d.oflux[*,0],/NODATA,/XSTY,/YSTY,$
        YRANGE=state.p.plotyrange,XRANGE=state.p.plotxrange,$
        XTITLE=state.p.xtitle,YTITLE=state.p.ytitle, $
        CHARSIZE=1.5,/NOERASE
  
  for i = 0, state.r.nspec-1 do begin
     
     if state.r.plotspec[i] eq 1 then begin
        
        flux = state.d.oflux[*,i]*state.r.pixmask[*,i]
        oplot,state.d.owave,flux,COLOR=state.r.colors[i], $
              LINESTYLE=state.r.lines[i],PSYM=10
        
     endif
     
  endfor
  
  state.p.pscale = !p
  state.p.xscale = !x
  state.p.yscale = !y
  
;  Plot ID window
  
  wset, state.p.plotwin_wid
  device, COPY=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,0,$
                state.p.pixmap_wid]
  
  wset, state.p.idwin_wid
  erase
  polyfill,[0,0,1,1,0],[0,1,1,0,0],COLOR=20,/NORM  

  plot,indgen(state.r.nspec+2),/NODATA,XRANGE=[0,1],$
       YRANGE=[0,state.r.nspec+1],XSTY=5,YSTY=5,XMARGIN=[0,0],YMARGIN=[0,0],$
       /NOERASE
  
  for i = 0, state.r.nspec-1 do begin
     
     if state.r.removespec[i] eq 0 then begin
        
        plots,[0,0.5],[state.r.nspec-i,state.r.nspec-i],$
              COLOR=state.r.colors[i],LINESTYLE=state.r.lines[i]
        xyouts,0.6,state.r.nspec-i,'Spec '+string(i+1,FORMAT='(i2.2)'),$
               COLOR=state.r.colors[i],CHARSIZE=1.5
        
     endif
     
  endfor
  
  xmc_maskspec_setminmax

end
;
;******************************************************************************
;
pro xmc_maskspec_setminmax

  common xmc_maskspec_state
  
  widget_control, state.w.xmin_fld[1],SET_VALUE=strtrim(state.p.plotxrange[0],2)
  widget_control, state.w.xmax_fld[1],SET_VALUE=strtrim(state.p.plotxrange[1],2)
  widget_control, state.w.ymin_fld[1],SET_VALUE=strtrim(state.p.plotyrange[0],2)
  widget_control, state.w.ymax_fld[1],SET_VALUE=strtrim(state.p.plotyrange[1],2)
  
end
;
;******************************************************************************
;
pro xmc_maskspec_updatebg,label,plotset,maskset

  common xmc_maskspec_state

  widget_control, state.w.plot_bg, /DESTROY
  widget_control, state.w.mask_bg, /DESTROY
  
  if state.r.nspec gt 15 then begin
     
     value = string(findgen(state.r.nspec)+1,format='(i2.2)')
     state.w.plot_bg = cw_bgroup(state.w.plot_base,$
                                 FONT=state.w.buttonfont,$
                                 label,$
                                 /COLUMN,$
                                 /SCROLL,$
                                 XSIZE=120,$
                                 X_SCROLL_SIZE=120,$
                                 Y_SCROLL_SIZE=400,$
                                 /RETURN_NAME,$
                                 YSIZE=1000,$
                                 /NONEXCLUSIVE,$
                                 UVALUE='Plot Spectrum',$
                                 SET_VALUE=plotset)

     state.w.mask_bg = cw_bgroup(state.w.mask_base,$
                                 FONT=state.w.buttonfont,$
                                 label,$
                                 /COLUMN,$
                                 /SCROLL,$
                                 XSIZE=120,$
                                 X_SCROLL_SIZE=120,$
                                 Y_SCROLL_SIZE=400,$
                                 /RETURN_NAME,$
                                 YSIZE=1000,$
                                 /NONEXCLUSIVE,$
                                 UVALUE='Mask Spectrum',$
                                 SET_VALUE=maskset)
     
     
  endif else begin
     
     state.w.plot_bg = cw_bgroup(state.w.plot_base,$
                                 FONT=state.w.buttonfont,$
                                 label,$
                                 /COLUMN,$
                                 /RETURN_NAME,$
                                 /NONEXCLUSIVE,$
                                 UVALUE='Plot Spectrum',$
                                 SET_VALUE=plotset)
   
     state.w.mask_bg = cw_bgroup(state.w.mask_base,$
                                 FONT=state.w.buttonfont,$
                                 label,$
                                 /COLUMN,$
                                 /RETURN_NAME,$
                                 /NONEXCLUSIVE,$
                                 UVALUE='Mask Spectrum',$
                                 SET_VALUE=maskset)
     
  endelse



end
;
;******************************************************************************
;
pro xmc_maskspec_zoom,IN=in,OUT=out

common xmc_maskspec_state

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
        xmc_maskspec_plotupdate

    end

    'YZoom': begin

        z = alog10(delabsy/dely)/alog10(2)
        if keyword_set(IN) then z = z+1 else z=z-1
        hwin = delabsy/2.^z/2.
        state.p.plotyrange = [ycen-hwin,ycen+hwin]
        xmc_maskspec_plotupdate

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

        xmc_maskspec_plotupdate

    end

    else:

endcase

end
;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro xmc_maskspec_allnone_event,event

  common xmc_maskspec_state

  widget_control, event.id,  GET_UVALUE = uvalue  
  widget_control, /HOURGLASS

  labels = 'Spectrum '+string(indgen(state.r.nspec)+1,FORMAT='(I2.2)')

  if uvalue eq 'All' then begin

     case state.r.choicetype of 

        0: begin

           state.r.plotspec = 1
           state.r.plotspec = state.r.plotspec*(state.r.removespec le 0)
           z = where(state.r.plotspec eq 1,cnt)
           widget_control, state.w.plot_bg, SET_VALUE=intarr(cnt)+1
           
        end

        2: begin

           state.r.maskspec = 1
           state.r.maskspec = state.r.maskspec*(state.r.removespec le 0)
           z = where(state.r.maskspec eq 1,cnt)
           widget_control, state.w.mask_bg, SET_VALUE=intarr(cnt)+1


        end

     end

  endif else begin

     case state.r.choicetype of 

        0: begin

           state.r.plotspec = 0
           z = where(state.r.removespec eq 0,cnt)
           widget_control, state.w.plot_bg, SET_VALUE=intarr(cnt)

        end

        1: begin

           z = where(state.r.removespec eq 0,COMP=zcomp,NCOMP=ccnt,cnt)
           if ccnt eq 0 then return
           currentlabel = labels[z]
           widget_control, state.w.plot_bg, GET_VALUE=currentplotset
           widget_control, state.w.mask_bg, GET_VALUE=currentmaskset
          
           state.r.plotspec[zcomp] = 1
           state.r.maskspec[zcomp] = 0  
           
           z = where(state.r.removespec eq 0)

           currentlabel = [currentlabel,'Spectrum '+ $
                           string(zcomp+1,FORMAT='(I2.2)')]
           currentplotset = [currentplotset,replicate(1,ccnt)]
           currentmaskset = [currentmaskset,replicate(0,ccnt)]
           
           s = sort(currentlabel)
           
           currentlabel = currentlabel[s]
           currentplotset = currentplotset[s]
           currentmaskset = currentmaskset[s]
           
           state.r.removespec = 0    

           widget_control, state.w.remove_bg, SET_VALUE=state.r.removespec

           xmc_maskspec_updatebg,currentlabel,currentplotset,currentmaskset
           
        end

        2: begin

           state.r.maskspec = 0
           z = where(state.r.removespec eq 0,cnt)
           widget_control, state.w.mask_bg, SET_VALUE=intarr(cnt)

        end

     end
     

  endelse

  xmc_maskspec_plotupdate


end
;
;******************************************************************************
;

pro xmc_maskspec_event,event

  common xmc_maskspec_state
  
  widget_control, event.id,  GET_UVALUE = uvalue
  
  case uvalue of
     
     'Accept': widget_control, event.top, /DESTROY
     
     'Cancel': begin
        
        state.r.cancel = 1
        widget_control, event.top, /DESTROY
        
     end
     
     'Edit Type': begin
        
        widget_control, state.w.plot_base,MAP=0
        widget_control, state.w.remove_base,MAP=0
        widget_control, state.w.mask_base,MAP=0
        
        state.r.choicetype = event.index
        
        case state.r.choicetype of 
           
           0: begin
              
              widget_control, state.w.plot_base, MAP=1
              widget_control, state.w.all_but, SENSITIVE=1
              
           end
           
           1: begin
              
              widget_control, state.w.remove_base, MAP=1
              widget_control, state.w.all_but, SENSITIVE=0
              
           end
           
           2: begin
              
              widget_control, state.w.mask_base, MAP=1
              widget_control, state.w.all_but, SENSITIVE=1
              
           end
           
        endcase
        
     end
     
     'Keyboard': begin
        
        case strtrim(event.ch,2) of 
           
           'c': begin           ; Clear
              
              state.r.cursormode = 'None'
              state.p.reg=!values.f_nan
              xmc_maskspec_plotupdate
              
           end
           
           'i': xmc_maskspec_zoom,/IN
           
           'o': xmc_maskspec_zoom,/OUT
           
           's': begin
              
              state.r.cursormode = 'Select'
              state.p.reg = !values.f_nan
              xmc_maskspec_plotupdate
              
           end
           
           'u': begin

              z = where(state.r.maskspec eq 1,cnt)

              if cnt ne 0 then begin

                 state.r.pixmask[*,z] = 1.0
                 xmc_maskspec_plotupdate

              endif

           end

           'w': begin
              
              state.p.plotxrange = state.p.plotabsxrange
              state.p.plotyrange = state.p.plotabsyrange
              xmc_maskspec_plotupdate
              
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
     
     'Mask Spectrum': begin
  
        idx = fix(strmid(event.value,1,2,/REVERSE))-1
        state.r.maskspec[idx] = event.select

     end
     
     'Plot Spectrum': begin


        idx = fix(strmid(event.value,1,2,/REVERSE))-1
        state.r.plotspec[idx] = event.select
        xmc_maskspec_plotupdate
        
     end
     
     else:
     
  endcase
  
cont: 
  
end
;
;******************************************************************************
;
pro xmc_maskspec_minmax_event,event

  common xmc_maskspec_state
  
  xmin = mc_cfld(state.w.xmin_fld,4,/EMPTY,CANCEL=cancel)
  if cancel then return
  xmin2 = mc_crange(xmin,state.p.plotxrange[1],'X Min',/KLT,$
                    WIDGET_ID=state.w.xmc_maskspec_base,CANCEL=cancel)
  if cancel then begin
     
     widget_control, state.w.xmin_fld[0],SET_VALUE=state.p.plotxrange[0]
     return
     
  endif else state.p.plotxrange[0] = xmin2
  
  xmax = mc_cfld(state.w.xmax_fld,4,/EMPTY,CANCEL=cancel)
  if cancel then return
  xmax2 = mc_crange(xmax,state.p.plotxrange[0],'X Max',/KGT,$
                 WIDGET_ID=state.w.xmc_maskspec_base,CANCEL=cancel)
  if cancel then begin
     
     widget_control, state.w.xmax_fld[0],SET_VALUE=state.p.plotxrange[1]
     return
     
  endif else state.p.plotxrange[1] = xmax2
  
  ymin = mc_cfld(state.w.ymin_fld,4,/EMPTY,CANCEL=cancel)
  if cancel then return
  ymin2 = mc_crange(ymin,state.p.plotyrange[1],'Y Min',/KLT,$
                 WIDGET_ID=state.w.xmc_maskspec_base,CANCEL=cancel)
  if cancel then begin
     
     widget_control, state.w.ymin_fld[0],SET_VALUE=state.p.plotyrange[0]
     return
     
  endif else state.p.plotyrange[0] = ymin2
  
  ymax = mc_cfld(state.w.ymax_fld,4,/EMPTY,CANCEL=cancel)
  if cancel then return
  ymax2 = mc_crange(ymax,state.p.plotyrange[0],'Y Max',/KGT,$
                 WIDGET_ID=state.w.xmc_maskspec_base,CANCEL=cancel)
  if cancel then begin
     
     widget_control, state.w.ymax_fld[0],SET_VALUE=state.p.plotyrange[1]
     return
     
  endif else state.p.plotyrange[1] = ymax2
  
  xmc_maskspec_plotupdate
  
end
;
;******************************************************************************
;
pro xmc_maskspec_remove_event,event

  common xmc_maskspec_state

  widget_control, /HOURGLASS
  
  labels = 'Spectrum '+string(indgen(state.r.nspec)+1,FORMAT='(I2.2)')
  
  state.r.plotspec[event.value] = event.select le 0
  state.r.maskspec[event.value] = 0  

  z = where(state.r.removespec eq 0)
  currentlabel = labels[z]
  widget_control, state.w.plot_bg, GET_VALUE=currentplotset
  widget_control, state.w.mask_bg, GET_VALUE=currentmaskset
   
  if event.select eq 1 then begin

     z = where(currentlabel eq 'Spectrum '+ $
               string(event.value+1,FORMAT='(I2.2)'))

     remove,z,currentlabel
     remove,z,currentplotset
     remove,z,currentmaskset
     
  endif else begin

     currentlabel = [currentlabel,'Spectrum '+ $
                     string(event.value+1,FORMAT='(I2.2)')]
     currentplotset = [currentplotset,1]
     currentmaskset = [currentmaskset,0]

     s = sort(currentlabel)

     currentlabel = currentlabel[s]
     currentplotset = currentplotset[s]
     currentmaskset = currentmaskset[s]

  endelse

  state.r.removespec[event.value] = event.select       

  xmc_maskspec_updatebg,currentlabel,currentplotset,currentmaskset

  xmc_maskspec_plotupdate


end
;
;*****************************************************************************
;
pro xmc_maskspec_resize,event

  common xmc_maskspec_state

  widget_control, event.id,  GET_UVALUE=uvalue
  
  widget_control, state.w.xmc_maskspec_base, TLB_GET_SIZE=size
  
  state.p.plotsize[0] = size[0]-state.p.buffer[0]
  state.p.plotsize[1] = size[1]-state.p.buffer[1]

  widget_control, state.w.xmc_maskspec_base, UPDATE=0
  widget_control, state.w.plotwin, /DESTROY
  widget_control, state.w.idwin, /DESTROY

  state.w.idwin = widget_draw(state.w.plotrow,$
                              XSIZE=170,$
                              YSIZE=state.p.plotsize[1],$
                              UVALUE='ID Window')
  
  
  state.w.plotwin = widget_draw(state.w.plotrow,$
                                XSIZE=state.p.plotsize[0],$
                                YSIZE=state.p.plotsize[1],$
                                /TRACKING_EVENTS,$
                                /BUTTON_EVENTS,$
                                /MOTION_EVENTS,$
                                EVENT_PRO='xmc_maskspec_winevent',$
                                UVALUE='Plot Window 1')

  widget_control, state.w.xmc_maskspec_base, UPDATE=1

  wdelete,state.p.pixmap_wid
  window, /FREE, /PIXMAP,XSIZE=state.p.plotsize[0],$
          YSIZE=state.p.plotsize[1]
  state.p.pixmap_wid = !d.window
  
  xmc_maskspec_plotupdate

end
;
;*****************************************************************************
;
pro xmc_maskspec_winevent,event

  common xmc_maskspec_state
  
  widget_control, event.id,  GET_UVALUE = uvalue
  
;  Check to see if it is a TRACKING event.
  
  if strtrim(tag_names(event,/STRUCTURE_NAME),2) eq 'WIDGET_TRACKING' then begin
     
     if event.enter eq 0 then widget_control, state.w.keyboard, SENSITIVE=0
     
     wset, state.p.plotwin_wid
     device, COPY=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,0,$
                   state.p.pixmap_wid]
     
     goto, cont
     
  endif
  
;  If not, set the keyboard focus and active window.
  
  widget_control, state.w.keyboard, /INPUT_FOCUS, /SENSITIVE
  wset, state.p.plotwin_wid
  
  !p = state.p.pscale
  !x = state.p.xscale
  !y = state.p.yscale
  x  = event.x/float(state.p.plotsize[0])
  y  = event.y/float(state.p.plotsize[1])
  xy = convert_coord(x,y,/NORMAL,/TO_DATA)
  
  if event.type eq 1 then begin
     
     case state.r.cursormode of 
        
        'Select': begin
           
           z = where(finite(state.p.reg) eq 1,count)
           if count eq 0 then begin
              
              state.p.reg[*,0] = xy[0:1]
              wset, state.p.pixmap_wid
              plots, [event.x,event.x],[0,state.p.plotsize[1]],COLOR=7,$
                     /DEVICE,LINESTYLE=2,THICK=2
              wset, state.p.plotwin_wid
              device, COPY=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,$
                            0,state.p.pixmap_wid]
              
              
           endif else begin 
              
              state.p.reg[*,1] = xy[0:1]
              tmp = reform(state.p.reg[0,*])
              tmp = tmp[sort(tmp)]
              z = where(state.r.maskspec eq 1,cnt)

              if cnt ne 0 then begin

                 good = mc_nantrim(state.d.owave,2,CANCEL=cancel)
                 if cancel then return
                 tabinv,state.d.owave[good],tmp,idx
                 idx = idx+good[0] 
                 state.r.pixmask[idx[0]:idx[1],z] = !values.f_nan
                 
              endif

              state.r.cursormode = 'None'
              state.p.reg = !values.f_nan
              xmc_maskspec_plotupdate
              
           endelse
           
        end
        
        
        'XZoom': begin
           
           z = where(finite(state.p.reg) eq 1,count)
           if count eq 0 then begin
              
              state.p.reg[*,0] = xy[0:1]
              wset, state.p.pixmap_wid
              plots, [event.x,event.x],[0,state.p.plotsize[1]],COLOR=2,$
                     /DEVICE,LINESTYLE=1,THICK=2
              wset, state.p.plotwin_wid
              device, COPY=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,$
                            0,state.p.pixmap_wid]
              
           endif else begin
              
              state.p.reg[*,1] = xy[0:1]
              state.p.plotxrange = [min(state.p.reg[0,*],MAX=m),m]
              state.r.cursormode = 'None'
              state.p.reg = !values.f_nan
              xmc_maskspec_plotupdate
              
           endelse
           
        end
        
        'YZoom': begin
           
           z = where(finite(state.p.reg) eq 1,count)
           if count eq 0 then begin
              
              state.p.reg[*,0] = xy[0:1]
              wset, state.p.pixmap_wid
              plots, [0,state.p.plotsize[0]],[event.y,event.y],COLOR=2,$
                     /DEVICE,LINESTYLE=1,THICK=2
              
              wset, state.p.plotwin_wid
              device, COPY=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,$
                            0,state.p.pixmap_wid]
              
           endif else begin
              
              state.p.reg[*,1] = xy[0:1]
              state.p.plotyrange = [min(state.p.reg[1,*],MAX=m),m]
              state.r.cursormode = 'None'
              state.p.reg = !values.f_nan
              xmc_maskspec_plotupdate
              
           endelse
           
        end
        
        'Zoom': begin
           
           z = where(finite(state.p.reg) eq 1,count)
           if count eq 0 then state.p.reg[*,0] = xy[0:1] else begin 
              
              state.p.reg[*,1] = xy[0:1]
              state.p.plotxrange   = [min(state.p.reg[0,*],MAX=max),max]
              state.p.plotyrange   = [min(state.p.reg[1,*],MAX=max),max]
              state.r.cursormode   = 'None'
              state.p.reg = !values.f_nan
              xmc_maskspec_plotupdate
              
           endelse
           
        end
        
        else:
        
     endcase
     
  endif

;  Copy the pixmaps and draw the lines.

  wset, state.p.plotwin_wid
  device, COPY=[0,0,state.p.plotsize[0],state.p.plotsize[1],0,0,$
                state.p.pixmap_wid]
  
  case state.r.cursormode of 
     
     'XZoom': plots, [event.x,event.x],[0,state.p.plotsize[1]],COLOR=2,/DEVICE
     
     'YZoom': plots, [0,state.p.plotsize[0]],[event.y,event.y],COLOR=2,/DEVICE
     
     'Zoom': begin
        
        plots, [event.x,event.x],[0,state.p.plotsize[1]],COLOR=2,/DEVICE
        plots, [0,state.p.plotsize[0]],[event.y,event.y],COLOR=2,/DEVICE
        xy = convert_coord(event.x,event.y,/DEVICE,/TO_DATA)
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
  
;  Update cursor tracker
  
  label = 'Cursor X: '+strtrim(xy[0],2)+', Y:'+strtrim(xy[1],2)
  widget_control,state.w.message,SET_VALUE=label
  
cont:
  
end
;
;******************************************************************************
;
; ------------------------------Main Program-------------------------------- 
;
;******************************************************************************
;
pro xmc_maskspec,wave,stack,specmask,pixmask,XTITLE=xtitle,YTITLE=ytitle, $
                 GROUP_LEADER=group_leader,CANCEL=cancel
  
  if n_params() lt 2 then begin
     
     print, 'Syntax - xmc_maskspec,wave,stack,specmask,pixkmask,$'
     print, '                      XTITLE=xtitle,YTITLE=ytitle,$'
     print, '                      GROUP_LEADER=group_leader,CANCEL=cancel)'
     cancel = 1
     return
     
  endif
  
  cancel = mc_cpar('xmc_maskspec',wave,1,'Wave',[2,3,4,5],1)
  if cancel then return
  cancel = mc_cpar('xmc_maskspec',stack,2,'Stack',[2,3,4,5],[1,2])
  if cancel then return
  
  xmc_maskspec_initcommon,wave,stack,XTITLE=xtitle,YTITLE=ytitle
  
  if n_elements(GROUP_LEADER) ne 0 then widget_control, group_leader, $
     SENSITIVE=0
  
  common xmc_maskspec_state
  
  mc_mkct
  
  state.w.xmc_maskspec_base = widget_base(TITLE='Xmc_maskspec', $
                                           /TLB_SIZE_EVENTS,$
                                           GROUP_LEADER=group_leader,$
                                           /COLUMN)
  
     button = widget_button(state.w.xmc_maskspec_base,$
                            FONT=state.w.buttonfont,$
                            EVENT_PRO='xmc_maskspec_event',$
                            VALUE='Cancel',$
                            UVALUE='Cancel')
     
     row_base = widget_base(state.w.xmc_maskspec_base,$
                            /ROW)
       
        col1_base = widget_base(row_base,$
                                EVENT_PRO='xmc_maskspec_event',$
                                /COLUMN,$
                                FRAME=2)
     
           plot_dl = widget_droplist(col1_base,$
                                     FONT=state.w.buttonfont,$
                                     TITLE='',$
                                     VALUE=['Plot','Remove','Mask'],$
                                     UVALUE='Edit Type')   

           row = widget_base(col1_base,$
                             EVENT_PRO='xmc_maskspec_allnone_event',$
                             /ROW)
           
              state.w.all_but = widget_button(row,$
                                              FONT=state.w.buttonfont,$
                                              VALUE=' All ',$
                                              UVALUE='All')
              
              button = widget_button(row,$
                                     FONT=state.w.buttonfont,$
                                     VALUE='None',$
                                     UVALUE='None')
              
              state.w.choice_base = widget_base(col1_base)
           
           state.w.plot_base = widget_base(state.w.choice_base,$
                                           MAP=1)

           if state.r.nspec gt 15 then begin
              
              value = string(findgen(state.r.nspec)+1,format='(i2.2)')
              state.w.plot_bg = cw_bgroup(state.w.plot_base,$
                                            FONT=state.w.buttonfont,$
                                            'Spectrum '+value,$
                                            /COLUMN,$
                                            /SCROLL,$
                                            XSIZE=120,$
                                            X_SCROLL_SIZE=120,$
                                            Y_SCROLL_SIZE=400,$
                                            /RETURN_NAME,$
                                            YSIZE=1000,$
                                            /NONEXCLUSIVE,$
                                            UVALUE='Plot Spectrum',$
                                            SET_VALUE=state.r.plotspec)
              
              
           endif else begin
              
              value = string(findgen(state.r.nspec)+1,format='(i2.2)')
              state.w.plot_bg = cw_bgroup(state.w.plot_base,$
                                            FONT=state.w.buttonfont,$
                                            'Spectrum '+value,$
                                            /COLUMN,$
                                            /RETURN_NAME,$
                                            /NONEXCLUSIVE,$
                                            UVALUE='Plot Spectrum',$
                                            SET_VALUE=state.r.plotspec)
              
           endelse

           state.w.remove_base = widget_base(state.w.choice_base,$
                                     EVENT_PRO='xmc_maskspec_remove_event',$
                                           MAP=0)

           if state.r.nspec gt 15 then begin
              
              value = string(findgen(state.r.nspec)+1,format='(i2.2)')
              state.w.remove_bg = cw_bgroup(state.w.remove_base,$
                                            FONT=state.w.buttonfont,$
                                            'Spectrum '+value,$
                                            /COLUMN,$
                                            /SCROLL,$
                                            XSIZE=120,$
                                            X_SCROLL_SIZE=120,$
                                            Y_SCROLL_SIZE=400,$
                                            /RETURN_INDEX,$
                                            YSIZE=1000,$
                                            /NONEXCLUSIVE,$
                                            UVALUE='Remove Spectrum',$
                                            SET_VALUE=state.r.removespec)
              
              
           endif else begin
              
              value = string(findgen(state.r.nspec)+1,format='(i2.2)')
              state.w.remove_bg = cw_bgroup(state.w.remove_base,$
                                            FONT=state.w.buttonfont,$
                                            'Spectrum '+value,$
                                            /COLUMN,$
                                            /RETURN_INDEX,$
                                            /NONEXCLUSIVE,$
                                            UVALUE='Remove Spectrum',$
                                            SET_VALUE=state.r.removespec)
              
           endelse

           state.w.mask_base = widget_base(state.w.choice_base,$
                                           MAP=0)

           if state.r.nspec gt 15 then begin
              
              value = string(findgen(state.r.nspec)+1,format='(i2.2)')
              state.w.mask_bg = cw_bgroup(state.w.mask_base,$
                                          FONT=state.w.buttonfont,$
                                          'Spectrum '+value,$
                                          /COLUMN,$
                                          /SCROLL,$
                                          XSIZE=120,$
                                          X_SCROLL_SIZE=120,$
                                          Y_SCROLL_SIZE=400,$
                                          /RETURN_NAME,$
                                          YSIZE=1000,$
                                          /NONEXCLUSIVE,$
                                          UVALUE='Mask Spectrum',$
                                          SET_VALUE=state.r.maskspec)
              
              
           endif else begin
              
              value = string(findgen(state.r.nspec)+1,format='(i2.2)')
              state.w.mask_bg = cw_bgroup(state.w.mask_base,$
                                          FONT=state.w.buttonfont,$
                                          'Spectrum '+value,$
                                          /COLUMN,$
                                          /RETURN_NAME,$
                                          /NONEXCLUSIVE,$
                                          UVALUE='Mask Spectrum',$
                                          SET_VALUE=state.r.maskspec)
              
           endelse


        col2_base = widget_base(row_base,$
                                FRAME=2,$
                                /COLUMN)
             
           state.w.message = widget_text(col2_base, $
                                         YSIZE=1)
           
           state.w.keyboard = widget_text(col2_base, $
                                          EVENT_PRO='xmc_maskspec_event',$
                                          /ALL_EVENTS,$
                                          SCR_XSIZE=1, $
                                          SCR_YSIZE=1, $
                                          UVALUE='Keyboard',$
                                          VALUE='')
           
           state.w.plotrow = widget_base(col2_base,$
                                         /ROW)
           
              state.w.idwin = widget_draw(state.w.plotrow,$
                                          XSIZE=170,$
                                          YSIZE=state.p.plotsize[1],$
                                          UVALUE='ID Window')
              
              
              state.w.plotwin = widget_draw(state.w.plotrow,$
                                            XSIZE=state.p.plotsize[0],$
                                            YSIZE=state.p.plotsize[1],$
                                            /TRACKING_EVENTS,$
                                            /BUTTON_EVENTS,$
                                            /MOTION_EVENTS,$
                                            EVENT_PRO='xmc_maskspec_winevent',$
                                            UVALUE='Plot Window 1')
              
           row_base = widget_base(col2_base,$
;                                  FRAME=2,$
                                  /ROW)
           
              xmin = coyote_field2(row_base,$
                                   LABELFONT=state.w.buttonfont,$
                                   FIELDFONT=state.w.textfont,$
                                   TITLE='X Min:',$
                                   UVALUE='X Min',$
                                   XSIZE=12,$
                                   EVENT_PRO='xmc_maskspec_minmax_event',$
                                   /CR_ONLY,$
                                   TEXTID=textid)
              state.w.xmin_fld = [xmin,textid]
              
              xmax = coyote_field2(row_base,$
                                   LABELFONT=state.w.buttonfont,$
                                   FIELDFONT=state.w.textfont,$
                                   TITLE='X Max:',$
                                   UVALUE='X Max',$
                                   XSIZE=12,$
                                   EVENT_PRO='xmc_maskspec_minmax_event',$
                                   /CR_ONLY,$
                                   TEXTID=textid)
              state.w.xmax_fld = [xmax,textid]
              
              ymin = coyote_field2(row_base,$
                                   LABELFONT=state.w.buttonfont,$
                                   FIELDFONT=state.w.textfont,$
                                   TITLE='Y Min:',$
                                   UVALUE='Y Min',$
                                   XSIZE=12,$
                                   EVENT_PRO='xmc_maskspec_minmax_event',$
                                   /CR_ONLY,$
                                   TEXTID=textid)
              state.w.ymin_fld = [ymin,textid]
              
              ymax = coyote_field2(row_base,$
                                   LABELFONT=state.w.buttonfont,$
                                   FIELDFONT=state.w.textfont,$
                                   TITLE='Y Max:',$
                                   UVAL='Y Max',$
                                   XSIZE=12,$
                                   EVENT_PRO='xmc_maskspec_minmax_event',$
                                   /CR_ONLY,$
                                   TEXTID=textid)
              state.w.ymax_fld = [ymax,textid]
              
              accept = widget_button(state.w.xmc_maskspec_base,$
                                     EVENT_PRO='xmc_maskspec_event',$
                                     FONT=state.w.buttonfont,$
                                     VALUE='Accept',$
                                     UVALUE='Accept')            
              

; Get things running.  Center the widget using the Fanning routine.
          
   cgcentertlb,state.w.xmc_maskspec_base
   widget_control, state.w.xmc_maskspec_base, /REALIZE
   
;  Get plotwin ids
   
   widget_control, state.w.plotwin, GET_VALUE = x
   state.p.plotwin_wid = x
   
   widget_control, state.w.idwin, GET_VALUE = x
   state.p.idwin_wid = x

;  Get sizes for things.
 
  widget_geom = widget_info(state.w.xmc_maskspec_base, /GEOMETRY)
  
  state.p.buffer[0]=widget_geom.xsize-state.p.plotsize[0]
  state.p.buffer[1]=widget_geom.ysize-state.p.plotsize[1]

   
   window, /FREE, /PIXMAP,XSIZE=state.p.plotsize[0],$
           YSIZE=state.p.plotsize[1]
   state.p.pixmap_wid = !d.window
   
   xmc_maskspec_plotupdate
   
; Start the Event Loop. This will be a non-blocking program.
   
   XManager, 'xmc_maskspec', $
             state.w.xmc_maskspec_base,$
             EVENT_HANDLER='xmc_maskspec_resize'
   
   cancel = state.r.cancel
   
   specmask = state.r.removespec le 0
   pixmask = state.r.pixmask

   z = where(finite(pixmask) eq 0,cnt)
   if cnt ne 0 then pixmask[z] = 0.0
   pixmask = fix(pixmask)
   
   if n_elements(GROUP_LEADER) ne 0 then widget_control, group_leader, $
      SENSITIVE=1

   


end
