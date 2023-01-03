;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xmcphot_startup,position,APRADIUS=apradius,BGSTART=bgstart, $
                    BGWIDTH=bgwidth,GROUP_LEADER=group_leader,CANCEL=cancel

  mc_mkct, 0,BOT=bot

  apradius = keyword_set(APRADIUS) ? apradius:5.0
  bgstart  = keyword_set(BGSTART) ? bgstart:10.0
  bgwidth  = keyword_set(BGWIDTH) ? bgwidth:10.0

  common xmcphot_state, state

;  Load the fonts

  mc_getfonts,buttonfont,textfont
  
  state = {apradius:apradius,$
           apflux_lbl:0L,$
           bot:bot,$
           nap_lbl:0L,$
           bgstart:bgstart,$
           bgwidth:bgwidth,$
           bg_lbl:0L,$
           nbg_lbl:0L,$
           bgtype:'Mean',$
           bgval:0.0,$
           imgpos:[0.0,0.0],$
           simgpos:[0.0,0.0],$
           fwhm_lbl:0L,$
           dist:ptr_new(2),$
           prof:ptr_new(2),$
           pixmap_wid:0,$
           basic_base:0L,$
           bg_base:0L,$
           plottype:'Image',$
           magzpt_fld:[0L,0L],$
           magzpt:25.0,$
           mean_but:0L,$
           median_but:0L,$
           none_but:0L,$
           mag_lbl:0L,$
           lim_lbl:0L,$
           img:ptr_new(2.0),$
           imgbuffer:1,$
           subimg:ptr_new(2.0),$
           imgwin_wid:0L,$
           profwin_wid:0L,$
           signal_lbl:0L,$
           simgsize:0L,$
           units:0L,$
           ap_fld:[0L,0L],$
           robust_fld:[0L,0L],$
           ncols:0L,$
           nrows:0L,$
           robust:!values.f_nan,$
           bgstart_fld:[0L,0L],$
           bgwidth_fld:[0L,0L],$
           xscale:!x,$
           yscale:!y,$
           pscale:!p,$
           xcen_fld:[0L,0L],$
           ycen_fld:[0L,0L],$
           xrange:[0.0,0.0],$
           yrange:[0.0,0.0],$
           proffit:ptr_new(2),$
           xmcphot_base:0L}

;  Build the widget.
  
  state.xmcphot_base = widget_base(TITLE='Xmcphot',$
                                   EVENT_PRO='xmcphot_event',$
                                   GROUP_LEADER=group_leader,$
                                   /COLUMN)
  
     button = widget_button(state.xmcphot_base,$
                            FONT=buttonfont,$
                            VALUE='Done',$
                            UVALUE='Done')

     row_base = widget_base(state.xmcphot_base,$
                            /ROW)

     col1_base = widget_base(row_base,$
                            /COLUMN)

        input = widget_base(col1_base,$
                            FRAME=2)
        
        state.basic_base = widget_base(input,$
                                       /COLUMN,$
                                       /BASE_ALIGN_LEFT)

           row = widget_base(state.basic_base,$
                             /ROW,$
                             /BASE_ALIGN_CENTER)

              fld = coyote_field2(row,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='X Cen :',$
                                  UVALUE='X Cen',$
                                  XSIZE=6,$
                                  /CR_ONLY,$
                                  VALUE=strtrim(string(state.apradius, $
                                                       FORMAT='(f5.1)'),2),$
                                  EVENT_PRO='xmcphot_event',$
                                  TEXTID=textid)
              state.xcen_fld = [fld,textid]

              fld = coyote_field2(row,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='Y Cen :',$
                                  UVALUE='Y Cen',$
                                  XSIZE=6,$
                                  /CR_ONLY,$
                                  VALUE=strtrim(string(state.bgstart, $
                                                       FORMAT='(f5.1)'),2),$
                                  EVENT_PRO='xmcphot_event',$
                                  TEXTID=textid)
              state.ycen_fld = [fld,textid]

           row = widget_base(state.basic_base,$
                             /ROW,$
                             /BASE_ALIGN_CENTER)

              fld = coyote_field2(row,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='Ap Radius :',$
                                  UVALUE='Ap Radius',$
                                  XSIZE=6,$
                                  /CR_ONLY,$
                                  VALUE=strtrim(string(state.apradius, $
                                                       FORMAT='(f5.1)'),2),$
                                  EVENT_PRO='xmcphot_event',$
                                  TEXTID=textid)
              state.ap_fld = [fld,textid]

              fld = coyote_field2(row,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='BG Start :',$
                                  UVALUE='BG Start',$
                                  XSIZE=6,$
                                  /CR_ONLY,$
                                  VALUE=strtrim(string(state.bgstart, $
                                                       FORMAT='(f5.1)'),2),$
                                  EVENT_PRO='xmcphot_event',$
                                  TEXTID=textid)
              state.bgstart_fld = [fld,textid]

           row = widget_base(state.basic_base,$
                             /ROW,$
                             /BASE_ALIGN_CENTER)

              fld = coyote_field2(row,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='BG Width :',$
                                  UVALUE='BG Width',$
                                  XSIZE=6,$
                                  /CR_ONLY,$
                                  VALUE=strtrim(string(state.bgwidth, $
                                                       FORMAT='(f5.1)'),2),$
                                  EVENT_PRO='xmcphot_event',$
                                  TEXTID=textid)
              state.bgwidth_fld = [fld,textid]             
                            
              fld = coyote_field2(row,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='Mag Zpt :',$
                                  UVALUE='Mag Zpt',$
                                  XSIZE=6,$
                                  /CR_ONLY,$
                                  VALUE=state.magzpt,$
                                  EVENT_PRO='xmcphot_event',$
                                  TEXTID=textid)
              state.magzpt_fld = [fld,textid]
              
           bgsub_bg = cw_bgroup(state.basic_base,$
                                FONT=buttonfont,$
                                ['None','Mean','Median'],$
                                /ROW,$
                                /NO_RELEASE,$
                                /EXCLUSIVE,$
                                /RETURN_NAME,$
                                LABEL_LEFT='BG Type:',$
                                SET_VALUE=1,$
                                UVALUE='BG Subtraction Type')

           

;        state.bg_base = widget_base(input,$
;                                    /COLUMN,$
;                                    MAP=0)
;
;           row = widget_base(state.bg_base,$
;                             /ROW,$
;                             /TOOLBAR,$
;                             /EXCLUSIVE)
;           
;              state.mean_but = widget_button(row,$
;                                             FONT=buttonfont,$
;                                             VALUE='Mean',$
;                                             UVALUE='Mean BG')
;              widget_control,state.mean_but,/SET_BUTTON
;
;              state.median_but = widget_button(row,$
;                                               FONT=buttonfont,$
;                                               VALUE='Median',$
;                                               UVALUE='Median BG')
;              
;              state.none_but = widget_button(row,$
;                                             FONT=buttonfont,$
;                                             VALUE='None',$
;                                             UVALUE='None BG')
;           fld = coyote_field2(state.bg_base,$
;                               LABELFONT=buttonfont,$
;                               FIELDFONT=textfont,$
;                               TITLE='Robust :',$
;                               UVALUE='BG Robust Threshold',$
;                               XSIZE=5,$
;                               /CR_ONLY,$
;                               EVENT_PRO='xmcphot_event',$
;                               TEXTID=textid)
;           state.robust_fld = [fld,textid]

        output = widget_base(col1_base,$
                            /COLUMN)

        tmp = string(0.0,FORMAT='("FWHM: ",f7.1)')
        state.FWHM_lbl = widget_label(output,$
                                      VALUE=tmp,$
                                      /ALIGN_LEFT,$
                                      FONT=textfont)
        
        tmp = string(0.0,FORMAT='("Aperture Area: ",g12.6)')
        state.nap_lbl = widget_label(output,$
                                     VALUE=tmp,$
                                     FONT=textfont,$
                                     /ALIGN_LEFT)


        tmp = string(0.0,0.0,FORMAT='("Total Counts: ",g12.6," (",g12.6,")")')
        state.apflux_lbl = widget_label(output,$
                                        VALUE=tmp,$
                                        FONT=textfont,$
                                        /ALIGN_LEFT)

        tmp = string(0.0,FORMAT='("N Background Pixels: ",g12.6)')
        state.nbg_lbl = widget_label(output,$
                                     VALUE=tmp,$
                                     FONT=textfont,$
                                     /ALIGN_LEFT)

        tmp = string(0.0,0.0, $
                     FORMAT='("Background (RMS): ",g12.6," (",g12.6,")")')

        state.bg_lbl = widget_label(output,$
                                    VALUE=tmp,$
                                    FONT=textfont,$
                                   /ALIGN_LEFT)

        tmp = string(0.0,0.0,FORMAT='("Signal: ",g12.6," (",g12.6,")")')
        state.signal_lbl = widget_label(output,$
                                        VALUE=tmp,$
                                        FONT=textfont,$
                                          /ALIGN_LEFT)
        
        state.mag_lbl = widget_label(output,$
                                     VALUE=string(' ',FORMAT='(A40)'),$
                                     FONT=textfont,$
                                     /ALIGN_LEFT)

        state.lim_lbl = widget_label(output,$
                                     VALUE=string(' ',FORMAT='(A40)'),$
                                     FONT=textfont,$
                                     /ALIGN_LEFT)

     col2_base = widget_base(row_base,$
                             /ROW,$
                             FRAME=2)


           imgwin = widget_draw(col2_base,$
                                /MOTION_EVENTS,$
                                /BUTTON_EVENTS,$
                                XSIZE=350,$
                                YSIZE=350,$
                                UVALUE='Img Window')

           profwin = widget_draw(col2_base,$
                                 XSIZE=350,$
                                 YSIZE=350,$
                                 UVALUE='Plot Window')
           
        
; Get things running.  Center the widget using the Fanning routine.
           
  cgcentertlb,state.xmcphot_base,position[0],position[1]
  widget_control, state.xmcphot_base, /REALIZE
           
;  Get plotwin ids
           
  widget_control, imgwin, GET_VALUE=x
  state.imgwin_wid = x

  widget_control, profwin, GET_VALUE=x
  state.profwin_wid = x

  window, /FREE, /PIXMAP,XSIZE=350,YSIZE=350
  state.pixmap_wid = !d.window
  
; Start the Event Loop. This will be a non-blocking program.
  
  XManager, 'xmcphot', $
            state.xmcphot_base, $
            /NO_BLOCK
  
end
;
;******************************************************************************
;
pro xmcphot_cleanup,xmcphot_base

  common xmcphot_state
  
  widget_control, xmcphot_base, GET_UVALUE=state, /NO_COPY
  if n_elements(state) ne 0 then begin
     
     ptr_free, state.img
     ptr_free, state.subimg
     ptr_free, state.yfit
     ptr_free, state.dist
     ptr_free, state.prof
     
  endif
  state = 0B

end
;
;==============================================================================
;
pro xmcphot_phot,RESIZE=resize,CANCEL=cancel

  common xmcphot_state

  cancel = 0

  if keyword_set(RESIZE) then begin

     subimg = mc_snipimgc(*state.img,state.imgpos[0],state.imgpos[1], $
                          2*(state.bgstart+state.bgwidth+ $
                             state.imgbuffer),$
                          2*(state.bgstart+state.bgwidth+ $
                             state.imgbuffer),$
                          ncols,nrows,xrange,yrange, $
                          xyoffset,CANCEL=cancel)
     if cancel then return

     *state.subimg = subimg
     state.xrange = xrange
     state.yrange = yrange
  
  endif

;  endif else begin

;     xrange = state.xrange
;     yrange = state.yrange

;  endelse

  state.simgpos = state.imgpos-[state.xrange[0],state.yrange[0]]

  mc_radprof,*state.subimg,state.simgpos[0],state.simgpos[1],dist,prof, $
             CANCEL=cancel
  if cancel then return

  s = sort(dist)
  *state.dist = dist[s]
  *state.prof = prof[s]
  
  parinfo = replicate({value:0,fixed:0},4)
  parinfo(1).fixed = 1
  
  yfit = mpfitpeak(*state.dist,*state.prof,a,NTERMS=4, $
                   ESTIMATES=[prof[0],0D,3D,0D],PARINFO=parinfo)
  *state.proffit = yfit

  tmp = 'FWHM: '+strtrim(string(a[2]*2.354,FORMAT='(f7.1)'),2)
  widget_control,state.fwhm_lbl,SET_VALUE=tmp  
  
  s = size(*state.subimg,/DIMEN)
  
;  Determine the background
  
  xpos = rebin(lindgen(s[0]),s[0],s[1])
  ypos = rebin(reform(lindgen(s[1]),1,s[1]),s[0],s[1])
  dpos = sqrt((xpos-state.simgpos[0])^2 + (ypos-state.simgpos[1])^2)
  
  z = where(dpos gt state.bgstart and dpos lt state.bgstart+state.bgwidth,nbg)
  
  case state.bgtype of 

     'Mean': begin

        mc_moments,(*state.subimg)[z],bg,var,stddev,ROBUST=4,/SILENT, $
                   CANCEL=cancel
        if cancel then return
        bgerr = stddev
        label = '(RMS)'

     end

     'Median': begin

        bg = median((*state.subimg)[z],/EVEN)
        bgerr = (1.4826*median( abs((*state.subimg)[z]-bg), /EVEN))
        label = '(MAD)'

     end

     'None': begin

        bg = 0.0
        bgerr = 0.0
        label = ''

     end

  endcase

  state.bgval = bg

  tmp = 'N Background Pixels: '+strtrim(string(nbg,FORMAT='(I12)'),2)
  widget_control,state.nbg_lbl,SET_VALUE=tmp

  tmp = 'Background '+label+': '+$
        strtrim(string(bg,FORMAT='(g12.6)'),2)+' ('+$
        strtrim(string(bgerr,FORMAT='(g12.6)'),2)+')'

  widget_control,state.bg_lbl,SET_VALUE=tmp

;  Determine the counts in the aperture

  aper,*state.subimg,state.simgpos[0],state.simgpos[1],apflux,errap,sky, $
       skyerr,[1],state.apradius,skyrad,[0,0],/FLUX,SETSKYVAL=0,/SILENT, $
       /EXACT

  tmp = 'Total Counts: '+strtrim(string(apflux,FORMAT='(g12.6)'),2)
  widget_control,state.apflux_lbl,SET_VALUE=tmp

  area = !pi*state.apradius^2
  tmp = 'Aperture Area: '+strtrim(string(area,FORMAT='(g11.6)'),2)
  widget_control,state.nap_lbl,SET_VALUE=tmp

;  Determine the Signal

  signal = apflux-bg*area

  tmp = 'Signal: '+strtrim(string(signal,FORMAT='(g11.6)'),2)
  widget_control,state.signal_lbl,SET_VALUE=tmp

  sigerr = sqrt(area*(bgerr^2+bgerr^2/nbg^2))

;  Compute magnitude

  mag = -2.5*alog10(signal)+state.magzpt

  c = -2.5/alog(10)
  magerr = abs(c)*sqrt(sigerr^2/signal^2)


  tmp = 'Magnitude: '+strtrim(string(mag,FORMAT='(g11.6)'),2)+' ('+$
        strtrim(string(magerr,FORMAT='(g11.6)'),2)+')'
  widget_control,state.mag_lbl,SET_VALUE=tmp

;  Compute 5*sigma limit

  lim = -2.5*alog10(5.0*sqrt(state.apradius^2*!pi*bgerr^2)) + state.magzpt
  tmp = 'Mag Limit: '+strtrim(string(lim,FORMAT='(g11.6)'),2)
  widget_control, state.lim_lbl,SET_VALUE=tmp
  

end
;
;==============================================================================
;
pro xmcphot_plotupdate

  common xmcphot_state

  wset, state.pixmap_wid
  plotsym,0,1,/FILL

  mc_imgrange,*state.subimg,min,max,RANGE=0.95,CANCEL=cancel
  if cancel then return

  tv,congrid(mc_bytsclimg(*state.subimg,0,state.bot,MIN=min,MAX=max),350,350), $
     /DEVICE
  
  style = (keyword_set(PLOTAXES) eq 1) ? 1:5

  plot,[1],[1],/NODATA,/NOERASE,XSTY=style,YSTY=style, $
       XRANGE=xrange,YRANGE=yrange,XMARGIN=[0,0],YMARGIN=[0,0], $
       NOCLIP=0,POSITION=position,XMINOR=0,YMINOR=0,_EXTRA=_extra, $
       COLOR=6



  mc_dispimg,*state.subimg,RANGE=0.95,SCALING=0,CMAP=0,CANCEL=cancel
  if cancel then return
  
  state.xscale = !x
  state.yscale = !y
  state.pscale = !p

  plots,reform(state.simgpos[0]),reform(state.simgpos[1]), $
        PSYM=8,COLOR=7
  
  tvcircle,state.apradius,state.simgpos[0],state.simgpos[1], $
           COLOR=3,/DATA
  tvcircle,state.bgstart,state.simgpos[0],state.simgpos[1], $
           COLOR=2,/DATA
  tvcircle,state.bgstart+state.bgwidth,state.simgpos[0], $
           state.simgpos[1],COLOR=2,/DATA
  
  wset, state.imgwin_wid
  device, COPY=[0,0,350,350,0,0,state.pixmap_wid]        
  
  wset, state.profwin_wid
  plot,*state.dist,*state.prof,/XSTY,/YSTY,PSYM=4, $
       XRANGE=[0,state.bgstart+state.bgwidth+1],$
       XTITLE='Radial Distance (pixels)',$
       YTITLE='Intensity',FONT=0
  
  z = where(*state.dist lt state.apradius)
  oplot,(*state.dist)[z],(*state.prof)[z],PSYM=4,COLOR=3
  plots,[state.apradius,state.apradius],!y.crange,COLOR=3
  
  z = where(*state.dist gt state.bgstart and $
            *state.dist lt state.bgstart+state.bgwidth)
  oplot,(*state.dist)[z],(*state.prof)[z],PSYM=4,COLOR=2
  plots,[state.bgstart,state.bgstart],!y.crange,COLOR=2
  plots,[state.bgstart,state.bgstart]+state.bgwidth,!y.crange,COLOR=2
  
  plots,!x.crange,[state.bgval,state.bgval],COLOR=2,LINESTYLE=2

  oplot,*state.dist,*state.proffit,COLOR=6,THICK=3

  
end
;
;=============================================================================
;
pro xmcphot_track,event

  common xmcphot_state

  if state.plottype eq 'Profile' then return

  wset, state.imgwin_wid
  device, COPY=[0,0,350,350,0,0,state.pixmap_wid]        
  
  !x = state.xscale
  !y = state.yscale
  !p = state.pscale

  xy  = (convert_coord(event.x,event.y,/DEVICE,/TO_DATA))[0:1]+$
        [state.xrange[0],state.yrange[0]]

  if event.release then begin

     state.imgpos = xy[0:1]
     xmcphot_phot
     xmcphot_plotupdate
     widget_control,state.xcen_fld[1], $
                    SET_VALUE=string(state.imgpos[0],FORMAT='(G0.5)')
     widget_control,state.ycen_fld[1], $
                    SET_VALUE=string(state.imgpos[1],FORMAT='(G0.5)')
     
  
  endif else begin

     
     if xy[0] lt 0.0 or $
        xy[0] gt state.ncols-1 or $
        xy[1] lt 0.0 or $
        xy[1] gt state.nrows-1 then $
           z = !values.f_nan else $
              z = (*state.img)[xy[0],xy[1]]
     
     
     val = '('+strtrim(string(xy[0],FORMAT='(f7.2)'),2)+ $
           ','+strtrim(string(xy[1],FORMAT='(f7.2)'),2)+$
           ') '+strtrim(string(z) ,2)
     
     xyouts,0.03,0.02,val,/NORM,COLOR=5,ALIGNMENT=0,FONT=0
  

  endelse
  


end
;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro xmcphot_event, event

common xmcphot_state

widget_control, event.id,  GET_UVALUE = uvalue

case uvalue of

   'Ap Radius': begin
      
      aprad = *event.value
      
      if aprad ge state.bgstart then begin

         state.bgstart = aprad+1
         widget_control,state.bgstart_fld[1], $
                        SET_VALUE=strtrim(state.bgstart,2)
         resize = 1

      endif else resize = 0
      
      state.apradius = aprad
      xmcphot_phot,RESIZE=resize,CANCEL=cancel
      if cancel then return
      xmcphot_plotupdate
      
   end
   
   'BG Start': begin
      
      bgstart = *event.value
      if bgstart le state.apradius then begin

         junk = widget_message('BG Start must be greater than AP Radius.',$
                               /INFO,DIALOG_PARENT=state.xmcphot_base)
         widget_control,state.bgstart_fld[1], $
                        SET_VALUE=strtrim(string(state.bgstart, $
                                                 FORMAT='(f5.1)'),2)
         return

      endif 

      resize = (bgstart gt state.bgstart) ? 1:0
      state.bgstart = bgstart
      xmcphot_phot,/RESIZE,CANCEL=cancel
      if cancel then return
      xmcphot_plotupdate
      
   end

   'BG Width': begin
      
      state.bgwidth = *event.value
      xmcphot_phot,/RESIZE,CANCEL=cancel
      if cancel then return
      xmcphot_plotupdate
      
   end

   'Display Image': begin

      state.plottype = 'Image'
      xmcphot_plotupdate

   end

   'Display Profile': begin

      state.plottype = 'Profile'
      xmcphot_plotupdate

   end

   'Mag Zpt': begin

      state.magzpt = *event.value
      xmcphot_phot

   end

   'BG Subtraction Type': begin

      state.bgtype = strtrim(event.value,2)
      xmcphot_phot
      xmcphot_plotupdate

   end
   
   'Img Window': xmcphot_track,event


   'BG': begin

      widget_control,state.basic_base,MAP=0
      widget_control,state.sn_base,MAP=0
      widget_control,state.bg_base,MAP=1

   end

    'Done': widget_control, event.top, /DESTROY

    else:

endcase

cont: 

end
;
;******************************************************************************
;
; ------------------------------Main Program-------------------------------- 
;
;******************************************************************************
;
pro xmc_phot,img,xcen,ycen,APRADIUS=apradius,BGSTART=bgstart,BGWIDTH=bgwidth, $
             POSITION=position,GROUP_LEADER=group_leader,CANCEL=cancel
  
  cancel = 0

  common xmcphot_state
  
  if n_elements(POSITION) eq 0 then position = [0.5,0.5]

  if not xregistered('xmcphot') then begin

     if n_elements(APRADIUS) eq 0 then begin
        
        s = size(img,/DIMEN)

        simg = mc_snipimgc(img,xcen,ycen,50,50,s[0],s[1],CANCEL=cancel)
        if cancel then return
        
        yfit = mpfit2dpeak(simg,a,NTERMS=4)
        
        apradius = 3.5*a[2]
        bgstart = apradius+3
        apradius = (finite(apradius) eq 0) ? 3:apradius
        bgstart  = (finite(bgstart) eq 0) ? apradius*2:bgstart

        
     endif

     xmcphot_startup,position,APRADIUS=apradius,BGSTART=bgstart, $
                     BGWIDTH=bgwidth,GROUP_LEADER=group_leader,CANCEL=cancel
          
  endif
  
  *state.img = img
  s = size(img,/DIMEN)
  state.ncols = s[0]
  state.nrows = s[1]
  state.imgpos = [xcen,ycen]

  widget_control,state.xcen_fld[1], $
                 SET_VALUE=string(state.imgpos[0],FORMAT='(G0.5)')
  widget_control,state.ycen_fld[1], $
                 SET_VALUE=string(state.imgpos[1],FORMAT='(G0.5)')
  
  xmcphot_phot,/RESIZE,CANCEL=cancel
  if cancel then return
  xmcphot_plotupdate

end





