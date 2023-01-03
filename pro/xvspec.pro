;+
; NAME:
;     xvspec
;
; PURPOSE:
;     Displays Spextool spectral FITS data file.
;    
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     xvspec,[afile],[bfile],POSITION=position,FWHM=fwhm,VMODE=vmode,$
;            PLOTLINMAX=plotlinmax,PLOTREPLACE=plotreplace,$
;            PLOTFIX=plotfix,PLOTOPTFAIL=plotoptfail,$
;            PLOTATMOSPHERE=plotatmosphere,GROUP_LEADER=group_leader,$
;            CANCEL=cancel
;     
; INPUTS:
;     None
;    
; OPTIONAL INPUTS:
;     afile - A string giving the filename of a SpeX spectral FITS
;             image
;     bfile - A string giving the filename of a SpeX spectral FITS
;             image.  The file must effectively be identical to the
;             afile. That is, the bfile can be the "B Beam" spectrum
;             of a pair subtracted extraction.  
;
; KEYWORD PARAMETERS:
;     POSITION      - A 2-element array giving the position of the
;                     widget on the screen in normalized coordinates.
;     FWHM          - The FWHM (in pixels) of a gaussian kernel to
;                     smooth the spectra.
;     VMODE         - 'Ladder' or 'Continuous'.  Defaults to Ladder.
;     PLOTLINMAX    - Set to plot pixels beyond linearity maximum.
;     PLOTREPLACE   - Set to plot pixels that have been replaced.
;     PLOTFIX       - Set to plot pixels that have been fixed.
;     PLOTOPTFAIL   - Set to plot pixels that have failed optimal
;                     extraction.
;     PLOTAMOSPHERE - Set to plot pixels the atmospheric transmission.
;     GROUP_LEADER  - The widget ID of an existing widget that serves
;                     as "group leader" for the newly-created widget. 
;     CANCEL        - Set on return if there is a problem
;     
; OUTPUTS:
;     None
;     
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     xvspec_state
;
; SIDE EFFECTS:
;     None
;
; RESTRICTIONS:
;     None
;
; PROCEDURE:
;     Easy
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     M. Cushing, Institute for Astronomy, University of Hawaii
;-
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xvspec_startup,POSITION=position, FWHM=fwhm,GROUP_LEADER=group_leader,$
                   CANCEL=cancel

  common xvspec_state,state

  cleanplot,/SILENT

; Load color table

  device, RETAIN=2
  mc_mkct,BOT=offset,RED=red,GREEN=green,BLUE=blue

;  Get fonts
  
  mc_getfonts,buttonfont,textfont,CANCEL=cancel
  if cancel then return
  
;  get Spextool path.
    
  spextoolpath = file_dirname(file_dirname( $
                 file_which('Spextool_Instruments.dat'),/MARK))
 
  tfwhm = (n_elements(FWHM) eq 0) ? 0:fwhm

;  Build three structures which will hold important info.

  w = {buttonfont:buttonfont,$
       but_aperture:0L,$
       but_ap:ptr_new(0),$
       but_buffera:0L,$
       but_bufferb:0L,$
       but_continuous:0L,$
       mbut_continuous:0L,$
       but_help:0L,$
       but_fixyrange:0L,$
       but_flx:0L,$
       mbut_flx:0L,$
       but_flags:[0L,0L,0L,0L],$
       but_flux:0L,$
       but_fluxu:[0L,0L,0L,0L,0L,0L,0L],$
       but_ladder:0L,$
       but_plotatmos:0L,$
       mbut_ladder:0L,$
       but_order:0L,$
       but_ord:ptr_new(0),$
       but_um:0L,$
       but_ang:0L,$
       but_nm:0L,$
       but_normspec:0L,$
       but_unc:0L,$
       but_snr:0L,$
       but_smooth:0L,$
       mbut_unc:0L,$
       mbut_snr:0L,$
       but_xlog:0L,$
       but_ylog:0L,$
       but_jy:0L,$
       but_wavelength:0L,$
       but_waveu:[0L,0L,0L],$
       but_writefits:0L,$
       but_writeascii:0L,$
       panel:0L,$
       plotbase:0L,$
       tmp_fld:[0L,0L],$
       funits:['W m-2 um-1',$
               'ergs s-1 cm-2 A-1',$
               'W m-2 Hz-1',$
               'ergs s-1 cm-2 Hz-1',$
               'Jy',$
               'W m-2',$
               'ergs s-1 cm-2'],$
       wunits:['um','nm','A'],$
       plotwin:0L,$
       textfont:textfont,$
       xvspec_base:0L}

  r = {aph_arc:1.0,$
       minmax_idx:0L,$
       mode:'',$
       naps:0L,$
       norders:0L,$
       orders:ptr_new([1,2,3]),$
       packagepath:spextoolpath,$
       path:'',$
       fwhm:tfwhm,$
       filenamepanel:0,$
       smoothingpanel:0}
  
  p = {ap:1,$
       awave:ptr_new(2),$
       atrans:ptr_new(2),$
       blue:blue,$
       buffer:0,$
       winbuffer:[0L,0L],$
       charsize:1.5,$
       color:3,$
       color_bottom:offset,$
       fix:0,$
       freeze:1,$
       funits:ptr_new(2),$
       green:green,$
       mode:'Ladder',$
       ncolors:!d.n_colors<256.0,$
       normspec:0L,$
       nbuffers:0,$
       pixmap_wid:0L,$
       pixpp:250.0,$
       plotatmosphere:0L,$
       plotreplacepixel:1L,$
       plotfixpixel:1L,$
       plotsatpixel:1L,$
       plotoptfail:1L,$
       plotwin_size:[750,600],$
       plotwin_wid:0L,$
       yranges:ptr_new(fltarr(2)),$
       wranges:ptr_new(fltarr(2)),$
       xtitle:'',$
       ytitle:['','',''],$
       red:red,$
       scaling:'Linear',$
       scroll_size:[750,600],$
       spectype:'Flux',$
       xscale:!x,$
       yscale:!y,$
       pscale:!p,$
       wunits:ptr_new(2),$
       xlog:0,$
       ylog:0}
  
  
  d = {afile:'',$
       bfile:'',$
       ahdr:ptr_new(strarr(2)),$
       aspectra:ptr_new(fltarr(2)),$
       apspectra:ptr_new(fltarr(2)),$
       bhdr:ptr_new(strarr(2)),$
       bspectra:ptr_new(fltarr(2)),$
       bpspectra:ptr_new(fltarr(2))}
  
  state = {w:w,r:r,p:p,d:d}
  
;  Build the widget.
  
  state.w.xvspec_base = widget_base(TITLE='xvspec', $
                                    /COLUMN,$
                                    MBAR=mbar,$
                                    /TLB_SIZE_EVENTS,$
                                    GROUP_LEADER=group_leader)

  button = widget_button(mbar, $
                         VALUE='File', $
                         /MENU,$
                         FONT=buttonfont)

     sbutton = widget_button(button, $
                             VALUE='Load Spextool FITS',$
                             UVALUE='Load Spextool FITS Button',$
                             EVENT_PRO='xvspec_event',$
                             FONT=buttonfont)
     
     sbutton = widget_button(button, $
                             VALUE='View FITS Header',$
                             UVALUE='View FITS Header Button',$
                             EVENT_PRO='xvspec_event',$
                             FONT=buttonfont)

     state.w.but_writefits = widget_button(button, $
                                           VALUE='Write Spextool FITS File',$
                                           UVALUE='Write Spextool FITS File',$
                                           EVENT_PRO='xvspec_event',$
                                           FONT=buttonfont)
     
     state.w.but_writeascii = widget_button(button, $
                                            VALUE='Write ASCII File',$
                                            UVALUE='Write ASCII File',$
                                            EVENT_PRO='xvspec_event',$
                                            FONT=buttonfont)
     
     sbutton = widget_button(button, $
                             VALUE='Quit',$
                             UVALUE='Quit',$
                             EVENT_PRO='xvspec_event',$
                             FONT=buttonfont)

  button = widget_button(mbar, $
                         VALUE='Mode', $
                         /MENU,$
                         FONT=buttonfont)

     state.w.but_ladder = widget_button(button, $
                                        VALUE='Ladder Plot',$
                                        UVALUE='Ladder Plot Button',$
                                        EVENT_PRO='xvspec_event',$
                                        /CHECKED_MENU,$
                                        FONT=buttonfont)
     
     state.w.but_continuous = widget_button(button, $
                                            VALUE='Continuous Plot',$
                                            UVALUE='Continuous Plot Button',$
                                            EVENT_PRO='xvspec_event',$
                                            /CHECKED_MENU,$
                                            FONT=buttonfont)

     button = widget_button(mbar, $
                            VALUE='Buffer', $
                            /MENU,$
                            FONT=buttonfont)
     
     state.w.but_buffera = widget_button(button, $
                                         VALUE='Buffer A',$
                                         UVALUE='Buffer A Menu',$
                                         EVENT_PRO='xvspec_event',$
                                         /CHECKED_MENU,$
                                         FONT=buttonfont)

     state.w.but_bufferb = widget_button(button, $
                                         VALUE='Buffer B',$
                                         UVALUE='Buffer B Menu',$
                                         EVENT_PRO='xvspec_event',$
                                         /CHECKED_MENU,$
                                         FONT=buttonfont)
     
  button = widget_button(mbar, $
                         VALUE='Spectrum', $
                         EVENT_PRO='xvspec_event',$
                         UVALUE='Spectrum Button',$
                         /MENU,$
                         FONT=buttonfont)
                
     state.w.but_flx = widget_button(button,$
                                     VALUE='Flux',$
                                     UVALUE='Plot Flux Button',$
                                     FONT=buttonfont,$
                                     /CHECKED_MENU)  
     widget_control, state.w.but_flx,/SET_BUTTON

     state.w.but_unc = widget_button(button,$
                                     VALUE='Uncertainty',$
                                     UVALUE='Plot Uncertainty Button',$
                                     FONT=buttonfont,$
                                     /CHECKED_MENU)  

     state.w.but_snr = widget_button(button,$
                                     VALUE='S/N',$
                                     UVALUE='Plot S/N Button',$
                                     FONT=buttonfont,$
                                     /CHECKED_MENU)  

       state.w.but_order = widget_button(mbar, $
                                    VALUE='Order', $
                                    UVALUE='Order Menu',$
                                    EVENT_PRO='xvspec_event',$
                                    /MENU,$
                                    FONT=buttonfont)
     
     (*state.w.but_ord)[0] = widget_button(state.w.but_order,$
                                             VALUE='1',$
                                             UVALUE='Order 1 Button',$
                                             FONT=buttonfont)
     
  state.w.but_aperture = widget_button(mbar, $
                                       VALUE='Aperture', $
                                       UVALUE='Aperture Menu',$
                                       EVENT_PRO='xvspec_event',$
                                       /MENU,$
                                       FONT=buttonfont)
  
     (*state.w.but_ap)[0] = widget_button(state.w.but_aperture,$
                                          VALUE='1',$
                                          UVALUE='Ap 1 Button',$
                                          FONT=buttonfont,$
                                          /CHECKED_MENU)  
     widget_control, (*state.w.but_ap)[0],/SET_BUTTON


  button = widget_button(mbar, $
                         VALUE='Units', $
                         UVALUE='Units Button',$
                         /MENU,$
                         FONT=buttonfont)

     state.w.but_wavelength = widget_button(button, $
                                            VALUE='Wavelength', $
                                            EVENT_PRO='xvspec_event',$
                                            UVALUE='Wavelength Units Menu',$
                                            /MENU,$
                                            FONT=buttonfont)

        state.w.but_waveu[0] = widget_button(state.w.but_wavelength, $
                                            VALUE='um', $
                                            UVALUE='Wavelength Units Menu',$
                                            FONT=buttonfont,$
                                            /CHECKED_MENU)  
        
        state.w.but_waveu[1] = widget_button(state.w.but_wavelength, $
                                            VALUE='nm', $
                                            UVALUE='Wavelength Units Menu',$
                                            FONT=buttonfont,$
                                            /CHECKED_MENU)  
        
        state.w.but_waveu[2] = widget_button(state.w.but_wavelength, $
                                            VALUE='A', $
                                            UVALUE='Wavelength Units Menu',$
                                            FONT=buttonfont,$
                                            /CHECKED_MENU)  

     state.w.but_flux = widget_button(button, $
                                      VALUE='Flux', $
                                      EVENT_PRO='xvspec_event',$
                                      UVALUE='Flux Units Menu',$
                                      /MENU,$
                                      FONT=buttonfont)

        state.w.but_fluxu[0] = widget_button(state.w.but_flux, $
                                             VALUE='W m-2 um-1', $
                                             UVALUE='Flux Units Menu',$
                                             FONT=buttonfont,$
                                             /CHECKED_MENU)

        state.w.but_fluxu[1] = widget_button(state.w.but_flux, $
                                             VALUE='ergs s-1 cm-2 A-1', $
                                             UVALUE='Flux Units Menu',$
                                             FONT=buttonfont,$
                                             /CHECKED_MENU)

        state.w.but_fluxu[2] = widget_button(state.w.but_flux, $
                                             VALUE='W m-2 Hz-1', $
                                             UVALUE='Flux Units Menu',$
                                             FONT=buttonfont,$
                                             /CHECKED_MENU)     
        
        state.w.but_fluxu[3] = widget_button(state.w.but_flux, $
                                             VALUE='ergs s-1 cm-2 Hz-1', $
                                             UVALUE='Flux Units Menu',$
                                             FONT=buttonfont,$
                                             /CHECKED_MENU)  

        state.w.but_fluxu[4] = widget_button(state.w.but_flux, $
                                             VALUE='Jy', $
                                             UVALUE='Flux Units Menu',$
                                             FONT=buttonfont,$
                                             /CHECKED_MENU)
        
        state.w.but_fluxu[5] = widget_button(state.w.but_flux, $
                                             VALUE='W m-2', $
                                             UVALUE='Flux Units Menu',$
                                             FONT=buttonfont,$
                                             /CHECKED_MENU)
        
        state.w.but_fluxu[6] = widget_button(state.w.but_flux, $
                                             VALUE='ergs s-1 cm-2', $
                                             UVALUE='Flux Units Menu',$
                                             FONT=buttonfont,$
                                             /CHECKED_MENU)  
        
  button = widget_button(mbar, $
                         EVENT_PRO='xvspec_event',$
                         VALUE='Flags', $
                         UVALUE='Flags Button',$
                         /MENU,$
                         FONT=buttonfont)

     state.w.but_flags[0] = widget_button(button, $
                                          VALUE='Lincor Max Pixel (red)', $
                                          UVALUE='Saturated Pixel Menu',$
                                          FONT=buttonfont,$
                                          /CHECKED_MENU)
     widget_control, state.w.but_flags[0],SET_BUTTON=state.p.plotsatpixel
  
     state.w.but_flags[1] = widget_button(button, $
                                          VALUE='Replaced Pixel (blue)', $
                                          UVALUE='Replaced Pixel Menu',$
                                          FONT=buttonfont,$
                                          /CHECKED_MENU)
     widget_control, state.w.but_flags[1],SET_BUTTON=state.p.plotreplacepixel


     state.w.but_flags[2] = widget_button(button, $
                                          VALUE='Fixed Pixel (cyan)', $
                                          UVALUE='Fixed Pixel Menu',$
                                          FONT=buttonfont,$
                                          /CHECKED_MENU)
     widget_control, state.w.but_flags[2],SET_BUTTON=state.p.plotfixpixel

     state.w.but_flags[3] = widget_button(button, $
                                          VALUE='Opt Extract Fail (yellow)', $
                                          UVALUE='Opt Extract Fail Menu',$
                                          FONT=buttonfont,$
                                          /CHECKED_MENU)
     widget_control, state.w.but_flags[3],SET_BUTTON=state.p.plotoptfail




  
        
  button = widget_button(mbar, $
                         VALUE='Plot', $
                         EVENT_PRO='xvspec_event',$
                         UVALUE='Plot Button',$
                         /MENU,$
                         FONT=buttonfont)

     state.w.but_xlog = widget_button(button,$
                                      VALUE='X Log',$
                                      UVALUE='X Log Button',$
                                      FONT=buttonfont,$
                                      /CHECKED_MENU)  

     state.w.but_ylog = widget_button(button,$
                                      VALUE='Y Log',$
                                      UVALUE='Y Log Button',$
                                      FONT=buttonfont,$
                                      /CHECKED_MENU)  

     state.w.but_fixyrange = widget_button(button,$
                                           VALUE='Fix Y Range',$
                                           UVALUE='Fix Y Range Button',$
                                           FONT=buttonfont,$
                                           /CHECKED_MENU)

     state.w.but_normspec = widget_button(button,$
                                          VALUE='Normalize Spectra',$
                                          UVALUE='Normalize Spectra Button',$
                                          FONT=buttonfont,$
                                          /CHECKED_MENU)
     
     state.w.but_plotatmos = widget_button(button,$
                                           VALUE='Plot Atmosphere',$
                                           UVALUE='Plot Atmosphere Button',$
                                           FONT=buttonfont,$
                                           /CHECKED_MENU)  

  button = widget_button(mbar, $
                         VALUE='Tools', $
                         UVALUE='Tools Button',$
                         EVENT_PRO='xvspec_event',$
                         /MENU,$
                         FONT=buttonfont)

     state.w.but_smooth = widget_button(button,$
                                        VALUE='Smooth',$
                                        UVALUE='Smooth Button',$
                                        FONT=buttonfont,$
                                        /CHECKED_MENU)  

          
   button = widget_button(mbar, $
                          VALUE='Help', $
                          EVENT_PRO='xvspec_event',$
                          UVALUE='Help Button',$
                          /HELP,$
                          FONT=buttonfont)

      state.w.but_help = widget_button(button,$
                                       VALUE='xvspec Help',$
                                       UVALUE='Help Button',$
                                       FONT=buttonfont,$
                                       /CHECKED_MENU)  

      row = widget_base(state.w.xvspec_base,$
                        YPAD=0,$
                        XPAD=0,$
                        /BASE_ALIGN_CENTER,$
                        /ROW)

         button = widget_button(row, $
                                VALUE='Load Spextool FITS', $
                                EVENT_PRO='xvspec_event',$
                                UVALUE='Load Spextool FITS Button',$
                                FONT=buttonfont)

;         button = widget_button(row, $
;                                VALUE='View Header',$
;                                UVALUE='View FITS Header Button',$
;                                EVENT_PRO='xvspec_event',$
;                                FONT=buttonfont)


         blank = widget_label(row,$
                              VALUE='  ')

         subrow = widget_base(row,$
                              /ROW,$
                              /TOOLBAR,$
                              /EXCLUSIVE)
         
            state.w.mbut_ladder = widget_button(subrow, $
                                                VALUE='Ladder', $
                                                EVENT_PRO='xvspec_event',$
                                                UVALUE='Ladder Plot Button',$
                                                /NO_RELEASE,$
                                                FONT=buttonfont)
            
            state.w.mbut_continuous = widget_button(subrow, $
                                                    VALUE='Continuous', $
                                                    EVENT_PRO='xvspec_event',$
                                                  UVALUE='Continuous Plot Button',$
                                                    /NO_RELEASE,$
                                                    FONT=buttonfont)

         blank = widget_label(row,$
                              VALUE='  ')
            
         subrow = widget_base(row,$
                              /ROW,$
                              /TOOLBAR,$
                              /EXCLUSIVE)
         
            state.w.mbut_flx = widget_button(subrow, $
                                             VALUE='Flux', $
                                             EVENT_PRO='xvspec_event',$
                                             UVALUE='Plot Flux Button',$
                                             FONT=buttonfont)
            
            state.w.mbut_unc = widget_button(subrow, $
                                             VALUE='Uncertainty', $
                                             EVENT_PRO='xvspec_event',$
                                             UVALUE='Plot Uncertainty Button',$
                                             FONT=buttonfont)
            
            state.w.mbut_snr = widget_button(subrow, $
                                             VALUE='S/N', $
                                             EVENT_PRO='xvspec_event',$
                                             UVALUE='Plot S/N Button',$
                                             FONT=buttonfont)
            widget_control, state.w.mbut_flx,/SET_BUTTON

        button = widget_button(row, $
                               VALUE='Quit',$
                               UVALUE='Quit',$
                               EVENT_PRO='xvspec_event',$
                               FONT=buttonfont)
             
   state.w.plotwin = widget_draw(state.w.xvspec_base,$
                                 /ALIGN_CENTER,$
                                 XSIZE=state.p.plotwin_size[0],$
                                 YSIZE=state.p.plotwin_size[1])
   
; Get things running.  Center the widget using the Fanning routine.

   if n_elements(POSITION) eq 0 then position = [0.5,0.5]
   cgcentertlb,state.w.xvspec_base,position[0],position[1]

   widget_control, state.w.xvspec_base, /REALIZE
   
;  Get plotwin ids

   widget_control, state.w.plotwin, GET_VALUE=x
   state.p.plotwin_wid=x
   window, /FREE, /PIXMAP,XSIZE=state.p.plotwin_size[0], $
           YSIZE=state.p.plotwin_size[1]
   state.p.pixmap_wid = !d.window
   
;  Get sizes for things.
   
   widget_geom = widget_info(state.w.xvspec_base, /GEOMETRY)
   
   state.p.winbuffer[0]=widget_geom.xsize-state.p.plotwin_size[0]
   state.p.winbuffer[1]=widget_geom.ysize-state.p.plotwin_size[1]

; Start the Event Loop. This will be a non-blocking program.
   
   XManager, 'xvspec', $
             state.w.xvspec_base, $
             /NO_BLOCK,$
             EVENT_HANDLER='xvspec_resize',$
             CLEANUP='xvspec_cleanup'
  
end
;
;******************************************************************************
;
pro xvspec_cleanup,base
  
  common xvspec_state
  
  if n_elements(state) ne 0 then begin
     
     ptr_free, state.w.but_ap
     ptr_free, state.w.but_ord
     
     ptr_free, state.r.orders
     ptr_free, state.p.yranges
     ptr_free, state.p.awave
     ptr_free, state.p.atrans
     ptr_free, state.p.wranges
     ptr_free, state.d.ahdr
     ptr_free, state.d.aspectra
     ptr_free, state.d.apspectra
     ptr_free, state.d.bhdr
     ptr_free, state.d.bspectra
     ptr_free, state.d.bpspectra
     
     
  endif
  state = 0B
  
end
;
;******************************************************************************
;
pro xvspec_convertflux

  common xvspec_state

  ftitle = mc_getfunits((*state.p.funits)[1],CANCEL=cancel)
  if cancel then return

  lidx = strpos(ftitle,'(')
  ridx = strpos(ftitle,')')
  ypunits = strmid(ftitle,lidx+1,ridx-lidx-1)
  state.p.ytitle  = [ftitle,'!5Uncertainty ('+ypunits+')','!5S/N']
  
  for i = 0,state.r.norders-1 do begin
     
     for j = 0,state.r.naps-1 do begin
        
        x = (*state.d.aspectra)[*,0,i*state.r.naps+j]
        y = (*state.d.aspectra)[*,1,i*state.r.naps+j]
        e = (*state.d.aspectra)[*,2,i*state.r.naps+j]
        
        ny = mc_chfunits(x,y,(*state.p.wunits)[0],(*state.p.funits)[0],$
                         (*state.p.funits)[1],IERROR=e,OERROR=oe,CANCEL=cancel)
        if cancel then return
        (*state.d.aspectra)[*,1,i*state.r.naps+j] = ny
        (*state.d.aspectra)[*,2,i*state.r.naps+j] = oe

        x = (*state.d.apspectra)[*,0,i*state.r.naps+j]
        y = (*state.d.apspectra)[*,1,i*state.r.naps+j]
        e = (*state.d.apspectra)[*,2,i*state.r.naps+j]
        
        ny = mc_chfunits(x,y,(*state.p.wunits)[0],(*state.p.funits)[0],$
                         (*state.p.funits)[1],IERROR=e,OERROR=oe,CANCEL=cancel)
        if cancel then return
        (*state.d.apspectra)[*,1,i*state.r.naps+j] = ny
        (*state.d.apspectra)[*,2,i*state.r.naps+j] = oe

        if state.p.nbuffers eq 2 then begin


           x = (*state.d.bspectra)[*,0,i*state.r.naps+j]
           y = (*state.d.bspectra)[*,1,i*state.r.naps+j]
           e = (*state.d.bspectra)[*,2,i*state.r.naps+j]
           
           ny = mc_chfunits(x,y,(*state.p.wunits)[0],(*state.p.funits)[0],$
                            (*state.p.funits)[1],IERROR=e,OERROR=oe, $
                            CANCEL=cancel)
           if cancel then return
           (*state.d.bspectra)[*,1,i*state.r.naps+j] = ny
           (*state.d.bspectra)[*,2,i*state.r.naps+j] = oe
           
           x = (*state.d.bpspectra)[*,0,i*state.r.naps+j]
           y = (*state.d.bpspectra)[*,1,i*state.r.naps+j]
           e = (*state.d.bpspectra)[*,2,i*state.r.naps+j]
           
           ny = mc_chfunits(x,y,(*state.p.wunits)[0],(*state.p.funits)[0],$
                            (*state.p.funits)[1],IERROR=e,OERROR=oe, $
                            CANCEL=cancel)
           if cancel then return
           (*state.d.bpspectra)[*,1,i*state.r.naps+j] = ny
           (*state.d.bpspectra)[*,2,i*state.r.naps+j] = oe

        endif
        
     endfor
     
  endfor

  *state.p.funits = (*state.p.funits)[1]
  
end
;
;******************************************************************************
;
pro xvspec_convertwave

  common xvspec_state

  mc_getwunits,(*state.p.wunits)[1],wunits,xtitle,CANCEL=cancel
  if cancel then return
  
  state.p.xtitle = xtitle
  
  for i = 0,state.r.norders-1 do begin
     
     for j = 0,state.r.naps-1 do begin
        
        x = (*state.d.aspectra)[*,0,i*state.r.naps+j]
        nx = mc_chwunits(x,(*state.p.wunits)[0],(*state.p.wunits)[1], $
                         CANCEL=cancel)
        if cancel then return
        (*state.d.aspectra)[*,0,i*state.r.naps+j] = nx
        (*state.d.apspectra)[*,0,i*state.r.naps+j] = nx

        if state.p.nbuffers eq 2 then begin

        x = (*state.d.bspectra)[*,0,i*state.r.naps+j]
        nx = mc_chwunits(x,(*state.p.wunits)[0],(*state.p.wunits)[1], $
                         CANCEL=cancel)
        if cancel then return
        (*state.d.bspectra)[*,0,i*state.r.naps+j] = nx
        (*state.d.bpspectra)[*,0,i*state.r.naps+j] = nx
           
        endif
        
     endfor
     
  endfor

  x = *state.p.awave
  nx = mc_chwunits(*state.p.awave,(*state.p.wunits)[0],(*state.p.wunits)[1], $
                   CANCEL=cancel)
  if cancel then return

  *state.p.awave = nx
  
  *state.p.wunits = (*state.p.wunits)[1] 
 
end
;
;******************************************************************************
;
pro xvspec_getminmax

  common xvspec_state

  case state.p.buffer of

     0: spectra = *state.d.apspectra

     1: spectra = *state.d.bpspectra

  endcase
  
  for i = 0, state.r.norders-1 do begin

     cwave  = reform(spectra[*,0,i])
     cflux  = reform(spectra[*,1,i])
     cerror = reform(spectra[*,2,i])
     
     case state.p.spectype of 
        
        'Flux': spec = cflux
        
        'Uncertainty': spec = cerror
        
        'S/N': spec = cflux/cerror
        
     endcase

     if state.p.normspec then spec = spec/median(spec,/EVEN)

;  Smooth avoid bad pixels
     
     x = findgen(n_elements(spec))
     smooth = mc_robustsg(x,spec,5,3,0.1,CANCEL=cancel)
     if cancel then return
     
     min = min(smooth[*,1],/NAN,MAX=max)
     del = (max-min)*0.1

     (*state.p.yranges)[*,i] = [min-del,max+del]

;  Now do the wavelengths ignoring NaNs
     
     z = where(finite(cflux) eq 1)
     (*state.p.wranges)[*,i] = [min(cwave[z],MAX=max,/NAN),max]
          
  endfor
  
end
;
;******************************************************************************
;
pro xvspec_loadspec,afile,bfile,CANCEL=cancel

  cancel = 0

  common xvspec_state
  
  mc_getfonts,buttonfont,textfont,CANCEL=cancel
  if cancel then return
  
  if n_elements(afile) eq 0 then begin
     
     x        = findgen(1000)/10.
     y        = exp(-0.04*x)*sin(x)
     y        = y-min(y)+100
     e        = sqrt(y)
     f        = fltarr(1000)+0.0
     aspectra  = [[x],[y],[e],[f]]
     ahdr      = ''
     aobsmode  = '1D'
     aSTART    = min(x)
     aSTOP     = max(x)
     anorders  = 1
     aorders   = 1
     anaps     = 1
     axtitle   = '!7k!5 (pixels)'
     aytitle   = '!5f (DN s!U-1!N)'
     afile    = 'tmp'
     arp       = 0
     axunits   = 'pixels'
     ayunits   = 'flux'
     *state.p.wunits = !values.f_nan
     *state.p.funits = !values.f_nan

  endif else begin

     mc_readspec,afile,aspectra,ahdr,aobsmode,astart,astop,anorders,anaps, $
                 aorders,axunits,ayunits,aslith_pix,aslith_arc,aslitw_pix, $
                 aslitw_arc,arp,aairmass,axtitle,aytitle,/SILENT, $
                 WIDGET_ID=state.w.xvspec_base,CANCEL=cancel
     if cancel then return
     
;  Check for flag array

     aspectra = mc_caaspecflags(aspectra,CANCEL=cancel)
     if cancel then return
          
  endelse

;  Load the a buffer
  
  state.d.afile      = file_basename(afile)
  *state.d.ahdr      = ahdr
  *state.d.aspectra  = aspectra
  *state.d.apspectra = aspectra
  state.r.norders    = anorders
  state.r.naps       = anaps
  *state.r.orders    = (n_elements(AORDERS) ne 0) ? aorders:1
  state.p.ap         = 0
  state.p.nbuffers   = 1
  
  if n_elements(bfile) ne 0 then begin
     
     mc_readspec,bfile,bspectra,bhdr,bobsmode,bstart,bstop,bnorders,bnaps, $
                 borders,bxunits,byunits,bslith_pix,bslith_arc,bslitw_pix, $
                 bslitw_arc,arp,bairmass,bxtitle,bytitle,/SILENT, $
                 WIDGET_ID=state.w.xvspec_base,CANCEL=cancel
     if cancel then return
     
;  Check for flag array

     bspectra = mc_caaspecflags(bspectra,CANCEL=cancel)
     if cancel then return

     if aobsmode eq bobsmode and anorders eq bnorders and anaps eq bnaps and $
        total(aorders) eq total(borders) and axunits eq bxunits and $
        ayunits eq byunits and aslith_pix eq bslith_pix and $
        aslith_arc eq bslith_arc and aslitw_pix eq bslitw_pix and $
        aslitw_arc eq bslitw_arc then begin

        state.d.bfile      = file_basename(bfile)
        *state.d.bhdr      = bhdr
        *state.d.bspectra  = bspectra
        *state.d.bpspectra = bspectra
        state.p.nbuffers   = 2
        
     endif

     widget_control, state.w.but_buffera,/SET_BUTTON
     widget_control, state.w.but_bufferb,SET_BUTTON=0
     widget_control, state.w.but_bufferb,SENSITIVE=1
     
  endif else begin

     widget_control, state.w.but_buffera,/SET_BUTTON
     widget_control, state.w.but_bufferb,SET_BUTTON=0
     widget_control, state.w.but_bufferb,SENSITIVE=0
     state.p.buffer = 0
     
  endelse
 
;  Smooth data if necessary

  xvspec_smoothspectra

;  Deal with the units
  
  state.p.xtitle = axtitle
  lidx = strpos(aytitle,'(')
  ridx = strpos(aytitle,')')
  ypunits = strmid(aytitle,lidx+1,ridx-lidx-1)
  state.p.ytitle  = [aytitle,'!5Uncertainty ('+ypunits+')','!5S/N']
  
;  check Wavelength units
  
  z = where(strcompress(state.w.wunits,/RE) eq strcompress(axunits,/RE),cnt)

  for i = 0,2 do widget_control, state.w.but_waveu[i], SET_BUTTON=0

  if cnt eq 1 then begin

     widget_control, state.w.but_wavelength,SENSITIVE=1
     *state.p.wunits = z
     
     widget_control, state.w.but_waveu[z],/SET_BUTTON

;  Get atmospheric transmission

     widget_control, state.w.but_plotatmos,SENSITIVE=1

;  Load the atmospheric transmission

     if arp ge 10000.0 then trp = round(arp/10000.)*10000
     if arp lt 10000.0 and arp ge 1000.0 then trp = round(arp/1000.)*1000
     if arp lt 1000.0 and arp ge 100.0 then trp = round(arp/100.)*100
     if arp lt 100.0 and arp ge 10.0 then trp = 100
     if arp eq 0 then trp=2000
     
     spec = readfits(filepath('atran'+strtrim(trp,2)+'.fits', $
                              ROOT_DIR=state.r.packagepath,SUBDIR='data'))
     *state.p.awave = reform(spec[*,0])
     *state.p.atrans = reform(spec[*,1])
     
  endif else begin

     widget_control, state.w.but_wavelength,SENSITIVE=0
     *state.p.wunits = ''

     widget_control, state.w.but_plotatmos,SENSITIVE=0

  endelse
  
;  Check flux units

  z = where(strcompress(state.w.funits,/RE) eq strcompress(ayunits,/RE),cnt)

  for i = 0,6 do widget_control, state.w.but_fluxu[i],SET_BUTTON=0

  if cnt eq 1 then begin
     
     widget_control, state.w.but_flux,SENSITIVE=1
     *state.p.funits = z
     widget_control, state.w.but_fluxu[z],/SET_BUTTON
     

  endif else begin

     widget_control, state.w.but_flux,SENSITIVE=0
     *state.p.funits = ''

  endelse
     
  cont:
  
  *state.p.yranges = fltarr(2,anorders)
  *state.p.wranges = fltarr(2,anorders)
  
;  Modify plot window according to the number of orders

  if state.p.mode eq 'Ladder' then begin

     state.p.plotwin_size[0] = state.p.scroll_size[0]
     state.p.plotwin_size[1] = (state.p.scroll_size[1] > $
                                state.p.pixpp*state.r.norders)

  endif else begin

     state.p.plotwin_size[1] = state.p.scroll_size[1]
     state.p.plotwin_size[0] = (state.p.scroll_size[0] > $
                                state.p.pixpp*state.r.norders)
     

  endelse

  xvspec_modwinsize,/DESTROY

;  Update aperture button

  widget_control, state.w.xvspec_base, UPDATE=0
  for i = 0,n_elements(*state.w.but_ap)-1 do begin
     
     widget_control, (*state.w.but_ap)[i], /DESTROY
     
  endfor
  
  (*state.w.but_ap) = lonarr(anaps)
  
  for i = 0,anaps-1 do begin
     
     (*state.w.but_ap)[i] = widget_button(state.w.but_aperture,$
                                          VALUE=string(i+1,FORMAT='(I2.2)'),$
                                          UVALUE='Aperture Menu',$
                                          FONT=buttonfont,$
                                          /CHECKED_MENU)  
     
  endfor
  
  widget_control, (*state.w.but_ap)[0],/SET_BUTTON

;  Update order button

  for i = 0,n_elements(*state.w.but_ord)-1 do begin
     
     widget_control, (*state.w.but_ord)[i], /DESTROY
     
  endfor
  
  (*state.w.but_ord) = lonarr(anorders)
  
  for i = 0,anorders-1 do begin
     
     (*state.w.but_ord)[i] = widget_button(state.w.but_order,$
                                           VALUE=string((*state.r.orders)[i], $
                                                        FORMAT='(I2.2)'),$
                                           UVALUE='Order Menu',$
                                           FONT=buttonfont,$
                                           /CHECKED_MENU)  
     
  endfor
  
  widget_control, state.w.xvspec_base, /UPDATE  

  state.p.fix = 0
  state.p.freeze = 0
  
end
;
;******************************************************************************
;
pro xvspec_modwinsize,DESTROY=destroy

  common xvspec_state
  
  widget_control, state.w.xvspec_base,UPDATE=0

  if state.p.mode eq 'Ladder' then begin
     
     if state.p.plotwin_size[1] le state.p.scroll_size[1] then begin

        if keyword_set(DESTROY) then widget_control, state.w.plotwin, /DESTROY

        state.w.plotwin = widget_draw(state.w.xvspec_base,$
                                      /ALIGN_CENTER,$
                                      XSIZE=state.p.plotwin_size[0],$
                                      YSIZE=state.p.plotwin_size[1],$
                                      EVENT_PRO='xvspec_plotwinevent',$
                                      /KEYBOARD_EVENTS,$
                                      /BUTTON_EVENTS,$
                                      /TRACKING_EVENTS)

        widget_control, state.w.xvspec_base, UPDATE=1
        
        widget_geom = widget_info(state.w.xvspec_base, /GEOMETRY)
        
        state.p.winbuffer[0]=widget_geom.xsize-state.p.plotwin_size[0]
        state.p.winbuffer[1]=widget_geom.ysize-state.p.plotwin_size[1]
                       
     endif else begin

        if keyword_set(DESTROY) then widget_control, state.w.plotwin, /DESTROY

        state.w.plotwin = widget_draw(state.w.xvspec_base,$
                                      /ALIGN_CENTER,$
                                      XSIZE=state.p.plotwin_size[0],$
                                      YSIZE=state.p.plotwin_size[1],$
                                      X_SCROLL_SIZE=state.p.scroll_size[0],$
                                      Y_SCROLL_SIZE=state.p.scroll_size[1],$
                                      /SCROLL,$
                                      EVENT_PRO='xvspec_plotwinevent',$
                                      /KEYBOARD_EVENTS,$
                                      /BUTTON_EVENTS,$
                                      /TRACKING_EVENTS)
                
        widget_control, state.w.xvspec_base, UPDATE=1
        
        widget_geom = widget_info(state.w.xvspec_base, /GEOMETRY)
        
        state.p.winbuffer[0]=widget_geom.xsize-state.p.plotwin_size[0]
        state.p.winbuffer[1]=widget_geom.ysize-state.p.scroll_size[1]
                              
     endelse

  endif

  if state.p.mode eq 'Continuous' then begin

     if state.p.plotwin_size[0] le state.p.scroll_size[0] then begin

        if keyword_set(DESTROY) then widget_control, state.w.plotwin, /DESTROY
        
        state.w.plotwin = widget_draw(state.w.xvspec_base,$
                                      /ALIGN_CENTER,$
                                      XSIZE=state.p.plotwin_size[0],$
                                      YSIZE=state.p.plotwin_size[1],$
                                      EVENT_PRO='xvspec_plotwinevent',$
                                      /KEYBOARD_EVENTS,$
                                      /BUTTON_EVENTS,$
                                      /TRACKING_EVENTS)

        widget_control, state.w.xvspec_base, UPDATE=1
        
        widget_geom = widget_info(state.w.xvspec_base, /GEOMETRY)
        
        state.p.winbuffer[0]=widget_geom.xsize-state.p.plotwin_size[0]
        state.p.winbuffer[1]=widget_geom.ysize-state.p.plotwin_size[1]
                       

        
     endif else begin

        if keyword_set(DESTROY) then widget_control, state.w.plotwin, /DESTROY
       
        state.w.plotwin = widget_draw(state.w.xvspec_base,$
                                      /ALIGN_CENTER,$
                                      XSIZE=state.p.plotwin_size[0],$
                                      YSIZE=state.p.plotwin_size[1],$
                                      X_SCROLL_SIZE=state.p.scroll_size[0],$
                                      Y_SCROLL_SIZE=state.p.scroll_size[1],$
                                      /SCROLL,$
                                      EVENT_PRO='xvspec_plotwinevent',$
                                      /KEYBOARD_EVENTS,$
                                      /BUTTON_EVENTS,$
                                      /TRACKING_EVENTS)
                
        widget_control, state.w.xvspec_base, UPDATE=1
        
        widget_geom = widget_info(state.w.xvspec_base, /GEOMETRY)
        
        state.p.winbuffer[0]=widget_geom.xsize-state.p.scroll_size[0]
        state.p.winbuffer[1]=widget_geom.ysize-state.p.plotwin_size[1]
        
     endelse

  endif
     
  wdelete,state.p.pixmap_wid
  window, /FREE, /PIXMAP,XSIZE=state.p.plotwin_size[0],$
          YSIZE=state.p.plotwin_size[1]
  state.p.pixmap_wid = !d.window
     
     
end
;
;******************************************************************************
;
pro xvspec_plotspec

  common xvspec_state

  case state.p.buffer of

     0: begin

        spectra = *state.d.apspectra
        buffer = 'A'
        file = state.d.afile
        
     end
        
     1: begin

        spectra = *state.d.bpspectra
        buffer = 'B'
        file = state.d.bfile
        
     end
        
  endcase

;  Update title bar
  
  widget_control, state.w.xvspec_base,TLB_SET_TITLE='xvspec ('+buffer+')'+'- '+file
  
  case state.p.spectype of 
     
     'Flux': begin
                
        xtitle = state.p.xtitle
        ytitle = state.p.ytitle[0]
        if state.p.normspec then ytitle = 'Normalized Flux'
        
     end
     
     'Uncertainty': begin
                
        xtitle = state.p.xtitle
        ytitle = state.p.ytitle[1]
        if state.p.normspec then ytitle = 'Normalized Uncertainty'
        
     end
     
     'S/N': begin
                
        xtitle = state.p.xtitle
        ytitle = state.p.ytitle[2]
        if state.p.normspec then ytitle = 'Normalized S/N'
        
     end
     
  endcase

  idx = reverse( findgen(state.r.norders) * state.r.naps + (state.p.ap) )

  tvlct, state.p.red, state.p.green, state.p.blue, state.p.color_bottom

;  Ladder plot
  
  if state.p.mode eq 'Ladder' then begin

  
     !p.multi[2] = state.r.norders
     !p.multi[0] = state.r.norders
          
     charsize = state.p.charsize
     if state.r.norders ge 3 then charsize = state.p.charsize*2.0
     
     for i = 0, state.r.norders-1 do begin
        
        j = state.r.norders-1-i
        
        title= '!5Order '+string((*state.r.orders)[j],FORMAT='(i2.2)')+$
               ', Ap '+string(state.p.ap+1,FORMAT='(i2.2)')
        
        wave   = spectra[*,0,idx[i]]
        cflux  = spectra[*,1,idx[i]]
        cerror = spectra[*,2,idx[i]]
        flag   = spectra[*,3,idx[i]]
        
        case state.p.spectype of 
           
           'Flux': spec   = cflux

           'Uncertainty': spec   = cerror

           'S/N': spec   = cflux/cerror
         
        endcase

        if state.p.normspec then spec = spec/median(spec,/EVEN)

        if state.p.plotatmosphere then begin
        
           plot,wave,spec,XSTYLE=5,/NODATA,YRANGE=[0,1],YSTYLE=5,$
                CHARSIZE=charsize,XLOG=state.p.xlog,BACKGROUND=20
           oplot,*state.p.awave,*state.p.atrans,COLOR=5

           !p.multi[0] = !p.multi[0]+1
           plot,wave,spec,/XSTYLE,XTITLE=xtitle,YTITLE=ytitle,$
                CHARSIZE=charsize,TITLE=title,/NODATA,$
                YRANGE=(*state.p.yranges)[*,j],/YSTYLE,YLOG=state.p.ylog,$
                XLOG=state.p.xlog          
           
        endif else begin

           plot,wave,spec,/XSTYLE,XTITLE=xtitle,YTITLE=ytitle,$
                CHARSIZE=charsize,TITLE=title,/NODATA,$
                YRANGE=(*state.p.yranges)[*,j],/YSTYLE,YLOG=state.p.ylog,$
                XLOG=state.p.xlog
           
        endelse

        oplot,wave,spec,COLOR=state.p.color,PSYM=10
        
        if state.p.plotsatpixel then begin
           
           mask = mc_bitset(fix(flag),0,CANCEL=cancel)
           z = where(mask eq 1,cnt)
           plotsym,0,0.9,/FILL
           if cnt ne 0 then oplot,wave[z],spec[z],PSYM=8,COLOR=2
           
        endif
        
        
        if state.p.plotreplacepixel then begin
           
           mask = mc_bitset(fix(flag),1,CANCEL=cancel)
           z = where(mask eq 1,cnt)
           plotsym,0,1.0,/FILL
           if cnt ne 0 then oplot,wave[z],spec[z],PSYM=8,COLOR=4
           
        endif

        if state.p.plotfixpixel then begin
           
           mask = mc_bitset(fix(flag),2,CANCEL=cancel)
           z = where(mask eq 1,cnt)
           plotsym,0,1.1,/FILL
           if cnt ne 0 then oplot,wave[z],spec[z],PSYM=8,COLOR=7
           
        endif
        
        if state.p.plotoptfail then begin
           
           mask = mc_bitset(fix(flag),3,CANCEL=cancel)
           z = where(mask eq 1,cnt)
           plotsym,0,1.2,/FILL
           if cnt ne 0 then oplot,wave[z],spec[z],PSYM=8,COLOR=5
           
        endif
        
        if !y.crange[0] lt 0 then plots,!x.crange,[0,0],LINESTYLE=1
                
     endfor
     
     !p.multi=0


  endif

;  Continuous plot
  
  if state.p.mode eq 'Continuous' then begin

     xrange = [min(*state.p.wranges,MAX=max,/NAN),max]
     yrange = [min(*state.p.yranges,MAX=max),max]

     if state.p.plotatmosphere then begin
     
        plot,[1],[1],XSTYLE=5,/NODATA,YRANGE=[0,1],YSTYLE=5,$
             CHARSIZE=state.p.charsize,XLOG=state.p.xlog,XRANGE=xrange,$
             BACKGROUND=20
        oplot,*state.p.awave,*state.p.atrans,COLOR=5
        noerase = 1

     endif else noerase = 0
     
     plot,[1],[1],/XSTYLE,XTITLE=xtitle,YTITLE=ytitle,$
          CHARSIZE=state.p.charsize,TITLE=title,/NODATA,$
          YRANGE=yrange,XRANGE=xrange,/YSTYLE,YLOG=state.p.ylog,$
          XLOG=state.p.xlog,BACKGROUND=20,NOERASE=noerase
     
     for i = 0, state.r.norders-1 do begin
        
        wave   = spectra[*,0,i]
        cflux  = spectra[*,1,i]
        cerror = spectra[*,2,i]
        flag   = spectra[*,3,i]
        
        case state.p.spectype of 
           
           'Flux': spec = cflux
           
           'Uncertainty': spec = cerror
           
           'S/N': spec = cflux/cerror

        endcase

        if state.p.normspec then spec = spec/median(spec,/EVEN)
        
        color = (i mod 2) ? 1:3
        oplot,wave,spec,COLOR=color,PSYM=10

        lwave = mean((*state.p.wranges)[*,i])
        xy = convert_coord(lwave,!y.crange[1],/DATA,/TO_DEVICE)

        xyouts,xy[0],xy[1]+10,string((*state.r.orders)[i],FORMAT='(I2.2)'), $
               /DEVICE,COLOR=color,ALIGNMENT=0.5,CHARSIZE=state.p.charsize

        if state.p.plotsatpixel then begin
           
           mask = mc_bitset(fix(flag),0,CANCEL=cancel)
           z = where(mask eq 1,cnt)
           plotsym,0,0.9,/FILL
           if cnt ne 0 then oplot,wave[z],spec[z],PSYM=8,COLOR=2
           
        endif
                
        if state.p.plotreplacepixel then begin
           
           mask = mc_bitset(fix(flag),1,CANCEL=cancel)
           z = where(mask eq 1,cnt)
           plotsym,0,1.0,/FILL
           if cnt ne 0 then oplot,wave[z],spec[z],PSYM=8,COLOR=4
           
        endif

        if state.p.plotfixpixel then begin
           
           mask = mc_bitset(fix(flag),2,CANCEL=cancel)
           z = where(mask eq 1,cnt)
           plotsym,0,1.1,/FILL
           if cnt ne 0 then oplot,wave[z],spec[z],PSYM=8,COLOR=7
           
        endif
        
        if state.p.plotoptfail then begin
           
           mask = mc_bitset(fix(flag),3,CANCEL=cancel)
           z = where(mask eq 1,cnt)
           plotsym,0,1.2,/FILL
           if cnt ne 0 then oplot,wave[z],spec[z],PSYM=8,COLOR=5
           
        endif
        
     endfor

     if !y.crange[0] lt 0 then plots,!x.crange,[0,0],LINESTYLE=1
     
  endif

  state.p.xscale = !x
  state.p.yscale = !y
  state.p.pscale = !p
  
       
end
;
;******************************************************************************
;
pro xvspec_plotupdate

  common xvspec_state
  
  wset, state.p.pixmap_wid
  erase,COLOR=20
  if not state.p.fix then xvspec_getminmax
  xvspec_plotspec
  
  wset, state.p.plotwin_wid
  device, COPY=[0,0,state.p.plotwin_size[0],state.p.plotwin_size[1],0,0,$
                state.p.pixmap_wid]
  
end
;
;******************************************************************************
;
pro xvspec_smoothspectra

  common xvspec_state

  if state.r.fwhm eq 0 then begin

     *state.d.apspectra = *state.d.aspectra
     *state.d.bpspectra = *state.d.bspectra
     
  endif else begin

     for i = 0,state.r.norders-1 do begin
        
        for j = 0,state.r.naps-1 do begin

           y = (*state.d.aspectra)[*,1,i*state.r.naps+j]
           e = (*state.d.aspectra)[*,2,i*state.r.naps+j]
           x = findgen(n_elements(y))

           mc_convolvespec,x,y,state.r.fwhm,ny,oe,ERROR=e,CANCEL=cancel
           if cancel then return

           (*state.d.apspectra)[*,1,i*state.r.naps+j] = ny
           (*state.d.apspectra)[*,2,i*state.r.naps+j] = oe

           if state.p.nbuffers eq 2 then begin

              y = (*state.d.bspectra)[*,1,i*state.r.naps+j]
              e = (*state.d.bspectra)[*,2,i*state.r.naps+j]
              x = findgen(n_elements(y))
              
              mc_convolvespec,x,y,state.r.fwhm,ny,oe,ERROR=e,CANCEL=cancel
              if cancel then return
              
              (*state.d.bpspectra)[*,1,i*state.r.naps+j] = ny
              (*state.d.bpspectra)[*,2,i*state.r.naps+j] = oe             

           endif
           
        endfor
        
     endfor
     
  endelse 

end
;
; *****************************************************************************
;
pro xvspec_writefile,FITS=fits,ASCII=ascii

  common xvspec_state

  filename = mc_cfld(state.w.tmp_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return
  
;  Get hdr info
  
  hdr = *state.d.hdr

  if *state.p.funits ne '' then begin
  
     ytitle = mc_getfunits((*state.p.funits)[0],UNITS=yunits,CANCEL=cancel)
     if cancel then return

     fxaddpar,hdr,'YUNITS',yunits
     fxaddpar,hdr,'YTITLE',ytitle

  endif

  if *state.p.wunits ne '' then begin
  
     mc_getwunits,(*state.p.wunits)[0],xunits,xtitle,CANCEL=cancel
     if cancel then return
     
     fxaddpar,hdr,'XUNITS',xunits
     fxaddpar,hdr,'XTITLE',xtitle

  endif
  
  if keyword_set(FITS) then begin
     
     writefits,state.r.path+filename+'.fits',*state.d.pspectra,hdr

  endif

  if keyword_set(ASCII) then begin

     npix = long(fxpar(hdr,'NAXIS1'))
     
     openw,lun,state.r.path+filename+'.txt', /GET_LUN
     
     for j = 0, n_elements(hdr)-1 do printf, lun, '#'+hdr[j]
     
     for j = 0L, npix[0]-1L do begin
        
        printf, lun,  strjoin( reform((*state.d.pspectra)[j,*,*], $
                                      4*state.r.naps*state.r.norders),'  ' )
        
     endfor

     free_lun, lun     

  endif

  state.r.filenamepanel = 0
  
  widget_control, state.w.xvspec_base,UPDATE=0
  widget_control, state.w.panel, /DESTROY
  
  xvspec_modwinsize,/DESTROY
  xvspec_plotupdate        
  
end
;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro xvspec_event,event

  common xvspec_state

  widget_control, event.id,  GET_UVALUE=uvalue
  widget_control, /HOURGLASS

  case uvalue of

     'Aperture Menu': begin

        for i = 0,state.r.naps-1 do widget_control, (*state.w.but_ap)[i], $
           SET_BUTTON=0

        z = where(*state.w.but_ap eq event.id)
        widget_control, (*state.w.but_ap)[z],/SET_BUTTON
        state.p.ap = z[0]
        xvspec_plotupdate

     end

     'Buffer A Menu': begin

        widget_control, state.w.but_buffera,/SET_BUTTON
        widget_control, state.w.but_bufferb,SET_BUTTON=0
        state.p.buffer=0
        xvspec_plotupdate
        
     end

     'Buffer B Menu': begin

        widget_control, state.w.but_bufferb,/SET_BUTTON
        widget_control, state.w.but_buffera,SET_BUTTON=0
        state.p.buffer=1
        xvspec_plotupdate
        
     end
     
     'Continuous Plot Button': begin

        widget_control, state.w.but_ladder,SET_BUTTON=0
        widget_control, state.w.but_continuous,SET_BUTTON=1
        widget_control, state.w.mbut_continuous, /SET_BUTTON
        state.p.mode = 'Continuous'
        xvspec_resize

     end

     
     'Done Smoothing Button': begin
        
        state.r.smoothingpanel = 0

        widget_control, state.w.but_smooth,SET_BUTTON=0
        widget_control, state.w.xvspec_base,UPDATE=0
        widget_control, state.w.panel, /DESTROY
       
        xvspec_modwinsize,/DESTROY
        xvspec_plotupdate        

     end

     'Close Write Button': begin
        
        state.r.filenamepanel = 0

        widget_control, state.w.xvspec_base,UPDATE=0
        widget_control, state.w.panel, /DESTROY
       
        xvspec_modwinsize,/DESTROY
        xvspec_plotupdate        

     end

     
     'Gaussian FWHM': begin

        fwhm = mc_cfld(state.w.tmp_fld,3,/EMPTY,CANCEL=cancel)
        if cancel then return
        
        state.r.fwhm = fwhm
        xvspec_smoothspectra
        xvspec_plotupdate

     end

     'Fixed Pixel Menu': begin

        state.p.plotfixpixel = ~state.p.plotfixpixel
        widget_control, state.w.but_flags[2],SET_BUTTON=state.p.plotfixpixel
        xvspec_plotupdate
        
     end
     
     'Fix Y Range Button': begin

        state.p.fix = (state.p.fix eq 1) ? 0:1
        widget_control, state.w.but_fixyrange,SET_BUTTON=state.p.fix
        xvspec_plotupdate

     end

     'Flux Units Menu': begin

        for i = 0,6 do widget_control, state.w.but_fluxu[i],SET_BUTTON=0

        z = where(state.w.but_fluxu eq event.id)
        widget_control, state.w.but_fluxu[z],/SET_BUTTON

        *state.p.funits = [*state.p.funits,z]
        xvspec_convertflux
        xvspec_plotupdate

     end

     'Help Button': begin

        xmc_displaytext,filepath('xvspec_helpfile.txt', $
                                 ROOT_DIR=state.r.packagepath, $
                                 SUBDIR='helpfiles'), $
                        TITLE='xvspec Help File', $
                        GROUP_LEADER=state.w.xvspec_base, $
                        WSIZE=[600,400]

     end

     'Ladder Plot Button': begin
        
        widget_control, state.w.but_ladder,SET_BUTTON=1
        widget_control, state.w.but_continuous,SET_BUTTON=0
        widget_control, state.w.mbut_ladder, /SET_BUTTON
        state.p.mode = 'Ladder'
        xvspec_resize

     end


     'Load Spextool FITS Button': begin

        fullpath = dialog_pickfile(DIALOG_PARENT=state.w.xvspec_base,$
                                   FILTER='*.fits;*.dat;*.txt', $
                                   PATH=state.r.path,GET_PATH=newpath, $
                                   /MUST_EXIST)
        state.r.path = newpath

        if fullpath eq '' then goto, cont else begin
           
           xvspec_loadspec,fullpath
           xvspec_plotupdate
           
        endelse
        
     end

     'Normalize Spectra Button': begin

        state.p.normspec = ~state.p.normspec
        widget_control, state.w.but_normspec, SET_BUTTON=state.p.normspec
        xvspec_plotupdate
        
     end
     
     'Order Menu': begin

        if state.p.mode eq 'Ladder' then begin
        
           if state.p.plotwin_size[1] eq state.p.scroll_size[1] then goto, cont
           
           for i = 0,state.r.norders-1 do widget_control, (*state.w.but_ord)[i], $
              SET_BUTTON=0
           
           z = where(*state.w.but_ord eq event.id)
           
           del = max(*state.r.orders,MIN=min)-min+1
           offset = state.p.plotwin_size[1]/float((del+1))
           frac = ((*state.r.orders)[z]-min)/float(del)
           
           widget_control, state.w.plotwin, $
                           SET_DRAW_VIEW=[0,state.p.plotwin_size[1]*frac-offset]

        end

        if state.p.mode eq 'Continuous' then begin

           if state.p.plotwin_size[0] eq state.p.scroll_size[0] then goto, cont

           z = where(*state.w.but_ord eq event.id)
           
           wave = mean((*state.p.wranges)[*,z])
           xy = convert_coord(wave,1,/DATA,/TO_DEVICE)

           widget_control, state.w.plotwin, $
                           SET_DRAW_VIEW=[xy[0]-state.p.scroll_size[0]/2.,0]
           
        end
           
     end

     'Opt Extract Fail Menu': begin

        state.p.plotoptfail = ~state.p.plotoptfail
        widget_control, state.w.but_flags[3],SET_BUTTON=state.p.plotoptfail
        xvspec_plotupdate
        
     end
    
     'Plot Atmosphere Button': begin

        state.p.plotatmosphere = ~state.p.plotatmosphere
        widget_control, state.w.but_plotatmos, SET_BUTTON=state.p.plotatmosphere
        xvspec_plotupdate
        
     end

     
     'Plot Flux Button': begin

        widget_control, state.w.but_flx,SET_BUTTON=1
        widget_control, state.w.but_unc,SET_BUTTON=0
        widget_control, state.w.but_snr,SET_BUTTON=0
        widget_control, state.w.mbut_flx,/SET_BUTTON
        state.p.spectype = 'Flux'
        xvspec_plotupdate

     end

     'Plot S/N Button': begin

        widget_control, state.w.but_flx,SET_BUTTON=0
        widget_control, state.w.but_unc,SET_BUTTON=0
        widget_control, state.w.but_snr,SET_BUTTON=1
        widget_control, state.w.mbut_snr,/SET_BUTTON
        state.p.spectype = 'S/N'        
        xvspec_plotupdate

     end

     'Plot Uncertainty Button': begin

        widget_control, state.w.but_flx,SET_BUTTON=0
        widget_control, state.w.but_unc,SET_BUTTON=1
        widget_control, state.w.but_snr,SET_BUTTON=0
        widget_control, state.w.mbut_unc,/SET_BUTTON
        state.p.spectype = 'Uncertainty'        
        xvspec_plotupdate

     end

     'Replaced Pixel Menu': begin

        state.p.plotreplacepixel = ~state.p.plotreplacepixel
        widget_control, state.w.but_flags[1],SET_BUTTON=state.p.plotreplacepixel
        xvspec_plotupdate
        
     end
     
     'Saturated Pixel Menu': begin

        state.p.plotsatpixel = ~state.p.plotsatpixel
        widget_control, state.w.but_flags[0],SET_BUTTON=state.p.plotsatpixel
        xvspec_plotupdate
        
     end
     
     'Smooth Button': begin

        if state.r.smoothingpanel then return

        state.r.smoothingpanel = 1

        widget_control, state.w.but_smooth,/SET_BUTTON
        widget_control, state.w.xvspec_base,UPDATE=0
        widget_control, state.w.plotwin,/DESTROY

        state.w.panel = widget_base(state.w.xvspec_base,$
                                  /BASE_ALIGN_CENTER,$
                                  FRAME=2,$
                                  /ROW)

           fld = coyote_field2(state.w.panel,$
                               LABELFONT=state.w.buttonfont,$
                               FIELDFONT=state.w.textfont,$
                               TITLE='Gaussian FWHM:',$
                               UVALUE='Gaussian FWHM',$
                               VALUE=state.r.fwhm,$
                               XSIZE=10,$
                               EVENT_PRO='xvspec_event',$
                               /CR_ONLY,$
                               TEXTID=textid)
           state.w.tmp_fld = [fld,textid]

           button = widget_button(state.w.panel,$
                                  FONT=state.w.buttonfont,$
                                  EVENT_PRO='xvspec_event',$
                                  VALUE=' Done ',$
                                  UVALUE='Done Smoothing Button')

        xvspec_modwinsize
        xvspec_plotupdate        
        
     end

     'Quit': widget_control, event.top, /DESTROY

     'View FITS Header Button': begin

        if n_elements(*state.d.hdr) le 1 then begin
           
           ok = dialog_message('No FITS header.',/INFO, $
                               DIALOG_PARENT=state.w.xvspec_base)
           
           
        endif else begin
           
           xmc_displaytext,*state.d.hdr,TITLE='FITS Header', $
                           GROUP_LEADER=state.w.xvspec_base
           
        endelse
        
     end

     'Wavelength Units Menu': begin

        for i = 0,2 do widget_control, state.w.but_waveu[i],SET_BUTTON=0

        z = where(state.w.but_waveu eq event.id)
        widget_control, state.w.but_waveu[z],/SET_BUTTON

        *state.p.wunits = [*state.p.wunits,z]
        xvspec_convertwave
        xvspec_plotupdate

     end

     'Write ASCII File': begin

        if state.r.filenamepanel then return

        state.r.filenamepanel = 1

        widget_control, state.w.xvspec_base,UPDATE=0
        widget_control, state.w.plotwin,/DESTROY
        
        state.w.panel = widget_base(state.w.xvspec_base,$
                                    /BASE_ALIGN_CENTER,$
                                    FRAME=2,$
                                    /ROW)
        
        fld = coyote_field2(state.w.panel,$
                            LABELFONT=state.w.buttonfont,$
                            FIELDFONT=state.w.textfont,$
                            TITLE='Filename (sans suffix):',$
                            UVALUE='Write ASCII Filename',$
                            XSIZE=15,$
                            EVENT_PRO='xvspec_event',$
                            /CR_ONLY,$
                            TEXTID=textid)
        state.w.tmp_fld = [fld,textid]

        button = widget_button(state.w.panel,$
                               FONT=state.w.buttonfont,$
                               EVENT_PRO='xvspec_event',$
                               VALUE=' Close ',$
                               UVALUE='Close Write Button')
        
        xvspec_modwinsize
        xvspec_plotupdate
        mc_setfocus,state.w.tmp_fld

     end

     'Write ASCII Filename': xvspec_writefile,/ASCII

     'Write Spextool FITS File': begin

        if state.r.filenamepanel then return

        state.r.filenamepanel = 1
        widget_control, state.w.xvspec_base,UPDATE=0
        widget_control, state.w.plotwin,/DESTROY
        
        state.w.panel = widget_base(state.w.xvspec_base,$
                                    /BASE_ALIGN_CENTER,$
                                    FRAME=2,$
                                    /ROW)
        
        fld = coyote_field2(state.w.panel,$
                            LABELFONT=state.w.buttonfont,$
                            FIELDFONT=state.w.textfont,$
                            TITLE='Filename (sans suffix):',$
                            UVALUE='Write FITS Filename',$
                            XSIZE=15,$
                            EVENT_PRO='xvspec_event',$
                            /CR_ONLY,$
                            TEXTID=textid)
        state.w.tmp_fld = [fld,textid]

        button = widget_button(state.w.panel,$
                               FONT=state.w.buttonfont,$
                               EVENT_PRO='xvspec_event',$
                               VALUE=' Close ',$
                               UVALUE='Close Write Button')
        
        xvspec_modwinsize
        xvspec_plotupdate
        mc_setfocus,state.w.tmp_fld

     end

     'Write FITS Filename': xvspec_writefile,/FITS
             
     'X Log Button': begin

        state.p.xlog = (state.p.xlog eq 1) ? 0:1
        widget_control, state.w.but_xlog,SET_BUTTON=state.p.xlog
        xvspec_plotupdate

     end

     'Y Log Button': begin

        state.p.ylog = (state.p.ylog eq 1) ? 0:1
        widget_control, state.w.but_ylog,SET_BUTTON=state.p.ylog
        xvspec_plotupdate

     end

     else:
     
  endcase
  
cont:
  
end
;
;******************************************************************************
;
pro xvspec_plotwinevent,event

  common xvspec_state

  widget_control, event.id,  GET_UVALUE=uvalue
  
  if strtrim(tag_names(event,/STRUCTURE_NAME),2) eq 'WIDGET_TRACKING' then begin
     
     widget_control, state.w.plotwin,SENSITIVE=event.enter, $
                     INPUT_FOCUS=event.enter
     return

  endif

  if event.release ne 1 then return

;  Check for keyboard event

  if strtrim(event.ch,2) ne '' then begin
     
     case strtrim(event.ch,2) of 

        'a': begin

           widget_control, state.w.but_buffera,/SET_BUTTON
           widget_control, state.w.but_bufferb,SET_BUTTON=0
           state.p.buffer=0
           xvspec_plotupdate
           
        end

        'b': begin

           if state.p.nbuffers eq 2 then begin
              
              widget_control, state.w.but_bufferb,/SET_BUTTON
              widget_control, state.w.but_buffera,SET_BUTTON=0
              state.p.buffer=1
              xvspec_plotupdate

           endif
              
        end
        
        'f': begin

           widget_control, state.w.but_flx,SET_BUTTON=1
           widget_control, state.w.but_unc,SET_BUTTON=0
           widget_control, state.w.but_snr,SET_BUTTON=0
           widget_control, state.w.mbut_flx,/SET_BUTTON
           state.p.spectype = 'Flux'
           xvspec_plotupdate

        end

        'u': begin

           widget_control, state.w.but_flx,SET_BUTTON=0
           widget_control, state.w.but_unc,SET_BUTTON=1
           widget_control, state.w.but_snr,SET_BUTTON=0
           widget_control, state.w.mbut_unc,/SET_BUTTON
           state.p.spectype = 'Uncertainty'        
           xvspec_plotupdate

        end

        'q': widget_control, event.top, /DESTROY

        's': begin

           widget_control, state.w.but_flx,SET_BUTTON=0
           widget_control, state.w.but_unc,SET_BUTTON=0
           widget_control, state.w.but_snr,SET_BUTTON=1
           widget_control, state.w.mbut_snr,/SET_BUTTON
           state.p.spectype = 'S/N'        
           xvspec_plotupdate
           
        end

        else:
        
     endcase
     return
     
  endif 
  
;  Launch to xzoomplot

  if state.p.mode eq 'Ladder' then begin
     
     idx = floor(event.y/float(state.p.plotwin_size[1])*state.r.norders)
     idx = idx * state.r.naps + (state.p.ap)
     
  endif else begin

     wset, state.p.plotwin_wid 
     !p = state.p.pscale
     !x = state.p.xscale
     !y = state.p.yscale
             
     mean = total((*state.p.wranges),1)/2.
     xy = convert_coord(event.x,event.y,/DEVICE,/TO_DATA)
     del = abs(mean-xy[0])

     idx = where(del eq min(del))

     
  endelse

  case state.p.buffer of

     0: begin

        cwave  = (*state.d.apspectra)[*,0,idx]
        cflux  = (*state.d.apspectra)[*,1,idx]
        cerror = (*state.d.apspectra)[*,2,idx]
        
     end

     1: begin

        cwave  = (*state.d.bpspectra)[*,0,idx]
        cflux  = (*state.d.bpspectra)[*,1,idx]
        cerror = (*state.d.bpspectra)[*,2,idx]
        
     end

  endcase
  
  case state.p.spectype of 
     
     'Flux': begin
        
        spec   = cflux
        xtitle = state.p.xtitle
        ytitle = state.p.ytitle[0]
        
     end
     
     'Uncertainty': begin
        
        spec   = cerror
        xtitle = state.p.xtitle
        ytitle = state.p.ytitle[1]
        
     end
     
     'S/N': begin
        
        spec   = cflux/cerror
        xtitle = state.p.xtitle
        ytitle = '!5S/N'
        
     end
     
  endcase
  
  xzoomplot,cwave,spec,XTITLE=xtitle,YTITLE=ytitle,YLOG=state.p.ylog
  
  widget_control, state.w.plotwin,/INPUT_FOCUS

end
;
;******************************************************************************
;
pro xvspec_resize, event

  common xvspec_state

  if n_params() eq 0 then begin
  
     size = widget_info(state.w.xvspec_base, /GEOMETRY)
     xsize = size.xsize
     ysize = size.ysize
     
  endif else begin

     widget_control, state.w.xvspec_base, TLB_GET_SIZE=size
     xsize = size[0]
     ysize = size[1]
     
  endelse
   
  if state.p.mode eq 'Ladder' then begin
  
     state.p.plotwin_size[0] = xsize-state.p.winbuffer[0]
     state.p.scroll_size[0]  = state.p.plotwin_size[0]
     
     state.p.scroll_size[1]  = ysize-state.p.winbuffer[1]
     state.p.plotwin_size[1] = state.p.scroll_size[1] > $
                               state.p.pixpp*state.r.norders


  endif

  if state.p.mode eq 'Continuous' then begin

     state.p.plotwin_size[1] = ysize-state.p.winbuffer[1]
     state.p.scroll_size[1]  = state.p.plotwin_size[1]
          
     state.p.scroll_size[0]  = xsize-state.p.winbuffer[0]
     state.p.plotwin_size[0] = state.p.scroll_size[0] > $
                               state.p.pixpp*state.r.norders
     
  endif

  xvspec_modwinsize,/DESTROY
  if not state.p.freeze then xvspec_plotupdate
  
end
;
;******************************************************************************
;
;------------------------------Main Program------------------------------------
;
;******************************************************************************
;
pro xvspec,afile,bfile,POSITION=position,FWHM=fwhm,VMODE=vmode, $
           PLOTLINMAX=plotlinmax,PLOTREPLACE=plotreplace,PLOTFIX=plotfix, $
           PLOTOPTFAIL=plotoptfail,PLOTATMOSPHERE=plotatmosphere, $
           GROUP_LEADER=group_leader,CANCEL=cancel
  
  if n_params() eq 1 then begin
     
     cancel = mc_cpar('xvspec',afile,1,'AFile',7,0)
     if cancel then return
     afile = mc_cfile(afile,CANCEL=cancel)    
     if cancel then return
     
  endif

  if n_params() eq 2 then begin
     
     cancel = mc_cpar('xvspec',bfile,1,'BFile',7,0)
     if cancel then return
     bfile = mc_cfile(bfile,CANCEL=cancel)    
     if cancel then return
     
  endif
  
  if not xregistered('xvspec') then begin
     
     xvspec_startup,POSITION=position, FWHM=fwhm, GROUP_LEADER=group_leader,$
                    CANCEL=cancel
     if cancel then return

  endif

  common xvspec_state

  if n_elements(PLOTLINMAX) ne 0 and ~keyword_set(PLOTLINMAX) then $
     state.p.plotsatpixel = 0

  if n_elements(PLOTREPLACE) ne 0 and ~keyword_set(PLOTREPLACE) then $
     state.p.plotreplacepixel = 0

  if n_elements(PLOTFIX) ne 0 and ~keyword_set(PLOTFIX) then $
     state.p.plotfixpixel = 0

  if n_elements(PLOTOPTFAIL) ne 0 and ~keyword_set(PLOTOPTFAIL) then $
     state.p.plotoptfail = 0

  state.p.plotatmosphere = keyword_set(PLOTATMOSPHERE)
  
  if n_elements(VMODE) eq 0 then vmode = 'Ladder'

  case strlowcase(vmode) of

     'ladder': begin
        
        state.p.mode = 'Ladder'
        widget_control, state.w.but_ladder, /SET_BUTTON
        widget_control, state.w.mbut_ladder, /SET_BUTTON
        
     end

     'continuous': begin
        
        state.p.mode = 'Continuous'
        widget_control, state.w.but_continuous, /SET_BUTTON
        widget_control, state.w.mbut_continuous, /SET_BUTTON
        
     end

     else: begin  

        state.p.mode = 'Ladder'
        widget_control, state.w.but_ladder, /SET_BUTTON
        widget_contro, state.w.mbut_ladder,/SET_BUTTON
        
     end

  endcase
     
  widget_control, state.w.but_flags[0],SET_BUTTON=state.p.plotsatpixel
  widget_control, state.w.but_flags[1],SET_BUTTON=state.p.plotreplacepixel
  widget_control, state.w.but_flags[2],SET_BUTTON=state.p.plotfixpixel
  widget_control, state.w.but_flags[3],SET_BUTTON=state.p.plotoptfail
  widget_control, state.w.but_plotatmos,SET_BUTTON=state.p.plotatmosphere
  
  xvspec_loadspec,afile,bfile,CANCEL=cancel
  if cancel then return
  xvspec_plotupdate

end
