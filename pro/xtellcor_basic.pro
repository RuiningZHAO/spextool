;+
; NAME:
;     xtellcor_basic
;
; PURPOSE:
;     Telluric corrects SpeX spectra by simply dividing by the std star.
;    
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     xtellcor_basic
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
;     Write spectral FITS file to disk
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
;     None
;
; PROCEDURE:
;     Divide by standard and multiple by blackbody
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2002 - Written by M. Cushing, Institute for Astronomy, UH
;-

;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro xtellcor_basic_event,event

  widget_control, event.id,  GET_UVALUE = uvalue

  if uvalue eq 'Quit' then begin
     
     widget_control, event.top, /DESTROY
     goto, getout
     
  endif
  
  
  widget_control, event.top, GET_UVALUE = state, /NO_COPY
  widget_control, /HOURGLASS
  
  case uvalue of
     
     'Construct Spectra': xtellcor_basic_tellcor,state
     
     'Get Shift': xtellcor_basic_getshift,state

     'Help': begin
        
        pre = (strupcase(!version.os_family) eq 'WINDOWS') ? 'start ':'open '
        
        spawn, pre+filepath('spextoolmanual.pdf', $
                            ROOT=state.r.spextoolpath,$
                            SUBDIR='manuals')
        
     end
     
     'Load Spectra Button': xtellcor_basic_loadspec,state
     
     'Object Spectra Button': begin
        
        obj = dialog_pickfile(DIALOG_PARENT=state.w.xtellcor_basic_base,$
                              PATH=state.r.objpath,/MUST_EXIST, $
                              GET_PATH=path,FILTER='*.fits')
        if obj eq '' then goto, cont
        widget_control,state.w.objspectra_fld[1],SET_VALUE=file_basename(obj)
        state.r.objpath = path
        mc_setfocus,state.w.objspectra_fld
        
     end
       
     'Restore Continuum': begin
        
        state.r.restore = event.value
        if event.value eq 'Yes' then begin
           
           widget_control, state.w.temp_base, MAP=1
           widget_control, state.w.dl_base, MAP=1
           
        endif

        if event.value eq 'No'  then begin
           
           widget_control, state.w.temp_base, MAP=0
           widget_control, state.w.dl_base, MAP=0
           
        endif
        
     end
     
     'Shift Object Ap': begin

        state.r.shiftobjap = event.index
        z = where(*state.d.objorders eq state.r.shiftobjorder)
        widget_control, state.w.shift,SET_VALUE='Shift: '+ $
                        string((*state.r.shifts)[z,state.r.shiftobjap], $
                               FORMAT='(f6.2)')
     end

     'Shift Object Order': begin

        state.r.shiftobjorder = (*state.d.objorders)[event.index]
        widget_control, state.w.shift,SET_VALUE='Shift: '+ $
                        string((*state.r.shifts)[event.index, $
                                                state.r.shiftobjap], $
                               FORMAT='(f6.2)')     
     end


     'Spectrum Units': begin
        
        case event.index of 
           
           0: state.r.units = 'ergs s-1 cm-2 A-1'
           1: state.r.units = 'ergs s-1 cm-2 Hz-1'
           2: state.r.units = 'W m-2 um-1'
           3: state.r.units = 'W m-2 Hz-1'
           4: state.r.units = 'Jy'
           
        endcase 
        
     end
     
     'Standard Spectra Button': begin
        
        std = dialog_pickfile(DIALOG_PARENT=state.w.xtellcor_basic_base,$
                              PATH=state.r.stdpath,/MUST_EXIST, $
                              GET_PATH=path,FILTER='*.fits')
        if std eq '' then goto, cont
        widget_control,state.w.stdspectra_fld[1],SET_VALUE=file_basename(std)
        state.r.stdpath = path
        mc_setfocus,state.w.stdspectra_fld
        
     end
     
  endcase
  
;  Put state variable into the user value of the top level base.
  
  cont: 
  widget_control, state.w.xtellcor_basic_base, SET_UVALUE=state, /NO_COPY
  getout:

end
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xtellcor_basic_changeunits,wave,spec,spec_error,state

c = 2.99792458e+8
ang = '!5!sA!r!u!9 %!5!n'

case state.r.units of 

    'ergs s-1 cm-2 A-1': state.r.nytitle = $
      '!5f!D!7k!N!5 (ergs s!E-1!N cm!E-2!N '+ang+'!E-1!N)' 

    'ergs s-1 cm-2 Hz-1': begin

        spec            = temporary(spec)* wave^2 * (1.0e-2 / c)
        spec_error      = temporary(spec_error)* wave^2 * (1.0e-2 / c)
        state.r.nytitle = '!5f!D!7m!N!5 (ergs s!E-1!N cm!E-2!N Hz!E-1!N)'

    end
    'W m-2 um-1': begin 

        spec            = temporary(spec)*10.
        spec_error      = temporary(spec_error)*10.
        state.r.nytitle = '!5f!D!7k!N!5 (W m!E-2!N !7l!5m!E-1!N)'
            
    end
    'W m-2 Hz-1': begin

        spec            = temporary(spec)* wave^2 * (1.0e-5 / c)
        spec_error      = temporary(spec_error)* wave^2 * (1.0e-5 / c)
        state.r.nytitle = '!5f!D!7m!N!5 (W m!E-2!N Hz!E-1!N)' 

    end
    
    'Jy': begin

        spec            = temporary(spec)* wave^2 * (1.0e21 / c) 
        spec_error      = temporary(spec_error)* wave^2 * (1.0e21 / c) 
        state.r.nytitle = '!5f!D!7m!N!5 (Jy)' 

    end

endcase

end
;
;******************************************************************************
;
pro xtellcor_basic_cleanup,base

widget_control, base, GET_UVALUE = state, /NO_COPY
if n_elements(state) ne 0 then begin

    ptr_free, state.r.shifts
    ptr_free, state.d.awave
    ptr_free, state.d.atrans

endif
state = 0B

end
;
;******************************************************************************
;
pro xtellcor_basic_getshift,state

  zobj = where((*state.d.objorders) eq state.r.shiftobjorder)
  
  obj_wave = (*state.d.obj)[*,0,zobj*state.d.objnaps+state.r.shiftobjap]
  obj_flux = (*state.d.obj)[*,1,zobj*state.d.objnaps+state.r.shiftobjap]
  
  trim = mc_nantrim(obj_wave,2)
  obj_wave = obj_wave[trim]
  obj_flux = obj_flux[trim]


  zstd = where((*state.d.stdorders) eq state.r.shiftobjorder)
  
  tel_wave = (*state.d.std)[*,0,zstd]
  tel_flux = 1./(*state.d.std)[*,1,zstd]
  
  trim = mc_nantrim(tel_wave,2)
  tel_wave = tel_wave[trim]
  tel_flux = tel_flux[trim]
  
  mc_interpspec,tel_wave,tel_flux,obj_wave,new_flux,CANCEL=cancel
  if cancel then return

  shift = xmc_findshift(obj_wave,obj_flux,new_flux,*state.d.awave,*state.d.atrans,$
                        INITSHIFT=total((*state.r.shifts)[zobj,state.r.shiftobjap]),$
                        XTITLE=state.r.xtitle,$
                        PARENT=state.w.xtellcor_basic_base,CANCEL=cancel)
  
  if not cancel then begin

     (*state.r.shifts)[zobj,state.r.shiftobjap] = shift
     widget_control, state.w.shift,SET_VALUE='Shift: '+string(shift,FORMAT='(f6.2)') 
  
  endif


end
;
;******************************************************************************
;
pro xtellcor_basic_loadspec,state

;  Get files.

  stdfile = mc_cfld(state.w.stdspectra_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return
  stdfile = mc_cfile(state.r.stdpath+stdfile, $
                     WIDGET_ID=state.w.xtellcor_basic_base,$
                     CANCEL=cancel)
  if cancel then return
  
  objfile = mc_cfld(state.w.objspectra_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return
  objfile = mc_cfile(state.r.stdpath+objfile, $
                     WIDGET_ID=state.w.xtellcor_basic_base,CANCEL=cancel)
  if cancel then return
  
  mc_readspec,stdfile,std,stdhdr,stdobsmode,start,stop,stdnorders,stdnaps,$
              stdorders,stdxunits,stdyunits,slith_pix,slith_arc,slitw_pix, $
              slitw_arc,stdrp,stdairmass,stdxtitle,CANCEL=cancel
  if cancel then return
  std = mc_caaspecflags(std,CANCEL=cancel)
  if cancel then return
  
  mc_readspec,objfile,obj,objhdr,objobsmode,start,stop,objnorders,objnaps,$
              objorders,objxunits,objyunits,slith_pix,slith_arc,slitw_pix, $
              slitw_arc,objrp,objairmass,CANCEL=cancel
  if cancel then return
  obj = mc_caaspecflags(obj,CANCEL=cancel)
  if cancel then retunr


;  Check to see if objairmass is zero.  If so, search for TC_AM
;  instead.  This is because the AIRMASS keyword was changed, and if
;  someone does a telluric correction on a single spectrum instead of
;  a spectr run through xcombspec, then the airmass difference will be
;  zero.  This is the best of the bad choices on how to fix this.

  if objairmass eq 0.0 then objairmass = fxpar(objhdr,'TCS_AM')
  
  *state.d.stdorders = stdorders
  *state.d.objorders = objorders
  state.d.objnaps    = objnaps
  *state.d.std       = std
  *state.d.obj       = obj
  *state.d.objhdr    = objhdr
  *state.d.stdhdr    = stdhdr
  state.r.xtitle     = stdxtitle
  
  state.r.shiftobjap = 0
  state.r.shiftobjorder = objorders[0]
  *state.r.shifts = fltarr(objnorders,objnaps)

  if abs(stdairmass-objairmass) gt 0.1 then begin

     color = 17
     beep
     beep
     beep

  endif else color = 16

  wset, state.w.message
  erase,COLOR=color
  xyouts,10,8,'Std Airmass:'+string(stdairmass,FORMAT='(f7.4)')+ $
         ', Obj Airmass:'+string(objairmass,FORMAT='(f7.4)')+ $
         ', (Std-Obj) Airmass: '+ $
         string((stdairmass-objairmass),FORMAT='(f7.4)'),/DEVICE,$
         CHARSIZE=1.1,FONT=0
  
  widget_control, state.w.objorder_dl,$
                  SET_VALUE=string(objorders,FORMAT='(i2.2)')

  widget_control, state.w.objap_dl,$
                  SET_VALUE=string(indgen(objnaps)+1,FORMAT='(i2.2)')
  
;  Load atmosphere

  if objrp ge 10000.0 then trp = round(objrp/10000.)*10000
  if objrp lt 10000.0 and objrp ge 1000.0 then trp = round(objrp/1000.)*1000
  if objrp lt 1000.0 and objrp ge 100.0 then trp = round(objrp/100.)*100
  if objrp lt 100.0 and objrp ge 10.0 then trp = 100
  if objrp eq 0 then trp=2000

  spec = readfits(filepath('atran'+strtrim(trp,2)+'.fits', $
                           ROOT_DIR=state.r.spextoolpath,SUBDIR='data'))
  *state.d.awave = reform(spec[*,0])
  *state.d.atrans = reform(spec[*,1])


end
;
;******************************************************************************
;
pro xtellcor_basic_tellcor,state

  std = *state.d.std
  obj = *state.d.obj
  
  objnaps = state.d.objnaps
  stdorders = *state.d.stdorders
  objorders = *state.d.objorders
  objnorders = n_elements(objorders)
  
  objhdr = *state.d.objhdr
  stdhdr = *state.d.stdhdr
  
  
  stdfile = mc_cfld(state.w.stdspectra_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return
  
  oname = mc_cfld(state.w.objoname_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return
  
  if state.r.restore eq 'Yes' then begin
     
     temp = mc_cfld(state.w.temp_fld,4,/EMPTY,CANCEL=cancel)
     if cancel then return
     
     vmag = mc_cfld(state.w.vmag_fld,4,/EMPTY,CANCEL=cancel)
     if cancel then return
     
  endif 
  
  for i = 0,objnaps-1 do begin
     
     for j = 0,objnorders-1 do begin
        
        k = j*objnaps+i
        z = where(stdorders eq objorders[j],cnt)
        if cnt eq 0 then goto, cont
        
;  Interpolate telluric spectrum onto object wavelength sampling.
        
        mc_interpspec,std[*,0,z],std[*,1,z],obj[*,0,k],std_flux,std_error,$
                      IYERROR=std[*,2,z],CANCEL=cancel
        if cancel then return
        
        tell_flux  = 1.0/std_flux
        tell_error = (1.0/std_flux^2) * std_error
        
;  Shift the spectrum
        
        x = findgen(n_elements(std_flux))
        mc_interpspec,x+(*state.r.shifts)[j,i],tell_flux,x,stell_flux,$
                   stell_error,IYERROR=tell_error,CANCEL=cancel
        if cancel then return
        
        corspec       = obj[*,1,k]*stell_flux
        corspec_error = sqrt(stell_flux^2 * obj[*,2,k]^2 + $
                             obj[*,1,k]^2 * stell_error^2)
        
        if state.r.restore eq 'Yes' then begin
           
;  Flux calibrate the Blackbody
           
           bbflux_v = planck(5556,temp)
           scale = ( 3.46e-9*10^(-0.4*(vmag-0.03)) )/bbflux_v
           bbflux = PLANCK(obj[*,0,k]*10000.,temp)*scale
           
           corspec       = temporary(corspec) * bbflux
           corspec_error = temporary(corspec_error) * bbflux
           
        endif
        
        xtellcor_basic_changeunits,obj[*,0,k],corspec,corspec_error,state
        
        obj[*,1,k] = corspec
        obj[*,2,k] = corspec_error

     endfor
     
     cont:
     
  endfor

;  Get hdr and history down

  history = 'The spectra were divided by the spectra of the standard star '+$
            strtrim(stdfile,2)+'.'
    
  if state.r.restore eq 'Yes' then begin
          
     history = history+'  The spectra were also multiplied by a  '+$
               'blackbody with a temperature of '+strtrim(temp,2)+' K ' + $
               'scaled to a V-band magnitude of' + $
               ' '+string(vmag,FORMAT='(f6.3)')+'.'
     
     yunits = state.r.units
     ytitle = state.r.nytitle
     
  endif else begin 

     yunits = 'None'
     ytitle = '!5Ratio ()'
     history = history+' Therefore the spectrum is unitless.'

  endelse

  
;  Write the corrected spectrum to disk.
  
  hdr = objhdr
  
  fxaddpar,hdr,'IRAFNAME',strtrim(oname+'.fits',2)
  fxaddpar,hdr,'YUNITS',yunits, 'Units of the Y axis'
  fxaddpar,hdr,'YTITLE',ytitle, 'IDL Y Title'
  
  hdr = mc_addhistlabel(hdr,'Xtellcor Basic History',CANCEL=cancel)
  if cancel then return
  
  history = mc_splittext(history,70,CANCEL=cancel)
  if cancel then return
  sxaddhist,history,hdr
  
  writefits, state.r.objpath+oname+'.fits',obj,hdr
  xvspec,state.r.objpath+oname+'.fits'

end
;
;******************************************************************************
;
; ------------------------------Main Program---------------------------------- 
;
;******************************************************************************
;
;
pro xtellcor_basic

;  Get Spextool path
  
  spextoolpath = file_dirname( $
                 file_dirname(file_which('Spextool_Instruments.dat')),/MARK)

;  Set the fonts

  mc_getfonts,buttonfont,textfont,CANCEL=cancel
  if cancel then return
  
  w = {bmag_fld:[0L,0L],$
       dl_base:0L,$
       objap_dl:0L,$
       objoname_fld:[0L,0L],$
       objorder_dl:0L,$
       objspectra_fld:[0L,0L],$
       path_fld:[0L,0L],$
       shift:0L,$
       stdspectra_fld:[0L,0L],$
       temp_base:0L,$
       temp_fld:[0L,0L],$
       xtellcor_basic_base:0L,$
       message:0L,$
       vmag_fld:[0L,0L]}
  
  r = {fitsoutput:1,$
       nytitle:'',$
       objpath:'',$
       spextoolpath:spextoolpath,$
       stdpath:'',$
       restore:'Yes',$
       shiftobjorder:0,$
       shiftobjap:0,$
       shifts:ptr_new(2),$
       textoutput:0,$
       temp:0.,$
       xtitle:'',$
       units:'ergs s-1 cm-2 A-1'}
  
  d = {awave:ptr_new(2),$
       atrans:ptr_new(2),$
       std:ptr_new(fltarr(2)),$
       stdorders:ptr_new(fltarr(2)),$
       objorders:ptr_new(fltarr(2)),$
       obj:ptr_new(fltarr(2)),$
       objnaps:0,$
       objhdr:ptr_new(fltarr(2)),$
       stdhdr:ptr_new(fltarr(2))}
  
  state = {w:w,r:r,d:d}
  
  state.w.xtellcor_basic_base = widget_base(TITLE='Xtellcor (Basic)', $
                                            EVENT_PRO='xtellcor_basic_event',$
                                            /COLUMN)
  
     button = widget_button(state.w.xtellcor_basic_base,$
                            FONT=buttonfont,$
                            VALUE='Done',$
                            UVALUE='Quit')

     message = widget_draw(state.w.xtellcor_basic_base,$
                           FRAME=2,$
                           XSIZE=10,$
                           YSIZE=25)
         
     row_base = widget_base(state.w.xtellcor_basic_base,$
                            /ROW)

        col1_base = widget_base(row_base,$
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
              
                 but = widget_button(row,$
                                     FONT=buttonfont,$
                                     VALUE='Std Spectra',$
                                     UVALUE='Standard Spectra Button')
                 
                 std_fld = coyote_field2(row,$
                                         LABELFONT=buttonfont,$
                                         FIELDFONT=textfont,$
                                         TITLE=':',$
                                         UVALUE='Standard Spectra Field',$
                                         XSIZE=18,$
                                         /CR_ONLY,$
                                         TEXTID=textid)
                 state.w.stdspectra_fld = [std_fld,textid]
                 
              row = widget_base(box1_base,$
                                /ROW,$
                                /BASE_ALIGN_CENTER)
              
                 but = widget_button(row,$
                                     FONT=buttonfont,$
                                     VALUE='Obj Spectra',$
                                     UVALUE='Object Spectra Button')
                 
                 obj_fld = coyote_field2(row,$
                                         LABELFONT=buttonfont,$
                                         FIELDFONT=textfont,$
                                         TITLE=':',$
                                         UVALUE='Object Spectra Field',$
                                         XSIZE=18,$
                                         /CR_ONLY,$
                                         TEXTID=textid)
                 state.w.objspectra_fld = [obj_fld,textid]
                 
              button = widget_button(box1_base,$
                                     FONT=buttonfont,$
                                     VALUE='Load Raw Spectra',$
                                     UVALUE='Load Spectra Button')
              
           box2_base = widget_base(col1_base,$
                                   /COLUMN,$
                                   FRAME=2)
           
              label = widget_label(box2_base,$
                                   VALUE='2.  Determine Shift',$
                                   FONT=buttonfont,$
                                   /ALIGN_LEFT)
              
              row = widget_base(box2_base,$
                                /ROW,$
                                /BASE_ALIGN_CENTER)
              
                 state.w.objorder_dl = widget_droplist(row,$
                                                       FONT=buttonfont,$
                                                       TITLE='Order:',$
                                                       VALUE='01',$
                                                    UVALUE='Shift Object Order')
                 
                 state.w.objap_dl = widget_droplist(row,$
                                                    FONT=buttonfont,$
                                                    TITLE='Aperture:',$
                                                    VALUE='01',$
                                                    UVALUE='Shift Object Ap')
                 
              state.w.shift = widget_label(box2_base,$
                                           /ALIGN_LEFT,$
                                           /DYNAMIC_RESIZE,$
                                           FONT=buttonfont,$
                                           VALUE='Shift: 0.0')
              
              shift = widget_button(box2_base,$
                                    VALUE='Get Shift',$
                                    UVALUE='Get Shift',$
                                    FONT=buttonfont)
              
        col2_base = widget_base(row_base,$
                                /COLUMN)
        
           box3_base = widget_base(col2_base,$
                                   /COLUMN,$
                                   FRAME=2)
           
              label = widget_label(box3_base,$
                                   VALUE='3.  Restore Continuum',$
                                   FONT=buttonfont,$
                                   /ALIGN_LEFT)
              
              convolve_bg = cw_bgroup(box3_base,$
                                      FONT=buttonfont,$
                                      ['Yes','No'],$
                                      /ROW,$
                                      /RETURN_NAME,$
                                      /NO_RELEASE,$
                                      /EXCLUSIVE,$
                                      LABEL_LEFT='Restore Continuum:',$
                                      UVALUE='Restore Continuum',$
                                      SET_VALUE=0)
              
              state.w.temp_base = widget_base(box3_base,$
                                              /ROW,$
                                              /BASE_ALIGN_CENTER)
              
                 fld = coyote_field2(state.w.temp_base,$
                                     LABELFONT=buttonfont,$
                                     FIELDFONT=textfont,$
                                     TITLE='BB Temp:',$
                                     UVALUE='BB Temp Field',$
                                     XSIZE=5,$
                                     TEXTID=textid)
                 state.w.temp_fld = [fld,textid]         
                 
                 label = widget_label(state.w.temp_base,$
                                      VALUE='K, ',$
                                      FONT=buttonfont)
                 
                 fld = coyote_field2(state.w.temp_base,$
                                     LABELFONT=buttonfont,$
                                     FIELDFONT=textfont,$
                                     TITLE='V Mag:',$
                                     UVALUE='V Mag Field',$
                                     XSIZE=5,$
                                     TEXTID=textid)
                 state.w.vmag_fld = [fld,textid]         
                 
              state.w.dl_base = widget_base(box3_base,$
                                            /ROW,$
                                            /BASE_ALIGN_CENTER)
              
              value =['ergs s-1 cm-2 A-1','ergs s-1 cm-2 Hz-1',$
                      'W m-2 um-1','W m-2 Hz-1','Jy']
              units_dl = widget_droplist(state.w.dl_base,$
                                         FONT=buttonfont,$
                                         TITLE='Units:',$
                                         VALUE=value,$
                                         UVALUE='Spectrum Units')
              
           box4_base = widget_base(col2_base,$
                                   /COLUMN,$
                                   FRAME=2)
           
              label = widget_label(box4_base,$
                                   VALUE='4.  Write File',$
                                   FONT=buttonfont,$
                                   /ALIGN_LEFT)
              
              oname = coyote_field2(box4_base,$
                                    LABELFONT=buttonfont,$
                                    FIELDFONT=textfont,$
                                    TITLE='Object File:',$
                                    UVALUE='Object File',$
                                    XSIZE=18,$
                                    TEXTID=textid)
              state.w.objoname_fld = [oname,textid]
              
              button = widget_button(box4_base,$
                                     VALUE='Construct Corrected Spectra',$
                                     UVALUE='Construct Spectra',$
                                     FONT=buttonfont)
              
           help = widget_button(state.w.xtellcor_basic_base,$
                                VALUE='Help',$
                                UVALUE='Help',$
                                FONT=buttonfont)

      
; Get things running.  Center the widget using the Fanning routine.

   mc_mkct            
   cgcentertlb,state.w.xtellcor_basic_base
   widget_control, state.w.xtellcor_basic_base, /REALIZE

  widget_control, state.w.xtellcor_basic_base, /REALIZE
  widget_control, message, GET_VALUE=x
  state.w.message = x
  wset, x
  erase, color=196

  widget_geom = widget_info(state.w.xtellcor_basic_base, /GEOMETRY)
  widget_control, message, XSIZE=widget_geom.xsize-17
  erase, color=196   
   
; Start the Event Loop. This will be a non-blocking program.

   XManager, 'xtellcor_basic', $
             state.w.xtellcor_basic_base, $
             CLEANUP='xtellcor_basic_cleanup',$
             /NO_BLOCK
   
   widget_control, state.w.xtellcor_basic_base, SET_UVALUE=state, /NO_COPY
   

end
