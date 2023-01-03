;+
; NAME:
;     xtellcor_finish
;
; PURPOSE:
;     Telluric corrects a spectrum using an output telluric spectrum.
;    
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     xtellcor_finish
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
;     Writes a spectral SpeX FITS image to disk
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
;     Just applies a previously derived telluric correction spectrum
;     to a object spectrum.
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2002 - Written by M. Cushing, Institute for Astronomy, UH
;     2003-04-30 - Fixed input the xfindshift and fixed writing
;                  procedure.
;     2006-06-12 - Fixed bug with the shifts found by Dawn.
;     2006-08-01 - Fixed bug with the YUNITS keyword.
;-
;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro xtellcor_finish_event,event

widget_control, event.id,  GET_UVALUE = uvalue
if uvalue eq 'Quit' then begin

    widget_control, event.top, /DESTROY
    goto, getout

endif

widget_control, event.top, GET_UVALUE = state, /NO_COPY
widget_control, /HOURGLASS

case uvalue of

    'Correct Spectrum': xtellcor_finish_tellcor,state

    'Get Shift': xtellcor_finish_getshift,state

    'Object Spectra Button': begin

        obj = dialog_pickfile(DIALOG_PARENT=state.w.xtellcor_finish_base,$
                              /MUST_EXIST,FILTER='*.fits')
        if obj eq '' then goto, cont
        widget_control,state.w.objspectra_fld[1],SET_VALUE = obj
        mc_setfocus,state.w.objspectra_fld

    end

    'Load Spectra': xtellcor_finish_loadspec,state

    'Plot Object Ap': begin

        state.r.plotobjap = event.index
        z = where(*state.d.objorders eq state.r.plotobjorder)
        widget_control, state.w.shift,SET_VALUE='Shift: '+ $
                        string((*state.r.shift)[z,state.r.plotobjap], $
                               FORMAT='(f6.2)')
        
    end
    
    'Plot Object Order': begin
        
        state.r.plotobjorder = (*state.d.objorders)[event.index]
        z = where(*state.d.objorders eq state.r.plotobjorder)
        widget_control, state.w.shift,SET_VALUE='Shift: '+ $
                        string((*state.r.shift)[z,state.r.plotobjap], $
                               FORMAT='(f6.2)')        

    end
    'Shift Spectra': begin

        state.r.restore = event.value
        if event.value eq 'Manual' then begin

            widget_control, state.w.temp_base, MAP=1
            widget_control, state.w.dl_base, MAP=1

        endif
        if event.value eq 'No'  then begin

            widget_control, state.w.temp_base, MAP=0
            widget_control, state.w.dl_base, MAP=0

        endif

    end

    'Telluric Spectra Button': begin

        tel = dialog_pickfile(DIALOG_PARENT=state.w.xtellcor_finish_base,$
                             /MUST_EXIST,FILTER='*.fits')
        
        if tel eq '' then goto, cont
        widget_control,state.w.telspectra_fld[1],SET_VALUE =tel
        mc_setfocus,state.w.telspectra_fld

    end


endcase

;  Put state variable into the user value of the top level base.
 
cont: 
widget_control, state.w.xtellcor_finish_base, SET_UVALUE=state, /NO_COPY
getout:

end
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xtellcor_finish_cleanup,base

widget_control, base, GET_UVALUE = state, /NO_COPY
if n_elements(state) ne 0 then begin

    ptr_free, state.d.telspec
    ptr_free, state.d.objspec
    ptr_free, state.d.objorders
    ptr_free, state.d.telorders
    ptr_free, state.d.objhdr
    ptr_free, state.d.telhdr
    ptr_free, state.d.telhistory
    ptr_free, state.d.awave
    ptr_free, state.d.atrans
    
endif
state = 0B

end
;
;******************************************************************************
;
pro xtellcor_finish_getshift,state

  zz = where((*state.d.objorders) eq state.r.plotobjorder)
  
  obj_wave = (*state.d.objspec)[*,0,zz*state.d.objnaps+state.r.plotobjap]
  obj_flux = (*state.d.objspec)[*,1,zz*state.d.objnaps+state.r.plotobjap]

  trim = mc_nantrim(obj_wave,2)
  obj_wave = obj_wave[trim]
  obj_flux = obj_flux[trim]

  
  z        = where((*state.d.telorders) eq state.r.plotobjorder)
  tel_wave = (*state.d.telspec)[*,0,z]
  tel_flux = (*state.d.telspec)[*,1,z]
  
  trim = mc_nantrim(tel_wave,2)  
  tel_wave = tel_wave[trim]
  tel_flux = tel_flux[trim]

  mc_interpspec,tel_wave,tel_flux,obj_wave,new_flux,CANCEL=cancel
  if cancel then return
  
  shift = xmc_findshift(obj_wave,obj_flux,new_flux,*state.d.awave, $
                        *state.d.atrans, $
                        INITSHIFT=total((*state.r.shift)[zz,state.r.plotobjap]),$
                        XTITLE=state.r.xtitle,CANCEL=cancel)
  
  if not cancel then begin
     
     (*state.r.shift)[zz,state.r.plotobjap] = shift
     z = where(*state.d.objorders eq state.r.plotobjorder)
     widget_control, state.w.shift,SET_VALUE='Shift: '+ $
                     string((*state.r.shift)[zz,state.r.plotobjap], $
                            FORMAT='(f6.2)')
     
endif

end
;
;******************************************************************************
;
pro xtellcor_finish_loadspec,state

  
  telfile = mc_cfld(state.w.telspectra_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return
  telfile = mc_cfile(telfile,WIDGET_ID=state.w.xtellcor_finish_base,$
                     CANCEL=cancel)
  if cancel then return
  
  objfile = mc_cfld(state.w.objspectra_fld, 7,/EMPTY,CANCEL=cancel)
  if cancel then return
  objfile = mc_cfile(objfile,WIDGET_ID=state.w.xtellcor_finish_base,$
                     CANCEL=cancel)
  if cancel then return
  
  mc_readspec,telfile,tel,telhdr,telobsmode,start,stop,telnorders,telnaps,$
              telorders,telxunits,telyunits,slith_pix,slith_arc,slitw_pix, $
              slitw_arc,rp,telairmass,xtitle,ytitle,HISTORY=history,CANCEL=cancel
  if cancel then return
  
  mc_readspec,objfile,obj,objhdr,objobsmode,start,stop,objnorders,objnaps,$
              objorders,objxunits,objyunits,slith_pix,slith_arc,slitw_pix, $
              slitw_arc,objrp,objairmass,CANCEL=cancel
  if cancel then return
  
  *state.d.telspec = tel
  *state.d.objspec = obj
  *state.d.objhdr  = objhdr
  *state.d.telhdr  = telhdr
  *state.d.telorders = telorders
  *state.d.objorders = objorders
  state.d.objnaps    = objnaps
  state.r.plotobjorder = objorders[0]
  *state.d.telhistory = strjoin(history)
  
  *state.r.shift = fltarr(objnorders,objnaps)
  state.r.dairmass = telairmass-objairmass
  
  state.r.xtitle = xtitle
  state.r.ytitle = ytitle
  
  widget_control, state.w.objorder_dl,SET_VALUE=string(objorders,FORMAT='(i2.2)')
  widget_control, state.w.objap_dl,SET_VALUE=string(indgen(objnaps)+1,$
                                                    FORMAT='(i2.2)')

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

  
;  Compute airmass difference between obj and std.

  if abs(telairmass-objairmass) gt 0.1 then begin

     color = 17
     beep
     beep
     beep

  endif else color = 16

  wset, state.w.message
  erase,COLOR=color
  xyouts,10,30,'Std Airmass:'+string(telairmass,FORMAT='(f7.4)')+ $
         ', Obj Airmass:'+string(objairmass,FORMAT='(f7.4)')+ $
         '!C!C(Std-Obj) Airmass: '+ $
         string((telairmass-objairmass),FORMAT='(f7.4)'),/DEVICE,$
         CHARSIZE=1.1,FONT=0

end
;
;******************************************************************************
;
pro xtellcor_finish_tellcor,state,CANCEL=cancel

  oname = mc_cfld(state.w.objoname_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return
    
  telfile = mc_cfld(state.w.telspectra_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return
  
  tel = *state.d.telspec
  obj = *state.d.objspec
  
  for i = 0, n_elements(*state.d.objorders)-1 do begin
     
     z = where((*state.d.telorders) eq (*state.d.objorders)[i],count)
     if count eq 0 then goto, cont
     for j = 0,state.d.objnaps-1 do begin
        
        k = i*state.d.objnaps+j
        mc_interpspec,tel[*,0,z],tel[*,1,z],obj[*,0,k],nflux,nerror,$
                      IYERROR=tel[*,2,z]
        
;  Now shift spectrum.
        
        x = findgen(n_elements(tel[*,0,z]))
        mc_interpspec,x+(*state.r.shift)[i,j],nflux,x,nnflux,nnerror, $
                      IYERROR=nerror
        
        obj[*,1,k] = nnflux*obj[*,1,k]
        obj[*,2,k] = sqrt(nnflux^2*obj[*,2,k]^2 + obj[*,1,k]^2*nnerror^2 )
        
     endfor
     
  endfor
  
cont:
  
;  Now write it to disk
  
  hdr = *state.d.objhdr
  sxdelpar,hdr,['DIVISOR','BEAM','GRSTCNT','CALMIR','DIT','OSF','QTH_LAMP',$
                'INC_LAMP','IR_SRC','ARG_SRC','SHUTTER','AIMAGE','SKY']
  

  fxaddpar,hdr,'IRAFNAME',strtrim(oname+'.fits',2)
  yunits = (strsplit(fxpar(*state.d.telhdr,'YUNITS'),'/',/EXTRACT))[0]

  fxaddpar,hdr,'A0VStd',fxpar(*state.d.telhdr,'A0Vstd',COMMENT=comment), $
           comment
  fxaddpar,hdr,'A0VBmag',fxpar(*state.d.telhdr,'A0VBmag',COMMENT=comment), $
           comment
  fxaddpar,hdr,'A0VVmag',fxpar(*state.d.telhdr,'A0VVmag',COMMENT=comment), $
           comment
  fxaddpar,hdr,'DAIRMASS',fxpar(*state.d.telhdr,'DAIRMASS',COMMENT=comment), $
           comment

  telmeth = strtrim(fxpar(*state.d.telhdr,'TELMETH',COMMENT=comment),2)
  fxaddpar,hdr,'TELMETH',telmeth,comment

  vegadv = fxpar(*state.d.telhdr,'Vegadv',COMMENT=comment)
  fxaddpar,hdr,'VEGADV',vegadv,comment

  telfile = fxpar(*state.d.telhdr,'IRAFNAME',COMMENT=comment)
  fxaddpar,hdr,'TELFILE',telfile,comment

  history = 'This spectrum was telluric corrected using the telluric '+$
            'correction spectra '+telfile+'.  The velocity shift of Vega ' + $
            'is '+strtrim(vegadv,2)+' km s-1.'

  if telmeth eq 'Deconvolution' then begin

     maxdev = fxpar(*state.d.telhdr,'TCMaxDev',COMMENT=comment)
     fxaddpar,hdr,'TCMAXDEV',maxdev,comment

     rmsdev = fxpar(*state.d.telhdr,'TCRMSDev',COMMENT=comment)
     fxaddpar,hdr,'TCRMSDEV',rmsdev,comment
     
     history = history+'  The Vega model was modified using the ' + $
               'deconvolution method.  The maximum deviation and ' + $
               'rms deviation between the modified Vega model and the ' + $
               'data over the kernel wavelength range are '+ $
               strtrim(maxdev,2)+'% and '+strtrim(rmsdev,2)+ $
               '%, respectively.  '
        
     endif else begin
        
        history = history+'The Vega model was modified using the IP method.  '
        
     endelse
  
;  Add shifts

  for i = 0, state.d.objnaps-1 do begin
     
     history = history+'The telluric correction spectra for aperture '+ $
               string(i+1,FORMAT='(i2.2)')+' were shifted by '+ $
               strjoin(strtrim((*state.r.shift)[*,i],2),', ')+' pixels.  '
     
  endfor

  fxaddpar,hdr,'YUNITS',yunits, 'Units of the Y axis'
  ytitle = (strsplit(fxpar(*state.d.telhdr,'YTITLE'),'/',/EXTRACT))[0]
  fxaddpar,hdr,'YTITLE',ytitle+')',' IDL Y title'


  hdr = mc_addhistlabel(hdr,'Xtellcor History',CANCEL=cancel)
  if cancel then return
    
  history = mc_splittext(history,70,CANCEL=cancel)
  if cancel then return
  sxaddhist,history,hdr
  
  writefits,oname+'.fits',obj,hdr
  xvspec,oname+'.fits'
  
  print
  print,'Wrote the corrected spectrum to '+oname+'.fits'
  print

end
;
;******************************************************************************
;
; ------------------------------Main Program---------------------------------- 
;
;******************************************************************************
;
;
pro xtellcor_finish

;  Get Spextool path

  spextoolpath = file_dirname( $
                 file_dirname(file_which('Spextool_Instruments.dat')),/MARK)

;  Load color table
  
  mc_mkct
  
;  Set the fonts
  
  mc_getfonts,buttonfont,textfont,CANCEL=cancel
  if cancel then return
  
  w = {bmag_fld:[0L,0L],$
       dl_base:0L,$
       inprefix_fld:[0L,0L],$
       message:0L,$
       objoname_fld:[0L,0L],$
       objorder_dl:0L,$
       objspectra_fld:[0L,0L],$
       path_fld:[0L,0L],$
       telspectra_fld:[0L,0L],$
       temp_base:0L,$
       objap_dl:0L,$
       pixshift_fld:[0L,0L],$
       shift:0L,$
       xtellcor_finish_base:0L,$
       vmag_fld:[0L,0L]}
  
  r = {dairmass:0.0,$
       plotobjap:0,$
       plotobjorder:0,$
       spextoolpath:spextoolpath,$
       shift:ptr_new(2),$
       textoutput:0,$
       xtitle:'',$
       ytitle:''}
  
  d = {awave:ptr_new(2),$
       atrans:ptr_new(2),$
       telspec:ptr_new(fltarr(2)),$
       telhdr:ptr_new(2),$
       telhistory:ptr_new(2),$
       objhdr:ptr_new(fltarr(2)),$
       objspec:ptr_new(fltarr(2)),$
       objorders:ptr_new(fltarr(2)),$
       objnaps:0,$
       telorders:ptr_new(fltarr(2))}
  
  state = {w:w,r:r,d:d}
  
  state.w.xtellcor_finish_base = widget_base(TITLE='Xtellcor (Finish)', $
                                             EVENT_PRO='xtellcor_finish_event',$
                                             /COLUMN)
  
     quit_button = widget_button(state.w.xtellcor_finish_base,$
                                 FONT=buttonfont,$
                                 VALUE='Done',$
                                 UVALUE='Quit')
     
     message = widget_draw(state.w.xtellcor_finish_base,$
                           /ALIGN_CENTER,$
                           FRAME=2,$
                           XSIZE=10,$
                           YSIZE=50)
   
     box1_base = widget_base(state.w.xtellcor_finish_base,$
                             /COLUMN,$
                             FRAME=2)
     
        label = widget_label(box1_base,$
                             VALUE='1.  Load Spectra',$
                             FONT=buttonfont,$
                             /ALIGN_LEFT)
        
        row = widget_base(box1_base,$
                          /ROW,$
                          /BASE_ALIGN_CENTER)
        
        row = widget_base(box1_base,$
                          /ROW,$
                          /BASE_ALIGN_CENTER)
        
           obj_but = widget_button(row,$
                                   FONT=buttonfont,$
                                   VALUE='Obj Spectra',$
                                   UVALUE='Object Spectra Button')
           
           obj_fld = coyote_field2(row,$
                                   LABELFONT=buttonfont,$
                                   fieldfont=textfont,$
                                   TITLE=':',$
                                   UVALUE='Object Spectra Field',$
                                   VALUE='cspectra25-34.fits',$
                                   XSIZE=25,$
                                   /CR_ONLY,$
                                   TEXTID=textid)
           state.w.objspectra_fld = [obj_fld,textid]
           
        row = widget_base(box1_base,$
                          /ROW,$
                          /BASE_ALIGN_CENTER)
        
           tel_but = widget_button(row,$
                                   FONT=buttonfont,$
                                   VALUE='Tel Spectra',$
                                   UVALUE='Telluric Spectra Button')
           
           tel_fld = coyote_field2(row,$
                                   LABELFONT=buttonfont,$
                                   FIELDFONT=textfont,$
                                   TITLE=':',$
                                   UVALUE='Telluric Spectra Field',$
                                   VALUE='test_tellspec.fits',$
                                   XSIZE=25,$
                                   /CR_ONLY,$
                                   TEXTID=textid)
           state.w.telspectra_fld = [tel_fld,textid]
           
        button = widget_button(box1_base,$
                               FONT=buttonfont,$
                               VALUE='Load Spectra',$
                               UVALUE='Load Spectra')
        
     box2_base = widget_base(state.w.xtellcor_finish_base,$
                             /COLUMN,$
                             FRAME=2)
     
        label = widget_label(box2_base,$
                             VALUE='2.  Get Shift',$
                             FONT=buttonfont,$
                             /ALIGN_LEFT)
        
        row = widget_base(box2_base,$
                          /ROW,$
                          /BASE_ALIGN_CENTER)
        
           state.w.objap_dl = widget_droplist(row,$
                                              FONT=buttonfont,$
                                              TITLE='Aperture:',$
                                              VALUE='01',$
                                              UVALUE='Plot Object Ap')
           
           state.w.objorder_dl = widget_droplist(row,$
                                                 FONT=buttonfont,$
                                                 TITLE='Order:',$
                                                 VALUE='01',$
                                                 UVALUE='Plot Object Order')
           
        state.w.shift = widget_label(box2_base,$
                                     /ALIGN_LEFT,$
                                     /DYNAMIC_RESIZE,$
                                     FONT=buttonfont,$
                                     VALUE='Shift: 0.0')
        
           shift = widget_button(box2_base,$
                                 VALUE='Get Shift',$
                                 UVALUE='Get Shift',$
                                 FONT=buttonfont)
           
     box3_base = widget_base(state.w.xtellcor_finish_base,$
                             /COLUMN,$
                             FRAME=2)
      
        label = widget_label(box3_base,$
                             VALUE='3.  Write File',$
                             FONT=buttonfont,$
                             /ALIGN_LEFT)
        
        oname = coyote_field2(box3_base,$
                              LABELFONT=buttonfont,$
                              FIELDFONT=textfont,$
                              TITLE='Output File:',$
                              UVALUE='Output File Oname',$
                              VALUE='junk',$
                              xsize=25,$
                              textID=textid)
        state.w.objoname_fld = [oname,textid]
        
        write = widget_button(box3_base,$
                              VALUE='Correct Spectrum',$
                              UVALUE='Correct Spectrum',$
                              FONT=buttonfont)
        
; Get things running.  Center the widget using the Fanning routine.

  cgcentertlb,state.w.xtellcor_finish_base
  widget_control, state.w.xtellcor_finish_base, /REALIZE
  widget_control, message, GET_VALUE=x
  state.w.message = x
  wset, x
  erase, color=196
  
  widget_geom = widget_info(state.w.xtellcor_finish_base, /GEOMETRY)
  widget_control, message, XSIZE=widget_geom.xsize-17
  erase, color=196

; Start the Event Loop. This will be a non-blocking program.

  XManager, 'xtellcor_finish', $
            state.w.xtellcor_finish_base, $
            CLEANUP='xtellcor_finish_cleanup',$
            /NO_BLOCK
  
  widget_control, state.w.xtellcor_finish_base, SET_UVALUE=state, /NO_COPY
  

end
