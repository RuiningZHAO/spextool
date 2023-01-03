;+
; NAME:
;     xtellcor_general
;    
; PURPOSE:
;     General telluric correction widget.
;    
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     xtellcor_general
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
;     Writes a text file to disk of the telluric corrected spectrum
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
;     Follow the directions
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2001 - Written by M. Cushing, Institute for Astonomy, UH
;     2008-01-28 - Modified so that it checks to see if it is
;                  FITS and then just accepts it as a text file.
;-


;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro xtellcor_general_event, event

  widget_control, event.top, GET_UVALUE = state, /NO_COPY
  widget_control, event.id,  GET_UVALUE = uvalue
  widget_control, /HOURGLASS
  
  case uvalue of
     
     'Additional Output': begin
        
        if event.value eq 'Telluric' then state.r.telluricoutput=event.select
        if event.value eq 'A0 V' then state.r.vegaoutput=event.select
        
     end
     
     'B Magnitude Field': mc_setfocus, state.w.vmag_fld
     
     'Construct Telluric Spectra': xtellcor_general_contellspec,state
     
     'Get Shift': xtellcor_general_getshift,state
     
     'Help': begin

        pre = (strupcase(!version.os_family) eq 'WINDOWS') ? 'start ':'open '
        
        spawn, pre+filepath('xtellcor_generalmanual.pdf', $
                            ROOT=state.r.spextoolpath,$
                            SUBDIR='manuals')
        


     end
        
     'Load Spectra': xtellcor_general_loadspec,state
     
     'Method': begin
        
        state.r.method = event.value
        
        if event.value eq 'Deconvolution' then widget_control, $
           state.w.box2a_base,MAP=1 else $
              widget_control, state.w.box2a_base, MAP=0
        
     end
     
     'Object Spectra Button': begin
        
        obj = dialog_pickfile(DIALOG_PARENT=state.w.xtellcor_general_base,$
                              PATH=state.r.objpath,/MUST_EXIST,$
                              FILTER='*dat,*txt',GET_PATH=path)
        if obj eq '' then goto, cont
        widget_control,state.w.objspectra_fld[1], $
                       SET_VALUE = strmid(obj[0],strpos(obj,'/',/REVERSE_S)+1)
        state.r.objpath = path
        mc_setfocus,state.w.objspectra_fld
        
     end
     
     'Standard Order': state.r.stdorder = (*state.d.stdorders)[event.index]
     
     'Quit': begin
        
        widget_control, event.top, /DESTROY
        goto, getout
        
     end
     
     'Scale Lines': xtellcor_general_getscales,state
     
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
        
        std = dialog_pickfile(DIALOG_PARENT=state.w.xtellcor_general_base,$
                              PATH=state.r.stdpath,/MUST_EXIST, $
                              FILTER='*dat,*txt',GET_PATH=path)
        
        if std eq '' then goto, cont
        widget_control,state.w.stdspectra_fld[1],$
                       SET_VALUE = strmid(std[0],strpos(std,'/',/REVERSE_S)+1)
        state.r.stdpath = path
        mc_setfocus,state.w.stdspectra_fld
        
     end
     
     'Standard Spectra Field': mc_setfocus,state.w.bmag_fld
     
     'V Magnitude Field': mc_setfocus, state.w.objspectra_fld
     
     'Wavelength Units': begin
        
        xunits = ['um','nm','A']
        state.r.wunits = event.index
        state.r.xunits = xunits[event.index]
        widget_control, state.w.fwhm_lbl,SET_VALUE=xunits[event.index]
        
     end

     'Write File': xtellcor_general_writefile,state
     
     else:
     
  endcase
  
;  Put state variable into the user value of the top level base.
 
  cont: 
  widget_control, state.w.xtellcor_general_base, SET_UVALUE=state, /NO_COPY
  getout:

end
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xtellcor_general_cleanup,base
  
  widget_control, base, GET_UVALUE = state, /NO_COPY
  if n_elements(state) ne 0 then begin
     
     ptr_free, state.d.fwhm
     ptr_free, state.r.normmask
     ptr_free, state.d.atrans
     ptr_free, state.d.awave
     ptr_free, state.d.corspec
     ptr_free, state.d.objhdr
     ptr_free, state.d.objspec
     ptr_free, state.d.nstd
     ptr_free, state.d.stdhdr
     ptr_free, state.d.stdorders
     ptr_free, state.d.stdspec
     ptr_free, state.d.tellspec
     ptr_free, state.d.wvega
     ptr_free, state.d.swvega
     ptr_free, state.d.fvega
     ptr_free, state.d.cfvega
     ptr_free, state.d.cf2vega
     
     ptr_free, state.p.flux
     ptr_free, state.p.wave
     
     
     cutreg = *state.r.cutreg
     for i = 0, n_tags(cutreg)-1 do ptr_free, cutreg.(i)
     
     ptr_free, state.r.cutreg
     
     
  endif
  state = 0B

end
;
;******************************************************************************
;
pro xtellcor_general_changeunits,wave,tellspec,tellspec_error,scvega,state

  c = 2.99792458e+8
  
  case state.r.units of 
     
     'ergs s-1 cm-2 A-1': 
     
     'ergs s-1 cm-2 Hz-1': begin
        
        tellspec = mc_chfunits(wave,tellspec,0,1,3,IERROR=tellspec_error, $
                               OERROR=tellspec_error)
        scvega = mc_chfunits(wave,scvega,0,1,3)
        
     end
     'W m-2 um-1': begin 
        
        tellspec = mc_chfunits(wave,tellspec,0,1,0,IERROR=tellspec_error, $
                               OERROR=tellspec_error)
        scvega   = mc_chfunits(wave,scvega,0,1,0)
        
     end
     'W m-2 Hz-1': begin
        
        tellspec = mc_chfunits(wave,tellspec,0,1,2, IERROR=tellspec_error, $
                               OERROR=tellspec_error)
        scvega   = mc_chfunits(wave,scvega,0,1,2)
        
     end
     
     'Jy': begin
        
        tellspec = mc_chfunits(wave,tellspec,0,1,4,IERROR=tellspec_error, $
                               OERROR=tellspec_error)
        scvega   = mc_chfunits(wave,scvega,0,1,4)
        
     end
     
  endcase
  
end
;
;******************************************************************************
;
pro xtellcor_general_conkernel,state

  if state.r.continue lt 1 then begin
     
     ok = dialog_message('Previous steps not complete.',/ERROR,$
                         DIALOG_PARENT=state.w.xtellcor_general_base)
     return
     
  endif
  
  vkernw_pix = state.d.fwhm/state.r.vdisp
  
  nkernel = round(10.*vkernw_pix)

  if not nkernel mod 2 then nkernel = nkernel + 1
     
  kernel = psf_gaussian(NDIMEN=1,NPIX=nkernel,FWHM=vkernw_pix,/NORMALIZE)
  
  key = 'Order'+string(1,FORMAT='(i2.2)')
  str = create_struct(key,kernel)

  *state.d.kernels  = str
  state.r.vshift   = 0.0
  state.r.scale    = 1.0
  state.r.continue = 2
  
end
;
;******************************************************************************
;
pro xtellcor_general_contellspec,state

  if state.r.continue lt 3 then begin
     
     ok = dialog_message('Previous steps not complete.',/ERROR,$
                         DIALOG_PARENT=state.w.xtellcor_general_base)
     return
     
  endif
  
  if n_elements(*state.d.objorders) gt 1 then begin
     
     if (*state.d.objorders)[0] gt (*state.d.objorders)[1] then begin
        
        stdidx = reverse(stdidx)
        objidx = reverse(objidx)
        
     endif

  endif

  *state.d.tellspec = *state.d.stdspec
  *state.d.vegaspec = *state.d.stdspec
  (*state.d.vegaspec)[*,2,*] = 1.0
  

  print, 'Constructing Telluric Correction Spectrum...'
  
  mc_mktellspec, (*state.d.stdspec)[*,0],(*state.d.stdspec)[*,1],$
                 (*state.d.stdspec)[*,2],state.r.vmag, $
                 (state.r.bmag-state.r.vmag),(*state.d.kernels).(0), $
                 *state.r.scales,*state.d.wvega,*state.d.fvega, $
                 *state.d.cfvega,*state.d.cf2vega,state.r.vshift,tellcor, $
                 tellcor_error,scvega,CANCEL=cancel
  if cancel then return

;  Perform interpolations if necessary

  cutreg = *state.r.cutreg
  ndat = n_elements(*cutreg.(0))
  
  if ndat ne 1 then begin
        
     nreg = ndat/2
     for j = 0, nreg-1 do begin

        
        xrange = reform((*cutreg.(0))[(j*2):(j*2+1)])
        tabinv,(*state.d.stdspec)[*,0],xrange,idx
        idx = round(idx)

        x = [(*state.d.stdspec)[idx[0],0],$
             (*state.d.stdspec)[idx[1],0]]
        
        y = [tellcor[idx[0]],tellcor[idx[1]]]
        
        e = [tellcor_error[idx[0]],tellcor_error[idx[1]]]
        
        coeff  = poly_fit1d(x,y,1,/SILENT)
        coeffe = poly_fit1d(x,e,1,/SILENT)
        
        tellcor[idx[0]:idx[1]]=$
           poly((*state.d.stdspec)[idx[0]:idx[1],0],coeff)
        
        tellcor_error[idx[0]:idx[1]]=$
           poly((*state.d.stdspec)[idx[0]:idx[1],0],coeffe)
        
     endfor
     
  endif
  
  xtellcor_general_changeunits,(*state.d.stdspec)[*,0],tellcor, $
                               tellcor_error,scvega,state

  (*state.d.tellspec)[*,1] = tellcor
  (*state.d.tellspec)[*,2] = tellcor_error
  (*state.d.vegaspec)[*,1] = scvega
    
  state.r.continue = 4
  
end
;
;******************************************************************************
;
pro xtellcor_general_getscales,state

  if state.r.continue lt 2 then begin
     
     ok = dialog_message('Previous steps not complete.',/ERROR,$
                         DIALOG_PARENT=state.w.xtellcor_general_base)
     return
     
  endif
  
  vshift = state.r.vshift
  
  wvega  = *state.d.wvega
  fvega  = *state.d.fvega
  fcvega = *state.d.cfvega

  xmc_scalelines,*state.d.stdspec,1,state.r.vmag,(state.r.bmag-state.r.vmag), $
                 wvega,fvega,fcvega,*state.d.cf2vega,*state.d.kernels, $
                 vshift,*state.d.objspec,*state.d.objorders,state.r.objnaps, $
                 *state.d.awave,*state.d.atrans,state.r.hlines,state.r.hnames, $
                 state.r.scale,scales,cutreg,XTITLE='!5!7k!5 (!7l!5m)', $
                 YTITLE='!5Arbitrary Flux',$
                 PARENT=state.w.xtellcor_general_base,$
                 CANCEL=cancel
  
  if cancel then return
     
  *state.r.scales = scales
  *state.r.cutreg = cutreg
  state.r.continue = 3
  state.r.vshift = vshift
  
end
;
;******************************************************************************
;
pro xtellcor_general_loadspec,state

  case state.r.wunits of 

     0: scale = 1.0
     
     1: scale = 0.001
     
     2: scale = 0.0001
     
  endcase
  
;  Get files.
  
  std = mc_cfld(state.w.stdspectra_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return
  std = mc_cfile(state.r.stdpath+std,WIDGET_ID=state.w.xtellcor_general_base, $
                 CANCEL=cancel)
  if cancel then return
  
  bmag = mc_cfld(state.w.bmag_fld,4,/EMPTY,CANCEL=cancel)
  if cancel then return
  vmag = mc_cfld(state.w.vmag_fld,4,/EMPTY,CANCEL=cancel)
  if cancel then return
  
  obj = mc_cfld(state.w.objspectra_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return
  obj = mc_cfile(state.r.objpath+obj,WIDGET_ID=state.w.xtellcor_general_base, $
                 CANCEL=cancel)
  if cancel then return
  
  fwhm = mc_cfld(state.w.fwhm_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return

  vrot = mc_cfld(state.w.vrot_fld,4,/EMPTY,CANCEL=cancel)
  if cancel then return
  
;  Read spectra and load data.
     
  junk = (read_ascii(obj,COMMENT='#')).(0)
  wo = reform(junk[0,*])
  fo = reform(junk[1,*])
  if n_elements(junk[*,0]) gt 2 then eo = reform(junk[2,*])
  
  wo = temporary(wo)*scale
  
  if (size(eo))[0] ne 0 then begin
     
     ospec = [[wo],[fo],[eo]]
     
  endif else begin
     
     ndat = n_elements(wo)
     ospec = [[wo],[fo],[replicate(1.0,ndat)]]
     
  endelse
  
  junk = (read_ascii(std,COMMENT='#')).(0)
  ws = reform(junk[0,*])
  fs = reform(junk[1,*])
  if n_elements(junk[*,0]) gt 2 then es = reform(junk[2,*])

  ws = temporary(ws)*scale

  if (size(es))[0] ne 0 then begin
     
     sspec = [[ws],[fs],[es]]
     
  endif else begin
     
     ndat = n_elements(ws)
     sspec = [[ws],[fs],[replicate(1.0,ndat)]]
     
  endelse
  
  state.r.stdnorders = 1
  state.r.objnorders = 1
  *state.d.objorders = [1]
  state.d.fwhm = fwhm*scale
  
  *state.d.stdspec = sspec
  *state.d.objspec = ospec

  state.r.bmag = bmag
  state.r.vmag = vmag
  state.r.vshift = 0.0
  state.r.vrot = vrot
  
;  Load Vega model

  restore, filepath('lvega5.sav',ROOT_DIR=state.r.spextoolpath,SUBDIR='data') 
  state.r.vdisp = 1.49774e-5
  
  *state.d.wvega = wvin/10000.

;  Spin up vega model
  
  fvin   = add_rotation(wvin,fvin,vrot)
  fcvin  = add_rotation(wvin,fcvin,vrot)
  fc2vin = add_rotation(wvin,fc2vin,vrot)

  *state.d.cf2vega = fc2vin
  *state.d.cfvega = fcvin
  *state.d.fvega = fvin

;  Load atmospheric transmission

  rp = median(ospec[*,0],/EVEN)/state.d.fwhm

  if rp ge 10000.0 then trp = round(rp/10000.)*10000
  if rp lt 10000.0 and rp ge 1000.0 then trp = round(rp/1000.)*1000
  if rp lt 1000.0 and rp ge 100.0 then trp = round(rp/100.)*100
  if rp lt 100.0 and rp ge 10.0 then trp = 100
  if rp eq 0 then trp=2000
  
  spec = readfits(filepath('atran'+strtrim(trp,2)+'.fits', $
                           ROOT_DIR=state.r.spextoolpath,SUBDIR='data'), $
                  /SILENT)
  *state.d.awave = reform(spec[*,0])
  *state.d.atrans = reform(spec[*,1])
  state.r.shift = 0.0

  state.r.continue=1
  
  xtellcor_general_conkernel,state
  
  state.r.continue=2
  
end
;
;******************************************************************************
;
pro xtellcor_general_getshift,state

  if state.r.continue lt 4 then begin
     
     ok = dialog_message('Previous steps not complete.',/ERROR,$
                         DIALOG_PARENT=state.w.xtellcor_general_base)
     return
     
  endif
  
  obj_wave = (*state.d.objspec)[*,0]
  obj_flux = (*state.d.objspec)[*,1]
    
  tel_wave = (*state.d.tellspec)[*,0]
  tel_flux = (*state.d.tellspec)[*,1]

  
  mc_interpspec,tel_wave,tel_flux,obj_wave,new_flux,CANCEL=cancel
  if cancel then return


  
  shift = xmc_findshift(obj_wave,obj_flux,new_flux,*state.d.awave, $
                        *state.d.atrans,INITSHIFT=state.r.shift,$
                        PARENT=state.w.xtellcor_general_base,CANCEL=cancel)
  
  if cancel then return
       
  state.r.shift = shift
  widget_control, state.w.shift,SET_VALUE='Shift: '+ $
                  string(state.r.shift,FORMAT='(f6.2)')
  
end
;
;******************************************************************************
;
pro xtellcor_general_writefile,state

  if state.r.continue lt 4 then begin
     
     ok = dialog_message('Previous steps not complete.',/ERROR,$
                         DIALOG_PARENT=state.w.xtellcor_general_base)
     return
     
  endif
  
  obj = mc_cfld(state.w.objoname_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return
  
  stdfile = mc_cfld(state.w.stdspectra_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return
  
  objiname = mc_cfld(state.w.objspectra_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return
  
  case state.r.wunits of 
     
     0: begin

        scale = 1.0
        xunits = 'um'
        
     end
     1: begin

        scale = 1000.
        xunits = 'nm'

     end
     
     2: begin

        scale = 10000.
        xunits = 'A'

     end
  endcase
  
;  Scale the wavelengths back to the user input.
  
  (*state.d.tellspec)[*,0] = (*state.d.tellspec)[*,0]*scale
  (*state.d.vegaspec)[*,0] = (*state.d.vegaspec)[*,0]*scale
  (*state.d.objspec)[*,0]  = (*state.d.objspec)[*,0]*scale
  
;  Write the telluric correction spectrum to disk

  if state.r.telluricoutput then begin
     
     npix = n_elements((*state.d.tellspec)[*,0])
     openw,lun,state.r.objpath+obj+'_tellspec.dat', /GET_LUN

     printf, lun, mc_datetag(PERSON='xtellcor (General)')
     
     printf, lun, '# A0VBmag = '+strtrim(string(state.r.bmag),2)+ $
             '  - B-band magnitude of A0 V star'
     printf, lun, '# A0VVmag = '+strtrim(string(state.r.vmag),2)+ $
             '  - V-band magnitude of A0 V star'
     printf, lun, '# FWHM = '+strtrim(string(state.d.fwhm*scale),2)+ $
             ' - FWHM of gaussian kernel in XUNITS'
     printf, lun, '# Vrot = '+strtrim(string(state.r.vrot),2)+ $
             ' - Rotation speed of standard star in km s-1'

     printf, lun, '# VEGADV = '+strtrim(string(state.r.vshift),2)+$
             ' - Vega velocity shift in km s-1'
     printf, lun, '# XUNITS = '+xunits+' - Wavelength units'
     printf, lun, '# YUNITS = '+strtrim(state.r.units,2)+'/input units'+ $
             ' - Intensity units'

     printf, lun, '#'
     
     for i = 0, npix-1 do begin
        
        printf, lun,  strjoin( reform((*state.d.tellspec)[i,*],3),'  ' )
        
     endfor
     free_lun, lun
     
     print,'Wrote the telluric correction spectrum to '+ $
           strtrim(state.r.objpath+obj,2)+'_tellcor.dat'
     
     
  endif
  
;  Write the convolved resampled Vega spectrum to disk

  if state.r.vegaoutput then begin

     npix = n_elements((*state.d.vegaspec)[*,0])        
     openw,lun,state.r.objpath+obj+'_A0V.dat', /GET_LUN

     printf, lun, mc_datetag(PERSON='xtellcor (General)')
     
     printf, lun, '# A0VBmag = '+strtrim(string(state.r.bmag),2)+ $
             '  - B-band magnitude of A0 V star'
     printf, lun, '# A0VVmag = '+strtrim(string(state.r.vmag),2)+ $
             '  - V-band magnitude of A0 V star'
     printf, lun, '# FWHM = '+strtrim(string(state.d.fwhm*scale),2)+ $
             ' - FWHM of gaussian kernel in XUNITS'
     printf, lun, '# Vrot = '+strtrim(string(state.r.vrot),2)+ $
             ' - Rotation speed of standard star in km s-1'

     printf, lun, '# VEGADV = '+strtrim(string(state.r.vshift),2)+$
             ' - Vega velocity shift in km s-1'
     printf, lun, '# XUNITS = '+xunits+' - Wavelength units'
     printf, lun, '# YUNITS = '+strtrim(state.r.units,2)+ $
             ' - Intensity units'

     printf, lun, '#'

     
     for i = 0, npix-1 do begin
        
        printf, lun,  strjoin( reform((*state.d.vegaspec)[i,*],3),'  ' )
        
     endfor
     free_lun, lun
     
     print,'Wrote the Vega spectrum to '+strtrim(state.r.objpath+obj,2)+ $
           '_modVega.dat'

  endif

;  Write the telluric corrected object spectrum to disk.

  corspec  = *state.d.objspec
        
  mc_interpspec,(*state.d.tellspec)[*,0],(*state.d.tellspec)[*,1],$
                (*state.d.objspec)[*,0],nflux,nerror, $
                IYERROR=(*state.d.tellspec)[*,2],CANCEL=cancel
  if cancel then return
  
;  Now shift spectrum.
        
  x = findgen(n_elements((*state.d.objspec)[*,0]))
  mc_interpspec,x+state.r.shift,nflux,x,nnflux,nnerror,IYERROR=nerror, $
                CANCEL=cancel
  if cancel then return
  
  corspec[*,1] = nnflux*(*state.d.objspec)[*,1]
  corspec[*,2] = sqrt( nnflux^2 * (*state.d.objspec)[*,2]^2 + $
                       (*state.d.objspec)[*,1]^2 * nnerror^2 )
  
;  Now write it to disk

  openw,lun,state.r.objpath+obj+'.dat', /GET_LUN

  printf, lun, mc_datetag(PERSON='xtellcor (General)')
  
  printf, lun, '# A0VBmag = '+strtrim(string(state.r.bmag),2)+ $
          '  - B-band magnitude of A0 V star'
  printf, lun, '# A0VVmag = '+strtrim(string(state.r.vmag),2)+ $
          '  - V-band magnitude of A0 V star'
  printf, lun, '# FWHM = '+strtrim(string(state.d.fwhm*scale),2)+ $
          ' - FWHM of gaussian kernel in XUNITS'
  printf, lun, '# Vrot = '+strtrim(string(state.r.vrot),2)+ $
          ' - Rotation speed of standard star in km s-1'
  
  printf, lun, '# VEGADV = '+strtrim(string(state.r.vshift),2)+$
          ' - Vega velocity shift in km s-1'
  printf, lun, '# XUNITS = '+xunits+' - Wavelength units'
  printf, lun, '# YUNITS = '+strtrim(state.r.units,2)+ $
          ' - Intensity units'
  
  printf, lun, '#'
  
  npix = n_elements(corspec[*,0])        
  for i = 0, npix-1 do begin
     
     printf, lun,  strjoin( reform(corspec[i,*],3),'  ' )
     
  endfor

  free_lun, lun
  
  print,'Wrote the corrected spectrum to '+strtrim(state.r.objpath+obj,2)+'.dat'
  
  xzoomplot,corspec[*,0],corspec[*,1],XTITLE='Wavelength ('+xunits+')',$
            YTITLE='Flux ('+state.r.units+')'

end
;
;******************************************************************************
;
; ------------------------------Main Program---------------------------------- 
;
;******************************************************************************
;
pro xtellcor_general

  mc_mkct

;  Startup

  mc_getosinfo,dirsep,strsep,CANCEL=cancel
  if cancel then return

;  Get Spextool path

  spextoolpath = file_dirname(file_dirname($
                 file_which('Spextool_Instruments.dat')),/MARK)
  
;  Get hydrogen lines

  readcol,filepath('HI.dat',ROOT_DIR=spextoolpath,SUBDIR='data'),$
         hlines,hnames,FORMAT='D,A',COMMENT='#',DELIMITER='|',/SILENT

;  Set the fonts

  mc_getfonts,buttonfont,textfont
  
;  Build three structures which will hold important info.
  
  w = {bmag_fld:[0,0],$
       fwhm_fld:[0L,0L],$
       fwhm_lbl:0L,$
       objoname_fld:[0L,0L],$
       objorder_dl:0L,$
       objspectra_fld:[0,0],$
       shift:0L,$
       stdspectra_fld:[0L,0L],$
       stdorder_dl:0L,$
       vmag_fld:[0,0],$
       vrot_fld:[0L,0L],$
       xtellcor_general_base:0L}
  
  r = {bmag:0.,$
       cutreg:ptr_new(2),$
       hlines:hlines,$
       hnames:hnames,$
       continue:0L,$
       spextoolpath:spextoolpath,$
       plotobjorder:0,$
       plotobjap:0,$
       stdnorders:0,$
       objnorders:0,$
       objnaps:0,$
       objpath:'',$
       scale:0.,$
       scales:ptr_new(fltarr(2)),$
       shift:0.0,$
       spec:'',$
       stdpath:'',$
       vegaoutput:0,$
       telluricoutput:1,$
       units:'ergs s-1 cm-2 A-1',$
       vdisp:0.,$
       vmag:0.,$
       vrot:0.,$
       vshift:0.,$
       wline:0.,$
       wunits:0,$
       xunits:'um',$
       yunits:''}
  
  d = {airmass:'',$
       awave:ptr_new(2),$
       atrans:ptr_new(2),$
       fwhm:0.0,$
       kernels:ptr_new(fltarr(2)),$
       objhdr:ptr_new(fltarr(2)),$
       objorders:ptr_new(2),$
       objspec:ptr_new(fltarr(2)),$
       stdorders:ptr_new(2),$
       stdspec:ptr_new(fltarr(2)),$
       tellspec:ptr_new(fltarr(2)),$
       fvega:ptr_new(fltarr(2)),$
       wvega:ptr_new(fltarr(2)),$
       vegaspec:ptr_new(fltarr(2)),$
       cfvega:ptr_new(fltarr(2)),$
       cf2vega:ptr_new(fltarr(2))}

;  Load the three structures in the state structure.

  state = {w:w,r:r,d:d}

  state.w.xtellcor_general_base = widget_base(TITLE='Xtellcor (General)', $
                                           EVENT_PRO='xtellcor_general_event',$
                                              /COLUMN)
  
  quit_button = widget_button(state.w.xtellcor_general_base,$
                              FONT=buttonfont,$
                              VALUE='Done',$
                              UVALUE='Quit')
  
     row_base = widget_base(state.w.xtellcor_general_base,$
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
                 
                 fld = coyote_field2(row,$
                                     LABELFONT=buttonfont,$
                                     FIELDFONT=textfont,$
                                     TITLE=':',$
                                     UVALUE='Standard Spectra Field',$
                                     XSIZE=18,$
                                     VALUE='HIP20379_out.txt',$
                                     /CR_ONLY,$
                                     TEXTID=textid)
                 state.w.stdspectra_fld = [fld,textid]
                 
              row = widget_base(box1_base,$
                                /ROW,$
                                /BASE_ALIGN_CENTER)
              
                 fld = coyote_field2(row,$
                                     LABELFONT=buttonfont,$
                                     FIELDFONT=textfont,$
                                     TITLE='Std Mag (B,V):',$
                                     UVALUE='B Magnitude Field',$
                                     XSIZE=6,$
                                     VALUE='7',$
                                     /CR_ONLY,$
                                     TEXTID=textid)
                 state.w.bmag_fld = [fld,textid]
               
                 fld = coyote_field2(row,$
                                     LABELFONT=buttonfont,$
                                     FIELDFONT=textfont,$
                                     UVALUE='V Magnitude Field',$
                                     XSIZE=6,$
                                     VALUE='7',$
                                     TITLE='',$
                                     /CR_ONLY,$
                                     TEXTID=textid)
                 state.w.vmag_fld = [fld,textid]
               
              row = widget_base(box1_base,$
                                /ROW,$
                                /BASE_ALIGN_CENTER)
              
                 but = widget_button(row,$
                                     FONT=buttonfont,$
                                     VALUE='Obj Spectra',$
                                     UVALUE='Object Spectra Button',$
                                     EVENT_PRO='xtellcor_general_event')
               
                 fld = coyote_field2(row,$
                                     LABELFONT=buttonfont,$
                                     FIELDFONT=textfont,$
                                     TITLE=':',$
                                     UVALUE='Object Spectra Field',$
                                     VALUE='SN13gy_out.txt',$
                                     XSIZE=18,$
                                     /CR_ONLY,$
                                     TEXTID=textid)
                 state.w.objspectra_fld = [fld,textid]

              value =['um','nm','A']
              wunits_dl = widget_droplist(box1_base,$
                                          FONT=buttonfont,$
                                          TITLE='Wavelength Units:',$
                                          VALUE=value,$
                                          UVALUE='Wavelength Units')
              
              row = widget_base(box1_base,$
                                /ROW,$
                                /BASE_ALIGN_CENTER)
              
                 fld = coyote_field2(row,$
                                     LABELFONT=buttonfont,$
                                     FIELDFONT=textfont,$
                                     UVALUE='FWHM Field',$
                                     XSIZE=15,$
                                     VALUE='0.01',$
                                     TITLE='FWHM = ',$
                                     TEXTID=textid)
                 state.w.fwhm_fld = [fld,textid]

                 state.w.fwhm_lbl = widget_label(row,$
                                                 VALUE='um',$
                                                 FONT=buttonfont,$
                                                 /DYNAMIC_RESIZE)
                 
              row = widget_base(box1_base,$
                                /ROW,$
                                /BASE_ALIGN_CENTER)
              
              fld = coyote_field2(row,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  UVALUE='V_rot',$
                                  XSIZE=15,$
                                  VALUE='0.0',$
                                  TITLE='V_rot = ',$
                                  TEXTID=textid)
              state.w.vrot_fld = [fld,textid]
               
              label = widget_label(row,$
                                   VALUE='km / s',$
                                   FONT=buttonfont)
              
           load = widget_button(box1_base,$
                                VALUE='Load Spectra',$
                                FONT=buttonfont,$
                                UVALUE='Load Spectra')
           
      col2_base = widget_base(row_base,$
                              EVENT_PRO='xtellcor_general_event',$
                              /COLUMN)


         box3_base = widget_base(col2_base,$
                                 /COLUMN,$
                                 FRAME=2)
            
            label = widget_label(box3_base,$
                                 VALUE='2.  Construct Telluric Spectra',$
                                 FONT=buttonfont,$
                                 /ALIGN_LEFT)

            getshifts = widget_button(box3_base,$
                                      VALUE='Scale Lines',$
                                      UVALUE='Scale Lines',$
                                      FONT=buttonfont)

            value =['ergs s-1 cm-2 A-1','ergs s-1 cm-2 Hz-1',$
                    'W m-2 um-1','W m-2 Hz-1','Jy']
            units_dl = widget_droplist(box3_base,$
                                       FONT=buttonfont,$
                                       TITLE='Units:',$
                                       VALUE=value,$
                                       UVALUE='Spectrum Units')

            constructspec = widget_button(box3_base,$
                                          VALUE='Construct Telluric Spectra',$
                                          UVALUE='Construct Telluric Spectra',$
                                          FONT=buttonfont)

         box4_base = widget_base(col2_base,$
                                 /COLUMN,$
                                 FRAME=2)

            label = widget_label(box4_base,$
                                 VALUE='3.  Determine Shift',$
                                 FONT=buttonfont,$
                                 /ALIGN_LEFT)


               state.w.shift = widget_label(box4_base,$
                                            /ALIGN_LEFT,$
                                            /DYNAMIC_RESIZE,$
                                            FONT=buttonfont,$
                                            VALUE='Shift: 0.0')
            
            shift = widget_button(box4_base,$
                                  VALUE='Get Shift',$
                                  UVALUE='Get Shift',$
                                  FONT=buttonfont)

          box5_base = widget_base(col2_base,$
                                  /COLUMN,$
                                  FRAME=2)

             label = widget_label(box5_base,$
                                  VALUE='4.  Write File',$
                                  FONT=buttonfont,$
                                  /ALIGN_LEFT)

            oname = coyote_field2(box5_base,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE='Object File:',$
                                  UVALUE='Object File Oname',$
                                  xsize=18,$
                                  textID=textid)
            state.w.objoname_fld = [oname,textid]
            

            addoutput_bg = cw_bgroup(box5_base,$
                                     FONT=buttonfont,$
                                     ['Telluric','A0 V'],$
                                     /ROW,$
                                     /RETURN_NAME,$
                                     /NONEXCLUSIVE,$
                                     LABEL_LEFT='Additional:',$
                                     UVALUE='Additional Output',$
                                     SET_VALUE=[1,0])

            write = widget_button(box5_base,$
                                  VALUE='Write File',$
                                  UVALUE='Write File',$
                                  FONT=buttonfont)

   help = widget_button(state.w.xtellcor_general_base,$
                        VALUE='Help',$
                        UVALUE='Help',$
                        FONT=buttonfont)
      
; Get things running.  Center the widget using the Fanning routine.
            
cgcentertlb,state.w.xtellcor_general_base
widget_control, state.w.xtellcor_general_base, /REALIZE

; Start the Event Loop. This will be a non-blocking program.

XManager, 'xtellcor_general', $
  state.w.xtellcor_general_base, $
  CLEANUP='xtellcor_general_cleanup',$
  /NO_BLOCK

widget_control, state.w.xtellcor_general_base, SET_UVALUE=state, /NO_COPY

end
