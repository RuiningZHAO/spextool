;+
; NAME:
;     xtellcor
;    
; PURPOSE:
;     Runs the SpeX telluric correction.
;    
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     xtellcor
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
;     Writes a SpeX spectral FITS file to disk of the telluric
;     corrected spectra and optional the telluric correction spectrum
;     and the convolved Vega model.
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
;     2008-02-13 - Removed output as a text file as an option.
;-
;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro xtellcor_event, event

  widget_control, event.id,  GET_UVALUE=uvalue
  if uvalue eq 'Quit' then begin
     
     widget_control, event.top, /DESTROY
     goto, getout
     
  endif
  
  
  widget_control, event.top, GET_UVALUE=state, /NO_COPY
  widget_control, /HOURGLASS
  
  case uvalue of
     
     'Additional Output': begin
        
        if event.value eq 'Telluric' then state.r.telluricoutput=event.select
        if event.value eq 'Model' then state.r.vegaoutput=event.select
        
     end
     
     'B Magnitude Field': mc_setfocus, state.w.vmag_fld
     
     'Construct Kernel': xtellcor_conkernel,state
     
     'Construct Telluric Spectra': xtellcor_contellspec,state
     
     'Get Shift': xtellcor_getshift,state
     
     'Help': begin

        pre = (strupcase(!version.os_family) eq 'WINDOWS') ? 'start ':'open '

        spawn, pre+filepath('spextoolmanual.pdf', $
                            ROOT=state.r.spextoolpath,$
                            SUBDIR='manuals')
        
     end
          
     'Load Spectra': xtellcor_loadspec,state
     
     'Method': begin
        
        state.r.method = event.value
        
        widget_control, state.w.box2a_base,MAP=0
        if event.value eq 'Deconvolution' then widget_control, $
           state.w.box2a_base,MAP=1
        
     end
     
     'Object Spectra Button': begin
        
        obj = dialog_pickfile(DIALOG_PARENT=state.w.xtellcor_base,$
                              PATH=state.r.objpath,/MUST_EXIST, $
                              GET_PATH=path,FILTER='*.fits')
        if obj eq '' then goto, cont
        widget_control,state.w.objspectra_fld[1], $
                       SET_VALUE=strmid(obj[0],strpos(obj,state.w.dirsep, $
                                                      /REVERSE_S)+1)
        state.r.objpath = path
        mc_setfocus,state.w.objspectra_fld
        
     end
     
     'Plot Object Ap': begin
        
        if state.r.continue lt 4 then begin
           
           ok = dialog_message('Previous steps not complete.',/ERROR,$
                               DIALOG_PARENT=state.w.xtellcor_base)
           goto, cont
           
        endif
        
        state.r.shiftobjap = event.index
        z = where(*state.d.objorders eq state.r.shiftobjorder)
        widget_control, state.w.shift,SET_VALUE='Shift: '+ $
                        string((*state.r.shift)[z,state.r.shiftobjap], $
                               FORMAT='(f6.2)')
        
     end
     
     'Plot Object Order': begin
        
        if state.r.continue lt 4 then begin
           
           ok = dialog_message('Previous steps not complete.',/ERROR,$
                               DIALOG_PARENT=state.w.xtellcor_base)
           goto, cont
           
        endif
        
        state.r.shiftobjorder = (*state.d.objorders)[event.index]
        widget_control, state.w.shift,SET_VALUE='Shift: '+ $
                        string((*state.r.shift)[event.index, $
                                                state.r.shiftobjap], $
                               FORMAT='(f6.2)')
        
     end
     
     'Propagate Std Uncertainty': state.r.propstdunc = event.select
     
     'Scale Lines': xtellcor_getscales,state
     
     'Spectrum Units': begin
        
        case event.index of 
           
           0: state.r.units = 'ergs s-1 cm-2 A-1'
           1: state.r.units = 'ergs s-1 cm-2 Hz-1'
           2: state.r.units = 'W m-2 um-1'
           3: state.r.units = 'W m-2 Hz-1'
           4: state.r.units = 'Jy'
           
        endcase 
        
     end
     
     'Standard Order': state.r.stdorder = (*state.d.stdorders)[event.index]
     
     'Standard Spectra Button': begin
        
        std = dialog_pickfile(DIALOG_PARENT=state.w.xtellcor_base,$
                              PATH=state.r.stdpath,/MUST_EXIST, $
                              GET_PATH=path,FILTER='*.fits')
        
        if std eq '' then goto, cont
        widget_control,state.w.stdspectra_fld[1],$
               SET_VALUE = strmid(std[0],strpos(std,state.w.dirsep,/REVERSE_S)+1)
        state.r.stdpath = path
        mc_setfocus,state.w.stdspectra_fld
        mc_setfocus,state.w.bmag_fld
        
     end
     
     'Standard Spectra Field': mc_setfocus,state.w.bmag_fld
     
     'V Magnitude Field': mc_setfocus, state.w.objspectra_fld
     
     'Write File': xtellcor_writefile,state
     
     else:
     
  endcase
  
;  Put state variable into the user value of the top level base.
   
  cont: 
  widget_control, state.w.xtellcor_base, SET_UVALUE=state, /NO_COPY
  getout:
  
end
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xtellcor_cleanup,base

  widget_control, base, GET_UVALUE = state, /NO_COPY
  if n_elements(state) ne 0 then begin
     
     ptr_free, state.r.scales
     ptr_free, state.r.shift
     ptr_free, state.d.kernels
     ptr_free, state.d.objhdr
     ptr_free, state.d.objorders
     ptr_free, state.d.objspec
     ptr_free, state.d.nstd
     ptr_free, state.d.stddisp
     ptr_free, state.d.stdfwhm
     ptr_free, state.d.stdhdr
     ptr_free, state.d.stdorders
     ptr_free, state.d.stdspec
     ptr_free, state.d.tellspec
     ptr_free, state.d.fvega
     ptr_free, state.d.wvega
     ptr_free, state.d.vegaspec
     ptr_free, state.d.cfvega
     ptr_free, state.d.cf2vega
     
     cutreg = *state.r.cutreg
     for i = 0, n_tags(cutreg)-1 do ptr_free, cutreg.(i)
     
     ptr_free, state.r.cutreg
     
     
  endif
  state = 0B
  
end
;
;******************************************************************************
;
pro xtellcor_changeunits,wave,tellspec,tellspec_error,scvega,state

  ang = '!5!sA!r!u!9 %!5!n'
  case state.r.units of 
     
     'ergs s-1 cm-2 A-1': state.r.nytitle = $
        '!5f!D!7k!N!5 (ergs s!E-1!N cm!E-2!N '+ang+'!E-1!N'
     
     'ergs s-1 cm-2 Hz-1': begin
        
        tellspec = mc_chfunits(wave,tellspec,0,1,3,IERROR=tellspec_error, $
                               OERROR=tellspec_error)
        scvega = mc_chfunits(wave,scvega,0,1,3)
        
        state.r.nytitle = '!5f!D!7m!N!5 (ergs s!E-1!N cm!E-2!N Hz!E-1!N'
        
        
     end
     'W m-2 um-1': begin 
        
        tellspec = mc_chfunits(wave,tellspec,0,1,0,IERROR=tellspec_error, $
                               OERROR=tellspec_error)
        scvega   = mc_chfunits(wave,scvega,0,1,0)

        state.r.nytitle = '!5f!D!7k!N!5 (W m!E-2!N !7l!5m!E-1!N'
        
     end
     'W m-2 Hz-1': begin

        tellspec = mc_chfunits(wave,tellspec,0,1,2, IERROR=tellspec_error, $
                               OERROR=tellspec_error)
        scvega   = mc_chfunits(wave,scvega,0,1,2)

        state.r.nytitle = '!5f!D!7m!N!5 (W m!E-2!N Hz!E-1!N' 
        
     end
     
     'Jy': begin
        
        tellspec = mc_chfunits(wave,tellspec,0,1,4,IERROR=tellspec_error, $
                               OERROR=tellspec_error)
        scvega   = mc_chfunits(wave,scvega,0,1,4)

        state.r.nytitle = '!5f!D!7m!N!5 (Jy' 
        
     end
     
  endcase
  
end
;
;******************************************************************************
;
pro xtellcor_conkernel,state

  if state.r.continue lt 1 then begin
     
     ok = dialog_message('Previous steps not complete.',/ERROR,$
                         DIALOG_PARENT=state.w.xtellcor_base)
     return
     
  endif
  
  vdisp = (state.d.stdobsmode eq 'LowRes15') ? 2.96495e-4:1.49774e-5
  
  if state.r.method eq 'Deconvolution' then begin
     
     idx = where(*state.d.stdorders eq state.r.stdorder)
     
     xmc_conkern,(*state.d.stdspec)[*,0,idx],(*state.d.stdspec)[*,1,idx],$
                 *state.d.wvega,*state.d.fvega,*state.d.cfvega, $
                 *state.d.cf2vega,*state.d.awave,*state.d.atrans,wline, $
                 kernel,scale,vshift,maxdev,rmsdev, $
                 PARENT=state.w.xtellcor_base,XTITLE=state.r.xtitle,$
                 YTITLE=state.r.ytitle,CANCEL=cancel
     if cancel then return
     
;  Now we must scale the kernel to each order.
;  First, get this kernel in SpeX pixels.
     
     ndat = n_elements(kernel)
     kx_vegapix = findgen(ndat)
     result = gaussfit(kx_vegapix,kernel,a,NTERMS=3)
     
     kx_spexpix = (kx_vegapix - a[1])*vdisp/total((*state.d.stddisp)[idx])
     
;  Now construct the kernels for the other orders.
     
     for i = 0, state.d.stdnorders-1 do begin
        
        disp = total((*state.d.stddisp)[i])
        kx_spexwave = kx_spexpix*total((*state.d.stddisp)[i])
        
        del = max(kx_spexwave,MIN=min)-min
        npix = del/vdisp
        if not npix mod 2 then npix = npix + 1
        kx_vegawave = (findgen(npix)-npix/2.)*vdisp
        
        linterp,kx_spexwave,kernel,kx_vegawave,nkern
        
        nkern = nkern/total(nkern)
        key = 'Order'+string((*state.d.stdorders)[i],FORMAT='(i2.2)')
        str = (i eq 0) ? create_struct(key,nkern): $
              create_struct(str,key,nkern)
        
     endfor
     
     
     state.r.vshift   = vshift
     state.r.scale    = scale
     *state.d.kernels = str
     state.r.maxdev   = maxdev
     state.r.rmsdev   = rmsdev
     
  endif 
  
  if state.r.method eq 'IP' then begin
     
;  Construct the kernels for each order
     
     case state.d.slitw_arc of 
        
        0.3: parms = state.r.ipcoeffs_sw03
        
        0.5: parms = state.r.ipcoeffs_sw05
        
        0.8: parms = state.r.ipcoeffs_sw08
        
        1.6: parms = state.r.ipcoeffs_sw16
        
        3.0: parms = state.r.ipcoeffs_sw30
        
     endcase
     
     for i = 0, state.d.stdnorders-1 do begin

        vkernw_pix = (*state.d.stdfwhm)[i]/vdisp
        
        nkernel = round(10.*vkernw_pix)
        if not nkernel mod 2 then nkernel = nkernel + 1
        
        kernel_vx = (findgen(nkernel)-fix(nkernel)/2)*vdisp
        kernel_sx = kernel_vx/(*state.d.stddisp)[i]

        kern   = mc_instrprof(kernel_sx,parms,CANCEL=cancel)
        if cancel then return
        
        key = 'Order'+string((*state.d.stdorders)[i],FORMAT='(i2.2)')
        str1 = (i eq 0) ? create_struct(key,kern): $
               create_struct(str1,key,kern)
        
     endfor
     
     *state.d.kernels = str1
     state.r.vshift   = 0.0
     state.r.scale    = 1.0
     
  endif
  
  state.r.continue = 2

end
;
;******************************************************************************
;
pro xtellcor_contellspec,state

  if state.r.continue lt 3 then begin
     
     ok = dialog_message('Previous steps not complete.',/ERROR,$
                         DIALOG_PARENT=state.w.xtellcor_base)
     return
     
  endif
  
  norders = fxpar(*state.d.objhdr,'NORDERS')
  match,*state.d.stdorders,*state.d.objorders,stdidx
  
  *state.d.tellspec = (*state.d.stdspec)[*,*,stdidx]
  *state.d.vegaspec = (*state.d.stdspec)[*,*,stdidx]
  (*state.d.vegaspec)[*,2,*] = 1.0
  
  for i = 0, norders-1 do begin
     
     print, 'Constructing Telluric Correction Spectrum for Order '+$
            strtrim((*state.d.objorders)[i],2)+'...'
     
     z = where(*state.d.stdorders eq (*state.d.objorders)[i],count)
     mc_mktellspec, (*state.d.stdspec)[*,0,z],(*state.d.stdspec)[*,1,z],$
                    (*state.d.stdspec)[*,2,z],state.r.vmag, $
                    (state.r.bmag-state.r.vmag),(*state.d.kernels).(i), $
                    (*state.r.scales)[*,z],*state.d.wvega,*state.d.fvega, $
                    *state.d.cfvega,*state.d.cf2vega,state.r.vshift,$
                    tellcor,tellcor_error,scvega,CANCEL=cancel

;  Perform interpolations if necessary
     
     cutreg = *state.r.cutreg
     ndat = n_elements(*cutreg.(i))
     
     if ndat ne 1 then begin
        
        nreg = ndat/2

        stdwave = (*state.d.stdspec)[*,0,z]
        stdflux = (*state.d.stdspec)[*,1,z]
        stderr  = (*state.d.stdspec)[*,2,z]

        nonan = mc_nantrim(stdwave,3)
        tstdwave = stdwave[nonan]
        tstdflux = stdflux[nonan]
        tstderr  = stderr[nonan]
        ttellcor = tellcor[nonan]
        ttellcor_error = tellcor_error[nonan]


        for j = 0, nreg-1 do begin
           
           xrange = reform((*cutreg.(i))[(j*2):(j*2+1)])
           tabinv,tstdwave,xrange,idx
           idx = round(idx)
           
           x = [tstdwave[idx[0]],tstdwave[idx[1]]]
           y = [ttellcor[idx[0]],ttellcor[idx[1]]]
           e = [ttellcor_error[idx[0]],ttellcor_error[idx[1]]]

           coeff  = poly_fit1d(x,y,1,/SILENT)
           coeffe = poly_fit1d(x,e,1,/SILENT)
           
           ttellcor[idx[0]:idx[1]]=poly(tstdwave[idx[0]:idx[1]],coeff)
           tellcor_error[idx[0]:idx[1]]=poly(tstdwave[idx[0]:idx[1]],coeffe)
           
        endfor
        tellcor[nonan] = ttellcor
        tellcor_error[nonan] = ttellcor_error
        
     endif
     
     xtellcor_changeunits,(*state.d.stdspec)[*,0,z],tellcor,tellcor_error, $
                          scvega,state
     (*state.d.tellspec)[*,1,i] = tellcor
     (*state.d.tellspec)[*,2,i] = tellcor_error
     (*state.d.vegaspec)[*,1,i] = scvega
     
  endfor
  
  state.r.continue = 4

end
;
;******************************************************************************
;
pro xtellcor_getscales,state

  if state.r.continue lt 2 then begin
     
     ok = dialog_message('Previous steps not complete.',/ERROR,$
                         DIALOG_PARENT=state.w.xtellcor_base)
     return
     
  endif
  
  vshift = state.r.vshift
  IP     = (state.r.method eq 'IP') ? 1:0
  
  wvega  = *state.d.wvega
  fvega  = *state.d.fvega
  fcvega = *state.d.cfvega
  
;  if state.d.stdobsmode eq 'ShortXD' then xputs = state.d.xputs
  
  match,*state.d.stdorders,*state.d.objorders,stdidx,objidx
  
  xmc_scalelines,(*state.d.stdspec)[*,*,stdidx],(*state.d.stdorders)[stdidx],$
                 state.r.vmag,(state.r.bmag-state.r.vmag),wvega,fvega,fcvega, $
                 *state.d.cf2vega,*state.d.kernels,vshift,*state.d.objspec, $
                 *state.d.objorders,state.d.objnaps,*state.d.awave, $
                 *state.d.atrans,state.r.hlines,state.r.hnames,state.r.scale, $
                 scales,cutreg,PARENT=state.w.xtellcor_base, $
                 XTITLE=state.r.xtitle,YTITLE=state.r.ytitle,CANCEL=cancel
  
  if not cancel then begin
     
     *state.r.scales = scales
     *state.r.cutreg = cutreg
     state.r.continue = 3
     
     state.r.vshift = vshift
     state.r.vrot   = 0.0
     
  endif

end
;
;******************************************************************************
;
pro xtellcor_loadspec,state

;  Get files.

  std = mc_cfld(state.w.stdspectra_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return
  std = mc_cfile(state.r.stdpath+std,WIDGET_ID=state.w.xtellcor_base, $
                 CANCEL=cancel)
  if cancel then return
  
  bmag = mc_cfld(state.w.bmag_fld,4,/EMPTY,CANCEL=cancel)
  if cancel then return
  vmag = mc_cfld(state.w.vmag_fld,4,/EMPTY,CANCEL=cancel)
  if cancel then return
  
  obj = mc_cfld(state.w.objspectra_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return
  obj = mc_cfile(state.r.objpath+obj,WIDGET_ID=state.w.xtellcor_base, $
                 CANCEL=cancel)
  if cancel then return
  
;  Read spectra and load data.
  
  mc_readspec,std,std,stdhdr,stdobsmode,start,stop,stdnorders, $
              stdnaps,stdorders,stdxunits,stdyunits,slith_pix,slith_arc, $
              slitw_pix,slitw_arc,stdrp,stdairmass,xtitle,ytitle,CANCEL=cancel
  if cancel then return

  std = mc_caaspecflags(std,CANCEL=cancel)
  if cancel then return
  
  
  mc_readspec,obj,obj,objhdr,objobsmode,start,stop,objnorders, $
              objnaps,objorders,objxunits,objyunits,slith_pix,slith_arc, $
              slitw_pix,slitw_arc,objrp,objairmass,CANCEL=cancel
  if cancel then return  

  obj = mc_caaspecflags(obj,CANCEL=cancel)
  if cancel then return
  
  *state.d.stdspec   = std
  *state.d.stdorders = stdorders
  state.r.stdorder   = stdorders[0]
  *state.d.stdhdr    = stdhdr
  state.d.slitw_pix  = slitw_pix
  state.d.slitw_arc  = slitw_arc
  state.d.stdobsmode = strtrim(stdobsmode,2)
  *state.d.stddisp   = mc_fxpar(stdhdr,'DISP*')
  *state.d.stdfwhm   = *state.d.stddisp*state.d.slitw_pix
  state.d.stdnorders = stdnorders
  
  *state.r.shift = fltarr(objnorders,objnaps)
  
  *state.d.objspec   = obj
  *state.d.objorders = objorders
  *state.d.objhdr    = objhdr
  state.d.objnaps    = objnaps
  
  state.r.bmag = bmag
  state.r.vmag = vmag
  state.r.vshift = 0.0
  state.r.stdairmass = stdairmass
  state.d.dairmass = stdairmass-objairmass

  state.r.xtitle = xtitle
  state.r.ytitle = ytitle
  
;  Compute airmass difference between obj and std.

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
  widget_control, state.w.stdorder_dl,$
                  SET_VALUE=string(stdorders,FORMAT='(i2.2)')
  state.r.shiftobjorder = (*state.d.objorders)[0]
  
;  Load Vega model

  if state.d.stdobsmode eq 'LowRes15' then begin 
     
     restore, filepath('lvega99.sav',ROOT_DIR=state.r.spextoolpath,$
                       SUBDIR='data') 
     
  endif else restore, filepath('lvega5.sav',ROOT_DIR=state.r.spextoolpath, $
                               SUBDIR='data') 
  
  *state.d.wvega = wvin/10000.
  *state.d.cf2vega = fc2vin
  *state.d.cfvega = fcvin
  *state.d.fvega = fvin

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


  state.r.continue=1

end
;
;******************************************************************************
;
pro xtellcor_getshift,state

  if state.r.continue lt 4 then begin
     
     ok = dialog_message('Previous steps not complete.',/ERROR,$
                         DIALOG_PARENT=state.w.xtellcor_base)
     return
     
  endif
  
  zobj = where((*state.d.objorders) eq state.r.shiftobjorder)
  
  obj_wave = (*state.d.objspec)[*,0,zobj*state.d.objnaps+state.r.shiftobjap]
  obj_flux = (*state.d.objspec)[*,1,zobj*state.d.objnaps+state.r.shiftobjap]
  
  trim = mc_nantrim(obj_wave,2)
  obj_wave = obj_wave[trim]
  obj_flux = obj_flux[trim]

  
  zstd = where((*state.d.stdorders) eq state.r.shiftobjorder)

  tel_wave = (*state.d.tellspec)[*,0,zstd]
  tel_flux = (*state.d.tellspec)[*,1,zstd]

  trim = mc_nantrim(tel_wave,2)  
  tel_wave = tel_wave[trim]
  tel_flux = tel_flux[trim]

  mc_interpspec,tel_wave,tel_flux,obj_wave,new_flux,CANCEL=cancel
  if cancel then return
  
  shift = xmc_findshift(obj_wave,obj_flux,new_flux,*state.d.awave, $
                        *state.d.atrans, $
                        INITSHIFT=total((*state.r.shift)[zobj, $
                                                         state.r.shiftobjap]),$
                        XTITLE=state.r.xtitle,PARENT=state.w.xtellcor_base, $
                        CANCEL=cancel)
  if cancel then return

  if not cancel then begin
     
     (*state.r.shift)[zobj,state.r.shiftobjap] = shift
     widget_control, state.w.shift,SET_VALUE='Shift: '+ $
                     string(shift,FORMAT='(f6.2)')
     
  endif
  
  state.r.continue=5
  
end
;
;******************************************************************************
;
pro xtellcor_writefile,state

  if state.r.continue lt 4 then begin
     
     ok = dialog_message('Previous steps not complete.',/ERROR,$
                         DIALOG_PARENT=state.w.xtellcor_base)
     return
     
  endif
  
  obj = mc_cfld(state.w.objoname_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return
  
  stdfile = mc_cfld(state.w.stdspectra_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return
  
;
;  Write the telluric correction spectrum to disk
;
  
  if state.r.telluricoutput then begin
     
;  Get info from obj and std hdr.
     
     orders    = fxpar(*state.d.objhdr,'ORDERS',COMMENT=corders)
     norders   = fxpar(*state.d.objhdr,'NORDERS',COMMENT=cnorders)
     obsmode   = fxpar(*state.d.stdhdr,'MODENAME',COMMENT=cobsmode)
     std       = fxpar(*state.d.stdhdr,'OBJECT')
     xunits    = fxpar(*state.d.stdhdr,'XUNITS',COMMENT=cxunits)
     xtitle    = fxpar(*state.d.stdhdr,'XTITLE',COMMENT=cxtitle,COUNT=count)
     
;  For backwards compatibility
     
     if count eq 0 then begin
        
        xtitle  = '!7k!5 ('+strtrim(xunits,2)+')'
        cxtitle = 'IDL X Title'
        
     endif
     
;  Create hdr for the telluric correction spectrum
     
     fxhmake,hdr,*state.d.tellspec
     fxaddpar,hdr,'IRAFNAME',strtrim(obj+'_tellspec.fits',2)
     fxaddpar,hdr,'ORDERS',orders,corders
     fxaddpar,hdr,'NORDERS',norders,cnorders
     fxaddpar,hdr,'A0VStd',std,' Tellluric Correction A0 V Standard'
     fxaddpar,hdr,'A0VBmag',state.r.bmag,' B-band magnitude'
     fxaddpar,hdr,'A0VVmag',state.r.vmag,' V-band magnitude'
     fxaddpar,hdr,'GRAT',obsmode,cobsmode 
     fxaddpar,hdr,'NAPS',1, ' Number of apertures'
     fxaddpar,hdr,'AIRMASS',state.r.stdairmass, 'Average airmass'

     fxaddpar,hdr,'DAIRMASS',state.d.dairmass, $
              ' Average of (std-obj) airmass'
     fxaddpar,hdr,'TELMETH',strtrim(state.r.method,2), $
              ' Telluric Correction Method'
     fxaddpar,hdr,'Vegadv',state.r.vshift, ' Vega velocity shift in km s-1'
     
     
     history = 'These telluric correction spectra were constructed from the '+$
               'spectra '+strtrim(stdfile,2)+'.  The velocity shift of' + $
               ' Vega is '+strtrim(state.r.vshift,2)+' km s-1.  '
     
     if state.r.method eq 'Deconvolution' then begin

        fxaddpar,hdr,'TCMaxDev',state.r.maxdev*100, $
                 ' The maxmimum % deviation of Vega-data'
        
        fxaddpar,hdr,'TCRMSDev',state.r.rmsdev*100, $
                 ' The RMS % deviation of Vega-model'
        
        history = history+'The Vega model was modified using the ' + $
                  'deconvolution method.  The maximum deviation and ' + $
                  'rms deviation between the modified Vega model and the ' + $
                  'data over the kernel wavelength range are '+ $
                  strtrim(state.r.maxdev*100.0,2)+'% and '+ $
                  strtrim(state.r.rmsdev*100.0,2)+'%, respectively.'
        
     endif else begin
        
        history = history+' The Vega model was modified using the IP method.'
        
     endelse

     fxaddpar,hdr,'XUNITS',xunits,cxunits
     fxaddpar,hdr,'XTITLE',xtitle,cxtitle
     fxaddpar,hdr,'YUNITS',strcompress(state.r.units,/RE)+'/DNs-1', $
              ' Units of the Y axis'
     fxaddpar,hdr,'YTITLE',state.r.nytitle+' / DN s!U-1!N)','IDL Y title'
     
     hdr = mc_addhistlabel(hdr,'Xtellcor History',CANCEL=cancel)
     if cancel then return

     history = mc_splittext(history,70,CANCEL=cancel)
     if cancel then return
     sxaddhist,history,hdr
     
;  Write it  to disk
     
     writefits,state.r.objpath+obj+'_tellspec.fits',*state.d.tellspec,hdr

     print
     print,'Wrote the telluric correction spectrum to '+ $
           strtrim(state.r.objpath+obj,2)+'_tellspec'
     print

  endif
  
;
;  Write the convolved resampled Vega spectrum to disk
;
  
  if state.r.vegaoutput then begin
     
;  Get info from std hdr.
    
     orders    = fxpar(*state.d.objhdr,'ORDERS',COMMENT=corders)
     norders   = fxpar(*state.d.objhdr,'NORDERS',COMMENT=cnorders)
     obsmode   = fxpar(*state.d.stdhdr,'GRAT',COMMENT=cobsmode)
     std       = fxpar(*state.d.stdhdr,'OBJECT')
     xunits    = fxpar(*state.d.stdhdr,'XUNITS',COMMENT=cxunits)
     xtitle    = fxpar(*state.d.stdhdr,'XTITLE',COMMENT=cxtitle)
     
;  Create FITS header for the Vega spectrum
     
     delvarx,hdr
     fxhmake,hdr,*state.d.vegaspec
     
     fxaddpar,hdr,'IRAFNAME',strtrim(obj+'_modvega.fits',2)
     fxaddpar,hdr,'ORDERS',orders,corders
     fxaddpar,hdr,'NORDERS',norders,cnorders
     fxaddpar,hdr,'OBJECT','Convolved Vega Spectrum'
     fxaddpar,hdr,'GRAT',obsmode,cobsmode 
     fxaddpar,hdr,'NAPS',1, ' Number of apertures'
          
     history = 'This Vega spectrum has been scaled to a V magnitude of '+$
               strtrim(state.r.vmag,2)+', shifted by '+ $
               strtrim(state.r.vshift,2)+$
               ' km s-1, convolved with the kernel and resampled '+$
               'onto the wavelength grid of '+strtrim(stdfile,2)+'.'
     
     if state.r.method eq 'Deconvolution' then begin
        
        fxaddpar,hdr,'TCMaxDev',state.r.maxdev*100, $
                 ' The maxmimum % deviation of Vega-data'
        
        fxaddpar,hdr,'TCRMSDev',state.r.rmsdev*100, $
                 ' The RMS % deviation of Vega-data'
        
        history = history+'  The Vega model was modified using the ' + $
                  'deconvolution method.  The maximum deviation and ' + $
                  'rms deviation between the modified Vega model and the ' + $
                  'data over the kernel wavelength range are '+ $
                  strtrim(state.r.maxdev*100.0,2)+'% and '+ $
                  strtrim(state.r.rmsdev*100.0,2)+'%, respectively.'
        
     endif else begin
        
        history = history+' The Vega model was modified using the IP method.'
        
     endelse

     fxaddpar,hdr,'XUNITS',xunits,cxunits
     fxaddpar,hdr,'XTITLE',xtitle,cxtitle
     fxaddpar,hdr,'YUNITS',strcompress(state.r.units,/RE), 'Units of the Y axis'
     fxaddpar,hdr,'YTITLE',state.r.nytitle+')','IDL Y title'


     hdr = mc_addhistlabel(hdr,'Xtellcor History',CANCEL=cancel)
     if cancel then return

     history = mc_splittext(history,70,CANCEL=cancel)
     if cancel then return
     sxaddhist,history,hdr
     
     writefits,state.r.objpath+obj+'_modVega.fits',*state.d.vegaspec,hdr
     
     print
     print,'Wrote the Vega spectrum to '+strtrim(state.r.objpath+obj,2)+'_vega'
     print

  endif
  
;  Write the telluric corrected object spectrum to disk.  First
;  telluric correct the object spectra
  
  corspec  = *state.d.objspec
  for i = 0, n_elements(*state.d.objorders)-1 do begin
     
     z = where((*state.d.stdorders) eq (*state.d.objorders)[i],count)
     if count eq 0 then goto, cont
     
     for j = 0, state.d.objnaps-1 do begin
        
        k = i*state.d.objnaps+j
        
;  Interpolate telluric spectrum onto object wavelength sampling.
        
        mc_interpspec,(*state.d.tellspec)[*,0,i],(*state.d.tellspec)[*,1,i],$
                   (*state.d.objspec)[*,0,k],nflux,nerror,$
                   IYERROR=(*state.d.tellspec)[*,2,i],CANCEL=cancel
        if cancel then return

;  Interpolate mask into object wavelength sampling

        mc_interpflagspec,(*state.d.tellspec)[*,0,i],$
                          byte((*state.d.tellspec)[*,3,i]),$
                          (*state.d.objspec)[*,0,k],nflag,CANCEL=cancel
        if cancel then return

        
;  Now shift spectrum.
        
        x = findgen(n_elements((*state.d.objspec)[*,0,k]))
        mc_interpspec,x+(*state.r.shift)[i,j],nflux,x,nnflux,nnerror, $
                      IYERROR=nerror,CANCEL=cancel
        if cancel then return

        mc_interpflagspec,x+(*state.r.shift)[i,j],byte(nflag),x,nnflag,$
                          CANCEL=cancel
        if cancel then return
        
        corspec[*,1,k] = nnflux*(*state.d.objspec)[*,1,k]

        if state.r.propstdunc then begin

           corspec[*,2,k] = sqrt( nnflux^2 * (*state.d.objspec)[*,2,k]^2 + $
                                  (*state.d.objspec)[*,1,k]^2 * nnerror^2 )
        
        endif else corspec[*,2,k] = nnflux * (*state.d.objspec)[*,2,k]

        result = mc_combflagstack(byte([[[(*state.d.objspec)[*,3,k]]], $
                                   [[nnflag]]]),CANCEL=cancel)
        if cancel then return
        
        corspec[*,3,k] = result
        
     endfor
     
     cont:
     
  endfor
  
;  Now write it to disk
  
  std       = fxpar(*state.d.stdhdr,'OBJECT')
  hdr = *state.d.objhdr
  sxdelpar,hdr,['DIVISOR','BEAM','GRSTCNT','CALMIR','DIT','OSF','QTH_LAMP',$
                'INC_LAMP','IR_SRC','ARG_SRC','SHUTTER','AIMAGE','SKY']
  
  fxaddpar,hdr,'IRAFNAME',strtrim(obj+'.fits',2)
  fxaddpar,hdr,'YUNITS',strcompress(state.r.units,/RE), 'Units of the Y axis'
  fxaddpar,hdr,'YTITLE',state.r.nytitle+')',' IDL Y title'
  fxaddpar,hdr,'A0VStd',std,' Tellluric Correction A0 V Standard'
  fxaddpar,hdr,'A0VBmag',state.r.bmag,' B-band magnitude'
  fxaddpar,hdr,'A0VVmag',state.r.vmag,' V-band magnitude'
  fxaddpar,hdr,'DAIRMASS',state.d.dairmass, $
           ' Average of (std-obj) airmass'
  fxaddpar,hdr,'TELMETH',strtrim(state.r.method,2), $
           ' Telluric Correction Method'
  fxaddpar,hdr,'Vegadv',state.r.vshift, ' Vega velocity shift in km s-1'
  
  history = 'This spectrum was telluric corrected using the telluric '+$
            'correction spectra '+strtrim(obj,2)+'_tellspec.fits.  ' + $
            'The velocity shift of Vega is '+strtrim(state.r.vshift,2)+ $
            ' km s-1.'
  
  if state.r.method eq 'Deconvolution' then begin
     
     fxaddpar,hdr,'TCMaxDev',state.r.maxdev*100, $
              ' The maxmimum % deviation of Vega-data'

     fxaddpar,hdr,'TCRMSDev',state.r.rmsdev*100, $
              ' The RMS % deviation of Vega-data'

     history = history+'  The Vega model was modified using the ' + $
               'deconvolution method.  The maximum deviation and ' + $
               'rms deviation between the modified Vega model and the ' + $
               'data over the kernel wavelength range are '+ $
               strtrim(state.r.maxdev*100.0,2)+'% and '+ $
               strtrim(state.r.rmsdev*100.0,2)+'%, respectively.'
     
  endif else begin
     
     history = history+' The Vega model was modified using the IP method.'
     
  endelse
  
  fxaddpar,hdr,'Telfile',obj+'_tellspec.fits',' The telluric correction file'

  
  for i = 0, state.d.objnaps-1 do begin
     
     history = history+'  The telluric correction spectra for aperture '+ $
               string(i+1,FORMAT='(i2.2)')+' were shifted by '+ $
               strjoin(strtrim((*state.r.shift)[*,i],2),', ')+' pixels.'
     
  endfor
 
  hdr = mc_addhistlabel(hdr,'Xtellcor History',CANCEL=cancel)
  if cancel then return
  
  history = mc_splittext(history,70,CANCEL=cancel)
  if cancel then return
  sxaddhist,history,hdr
  
  writefits,state.r.objpath+obj+'.fits',corspec,hdr
  xvspec,state.r.objpath+obj+'.fits',/PLOTLINMAX
  
  print
  print,'Wrote the corrected spectrum to '+strtrim(state.r.objpath+obj,2)
  print

end
;
;******************************************************************************
;
; ------------------------------Main Program---------------------------------- 
;
;******************************************************************************
;
pro xtellcor,instrument,FINISH=finish,BASIC=basic,GENERAL=general

  if keyword_set(FINISH) then begin
     
     xtellcor_finish
     return
     
  endif

  if keyword_set(BASIC) then begin
     
     xtellcor_basic
     return
     
  endif

  if keyword_set(GENERAL) then begin
     
     xtellcor_general
     return
     
  endif

;  Get spextool and instrument information 
  
  mc_getspextoolinfo,spextoolpath,packagepath,instr,notspex,version, $
                     INSTRUMENT=instrument,CANCEL=cancel
  if cancel then return
  
;  Get OS info

  mc_getosinfo,dirsep,strsep,CANCEL=cancel
  if cancel then return
  
;  Get hydrogen lines
  
  readcol,filepath('HI.dat',ROOT_DIR=spextoolpath,SUBDIR='data'),$
         hlines,hnames,FORMAT='D,A',COMMENT='#',DELIMITER='|',/SILENT

;  Get IP profile coefficients

  readcol,filepath('IP_coefficients.dat',ROOT_DIR=packagepath,SUBDIR='data'),$
          tmp,FORMAT='A',COMMENT='#',DELIMITER=':'

  ipcoeffs_sw03 = double(strsplit(tmp[0],' ',/EXTRACT))
  ipcoeffs_sw05 = double(strsplit(tmp[1],' ',/EXTRACT))
  ipcoeffs_sw08 = double(strsplit(tmp[2],' ',/EXTRACT))
  ipcoeffs_sw16 = double(strsplit(tmp[3],' ',/EXTRACT))
  ipcoeffs_sw30 = double(strsplit(tmp[4],' ',/EXTRACT))
                
;  Load color table

  mc_mkct
  
;  Set the fonts

  mc_getfonts,buttonfont,textfont,CANCEL=cancel
  if cancel then return
  
;  Build three structures which will hold important info.

  w = {bmag_fld:[0,0],$
       box2a_base:0L,$
       dirsep:dirsep,$
       fwhm_fld:[0L,0L],$
       message:0L,$
       objap_dl:0L,$
       objoname_fld:[0L,0L],$
       objorder_dl:0L,$
       objspectra_fld:[0,0],$
       shift:0L,$
       stdspectra_fld:[0L,0L],$
       stdorder_dl:0L,$
       vmag_fld:[0,0],$
       xtellcor_base:0L}
  
  r = {bmag:0.,$
       continue:0L,$
       cutreg:ptr_new(2),$
       hlines:hlines,$
       hnames:hnames,$
       maxdev:0.0,$
       method:'Deconvolution',$
       nytitle:'',$
       objpath:'',$
       packagepath:packagepath,$
       spextoolpath:spextoolpath,$
       shiftobjorder:0,$
       shiftobjap:0,$
       propstdunc:1L,$
       rmsdev:0.0,$
       stdorder:0,$
       scale:0.,$
       scales:ptr_new(fltarr(2)),$
       shift:ptr_new(fltarr(2)),$
       spec:'',$
       stdairmass:0.,$
       stdpath:'',$
       ipcoeffs_sw03:ipcoeffs_sw03,$
       ipcoeffs_sw05:ipcoeffs_sw05,$
       ipcoeffs_sw08:ipcoeffs_sw08,$
       ipcoeffs_sw16:ipcoeffs_sw16,$
       ipcoeffs_sw30:ipcoeffs_sw30,$
       vegaoutput:0,$
       telluricoutput:1,$
       units:'ergs s-1 cm-2 A-1',$
       vmag:0.,$
       vrot:0.0,$
       vshift:0.,$
       wline:0.,$
       xtitle:'',$
       ytitle:''}
  
  d = {airmass:'',$
       dairmass:0.0,$
       awave:ptr_new(2),$
       atrans:ptr_new(2),$
       kernels:ptr_new(fltarr(2)),$
       objhdr:ptr_new(fltarr(2)),$
       objorders:ptr_new(fltarr(2)),$
       objnaps:0,$
       objspec:ptr_new(fltarr(2)),$
       nstd:ptr_new(fltarr(2)),$
       slitw_arc:0.,$
       slitw_pix:0.,$
       stddisp:ptr_new(fltarr(2)),$
       stdfwhm:ptr_new(fltarr(2)),$
       stdhdr:ptr_new(fltarr(2)),$
       stdnorders:0,$
       stdobsmode:'',$
       stdorders:ptr_new(fltarr(2)),$
       stdspec:ptr_new(fltarr(2)),$
       tellspec:ptr_new(fltarr(2)),$
       fvega:ptr_new(fltarr(2)),$
       wvega:ptr_new(fltarr(2)),$
       vegaspec:ptr_new(fltarr(2)),$
       cfvega:ptr_new(fltarr(2)),$
       cf2vega:ptr_new(fltarr(2))}
  
;  Load the three structures in the state structure.
  
  state = {w:w,r:r,d:d}

  title = 'Xtellcor '+version+' for '+instr.instr
  
  state.w.xtellcor_base = widget_base(TITLE=title,$
                                      EVENT_PRO='xtellcor_event',$
                                      /COLUMN)
  
     button = widget_button(state.w.xtellcor_base,$
                            FONT=buttonfont,$
                            VALUE='Done',$
                            UVALUE='Quit')
     
     message = widget_draw(state.w.xtellcor_base,$
                           FRAME=2,$
                           XSIZE=10,$
                           YSIZE=25)
     
     row_base = widget_base(state.w.xtellcor_base,$
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
              
                 button = widget_button(row,$
                                        FONT=buttonfont,$
                                        VALUE='Std Spectra',$
                                        UVALUE='Standard Spectra Button')
                 
                 fld = coyote_field2(row,$
                                     LABELFONT=buttonfont,$
                                     FIELDFONT=textfont,$
                                     TITLE=':',$
                                     UVALUE='Standard Spectra Field',$
                                     XSIZE=18,$
 ;                                    VALUE='cspectra11-20.fits',$
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
  ;                                VALUE='7',$
                                     /CR_ONLY,$
                                     TEXTID=textid)
                 state.w.bmag_fld = [fld,textid]
                 
                 fld = coyote_field2(row,$
                                     LABELFONT=buttonfont,$
                                     FIELDFONT=textfont,$
                                     UVALUE='V Magnitude Field',$
                                     XSIZE=6,$
   ;                               VALUE='7',$
                                     TITLE='',$
                                     /CR_ONLY,$
                                     TEXTID=textid)
                 state.w.vmag_fld = [fld,textid]
                 
              row = widget_base(box1_base,$
                                /ROW,$
                                /BASE_ALIGN_CENTER)
              
                 buton = widget_button(row,$
                                       FONT=buttonfont,$
                                       VALUE='Obj Spectra',$
                                       UVALUE='Object Spectra Button')
                 
                 fld = coyote_field2(row,$
                                     LABELFONT=buttonfont,$
                                     FIELDFONT=textfont,$
                                     TITLE=':',$
                                     UVALUE='Object Spectra Field',$
;                                  VALUE='cspectra1-10.fits',$
                                     XSIZE=18,$
                                     /CR_ONLY,$
                                     TEXTID=textid)
                 state.w.objspectra_fld = [fld,textid]
                 
              load = widget_button(box1_base,$
                                   VALUE='Load Spectra',$
                                   FONT=buttonfont,$
                                   UVALUE='Load Spectra')

           box2_base = widget_base(col1_base,$
                                   /COLUMN,$
                                   FRAME=2)
           
              label = widget_label(box2_base,$
                                   VALUE='2.  Construct Convolution Kernel',$
                                   FONT=buttonfont,$
                                   /ALIGN_LEFT)
                     
              box = widget_base(box2_base)
           
                 spex = widget_base(box,$
                                    /COLUMN)
                 
                    if strlowcase(instrument) eq 'spex' or $
                       strlowcase(instrument) eq 'uspex' then begin
                       
                       method_bg = cw_bgroup(spex,$
                                             FONT=buttonfont,$
                                             ['Deconvolution','IP'],$
                                             /ROW,$
                                             /RETURN_NAME,$
                                             /NO_RELEASE,$
                                             /EXCLUSIVE,$
                                             LABEL_LEFT='Method:',$
                                             UVALUE='Method',$
                                             SET_VALUE=0)
                       
                    endif

                    row = widget_base(spex)
                    
                       state.w.box2a_base = widget_base(row,$
                                                        /ROW,$
                                                        /BASE_ALIGN_CENTER,$
                                                        MAP=1)
                       
                       state.w.stdorder_dl = widget_droplist(state.w.box2a_base,$
                                                             FONT=buttonfont,$
                                                             TITLE='Order:',$
                                                             VALUE='01',$
                                                             UVALUE='Standard Order')
                       widget_control, state.w.box2a_base,MAP=1

              button = widget_button(box2_base,$
                                     VALUE='Construct Kernel',$
                                     UVALUE='Construct Kernel',$
                                     FONT=buttonfont)
              
        col2_base = widget_base(row_base,$
                                /COLUMN)
        
           box3_base = widget_base(col2_base,$
                                   /COLUMN,$
                                   FRAME=2)
           
              label = widget_label(box3_base,$
                                   VALUE='3.  Construct Telluric Spectra',$
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
                                   VALUE='4.  Determine Shift',$
                                   FONT=buttonfont,$
                                   /ALIGN_LEFT)
              
              row = widget_base(box4_base,$
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
                                   VALUE='5.  Write File',$
                                   FONT=buttonfont,$
                                   /ALIGN_LEFT)
              
              oname = coyote_field2(box5_base,$
                                    LABELFONT=buttonfont,$
                                    FIELDFONT=textfont,$
                                    TITLE='File Name:',$
                                    UVALUE='Output File Oname',$
                                    xsize=18,$
                                    textID=textid)
              state.w.objoname_fld = [oname,textid]
              
;              bg = cw_bgroup(box5_base,$
;                             FONT=buttonfont,$
;                             ['Propagate Std. Error'],$
;                             /ROW,$
;                             /NONEXCLUSIVE,$
;                             SET_VALUE=r.propstdunc,$
;                             UVALUE='Propagate Std Uncertainty')
              
              
              addoutput_bg = cw_bgroup(box5_base,$
                                       FONT=buttonfont,$
                                       ['Telluric','Model'],$
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
              
  help = widget_button(state.w.xtellcor_base,$
                       VALUE='Help',$
                       UVALUE='Help',$
                       FONT=buttonfont)
  
; Get things running.  Center the widget using the Fanning routine.
            
  cgcentertlb,state.w.xtellcor_base
  widget_control, state.w.xtellcor_base, /REALIZE
  widget_control, message, GET_VALUE=x
  state.w.message = x
  wset, x
  erase, color=196
  
  widget_geom = widget_info(state.w.xtellcor_base, /GEOMETRY)
  widget_control, message, XSIZE=widget_geom.xsize-17
  erase, color=196
    
; Start the Event Loop. This will be a non-blocking program.

  XManager, 'xtellcor', $
            state.w.xtellcor_base, $
            CLEANUP='xtellcor_cleanup',$
            /NO_BLOCK
  
  widget_control, state.w.xtellcor_base, SET_UVALUE=state, /NO_COPY

end
