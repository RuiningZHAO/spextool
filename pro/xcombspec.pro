;+
; NAME:
;     xcombspec,BASIC=basic
;    
; PURPOSE:
;     Combines SpeX spectra.
;    
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     xcombspec
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
;     Writes a SpeX spectra FITS file to disk
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
;     See xcombspec_helpfile.txt in Spextool/helpfiles
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2000       - Written by M. Cushing, Institute for Astronomy, UH
;     2002-07-19 - Heavily modified by M. Cushing
;     2005-04-x  - Added new plotting controls.
;     2005-10-20 - Modified so that NaNs in the spectra are not
;                  removed before the interpolation is performed in
;                  the loadimages program
;     2008-02-13 - Removed output as a text file as an option.
;     2013       - Many modifications
;-
;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro xcombspec_event, event

  widget_control, event.id,  GET_UVALUE = uvalue
  if uvalue eq 'Quit' then begin
     
     
     widget_control, event.top, /DESTROY
     goto, getout
     
  endif
  
  widget_control, event.top, GET_UVALUE = state, /NO_COPY
  widget_control, /HOURGLASS
  
  case uvalue of
     
     'Aperture': begin
        
        if state.w.freeze then goto, cont
        state.r.ap = event.index
        
        z = where((*state.r.orders) eq (*state.r.scaleorder)[state.r.ap])
        widget_control, state.w.scaleorder_dl, SET_DROPLIST_SELECT=total(z)+1
        
        xcombspec_plotupdate,state
        
     end
     
     'Combine Apertures': state.r.combineaps = event.value
     
     'Write File': begin
        
        if state.w.freeze then goto, cont
        xcombspec_combinespec,state,CANCEL=cancel
        if cancel then goto, cont
        xcombspec_writefile,state
        
     end

     'Combination Statistic':  begin
        
        state.r.combinestat = event.index
        sensitive = (event.index le 2) ? 1:0
        widget_control, state.w.rthresh_fld[0], SENSITIVE=sensitive
        if state.w.freeze then goto, cont
        xcombspec_combinespec,state,CANCEL=cancel
        if cancel then goto, cont
        xcombspec_plotupdate,state
        
     end
     
    'Correct Spectral Shape': xcombspec_correctspec,state
     
     'File Name': begin
        
        path =dialog_pickfile(DIALOG_PARENT=state.w.xcombspec_base,$
                              /MUST_EXIST)
        
        if path ne '' then begin
           
           widget_control,state.w.filename_fld[1],SET_VALUE=path
           
        endif
        
     end
     
     'Help': begin

        pre = (strupcase(!version.os_family) eq 'WINDOWS') ? 'start ':'open '
        
        spawn, pre+filepath('spextoolmanual.pdf', $
                            ROOT=state.r.spextoolpath,$
                            SUBDIR='manuals')
        
     end
     
     'Ignore NaNs': begin

        state.r.ignorenans=event.select        
        if state.w.freeze then goto, cont

        xcombspec_combinespec,state,CANCEL=cancel
        if cancel then goto, cont
        xcombspec_plotupdate,state
        
     end
     
     'Input Mode': begin
        
        widget_control, state.w.manual_base,MAP=0
        widget_control, state.w.file_base,MAP=0
        state.r.inputmode = event.value
        if event.value eq 'Text File' then widget_control, state.w.file_base, $
           /MAP
        if event.value eq 'Manual' then widget_control, state.w.manual_base,$
           /MAP
        
     end
     
     'Input Prefix': mc_setfocus, state.w.spemc_cfiles_fld
     
     'Load Spectra': xcombspec_loadspec,state
     
     'Scale Order': begin

        if state.w.freeze then goto, cont
        state.r.scaleorderidx = event.index

     end

     'Scale Spectra': begin

        if state.w.freeze then goto, cont
        xcombspec_scalespec,state

     end

     'Path Button': begin
        
        path= dialog_pickfile(/DIRECTOR,DIALOG_PARENT=state.w.xcombspec_base,$
                              TITLE='Select Path',/MUST_EXIST)
        
        if path ne '' then begin
           
           path = mc_cpath(path,WIDGET_ID=state.w.xcombspec_base,CANCEL=cancel)
           if cancel then return
           widget_control,state.w.path_fld[1],SET_VALUE = path
           mc_setfocus,state.w.path_fld
           
        endif
        
     end
     
     'Plot Type': begin
        
        state.p.plottype = event.index
        if state.w.freeze then goto, cont
        xcombspec_plotupdate,state
        
     end

     'Prune Order': begin

        if state.w.freeze then goto, cont
        state.r.pruneorderidx = event.index
        
     end

     'Prune Spectra': begin

        if state.w.freeze then goto, cont
        xcombspec_prunespec,state
        
     end

     'Readmode': begin
        
        widget_control, state.w.inprefix_fld[0],  SENSITIVE=0
        if event.value eq 'Filename' then begin
           
           state.r.filereadmode = event.value
           widget_control, state.w.inprefix_fld[0], SENSITIVE=0
           mc_setfocus,state.w.spemc_cfiles_fld
           
        endif else begin
           
           state.r.filereadmode = event.value
           widget_control, state.w.inprefix_fld[0], /SENSITIVE
           mc_setfocus,state.w.inprefix_fld
           
        endelse
        
     end
     
     'Robust Threshold': begin
        
        xcombspec_combinespec,state,CANCEL=cancel
        if cancel then goto, cont
        xcombspec_plotupdate,state
        
     end
     
     'Spectra Files Button': begin
        
        path = mc_cfld(state.w.path_fld,7,CANCEL=cancel)
        if cancel then return
        
        fullpath = dialog_pickfile(DIALOG_PARENT=state.w.xcombspec_base,$
                                   PATH=path,/MUST_EXIST, $
                                   FILTER='*.fits',/MULTIPLE_FILES)
        
        case (size(fullpath))[1] of 
           
           1: begin
              
              if fullpath[0] ne '' then begin
                 
                 widget_control,state.w.spemc_cfiles_fld[1],$
                                SET_VALUE=strmid(fullpath[0],$
                                            strpos(fullpath,'/',/REVERSE_S)+1)
                 mc_setfocus, state.w.outfile_fld
                 
              endif
              
           end
           
           else: begin
              
              for i =0,(size(fullpath))[1]-1 do begin
                 
                 tmp = strmid(fullpath[i],strpos(fullpath[i],'/',$
                                                 /REVERSE_S)+1)
                 arr = (i eq 0) ? tmp:[arr,tmp]
                 
              endfor
              widget_control,state.w.spemc_cfiles_fld[1],$
                             SET_VALUE=strjoin(arr,',',/SINGLE)
              
           end
           
        endcase
        
     end
     
     'Spectra Files Field': mc_setfocus,state.w.outfile_fld
     
     'Spectra Type': begin
        
        state.p.spectype = event.index
        if state.w.freeze then goto, cont
        xcombspec_plotupdate,state
        
     end
     
     else:
     
  endcase

;  Put state variable into the user value of the top level base.
 
cont: 

  widget_control, state.w.xcombspec_base, SET_UVALUE=state, /NO_COPY

getout:
  
end
;
;******************************************************************************
;
pro xcombspec_plotwin_event,event

  widget_control, event.top, GET_UVALUE = state, /NO_COPY
  
  if state.p.plottype eq 1 then begin
     
     idx = floor(event.y/float(state.p.plotwinsize[1])*state.r.norders)
     
     case state.p.spectype of 
        
        0: spec = reform((*state.r.combspec)[*,1,idx])
        
        1: spec = reform((*state.r.combspec)[*,2,idx])
        
        2: spec = reform((*state.r.combspec)[*,1,idx]) / $
                  reform((*state.r.combspec)[*,2,idx])
        
     endcase
     
     xzoomplot,reform((*state.r.combspec)[*,0,idx]),spec
     
  endif
  
  widget_control, event.top, SET_UVALUE=state, /NO_COPY
  
end
;
;******************************************************************************
;
pro xcombspec_resize,event

  widget_control, event.top, GET_UVALUE = state, /NO_COPY
  widget_control, event.id,  GET_UVALUE = uvalue
  
  widget_control, state.w.xcombspec_base, TLB_GET_SIZE=size
  
  state.p.plotwinsize[0]=size[0]-state.p.buffer[0]
  state.p.scrollsize[0] =state.p.plotwinsize[0]
  state.p.scrollsize[1]=size[1]-state.p.buffer[1]
  
  state.p.plotwinsize[1] = state.p.scrollsize[1] > state.p.pixpp*state.r.norders

  xcombspec_modwinsize,state
  xcombspec_plotupdate,state
  
  widget_control, state.w.xcombspec_base, SET_UVALUE=state, /NO_COPY
  
end
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xcombspec_cleanup,xcombspec_base

  widget_control, xcombspec_base, GET_UVALUE = state, /NO_COPY
  if n_elements(state) ne 0 then begin

     ptr_free, state.w.modspeccontinue
     
     ptr_free, state.r.files
     ptr_free, state.r.spcmask
     ptr_free, state.r.pixmask
     ptr_free, state.r.orders
     ptr_free, state.r.corspec
     ptr_free, state.r.scaleorder

     ptr_free, state.d.ospec
     ptr_free, state.d.wspec
     ptr_free, state.d.hdrinfo
     
  endif 
  state = 0B
  
end
;
;******************************************************************************
;
pro xcombspec_combinespec,state,CANCEL=cancel

  cancel = 0

  spc     = *state.d.wspec
  spcmask = *state.r.spcmask
  pixmask = *state.r.pixmask
  
  sspec = (state.r.combineaps eq 'Yes') ? $
          fltarr(state.r.npix,4,state.r.norders):$
          fltarr(state.r.npix,4,state.r.norders*state.r.naps)
  
  if state.r.combinestat le 2 then begin
     
     thresh = mc_cfld(state.w.rthresh_fld,4,/EMPTY,CANCEL=cancel)
     if cancel then return
     
  endif

  for i = 0, state.r.fnaps-1 do begin
     
     for j = 0, state.r.norders-1 do begin
        
        z = where(spcmask[*,j,i] eq 1,count)
        spec = reform(spc[i].(j)[*,1,z])
        err  = reform(spc[i].(j)[*,2,z])
        flag = reform(spc[i].(j)[*,3,z])
        mask = reform(pixmask[i].(j))
                
        case state.r.combinestat of
           
           0: begin

              mc_meancomb,spec,mean,mvar,DATAVAR=err^2,MASK=mask, $
                          ROBUST=thresh,OGOODBAD=ogoodbad,/SILENT,CANCEL=cancel
              if cancel then return

           end
           
           1: begin
              
              mc_meancomb,spec,mean,mvar,/RMS,MASK=mask,ROBUST=thresh,$
                          OGOODBAD=ogoodbad,/SILENT,CANCEL=cancel
              if cancel then return

           end
           
           2: begin
              
              mc_meancomb,spec,mean,mvar,MASK=mask,ROBUST=thresh,$
                          OGOODBAD=ogoodbad,/SILENT,CANCEL=cancel
              if cancel then return

           end
           
           3: begin
              
              mc_meancomb,spec,mean,mvar,DATAVAR=err^2,MASK=mask,/SILENT, $
                          CANCEL=cancel
              if cancel then return
              
           end
           
           4: begin
              
              mc_meancomb,spec,mean,mvar,/RMS,MASK=mask,/SILENT,CANCEL=cancel
              if cancel then return
              
           end
            
           5: begin
              
              mc_meancomb,spec,mean,mvar,MASK=mask,/SILENT,CANCEL=cancel
              if cancel then return
              
           end
           
           6: begin
              
              mc_medcomb,spec,mean,mvar,/MAD,MASK=mask,CANCEL=cancel
              if cancel then return
              
           end
           
           7: begin
              
              mc_medcomb,spec,mean,mvar,MASK=mask,CANCEL=cancel
              if cancel then return
              
           end
           
           8: begin
                            
              mean = total(spec*mask,2,/NAN)
              mvar = total(mask*double(err)^2,2,/NAN)
              
           end
           
        endcase
        
        sspec[*,0,j*state.r.fnaps+i] = reform(spc[i].(j)[*,0,0])
        sspec[*,1,j*state.r.fnaps+i] = mean
        sspec[*,2,j*state.r.fnaps+i] = sqrt(mvar)

;  Combine the flags

        tmp = mask*byte(flag)
        if state.r.combinestat le 2 then tmp = tmp*ogoodbad
        result = mc_combflagstack(tmp[*,z],CANCEL=cancel)
        if cancel then return
        sspec[*,3,j*state.r.fnaps+i] = result
        
     endfor

  endfor
  
  *state.r.combspec = sspec
  
end
;
;*****************************************************************************
;
pro xcombspec_correctspec,state

  cancel = 0
  
  if (*state.w.modspeccontinue)[state.r.ap] ge 3 then begin
     
     ok = dialog_message([['Cannot perform this operation again.'],$
                          ['Please reload spectra and start over.']],/ERROR,$
                         DIALOG_PARENT=state.w.xcombspec_base)
     cancel = 1
     return
     
  endif
  
  spc  = (*state.d.wspec)[state.r.ap]
  
  for i = 0, state.r.norders-1 do begin
     
     spec = reform(spc.(i)[*,1,*])
     err  = reform(spc.(i)[*,2,*])
     pixmask = reform((*state.r.pixmask).(i))

     nstack = mc_speccor(spec,4,IERRSTACK=err,OERRSTACK=nerrstack, $
                         SPECMASK=(*state.r.spcmask)[*,i,state.r.ap], $
                         PIXMASK=(*state.r.pixmask).(i), $
                         CORRECTIONS=corrections,CANCEL=cancel)
     if cancel then return

     spc.(i)[*,1,*] = nstack
     spc.(i)[*,2,*] = nerrstack
     
  endfor
  

  (*state.d.wspec)[state.r.ap] = spc
  (*state.r.corspec)[state.r.ap] = 1
  xcombspec_combinespec,state,CANCEL=cancel
  if cancel then return
  xcombspec_plotupdate,state
  
  (*state.w.modspeccontinue)[state.r.ap] = 3
  
end
;
;*****************************************************************************
;
pro xcombspec_writefile,state

  path = mc_cfld(state.w.path_fld,7,CANCEL=cancel)
  if cancel then return

  outname = mc_cfld(state.w.outfile_fld,7,CANCEL=cancel,/EMPTY)
  if cancel then return
  
  sspec = *state.r.combspec
  
;  Make new hdr.
  
  if not state.w.basic then begin
     
     avehdr = mc_avehdrs(*state.d.hdrinfo,TIME_OBS=state.r.time,$
                         POSANGLE=state.r.posangle,HA=state.r.ha,$
                         AIRMASS=state.r.airmass,CANCEL=cancel)
     history = ''    
     
  endif else begin
     
     avehdr = (*state.d.hdrinfo)[0]
     history = 'This FITS header is from the first spectrum in the stack.  '
     
  endelse

  avehdr.vals.NAPS = state.r.fnaps
  
;  Create history
  
  values = ['Robust Weighted Mean','Robust Mean (RMS)', $
            'Robust Mean (Std Error)','Weighted Mean',$
            'Mean (RMS)','Mean (Std Error)','Median (MAD)', $
            'Median (Median Error)','Sum']
  
  
  for i = 0, state.r.fnaps-1 do begin
     
     history=history+'  The files '+strjoin((*state.r.files),', ')+$
             ' were combined.  '
     
     history = history+'  Aperture '+string(i+1,FORMAT='(i2.2)')+' Info: '
     
     if finite((*state.r.scaleorder)[i]) eq 1 then begin
        
        history = history+'The scale factors were determined using order '+$
                  string((*state.r.scaleorder)[i],FORMAT='(i2.2)')+$
                  '.  The scale factors are '+$
                  strjoin(strtrim((*state.r.scales)[*,i],2),', ')+ $
                  '.  The median RMS' + ' deviation in each order is '+ $
                  strjoin(strtrim((*state.r.medrms)[*,i],2),', ')+'.  '
        
     endif 
     
     for j = 0,state.r.norders-1 do begin
        
        z = where((*state.r.spcmask)[*,j,i] eq 0,count)
        if count ne 0 then begin
           
           history = history+'  The spectrum(a) from file(s) '+ $
                     strjoin((*state.r.files)[z],', ')+' was (ere) ' + $
                     'removed from order '+ $
                     string(total((*state.r.orders)[j]),FORMAT='(i2.2)')+'.'
           
        endif
        
     endfor
     
     if (*state.r.corspec)[i] then $
        history = history+'  The spectral shapes have been corrected.  '
     
  endfor

  history = history+'The spectra were combined using a '+ $
            values[state.r.combinestat]+'.  '
  
  if state.r.combinestat le 2 then begin
     
     thresh = mc_cfld(state.w.rthresh_fld,4,/EMPTY,CANCEL=cancel)
     if cancel then return
     history = history+'The robust threshold was '+strtrim(thresh,2)+'.'
     
  endif
  
  fxhmake,newhdr,sspec
  
  ntags    = n_tags(avehdr.vals)
  names    = tag_names(avehdr.vals)


  for i = 0, ntags - 1 do begin
     
     if names[i] eq 'HISTORY' then begin
        
        newhdr = mc_addhistlabel(newhdr,'Xspextool History',CANCEL=cancel)
        if cancel then return

        for j = 0, ceil(float(strlen(avehdr.vals.(i)))/70.)-1 do begin
           
           hist = strmid(avehdr.vals.(i),70*j,70)
           fxaddpar,newhdr,'HISTORY',hist
           
        endfor
        
     endif else fxaddpar,newhdr,names[i],avehdr.vals.(i),avehdr.coms.(i)
     
  endfor

  newhdr = mc_addhistlabel(newhdr,'Xcombspec History',CANCEL=cancel)
  if cancel then return
  
  history = mc_splittext(history,70,CANCEL=cancel)
  if cancel then return
  sxaddhist,history,newhdr
  
  fxaddpar,newhdr,'ITOT',state.r.itot,' Total integration time (sec)'
  fxaddpar,newhdr,state.r.irafname,outname+'.fits'
  
  
  writefits,path+outname+'.fits',sspec,newhdr
  
  xvspec,path+outname+'.fits',/PLOTLINMAX
  
end
;
;******************************************************************************
;
pro xcombspec_loadspec,state

;  Construct full file names

  if state.r.inputmode eq 'Manual' then begin
    
;  Get path and file names
    
     path = mc_cfld(state.w.path_fld,7,CANCEL=cancel)
     if cancel then return
     
     files = mc_cfld(state.w.spemc_cfiles_fld,7,/EMPTY,CANCEL=cancel)
     if cancel then return
    
;  Construct array of full file names
     
     index    = (state.r.filereadmode eq 'Index') ? 1:0
     filename = (state.r.filereadmode eq 'Filename') ? 1:0
     if index then prefix = mc_cfld(state.w.inprefix_fld,7,/EMPTY,CANCEL=cancel)
     if cancel then return
     
     files = mc_fsextract(files,INDEX=index,FILENAME=filename,CANCEL=cancel)
     if cancel then return
     
     fullpaths = mc_mkfullpath(path,files,INDEX=index,FILENAME=filename,$
                               NI=state.r.nint,PREFIX=prefix,SUFFIX='.fits',$
                               WIDGET_ID=state.w.xcombspec_base,/EXIST,$
                               CANCEL=cancel)
     if cancel then return
     
  endif else begin
     
     filename = mc_cfld(state.w.filename_fld,7,CANCEL=cancel)
     if cancel then return    
     nl = file_lines(filename)
     fullpaths = strarr(nl)
     openr,lun, filename,/GET_LUN
     line = ' '
     for i = 0, nl-1 do begin
        
        readf, lun, line
        fullpaths[i] = strtrim(line,2)
        
     endfor
     free_lun, lun
     path = ''
     
  endelse
  
;  Read the first spectrum to get initial data

  mc_readspec,fullpaths[0],first,hdr,obsmode,start,stop,norders,naps,orders,$
              xunits,yunits,slith_pix,slith_arc,slitw_pix,slitw_arc,$
              rp,airmass,xtitle,ytitle,instr,/SILENT,CANCEL=cancel
  
  state.r.npix     = n_elements(first[*,0,0])
  state.r.norders  = norders
  state.r.naps     = naps
  state.p.xtitle   = xtitle
  lidx             = strpos(ytitle,'(')
  ridx             = strpos(ytitle,')')
  yunits           = strmid(ytitle,lidx+1,ridx-lidx-1)
  state.p.ytitle   = [ytitle,'!5Error ('+yunits+')','!5S/N']
  *state.r.orders  = orders
  state.r.nfiles   = n_elements(fullpaths)

  *state.w.modspeccontinue = intarr(naps)
  
;  Update box 2

  widget_control, state.w.scaleorder_dl,$
                  SET_VALUE=[string(*state.r.orders,FORMAT='(i2.2)')]

  widget_control, state.w.pruneorder_dl,$
                  SET_VALUE=[string(*state.r.orders,FORMAT='(i2.2)')]

  widget_control, state.w.scale_base, SENSITIVE=1
  widget_control, state.w.prune_base, SENSITIVE=1
  
  state.r.modspec = 0
  
;  Modify plot window according to the number of orders
  
  state.p.plotwinsize[1] = state.p.plotwinsize[1] > state.p.pixpp* $
                           state.r.norders
  xcombspec_modwinsize,state
  
;  Set up arrays checking to see if combining apertures

  state.r.ap = 0

  if state.r.combineaps eq 'No' then begin
     
     state.r.fnaps   = state.r.naps
     *state.r.medrms = fltarr(state.r.norders,state.r.fnaps)+!values.f_nan
     *state.r.scales = fltarr(state.r.nfiles,state.r.fnaps)+1
     state.r.nspec   = state.r.nfiles
     
     value   = strcompress(indgen(state.r.naps)+1, /RE)
     array   = fltarr(state.r.npix,4,state.r.nfiles)*!values.f_nan
     spcmask = intarr(state.r.nfiles,state.r.norders,state.r.fnaps)+1
     pmask   = intarr(state.r.npix,state.r.nfiles)+1
     
     widget_control, state.w.aperture_dl, SET_VALUE=value
     widget_control, state.w.aperture_dl, /SENSITIVE
     
     
  endif else begin
     
     state.r.fnaps   = 1
     *state.r.medrms = fltarr(state.r.norders)+!values.f_nan
     *state.r.scales = fltarr(state.r.nfiles*state.r.naps)
     state.r.nspec   = state.r.nfiles*state.r.naps
     
     array   = fltarr(state.r.npix,4,state.r.nfiles*state.r.naps)+!values.f_nan
     spcmask = intarr(state.r.naps*state.r.nfiles,state.r.norders)+1
     pmask = intarr(state.r.npix,state.r.nfiles*state.r.naps)+1
     
     widget_control, state.w.aperture_dl, SENSITIVE=0
     
  endelse

  *state.r.scaleorder   = fltarr(state.r.naps)+!values.f_nan
  state.r.scaleorderidx = 0
  state.r.pruneorderidx = 0
  *state.r.corspec      = intarr(state.r.fnaps)
  
;  Create data structure for a single aperture
  
  key     = 'Order'+string(00,FORMAT='(i2.2)')
  spc     = create_struct(key,array)
  pixmask = create_struct(key,pmask)
  
  for i = 1, state.r.norders-1 do begin
     
     key     = 'Order'+string(i,FORMAT='(i2.2)')
     spc     = create_struct(spc,key,array)
     pixmask = create_struct(pixmask,key,pmask)
     
  endfor
  
  hdrinfo = replicate(mc_gethdrinfo(hdr),state.r.nfiles)
  
;  If there are more than 1 apertures replicate the data structure.
  
  spc     = replicate(spc,state.r.fnaps)
  pixmask = replicate(pixmask,state.r.fnaps)
  
  itot = 0

;  Load the data
  
  for i = 0, state.r.nfiles-1 do begin
     
     data = readfits(fullpaths[i],hdr,/SILENT)
     data = mc_caaspecflags(data,CANCEL=cancel)
     if cancel then return
     
     tmp = fxpar(hdr,'ITOT')
     if state.r.combineaps ne 'Yes' then begin

        itot = itot+tmp

     endif else begin

        itot = itot+tmp*2

     endelse

     for j = 0, state.r.norders-1 do begin

        if state.r.combineaps ne 'Yes' then begin
           
           for k = 0, state.r.naps-1 do begin
              
              z = where(finite(first[*,0,j*state.r.naps+k]) eq 1)

              mc_interpspec,data[z,0,j*state.r.naps+k],$
                            data[z,1,j*state.r.naps+k], $
                            first[z,0,j*state.r.naps+k],newflux,newerror, $
                            IYERROR=data[z,2,j*state.r.naps+k],LEAVENANS=1,$
                            CANCEL=cancel
              if cancel then return

              mc_interpflagspec,data[z,0,j*state.r.naps+k],$
                                byte(data[z,3,j*state.r.naps+k]),$
                                first[z,0,j*state.r.naps+k], $
                                newflag,CANCEL=cancel
              if cancel then return
              
              spc[k].(j)[z,0,i] = first[z,0,j*state.r.naps+k]
              spc[k].(j)[z,1,i] = newflux
              spc[k].(j)[z,2,i] = newerror
              spc[k].(j)[z,3,i] = float(newflag)
              
           endfor
           
        endif else begin

           for k = 0,state.r.naps-1 do begin
              
              z = where(finite(first[*,0,j*state.r.naps+k]) eq 1)

              mc_interpspec,data[z,0,j*state.r.naps+k],$
                            data[z,1,j*state.r.naps+k], $
                            first[z,0,j*state.r.naps+k],newflux,newerror, $
                            IYERROR=data[z,2,j*state.r.naps+k],LEAVENANS=1,$
                            CANCEL=cancel
              if cancel then return

              mc_interpflagspec,data[z,0,j*state.r.naps+k],$
                                byte(data[z,3,j*state.r.naps+k]),$
                                first[z,0,j*state.r.naps+k], $
                                newflag,CANCEL=cancel
              if cancel then return
             
                spc[0].(j)[z,0,i*state.r.naps+k] = first[z,0,j*state.r.naps+k]
                spc[0].(j)[z,1,i*state.r.naps+k] = newflux
                spc[0].(j)[z,2,i*state.r.naps+k] = newerror
                spc[0].(j)[z,3,i*state.r.naps+k] = float(newflag)

             endfor
           
        endelse
        
     endfor
     
  endfor

  *state.d.ospec    = spc
  *state.d.wspec    = spc
  *state.r.spcmask  = spcmask
  *state.r.pixmask  = pixmask
  *state.d.hdrinfo  = hdrinfo

  state.r.itot = itot
  
;  Create filename string for output FITS header later
  
  for i = 0,state.r.nfiles-1 do begin
     
     file = strmid(fullpaths[i],strpos(fullpaths[i],'/',/REVERSE_S)+1)
     sfile = (i eq 0) ? file:[sfile,file]
     
  endfor
  *state.r.files = sfile
  
;  Unfreeze the widget
  
  state.w.freeze = 0
  xcombspec_combinespec,state,CANCEL=cancel
  if cancel then return
  xcombspec_plotupdate,state

end
;
;******************************************************************************
;
pro xcombspec_prunespec,state

  cancel = 0

  index = state.r.pruneorderidx

  spc    = (*state.d.ospec)[state.r.ap]
  zorder = total(where(*state.r.orders eq (*state.r.orders)[index]))
  
  x = reform(spc.(zorder)[*,0,0])
  y = reform(spc.(zorder)[*,1,*])

  s = size(y,/DIMEN)

  scales = rebin(reform((*state.r.scales)[*,state.r.ap],1,s[1]),s[0],s[1])

  xmc_maskspec,reform(spc.(zorder)[*,0,0]), $
               reform(spc.(zorder)[*,1,*])*scales,$
               specmask,pixmask,XTITLE=state.p.xtitle, $
               YTITLE=state.p.ytitle[0], $
               GROUP_LEADER=state.w.xcombspec_base,CANCEL=cancel
 if cancel then return

  (*state.r.spcmask)[*,index,state.r.ap] = specmask
  (*state.r.pixmask)[state.r.ap].(zorder) = pixmask

  
  widget_control, /HOURGLASS
  
  xcombspec_combinespec,state,CANCEL=cancel
  if cancel then return
  xcombspec_plotupdate,state

end
;
;******************************************************************************
;
pro xcombspec_modwinsize,state


  widget_control, state.w.col2_base, UPDATE=0
  widget_control, state.w.plotwin, /DESTROY
  
  if state.p.plotwinsize[1] le state.p.scrollsize[1] then begin

     state.w.plotwin = widget_draw(state.w.col2_base,$
                                   XSIZE=state.p.plotwinsize[0],$
                                   YSIZE=state.p.plotwinsize[1],$
                                   UVALUE='Plot Window',$
                                   /BUTTON_EVENTS,$
                                   EVENT_PRO='xcombspec_plotwin_event')

  endif else begin

     state.w.plotwin = widget_draw(state.w.col2_base,$
                                   XSIZE=state.p.plotwinsize[0],$
                                   YSIZE=state.p.plotwinsize[1],$
                                   X_SCROLL_SIZE=state.p.scrollsize[0],$
                                   Y_SCROLL_SIZE=state.p.scrollsize[1],$
                                   /SCROLL,$
                                   UVALUE='Plot Window',$
                                   /BUTTON_EVENTS,$
                                   EVENT_PRO='xcombspec_plotwin_event')


  endelse

  widget_control, state.w.plotwin, GET_VALUE = wid
  state.p.plotwin_wid = wid
  wset, wid
  erase, COLOR=20

  widget_control, state.w.col2_base, UPDATE=1
  
  widget_geom = widget_info(state.w.xcombspec_base, /GEOMETRY)
  
  state.p.buffer[0]=widget_geom.xsize-state.p.scrollsize[0]
  state.p.buffer[1]=widget_geom.ysize-state.p.scrollsize[1]
  

  
  wdelete, state.p.pixmap_wid
  window, /FREE, /PIXMAP,XSIZE=state.p.plotwinsize[0],$
          YSIZE=state.p.plotwinsize[1]
  state.p.pixmap_wid = !d.window
  

end
;
;******************************************************************************
;
pro xcombspec_plotspec,state

  spc  = *state.d.wspec
  mask = *state.r.spcmask
  pixmask = *state.r.pixmask
  
  !p.multi[0] = state.r.norders
  !p.multi[2] = state.r.norders
  
  case state.p.spectype of
     
     0: ytitle = state.p.ytitle[0]
     
     1: ytitle = state.p.ytitle[1]
     
     2: ytitle = state.p.ytitle[2]
     
  endcase

  charsize = state.p.charsize
  if state.r.norders ge 3 then charsize = charsize*2.0
  
  if state.p.plottype eq 0 then begin
     
     for i = 0, state.r.norders-1 do begin
        
        j     = state.r.norders-1-i
        title = 'Order '+string((*state.r.orders)[j],FORMAT='(i2.2)')
        
;  Get plot range.
        
        case state.p.spectype of 
           
           0: spec = reform(spc[state.r.ap].(j)[*,1,*])
           
           1: spec = reform(spc[state.r.ap].(j)[*,2,*])
           
           2: spec = reform(spc[state.r.ap].(j)[*,1,*] / $
                            spc[state.r.ap].(j)[*,2,*])
           
        end
        
        mc_medcomb,spec,med
        yrange = [0.4*min(med,/NAN,MAX=max),1.5*max]
        
;  Plot spectra    
        
        plot,spc[state.r.ap].(j)[*,0,0],spec[*,0],$
             /XSTY,/YSTY,YRANGE=yrange,/NODATA,TITLE=title,$
             XTITLE=state.p.xtitle,YTITLE=ytitle, $
             CHARSIZE=charsize
        
        for k = 0, state.r.nspec-1 do begin
            
            if mask[k,j,state.r.ap] eq 1 then begin

                flux = spec[*,k]
                z = where(pixmask[state.r.ap].(j)[*,k] eq 0,cnt)
                if cnt ne 0 then flux[z] = !values.f_nan

              oplot,spc[state.r.ap].(j)[*,0,k],flux,$
                    COLOR=state.r.colors[k],LINESTYLE=state.r.lines[k],$
                    PSYM=10

          endif
            
        endfor
        
    endfor
     
  endif

  if state.p.plottype eq 1 then begin
     
     wave = reform((*state.r.combspec)[*,0,*])
     
     case state.p.spectype of 
        
        0: spec = reform((*state.r.combspec)[*,1,*])
        
        1: spec = reform((*state.r.combspec)[*,2,*])
        
        2: spec = reform((*state.r.combspec)[*,1,*] / $
                         (*state.r.combspec)[*,2,*])
        
     end
     
     for i = 0, state.r.norders-1 do begin
        
        j     = state.r.norders-1-i
        title = 'Order '+string((*state.r.orders)[j],FORMAT='(i2.2)')
        
;  Get plot range.
        
        yrange = [0.4*min(spec[*,j*state.r.fnaps+state.r.ap],/NAN,MAX=max), $
                  1.5*max]
        
;  Plot spectra    
        
        plot,wave[*,j*state.r.fnaps+state.r.ap], $
             spec[*,j*state.r.fnaps+state.r.ap],$
             /XSTY,/YSTY,YRANGE=yrange,CHARSIZE=charsize,$
             TITLE=title, XTITLE=state.p.xtitle,YTITLE=ytitle,PSYM=10
        
     endfor
     
  endif
  
  !p.multi=0
  delvarx,spc

end
;
;******************************************************************************
;
pro xcombspec_plotupdate,state

  wset,state.p.pixmap_wid
  erase
  polyfill,[0,0,1,1,0],[0,1,1,0,0],COLOR=20,/NORM
  xcombspec_plotspec,state
  
  wset, state.p.plotwin_wid
  device, COPY=[0,0,state.p.plotwinsize[0],state.p.plotwinsize[1],0,0,$
                state.p.pixmap_wid]

end
;
;******************************************************************************
;
pro xcombspec_scalespec,state

  cancel = 0

  if (*state.w.modspeccontinue)[state.r.ap] gt 0 then begin
     
     ok = dialog_message([['Cannot perform this operation again.'],$
                          ['Please reload spectra and start over.']],/ERROR,$
                         DIALOG_PARENT=state.w.xcombspec_base)
     cancel = 1
     return
     
  endif

  index = state.r.scaleorderidx

  spc = (*state.d.ospec)[state.r.ap]
  good = where((*state.r.spcmask)[*,index,state.r.ap] eq 1)
  zorder = total(where(*state.r.orders eq (*state.r.orders)[index]))
  (*state.r.scaleorder)[state.r.ap] = (*state.r.orders)[index]


  if state.r.combineaps eq 'Yes' then begin

     files = *state.r.files+' Ap '+ $
             string(findgen(state.r.naps)+1,FORMAT='(I2.2)')


  endif else files = *state.r.files


  xmc_scalespec,reform(spc.(zorder)[*,0,0]),reform(spc.(zorder)[*,1,*]), $
                files,reform((*state.r.spcmask)[*,index,state.r.ap]), $
                scales,wrange,GROUP_LEADER=state.w.xcombspec_base, $
                XTITLE=state.p.xtitle,YTITLE=state.p.ytitle[0],CANCEL=cancel
  if cancel then return
  
  widget_control, /HOURGLASS
  for i = 0, state.r.norders-1 do begin
     
     s = reform(spc.(i)[*,1,*])
     e = reform(spc.(i)[*,2,*])
     
     for k = 0, n_elements((*state.r.spcmask)[*,i,state.r.ap])-1 do begin
        
        s[*,k] = scales[k] * s[*,k]
        e[*,k] = scales[k] * e[*,k]
        
     endfor
     
     spc.(i)[*,1,*] = s
     spc.(i)[*,2,*] = e        
     
;  Get median RMS across each order
     
     mc_meancomb,reform(s[*,good]),mean,var,/RMS
     (*state.r.medrms)[i,state.r.ap] = median(sqrt(var),/EVEN)
     
;  Store scales
     
     (*state.r.scales)[*,state.r.ap] = scales
     
  endfor

  (*state.d.wspec)[state.r.ap] = spc
  
  
  xcombspec_combinespec,state,CANCEL=cancel
  if cancel then return
  xcombspec_plotupdate,state
  
  (*state.w.modspeccontinue)[state.r.ap]=1
  
end
;
;******************************************************************************
;
pro xcombspec,instrument,BASIC=basic

  mc_mkct

  device, RETAIN=2

;  Get spextool and instrument information 
  
  mc_getspextoolinfo,spextoolpath,packagepath,instr,notspex,version, $
                     INSTRUMENT=instrument,CANCEL=cancel
  if cancel then return
  
;  Set the fonts

  mc_getfonts,buttonfont,textfont

;  Build three structures that will hold important info.

  w = {aperture_dl:0L,$
       basic:keyword_set(BASIC),$
       bdpxthresh1_fld:[0L,0L],$
       bdpxthresh2_fld:[0L,0L],$
       box2_base:0L,$
       box3_base:0L,$
       box4_base:0L,$
       col2_base:0L,$
       correct_base:0L,$
       file_base:0L,$
       filename_fld:[0L,0L],$
       freeze:1,$
       inprefix_fld:[0L,0L],$
       keyboard:0L,$
       manual_base:0L,$
       mask_dl:0L,$
       medscale:0L,$
       modspeccontinue:ptr_new(2),$
       modtype_dl:0L,$
       nan_bg:0L,$
       outfile_fld:[0L,0L],$
       path_fld:[0L,0L],$
       plot_base:0L,$
       plotwin:0L,$
       prune_base:0L,$
       pruneorder_dl:0L,$
       rthresh_fld:[0L,0L],$
       scale_base:0L,$
       scaleorder_dl:0L,$
       scalerange_fld:[0L,0L],$
       spemc_cfiles_fld:[0L,0L],$
       xcombspec_base:0L}
  
  r = {absscalerange:[!values.f_nan,!values.f_nan],$
       airmass:instr.airmass,$
       ap:0,$
       colors:[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,$
               1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,$
               1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,$
               1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,$
               1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,$
               1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,$
               1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,$
               1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,$
               1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,$
               1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,$
               1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,$
               1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,$
               1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,$
               1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,$
               1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,$
               1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,$
               1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,$
               1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],$
       combineaps:'No',$
       combinestat:0,$
       combspec:ptr_new(2),$
       corspec:ptr_new(2),$
       exptime:instr.exptime,$
       files:ptr_new(fltarr(2)),$
       filereadmode:'Index',$
       fnaps:0,$
       ha:instr.ha,$
       ifileformat:'FITS',$
       ignorenans:0,$
       inputmode:'Manual',$
       instr:instr.instr,$
       irafname:instr.irafname,$
       itot:0.0,$
       lines:[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,$
              1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,$
              2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,$
              3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,$
              4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,$
              5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,$
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,$
              1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,$
              2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,$
              3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,$
              4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,$
              5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,$
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,$
              1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,$
              2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,$
              3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,$
              4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,$
              5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5],$
       median:0,$
       medrms:ptr_new(2),$
       modspec:0,$
       naps:0,$
       nfiles:0,$
       nint:instr.nint,$
       norders:0,$
       npix:0,$
       nspec:0,$
       orders:ptr_new(2),$     
       packagepath:packagepath,$
       pixmask:ptr_new(2),$
       posangle:instr.posangle,$
       pruneorderidx:0L,$
       scales:ptr_new(scales),$
       scaleorder:ptr_new(0),$
       scaleorderidx:0L,$
       scalerange:[!values.f_nan,!values.f_nan],$
       spcmask:ptr_new(2),$
       specidx:0,$
       spextoolpath:spextoolpath,$
       stackmask:ptr_new(2),$
       time:instr.time}
  
  
  p = {buffer:[0,0],$
       charsize:1.5,$
       pixpp:250.0,$
       pixmap_wid:0L,$
       plotwin_wid:0,$
       plotwinsize:[600,700],$
       plottype:0,$
       spectype:0,$
       scrollsize:[600,670],$
       xtitle:'',$
       ytitle:['','','']}
  
  d = {ospec:ptr_new(fltarr(2)),$
       wspec:ptr_new(fltarr(2)),$
       hdrinfo:ptr_new(strarr(2))}
  
  state = {w:w,r:r,d:d,p:p}
  
;  Build the widget.

  title = 'Xcombspec '+version+' for '+r.instr

  state.w.xcombspec_base = widget_base(TITLE=title,$
                                       /COLUMN,$
                                       /TLB_SIZE_EVENTS)

     button = widget_button(state.w.xcombspec_base,$
                            FONT=buttonfont,$
                            VALUE='Quit',$
                            EVENT_PRO='xcombspec_event',$
                            UVALUE='Quit')

     state.w.keyboard = widget_text(state.w.xcombspec_base, $
                                    /ALL_EVENTS, $
                                    SCR_XSIZE=1, $
                                    SCR_YSIZE=1, $
                                    UVALUE='Keyboard', $
                                    VALUE='')
     
     row_base = widget_base(state.w.xcombspec_base,$
                            /ROW)

        col1_base = widget_base(row_base,$
                                EVENT_PRO='xcombspec_event',$
                                /COLUMN)

           box1_base = widget_base(col1_base,$
                                   /COLUMN,$
                                   FRAME=2)

              label = widget_label(box1_base,$
                                   VALUE='1.  Load Spectra:',$
                                   /ALIGN_LEFT,$
                                   FONT=buttonfont)
           
               
              bg = cw_bgroup(box1_base,$
                             ['Manual','Text File'],$
                             /ROW,$
                             LABEL_LEFT='Method:',$
                             /RETURN_NAME,$
                             /NO_RELEASE,$
                             UVALUE='Input Mode',$
                             FONT=buttonfont,$
                             /EXCLUSIVE,$
                             SET_VALUE=0)
                 
              blank = widget_base(box1_base,$
                                /BASE_ALIGN_CENTER)

                 state.w.manual_base = widget_base(blank,$
                                                   /COLUMN,$
                                                   /BASE_ALIGN_LEFT)
                 
                    row = widget_base(state.w.manual_base,$
                                      /ROW,$
                                      /BASE_ALIGN_CENTER)
                   
                       button = widget_button(row,$
                                              FONT=buttonfont,$
                                              VALUE='Path',$
                                              UVALUE='Path Button')
                      
                       fld = coyote_field2(row,$
                                           LABELFONT=buttonfont,$
                                           FIELDFONT=textfont,$
                                           TITLE=':',$
                                           UVALUE='Path Field',$
                                           XSIZE=25,$
                                           TEXTID=textid)
                       state.w.path_fld = [fld,textid]               
               
                    bg = cw_bgroup(state.w.manual_base,$
                                   ['Filename','Index'],$
                                   /ROW,$
                                   LABEL_LEFT='File Read Mode:',$
                                   /RETURN_NAME,$
                                   /NO_RELEASE,$
                                   UVALUE='Readmode',$
                                   FONT=buttonfont,$
                                   /EXCLUSIVE,$
                                   SET_VALUE=1)
                    
                    fld = coyote_field2(state.w.manual_base,$
                                        LABELFONT=buttonfont,$
                                        FIELDFONT=textfont,$
                                        TITLE='Input Prefix:',$
                                        UVALUE='Input Prefix',$
                                        XSIZE=25,$
                                        VALUE='spectra',$
                                        TEXTID=textid)
                  state.w.inprefix_fld = [fld,textid]
                  
                  row = widget_base(state.w.manual_base,$
                                    /ROW,$
                                    /BASE_ALIGN_CENTER)
                  
                     button = widget_button(row,$
                                            FONT=buttonfont,$
                                            VALUE='Files',$
                                            UVALUE='Spectra Files Button')
                     
                     field = coyote_field2(row,$
                                           LABELFONT=buttonfont,$
                                           FIELDFONT=textfont,$
                                           TITLE=':',$
                                           UVALUE='Spectra Files Field',$
                                           VALUE='205-212',$
;                                           VALUE='207-216',$
                                           XSIZE=25,$
                                           /CR_ONLY,$
                                           TEXTID=textid)
                     state.w.spemc_cfiles_fld = [field,textid]
                     
               state.w.file_base = widget_base(blank,$
                                               /COLUMN,$
                                               /BASE_ALIGN_CENTER)

                  row = widget_base(state.w.file_base,$
                                    /ROW,$
                                    /BASE_ALIGN_CENTER)
                  
                     button = widget_button(row,$
                                            FONT=buttonfont,$
                                            VALUE='File Name',$
                                            UVALUE='File Name')
                     
                     field = coyote_field2(row,$
                                           LABELFONT=buttonfont,$
                                           FIELDFONT=textfont,$
                                           TITLE=':',$
                                           UVALUE='File Name Field',$
                                           XSIZE=25,$
                                           /CR_ONLY,$
                                           TEXTID=textid)
                     state.w.filename_fld = [field,textid]
                     
            combineap_bg = cw_bgroup(box1_base,$
                                     FONT=buttonfont,$
                                     ['Yes','No'],$
                                     /ROW,$
                                     /RETURN_NAME,$
                                     /NO_RELEASE,$
                                     /EXCLUSIVE,$
                                     LABEL_LEFT='Combine Two Apertures:',$
                                     UVALUE='Combine Apertures',$
                                     SET_VALUE=1)

            load = widget_button(box1_base,$
                                 FONT=buttonfont,$
                                 VALUE='Load Spectra',$
                                 UVALUE='Load Spectra')

         state.w.box2_base = widget_base(col1_base,$
                                         /COLUMN,$
                                         FRAME=2)

            label = widget_label(state.w.box2_base,$
                                 VALUE='2.  Modify Spectra',$
                                 /ALIGN_LEFT,$
                                 FONT=buttonfont)

            state.w.scale_base = widget_base(state.w.box2_base,$
                                             /ROW,$
                                             /BASE_ALIGN_CENTER)

               state.w.scaleorder_dl = widget_droplist(state.w.scale_base,$
                                                       FONT=buttonfont,$
                                                       TITLE='Order:',$
                                                       VALUE='01',$
                                                       UVALUE='Scale Order')

               button = widget_button(state.w.scale_base,$
                                      FONT=buttonfont,$
                                      VALUE='Scale Spectra',$
                                      UVALUE='Scale Spectra')


            state.w.prune_base = widget_base(state.w.box2_base,$
                                             /ROW,$
                                             /BASE_ALIGN_CENTER)

               state.w.pruneorder_dl = widget_droplist(state.w.prune_base,$
                                                       FONT=buttonfont,$
                                                       TITLE='Order:',$
                                                       VALUE='01',$
                                                       UVALUE='Prune Order')

               button = widget_button(state.w.prune_base,$
                                      FONT=buttonfont,$
                                      VALUE='Prune Spectra',$
                                      UVALUE='Prune Spectra')

            state.w.correct_base = widget_base(state.w.box2_base,$
                                               /ROW,$
                                               /BASE_ALIGN_CENTER)

               button = widget_button(state.w.box2_base,$
                                      FONT=buttonfont,$
                                      VALUE='Correct Spectral Shape',$
                                      UVALUE='Correct Spectral Shape')
                                           
         state.w.box3_base = widget_base(col1_base,$
                                         /COLUMN,$
                                         FRAME=2)
         
            label = widget_label(state.w.box3_base,$
                                 VALUE='3.  Combine Spectra',$
                                 /ALIGN_LEFT,$
                                 FONT=buttonfont)

;            state.w.nan_bg = cw_bgroup(state.w.box3_base,$
;                                       ['Ignore NaNs'],$
;                                       FONT=buttonfont,$
;                                       UVALUE='Ignore NaNs',$
;                                       /NONEXCLUSIVE) 
            
            values = ['Robust Weighted Mean','Robust Mean (RMS)', $
                      'Robust Mean (Std Error)','Weighted Mean',$
                      'Mean (RMS)','Mean (Std Error)','Median (MAD)', $
                      'Median (Std Error)','Sum']
            combine_dl = widget_droplist(state.w.box3_base,$
                                         FONT=buttonfont,$
                                         TITLE='Statistic:',$
                                         VALUE=values,$
                                         UVALUE='Combination Statistic')
            widget_control, combine_dl, SET_DROPLIST_SELECT=state.r.combinestat

            fld = coyote_field2(state.w.box3_base,$
                                LABELFONT=buttonfont,$
                                FIELDFONT=textfont,$
                                TITLE='Robust Threshold:',$
                                UVALUE='Robust Threshold',$
                                XSIZE=7,$
                                VALUE='8.0',$
                                EVENT_PRO='xcombspec_event',$
                                /CR_ONLY,$
                                TEXTID=textid)
            state.w.rthresh_fld = [fld,textid]

         state.w.box4_base = widget_base(col1_base,$
                                         /COLUMN,$
;                                         /BASE_ALIGN_LEFT,$
                                         FRAME=2)

            label = widget_label(state.w.box4_base,$
                                 VALUE='4.  Write Spectra',$
                                 /ALIGN_LEFT,$
                                 FONT=buttonfont)
            
            row = widget_base(state.w.box4_base,$
                              /ROW,$
                              /BASE_ALIGN_CENTER)


               field = coyote_field2(row,$
                                     LABELFONT=buttonfont,$
                                     FIELDFONT=textfont,$
                                     TITLE='Output File:',$
                                     UVALUE='Output File',$
                                     XSIZE=15,$
                                     /CR_ONLY,$
                                     TEXTID=textid)
               state.w.outfile_fld = [field,textid]

            combine_button = widget_button(state.w.box4_base,$
                                           FONT=buttonfont,$
                                           VALUE='Write File',$
                                           UVALUE='Write File')

         state.w.col2_base = widget_base(row_base,$
                                         EVENT_PRO='xcombspec_event',$
                                         /COLUMN)

            state.w.plot_base = widget_base(state.w.col2_base,$
                                            /ROW,$
                                            /BASE_ALIGN_CENTER,$
                                            FRAME=2)
            
               state.w.aperture_dl = widget_droplist(state.w.plot_base,$
                                                     FONT=buttonfont,$
                                                     TITLE='Aperture:',$
                                                     VALUE='1',$
                                                     UVALUE='Aperture')   

               plot_dl = widget_droplist(state.w.plot_base,$
                                         FONT=buttonfont,$
                                         TITLE='Plot:',$
                                         VALUE=['Raw','Combined'],$
                                         UVALUE='Plot Type')   

               plot_dl = widget_droplist(state.w.plot_base,$
                                         FONT=buttonfont,$
                                         TITLE='Type:',$
                                         VALUE=['Flux','Error','SNR'],$
                                         UVALUE='Spectra Type')   

               state.w.plotwin = widget_draw(state.w.col2_base,$
                                             XSIZE=state.p.plotwinsize[0],$
                                             YSIZE=state.p.plotwinsize[1],$
                                             UVALUE='Plot Window',$
                                             /BUTTON_EVENTS,$
                                             EVENT_PRO= $
                                             'xcombspec_plotwin_event')

     button = widget_button(state.w.xcombspec_base,$
                            FONT=buttonfont,$
                            VALUE='Help',$
                            EVENT_PRO='xcombspec_event',$
                            UVALUE='Help')
     
; Get things running.  Center the widget using the Fanning routine.

  cgcentertlb,state.w.xcombspec_base

  widget_control, state.w.xcombspec_base, /REALIZE
  
;  Get plotwin ids
  
  widget_control, state.w.plotwin, GET_VALUE=wid
  state.p.plotwin_wid = wid
  wset, wid
  erase, COLOR=20
     
  window, /FREE, /PIXMAP,XSIZE=state.p.plotwinsize[0],$
          YSIZE=state.p.plotwinsize[1]
  state.p.pixmap_wid = !d.window
   
; Start the Event Loop. This will be a non-blocking program.
   
  XManager, 'xcombspec', $
            state.w.xcombspec_base, $
            /NO_BLOCK,$
            EVENT_HANDLER='xcombspec_resize',$
            CLEANUP='xcombspec_cleanup'
  
;  Get sizes of things now for resizing
  
  widget_control, state.w.xcombspec_base, TLB_GET_SIZE=result
  
  widget_geom = widget_info(state.w.xcombspec_base, /GEOMETRY)

  state.p.buffer[0] = widget_geom.xsize-state.p.scrollsize[0]
  state.p.buffer[1] = widget_geom.ysize-state.p.scrollsize[1]
  
  
; Put state variable into the user value of the top level base.
  
  widget_control, state.w.xcombspec_base, SET_UVALUE=state, /NO_COPY
  
end






