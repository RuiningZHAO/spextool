;
;=============================================================================
;
; ----------------------------Support procedures------------------------------ 
;
;=============================================================================
;
pro spexcals1d_doit,w,CANCEL=cancel

  cancel = 0

  common xspextool_state

  widget_control, /HOURGLASS

  widget_control, w.table, GET_VALUE=array
     
  prefix = mc_cfld(state.w.iprefix_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then goto, out
  
;  Inspect all of the cal sets to make sure they are really cal sets

  flatinfo = strarr(2,w.nrows)
  arcinfo  = strarr(5,w.nrows)
  
  for i = 0, w.nrows-1 do begin
     
     files = strtrim(array[0,i],2)
     if files eq '' then continue
     
     xspextool_message,'Inspecting Cal Set '+strcompress(i+1,/RE)+'...'
     
     files = mc_fsextract(files,/INDEX,CANCEL=cancel)
     if cancel then goto, out

     result = mc_checkdups(state.r.datapath,files,state.r.nint, $
                           WIDGET_ID=state.w.xspextool_base,$
                           CANCEL=cancel)
     if cancel then goto, out
     
     fullpaths = mc_mkfullpath(state.r.datapath,files,/INDEX,NI=state.r.nint, $
                               PREFIX='*',SUFFIX=state.r.suffix, $
                               WIDGET_ID=state.w.xspextool_base,$
                               /EXIST,CANCEL=cancel)
     if cancel then begin
        
        xspextool_message,'Cal Set '+strcompress(i+1,/RE)+' does not exist.'
        goto, out
        
     endif
     
     flats = mc_findspexflats(fullpaths,MASK=flatmask,CANCEL=cancel)
     if cancel then goto, out
     
     z = where(flatmask eq 1,count)

     if count eq 0 then begin

        xspextool_message,'No flats in Cal Set '+strcompress(i+1,/RE)+'.'
        goto, out

     endif
     
     flatoname = 'flat'+strcompress(files[z[0]],/RE)+'-'+$
                 strcompress(files[z[count-1]],/RE)
     
     
     flatinfo[0,i] = strjoin(flats,',')
     flatinfo[1,i] = flatoname
     
     arcs = mc_findspexarcs(fullpaths,MASK=arcmask,AB=ab,CANCEL=cancel)
     if cancel then goto, out
     
     z = where(arcmask eq 1,count)
     if count eq 0 then begin
        
        xspextool_message,'No arcs in Cal Set '+strcompress(i+1,/RE)+'.'
        goto, out
        
     endif
     
     arcinfo[0,i] = strjoin(arcs,',')
     arcinfo[1,i] = flatoname+'.fits'
     arcinfo[2,i] = (AB eq 0) ? string(0):string(1)
     
     hdr     = headfits(arcs[0])
     obsmode = strcompress(fxpar(hdr,'GRAT'),/RE)

     if obsmode eq 'LongXD1.9' or $
        obsmode eq 'LongXD2.1' or $
        obsmode eq 'LongXD2.3' then begin
        
        skys = strtrim(array[1,i],2)
        prefix = strtrim(array[2,i],2)
        if skys eq '' then begin
           
           xspextool_message, $
              'Please load a sky frame(s) for Cal Set '+strtrim(i+1,2)+'.'
           goto, out
           
        endif
        
        skys = mc_fsextract(skys,/INDEX,CANCEL=cancel)
        IF cancel THEN goto, out
        skys = mc_mkfullpath(state.r.datapath,skys,/INDEX,NI=state.r.nint, $
                             PREFIX=prefix,SUFFIX=state.r.suffix, $
                             WIDGET_ID=state.w.xspextool_base,$
                             /EXIST,CANCEL=cancel)
        if cancel then goto, out
        
        if n_elements(skys) gt 1 then skys = strjoin(skys,',')
        arcinfo[3,i] = skys
        
     endif
     
     z = where(arcmask eq 1,count)
     
     waveoname = 'wavecal'+strcompress(files[z[0]],/RE)+'-'+$
                 strcompress(files[z[count-1]],/RE)       
     arcinfo[4,i] = waveoname
     
  endfor
  
  dowavecal = 1
  
  xspextool_message,'Inspection complete.'
  
  for i = 0,w.nrows-1 do begin
     
     if flatinfo[0,i] eq '' then break
     spexcals1d_mkflat,w,reform(flatinfo[*,i]),CANCEL=cancel
     if cancel then goto, out
     spexcals1d_wavecal,w,reform(arcinfo[*,i]),CANCEL=cancel
     if cancel then goto, out

  endfor

  xspextool_message,'Task complete.',/WINONLY

  out:

end
;
;==============================================================================
;
pro spexcals1d_mkflat,w,flatinfo,DISPLAY=display,CANCEL=cancel

  cancel = 0

  common xspextool_state

  if w.basic then begin

     widget_control, /HOURGLASS

  endif

  if n_params() eq 1 then begin

     flatinfo = strarr(2)

     index    = (state.r.filereadmode eq 'Index') ? 1:0
     filename = (state.r.filereadmode eq 'Filename') ? 1:0
     
     if index then prefix = mc_cfld(state.w.iprefix_fld,7,/EMPTY,CANCEL=cancel)
     if cancel then goto, out
     
;  Get user inputs.
     
     files = mc_cfld(w.flats_fld,7,/EMPTY,CANCEL=cancel)
     if cancel then goto, out
     files = mc_fsextract(files,INDEX=index,FILENAME=filename,NFILES=nfiles,$
                          CANCEL=cancel)
     if cancel then goto, out
     
     files = mc_mkfullpath(state.r.datapath,files,INDEX=index, $
                           FILENAME=filename,NI=state.r.nint,PREFIX=prefix, $
                           SUFFIX='*.fits',WIDGET_ID=state.w.xspextool_base, $
                           /EXIST,CANCEL=cancel)
     if cancel then goto, out
     flatinfo[0] = strjoin(files,',')
     
     flatoname = mc_cfld(w.flatoname_fld,7,/EMPTY,CANCEL=cancel)
     if cancel then goto, out
     
     flatinfo[1] = flatoname
     
  endif
  
  files = strsplit(flatinfo[0],',',/EXTRACT)
  
  flatoname = (reform(flatinfo[1]))[0]

  ofile = filepath(flatoname+'.fits',ROOT=state.r.calpath)

;  Get modeinfo

  hdr  = headfits(files[0])
  mode = strcompress(fxpar(hdr,'GRAT'),/RE)

  if n_params() eq 3 then ofile = mode+'_Flat.fits'
  
  modefile = filepath(mode+'.dat',ROOT=state.r.packagepath,SUB='data')
  modeinfo = mc_readmodefile(modefile,CANCEL=cancel)
  if cancel then return

;  Compute slitw and resolving power

  slitw_arc = float( strmid(strtrim(fxpar(hdr,'SLIT'),2),0,3) )
  slitw_pix = slitw_arc /modeinfo.ps
  
  resolvingpower = modeinfo.rpppix/slitw_pix
  
;  Load the images

  call_procedure,'mc_readspexfits',files,data,hdrinfo,var, $
                 KEYWORDS=state.r.keywords, $
                 BITINFO={lincormax:state.r.lincormax,lincormaxbit:0},$
                 BITMASK=bitmask,ROTATE=modeinfo.rotation,NIMAGES=nimages, $
                 CANCEL=cancel
  if cancel then return

;  Combine flags

  bitmask = mc_combflagstack(bitmask,CANCEL=cancel)
  if cancel then return
  
;  Scale the flats to a common flux level 

  mc_scaleimgs,data,var,CANCEL=cancel
  if cancel then return
     
;  Combine the images together

  mc_combimgs,data,7,mean,var,/UPDATE,IVCUBE=var,CANCEL=cancel
  if cancel then return

;  Locate orders
  
  if ~keyword_set(NODISPLAY) then begin
     
     ximgtool, mean, SCALE='Hist Eq',WID=wid,GROUP_LEADER=group_leader, $
               BUFFER=1,STDIMAGE=state.r.stdimage, $
               PLOTWINSIZE=state.r.plotwinsize,PANNER=0,MAG=0,/ZTOFIT,$
               POSITION=[1,0]

  endif

;  Locate the orders
  
  mc_findorders,mean,modeinfo.guesspos,modeinfo.findrange,modeinfo.step, $
                modeinfo.slith_pix_range,modeinfo.edgedeg,$
                modeinfo.norm_ybuffer,modeinfo.flatfrac,modeinfo.comwin, $
                edgecoeffs,xranges,WID=wid,CANCEL=cancel
  if cancel then return

;  Normalize the flat

  if ~keyword_set(NONORM) then begin

     mean = mc_normspecflat(mean,edgecoeffs,xranges,modeinfo.slith_arc, $
                            modeinfo.norm_nxg,modeinfo.norm_nyg, $
                            modeinfo.oversamp,modeinfo.norm_ybuffer,$
                            RMS=rms,IVAR=var,OVAR=ovar, $
                            /UPDATE,WIDGET_ID=GROUP_LEADER, $
                            MODEL=model,CANCEL=cancel)
     if cancel then return    

  endif

;  Write the results to disk

  junk = tag_exist(hdrinfo[*].vals,state.r.irafname,INDEX=idx)

  history='This flat was created by scaling the files '+$
          strjoin(hdrinfo[*].vals.(idx), ', ')+ $
          ' to a common median flux value and then median combining ' + $
          'the scaled images.  The variance is given by (MAD^2/nimages) ' + $
          'where MAD is the median absolute deviation and nimages is the ' + $
          'number of input images.  The zeroth bit of pixels generated ' + $
          'from data with values greater than LINCORMX are set.'
  
  newhdr = hdrinfo[0]
  newhdr.vals.(idx) = file_basename(ofile)

  mc_writeflat,mc_unrotate(mean,modeinfo.rotation),$
               mc_unrotate(ovar,modeinfo.rotation),$
               mc_unrotate(bitmask,modeinfo.rotation),$
               newhdr,modeinfo.rotation, $
               fix(modeinfo.orders),edgecoeffs,xranges,modeinfo.ps, $
               modeinfo.slith_pix,modeinfo.slith_arc,slitw_pix,slitw_arc, $
               modeinfo.modename,rms,resolvingpower,state.w.version, $
               ofile,LINCORMAX=state.r.lincormax,HISTORY=history,CANCEL=cancel
  if cancel then return

  xspextool_message,'Wrote flat to '+ofile
  
  if w.basic then widget_control, w.flatfield_fld[1],SET_VALUE=flatoname+'.fits'

;  Display if requested.

  if keyword_set(DISPLAY) then begin
     
     mc_readflat,ofile,image,ncols,nrows,modename,ps,slith_pix,slith_arc,$
                 slitw_pix,slitw_arc,rp,norders,orders,edgecoeffs,xranges, $
                 rms,rotation,edgedeg,OMASK=omask,FLAGS=flags,CANCEL=cancel
     
     a = obj_new('mcoplotedge')
     a->set,xranges,edgecoeffs
     
     ximgtool,ofile,EXT=1,ZRANGE=[0.9,1.1], $
              GROUP_LEADER=state.w.xspextool_base,BUFFER=1, $
              ROTATION=rotation,OPLOT=a,STDIMAGE=state.r.stdimage,$
              PLOTWINSIZE=state.r.plotwinsize,POSITION=[1,0],$
              BITMASK={bitmask:flags*(omask gt 0),plot1:[0,2]}
     
  endif

  xspextool_message,'Task complete.',/WINONLY
  
  out:

end
;
;=============================================================================
;
;------------------------------ Event Handler -------------------------------
;
;=============================================================================
;
pro spexcals1d_event,event

  common xspextool_state

  widget_control, event.handler, GET_UVALUE = w, /NO_COPY
  widget_control, event.id,  GET_UVALUE = uvalue

  case uvalue of 

     'Arc Images Button': begin
        
        path= dialog_pickfile(DIALOG_PARENT=state.w.xspextool_base,$
                              /MUST_EXIST,PATH=state.r.datapath,$
                              FILTER=['*.fits','*gz'],/FIX_FILTER,$
                              /MULTIPLE_FILES)
        
        if path[0] ne '' then $
           widget_control,w.arcimages_fld[1],$
                          SET_VALUE=strjoin(file_basename(path),',',/SINGLE)
        
     end

     'Clear Table': widget_control, w.table, SET_VALUE=strarr(2,w.nrows)
     
     'Flat Field Images Button': begin
        
        path= dialog_pickfile(DIALOG_PARENT=state.w.xspextool_base,$
                              /MUST_EXIST,PATH=state.r.datapath,$
                              FILTER=['*.fits','*gz'],/FIX_FILTER,$
                              /MULTIPLE_FILES)
        
        if path[0] ne '' then $
           widget_control,w.flats_fld[1],$
                          SET_VALUE=strjoin(file_basename(path),',',/SINGLE)
        
     end

     'Full Flat Name Button': begin
        
        path= dialog_pickfile(DIALOG_PARENT=state.w.xspextool_base,$
                              /MUST_EXIST,PATH=state.r.calpath,$
                              FILTER='*.fits',/FIX_FILTER)
        
        if path ne '' then widget_control,w.flatfield_fld[1], $
                                          SET_VALUE=file_basename(path)

     end

     'Make Flat Field Button': begin

        spexcals1d_mkflat,w,/DISPLAY,CANCEL=cancel
        if cancel then goto, out
        xspextool_message,'Task complete.',/WINONLY

     end

     'LongXD': begin

        w.lxd = event.select
        widget_control, w.sky, SENSITIVE=event.select

     end

     'Wavelength Calibrate': begin

        spexcals1d_wavecal,w
        xspextool_message,'Task complete.',/WINONLY

     end

     'Make Calibration Frames': begin

        spexcals1d_doit,w
        xspextool_message,'Task complete.',/WINONLY

     end

     'Sky Images Button': begin
        
        path =dialog_pickfile(DIALOG_PARENT=state.w.xspextool_base,$
                              PATH=state.r.datapath,/MUST_EXIST,$
                              /MULTIPLE_FILES,FILTER='*.fits',/FIX_FILTER)
        
        if path[0] ne '' then $
           widget_control, w.sky_fld[1], $
                           SET_VALUE=strjoin(file_basename(path),',')
        
     end
     
     else:

  endcase

  out:
  
  widget_control, event.handler, SET_UVALUE=w, /NO_COPY

end
;
;=============================================================================
;
pro spexcals1d_wavecal,w,arcinfo,CANCEL=cancel

  cancel = 0

  common xspextool_state

  if w.basic then begin

     widget_control, /HOURGLASS

  endif

  if n_params() eq 1 then begin

     arcinfo = strarr(6)

     index    = (state.r.filereadmode eq 'Index') ? 1:0
     filename = (state.r.filereadmode eq 'Filename') ? 1:0
     
     if index then prefix = mc_cfld(state.w.iprefix_fld,7,/EMPTY,CANCEL=cancel)
     if cancel then goto, out
        
     armc_cfiles = mc_cfld(w.arcimages_fld,7,CANCEL=cancel)
     if cancel then goto, out

     armc_cfiles = mc_fsextract(armc_cfiles,INDEX=index,FILENAME=filename, $
                                CANCEL=cancel)
     if cancel then goto, out
     
     armc_cfiles = mc_mkfullpath(state.r.datapath,armc_cfiles,INDEX=index, $
                              FILENAME=filename,NI=state.r.nint, $
                              PREFIX=prefix,SUFFIX='*.fits', $
                              WIDGET_ID=state.w.xspextool_base, $
                              /EXIST,CANCEL=cancel)
     if cancel then goto, out
     arcinfo[0] = strjoin(armc_cfiles,',')

     flatname = mc_cfld(w.flatfield_fld,7,/EMPTY,CANCEL=cancel)
     if cancel then goto, out
     arcinfo[1] = flatname

     arcinfo[2] = strtrim(fix(w.lxd))

     if w.lxd then begin
        
        sky = mc_cfld(w.sky_fld,7,/EMPTY,CANCEL=cancel)
        if cancel then goto, out
        
        sky = mc_fsextract(sky,/FILENAME,NFILES=nfiles,CANCEL=cancel)
        if cancel then goto, out

        sky = mc_mkfullpath(state.r.datapath,sky,/FILENAME,$
                            WIDGET_ID=state.w.xspextool_base,/EXIST,$
                            CANCEL=cancel)
        if cancel then goto, out
        
     endif
     
     waveoname = mc_cfld(w.wavecaloname_fld,7,/EMPTY,CANCEL=cancel)
     if cancel then goto, out
     arcinfo[4] = waveoname
              
  endif

  armc_cfiles = strsplit(arcinfo[0],',',/EXTRACT)
  flat = (state.r.calpath+arcinfo[1])[0]
  pairarc = fix(arcinfo[2])
  wconame = (reform(arcinfo[4]))[0]

  if arcinfo[3] ne '' then skyfiles = strsplit(arcinfo[3],',',/EXTRACT)

  mc_mkspexwavecal1d,armc_cfiles,flat,state.r.spextoolpath, $
                     state.r.packagepath,state.r.calpath+wconame, $
                     MANUALXCORR=~state.r.autoxcorr, $
                     PLOTXCORR=state.r.plotautoxcorr,SKYFILES=skyfiles, $
                     USESTOREDSOLUTION=state.r.usestoredwc,$
                     PLOTLINEFIND=state.r.plotlinefind,$
                     GROUP_LEADER=state.w.xspextool_base,CANCEL=cancel
  if cancel then return

;  DIsplay results
 
  mc_readflat,flat,image,ncols,nrows,modename,ps,slith_pix,slith_arc,$
              slitw_pix,slitw_arc,rp,norders,orders,edgecoeffs,xranges, $
              rms,rotation,edgedeg,OMASK=omask,FLAGS=flags,CANCEL=cancel
  
  a = obj_new('mcoplotedge')
  a->set,xranges,edgecoeffs
  
  ximgtool,state.r.calpath+wconame+'.fits', $
           GROUP_LEADER=state.w.xspextool_base,STDIMAGE=state.r.stdimage, $
           PLOTWINSIZE=state.r.plotwinsize,EXT=2,BUFFER=3,OPLOT=a,$
           ZUNITS='arcsec',ROTATION=rotation,POSITION=[1,0]
  
  ximgtool,state.r.calpath+wconame+'.fits', $
           GROUP_LEADER=state.w.xspextool_base,STDIMAGE=state.r.stdimage, $
           PLOTWINSIZE=state.r.plotwinsize,EXT=1,BUFFER=2,OPLOT=a, $
           ZUNITS='um',ROTATION=rotation
  
  ximgtool,flat,GROUP_LEADER=state.w.xspextool_base,$
           STDIMAGE=state.r.stdimage, $
           PLOTWINSIZE=state.r.plotwinsize,EXT=1,BUFFER=1,OPLOT=a,/LOCK,$
           ROTATION=rotation,BITMASK={bitmask:flags*(omask gt 0),plot1:[0,2]}

  xspextool_message,'Task complete.',/WINONLY

  out:

end
;
;============================================================================
;
;------------------------------ Main Program -------------------------------
;
;============================================================================
;
pro mc_spexcals1d,parent,FAB=fab,CANCEL=cancel

  common xspextool_state

  w = {arcimages_fld:[0L,0L],$
       arconame_fld:[0L,0L],$
       basic:keyword_set(FAB),$
       flatoname_fld:[0L,0L],$
       flatfield_fld:[0L,0L],$
       flats_fld:[0L,0L],$
       lxd:0L,$
       sky:0L,$
       sky_fld:[0L,0L],$
       table:0L,$
       nrows:8,$
       wavecaloname_fld:[0L,0L]}

  mc_getfonts,buttonfont,textfont

  row_base = widget_base(parent,$
                         /ROW)
  
  if not keyword_set(FAB) then begin

     col1_base = widget_base(row_base,$
                             /COLUMN,$
                             FRAME=2)

        button = widget_button(col1_base,$
                               FONT=buttonfont,$
                               VALUE='Clear Table',$
                               UVALUE='Clear Table')      
        
        rowlab='Cal Set '+strcompress(indgen(w.nrows)+1,/RE)
        w.table = widget_table(col1_base,$
                               COLUMN_LABEL=['Flat & Arc Files', $
                                             'Sky Files (LXD)',$
                                             'Sky Prefix (LXD)'],$
                               ROW_LABEL=rowlab,$
                               COLUMN_WIDTHS=[150,150,150],$
                               XSIZE=3,$
                               YSIZE=w.nrows,$
                               ALIGNMENT=1,$
                               VALUE=strarr(3,w.nrows),$
                               /EDITABLE,$
                               UVALUE='Table')
        
     button = widget_button(col1_base,$
                            FONT=buttonfont,$
                            VALUE='Make Calibration Frames',$
                            UVALUE='Make Calibration Frames')

  endif else begin

     col1_base = widget_base(row_base,$
                             /COLUMN,$
                             FRAME=2)

        label = widget_label(col1_base,$
                             VALUE='1. Flat Field',$
                             FONT=buttonfont,$
                             /ALIGN_LEFT)
                   
        row = widget_base(col1_base,$
                          /ROW,$
                          /BASE_ALIGN_CENTER)
        
           button = widget_button(row,$
                                  FONT=buttonfont,$
                                  VALUE='Raw Flat Images',$
                                  UVALUE='Flat Field Images Button')
           
           fld = coyote_field2(row,$
                               LABELFONT=buttonfont,$
                               FIELDFONT=textfont,$
                               TITLE=':',$
                               UVALUE='Flat Field Images Field',$
                               XSIZE=20,$
;                               VALUE='646-650',$
                               TEXTID=textid) 
           w.flats_fld = [fld,textid]

        fld = coyote_field2(col1_base,$
                            LABELFONT=buttonfont,$
                            FIELDFONT=textfont,$
                            TITLE='Flat Output Name:',$
                            UVALUE='Flat Output Name',$
                            XSIZE=15,$
;                            VALUE='ftest',$
                            TEXTID=textid) 
        w.flatoname_fld = [fld,textid]

     button = widget_button(col1_base,$
                            FONT=buttonfont,$
                            VALUE='Make Flat Field',$
                            UVALUE='Make Flat Field Button')

     col2_base = widget_base(row_base,$
                             /COLUMN,$
                             FRAME=2)

        label = widget_label(col2_base,$
                             VALUE='2. Wavelength Calibration',$
                             FONT=buttonfont,$
                             /ALIGN_LEFT)
     


        row = widget_base(col2_base,$
                          /ROW,$
                          /BASE_ALIGN_CENTER)
        
           button = widget_button(row,$
                                  FONT=buttonfont,$
                                  VALUE='Raw Arc Images',$
                                  UVALUE='Arc Images Button')
           
           fld = coyote_field2(row,$
                               LABELFONT=buttonfont,$
                               FIELDFONT=textfont,$
                               TITLE=':',$
                               UVALUE='Arc Images Field',$
                               XSIZE=20,$
;                               VALUE='651-653',$
                               TEXTID=textid) 
           w.arcimages_fld = [fld,textid]

        row = widget_base(col2_base,$
                          /ROW,$
                          /BASE_ALIGN_CENTER)
        
           button = widget_button(row,$
                                  FONT=buttonfont,$
                                  VALUE='Full Flat Name',$
                                  UVALUE='Full Flat Name Button')
           
           fld = coyote_field2(row,$
                               LABELFONT=buttonfont,$
                               FIELDFONT=textfont,$
                               TITLE=':',$
;                               VALUE='ftest.fits',$
                               UVALUE='Full Flat Name',$
                               XSIZE=10,$
                               TEXTID=textID)
           w.flatfield_fld = [fld,textID]

        row = widget_base(col2_base,$
                          /ROW,$
                          /BASE_ALIGN_CENTER)

           bg = cw_bgroup(row,$
                          FONT=buttonfont,$
                          ['LongXD'],$
                          /ROW,$
                          /NONEXCLUSIVE,$
                          LABEL_LEFT='',$
                          UVALUE='LongXD',$
                          SET_VALUE=0)
           
           w.sky = widget_base(row,$
                            /ROW,$
                            /BASE_ALIGN_CENTER)
        
              button = widget_button(w.sky,$
                                     FONT=buttonfont,$
                                     VALUE='Full Sky Name(s)',$
                                     UVALUE='Sky Images Button')
              
              fld = coyote_field2(w.sky,$
                                  LABELFONT=buttonfont,$
                                  FIELDFONT=textfont,$
                                  TITLE=':',$
                                  UVALUE='Sky Images Field',$
                                  XSIZE=30,$
;                                  VALUE='spc0585.a.fits,spc0586.b.fits',$
                                  TEXTID=textid)
              w.sky_fld = [fld,textid]
           widget_control, w.sky,SENSITIVE=0

        fld = coyote_field2(col2_base,$
                            LABELFONT=buttonfont,$
                            FIELDFONT=textfont,$
                            TITLE='Wavecal Output Name:',$
                            UVALUE='Wavecal Output Name',$
                            XSIZE=15,$
;                            VALUE='wtest',$
                            TEXTID=textid) 
        w.wavecaloname_fld = [fld,textid]
        
        button = widget_button(col2_base,$
                               FONT=buttonfont,$
                               VALUE='Wavelength Calibrate',$
                               UVALUE='Wavelength Calibrate')

     endelse
        
; Start the Event Loop. This will be a non-blocking program.

  XManager, 'xmc_spexcals1d', $
            row_base, $
            EVENT_HANDLER='spexcals1d_event',$
            /NO_BLOCK

; Put state variable into the user value of the top level base.

  widget_control, row_base, SET_UVALUE=w, /NO_COPY


end
