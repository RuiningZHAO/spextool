;
;=============================================================================
;
; ----------------------------Support procedures------------------------------ 
;
;=============================================================================
;
pro uspexcals1d_doit,w,CANCEL=cancel

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

     xspextool_message,'Inspecting Cal Set '+strcompress(i+1,/RE)+'...'
     
     files = strtrim(array[0,i],2)
     if files eq '' then continue
          
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
     flats = mc_finduspexflats(fullpaths,MASK=flatmask,CANCEL=cancel)
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

     arcs = mc_finduspexarcs(fullpaths,MASK=arcmask,AB=ab,CANCEL=cancel)
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

     if obsmode eq 'LXD_long' or obsmode eq 'LXD_short' then begin

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
     uspexcals1d_mkflat,w,reform(flatinfo[*,i]),CANCEL=cancel
     if cancel then goto, out
     uspexcals1d_wavecal,w,reform(arcinfo[*,i]),CANCEL=cancel
     if cancel then goto, out

  endfor

  xspextool_message,'Task complete.',/WINONLY

  out:

end
;
;==============================================================================
;
pro uspexcals1d_mkflat,w,flatinfo,DISPLAY=display,CANCEL=cancel

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
  dit = fxpar(hdr,'DIT')

  if size(dit,/TYPE) ne 7 then mode = mode+'_'+string(dit,FORMAT='(F3.1)')

  modefile = filepath(mode+'.dat',ROOT=state.r.packagepath,SUB='data')
  modeinfo = mc_readmodefile(modefile,CANCEL=cancel)
  if cancel then return

;  Compute slitw and resolving power

  slitw_arc = float( strmid(strtrim(fxpar(hdr,'SLIT'),2),0,3) )
  slitw_pix = slitw_arc / modeinfo.ps
  
  resolvingpower = modeinfo.rpppix/slitw_pix
  
;  Load the images

  call_procedure,'mc_readuspexfits',files,data,hdrinfo,var, $
                 KEYWORDS=state.r.keywords,$
                 BITINFO={lincormax:state.r.lincormax,lincormaxbit:0},$
                 BITMASK=bitmask,ROTATE=modeinfo.rotation,NIMAGES=nimages, $
                 CANCEL=cancel
  if cancel then goto, out

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
               BITMASK={bitmask:bitmask,plot1:[0,2]}, $
               PLOTWINSIZE=state.r.plotwinsize,PANNER=0,MAG=0,/ZTOFIT, $
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
                            /UPDATE,WIDGET_ID=state.w.xspextool_base, $
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
               mc_unrotate(byte(bitmask),modeinfo.rotation),$
               newhdr,modeinfo.rotation,fix(modeinfo.orders),edgecoeffs, $
               xranges,modeinfo.ps,modeinfo.slith_pix,modeinfo.slith_arc, $
               slitw_pix,slitw_arc,modeinfo.modename,rms,resolvingpower, $
               state.w.version,ofile,LINCORMAX=state.r.lincormax, $
               HISTORY=history,CANCEL=cancel
  if cancel then return

  xspextool_message,'Wrote flat to '+ofile
  
  if w.basic then widget_control, w.flatfield_fld[1],SET_VALUE=flatoname+'.fits'

;  Display if requested.

  if keyword_set(DISPLAY) then begin
     
     mc_readflat,ofile,image,ncols,nrows,modename,ps,slith_pix,slith_arc,$
                 slitw_pix,slitw_arc,rp,norders,orders,edgecoeffs,xranges, $
                 rms,rotation,edgedeg,FLAGS=flags,OMASK=omask,CANCEL=cancel

     a = obj_new('mcoplotedge')
     a->set,xranges,edgecoeffs

     ximgtool,ofile,EXT=1,ZRANGE=[0.9,1.1], $
              GROUP_LEADER=state.w.xspextool_base, $
              BUFFER=1,ROTATION=rotation,OPLOT=a,POSITION=[1,0], $
              STDIMAGE=state.r.stdimage,$
              BITMASK={bitmask:flags*(omask gt 0),plot1:[0,2]}, $
              PLOTWINSIZE=state.r.plotwinsize
     
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
pro uspexcals1d_event,event

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

     'Clear Table': widget_control, w.table, SET_VALUE=strarr(3,w.nrows)
     
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

        uspexcals1d_mkflat,w,/DISPLAY,CANCEL=cancel
        if cancel then goto, out
        xspextool_message,'Task complete.',/WINONLY

     end

     'LongXD': begin

        w.lxd = event.select
        widget_control, w.sky, SENSITIVE=event.select

     end

     'Wavelength Calibrate': begin

        uspexcals1d_wavecal,w,CANCEL=cancel
        if cancel then goto, out
        xspextool_message,'Task complete.',/WINONLY

     end

     'Make Calibration Frames': begin

        uspexcals1d_doit,w,CANCEL=cancel
        if cancel then goto, out
        
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
pro uspexcals1d_wavecal,w,arcinfo,CANCEL=cancel

  cancel = 0

  common xspextool_state

  if w.basic then begin

     widget_control, /HOURGLASS

  endif

;  If arcinfo isn't passed, then construct it from the widget info.

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

        arcinfo[3] = sky
        
     endif
     
     waveoname = mc_cfld(w.wavecaloname_fld,7,/EMPTY,CANCEL=cancel)
     if cancel then goto, out
     arcinfo[4] = waveoname
              
  endif

  armc_cfiles = strsplit(arcinfo[0],',',/EXTRACT)
  flatname = (state.r.calpath+arcinfo[1])[0]
  pairarc = fix(arcinfo[2])
  wconame = (reform(arcinfo[4]))[0]

  if arcinfo[3] ne '' then skyfiles = strsplit(arcinfo[3],',',/EXTRACT)

;  Get modeinfo

  hdr  = headfits(armc_cfiles[0])
  mode = strcompress(fxpar(hdr,'GRAT'),/RE)
  
  modefile = filepath(mode+'.dat',ROOT=state.r.packagepath,SUB='data')
  modeinfo = mc_readmodefile(modefile,CANCEL=cancel)
  if cancel then goto, out

;  Read flat field

  mc_readflat,flatname,flat,ncols,nrows,obsmode,ps,slith_pix,slith_arc, $
              slitw_pix,slitw_arc,rp,norders,orders,edgecoeffs,xranges,rms, $
              rotation,edgedeg,OMASK=omask,FLAGS=flatflags,CANCEL=cancel
  if cancel then goto, out


;  Set some keywords for the wavelength calibration

  if slitw_arc eq 1.6 or slitw_arc eq 3.0 then begin

     usestoredsolution = 1
     plotxcorr = 1
     
  endif else begin

     plotxcorr = state.r.plotautoxcorr
     usestoredsolution = 0
     
  endelse
  
;  Load images in

  pair = (n_elements(skyfiles) eq 0) ? 0:1

  if pair then begin

     armc_cfiles = mc_reorder(armc_cfiles,CANCEL=cancel)
     if cancel then goto, out

  endif
  
  call_procedure,'mc_readuspexfits',armc_cfiles,data,hdrinfo,var, $
                 KEYWORDS=state.r.keywords,$
                 BITINFO={lincormax:state.r.lincormax,lincormaxbit:0},$
                 BITMASK=arcbitmask,ROTATE=modeinfo.rotation,$
                 PAIR=pair,NIMAGES=nimages, $
                 CANCEL=cancel
  if cancel then goto, out

;  Combine images if necessary

  if nimages ne 1 then begin

     mc_meancomb,data,arc,arcvar,DATAVAR=datavar,CANCEL=cancel
     if cancel then goto, out

     arcbitmask = mc_combflagstack(arcbitmask,CANCEL=cancel)
     if cancel then goto, out
     
  endif else begin
  
     arc    = reform(data)
     arcvar = reform(var)
     arcbitmask = arcbitmask

     
  endelse
  
  arc = temporary(arc)/flat
  arcvar = temporary(arcvar)/flat^2
  
;  LXD?  Get sky

  if n_elements(SKYFILES) ne 0 then begin
     
     call_procedure,'mc_readuspexfits',skyfiles,skyimgs,hdr,skyvar, $
                    KEYWORDS=state.r.keywords,$
                    BITINFO={lincormax:state.r.lincormax,lincormaxbit:0},$
                    BITMASK=skybitmask,ROTATE=modeinfo.rotation, $
                    NIMAGES=nimages,CANCEL=cancel
     if cancel then goto, out
     
     
     if nimages eq 2 then begin
        
        sky = (skyimgs[*,*,0]+skyimgs[*,*,1])- $
              abs(skyimgs[*,*,0]-skyimgs[*,*,1])
        skyvar = 2*total(skyvar,3)
        
        skyhistory = '  The sky frames are '+ $
                     strjoin([hdr[0].vals.IRAFNAME,hdr[1].vals.IRAFNAME],', ')+$
                     '.'
        
        skybitmask = mc_combflagstack(skybitmask,CANCEL=cancel)
        if cancel then goto, out
        
     endif else begin
        
        sky = skyimgs
        skyhistory = '  The sky frame is '+hdr[0].vals.IRAFNAME+'.'
        
     endelse

     tmpsky = sky
     tmparc = arc
     
     case obsmode of 
        
        'LXD_short': begin

           arc = mc_mixorders(arc,sky,omask,orders,[5,7,8,9,10,11],[6], $
                              CANCEL=cancel)
           if cancel then goto, out
           mask = mc_mixorders(arcbitmask,skybitmask,omask,orders,$
                               [5,7,8,9,10,11],[6],CANCEL=cancel)
           if cancel then goto, out
        end

        'LXD_long': begin

           arc = mc_mixorders(arc,sky,omask,orders,[5,7,8,9,10],[4,6], $
                              CANCEL=cancel)
           if cancel then goto, out
           
           mask = mc_mixorders(arcbitmask,skybitmask,omask,orders,$
                               [5,7,8,9,10],[4,6],CANCEL=cancel)
           if cancel then goto, out
           
        end

     endcase
     if cancel then goto, out

  endif else begin

     skyhistory = ''
     mask = arcbitmask

  endelse
  
;  Create fake wavecal arrays to do a simple 1D sum extraction on the arc

  fake = mc_simwavecal2d(ncols,nrows,edgecoeffs,xranges,slith_arc,CANCEL=cancel)
  if cancel then goto, out

  wavecal = reform(fake[*,*,0])
  spatcal = reform(fake[*,*,1])
;  
;;  Create fake traces to plot on ximgtool
;
;  coeffs = fltarr(2,2*norders)
;  ap = [6,8]
;  coeffs[0,*] = reform(replicas(ap,norders),1,2*norders)
;
;;  Triangulate the cal files
;  
;  for i = 0,norders-1 do begin
;        
;     z = where(omask eq orders[i])
;        
;;  Triangulate the wavecal and spatcal arrays
;        
;     triangulate,double(wavecal[z]),double(spatcal[z]),tri
;     
;     name = 'ORD'+string(orders[i],FORMAT='(I2.2)')
;     tris = (i eq 0) ? create_struct(name,tri):create_struct(tris,name,tri)
;     
;  endfor
;  
;  
;  mc_tracetoap2d,omask,fake[*,*,0],fake[*,*,1],tris,coeffs,2, $
;                 orders,make_array(norders,VALUE=1),appos,struc,CANCEL=cancel
;  if cancel then return
;
;  d = obj_new('mcplotap')
;  d->set,struc,3

 ;  Display arc
  
  ximgtool,arc,GROUP_LEADER=state.w.xspextool_base,STDIMAGE=state.r.stdimage, $
           PLOTWINSIZE=state.r.plotwinsize,BUFFER=1,ROTATION=rotation, $
           POSITION=[1,0],BITMASK={mask:mask,plot1:[0,2]}

  
;  Get Line Cal file

  file = filepath(obsmode+'_LineCal.fits',ROOT=state.r.packagepath,SUB='data')
  calinfo = mc_readlinecalfile(file,CANCEL=cancel)
  if cancel then goto, out
  
;  Get setup for the cross correlation 
  
  wanchor = calinfo.xcorspec[*,0]
  fanchor = calinfo.xcorspec[*,1]

;  Extract the arc down the middle of the slit in the anchororder

  arcspec = mc_sumextspec1d(arc,arcvar,omask,orders,wavecal,spatcal, $
                            [1],APRANGES=rebin([6,8],2,norders),CANCEL=cancel)
  if cancel then goto, out

  z = where(orders eq calinfo.xcororder)
  
  warc = (arcspec.(z))[*,0]
  farc = (arcspec.(z))[*,1]
  
;  Do the cross correlation

  linterp,warc,farc,wanchor,ifarc,MISSING=0

  if state.r.autoxcorr then begin

     npixels = n_elements(fanchor)
     lag = findgen(npixels)-fix(npixels/2.)

     bad = where(finite(fanchor) eq 0,badcnt)
     if badcnt ne 0 then fanchor[bad] = 0.0

     bad = where(ifarc lt 0,badcnt)
     if badcnt ne 0 then ifarc[bad] = 0.0
     
     corr = c_correlate(fanchor,ifarc,lag)

     max = max(corr,idx)
     
     win = 10*(slitw_arc/0.3*0.5 >1 )

     fit = mpfitpeak(lag[idx-win:idx+win],corr[idx-win:idx+win],a,NTERMS=4)
     offset = a[1]
     
     if plotxcorr then begin
        
        window, /FREE,XSIZE=512,YSIZE=768
        xwid = !d.window
        !p.multi[0] = 2
        !p.multi[2] = 2
        
        plot, lag,corr,/xsty,/ysty
        plot,lag[idx-win:idx+win],corr[idx-win:idx+win],/xsty,/ysty,$
             TITLE='!5Offset = '+strtrim(offset,2)
        oplot, lag[idx-win:idx+win],fit,color=2
        plots,[offset,offset],!y.crange,COLOR=2

        result = dialog_message('Is this ok?',/QUESTION)
        
        if strtrim(result,2) eq 'No' then begin
           
           wdelete, xwid
           goto, manual1
           
        endif else wdelete, xwid
        
     endif
     
  endif else begin 
     
     manual1:
     beep
     xgetoffset,wanchor,fanchor,wanchor,ifarc,offset,ORDER=anchororder

  endelse

  print
  print, 'Cross-correlation offset = '+strtrim(offset,2)+' pixels.'
  print

;  Now compute the wavelength solution

  if obsmode eq 'Prism' or usestoredsolution then begin
     
     homeorder = calinfo.homeorder
     dispdeg = calinfo.dispdeg
     ordrdeg = calinfo.ordrdeg
     p2wcoeffs = calinfo.p2wcoeffs
     
  endif else begin
     
     homeorder = modeinfo.homeorder
     dispdeg = modeinfo.dispdeg
     ordrdeg = modeinfo.dispdeg
     
;  Grab the lines.
     
     file = filepath('lines.dat',ROOT=state.r.packagepath,SUB='data')
     mc_rdarll,file,plines,ptypes,RES=rp,THRESH=3,CANCEL=cancel
     if cancel then return
     
;  Perform wavelength calibration
     
     p2wcoeffs = mc_xdwavecal1d(arcspec,orders,calinfo.w2pcoeffs, $
                                offset,plines,ptypes,calinfo.ordertype, $
                                slitw_pix,homeorder,dispdeg,ordrdeg,4, $
                                state.r.calpath+wconame+'_QA',RMS=rms,$
                                NLINES=nlines,/UPDATE,WIDGET_ID=group_leader,$
                                PLOT=state.r.plotlinefind,CANCEL=cancel)
     if cancel then return

     bad = where(nlines le 2,cnt)
     if cnt ne 0 then begin
        
        message = strjoin(strtrim(nlines[bad],2),',')+ $
                  ' lines were found in orders '+ $
                  strjoin(strtrim(orders[bad],2),',')+'.  Although the ' + $
                  'program may finish, your wavelength solution is ' + $
                  'probably bad.  Please check both the accuracy of the ' + $
                  'wavelength solution and the QA plot in the cal/ directory.'
        
        ok = dialog_message(mc_splittext(message,50),$
                            DIALOG_PARENT=state.w.xspextool_base)
        
     endif
     
  endelse

;  Now create wavelength calibration image

  wave = fltarr(ncols,nrows,/NOZERO)*!values.f_nan
  spat = fltarr(ncols,nrows,/NOZERO)*!values.f_nan
  
  disp = fltarr(norders)
  
  for j = 0, norders-1 do begin
     
     start = xranges[0,j]
     stop  = xranges[1,j]
     x     = findgen(stop-start+1)+start
     y     = findgen(nrows)
     
     if obsmode eq 'Prism' then begin
        
        result = poly(x-offset,p2wcoeffs)

     endif else begin

        result = poly2d(x,replicate(fix(orders[j]),n_elements(x)),dispdeg,$
                        ordrdeg,p2wcoeffs,CANCEL=cancel)*$
                 (float(homeorder)/float(orders[j]))
        if cancel then return
        
     endelse
     
     botedge = poly1d(x,edgecoeffs[*,0,j])
     topedge = poly1d(x,edgecoeffs[*,1,j])
     dif = topedge-botedge
     pixtoarc = slith_arc/dif
     
     for k = 0,stop-start do begin
        
        if topedge[k] gt nrows-0.5 or botedge[k] lt -0.5 then continue
        wave[k+start,botedge[k]:topedge[k]] = result[k]
        
        spat[x[k],(botedge[k]):topedge[k]] = $
           poly(y[(botedge[k]):topedge[k]],$
                [-1*pixtoarc[k]*botedge[k],pixtoarc[k]])
                     
     endfor
     
     coeff = poly_fit1d(x,result,1,/SILENT)
     disp[j] = coeff[1]
     
  endfor

;  Write the resulting wave and spat cal images to disk

  mwrfits,input,state.r.calpath+wconame+'.fits',nhdr,/CREATE
  sxaddpar,nhdr,state.r.calpath+wconame+'.fits',' Filename'
  sxaddpar,nhdr,'WCTYPE','1D',' Wavelength calibration type'
  if obsmode ne 'Prism' and ~keyword_set(USESTOREDSOLUTION) then $
     sxaddpar,nhdr,'RMS',rms,' RMS of wavecal fit in Angstroms'

  for j = 0, norders-1 do begin
        
        name = 'DISPO'+string(orders[j],format='(i2.2)')
        comment = ' Dispersion (microns pixel-1) for order '+ $
                  string(orders[j],FORMAT='(i2.2)')
        sxaddpar,nhdr,name,disp[j],comment
        
     endfor

  if skyhistory ne '' then begin

     history = mc_splittext(skyhistory,70,CANCEL=cancel)
     if cancel then return
     sxaddhist,history,nhdr
     
  endif

  mwrfits,input,state.r.calpath+wconame+'.fits',nhdr,/CREATE

;  Write wavelength calibration extension

  writefits,state.r.calpath+wconame+'.fits',mc_unrotate(wave,0),/APPEND

;  Write spatial calibration to second extension

  writefits,state.r.calpath+wconame+'.fits',mc_unrotate(spat,0),/APPEND

;  Write the bitmask the third extension

  writefits,state.r.calpath+wconame+'.fits',mc_unrotate(byte(mask),0),/APPEND

  
  a = obj_new('mcoplotedge')
  a->set,xranges,edgecoeffs
  
  ximgtool,mc_unrotate(spat,0), $
           GROUP_LEADER=state.w.xspextool_base,STDIMAGE=state.r.stdimage, $
           PLOTWINSIZE=state.r.plotwinsize,EXT=2,BUFFER=3,OPLOT=a,$
           ZUNITS='arcsec',ROTATION=rotation,POSITION=[1,0],$
           BITMASK={bitmask:mask*(omask gt 0),plot1:[0,2]},$
           FILENAME='Spatial Calibration'
  
  ximgtool,mc_unrotate(wave,0),$
           GROUP_LEADER=state.w.xspextool_base,STDIMAGE=state.r.stdimage, $
           PLOTWINSIZE=state.r.plotwinsize,EXT=1,BUFFER=2,OPLOT=a, $
           ZUNITS='um',ROTATION=rotation,$
           BITMASK={bitmask:mask*(omask gt 0),plot1:[0,2]},$
           FILENAME='Wavelength Calibration'
  
  ximgtool,flat,GROUP_LEADER=state.w.xspextool_base,$
           STDIMAGE=state.r.stdimage, $
           PLOTWINSIZE=state.r.plotwinsize,EXT=1,BUFFER=1,OPLOT=a,/LOCK,$
           ZRANGE=[0.9,1.1],ROTATION=rotation, $
           BITMASK={bitmask:flatflags*(omask gt 0),plot1:[0,2]},$
           FILENAME='Normalized Flat'

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
pro mc_uspexcals1d,parent,FAB=fab,CANCEL=cancel

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
                               XSIZE=19,$
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

  XManager, 'xmc_uspexcals1d', $
            row_base, $
            EVENT_HANDLER='uspexcals1d_event',$
            /NO_BLOCK

; Put state variable into the user value of the top level base.

  widget_control, row_base, SET_UVALUE=w, /NO_COPY


end
