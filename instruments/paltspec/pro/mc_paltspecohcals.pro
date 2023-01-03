
;;==============================================================================
;
; ----------------------------Support procedures------------------------------ 
;
;==============================================================================
;
pro tspeccals_mkwavecal,w,CANCEL=cancel

  cancel = 0

  common xspextool_state

;  widget_control, state.w.xspextool_base, SENSITIVE=0

  ;  Check filemode.

  index    = (state.r.filereadmode eq 'Index') ? 1:0
  filename = (state.r.filereadmode eq 'Filename') ? 1:0
  
  if index then begin

     prefix = mc_cfld(state.w.iprefix_fld,7,/EMPTY,CANCEL=cancel)
     if cancel then goto, out
  
  endif

;  Get user inputs.
  
  oname = mc_cfld(w.wconame_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then goto, out

  armc_cfiles = mc_cfld(w.skyimages_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then goto, out
  armc_cfiles = mc_fsextract(armc_cfiles,INDEX=index,FILENAME=filename, $
                             CANCEL=cancel)
  if cancel then goto, out

  if index then begin

     result = mc_checkdups(state.r.datapath,armc_cfiles,state.r.nint, $
                           WIDGET_ID=state.w.xspextool_base,$
                           CANCEL=cancel)
     if cancel then goto, out

  endif

  ;  Load files
  
  armc_cfiles = mc_mkfullpath(state.r.datapath,armc_cfiles,INDEX=index, $
                           FILENAME=filename,NI=state.r.nint,PREFIX=prefix, $
                           SUFFIX='*.fits',WIDGET_ID=state.w.xspextool_base, $
                           /EXIST,CANCEL=cancel)
  if cancel then goto, out

  flatname = mc_cfld(w.arcflat_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then goto, out
  flatfile = mc_cfile(state.r.calpath+flatname, $
                      WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
  if cancel then goto, out

;  Load data in
  
  mc_readflat,flatfile,flat,ncols,nrows,obsmode,ps,slith_pix,slith_arc, $
              slitw_pix,slitw_arc,rp,norders,orders,edgecoeffs,xranges,rms, $
              rotation,edgedeg,OMASK=omask,CANCEL=cancel
  if cancel then goto, out

  modefile = mc_cfile(filepath('XD.dat',ROOT=state.r.packagepath,SUB='data'), $
                      WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
  if cancel then goto, out

  modeinfo = mc_readmodefile(modefile,CANCEL=cancel)
  if cancel then goto, out

;  Read sky frames
  
  xspextool_message, 'Loading the sky frames...'

  mc_readpaltspecfits,armc_cfiles,data,hdrinfo,var,ROTATE=rotation, $
                      KEYWORDS=state.r.keywords,NIMAGES=nimages, $
                      CANCEL=cancel
  if cancel then goto, out
  
;  Combine sky frames together using a median.

  arccombstat = 6
  if nimages ne 1 then begin
     
     arc = median(data,/EVEN,DIMENSION=3)
     
  endif else arc = reform(data)

  arc = temporary(arc)/flat

; Do the 1D wavelength calibration across all orders

  xspextool_message, 'Wavelength Calibrating...'

;  Create fake wavecal arrays to do a simple 1D sum extraction 

  fake = mc_simwavecal2d(ncols,nrows,edgecoeffs,xranges,slith_arc,CANCEL=cancel)
  
;  Extract down the middle of the slit

  spec = mc_sumextspec1d(data,var,omask,orders,fake[*,*,0],fake[*,*,1],[1],$
                         APRANGES=rebin([slith_arc/2.-5,slith_arc/2+5],2,4),$
                         CANCEL=cancel)
  if cancel then goto, out
  
;  Read cal files and get home spectrum for cross correlation
  
  file = filepath('OHXD_LineCal.fits',ROOT=state.r.packagepath,SUB='data')

  calinfo = mc_readlinecalfile(file,CANCEL=cancel)
  if cancel then goto, out

  whome = reform(calinfo.xcorspec[*,0])
  fhome = reform(calinfo.xcorspec[*,1])
  ndat  = n_elements(whome)

  z = where(orders eq calinfo.xcororder)
  wobs = reform((spec.(z))[*,0])
  fobs = reform((spec.(z))[*,1])

  linterp,wobs,fobs,whome,ifobs,MISSING=0

  if state.r.autoxcorr then begin

     lag = findgen(ncols)-fix(ncols/2.)
     corr = c_correlate(fhome,ifobs,lag)
     max = max(corr,idx)
     
     fit = gaussfit(lag[idx-20:idx+20],corr[idx-20:idx+20],a,NTERMS=4)
     offset = a[1]

  endif else begin

     beep
     message = 'Please Identify the same line in each spectrum...'
     xspextool_message,message
     xgetoffset,whome,fhome,wobs,fobs,offset,ORDER=homeorder
     message = 'Cross-correlation offset = '+strtrim(offset,2)+' pixels.'
     xspextool_message, message 

  endelse
  
;  Load the line list

  file = filepath('OHll.dat',ROOT=state.r.packagepath,SUB='data')
  
  readcol,file,lines,FORMAT='D',COMMENT='#',/SILENT
  
  linetypes = replicate('OH',n_elements(lines))

  ordertypes = replicate('OH',norders)

  coeffs = mc_xdwavecal1d(spec,orders,calinfo.w2pcoeffs,offset,lines, $
                          linetypes,calinfo.ordertype,slitw_pix, $
                          calinfo.homeorder,modeinfo.dispdeg, $
                          modeinfo.ordrdeg,3.5, $
                          state.r.calpath+oname+'_Wavecal.ps',$
                          NLINES=nlines,WRANGES=wranges,CANCEL=cancel)
  if cancel then goto, out
  wcinfo1d = {coeffs:coeffs,dispdeg:modeinfo.dispdeg, $
              ordrdeg:modeinfo.ordrdeg,homeorder:modeinfo.homeorder}

;  Now map out the tilt of the slits

  mc_rdohll,file,plines,RES=rp,THRESH=3,CANCEL=cancel

; Display image to shows points on

  ximgtool,arc,WID=wid,GROUP_LEADER=state.w.xspextool_base, $
           STDIMAGE=state.r.stdimage,PLOTWINSIZE=state.r.plotwinsize
  
;  Find lines

  wcinfo2d = mc_findlines2d(arc,edgecoeffs,xranges,orders,plines, $
                            calinfo.w2pcoeffs,offset,wranges,slitw_pix, $
                            slith_arc,modeinfo.linedeg,modeinfo.linereg/2., $
                            modeinfo.ystep,modeinfo.ysum,WID=wid,CANCEL=cancel)
  if cancel then goto, out

;  Generate ximgtool data

  cx = wcinfo2d.(0).sx
  cy = wcinfo2d.(0).sy
  for i = 0,norders-1 do begin

     cx = [cx,wcinfo2d.(i).sx]
     cy = [cy,wcinfo2d.(i).sy]

  endfor

  a = obj_new('mcoplot')
  a->set,cx,cy,PSYM=6,COLOR=3

  ximgtool,arc,WID=wid,GROUP_LEADER=state.w.xspextool_base, $
           STDIMAGE=state.r.stdimage,PLOTWINSIZE=state.r.plotwinsize,OPLOT=a

  mc_wavecal2d,wcinfo1d,wcinfo2d,edgecoeffs,omask,orders,xranges,rotation, $
               modeinfo.wxdeg,modeinfo.wydeg,modeinfo.sxdeg,modeinfo.sydeg, $
               state.r.calpath+oname+'.fits',modeinfo.ps, $
               state.r.calpath+oname+'_Distortion.ps', $
               WIDGET_ID=state.w.xspextool_base,OPLOT=oplot,CANCEL=cancel
  if cancel then return
  
  ximgtool,arc,WID=wid,GROUP_LEADER=state.w.xspextool_base, $
           STDIMAGE=state.r.stdimage,PLOTWINSIZE=state.r.plotwinsize, $
           OPLOT=[a,oplot]

  out:

;  widget_control, state.w.xspextool_base,SENSITIVE=1

end
;
;============================================================================
;
pro tspeccals_mkflat,w,CANCEL=cancel

  cancel = 0

  common xspextool_state

;  widget_control, state.w.xspextool_base,SENSITIVE=0

;  Fix the slitw for Palomar

  slitw_arc = 1
  slitw_pix = 4.3

  ;  Check file readmode.

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

  if index then begin

     result = mc_checkdups(state.r.datapath,files,state.r.nint, $
                           WIDGET_ID=state.w.xspextool_base,$
                           CANCEL=cancel)
     if cancel then goto, out

  endif

;  Load the files

  files = mc_mkfullpath(state.r.datapath,files,INDEX=index,FILENAME=filename,$
                        NI=state.r.nint,PREFIX=prefix,SUFFIX='*.fits',$
                        WIDGET_ID=state.w.xspextool_base,/EXIST,CANCEL=cancel)
  if cancel then goto, out
     
  dark = strtrim(mc_cfld(w.flatdarks_fld,7,CANCEL=cancel),2)
  if cancel then goto, out
  if strtrim(dark,2) ne '' then $
     darkfile = mc_cfile(state.r.calpath+dark,WIDGET_ID=state.w.xspextool_base,$
                         CANCEL=cancel) else delvarx, dark
  if cancel then goto, out
  
  flatoname = mc_cfld(w.foname_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then goto, out

  step = mc_cfld(state.w.tracestepsize_fld,3,/EMPTY,CANCEL=cancel)
  if cancel then goto, out

;  Read info from cal file
  
  modefile = mc_cfile(filepath('XD.dat',ROOT=state.r.packagepath,SUB='data'), $
                      WIDGET_ID=state.w.xspextool_base,CANCEL=cancel)
  if cancel then goto, out
  
  modeinfo = mc_readmodefile(modefile,CANCEL=cancel)
  if cancel then goto, out

  resolvingpower = modeinfo.rpppix/slitw_pix
 
  xspextool_message,'Loading the flats...'

  call_procedure,state.r.fitsreadprog,files,data,hdrinfo,var, $
                 KEYWORDS=state.r.keywords,ROTATE=modeinfo.rotation,$
                 BITINFO={lincormax:state.r.lincormax,lincormaxbit:0},$
                 BITMASK=bitmask,NIMAGES=nimages,CANCEL=cancel
  if cancel then goto, out

;  Subtract dark if necessary

  if n_elements(DARKFILE) then begin
     
     xspextool_message,'Loading the dark...'
     
     dark = rotate(mrdfits(darkfile,1,/SILENT),state.r.rotation)
     darkvar  = rotate(mrdfits(darkfile,2,/SILENT),state.r.rotation)
     darkbitmask = rotate(mrdfits(darkfile,3,/SILENT),state.r.rotation)
     
     dark = rebin(reform(dark,state.r.ncols,state.r.nrows,1), $
                  state.r.ncols,state.r.nrows,nimages)
     
     darkvar = rebin(reform(darkvar,state.r.ncols,state.r.nrows,1), $
                     state.r.ncols,state.r.nrows,nimages)
     
     data = temporary(data)-dark
     var  = var+darkvar

     for i = 0,nimages-1 do begin

        bitmask[*,*,i] = mc_combflagstack([[[reform(bitmask[*,*,i])]], $
                                           [[darkbitmask]]], $
                                          CANCEL=cancel)
        if cancel then goto, out

     endfor
     
     darkfile = file_basename(darkfile)
     
  endif

  if nimages ne 1 then begin
     
;  Scale the the images (each set of orders) to the same level.
     
     xspextool_message,'Scaling the flats...'
     
     mc_scaleimgs,data,var,CANCEL=cancel
     if cancel then goto, out
     
;  Combine the images together.
     
     xspextool_message,'Combining the flats...'
     
     mc_combimgs,data,6,mean,var,/UPDATE,THRESH=thresh,IVCUBE=var,CANCEL=cancel
     if cancel then goto, out
     
  endif else mean = data
  if cancel then goto, out

;  Combine flags

  bitmask = mc_combflagstack(bitmask,CANCEL=cancel)
  if cancel then goto, out
  
;  Find orders

  ximgtool, mean, SCALE='Hist Eq',WID=wid, $
            GROUP_LEADER=state.w.xspextool_base,RANGE='0-Max',BUFFER=1, $
            STDIMAGE=state.r.stdimage,PLOTWINSIZE=state.r.plotwinsize,/ZTOFIT

  xspextool_message,'Locating the orders...'
  
  if state.w.notspex and modeinfo.fixed eq 'Yes' then begin
          
     plots, !x.mc_crange,[modeinfo.guesspos[0],modeinfo.guesspos[0]],COLOR=2
     plots, !x.mc_crange,[modeinfo.guesspos[1],modeinfo.guesspos[1]],COLOR=2
     xranges = modeinfo.findrange
     edgecoeffs = fltarr(2,2,1)
     edgecoeffs[0,*,0] = modeinfo.guesspos
     edgecoeffs[1,*,0] = [0.,0.]
     
  endif else begin

     mc_findorders,mean,modeinfo.guesspos,modeinfo.findrange,modeinfo.step,$
                   modeinfo.slith_pix_range,modeinfo.edgedeg,$
                   modeinfo.norm_ybuffer,modeinfo.flatfrac,modeinfo.comwin,$
                   edgecoeffs,xranges,WID=wid,CANCEL=cancel
     if cancel then goto, out
     
  endelse
  
  ximgtool, mean, SCALE='Hist Eq',WID=wid,GROUP_LEADER=group_leader, $
            BUFFER=1,STDIMAGE=state.r.stdimage, $
            BITMASK={bitmask:bitmask,plot1:[0,2]}, $
            PLOTWINSIZE=state.r.plotwinsize,PANNER=0,MAG=0,/ZTOFIT, $
            POSITION=[1,0]
  
  normflat = 1
  if normflat then begin
     
     xspextool_message,'Normalizing the flat...'

     mean = mc_normspecflat(mean,edgecoeffs,xranges,modeinfo.slith_arc,$
                            modeinfo.norm_nxg,modeinfo.norm_nyg,$
                            modeinfo.oversamp,modeinfo.norm_ybuffer,RMS=rms,$
                            /UPDATE,WIDGET_ID=state.w.xspextool_base,$
                            MODEL=model,CANCEL=cancel)
     if cancel then goto, out    

;  Replace bad pixels with 
     
     z = where(rotate(*state.d.mask,modeinfo.rotation) eq 0,count)
     if count ne 0 then mean[z] = 1.0
     
  endif else rms = replicate(0.0,norders)
  
;  Write the results to disk.
  
  junk = tag_exist(hdrinfo[*].vals,state.r.irafname,INDEX=idx)

  history='This flat was created by scaling the files '+$
          strjoin(hdrinfo[*].vals.(idx), ', ')+ $
          ' to a common median flux value and then median combining ' + $
          'the scaled images.  The variance is given by (MAD^2/nimages) ' + $
          'where MAD is the median absolute deviation and nimages is the ' + $
          'number of input images.  The zeroth bit of pixels generated ' + $
          'from data with values greater than LINCORMX are set.'

  newhdr = hdrinfo[0]
  newhdr.vals.(idx) = flatoname+'.fits'

  mc_writeflat,mc_unrotate(mean,modeinfo.rotation),$
               mc_unrotate(var,modeinfo.rotation),$
               mc_unrotate(byte(bitmask),modeinfo.rotation),$
               newhdr,modeinfo.rotation,fix(modeinfo.orders),edgecoeffs, $
               xranges,modeinfo.ps,modeinfo.slith_pix,modeinfo.slith_arc, $
               slitw_pix,slitw_arc,modeinfo.modename,rms,resolvingpower, $
               state.w.version,state.r.calpath+flatoname+'.fits', $
               LINCORMAX=state.r.lincormax,HISTORY=history,CANCEL=cancel
  if cancel then return

  
  xspextool_message,'Wrote flat to '+filepath(flatoname+'.fits', $
                                              ROOT=state.r.calpath)
  
  a = obj_new('mcoplotedge')
  a->set,xranges,edgecoeffs

  mc_readflat,state.r.calpath+flatoname+'.fits',image,ncols,nrows, $
              modename,ps,slith_pix,slith_arc,slitw_pix,slitw_arc,rp, $
              norders,orders,edgecoeffs,xranges,rms,rotation,edgedeg, $
              FLAGS=flags,OMASK=omask,CANCEL=cancel

  
  if normflat then begin

     ximgtool,state.r.calpath+flatoname+'.fits',EXT=1,ZRANGE=[0.9,1.1], $
              GROUP_LEADER=state.w.xspextool_base, $
              BUFFER=1,ROTATION=rotation,OPLOT=a,POSITION=[1,0], $
              STDIMAGE=state.r.stdimage,$
              BITMASK={bitmask:flags*(omask gt 0),plot1:[0,2]}, $
              PLOTWINSIZE=state.r.plotwinsize
          
  endif else begin

     ximgtool,state.r.calpath+flatoname+'.fits',EXT=1,/ZSCL, $
              GROUP_LEADER=state.w.xspextool_base, $
              BUFFER=1,ROTATION=rotation,OPLOT=a,POSITION=[1,0], $
              STDIMAGE=state.r.stdimage,$
              BITMASK={bitmask:flags*(omask gt 0),plot1:[0,2]}, $
              PLOTWINSIZE=state.r.plotwinsize
         
  endelse
  
  widget_control, w.arcflat_fld[1],SET_VALUE=flatoname+'.fits'

  out:

;  widget_control, state.w.xspextool_base,SENSITIVE=1
  
end
;
;============================================================================
;
;------------------------------ Event Handler -------------------------------
;
;============================================================================
;
pro tspeccals_event,event

  common xspextool_state

  widget_control, event.handler, GET_UVALUE = w, /NO_COPY
  widget_control, event.id,  GET_UVALUE = uvalue
  widget_control, /HOURGLASS

  case uvalue of 

     'Arc Flat Field Button': begin
        
        path =dialog_pickfile(DIALOG_PARENT=state.w.xspextool_base,$
                              PATH=state.r.calpath,/MUST_EXIST,$
                              FILTER='*.fits',/FIX_FILTER)
        
        if path ne '' then widget_control,w.arcflat_fld[1], $
                                          SET_VALUE=file_basename(path)
        
     end

     'Sky Images Button': begin
        
        path= dialog_pickfile(DIALOG_PARENT=state.w.xspextool_base,$
                              /MUST_EXIST,PATH=state.r.datapath,$
                              FILTER=['*.fits'],/FIX_FILTER,$
                              /MULTIPLE_FILES)
        
        if path[0] ne '' then $
           widget_control,w.skyimages_fld[1],$
                          SET_VALUE=strjoin(file_basename(path),',',/SINGLE)
        
     end

     'Construct Flat': tspeccals_mkflat,w,CANCEL=cancel

     'Flat Field Dark Button': begin
        
        path =dialog_pickfile(DIALOG_PARENT=state.w.xspextool_base,$
                              PATH=state.r.calpath,/MUST_EXIST,$
                              FILTER='*.fits',/FIX_FILTER)
        
        if path ne '' then $
           widget_control,w.flatdarks_fld[1],$
                          SET_VALUE =strjoin(file_basename(path),',',/SINGLE)

     end

     'Flat Field Images Button': begin
        
        path= dialog_pickfile(DIALOG_PARENT=state.w.xspextool_base,$
                              /MUST_EXIST,PATH=state.r.datapath,$
                              FILTER=['*.fits','*gz'],/FIX_FILTER,$
                              /MULTIPLE_FILES)
        
        if path[0] ne '' then $
           widget_control,w.flats_fld[1],$
                          SET_VALUE=strjoin(file_basename(path),',',/SINGLE)
        
     end

     'Wavelength Calibrate': tspeccals_mkwavecal,w,CANCEL=cancel

  endcase

  widget_control, event.handler, SET_UVALUE=w, /NO_COPY

end
;
;============================================================================
;
;------------------------------ Main Program -------------------------------
;
;============================================================================
;
pro mc_paltspecohcals,parent,FAB=fab,CANCEL=cancel
  
  cancel = 0
  
;  Create the widget

  w = {skyimages_fld:[0L,0L],$
       arcflat_fld:[0L,0L],$
       arcskyrow:0L,$
       flats_fld:[0L,0L],$
       flatdarks_fld:[0L,0L],$
       foname_fld:[0L,0L],$
       linedeg_fld:[0L,0L],$
       linewin_fld:[0L,0L],$
       parms_base:0L,$
       showparms:0L,$
       rotation:0,$
       ysum_fld:[0L,0L],$
       ystep_fld:[0L,0L],$
       wdx_fld:[0L,0L],$
       wdy_fld:[0L,0L],$
       slitw_arc_fld:[0L,0L],$
       slitw_pix_fld:[0L,0L],$
       sdx_fld:[0L,0L],$
       sdy_fld:[0L,0L],$
       wconame_fld:[0L,0L]}

  mc_getfonts,buttonfont,textfont,CANCEL=cancel
  if cancel then return

  base = widget_base(parent,$
                     /ROW)

  col1_base = widget_base(base,$
                          /COLUMN,$
                          FRAME=2)

     label = widget_label(col1_base,$
                          VALUE='1. Create Flat',$
                          /ALIGN_LEFT,$
                          FONT=buttonfont)

;     row = widget_base(col1_base,$
;                       /ROW,$
;                       /BASE_ALIGN_CENTER)
;     
;        fld = coyote_field2(row,$
;                            LABELFONT=buttonfont,$
;                            FIELDFONT=textfont,$
;                            TITLE='Slit Width (Pix/Arc):',$
;                            UVALUE='Slit Width Pix',$
;                            XSIZE=5,$
;                            TEXTID=textid) 
;        w.slitw_pix_fld = [fld,textid]
;        
;        fld = coyote_field2(row,$
;                            LABELFONT=buttonfont,$
;                            FIELDFONT=textfont,$
;                            TITLE='/ ',$
;                            UVALUE='Slit Width Arc',$
;                            XSIZE=5,$
;                            TEXTID=textid) 
;        w.slitw_arc_fld = [fld,textid]

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
                            XSIZE=15,$
                            TEXTID=textid) 
        w.flats_fld = [fld,textid]
        
     row = widget_base(col1_base,$
                       /ROW,$
                       /BASE_ALIGN_CENTER)
     
        button = widget_button(row,$
                               FONT=buttonfont,$
                               VALUE='Full Dark Name',$
                               UVALUE='Flat Field Dark Button')
        
        fld = coyote_field2(row,$
                            LABELFONT=buttonfont,$
                            FIELDFONT=textfont,$
                            TITLE=':',$
                            UVALUE='Flat Field Dark Field',$
                            XSIZE=15,$
                            TEXTID=textid) 
        w.flatdarks_fld = [fld,textid]

     fld = coyote_field2(col1_base,$
                         LABELFONT=buttonfont,$
                         FIELDFONT=textfont,$
                         TITLE='Output Flat Name:',$
                         UVALUE='Output Flat Name',$
                         XSIZE=15,$
;                         VALUE='wtest',$
                         TEXTID=textid) 
     w.foname_fld = [fld,textid]

     button = widget_button(col1_base,$
                            FONT=buttonfont,$
                            VALUE='Construct Flat',$
                            UVALUE='Construct Flat')

  col2_base = widget_base(base,$
                          /COLUMN,$
                          FRAME=2)

     label = widget_label(col2_base,$
                          VALUE='2. Wavelength Calibration',$
                          /ALIGN_LEFT,$
                          FONT=buttonfont)

     row = widget_base(col2_base,$
                       /BASE_ALIGN_CENTER,$
                       /ROW)
     
        button = widget_button(row,$
                               FONT=buttonfont,$
                               VALUE='Sky Images',$
                               UVALUE='Sky Images Button')
        
        fld = coyote_field2(row,$
                            LABELFONT=buttonfont,$
                            FIELDFONT=textfont,$
                            TITLE=':',$
                            UVALUE='Sky Images Field',$
                            XSIZE=30,$
;                            VALUE='25',$
                            TEXTID=textid) 
        w.skyimages_fld = [fld,textid]
        
     row = widget_base(col2_base,$
                       /ROW,$
                       /BASE_ALIGN_CENTER)
     
        button = widget_button(row,$
                               FONT=buttonfont,$
                               VALUE='Full Flat Name',$
                               UVALUE='Arc Flat Field Button')
        
        fld = coyote_field2(row,$
                            LABELFONT=buttonfont,$
                            FIELDFONT=textfont,$
                            TITLE=':',$
                            UVALUE='Arc Flat Field',$
                            XSIZE=15,$
;                            VALUE='flat12-20.fits',$
                            TEXTID=textid) 
        w.arcflat_fld = [fld,textid]

     fld = coyote_field2(col2_base,$
                         LABELFONT=buttonfont,$
                         FIELDFONT=textfont,$
                         TITLE='Output Wavecal Name:',$
                         UVALUE='Output Wavecal Name',$
                         XSIZE=15,$
;                         VALUE='wavecal25',$
                         TEXTID=textid) 
     w.wconame_fld = [fld,textid]
     
     button = widget_button(col2_base,$
                            FONT=buttonfont,$
                            VALUE='Wavelength Calibrate',$
                            UVALUE='Wavelength Calibrate')
              
; Start the Event Loop. This will be a non-blocking program.

  XManager, 'xmc_paltspeccals', $
            base, $
            EVENT_HANDLER='tspeccals_event',$
            /NO_BLOCK
  
; Put state variable into the user value of the top level base.
  
  widget_control, base, SET_UVALUE=w, /NO_COPY

end
