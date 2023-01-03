;+
; NAME:
;     xqspextool
;    
; PURPOSE:
;     The quicklook version of Spextool
;    
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     xquicklook,CANCEL=cancel
;    
; INPUTS:
;     None
;    
; OPTIONAL INPUTS:
;     The name of the instrument to be used.
;
; KEYWORD PARAMETERS:
;     None
;     
; OUTPUTS:
;     None
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
;     2010-08-17 - Written by M. Cushing, NASA JPL
;-


;
;******************************************************************************
;
; ------------------------------Event Handlers-------------------------------- 
;
;******************************************************************************
;
pro xqspextool_event, event

  widget_control, event.id,  GET_UVALUE = uvalue

  if uvalue eq 'Done' then begin
     
     widget_control, event.top, /DESTROY
     goto, getout
     
  endif
  
  widget_control, event.top, GET_UVALUE = state, /NO_COPY
  widget_control, /HOURGLASS
  
  case uvalue of
     
     'Cal File Readmode': state.calfilereadmode = event.value        

     'Cals Button': begin

        widget_control,state.setup_base,MAP=0
        widget_control,state.ext_base,MAP=0
        widget_control,state.cal_base,MAP=1

     end

     'Construct Calibration Frames Button': xqspextool_mkcals,state

     'Displays': state.displays[event.value] = event.select
     
     'Do All Steps': begin

        xqspextool_mkprofiles,state,CANCEL=cancel
        if cancel then goto, cont

        xqspextool_storeappos,state,CANCEL=cancel
        if cancel then goto, cont

        xqspextool_storeaprad,state,CANCEL=cancel
        if cancel then goto, cont

        xqspextool_extract,state,CANCEL=cancel
        if cancel then goto, cont


     end

     'Extract': xqspextool_extract,state

     'Extraction Button': begin

        widget_control,state.cal_base,MAP=0
        widget_control,state.setup_base,MAP=0
        widget_control,state.ext_base,MAP=1

     end

     'Extraction Type': begin

        state.exttype = event.value        

        state.displays[1] = (state.exttype eq 'Fix Apertures') ? 1:0
        widget_control, state.displays_bg,SET_VALUE=state.displays
        
        widget_control, state.appos_but, $
                        SENSITIVE=(state.exttype eq 'Fix Apertures') ? 1:0

     end

     'Extraction File Readmode': state.extfilereadmode = event.value        
        
     'LXD Sky Images Button': begin
        
        path = mc_cfld(state.path_fld,7,CANCEL=cancel)
        if cancel then goto, cont

        file = dialog_pickfile(DIALOG_PARENT=state.xqspextool_base,$
                               /MUST_EXIST,PATH=path,$
                               FILTER=['*.fits','*gz','*'],/FIX_FILTER,$
                               /MULTIPLE_FILES)
        
        if file[0] ne '' then begin

           widget_control,state.skys_fld[1],$
                          SET_VALUE=strjoin(file_basename(file),',',/SINGLE)
              
        endif

     end

     'Make Spatial Profiles': begin

        state.displays[1] = 1
        widget_control, state.displays_bg,SET_VALUE=state.displays
        xqspextool_mkprofiles,state,CANCEL=cancel
        
     end

     'Path Button': begin
        
        path= dialog_pickfile(/DIRECTORY,$
                              DIALOG_PARENT=state.xqspextool_base,$
                              TITLE='Select Path',/MUST_EXIST)
        
        if path ne '' then begin
           
           path = mc_cpath(path,WIDGET_ID=state.xqspextool_base,CANCEL=cancel)
           if cancel then goto, cont

           widget_control,state.path_fld[1],SET_VALUE = path
           mc_setfocus,state.path_fld
           
        endif
        
     end

     'PS BG Sub': state.psbgsub = event.select

     'Setup Button': begin

        widget_control,state.cal_base,MAP=0
        widget_control,state.ext_base,MAP=0
        widget_control,state.setup_base,MAP=1

     end

     'Source Images Button': begin
        
        path = mc_cfld(state.path_fld,7,CANCEL=cancel)
        if cancel then goto, cont

        file = dialog_pickfile(DIALOG_PARENT=state.xqspextool_base,$
                               /MUST_EXIST,PATH=path,$
                               FILTER=['*.fits','*gz','*'],/FIX_FILTER,$
                               /MULTIPLE_FILES)
        
        if file[0] ne '' then begin

           widget_control,state.sourceimages_fld[1],$
                          SET_VALUE=strjoin(file_basename(file),',',/SINGLE)
              
        endif

     end

     'Store Ap Radii': xqspextool_storeaprad,state


     'Store Ap Positions': begin

        xqspextool_storeappos,state,CANCEL=cancel
        if cancel then goto, cont

        xmc_plotprofiles,*state.profiles,*state.orders, $
                         replicate(1,state.norders),state.slith_arc, $
                         APPOS=*state.appos,GROUP_LEADER=state.xqspextool_base
        
     end

     else:
     
  endcase
  
;  Put state variable into the user value of the top level base.
  
cont: 

  widget_control, state.xqspextool_base, SET_UVALUE=state, /NO_COPY

getout:

end
;
;******************************************************************************
;
; ----------------------------Support procedures------------------------------ 
;
;******************************************************************************
;
pro xqspextool_cleanup,base

  widget_control, base, GET_UVALUE = state, /NO_COPY
  if n_elements(state) ne 0 then begin

     ptr_free, state.flat
     ptr_free, state.edgecoeffs
     ptr_free, state.wavecal
     ptr_free, state.spatcal
     ptr_free, state.tri
     ptr_free, state.omask
     ptr_free, state.medspatcoeffs
     ptr_free, state.awave
     ptr_free, state.atrans
     ptr_free, state.orders
     ptr_free, state.profiles
     ptr_free, state.appos
     ptr_free, state.img
     ptr_free, state.var
     ptr_free, state.xranges
     
  endif
  state = 0B

end
;
;==============================================================================
;
pro xqspextool_extract,state,CANCEL=cancel
  
  cancel = 0

  tracecoeffs = fltarr(2,state.norders*state.naps)
  
  l = 0
  for i = 0,state.norders-1 do begin
     
     for j = 0,state.naps-1 do begin
             
        tracecoeffs[*,l] = [(*state.appos)[j,i],0]
        l =l + 1

     endfor

  endfor

  apradius = mc_cfld(state.aprad_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return
  tmp = float(strsplit(apradius,',',/EXTRACT))
  apradii = tmp
  
  if state.psbgsub then begin

     psbginfo = [max(tmp)*2,5]
     bgdeg = 1

  endif

  if state.exttype eq 'Find 2 Apertures' then begin
     
     apsign = [1.0,-1.0]
     
     
  endif else begin
     
     apsign = fltarr(state.naps)+1.0

  endelse

;  Create spatial map info if necessary

  idx=0
  c = *state.medspatcoeffs
  
  for i = 0,state.norders-1 do begin
     
     key = 'Order'+string(i,format='(i2.2)')
     
     if idx eq 0 then begin
        
        smapinfo = create_struct(key+'Y',c.(i*2),key+'C',c.(i*2+1))
        
     endif else begin
        
        smapinfo = create_struct(smapinfo,key+'Y',c.(i*2),key+'C',c.(i*2+1))
        
     endelse
     
     idx = idx + 1
     
  endfor
  


  s = mc_extpsspec(*state.img,*state.var,*state.wavecal,*state.edgecoeffs, $
                   tracecoeffs,state.norders,state.naps,*state.xranges,$
                   state.slith_arc,apradii,apsign,BGORDER=bgdeg, $
                   PSBGINFO=PSBGINFO,UPDATE=1,SPATCOEFF=smapinfo,$
                   BDPIXTHRESH=4,$
                   WIDGET_ID=state.xqspextool_base,CANCEL=cancel)


;  s = mc_extxsspec(*state.img,*state.var,*state.wavecal,*state.edgecoeffs, $
;                   tracecoeffs,state.norders,state.naps,*state.xranges, $
;                   state.slith_arc,apradii,/UPDATE, $
;                   WIDGET_ID=state.xqspextool_base,CANCEL=cancel)
  if cancel then return

;  Convert output structure to old array format and compute
;  approximate dispersion.

  ntags = n_tags(s)
  sizes = lonarr(2,ntags)
  
  for i = 0,ntags-1 do sizes[*,i] = size(s.(i),/DIMEN)
  
  spec = dblarr(max(sizes),3,ntags)*!values.f_nan
  disp = fltarr(ntags)
  
  for i = 0,ntags-1 do begin
     
     tmp = s.(i)
     tmp[*,1] = tmp[*,1]
     spec[0:(sizes[0,i]-1),*,i] = tmp
     coeff = poly_fit1d(findgen(sizes[0,i]),spec[*,0,i],1,/SILENT)
     disp[i] = coeff[1]
     
  endfor

  fxhmake,hdr,spec

  fxaddpar,hdr,'NAPS',fix(state.naps), ' Number of apertures'
  fxaddpar,hdr,'NORDERS',fix(state.norders), ' Number of orders'
  fxaddpar,hdr,'ORDERS',strjoin(strcompress(fix(*state.orders),/re),','), $
           ' Order numbers'

  yunits='DN / s'
  xunits= 'um'
  
  xtitle='!7k!5 (!7l!5m)'
  ytitle='f (!5DN s!u-1!N)'

  fxaddpar,hdr,'XUNITS',xunits, ' Units of the X axis'
  fxaddpar,hdr,'YUNITS',yunits, ' Units of the Y axis'
  fxaddpar,hdr,'XTITLE',xtitle, ' IDL X title'
  fxaddpar,hdr,'YTITLE',ytitle, ' IDL Y title'

  oname = mc_cfld(state.oname_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return
  
  writefits,oname+'.fits',spec,hdr

  xvspec,oname+'.fits'
  

end
;
;==============================================================================
;
pro xqspextool_mkprofiles,state,CANCEL=cancel

; Figure out the files

  path = mc_cfld(state.path_fld,7,CANCEL=cancel)
  if cancel then return

  index    = (state.extfilereadmode eq 'Index') ? 1:0
  filename = (state.extfilereadmode eq 'Filename') ? 1:0

  if index then begin
     
     prefix = mc_cfld(state.iprefix_fld,7,/EMPTY,CANCEL=cancel)
     if cancel then return
     
  endif

  img = mc_cfld(state.sourceimages_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return
  img = mc_fsextract(img,INDEX=index,FILENAME=filename,CANCEL=cancel)
  if cancel then return

  fullpaths = mc_mkfullpath(path,img,INDEX=index,FILENAME=filename, $
                            NI=state.nint,PREFIX=prefix,$
                            SUFFIX=state.suffix, $
                            WIDGET_ID=state.xqspextool_base,/EXIST, $
                            CANCEL=cancel)
  if cancel then return

;  Get mode name

  hdr = headfits(fullpaths[0],/SILENT)
  mode = strtrim(fxpar(hdr,'GRAT'),2)

;  Same mode as before?  If not, then load the flat and wavecal files

  if mode ne state.mode then begin

  widget_control,state.messwin, SET_VALUE='Loading default flat and wavecal...'

; Flat File

     mc_readflat,mode+'_Flat.fits',flat,ncols,nrows,modename,ps,slith_pix, $
                 slith_arc,slitw_pix,slitw_arc,rp,norders,orders,edgecoeffs, $
                 xranges,rms,rotation,OMASK=omask,CANCEL=cancel
     if cancel then return

     *state.flat = flat
     *state.edgecoeffs = edgecoeffs
     *state.omask = omask
     state.rotation = rotation
     state.slith_arc = slith_arc
     state.slith_pix = slith_pix
     *state.orders = orders
     state.norders = norders
     *state.xranges = xranges

;  Load the atmospheric transmission

     if rp ge 10000.0 then trp = round(rp/10000.)*10000
     if rp lt 10000.0 and rp ge 1000.0 then trp = round(rp/1000.)*1000
     if rp lt 1000.0 and rp ge 100.0 then trp = round(rp/100.)*100
     if rp lt 100.0 and rp ge 10.0 then trp = 100
     if rp eq 0 then trp=2000

     spec = readfits(filepath('atran'+strtrim(trp,2)+'.fits', $
                              ROOT_DIR=state.spextoolpath,SUBDIR='Data'), $
                     /SILENT)
     *state.awave = reform(spec[*,0])
     *state.atrans = reform(spec[*,1])

;  Wavecal File

     mc_rdwavecal2d,mode+'_Wavecal.fits',wavecal,spatcal,wctype,xo2w,wdeg, $
                    odeg,homeorder,xy2w,xy2s,wxdeg,wydeg,sxdeg,sydeg, $
                    ROTATE=rotation,CANCEL=cancel
     if cancel then return

     *state.wavecal = wavecal
     *state.spatcal = spatcal

     for i = 0,norders-1 do begin
        
        z = where(omask eq orders[i])
        
;  Triangulate the wavecal and spatcal arrays
        
        triangulate,wavecal[z],spatcal[z],tri
        
        name = 'ORD'+string(orders[i],FORMAT='(I2.2)')
        struc = (i eq 0) ? create_struct(name,tri):create_struct(struc,name,tri)
        
     endfor
     
     *state.tri = struc

     state.mode = mode
     
  endif 

;  Load data

  widget_control,state.messwin, SET_VALUE='Loading the data...'

  fullpaths = mc_reorder(fullpaths,CANCEL=cancel)
  if cancel then return

  call_procedure,state.fitsreadprogram,fullpaths,data,hdrinfo,var, $
                 KEYWORD=state.keywords,ROTATE=state.rotation, $
                 LINCORR=state.lc,/PAIR,SAT=sat,ITOT=itot, $
                 MASK=mask,CANCEl=cancel
  if cancel then return

;  Combine images if necessary

  if n_elements(fullpaths) gt 2 then begin
     
     widget_control,state.messwin, SET_VALUE='Combining the images...'

     mc_combimgs,data,5,mean,mvar,IVCUBE=var,CANCEL=cancel
     if cancel then return
     
     data = mean
     var = mvar

  endif

  *state.img = data
  *state.var = var

;  Display the image is requested

  if state.displays[0] then begin

     ximgtool,data,RANGE='98%',GROUP_LEADER=state.xqspextool_base, $
              MASK=mask,STD=state.stdimage,PLOTWINSIZE=state.plotwinsize,$
              ZUNITS='DN / s'

  endif

;  Construct spatial profiles

  widget_control,state.messwin, SET_VALUE='Constructing Spatial Profiles...'

  prof = mc_mkspatprof2d(data,*state.omask,*state.wavecal,*state.spatcal, $
                         state.slith_arc/state.slith_pix,state.ybuffer,$
                         *state.awave,*state.atrans,state.atmosthresh, $
                         SPATCOEFFS=spatcoeffs,MEDSPATCOEFFS=medspatcoeffs,$
                         WIDGET_ID=state.xqspextool_base,/UPDATE, $
                         CANCEL=cancel)  
  if cancel then return

  *state.profiles = prof
  *state.medspatcoeffs = medspatcoeffs

  if state.exttype eq 'Find 2 Apertures' then begin

     xqspextool_storeappos,state,CANCEL=cancel
     if cancel then return

     appos = *state.appos

  endif 


  if state.displays[1] then begin

     xmc_plotprofiles,*state.profiles,*state.orders, $
                      intarr(n_elements(*state.orders))+1,state.slith_arc, $
                      APPOS=appos,$
                      NOTIFY=[state.plotprofiles,state.xqspextool_base],$
                      GROUP_LEADER=state.xqspextool_base

  endif

  widget_control,state.messwin, SET_VALUE='Task complete.'

end
;
;=============================================================================
;
pro xqspextool_mkcals,state,CANCEL=cancel

  cancel = 0

  path = mc_cfld(state.path_fld,7,CANCEL=cancel)
  if cancel then return

  files = mc_cfld(state.cals_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return

  files = mc_fsextract(files,/INDEX,NFILES=nfiles,CANCEL=cancel)
  if cancel then return

  files = mc_mkfullpath(path,files,/INDEX,NI=4,PREFIX='*', $
                        SUFFIX='*.fits',WIDGET_ID=state.xqspextool_base, $
                        /EXIST,CANCEL=cancel)
  if cancel then return

  flats = mc_findspexflats(files,MASK=flatmask,CANCEL=cancel)
  if cancel then return

  arcs = mc_findspexarcs(files,MASK=arcmask,AB=pair,CANCEL=cancel)
  if cancel then return

  if pair then begin
     
     skys = mc_cfld(state.skys_fld,7,/EMPTY,CANCEL=cancel)
     if cancel then return
  
     index    = (state.calfilereadmode eq 'Index') ? 1:0
     filename = (state.calfilereadmode eq 'Filename') ? 1:0

     if index then begin
        
        prefix = mc_cfld(state.iprefix_fld,7,/EMPTY,CANCEL=cancel)
        if cancel then return
        
     endif

     skys = mc_fsextract(skys,INDEX=index,FILENAME=filename,CANCEL=cancel)
     if cancel then return

     skys = mc_mkfullpath(path,skys,INDEX=index,FILENAME=filename, $
                          NI=4,PREFIX=prefix,SUFFIX='*.fits', $
                          WIDGET_ID=state.xqspextool_base,/EXIST, $
                          CANCEL=cancel)
     if cancel then return

  endif

;  Get mode

  hdr  = headfits(files[0])
  mode = strcompress(fxpar(hdr,'GRAT'),/RE)

  mc_mkspexflat,flats,state.spextoolpath,state.packagepath,mode+'_Flat.fits', $
                NODISPLAY=~state.displays[0], $
                GROUP_LEADER=state.xqspextool_base,CANCEL=cancel
  if cancel then return

  mc_mkspexwavecal1d,arcs,mode+'_Flat.fits',state.spextoolpath, $
                     state.packagepath,SKYFILES=skys, $
                     GROUP_LEADER=state.xqspextool_base, $
                     NODISPLAY=~state.displays[0],CANCEL=cancel
  if cancel then return

  widget_control, state.messwin, SET_VALUE='Calibrations completed.'


end
;
;=============================================================================
;
pro xqspextool_querybigdog,path,prefix,CANCEL=cancel

;  path = ''
;  prefix = ''

  spawn, ['bigdogio','query.path.filename'],results,/NOSHELL
  results = strsplit(results,' ',/EXTRACT)

  if n_elements(results) eq 1 then begin

     path = ''
     prefix = ''

  endif else begin

     path = strtrim(results[1],2)+path_sep()
     prefix = strtrim(results[2],2)

  endelse

end
;
;==============================================================================
;
pro xqspextool_storeappos,state,CANCEL=cancel

  cancel = 0

;  if state.cont lt 1 then begin
;     
;     ok = dialog_message('Previous steps not complete.',/ERROR,$
;                         DIALOG_PARENT=state.xqspextool_base)
;     cancel = 1
;     
;  endif

  if state.exttype eq 'Find 2 Apertures' then begin

     state.naps = 2
     pos = rebin([4,11.8],state.naps,state.norders)

     mc_findpeaks,*state.profiles,state.naps,replicate(1,state.norders),$
                  positions,apsign,GUESS=pos,CANCEL=cancel

  endif else begin

     xmc_plotprofiles,GETAPPOS=x

     z = where(finite(x) eq 1,cnt)
     if cnt eq 0 then begin
        
        cancel = 1
        mess = 'No apertures selected.'
        ok = dialog_message(mess, /ERROR,DIALOG_PARENT=state.xqspextool_base)
        return
        
     endif
     
;  Now find the order with the most apertures 
     
     tmp = x
     tmp[z] = 1.0
     tmp = total(tmp,1,/NAN)
     state.naps = max(tmp,/NAN)
     
     pos = fltarr(state.naps,state.norders)
     for i =0,state.norders-1 do pos[*,i] = x[sort(x[0:(state.naps)-1,i]),i]
     
     
;  Now check to make sure there are an equal number of apertures in
;  the orders selected
     
     z = where(finite(pos[*,z]) eq 0,cnt)
     if cnt ne 0 then begin
        
        cancel = 1
        mess = 'Not an equal number of apertures in each order.'
        ok = dialog_message(mess, /ERROR,DIALOG_PARENT=state.xqspextool_base)
        return
        
     endif
     
     mc_findpeaks,*state.profiles,state.naps,[lindgen(state.norders)+1], $
                  positions,apsign,FIXED=pos,CANCEL=cancel
     
  endelse

  *state.appos = positions

;  state.cont = 2

end
;
;==============================================================================
;
pro xqspextool_storeaprad,state,CANCEL=cancel

  cancel = 0

  apradius = mc_cfld(state.aprad_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return
  tmp = float(strsplit(apradius,',',/EXTRACT))
  apradii = tmp
  
  if state.psbgsub then psbginfo = [max(tmp)*2,5]
  
  for i = 0,state.norders-1 do begin
     
     profile = (*state.profiles).(i)
     
     m  = mc_mkapmask1d(profile[*,0],(*state.appos)[*,i],apradii, $
                        PSBGINFO=psbginfo,WIDGET_ID=state.xqspextool_base, $
                        CANCEL=cancel)
     if cancel then return
     
     word = 'mask'+strtrim(i+1,2)
     mask = (i eq 0) ? create_struct(word,[[reform(profile[*,0])],[m]]):$
            create_struct(mask,word,[[reform(profile[*,0])],[m]]) 
     
  endfor


  if state.displays[1] then begin


     xmc_plotprofiles,*state.profiles,*state.orders, $
                      replicate(1,state.norders),state.slith_arc, $
                      APPOS=*state.appos,APRADII=apradii,$
                      MASK=mask,GROUP_LEADER=state.xqspextool_base
  

  endif

end
;
;******************************************************************************
;
; ------------------------------Main Program---------------------------------- 
;
;******************************************************************************
;
pro xqspextool,instrument,CANCEL=cancel

  cancel = 0

;  Get Spextool path

  spextoolpath = file_dirname(file_dirname(file_which('Instrument.dat')),/MARK)
 
  if ~keyword_set(INSTRUMENT) then begin
  
     readcol,filepath('Instrument.dat',ROOT_DIR=spextoolpath,SUBDIR='Data'),$
             instrument,COMMENT='#',FORMAT='A',/SILENT
     
     instrfile = strcompress(instrument[0],/RE)+'.dat'

  endif else instrfile = instrument+'.dat'
  
  notspex = (instrument[0] ne 'SpeX') ? 1:0

;  get package path.

  packagepath = file_dirname(file_dirname(file_which(instrfile)),/MARK)

;  Set the fonts 

  mc_getfonts,buttonfont,textfont,CANCEL=cancel
  if cancel then return

;  Get instrument info and default settings

  instr = mc_readinstrfile(filepath(instrfile, $
                                    ROOT_DIR=packagepath,SUBDIR='Data'), $
                           CANCEL=cancel)
  if cancel then return

;  Get bigdogXUI path if possible

  xqspextool_querybigdog,path,prefix,CANCEL=cancel

;  Build three structures which will hold important info.
  
  state = {aprad_fld:[0L,0L],$
           appos:ptr_new(2),$
           appos_but:0L,$
           atmosthresh:float(instr.atmosthresh),$
           atrans:ptr_new(2),$
           awave:ptr_new(2),$
           cal_base:0L,$
           cals_fld:[0L,0L],$
           cont:0L,$
           displays:[0,0],$
           displays_bg:0L,$
           displayprofiles:0L,$
           edgecoeffs:ptr_new(2),$
           exttype:'Find 2 Apertures',$
           ext_base:0L,$
           extfilereadmode:'Index',$
           calfilereadmode:'Index',$
           fitsreadprogram:instr.fitsreadprogram,$
           flat:ptr_new(2),$
           img:ptr_new(2),$
           instrument:instrument,$
           iprefix_fld:[0L,0L],$
           keywords:instr.keywords,$
           lc:instr.lincorrect,$
           medspatcoeffs:ptr_new(2),$
           messwin:0L,$
           mode:'',$
           naps:0L,$
           nint:instr.nint,$
           norders:0L,$
           omask:ptr_new(2),$
           oname_fld:[0L,0L],$
           orders:ptr_new(2),$
           packagepath:packagepath,$
           path_fld:[0L,0L],$
           plotprofiles:0L,$
           plotwinsize:instr.plotwinsize,$
           profiles:ptr_new(2),$
           psbgsub:1,$
           rotation:0L,$
           setup_base:0L,$
           skys_fld:[0L,0L],$
           slith_arc:0L,$
           slith_pix:0L,$
           sourceimages:'',$
           sourceimages_fld:[0L,0L],$
           spatcal:ptr_new(2),$
           spextoolpath:spextoolpath,$
           stdimage:instr.stdimage,$
           suffix:instr.suffix,$
           tri:ptr_new(2),$
           var:ptr_new(2),$
           wavecal:ptr_new(2),$
           xqspextool_base:0L,$
           xranges:ptr_new(2),$
           ybuffer:long(instr.ybuffer)}

  state.xqspextool_base = widget_base(TITLE='Quick Look Spextool for ' + $
                                      ''+instrument, $
                                      EVENT_PRO='xqspextool_event',$
                                      /COLUMN)

     mess = 'Welcome to the quicklook version of Spextool.'
     state.messwin = widget_text(state.xqspextool_base, $
                                 FONT=textfont,$
                                 VALUE=mess, $
                                 YSIZE=1)

     menu_base = widget_base(state.xqspextool_base,$
                             /ROW,$
                            /BASE_ALIGN_CENTER)

        tmp = widget_base(menu_base,$
                          /ROW,$
                          /EXCLUSIVE,$
                          /BASE_ALIGN_CENTER,$
                          /TOOLBAR)

        button = widget_button(tmp,$
                               FONT=buttonfont,$
                               VALUE='Setup',$
                               UVALUE='Setup Button')
        widget_control, button, /SET_BUTTON

        button = widget_button(tmp,$
                               FONT=buttonfont,$
                               VALUE='Cals',$
                               UVALUE='Cals Button')

        button = widget_button(tmp,$
                               FONT=buttonfont,$
                               VALUE='Extraction',$
                               UVALUE='Extraction Button')

        tmp = widget_base(menu_base,$
                          /ROW,$
                          /BASE_ALIGN_CENTER)
  
        button = widget_button(tmp,$
                               FONT=buttonfont,$
                               VALUE='Help',$
                               UVALUE='Help')

        button = widget_button(tmp,$
                               FONT=buttonfont,$
                               VALUE='Done',$
                               UVALUE='Done')
     
     box_base = widget_base(state.xqspextool_base,$
                            FRAME=5)

     state.setup_base = widget_base(box_base,$
                                    /COLUMN)
     
        prefix = 'spc'
        fld = coyote_field2(state.setup_base,$
                            LABELFONT=buttonfont,$
                            FIELDFONT=textfont,$
                            TITLE='Input Prefix:',$
                            UVALUE='Input Prefix',$
                            XSIZE=15,$
                            VALUE=prefix,$
                            TEXTID=textid)
        state.iprefix_fld = [fld,textid]

        row = widget_base(state.setup_base,$
                          /ROW,$
                          /BASE_ALIGN_CENTER)
        
           button = widget_button(row,$
                                  FONT=buttonfont,$
                                  VALUE='Raw Path',$
                                  UVALUE='Path Button')
           
           fld = coyote_field2(row,$
                               LABELFONT=buttonfont,$
                               FIELDFONT=textfont,$
                               TITLE=':',$
                               UVALUE='Path Field',$
                               XSIZE=40,$
                               VALUE=path,$
                               /CR_ONLY,$
                               TEXTID=textid)
           state.path_fld = [fld,textid]    


           
        button = widget_button(state.setup_base,$
                               FONT=buttonfont,$
                               VALUE='Import File Info from BigdogXUI',$
                               UVALUE='BigdogXUI Prefix Button')

        state.displays_bg = cw_bgroup(state.setup_base,$
                                      ['Image','Profiles'],$
                                      /ROW,$
                                      LABEL_LEFT='Display:',$
                                      /RETURN_INDEX,$
                                      UVALUE='Displays',$
                                      FONT=buttonfont,$
                                      /NONEXCLUSIVE)
        
        widget_control,state.displays_bg, SET_VALUE=[0,0]
        
     state.cal_base = widget_base(box_base,$
                                  /COLUMN)

        fld = coyote_field2(state.cal_base,$
                            LABELFONT=buttonfont,$
                            FIELDFONT=textfont,$
                            TITLE='Cal Files:',$
                            UVALUE='Cal Files',$
                            VALUE='646-653',$
                            XSIZE=15,$
                            TEXTID=textid)
        state.cals_fld = [fld,textid]

     bg = cw_bgroup(state.cal_base,$
                    ['Filename','Index'],$
                    /ROW,$
                    LABEL_LEFT='File Read Mode:',$
                    /RETURN_NAME,$
                    /NO_RELEASE,$
                    UVALUE='Cal File Readmode',$
                    FONT=buttonfont,$
                    /EXCLUSIVE)
     
     widget_control,bg, SET_VALUE=1

        row = widget_base(state.cal_base,$
                          /ROW,$
                          /BASE_ALIGN_CENTER)
        
           button = widget_button(row,$
                                  FONT=buttonfont,$
                                  VALUE='LXD Sky Images',$
                                  UVALUE='LXD Sky Images Button')

        fld = coyote_field2(row,$
                            LABELFONT=buttonfont,$
                            FIELDFONT=textfont,$
                            TITLE=':',$
                            UVALUE='Sky Files',$
                            XSIZE=40,$
                            TEXTID=textid)
        state.skys_fld = [fld,textid]

        button = widget_button(state.cal_base,$
                               FONT=buttonfont,$
                               VALUE='Construct Calibration Frames',$
                               UVALUE='Construct Calibration Frames Button')
     widget_control, state.cal_base,MAP=0


     state.ext_base = widget_base(box_base,$
                                  /COLUMN)
     widget_control, state.ext_base,MAP=0

        row = widget_base(state.ext_base,$
                          /ROW)
        
           bg = cw_bgroup(row,$
                          ['Filename','Index'],$
                          /ROW,$
                          LABEL_LEFT='File Read Mode:',$
                          /RETURN_NAME,$
                          /NO_RELEASE,$
                          UVALUE='Extraction File Readmode',$
                          FONT=buttonfont,$
                          /EXCLUSIVE)
           
           widget_control,bg, SET_VALUE=1
           
           state.plotprofiles = widget_label(row,$
                                             UVALUE='Plot Profiles',$
                                             VALUE=' ')
           
        row = widget_base(state.ext_base,$
                          /ROW,$
                          /BASE_ALIGN_CENTER)
        
           button = widget_button(row,$
                                  FONT=buttonfont,$
                                  VALUE='Source Images',$
                                  UVALUE='Source Images Button')
           
           fld = coyote_field2(row,$
                               LABELFONT=buttonfont,$
                               FIELDFONT=textfont,$
                               TITLE=':',$
                               UVALUE='Source Images',$
                               XSIZE=30,$
                               VALUE='13,14',$
                               TEXTID=textid)
           state.sourceimages_fld = [fld,textid]
           
        button = widget_button(state.ext_base,$
                               FONT=buttonfont,$
                               VALUE='Do All Steps',$
                               UVALUE='Do All Steps')

        bg = cw_bgroup(state.ext_base,$
                       ['Find 2 Apertures','Fix Apertures'],$
                       /ROW,$
                       LABEL_LEFT='Type: ',$
                       /RETURN_NAME,$
                       /NO_RELEASE,$
                       UVALUE='Extraction Type',$
                       FONT=buttonfont,$
                       /EXCLUSIVE)
        widget_control,bg, SET_VALUE=0
           


     row = widget_base(state.ext_base,$
                       /ROW,$
                       /BASE_ALIGN_CENTER)

        fld = coyote_field2(row,$
                            LABELFONT=buttonfont,$
                            FIELDFONT=textfont,$
                            TITLE='Aperture Radii:',$
                            UVALUE='Aperture Radii',$
                            VALUE='1.0,1.0',$
                            XSIZE=15,$
                            TEXTID=textid) 
        state.aprad_fld = [fld,textid]

        bg = cw_bgroup(row,$
                       FONT=buttonfont,$
                       ['PS BG Sub'],$
                       /ROW,$
                       /NONEXCLUSIVE,$
                       SET_VALUE=1,$
                       UVALUE='PS BG Sub')
        
     fld = coyote_field2(state.ext_base,$
                         LABELFONT=buttonfont,$
                         FIELDFONT=textfont,$
                         TITLE='File Name:',$
                         UVALUE='File Name',$
                         VALUE='tmpspec',$
                         XSIZE=20,$
                         TEXTID=textid) 
     state.oname_fld = [fld,textid]

     row = widget_base(state.ext_base,$
                       /ROW,$
                       /BASE_ALIGN_CENTER)

        button = widget_button(row,$
                               FONT=buttonfont,$
                               VALUE='Load Img',$
                               UVALUE='Make Spatial Profiles')
        
        state.appos_but = widget_button(row,$
                                        FONT=buttonfont,$
                                        VALUE='Store Ap Positions',$
                                        UVALUE='Store Ap Positions')
        widget_control, state.appos_but,SENSITIVE=0

        button = widget_button(row,$
                               FONT=buttonfont,$
                               VALUE='Store Ap Radii',$
                               UVALUE='Store Ap Radii')

        button = widget_button(row,$
                               FONT=buttonfont,$
                               VALUE='Extract',$
                               UVALUE='Extract')


           
              
; Get things running.  Center the widget using the Fanning routine.
            
  cgcentertlb,state.xqspextool_base
  widget_control, state.xqspextool_base, /REALIZE
 
; Start the Event Loop. This will be a non-blocking programG.

  XManager, 'xqspextool', $
            state.xqspextool_base, $
            CLEANUP='xqspextool_cleanup',$
            /NO_BLOCK
  
  widget_control, state.xqspextool_base, SET_UVALUE=state, /NO_COPY

end
