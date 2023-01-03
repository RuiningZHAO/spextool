;+
; NAME:
;     spextoolette
;
; PURPOSE:
;     To fully reduced LowRes15 observations taken under certain conditions
;
; CALLING SEQUENCE:
;     spextoolette,ifile,CANCEL=cancel
;
; INPUTS:
;     ifile - A string given fullpath to an ASCII file that gives the
;             list of input data.  Structure TBD.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem.
;
; OUTPUTS:
;     Writes normalized flats, wavecal files, extracted spectra,
;     combined spectra, and telluric corrected spectra to disk.
;
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     spextoolette_state
;
; RESTRICTIONS:
;     Only works on IRTF SpeX prism data.
;
; DEPENDENCIES:
;     Requires the Spextool package (and all packages required by Spextool).
;
; PROCEDURE:
;     NA
;
; EXAMPLES:
;    spextoolette, 'ifile.dat'
;
; MODIFICATION HISTORY:
;    2015-07-17 - Written by M. Cushing, University of Toledo
;-
;
; ==============================================================================
;
function spextoolette_combine,ifiles,sclwrange,shapecor,force,CANCEL=cancel

  print
  print, 'Combining the spectra together...'
  print

  common spextoolette_state,state,instr,bdpxmask

  files = mc_fsextract(ifiles,/INDEX,CANCEL=cancel)

  fullpaths = mc_mkfullpath(state.procpath,files,/INDEX,NI=instr.nint,$
                            PREFIX='spectra',SUFFIX=instr.suffix, $
                            /EXIST,CANCEL=cancel)
  if cancel then return, -1

  opath = state.procpath+'cspectra'+strtrim(ifiles,2)+'.fits'

;  Check for files already

  junk = mc_cfile(opath,/SILENT,CANCEL=cancel)
  
  if cancel eq 0 and force ne 1 then begin
     
     print
     print, 'File '+file_basename(opath)+' already exist.'
     print
     return, file_basename(opath)
     
  endif
  
  ;  Read the first spectrum to get initial data

  mc_readspec,fullpaths[0],first,hdr,obsmode,start,stop,norders,naps,orders,$
              xunits,yunits,slith_pix,slith_arc,slitw_pix,slitw_arc,$
              rp,airmass,xtitle,ytitle,instrument,/SILENT,CANCEL=cancel
  if cancel then return, -1
  
  nfiles = n_elements(fullpaths)
  npix   = n_elements(first[*,0,0])
  medrms = fltarr(norders,naps)+!values.f_nan
  scales = fltarr(nfiles,naps)+1
  nspec  = nfiles
     
  value   = strcompress(indgen(naps)+1, /RE)
  array   = fltarr(npix,4,nfiles)*!values.f_nan
  spcmask = intarr(nfiles,norders,naps)+1
  pmask   = intarr(npix,nfiles)+1

  scaleorder   = fltarr(naps)+!values.f_nan
  corspec      = intarr(naps)
  
;  Create data structure for a single aperture
  
  key     = 'Order'+string(00,FORMAT='(i2.2)')
  spc     = create_struct(key,array)
  pixmask = create_struct(key,pmask)
  hdrinfo = replicate(mc_gethdrinfo(hdr),nfiles)

  itot = 0

;  Load the data
  
  for i = 0, nfiles-1 do begin

     data = readfits(fullpaths[i],hdr,/SILENT)
     data = mc_caaspecflags(data,CANCEL=cancel)
     if cancel then return, -1
     
     tmp = fxpar(hdr,'ITOT')
     itot = itot+tmp
     for j = 0, norders-1 do begin

        for k = 0,naps-1 do begin
           
           z = where(finite(first[*,0,j*naps+k]) eq 1)
           
           mc_interpspec,data[z,0,j*naps+k],data[z,1,j*naps+k], $
                         first[z,0,j*naps+k],newflux,newerror, $
                         IYERROR=data[z,2,j*naps+k],LEAVENANS=1,$
                         CANCEL=cancel
           if cancel then return, -1
           
           mc_interpflagspec,data[z,0,j*naps+k],byte(data[z,3,j*naps+k]),$
                             first[z,0,j*naps+k],newflag,CANCEL=cancel
           if cancel then return, -1
           
           spc[0].(j)[z,0,i*naps+k] = first[z,0,j*naps+k]
           spc[0].(j)[z,1,i*naps+k] = newflux
           spc[0].(j)[z,2,i*naps+k] = newerror
           spc[0].(j)[z,3,i*naps+k] = float(newflag)
           
        endfor
        
     endfor
     
  endfor

;  Create filename string for output FITS header later
  
  for i = 0,nfiles-1 do begin
     
     file = strmid(fullpaths[i],strpos(fullpaths[i],'/',/REVERSE_S)+1)
     sfile = (i eq 0) ? file:[sfile,file]
     
  endfor

;  Scale spectra

  srange = float(strsplit(sclwrange,'-',/EXTRACT))
  
  z = where(spc[0].(0)[*,0,0] gt srange[0] and spc[0].(0)[*,0,0] lt srange[1])

  scales = mc_getspecscale(reform(spc[0].(0)[z,1,*]),CANCEL=cancel)
  if cancel then return, -1

  for i = 0,nfiles-1 do begin

     spc[0].(0)[*,1,i]  = spc[0].(0)[*,1,i]*scales[i]
     spc[0].(0)[*,2,i]  = spc[0].(0)[*,2,i]*scales[i]

  endfor

;  Get median RMS across each order
     
  mc_meancomb,reform(spc[0].(0)[*,1,*]),mean,var,/RMS
  medrms = median(sqrt(var),/EVEN)
  
  
;  Perform shape correction if requested

  if shapecor then begin
     
     screen = get_screen_size()
     window, /FREE,XSIZE=screen[0]*0.75,YSIZE=screen[1]*0.5
     
     mc_medcomb,reform(spc[0].(0)[*,1,*]),med
     yrange = [0.4*min(med,/NAN,MAX=max),1.3*max]
     
     pos = mc_gridplotpos([0.1,0.1,0.95,0.95],[2,1],0.05)
     
     plot,reform(spc[0].(0)[*,0,0]),reform(spc[0].(0)[*,1,0]),$
          /XSTY,XTITLE='Wavelength (um)',$
          /YSTY,YRANGE=yrange,YTITLE='Flux (DN / s)',$
          /NODATA,POSITION=pos[*,0],CHARSIZE=1.3,TITLE='Raw Standards'
     
     for i = 0,nfiles-1 do begin
        
        oplot,reform(spc[0].(0)[*,0,i]),reform(spc[0].(0)[*,1,i]),COLOR=2+i
        
     endfor
     
     
     print
     print, 'Performing shape correction...'
     print
     spec = reform(spc[0].(0)[*,1,*])
     err = reform(spc[0].(0)[*,2,*])
     
     nstack = mc_speccor(spec,4,IERRSTACK=err,OERRSTACK=nerrstack, $
                         CORRECTIONS=corrections,CANCEL=cancel)
     if cancel then return, -1
     
     spc[0].(0)[*,1,*] = nstack
     spc[0].(0)[*,2,*] = nerrstack
     
     
     plot,reform(spc[0].(0)[*,0,0]),reform(spc[0].(0)[*,1,0]),$
          /XSTY,XTITLE='Wavelength (um)',$
          /YSTY,YRANGE=yrange,YTITLE='Flux (DN / s)',$
          /NODATA,POSITION=pos[*,1],CHARSIZE=1.3,/NOERASE,$
          TITLE='Shape Corrected Standards'
     
     for i = 0,nfiles-1 do begin
        
        oplot,reform(spc[0].(0)[*,0,i]),reform(spc[0].(0)[*,1,i]),COLOR=2+i
        
     endfor
     
     re = ' '
     read, re
     
     wdelete,!D.window
     
  endif
  
;  Combine spectra together
  
  spec = fltarr(npix,4,norders)            
  
  mc_meancomb,reform(spc[0].(0)[*,1,*]),mean,mvar, $
              DATAVAR=reform(spc[0].(0)[*,2,*]^2),MASK=mask, $
              ROBUST=8,OGOODBAD=ogoodbad, $
              /SILENT,CANCEL=cancel
  if cancel then return, -1
  
  
  spec[*,0,0] = reform(spc[0].(0)[*,0,0])
  spec[*,1,0] = mean
  spec[*,2,0] = sqrt(mvar)
  
;  Combine the flags

  tmp = byte(spc[0].(0)[*,3,*])*ogoodbad
  result = mc_combflagstack(tmp,CANCEL=cancel)
  if cancel then return, -1
  spec[*,3,0] = result
  
;  Write the file to disk

  ;  Make new hdr.
     
  avehdr = mc_avehdrs(hdrinfo,TIME_OBS=instr.time,POSANGLE=instr.posangle, $
                      HA=instr.ha,AIRMASS=instr.airmass,CANCEL=cancel)
  history = ''    
  
  avehdr.vals.NAPS = naps
  
;  Create history
  
  values = ['Robust Weighted Mean','Robust Mean (RMS)', $
            'Robust Mean (Std Error)','Weighted Mean',$
            'Mean (RMS)','Mean (Std Error)','Median (MAD)', $
            'Median (Median Error)','Sum']
  
  
  history=history+'  The files '+strjoin((sfile),', ')+' were combined.  '
  
  history = history+'  Aperture '+string(1,FORMAT='(i2.2)')+' Info: '
  
  history = history+'The scale factors were determined using order '+$
            string(1,FORMAT='(i2.2)')+$
            '.  The scale factors are '+$
            strjoin(strtrim(scales,2),', ')+ $
            '.  The median RMS' + ' deviation in each order is '+ $
            strjoin(strtrim(medrms,2),', ')+'.  '
  
  
  if shapecor then $
     history = history+'  The spectral shapes have been corrected.  '
  
  history = history+'The spectra were combined using a '+ $
            values[0]+'.  '
  
  history = history+'The robust threshold was '+strtrim(8,2)+'.'
  
  fxhmake,newhdr,spec
  
  ntags    = n_tags(avehdr.vals)
  names    = tag_names(avehdr.vals)
  
  for i = 0, ntags - 1 do begin
     
     if names[i] eq 'HISTORY' then begin
        
        newhdr = mc_addhistlabel(newhdr,'Xspextool History',CANCEL=cancel)
        if cancel then return, -1

        for j = 0, ceil(float(strlen(avehdr.vals.(i)))/70.)-1 do begin
           
           hist = strmid(avehdr.vals.(i),70*j,70)
           fxaddpar,newhdr,'HISTORY',hist
           
        endfor
        
     endif else fxaddpar,newhdr,names[i],avehdr.vals.(i),avehdr.coms.(i)
     
  endfor

  newhdr = mc_addhistlabel(newhdr,'Xcombspec History',CANCEL=cancel)
  if cancel then return, -1
  
  history = mc_splittext(history,70,CANCEL=cancel)
  if cancel then return, -1
  sxaddhist,history,newhdr
  
  fxaddpar,newhdr,'ITOT',itot,' Total integration time (sec)'
  fxaddpar,newhdr,instr.irafname,file_basename(opath)
  
  
  writefits,opath,spec,newhdr
  
  xvspec,opath,/PLOTLINMAX   

  return, file_basename(opath)
  

end
;
; ==============================================================================
;
pro spextoolette_extract,ifiles,iprefix,flatname,wavecalname,aps,force, $
                         CANCEL=cancel

  common spextoolette_state

  print
  print, 'Extracting the spectra...'
  print
  
  files = mc_fsextract(ifiles,/INDEX,CANCEL=cancel)

  if cancel then return
  
  if n_elements(files) mod 2 ne 0 then begin
     
     print
     print, 'Must be an even number of images for pair subtraction.'
     print
     cancel = 1
     return
     
  endif
  
;  Get loop size
  
  loopsize = (n_elements(files)/2.)
  
  for i = 0, loopsize-1 do begin
     
; ============================= Load Files ================================
        
     workfiles = files[i*2:i*2+1]
     
;  Check for duplicate files
        
     result = mc_checkdups(state.rawpath,workfiles,instr.nint,CANCEL=cancel)
     if cancel then return

;  Generate the file names
        
     fullpaths = mc_mkfullpath(state.rawpath,workfiles,/INDEX,NI=instr.nint,$
                               PREFIX=strtrim(iprefix,2), $
                               SUFFIX=instr.suffix, $
                               /EXIST,CANCEL=cancel)
     if cancel then return

     opaths = mc_mkfullpath(state.procpath,workfiles,/INDEX,NI=instr.nint, $
                            PREFIX='spectra',CANCEL=cancel)
     if cancel then return

;  Check for files already

     junka = mc_cfile(opaths[0]+'.fits',/SILENT,CANCEL=cancela)
     junkb = mc_cfile(opaths[1]+'.fits',/SILENT,CANCEL=cancelb)

     if cancela eq 0 or cancelb eq 0 and force ne 1 then begin

        print
        print, 'Files '+file_basename(opaths[0])+'.fits and '+ $
               file_basename(opaths[1])+'.fits already exist.'
        print
        continue
        
     endif
     
;  Load the flat field and wavecal file
     
     mc_readflat,flatname,flat,ncols,nrows,modename,ps,slith_pix,slith_arc,$ 
                 slitw_pix,slitw_arc,rp,norders,orders,edgecoeffs,xranges, $
                 rms,rotation,OMASK=omask,CANCEL=cancel
     if cancel then return
     
     mc_rdwavecal2d,wavecalname,wavecal,spatcal,wctype,xo2w,wdeg,odeg, $
                    homeorder,xy2w,xy2s,wxdeg,wydeg,sxdeg,sydeg,disp, $
                    ROTATE=rotation,CANCEL=cancel
     if cancel then return
     
     wavecal = wavecal
     spatcal = spatcal
     disp    = disp
     wctype  = wctype
     
;  Load the atmospheric transmission
     
     if rp ge 10000.0 then trp = round(rp/10000.)*10000
     if rp lt 10000.0 and rp ge 1000.0 then trp = round(rp/1000.)*1000
     if rp lt 1000.0 and rp ge 100.0 then trp = round(rp/100.)*100
     if rp lt 100.0 and rp ge 10.0 then trp = 100
     if rp eq 0 then trp=2000
     
     spec = readfits(filepath('atran'+strtrim(trp,2)+'.fits', $
                              ROOT_DIR=state.spextoolpath,SUBDIR='data'), $
                     /SILENT)
     awave = reform(spec[*,0])
     atrans = reform(spec[*,1])
     
;  Trigrid the wavelength and spatial arrays
     
     for j = 0,norders-1 do begin
        
        z = where(omask eq orders[j])
        
;  Triangulate the wavecal and spatcal arrays
        
        triangulate,double(wavecal[z]),double(spatcal[z]),tri
        
        name = 'ORD'+string(orders[j],FORMAT='(I2.2)')
        struc = (j eq 0) ? $
                create_struct(name,tri):create_struct(struc,name,tri)
        
     endfor
     
     tri = struc

     call_procedure,state.readfitsprog,fullpaths,data,hdrinfo,var, $
                    KEYWORD=instr.keywords,ROTATE=rotation, $
                    /LINCORR,/PAIR,$
                    BITINFO={lincormax:instr.lincormax,lincormaxbit:0},$
                    BITMASK=bitmask,ITOT=itot,_EXTRA=extra,CANCEl=cancel
     if cancel then return
     
     
     data = temporary(data)/flat
     var  = temporary(var)/(flat)^2
     
     delvarx, flat  
     workimage  = data
     delvarx, data
     
     varimage   = var
     delvarx, var
     
     bitmask = bitmask
     
     
     hdrinfo    = hdrinfo
     itot       = itot
     
;  Determine Julian date of the observation
     
     date = strsplit(hdrinfo[0].vals.DATE_OBS,'-',/EXTRACT)
     juldate = julday(date[1],date[2],date[0])
     
     
;  Display image
     
     spextoolette_ximgtool,spatcal,wavecal,workimage,varimage,bitmask, $
                           stdimage,plotwinsize,/FORCE,WID=wid,CANCEL=cancel
     
; ============================= Make Profiles ================================
     
;  Construct the Profiles
     
     prof = mc_mkspatprof2d(workimage,omask,wavecal,spatcal, $
                            slith_arc/slith_pix,fix(instr.ybuffer),awave, $
                            atrans,float(instr.atmosthresh), $
                            SPATCOEFFS=spatcoeffs,MEDSPATCOEFFS=medspatcoeffs,$
                            /UPDATE,CANCEL=cancel)
     if cancel then return
     
     profiles = prof
     profcoeffs = spatcoeffs
     aveprofcoeffs = medspatcoeffs
     
;  Display the profiles
     
     xmc_plotprofiles,profiles,orders,intarr(norders)+1,slith_arc,POSITION=[0,0]
     
; =========================== Find Apertures ==============================
     
     mc_findpeaks,profiles,2,[1],positions,apsign,/AUTO,CANCEL=cancel
     if cancel then return
     
     appos = positions
     print, ' '
     print,'Aperture Positions:'
     print, ' '
     
     print,'Order '+string(1,FORMAT='(i2.2)')+': ',mc_sigfig(positions,5)
     
;  Plot on xplotprofiles
     
     xmc_plotprofiles,profiles,orders,[1],slith_arc,APPOS=positions,$
                      POSITION=[0,0]

; =========================== Trace Spectra ==============================

     spextoolette_ximgtool,spatcal,wavecal,workimage,varimage,bitmask, $
                           stdimage,plotwinsize,WID=wid,CANCEL=cancel
     
     tracecoeffs = mc_tracespec2d(workimage,wavecal,spatcal, $
                                  edgecoeffs,xranges,appos, $
                                  instr.tracestepsize,instr.tracesumap, $
                                  instr.tracewinthresh,instr.tracesigthresh, $
                                  instr.tracedeg,WID=wid,OPLOT=oplot, $
                                  CANCEL=cancel)
     if cancel then return

;  Generate ximgtool data

     a = obj_new('mcoplot')
     a->set,oplot[*,0],oplot[*,1],PSYM=4,COLOR=6
     obj = a

     z = where(oplot[*,2] eq 0,cnt)
     if cnt ne 0 then begin

        b = obj_new('mcoplot')
        b = obj_new('mcoplot')
        b->set,oplot[z,0],oplot[z,1],PSYM=4,COLOR=4
        obj = [obj,b]

     endif

     mc_tracetoap2d,omask,wavecal,spatcal,tri,tracecoeffs,2,orders, $
                    [1],appos,struc,CANCEL=cancel
     if cancel then return

;  Generate ximgtool data

     c = obj_new('mcplotap')
     c->set,struc,7
     
     oplot = [obj,c]

;  Display results on ximgtool

     spextoolette_ximgtool,spatcal,wavecal,workimage,varimage,bitmask, $
                           stdimage,plotwinsize,OPLOT=oplot,CANCEL=cancel


; ========================= Define Apertures  ============================

     tmp = strsplit(aps,',',/EXTRACT)

     optextract = (n_elements(tmp) eq 5) ? 1:0

     if optextract then begin

        psfradius = float(tmp[0])
        apradius = float(tmp[1])
        bgstart = float(tmp[2])
        bgwidth = float(tmp[3])
        bgdeg = float(tmp[4])
        
     endif else begin

        apradius = float(tmp[0])
        bgstart = float(tmp[1])
        bgwidth = float(tmp[2])
        bgdeg = float(tmp[3])

     endelse

     sapradius = fltarr(2,1)
     apsigndat = fltarr(2)
     
     psbginfo = [bgstart,bgwidth]
  
     profile = profiles.(0)

     tmp = [appos+apradius,appos-apradius]
     z = where(tmp lt 0 or tmp gt slith_arc,cnt)
     if cnt ne 0 then begin
        
        message=[['Some of the extraction apertures extend beyond the slit.'],$
                 ['Please lower the aperture radius.']]
        junk = dialog_message(message,/INFORMATION)
        cancel = 1
        return
        
     endif
     m  = mc_mkapmask1d(profile[*,0],appos,replicate(apradius,2), $
                        PSBGINFO=psbginfo,CANCEL=cancel)
     if cancel then return
     
     word = 'mask'+strtrim(1,2)
     mask = create_struct(word,[[reform(profile[*,0])],[m]])

;  Determine the aperture signs

        for j = 0,1 do begin

           med = median(profile[*,1])
           tabinv,profile[*,0],appos[j],idx
           apsigndat[j] = (profile[idx,1] gt med) ? 1:-1
           
        endfor

     sapradius = replicate(apradius,2)
  
     zpos = where(apsigndat gt 0,cntpos)
     zneg = where(apsigndat lt 0,cntneg)
     
     if cntpos ne 0 then apsigndat[zpos] = 1
     if cntneg ne 0 then apsigndat[zneg] = -1 
     
     apsign = fix(apsigndat)
     
     string = strarr(2)
     zz = where(apsigndat eq -1,count)
     if count ne 0 then string[zz] = '-'
     zz = where(apsigndat eq 1,count)
     if count ne 0 then string[zz] = '+'
     
     print, 'Aperture signs: '+strjoin(string,',')
     
;  Plot on ximgtool
     
     low_tracecoeffs  = tracecoeffs
     Low_tracecoeffs[0,*]  = low_tracecoeffs[0,*]-total(apradius)
     
     mc_tracetoap2d,omask,wavecal,spatcal,tri,low_tracecoeffs, $
                    2,orders,[1],appos,struc,CANCEL=cancel
     if cancel then return
     
     c = obj_new('mcplotap')
     c->set,struc,3
     
     
     high_tracecoeffs = tracecoeffs
     high_tracecoeffs[0,*] = high_tracecoeffs[0,*]+total(apradius)
     
     mc_tracetoap2d,omask,wavecal,spatcal,tri,high_tracecoeffs,2,orders, $
                    [1],appos,struc,CANCEL=cancel
     if cancel then return
     
     d = obj_new('mcplotap')
     d->set,struc,3
     
     oplot = [c,d]
     
     spextoolette_ximgtool,spatcal,wavecal,workimage,varimage,bitmask, $
                           instr.stdimage,instr.plotwinsize,OPLOT=oplot, $
                           CANCEL=cancel
     if cancel then return
     
;  Plot in xmc_plotprofiles  
  
     xmc_plotprofiles,profiles,orders,[1],slith_arc,APPOS=positions, $
                      APRADII=replicate(apradius,2), $
                      MASK=mask,PSFAP=psfradius,POSITION=[0,0]

;  Extract the spectra
     
     if optextract then begin
        
        idx=0
;     c = (state.r.medprofile eq 1) ? *state.r.aveprofcoeffs:*state.r.profcoeffs
        c = profcoeffs
        
        key = 'Order'+string(1,format='(i2.2)')
        
        smapinfo = create_struct(key+'Y',c.(0),key+'C',c.(1))
        
     endif
     
     z = where([1] eq 1,norders)
     orders = orders[z]
     
;  Do the 1D SpeX-type extraction
     
     apradii = replicate(apradius,2,norders)
     
     bdpxthresh = instr.bdpxthresh

     s = mc_extpsspec(workimage,varimage,wavecal,edgecoeffs,tracecoeffs,$
                      norders,2,xranges,slith_arc,replicate(apradius,2), $
                      apsign,SPATCOEFFS=smapinfo,PSBGINFO=psbginfo, $
                      BGORDER=bgdeg,PSFWIDTH=psfradius, $
                      BDPXMASK=rotate(bdpxmask,rotation),BITMASK=bitmask,$
                      BDPIXTHRESH=bdpxthresh,/UPDATE, $
                      CANCEL=cancel)
     if cancel then return
     
;  Convert output structure to old array format and compute
;  approximate dispersion.
     
     ntags = n_tags(s)
     sizes = lonarr(2,ntags)
     
     for j = 0,ntags-1 do sizes[*,j] = size(s.(j),/DIMEN)
     
     spectra = dblarr(max(sizes),4,ntags)*!values.f_nan
     
     for j = 0,ntags-1 do begin
        
        spectra[0:(sizes[0,j]-1),*,j] = s.(j)
        
     endfor
     
;  Write file out
     
     wavetype = 'Vacuum'
     
     yunits='DN / s'
     xunits= 'um'
     
     xtitle='!7k!5 (!7l!5m)'
     ytitle='f (!5DN s!u-1!N)'
     
     psbginfo = [bgstart,bgwidth,bgdeg]
     
;  Do any positive apertures.       
     
     pos = where(apsign eq 1,pos_naps)
     if pos_naps ne 0 then begin
        
        hdrinfoa            = hdrinfo[0] 
        junk                = tag_exist(hdrinfoa.vals,instr.irafname,INDEX=idx)
        aimage              = hdrinfoa.vals.(idx)
        sky                 = hdrinfo[1].vals.(idx)
        hdrinfoa.vals.(idx) = file_basename(opaths[0])+'.fits'
        
        
        apsign  = replicas(apsign,norders)
        z       = where(apsign eq 1)
        
        mc_writespec,spectra[*,*,z],opaths[0]+'.fits',aimage,itot[0],sky, $
                     file_basename(flatname),pos_naps,orders,hdrinfoa, $
                     appos[pos,*],apradius,modename,slith_pix, $
                     slith_arc,slitw_pix,slitw_arc,rp,xunits,yunits,xtitle, $
                     ytitle,instr.instr,state.version,PSFRADIUS=psfradius, $
                     PSBGINFO=psbginfo,WAVECAL=file_basename(wavecalname), $
                     WAVETYPE=wavetype,DISP=disp,/LINEARITY, $
                     LINCORMAX=instr.lincormax,CANCEL=cancel
        if cancel then return
        
     endif
     
;  Do any negative apertures.       
     
     neg = where(apsign eq -1,neg_naps)
     if neg_naps ne 0 then begin
        
        hdrinfob            = hdrinfo[1] 
        junk                = tag_exist(hdrinfob.vals,instr.irafname,INDEX=idx)
        aimage              = hdrinfob.vals.(idx)
        sky                 = hdrinfo[0].vals.(idx)
        hdrinfob.vals.(idx) = file_basename(opaths[1])+'.fits'
        
        
        apsign  = replicas(apsign,norders)
        z       = where(apsign eq -1)
        
        mc_writespec,spectra[*,*,z],opaths[1]+'.fits',aimage,itot[0],sky, $
                     file_basename(flatname),pos_naps,orders,hdrinfoa, $
                     appos[pos,*],apradius,modename,slith_pix, $
                     slith_arc,slitw_pix,slitw_arc,rp,xunits,yunits,xtitle, $
                     ytitle,instr.instr,state.version,PSFRADIUS=psfradius, $
                     PSBGINFO=psbginfo,WAVECAL=file_basename(wavecalname), $
                     WAVETYPE=wavetype,DISP=disp,/LINEARITY, $
                     LINCORMAX=instr.lincormax,CANCEL=cancel
        if cancel then return
        
     endif
     
     xvspecfile = (pos_naps ne 0) ? opaths[0]:opaths[1]
     
     xvspec,xvspecfile+'.fits',POSITION=[1,0.5],/PLOTLINMAX
     
  endfor

end
;
; ==============================================================================
;
pro spextoolette_mkcals,cals,force,flatname,wavecalname,CANCEL=cancel

  cancel = 0

  common spextoolette_state

  print
  print,'Inspecting Cal Set ...'
  print
  
;  Creat the calibration frames

  flatinfo = strarr(2)
  arcinfo  = strarr(5)
  
  files = mc_fsextract(cals,/INDEX,CANCEL=cancel)
  if cancel then return
  
  result = mc_checkdups(state.rawpath,files,instr.nint,CANCEL=cancel)
  if cancel then return
  
  fullpaths = mc_mkfullpath(state.rawpath,files,/INDEX,NI=instr.nint, $
                            PREFIX='*',SUFFIX=instr.suffix, $
                            /EXIST,CANCEL=cancel)
  if cancel then begin
     
     print
     print,'Cal Set '+strcompress(i+1,/RE)+' does not exist.'
     print
     return
        
  endif
  
  if state.instrument eq 'spex' then begin
     
     flats = mc_findspexflats(fullpaths,MASK=flatmask,CANCEL=cancel)
     
  endif else begin
     
     flats = mc_finduspexflats(fullpaths,MASK=flatmask,CANCEL=cancel)
     
  endelse
  
  z = where(flatmask eq 1,count)
  if count eq 0 then begin
     
     print
     print,'No flats in Cal Set '+strcompress(i+1,/RE)+'.'
     print
     return
     
  endif
  
  flatoname = 'flat'+strcompress(files[z[0]],/RE)+'-'+$
              strcompress(files[z[count-1]],/RE)
  flatinfo[0] = strjoin(flats,',')
  flatinfo[1] = flatoname
  
  if state.instrument eq 'spex' then begin
     
     arcs = mc_findspexarcs(fullpaths,MASK=arcmask,CANCEL=cancel)
     
  endif else begin
     
     arcs = mc_finduspexarcs(fullpaths,MASK=arcmask,CANCEL=cancel)
     
  endelse
  
  z = where(arcmask eq 1,count)
  if count eq 0 then begin
     
     print
     print,'No arcs in Cal Set '+strcompress(i+1,/RE)+'.'
     print
     return
     
  endif
  
  arcinfo[0] = strjoin(arcs,',')
  arcinfo[1] = flatoname+'.fits'
  arcinfo[2] = string(0)
  
  hdr     = headfits(arcs[0])
  obsmode = strcompress(fxpar(hdr,'GRAT'),/RE)
  z = where(arcmask eq 1,count)
  
  waveoname = 'wavecal'+strcompress(files[z[0]],/RE)+'-'+$
              strcompress(files[z[count-1]],/RE)       
  arcinfo[4] = waveoname
  
;  ===================== Create the flat field ==============================

  files = strsplit(flatinfo[0],',',/EXTRACT)
  flatoname = (reform(flatinfo[1]))[0]
  flatname = filepath(flatoname+'.fits',ROOT=state.calpath)
  
  tmp = mc_cfile(flatname,/SILENT,CANCEL=cancel)
  if cancel eq 0 and force eq 0 then begin

     print
     print, 'The flat file '+file_basename(flatname)+' already exists.'
     print
     goto, startwavecal

  endif else begin

     print
     print,'Creating the flat field ...'
     print
     
  endelse
     
;  Get modeinfo
  
  hdr  = headfits(files[0])
  mode = strcompress(fxpar(hdr,'GRAT'),/RE)
  dit = fxpar(hdr,'DIT')
  
  if size(dit,/TYPE) ne 7 then mode = mode+'_'+string(dit,FORMAT='(F3.1)')
  
  modefile = filepath(mode+'.dat',ROOT=state.packagepath,SUB='data')
  modeinfo = mc_readmodefile(modefile,CANCEL=cancel)
  if cancel then return
  
;  Compute slitw and resolving power
  
  slitw_arc = float( strmid(strtrim(fxpar(hdr,'SLIT'),2),0,3) )
  slitw_pix = slitw_arc / modeinfo.ps
  
  resolvingpower = modeinfo.rpppix/slitw_pix
  
;  Load the images
  
  call_procedure,state.readfitsprog,files,data,hdrinfo,var, $
                 KEYWORDS=instr.keywords,$
                 BITINFO={lincormax:instr.lincormax,lincormaxbit:0},$
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
  
  ximgtool, mean, SCALE='Hist Eq',WID=wid,$
            BUFFER=1,STDIMAGE=instr.stdimage, $
            BITMASK={bitmask:bitmask,plot1:[0,2]}, $
            PLOTWINSIZE=instr.plotwinsize,PANNER=0,MAG=0,/ZTOFIT, $
            POSITION=[1,0]
  
;  Locate the orders
  
  mc_findorders,mean,modeinfo.guesspos,modeinfo.findrange,modeinfo.step, $
                modeinfo.slith_pix_range,modeinfo.edgedeg,$
                modeinfo.norm_ybuffer,modeinfo.flatfrac,modeinfo.comwin, $
                edgecoeffs,xranges,WID=wid,CANCEL=cancel
  if cancel then return
  
;  Normalize the flat
  
  mean = mc_normspecflat(mean,edgecoeffs,xranges,modeinfo.slith_arc, $
                         modeinfo.norm_nxg,modeinfo.norm_nyg, $
                         modeinfo.oversamp,modeinfo.norm_ybuffer,$
                         RMS=rms,IVAR=var,OVAR=ovar, $
                         /UPDATE,MODEL=model,CANCEL=cancel)
  if cancel then return    
  
;  Write the results to disk
  
  junk = tag_exist(hdrinfo[*].vals,instr.irafname,INDEX=idx)
  
  history='This flat was created by scaling the files '+$
          strjoin(hdrinfo[*].vals.(idx), ', ')+ $
          ' to a common median flux value and then median combining ' + $
          'the scaled images.  The variance is given by (MAD^2/nimages) ' + $
          'where MAD is the median absolute deviation and nimages is the ' + $
          'number of input images.  The zeroth bit of pixels generated ' + $
          'from data with values greater than LINCORMX are set.'
  
  newhdr = hdrinfo[0]
  newhdr.vals.(idx) = file_basename(flatname)

  mc_writeflat,mc_unrotate(mean,modeinfo.rotation),$
               mc_unrotate(ovar,modeinfo.rotation),$
               mc_unrotate(byte(bitmask),modeinfo.rotation),$
               newhdr,modeinfo.rotation,fix(modeinfo.orders),edgecoeffs, $
               xranges,modeinfo.ps,modeinfo.slith_pix,modeinfo.slith_arc, $
               slitw_pix,slitw_arc,modeinfo.modename,rms,resolvingpower, $
               state.version,flatname,LINCORMAX=instr.lincormax, $
               HISTORY=history,CANCEL=cancel
  if cancel then return
  
  
;  Display if requested.
  
  mc_readflat,flatname,image,ncols,nrows,modename,ps,slith_pix,slith_arc,$
              slitw_pix,slitw_arc,rp,norders,orders,edgecoeffs,xranges, $
              rms,rotation,edgedeg,FLAGS=flags,OMASK=omask,CANCEL=cancel
  
  a = obj_new('mcoplotedge')
  a->set,xranges,edgecoeffs
  
  ximgtool,flatname,EXT=1,ZRANGE=[0.9,1.1], $
           BUFFER=1,ROTATION=modeinfo.rotation,OPLOT=a,POSITION=[1,0], $
           STDIMAGE=instr.stdimage,$
           BITMASK={bitmask:flags*(omask gt 0),plot1:[0,2]}, $
           PLOTWINSIZE=instr.plotwinsize
  
;  ======================= Create the wavecal ==============================

  startwavecal:

  armc_cfiles = strsplit(arcinfo[0],',',/EXTRACT)
  flatname = (state.calpath+arcinfo[1])[0]
  pairarc = fix(arcinfo[2])
  wavecalname = (state.calpath+(reform(arcinfo[4]))[0]+'.fits')[0]
  tmp = mc_cfile(wavecalname,/SILENT,CANCEL=cancel)
  if cancel eq 0 and force eq 0 then begin

     print
     print, 'The wavecal file '+file_basename(wavecalname)+' already exists.'
     print
     return

  endif else begin

     print
     print,'Creating the wavecal ...'
     print
    
  endelse

;  Get modeinfo
  
  hdr  = headfits(armc_cfiles[0])
  mode = strcompress(fxpar(hdr,'GRAT'),/RE)
  modefile = filepath(mode+'.dat',ROOT=state.packagepath,SUB='data')
  modeinfo = mc_readmodefile(modefile,CANCEL=cancel)
  if cancel then return

;  Read flat field

  mc_readflat,flatname,flat,ncols,nrows,obsmode,ps,slith_pix,slith_arc, $
              slitw_pix,slitw_arc,rp,norders,orders,edgecoeffs,xranges,rms, $
              rotation,edgedeg,OMASK=omask,FLAGS=flatflags,CANCEL=cancel
  if cancel then return
  
;  Load images in
  
  call_procedure,state.readfitsprog,armc_cfiles,data,hdrinfo,var, $
                 KEYWORDS=instr.keywords,$
                 BITINFO={lincormax:instr.lincormax,lincormaxbit:0},$
                 BITMASK=arcbitmask,ROTATE=rotation,PAIR=pair,NIMAGES=nimages, $
                 CANCEL=cancel
  if cancel then return
  
;  Combine images if necessary
  
  if nimages ne 1 then begin
     
     mc_meancomb,data,arc,arcvar,DATAVAR=datavar,CANCEL=cancel
     if cancel then return
     
     arcbitmask = mc_combflagstack(arcbitmask,CANCEL=cancel)
     if cancel then return
     
  endif else begin
     
     arc    = reform(data)
     arcvar = reform(var)
     arcbitmask = arcbitmask
     
  endelse
  
  arc = temporary(arc)/flat
  arcvar = temporary(arcvar)/flat^2
  
  skyhistory = ''
  mask = arcbitmask
  
;  Create fake wavecal arrays to do a simple 1D sum extraction on the arc
  
  fake = mc_simwavecal2d(ncols,nrows,edgecoeffs,xranges,slith_arc, $
                         CANCEL=cancel)
  if cancel then return
  
  wavecal = reform(fake[*,*,0])
  spatcal = reform(fake[*,*,1])
  
;  Display arc

  ximgtool,arc,STDIMAGE=instr.stdimage,PLOTWINSIZE=instr.plotwinsize, $
           BUFFER=1,POSITION=[1,0],BITMASK={mask:mask,plot1:[0,2]}

;  Get Line Cal file
  
  file = filepath(obsmode+'_LineCal.fits',ROOT=state.packagepath,SUB='data')
  calinfo = mc_readlinecalfile(file,CANCEL=cancel)
  if cancel then return
  
;  Get setup for the cross correlation 
  
  wanchor = calinfo.xcorspec[*,0]
  fanchor = calinfo.xcorspec[*,1]
  
;  Extract the arc down the middle of the slit in the anchororder
  
  arcspec = mc_sumextspec1d(arc,arcvar,omask,orders,wavecal,spatcal, $
                            [1],APRANGES=rebin([6,8],2,norders), $
                            CANCEL=cancel)
  if cancel then return
  
  z = where(orders eq calinfo.xcororder)
  
  warc = (arcspec.(z))[*,0]
  farc = (arcspec.(z))[*,1]
  
;  Do the cross correlation
  
  linterp,warc,farc,wanchor,ifarc,MISSING=0
  
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
  
  print
  print, 'Cross-correlation offset = '+strtrim(offset,2)+' pixels.'
  print
  
;  Now compute the wavelength solution
  
  homeorder = calinfo.homeorder
  dispdeg = calinfo.dispdeg
  ordrdeg = calinfo.ordrdeg
  p2wcoeffs = calinfo.p2wcoeffs
  
;  Now create wavelength calibration image
  
  wave = fltarr(ncols,nrows,/NOZERO)*!values.f_nan
  spat = fltarr(ncols,nrows,/NOZERO)*!values.f_nan
  
  disp = fltarr(norders)

  for j = 0, norders-1 do begin
     
     start = xranges[0,j]
     stop  = xranges[1,j]
     x     = findgen(stop-start+1)+start
     y     = findgen(nrows)
     
     result = poly(x-offset,p2wcoeffs)
     
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

  mwrfits,input,wavecalname,nhdr,/CREATE
  sxaddpar,nhdr,file_basename(wavecalname),' Filename'
  sxaddpar,nhdr,'WCTYPE','1D',' Wavelength calibration type'
  
  for j = 0, norders-1 do begin
     
     name = 'DISPO'+string(orders[j],format='(i2.2)')
     comment = ' Dispersion (microns pixel-1) for order '+ $
               string(orders[j],FORMAT='(i2.2)')
     sxaddpar,nhdr,name,disp[j],comment
     
  endfor
  
  mwrfits,input,wavecalname,nhdr,/CREATE
  
;  Write wavelength calibration extension
  
  writefits,wavecalname,mc_unrotate(wave,rotation),/APPEND
  
;  Write spatial calibration to second extension
  
  writefits,wavecalname,mc_unrotate(spat,rotation),/APPEND
  
;  Write the bitmask the third extension
  
  writefits,wavecalname,mc_unrotate(byte(mask),rotation),/APPEND
  
  a = obj_new('mcoplotedge')
  a->set,xranges,edgecoeffs
  
  ximgtool,spat, $
           STDIMAGE=instr.stdimage, $
           PLOTWINSIZE=instr.plotwinsize,EXT=2,BUFFER=3,OPLOT=a,$
           ZUNITS='arcsec',POSITION=[1,0],$
           BITMASK={bitmask:mask*(omask gt 0),plot1:[0,2]},$
           FILENAME='Spatial Calibration'
  
  ximgtool,wave,$
           STDIMAGE=instr.stdimage, $
           PLOTWINSIZE=instr.plotwinsize,EXT=1,BUFFER=2,OPLOT=a, $
           ZUNITS='um',$
           BITMASK={bitmask:mask*(omask gt 0),plot1:[0,2]},$
           FILENAME='Wavelength Calibration'
  
  ximgtool,flat,$
           STDIMAGE=instr.stdimage, $
           PLOTWINSIZE=instr.plotwinsize,EXT=1,BUFFER=1,OPLOT=a,/LOCK,$
           ZRANGE=[0.9,1.1], $
           BITMASK={bitmask:flatflags*(omask gt 0),plot1:[0,2]},$
           FILENAME='Normalized Flat'
  
end
;
; ==============================================================================
;
pro spextoolette_telcor,objfile,stdfile,bmag,vmag,oname,force,SHIFTRANGE=shiftrange,$
                        CANCEL=cancel

  cancel = 0

  common spextoolette_state
  
;  Get hydrogen lines
  
  readcol,filepath('HI.dat',ROOT_DIR=state.spextoolpath,SUBDIR='data'),$
         hlines,hnames,FORMAT='D,A',COMMENT='#',DELIMITER='|',/SILENT

;  Get IP profile coefficients

  readcol,filepath('IP_coefficients.dat',ROOT_DIR=state.packagepath, $
                   SUBDIR='data'),tmp,FORMAT='A',COMMENT='#',DELIMITER=':'

  ipcoeffs_sw03 = double(strsplit(tmp[0],' ',/EXTRACT))
  ipcoeffs_sw05 = double(strsplit(tmp[1],' ',/EXTRACT))
  ipcoeffs_sw08 = double(strsplit(tmp[2],' ',/EXTRACT))
  ipcoeffs_sw16 = double(strsplit(tmp[3],' ',/EXTRACT))
  ipcoeffs_sw30 = double(strsplit(tmp[4],' ',/EXTRACT))

;  Read spectra and load data.
  
  mc_readspec,state.procpath+stdfile,std,stdhdr,stdobsmode,start,stop, $
              stdnorders,stdnaps,stdorders,stdxunits,stdyunits,slith_pix, $
              slith_arc,slitw_pix,slitw_arc,stdrp,stdairmass,xtitle,ytitle, $
              CANCEL=cancel
  if cancel then return

  std = mc_caaspecflags(std,CANCEL=cancel)
  if cancel then return
  
  
  mc_readspec,state.procpath+objfile,obj,objhdr,objobsmode,start,stop, $
              objnorders,objnaps,objorders,objxunits,objyunits,slith_pix, $
              slith_arc,slitw_pix,slitw_arc,objrp,objairmass,CANCEL=cancel
  if cancel then return  

  obj = mc_caaspecflags(obj,CANCEL=cancel)
  if cancel then return
  
  stdspec    = std
  stdhdr     = stdhdr
  stdobsmode = strtrim(stdobsmode,2)
  stddisp    = mc_fxpar(stdhdr,'DISP*')
  stdfwhm    = stddisp*slitw_pix
  stdnorders = stdnorders
  
  shift = fltarr(objnorders,objnaps)
  
  objspec   = obj
  vshift = 0.0
  dairmass = stdairmass-objairmass

;  Compute airmass difference between obj and std.

  if abs(stdairmass-objairmass) gt 0.1 then begin

     print
     print, '********Airmass difference of '+ $
            string(dairmass,FORMAT='(F4.2)')+' gt 0.1.*********'
     print
     beep
     beep
     beep

  endif

;  Load Vega model

  restore, filepath('lvega5.sav',ROOT_DIR=state.spextoolpath,SUBDIR='data') 

  wvega = wvin/10000.
  cf2vega = fc2vin
  fcvega = fcvin
  fvega = fvin

;  Load atmosphere

  if objrp ge 10000.0 then trp = round(objrp/10000.)*10000
  if objrp lt 10000.0 and objrp ge 1000.0 then trp = round(objrp/1000.)*1000
  if objrp lt 1000.0 and objrp ge 100.0 then trp = round(objrp/100.)*100
  if objrp lt 100.0 and objrp ge 10.0 then trp = 100
  if objrp eq 0 then trp=2000

  spec = readfits(filepath('atran'+strtrim(trp,2)+'.fits', $
                           ROOT_DIR=state.spextoolpath,SUBDIR='data'))
  awave = reform(spec[*,0])
  atrans = reform(spec[*,1])


;  Construct the kernel

  case float(slitw_arc) of 
     
     0.3: parms = ipcoeffs_sw03
     
     0.5: parms = ipcoeffs_sw05
     
     0.8: parms = ipcoeffs_sw08
     
     1.6: parms = ipcoeffs_sw16
     
     3.0: parms = ipcoeffs_sw30
     
  endcase

  vdisp = 1.49774e-5
  vkernw_pix = stdfwhm/vdisp
  
  nkernel = round(10.*vkernw_pix)
  if not nkernel mod 2 then nkernel = nkernel + 1
  
  kernel_vx = (findgen(nkernel)-fix(nkernel)/2)*vdisp
  kernel_sx = kernel_vx/stddisp
  
  kern   = mc_instrprof(kernel_sx,parms,CANCEL=cancel)
  if cancel then return
  
  key = 'Order'+string(stdorders,FORMAT='(i2.2)')
  kernels = create_struct(key,kern)
  
  vshift   = 0.0
  scale    = 1.0

  match,stdorders,objorders,stdidx,objidx
  
  xmc_scalelines,stdspec,stdorders,vmag,(bmag-vmag),wvega,fvega,fcvega, $
                 cf2vega,kernels,vshift,objspec,objorders,objnaps,awave, $
                 atrans,hlines,hnames,scale,scales,cutreg, $
                 XTITLE=xtitle,YTITLE=ytitle,CANCEL=cancel
  
  if not cancel then vrot   = 0.0
  if cancel then return

;  Construct telluric correction spectrum
  
  norders = fxpar(objhdr,'NORDERS')
  match,stdorders,objorders,stdidx
  
  tellspec = (stdspec)[*,*,stdidx]
  vegaspec = (stdspec)[*,*,stdidx]
  vegaspec[*,2,*] = 1.0
     
  mc_mktellspec, stdspec[*,0],stdspec[*,1],stdspec[*,2],vmag,bmag-vmag, $
                 (kernels).(0),scales,wvega,fvega,fcvega,cf2vega,vshift,$
                 tellcor,tellcor_error,scvega,CANCEL=cancel

;  Perform interpolations over features if necessary
     
  ndat = n_elements(cutreg.(0))
     
  if ndat ne 1 then begin
     
     nreg = ndat/2
     
     stdwave = stdspec[*,0]
     stdflux = stdspec[*,1]
     stderr  = stdspec[*,2]
     
     nonan = mc_nantrim(stdwave,3)
     tstdwave = stdwave[nonan]
     tstdflux = stdflux[nonan]
     tstderr  = stderr[nonan]
     ttellcor = tellcor[nonan]
     ttellcor_error = tellcor_error[nonan]
          
     for j = 0, nreg-1 do begin
        
        xrange = reform((*cutreg.(0))[(j*2):(j*2+1)])
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
  
  ttellspec = mc_chfunits(stdspec[*,0],tellcor,0,1,0,IERROR=tellcor_error, $
                         OERROR=ttellspec_error)
  scvega   = mc_chfunits(stdspec[*,0],scvega,0,1,0)
  
  nytitle = '!5f!D!7k!N!5 (W m!E-2!N !7l!5m!E-1!N'
  
  tellspec[*,1] = ttellspec
  tellspec[*,2] = ttellspec_error
  vegaspec[*,1] = scvega

;  Perform shift

  if keyword_set(SHIFTRANGE) then begin

;  Get plotting ranges

     owave = obj[*,0]
     oflux = obj[*,1]

     twave = tellspec[*,0]
     tflux = tellspec[*,1]
     
     
     smooth = median(oflux,4)
     oflux = oflux/max(smooth,/NAN)
     
     smooth = median(1./tflux,4)
     dtflux = 1./tflux/max(smooth,/NAN)
  
;  Get ranges.
  
     plot1xrange = [min(owave,/NAN,MAX=max),max]
     plot1yrange = [0,max([max(oflux,/NAN),max(dtflux,/NAN)])]
     
     mc_interpspec,twave,dtflux,owave,ndtflux,CANCEL=cancel
     if cancel then return
     
     z = where(owave gt shiftrange[0] and owave lt shiftrange[1],count)
     
     shifts = findgen(151)/50.-1.5
     rms    = fltarr(151)

     x2 = findgen(n_elements(ndtflux))

     screen = get_screen_size()
     winsize = screen*[0.25,0.75]
     window, /FREE,XSIZE=winsize[0],YSIZE=winsize[1]
     win1 = !D.window
     pos = mc_gridplotpos([0.1,0.1,0.95,0.95],[1,2],0.05)
     window, /FREE,XSIZE=winsize[0],YSIZE=winsize[1],/PIXMAP
     pixmap = !D.window


     divspec = oflux/ndtflux
     yrange = [min((median(divspec,4))[z],/NAN,MAX=max),max]
     
     for i = 0, 150 do begin
        
        x1 = x2+shifts[i]
        mc_interpspec,x1,ndtflux,x2,y,CANCEL=cancel
        if cancel then return
        
        divspec = oflux/y
        mc_moments,divspec[z],mean,var,stddev,/SILENT
        rms[i] = stddev

        wset, pixmap
        plot,owave,oflux,/XSTY,/YSTY,$
             XTICKNAME=replicate(' ',10),$
             YTITLE='Normalized Flux',PSYM=10,POSITION=pos[*,0]
        oplot,owave,y,COLOR=2,PSYM=10
        plots,[shiftrange[0],shiftrange[0]],!y.crange,LINESTYLE=1,COLOR=6
        plots,[shiftrange[1],shiftrange[1]],!y.crange,LINESTYLE=1,COLOR=6

        plot,owave,divspec,/XSTY,/YSTY,$
             YRANGE=yrange,$
             POSITION=pos[*,1],$
             XTITLE='Wavelength (um)',/NOERASE
        
        wset, win1
        
        device, COPY=[0,0,winsize[0],winsize[1],0,0,pixmap]
        
        
     endfor
     wait, 0.5
     wdelete,win1,pixmap
     
;  Choose best shift value and update plot and message window
  
     min = min(rms,minidx)
     del = 5

;     window, 2
;     plot,shifts,rms,/XSTY,/YSTY,xrange=[-0.5,0.5]
     
     coeff = poly_fit1d(shifts[(minidx-del):(minidx+del)],$
                        rms[(minidx-del):(minidx+del)],2,/SILENT)
     
;     oplot,shifts[(minidx-del):(minidx+del)],$
;           poly(shifts[(minidx-del):(minidx+del)],coeff),color=2
     
     bestshift = -coeff[1]/2./coeff[2]
     
  endif

;  Write file out

  units = 'Wm-2um-1'
;
;  Write the telluric correction spectrum to disk
;
  
;  Get info from obj and std hdr.
     
  orders    = fxpar(objhdr,'ORDERS',COMMENT=corders)
  norders   = fxpar(objhdr,'NORDERS',COMMENT=cnorders)
  obsmode   = fxpar(stdhdr,'MODENAME',COMMENT=cobsmode)
  std       = fxpar(stdhdr,'OBJECT')
  xunits    = fxpar(stdhdr,'XUNITS',COMMENT=cxunits)
  xtitle    = fxpar(stdhdr,'XTITLE',COMMENT=cxtitle,COUNT=count)
     
;  For backwards compatibility
     
  if count eq 0 then begin
     
     xtitle  = '!7k!5 ('+strtrim(xunits,2)+')'
     cxtitle = 'IDL X Title'
     
  endif
     
;  Create hdr for the telluric correction spectrum

  fxhmake,hdr,tellspec
  fxaddpar,hdr,'IRAFNAME',strtrim(oname+'_tellspec.fits',2)
  fxaddpar,hdr,'ORDERS',orders,corders
  fxaddpar,hdr,'NORDERS',norders,cnorders
  fxaddpar,hdr,'A0VStd',std,' Tellluric Correction A0 V Standard'
  fxaddpar,hdr,'A0VBmag',bmag,' B-band magnitude'
  fxaddpar,hdr,'A0VVmag',vmag,' V-band magnitude'
  fxaddpar,hdr,'GRAT',obsmode,cobsmode 
  fxaddpar,hdr,'NAPS',1, ' Number of apertures'
  fxaddpar,hdr,'AIRMASS',stdairmass, 'Average airmass'

  fxaddpar,hdr,'DAIRMASS',dairmass, ' Average of (std-obj) airmass'
  fxaddpar,hdr,'TELMETH',strtrim('IP',2), ' Telluric Correction Method'
  fxaddpar,hdr,'Vegadv',vshift, ' Vega velocity shift in km s-1'
  
  
  history = 'These telluric correction spectra were constructed from the '+$
            'spectra '+strtrim(stdfile,2)+'.  The velocity shift of' + $
            ' Vega is '+strtrim(vshift,2)+' km s-1.  '
  
  history = history+' The Vega model was modified using the IP method.'

  fxaddpar,hdr,'XUNITS',xunits,cxunits
  fxaddpar,hdr,'XTITLE',xtitle,cxtitle
  fxaddpar,hdr,'YUNITS',strcompress(units,/RE)+'/DNs-1',' Units of the Y axis'
  fxaddpar,hdr,'YTITLE',nytitle+' / DN s!U-1!N)','IDL Y title'
  
  hdr = mc_addhistlabel(hdr,'Xtellcor History',CANCEL=cancel)
  if cancel then return
  
  history = mc_splittext(history,70,CANCEL=cancel)
  if cancel then return
  sxaddhist,history,hdr
  
;  Write it  to disk
  
  writefits,state.procpath+oname+'_tellspec.fits',tellspec,hdr
  
  print
  print,'Wrote the telluric correction spectrum to '+strtrim(oname,2)+ $
        '_tellspec.fits'
  print
  
;  Write the telluric corrected object spectrum to disk.  First
;  telluric correct the object spectra
  
  corspec  = objspec
        
;  Interpolate telluric spectrum onto object wavelength sampling.
        
  mc_interpspec,tellspec[*,0],tellspec[*,1],objspec[*,0],nflux,nerror,$
                IYERROR=tellspec[*,2],CANCEL=cancel
  if cancel then return

;  Interpolate mask into object wavelength sampling

  mc_interpflagspec,tellspec[*,0],byte(tellspec[*,3]),objspec[*,0],nflag, $
                    CANCEL=cancel
  if cancel then return

;  Now shift spectrum.

  if keyword_set(SHIFTRANGE) then begin
  
     x = findgen(n_elements((objspec)[*,0]))
     mc_interpspec,x+bestshift,nflux,x,nnflux,nnerror,IYERROR=nerror, $
                   CANCEL=cancel
     if cancel then return
     
     mc_interpflagspec,x+bestshift,byte(nflag),x,nnflag,CANCEL=cancel
     if cancel then return
     
     corspec[*,1] = nnflux*(objspec)[*,1]
     corspec[*,2] = sqrt(nnflux^2*(objspec)[*,2]^2+(objspec)[*,1]^2*nnerror^2 )
     
     result = mc_combflagstack(byte([[[(objspec)[*,3]]],[[nnflag]]]), $
                               CANCEL=cancel)
     if cancel then return
     
     corspec[*,3] = result
     
     
  endif else begin
     
     corspec[*,1] = nflux*objspec[*,1]
     corspec[*,2] = sqrt(nflux^2 * objspec[*,2]^2 + objspec[*,1]^2 * nerror^2 )
     
     result = mc_combflagstack(byte([[[objspec[*,3]]],[[nflag]]]),CANCEL=cancel)
     if cancel then return
  
     corspec[*,3] = result

  endelse
     
;  Now write it to disk
  
  std       = fxpar(stdhdr,'OBJECT')
  hdr       = objhdr
  sxdelpar,hdr,['DIVISOR','BEAM','GRSTCNT','CALMIR','DIT','OSF','QTH_LAMP',$
                'INC_LAMP','IR_SRC','ARG_SRC','SHUTTER','AIMAGE','SKY']
  
  fxaddpar,hdr,'IRAFNAME',strtrim(oname+'.fits',2)
  fxaddpar,hdr,'YUNITS',strcompress(units,/RE), 'Units of the Y axis'
  fxaddpar,hdr,'YTITLE',nytitle+')',' IDL Y title'
  fxaddpar,hdr,'A0VStd',std,' Tellluric Correction A0 V Standard'
  fxaddpar,hdr,'A0VBmag',bmag,' B-band magnitude'
  fxaddpar,hdr,'A0VVmag',vmag,' V-band magnitude'
  fxaddpar,hdr,'DAIRMASS',dairmass, ' Average of (std-obj) airmass'
  fxaddpar,hdr,'TELMETH',strtrim('IP',2), ' Telluric Correction Method'
  fxaddpar,hdr,'Vegadv',vshift, ' Vega velocity shift in km s-1'
  
  history = 'This spectrum was telluric corrected using the telluric '+$
            'correction spectra '+strtrim(oname,2)+'_tellspec.fits.  ' + $
            'The velocity shift of Vega is '+strtrim(vshift,2)+ $
            ' km s-1.'
  
  history = history+' The Vega model was modified using the IP method.'

  if keyword_set(SHIFTRANGE) then begin

     history = history+'  The telluric correction spectra for aperture '+ $
               string(1,FORMAT='(i2.2)')+' were shifted by '+ $
               strjoin(strtrim(bestshift,2),', ')+' pixels.'
     
  endif
  
  fxaddpar,hdr,'Telfile',oname+'_tellspec.fits',' The telluric correction file'
 
  hdr = mc_addhistlabel(hdr,'Xtellcor History',CANCEL=cancel)
  if cancel then return

  print, history
  history = mc_splittext(history,70,CANCEL=cancel)
  if cancel then return
  sxaddhist,history,hdr
  
  writefits,state.procpath+oname+'.fits',corspec,hdr
  xvspec,state.procpath+oname+'.fits',/PLOTLINMAX
  
  print
  print,'Wrote the corrected spectrum to '+strtrim(oname,2)+'.fits'
  print
 
end
;
; ==============================================================================
;
pro spextoolette_ximgtool,spatcal,wavecal,workimage,varimage,bitmask,stdimage, $
                          plotwinsize,WID=wid,OPLOT=oplot,FORCE=force, $
                          CANCEL=cancel

  cancel = 0
  
  if xregistered('ximgtool') and ~keyword_set(FORCE) then begin
    
     ximgtool,spatcal,BUFFER=5, OPLOT=oplot,/NOUPDATE,/NODISPLAY

     ximgtool,wavecal,BUFFER=4, OPLOT=oplot,/NOUPDATE,/NODISPLAY
     
     ximgtool,workimage/sqrt(varimage),/NOUPDATE,BUFFER=3,OPLOT=oplot,/NODISPLAY

     ximgtool,sqrt(varimage),BUFFER=2,OPLOT=oplot,/NOUPDATE,/NODISPLAY
     
     ximgtool,workimage,BUFFER=1,OPLOT=oplot,/NOUPDATE,/LOCK,WID=wid

  endif else begin

     ximgtool,spatcal,$
              BUFFER=5,RANGE='98%',$
              WID=wid,STD=stdimage,PLOTWINSIZE=plotwinsize,$
              POSITION=[1,0],FILENAME='Spatial Calibration',$
              OPLOT=oplot,/NODISPLAY,ZUNITS='(arcsec)'
     
     ximgtool,wavecal,$
              BUFFER=4,RANGE='98%',$
              WID=wid,STD=stdimage,PLOTWINSIZE=plotwinsize,$
              POSITION=[1,0],FILENAME='Wavelength Calibration',$
              OPLOT=oplot,/NODISPLAY,ZUNITS='(um)'
     
     ximgtool,workimage/sqrt(varimage),$
              BUFFER=3,RANGE='98%',$
              WID=wid,STD=stdimage,PLOTWINSIZE=plotwinsize,$
              POSITION=[1,0],FILENAME='S/N', $
              BITMASK={bitmask:bitmask,plot1:[0,2]},$
              OPLOT=oplot,/NODISPLAY
     
     ximgtool,sqrt(varimage),$
              BUFFER=2,RANGE='98%',$
              WID=wid,STD=stdimage,PLOTWINSIZE=plotwinsize,$
              ZUNITS='(DN / s)',POSITION=[1,0],FILENAME='Uncertainty', $
              BITMASK={bitmask:bitmask,plot1:[0,2]},$
              OPLOT=oplot,/NODISPLAY
     
     ximgtool,workimage,$
              BUFFER=1,RANGE='98%',$
              WID=wid,STD=stdimage,PLOTWINSIZE=plotwinsize,$
              ZUNITS='(DN / s)',POSITION=[1,0],FILENAME='Flux',/LOCK,$
              BITMASK={bitmask:bitmask,plot1:[0,2]},OPLOT=oplot
     
  endelse



end
;
; ==============================================================================
;
pro spextoolette,ifile,CANCEL=cancel

  mc_mkct
  common spextoolette_state
  
  if n_params() ne 1 then begin

     print
     print, 'Syntax - spextoolette, ifile, CANCEL=cancel'
     print
     cancel = 1
     return
     
  endif

;  Read user input file
  
  ifile = mc_cfile(ifile,CANCEL=cancel)
  if cancel then return
  
  line = ''
  openr, lun, ifile,/GET_LUN
  readf, lun, line
    readf, lun, line
  instrument = strtrim((strsplit(line,'=',/EXTRACT))[1],2)
  readf, lun, line
  rawpath = strtrim((strsplit(line,'=',/EXTRACT))[1],2)
  readf, lun, line
  calpath = strtrim((strsplit(line,'=',/EXTRACT))[1],2)
  readf, lun, line
  procpath = strtrim((strsplit(line,'=',/EXTRACT))[1],2)
  
  free_lun, lun

  print
  print, 'Loading parameters for '+instrument+'...'
  print
  instrument = strlowcase(instrument)
  
  rawpath = mc_cpath(rawpath,CANCEL=cancel)
  if cancel then return

  calpath = mc_cpath(calpath,CANCEL=cancel)
  if cancel then return

  procpath = mc_cpath(procpath,CANCEL=cancel)
  if cancel then return


  readcol,ifile,cals,objprefix,objfiles,stdprefix,stdfiles,aps, $
          objsclwrange,objshapecor, stdsclwrange,stdshapecor,bmag,vmag,shiftflag, $
          shiftrange,oname,force,COMMENT='#',FORMAT='A,A,A,A,A,A,A,I,A,I,F,F,I,A,A,I', $
          /SILENT,DELIMITER='|'

  nsets = n_elements(cals)

;  Get Spextool path

  spextoolpath = file_dirname( $
                 file_dirname(file_which('Spextool_Instruments.dat')),/MARK)


;  Check to make sure the instrument file exists.

  check = file_which(instrument+'.dat')
  if check eq '' then begin

     print
     print, 'Error:  Spextool does not recognize the instrument '+instrument+'.'
     print
     cancel = 1
     return

  endif

;  Get package path.

  packagepath = file_dirname(file_dirname(check),/MARK)

;  Get instrument info and default settings

  instr = mc_readinstrfile(filepath(instrument+'.dat', $
                                    ROOT_DIR=packagepath,SUBDIR='data'), $
                           CANCEL=cancel)
  if cancel then return

;  Get version number

  readcol,file_which('Version.dat'),version,FORMAT='A'

;  Get readfits name

  readfitsprog = (instrument eq 'spex') ? 'mc_readspexfits':'mc_readuspexfits'

;  Get bad pixel mask

  bdpxmk = readfits(filepath(instr.bdpxmk,ROOT=packagepath,SUBDIR='data'))
  bdpxmask = byte(bdpxmk)

  state = {rawpath:rawpath,$
           calpath:calpath,$
           procpath:procpath,$
           packagepath:packagepath,$
           spextoolpath:spextoolpath,$
           readfitsprog:readfitsprog,$
           version:version[0],$
           instrument:instrument}
    
;  Start the giant loop
  
  for i = 0,nsets-1 do begin

     print
     print, 'Beginning data set '+string(i+1,FORMAT='(I2.2)')+'...'
     print

;  Make calibration frames

     spextoolette_mkcals,cals[i],force[i],flatname,wavecalname,CANCEL=cancel
     if cancel then return
     
;  Extract object spectra   
     
     spextoolette_extract,objfiles[i],objprefix[i],flatname,wavecalname, $
                          aps[i],force[i],CANCEL=cancel
     if cancel then return

;  Combine object spectra

     obj = spextoolette_combine(objfiles[i],objsclwrange[i],objshapecor[i], $
                                force[i],CANCEL=cancel)
     if cancel then return

;  Extract standard spectra   

     spextoolette_extract,stdfiles[i],stdprefix[i],flatname,wavecalname, $
                          aps[i],force[i],CANCEL=cancel
     if cancel then return

;  Combine standard spectra
     
     std = spextoolette_combine(stdfiles[i],stdsclwrange[i],stdshapecor[i], $
                                force[i],CANCEL=cancel)
     if cancel then return

;  Telluric correction

     
     if shiftflag[i] then range = float(strsplit(shiftrange[i],'-',/EXTRACT))

     spextoolette_telcor,obj,std,bmag[i],vmag[i],strtrim(oname[i],2),force[i],$
                         SHIFTRANGE=range,CANCEL=cancel
     if cancel then return
     
  endfor

  
  
end
