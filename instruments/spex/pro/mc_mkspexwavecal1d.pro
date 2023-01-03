;+
; NAME:
;     mc_mkspexwavecal1d
;
; PURPOSE:
;     To create a SpeX wavecal file
;
; CALLING SEQUENCE:
;     mc_mkspexwavecal1d,ifiles,flatfile,spextoolpath,packagepath,[ofile], $
;                        SKYFILES=skyfiles,MANUALXCORR=manualxcorr, $
;                        PLOTXCORR=plotxcorr,PLOTLINEFIND=plotlinefind, $
;                        NODISPLAY=nodisplay,USESTOREDSOLUTION=usestoredsolution,$
;                        GROUP_LEADER=group_leader,CANCEL=cancel
;
; INPUTS:
;     ifiles       - A string array giving the input arc files
;     flatfile     - A string giving the fullpath to the processed flat field.
;     spextoolpath - The path to the Spextool directory, e.g.,
;                    ~/IDL/Spextool/
;     packagepath  - The path to the Instrument directory, e.g.,
;                    ~/IDL/spextool/instruments/spex/
;     autoxcorr    - Set to do the correc
;
; OPTIONAL INPUTS:
;     ofile - A string giving the fullpath name of the new wavecal
;             file.  Note the .fits should NOT be included for this input.

;
; KEYWORD PARAMETERS:
;     SKYFILES     - Either a string or an array of strings giving the
;                    fullpaths to the sky files.  If two files, one should
;                    be an A beam and 1 should be a B beam.
;     MANUALXCORR  - Set to manually derive the cross correlation.
;     PLOTXCORR    - Plot the see the results of the auto cross
;                    correlation.
;     PLOTLINEFIND - Set to see the line finding routine work.
;     NODISPLAY    - Set to surpress displaying the results in ximgtooo.
;     GROUP_LEADER - The widget group leader for ximgtool.
;     CANCEL       - Set on return if there is a problem.
;
; OUTPUTS:
;     Will write a 1024x1024x2 FITS image where the first image is the
;     wavelength image and the second image is the spatial image.  The
;     default name is mode.fits in the current directory but the user
;     can override with the ofile input.
;
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     None
;
; RESTRICTIONS:
;
;
; DEPENDENCIES:
;
;
; PROCEDURE:
;
;
; EXAMPLES:
;
;
; MODIFICATION HISTORY:
;     2010-10-28 - Written by M. Cushing, JPL/Caltech
;-
pro mc_mkspexwavecal1d,ifiles,flatfile,spextoolpath,packagepath,ofile, $
                       SKYFILES=skyfiles,MANUALXCORR=manualxcorr, $
                       PLOTXCORR=plotxcorr,PLOTLINEFIND=plotlinefind, $
                       NODISPLAY=nodisplay,USESTOREDSOLUTION=usestoredsolution,$
                       GROUP_LEADER=group_leader, $
                       CANCEL=cancel

  cancel = 0

  if n_params() lt 4 then begin

     print, 'Syntax - mc_mkspexwavecal1d,ifiles,flatfile,spextoolpath, $'
     print, '                            packagepath,[ofile], $'
     print, '                            SKYFILES=skyfiles, $'
     print, '                            MANUALXCORR=manualxcorr, $'
     print, '                            PLOTXCORR=plotxcorr, $'
     print, '                            PLOTLINEFIND=plotlinefind, $'
     print, '                            NODISPLAY=nodisplay, $'
     print, '                            USESTOREDSOLUTION=usestoredsolution, $'
     print, '                            GROUP_LEADER=group_leader, $'
     print, '                            CANCEL=cancel'
     cancel = 1
     return

  endif

  cancel = mc_cpar('mc_mkspexwavecal1d',ifiles,1,'Ifiles',7,[0,1])
  if cancel then return
  cancel = mc_cpar('mc_mkspexwavecal1d',flatfile,2,'FlatField',7,0)
  if cancel then return
  cancel = mc_cpar('mc_mkspexwavecal1d',spextoolpath,3,'Spextoolpath',7,0)
  if cancel then return
  cancel = mc_cpar('mc_mkspexwavecal1d',packagepath,4,'Packagepath',7,0)
  if cancel then return

  if n_params() eq 5 then begin
     
     cancel = mc_cpar('mc_mkspexwavecal1d',ofile,5,'Ofile',7,0)
     if cancel then return

  endif

;  Get Spextool version

  versionfile = filepath('Version.dat',ROOT_DIR=spextoolpath,SUBDIR='data')
  version = ''
  openr, lun, versionfile,/GET_LUN
  readf, lun, version
  free_lun, lun

;  Get instrument info
  
  instrfile = filepath('spex.dat',ROOT_DIR=packagepath,SUBDIR='data')
  instr = mc_readinstrfile(instrfile,CANCEL=cancel)
  if cancel then return

;  Get modeinfo

  hdr  = headfits(ifiles[0])
  mode = strcompress(fxpar(hdr,'GRAT'),/RE)

  if n_params() eq 4 then ofile = mode+'_Wavecal'
  
  modefile = filepath(mode+'.dat',ROOT=packagepath,SUB='data')
  modeinfo = mc_readmodefile(modefile,CANCEL=cancel)
  if cancel then return

;  Read flat field

  mc_readflat,flatfile,flat,ncols,nrows,obsmode,ps,slith_pix,slith_arc, $
              slitw_pix,slitw_arc,rp,norders,orders,edgecoeffs,xranges,rms, $
              rotation,edgedeg,OMASK=omask,CANCEL=cancel
  if cancel then return

;  Set some keywords for the wavelength calibration

  if slitw_arc eq 1.6 or slitw_arc eq 3.0 then begin

     usestoredsolution = 1
     plotxcorr = 1
     
  endif

;  Load images in

  pair = (n_elements(skyfiles) eq 0) ? 0:1

  mc_readspexfits,ifiles,data,hdrinfo,datavar,ROTATE=rotation, $
                  KEYWORDS=instr.keywords,PAIR=pair,NIMAGES=nimages, $
                  CANCEL=cancel
  if cancel then return

;  Check to make sure this isn't early data

  date = strsplit(hdrinfo[0].vals.DATE_OBS,'-',/EXTRACT)
  juldate = julday(date[1],date[2],date[0])
  
  if obsmode eq 'ShortXD' and juldate lt 2451758L then begin

     ok = dialog_message('Please contact John Rayner to reduce data ' + $
                         'taken before Julian Date 2451758.', $
                         DIALOG_PARENT=group_leader)
     
  endif 
  
;  Combine images if necessary

  if nimages ne 1 then begin

     mc_meancomb,data,arc,arcvar,DATAVAR=datavar,CANCEL=cancel
     if cancel then return
     
  endif else begin
  
     arc    = reform(data)
     arcvar = reform(datavar)
     
  endelse

  arc = temporary(arc)/flat
  arcvar = temporary(arcvar)/flat^2

;  LXD?  Get sky

  if n_elements(SKYFILES) ne 0 then begin

     mc_readspexfits,skyfiles,skyimgs,hdr,skyvar,ROTATE=rotation, $
                     NIMAGES=nimages,CANCEL=cancel
     if cancel then return

     if nimages eq 2 then begin
        
        sky = (skyimgs[*,*,0]+skyimgs[*,*,1])- $
              abs(skyimgs[*,*,0]-skyimgs[*,*,1])
        skyvar = 2*total(skyvar,3)

        skyhistory = '  The sky frame is '+ $
                     strjoin([hdr[0].vals.IRAFNAME,hdr[1].vals.IRAFNAME],', ')
        
     endif else begin
        
        sky = skyimgs
        skyhistory = '  The sky frame is '+hdr[0].vals.IRAFNAME
        
     endelse
     
     case obsmode of 
        
        'LongXD1.9': begin

           arc = mc_mixorders(arc,sky,omask,orders,[5,7,8,9,10],[6], $
                              CANCEL=cancel)
           if cancel then return
        end

        'LongXD2.1': begin

           arc = mc_mixorders(arc,sky,omask,orders,[5,7,8,9],[4,6], $
                              CANCEL=cancel)
           if cancel then return
           
        end

        'LongXD2.3': begin

           arc = mc_mixorders(arc,sky,omask,orders,[5,7,8],[4,6],CANCEL=cancel)
           if cancel then return
           
        end

     endcase
     if cancel then return

  endif else skyhistory = ''

;  Display image

  if ~keyword_set(NODISPLAY) then begin

     ximgtool,arc,WID=wid,GROUP_LEADER=group_leader, $
              STDIMAGE=instr.stdimage,PLOTWINSIZE=instr.plotwinsize,$
              POSITION=[1,0]

  endif

;  Get Line Cal file

  file = filepath(obsmode+'_LineCal.fits',ROOT=packagepath,SUB='data')
  calinfo = mc_readlinecalfile(file,CANCEL=cancel)
  if cancel then return

;  Get setup for the cross correlation 

  wanchor = calinfo.xcorspec[*,0]
  fanchor = calinfo.xcorspec[*,1]

;  Create fake wavecal arrays to do a simple 1D sum extraction on the arc

  fake = mc_simwavecal2d(ncols,nrows,edgecoeffs,xranges,slith_arc,CANCEL=cancel)
  if cancel then return
  
;  Extract the arc down the middle of the slit in the anchororder

  arcspec = mc_sumextspec1d(arc,arcvar,omask,orders,fake[*,*,0],fake[*,*,1], $
                            [1],APRANGES=rebin([6,8],2,norders),CANCEL=cancel)
  if cancel then return

  z = where(orders eq calinfo.xcororder)

  warc = (arcspec.(z))[*,0]
  farc = (arcspec.(z))[*,1]
  
;  Do the cross correlation

    linterp,warc,farc,wanchor,ifarc,MISSING=0

  if ~keyword_set(MANUALXCORR) then begin
     
     lag = findgen(ncols)-fix(ncols/2.)
     corr = c_correlate(fanchor,ifarc,lag)
     max = max(corr,idx)
     
     win = 10*(slitw_arc/0.3*0.5) >1

     fit = mpfitpeak(lag[idx-win:idx+win],corr[idx-win:idx+win],a,NTERMS=4)
     offset = a[1]
     
     if keyword_set(PLOTXCORR) then begin
        
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

  if mode eq 'LowRes15' or keyword_set(USESTOREDSOLUTION) then begin
     
     homeorder = calinfo.homeorder
     dispdeg = calinfo.dispdeg
     ordrdeg = calinfo.ordrdeg
     p2wcoeffs = calinfo.p2wcoeffs
     rms = !values.f_nan

  endif else begin

     homeorder = modeinfo.homeorder
     dispdeg = modeinfo.dispdeg
     ordrdeg = modeinfo.dispdeg
     
;  Grab the lines.

     file = filepath('lines.dat',ROOT=packagepath,SUB='data')
     mc_rdarll,file,plines,ptypes,RES=rp,THRESH=3,CANCEL=cancel
     if cancel then return

     ;  Perform wavelength calibration
     
     p2wcoeffs = mc_xdwavecal1d(arcspec,orders,calinfo.w2pcoeffs, $
                                offset,plines,ptypes,calinfo.ordertype, $
                                slitw_pix,homeorder,dispdeg,ordrdeg,4, $
                                ofile+'_QA',RMS=rms,NLINES=nlines, $
                                /UPDATE,WIDGET_ID=group_leader,$
                                PLOT=plotlinefind,CANCEL=cancel)
     if cancel then return

     bad = where(nlines le 2,cnt)
     if cnt ne 0 then begin
        
        
        message = strjoin(strtrim(nlines[bad],2),',')+ $
                  ' lines were found in orders '+ $
                  strjoin(strtrim(orders[bad],2),',')+'.  Although the ' + $
                  'program may finish, your wavelength solution is ' + $
                  'probably bad.  Please check both the wavelength ' + $
                  'solution and the QA plot in the cal/ directory.'
        
        ok = dialog_message(mc_splittext(message,50),$
                            DIALOG_PARENT=group_leader)

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
     
     if obsmode eq 'LowRes15' then begin
        
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
  
  mwrfits,input,ofile+'.fits',nhdr,/CREATE
  sxaddpar,nhdr,ofile+'.fits',' Filename'
  sxaddpar,nhdr,'WCTYPE','1D',' Wavelength calibration type'
  sxaddpar,nhdr,'RMS',rms[0],' RMS of wavecal fit in Angstroms'

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


  mwrfits,input,ofile+'.fits',nhdr,/CREATE

;  Write wavelength calibration extension

  writefits,ofile+'.fits',mc_unrotate(wave,7),/APPEND

;  Write spatial calibration to second extension

  writefits,ofile+'.fits',mc_unrotate(spat,7),/APPEND
  
end
