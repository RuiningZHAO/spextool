;+
; NAME:
;     mc_writespec
;    
; PURPOSE:
;     Writes a SpeX spectra to disk
;   
; CATEGORY:
;     Spectroscopy
; 
; CALLING SEQUENCE:
;     mc_writespec,spectra,fullpath,aimage,itot,sky,flat,naps,orders,$
;                  hdrinfo,appos,apradius,mode,slith_pix,slith_arc,slitw_pix,$
;                  slitw_arc,rp,xunits,yunits,xtitle,ytitle,instrument,version,$
;                  PSFRADIUS=psfradius,BGSTART=bgstart,BGWIDTH=bgwidth,$
;                  BGORDER=bgorder,TRACEFILE=tracefile,WAVECAL=wavecal,$
;                  WAVETYPE=wavetype,DISP=disp,FITS=fits,TEXT=text,$
;                  RMS1D=rms1d,RMS2d=rms2d,RES=res,LINEARITY=linearity,$
;                  LINCORMAX=lincormax,EXT2D=ext2d,CANCEL=cancel
;    
; INPUTS:
;     spectra    - An array [*,3,naps*norders] array of spectra where
;                  array [*,0,0] = wavelengths
;                  array [*,1,0] = flux
;                  array [*,2,0] = error
;     fullpath   - The fullpath of the file to be written to
;     aimage     - A string of the name if the Aimage
;     itot       - The total integration time of the spectrum
;     sky        - A string of the sky image
;     flat       - A string of the flat field image
;     naps       - The number of apertures
;     orders     - An array of order numbers
;     hdrinfo    - A structure with FITS keywords and values
;     appos      - An array [naps,norders] of aperture positions
;     apradius   - The aperture radius of the extraction
;     mode       - A string giving the mode name of the spectra
;     slith_pix  - The slit length in pixels
;     slith_arc  - The slit length in arcseconds
;     slitw_pix  - The slit width in pixels
;     slitw_arc  - The slit width in arcseconds
;     rp         - Resolving power
;     xunits     - A string giving the units of array[*,0,0]
;     yunits     - A string giving the units of array[*,1,0]
;     xtitle     - A string of the Xtitle to be used for IDL plotting
;     ytitle     - A string of the Ytitle to be used for IDL plotting
;     instrument - The name of the instrument
;     version    - Spextool version number
;    
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     PSFRADIUS - The radius of the psf in the optimal extraction
;     BGSTART   - The background starting radius (see mkmask_ps.pro)
;     BGWIDTH   - The background width (see mkmask_ps.pro)
;     BGORDER   - The polynomial degree of the background fit
;     TRACEFILE - A string giving the name of the trace file used in 
;                 the extraction (if a trace file was used)
;     WAVECAL   - A string giving the name of the wavecal file
;     WAVETYPE  - Air or vacuum
;     DISP      - An array [nordesr*naps] of wavelength dispersion
;     FITS      - Write a FITS file
;     TEXT      - Write a text file
;     RMS1D     - An [norders] array of RMS values for the 1D
;                 wavelength solution of each order.
;     RMS2D     - The RMS value of the 2D wavelength solution.
;     RES       - The average resolution of the spectra 
;     LINEARITY - If set, a note will be put in the header that the 
;                 spectra were corrected for non-linearity   
;     LINCORMAX - The maxmimum of the linearity correction in DN.
;     EXT2D     - Set to write the 2D extraction history instead of
;                 the 1D version
;     CANCEL   - Set on return if there is a problem
;     
; OUTPUTS:
;     Writes a FITS or text file to disk
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
;     Easy
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2000-09-01 - Written by M. Cushing, Institute for Astronomy, UH
;     2002-10-09 - Added LINEARITY keyword
;     2003-02    - Added optimal extraction output
;     2005-07-04 - Modified to accept the new hdrinfo structure
;     2005-08-04 - Moved the xunits and ynunits parameters and added the
;                  xtitle and ytitle input parameters
;     2005-09-05 - Changed ARCIMAGE keyword to WAVECAL 
;     2006-06-09 - Added appos input
;     2007-07-23 - Added instrument input
;     2008-02-22 - Added rp (resolving power) input
;                - Added the PSBGINFO and XSBGINFO to merge
;                  writespec_xs and writespec_ps
;     2008-03-07 - Added RMS1D and RMS2D keywords.
;     2010-09-05 - Added itot input.
;     2010-10-09 - Added WAVETYPE keyword.
;     2015-01-08 - Added LINCORMAX keyword.
;-
pro mc_writespec,spectra,fullpath,aimage,itot,sky,flat,naps,orders, $
                 hdrinfo,appos,apradius,mode,slith_pix,slith_arc,slitw_pix, $
                 slitw_arc,rp,xunits,yunits,xtitle,ytitle,instrument,version, $
                 PSFRADIUS=psfradius,PSBGINFO=psbginfo,XSBGINFO=xsbginfo,$
                 TRACEFILE=tracefile,WAVECAL=wavecal,WAVETYPE=wavetype,$
                 DISP=disp,FITS=fits,RMS1D=rms1d,RMS2D=rms2d,RES=res, $
                 LINEARITY=linearity,LINCORMAX=lincormax,EXT2D=ext2d, $
                 CANCEL=cancel

  cancel = 0

;  Check parameters

  if n_params() lt 22 then begin
     
     print, 'Syntax - writespec_ps,spectra,fullpath,aimage,itot,sky,flat,naps,$'
     print, '                      orders,hdrinfo,appos,apradius,mode,$'
     print, '                      slith_pix,slith_arc,slitw_pix,$'
     print, '                      slitw_arc,rp,xunits,yunits,xtitle,ytitle,$'
     print, '                      instrument,version,PSFRADIUS=psfradius,$'
     print, '                      PSBGINFO=psbginfo,XSBGINFO=xsbginfo,$'
     print, '                      TRACEFILE=tracefile,WAVECAL=wavecal,$'
     print, '                      WAVETYPE=wavetype,DISP=disp,FITS=fits,$'
     print, '                      RMS1D=rms1d,RMS2D=rms2d,RES=res,$'
     print, '                      LINEARITY=linearity,EXT2D=ext2d,$'
     print, '                      CANCEL=cancel'
     
     cancel = 1
     return
     
  endif
  cancel = mc_cpar('mc_writespec',spectra,1,'Spectra',[2,3,4,5],[2,3])
  if cancel then return
  cancel = mc_cpar('mc_writespec',fullpath,2,'Fullpath',7,0) 
  if cancel then return
  cancel = mc_cpar('mc_writespec',aimage,3,'Aimage',7,0) 
  if cancel then return
  cancel = mc_cpar('mc_writespec',itot,4,'Itot',[2,3,4,5],0) 
  if cancel then return
  cancel = mc_cpar('mc_writespec',sky,5,'Sky',7,0)
  if cancel then return
  cancel = mc_cpar('mc_writespec',flat,6,'Flat',7,0)
  if cancel then return
  cancel = mc_cpar('mc_writespec',naps,7,'Naps',[2,3,4,5],0) 
  if cancel then return
  cancel = mc_cpar('mc_writespec',orders,8,'Orders',[2,3,4,5],[0,1]) 
  if cancel then return
  cancel = mc_cpar('mc_writespec',hdrinfo,9,'Hdrinfo',8,[0,1])
  if cancel then return
  cancel = mc_cpar('mc_writespec',appos,10,'Apradius',[2,3,4,5],[1,2]) 
  if cancel then return
  cancel = mc_cpar('mc_writespec',apradius,11,'Apradius',[2,3,4,5],[0,1]) 
  if cancel then return
  cancel = mc_cpar('mc_writespec',mode,12,'Mode',7,0)
  if cancel then return
  cancel = mc_cpar('mc_writespec',slith_pix,13,'Slith_pix',[2,3,4,5],0)
  if cancel then return
  cancel = mc_cpar('mc_writespec',slith_arc,14,'Slith_arc',[2,3,4,5],0)
  if cancel then return
  cancel = mc_cpar('mc_writespec',slitw_pix,15,'Slitw_pix',[2,3,4,5],0)
  if cancel then return
  cancel = mc_cpar('mc_writespec',slitw_arc,16,'Slitw_arc',[2,3,4,5],0)
  if cancel then return
  cancel = mc_cpar('mc_writespec',rp,17,'Resolving Power',[2,3,4,5],0)
  if cancel then return
  cancel = mc_cpar('mc_writespec',xunits,18,'Xunits',7,0)
  if cancel then return
  cancel = mc_cpar('mc_writespec',yunits,19,'Yunits',7,0)
  if cancel then return
  cancel = mc_cpar('mc_writespec',xtitle,20,'Xtitle',7,0)
  if cancel then return
  cancel = mc_cpar('mc_writespec',ytitle,21,'Ytitle',7,0)
  if cancel then return
  cancel = mc_cpar('mc_writespec',instrument,22,'Instrument',7,0)
  if cancel then return
  cancel = mc_cpar('mc_writespec',version,23,'Version',7,0)
  if cancel then return

  nonlin = (n_elements(LINEARITY) eq 0) ? 0:1
  
;  Make stock FITS header.

  fxhmake,hdr,spectra

;  Now add all the keywords from the hdrinfo structure.

  norders  = n_elements(orders)
  ntags    = n_tags(hdrinfo.vals)
  
  names = tag_names(hdrinfo.vals)
  for i = 0, ntags - 1 do begin
     
     fxaddpar,hdr,names[i],hdrinfo.vals.(i), hdrinfo.coms.(i)
     
  endfor
  
;  Now add all the info passed.

  fxaddpar,hdr,'INSTR',instrument,' Instrument'
  fxaddpar,hdr,'MODENAME',mode, ' Spectroscopy mode'
  fxaddpar,hdr,'AIMAGE',aimage, ' A image'
  fxaddpar,hdr,'ITOT',itot, ' Total integration time (sec)'

  if n_elements(LINCORMAX) ne 0 then $
     fxaddpar, hdr, 'LINCRMAX',lincormax, ' Maximum of linearity correction (DN)'

  fxaddpar,hdr,'SKY',sky, ' Sky image'
  fxaddpar,hdr,'FLAT',flat, ' Flat field image'
  if n_elements(WAVECAL) ne 0 then fxaddpar,hdr,'WAVECAL',wavecal, $
                       ' Wavecal file used for wavelength calibration'

  if n_elements(WAVETYPE) ne 0 then fxaddpar,hdr,'WAVETYPE',wavetype, $
                                ' Wavelength type (air or vacuum)'
  
  if n_elements(TRACEFILE) ne 0 then fxaddpar,hdr,'TRACEFIL',tracefile, $
     ' Trace file name'
  
  fxaddpar,hdr,'NAPS',fix(naps), ' Number of apertures'
  fxaddpar,hdr,'NORDERS',fix(norders), ' Number of orders'
  fxaddpar,hdr,'ORDERS',strjoin(strcompress(fix(orders),/re),','), $
           ' Order numbers'
  fxaddpar,hdr,'SLTH_PIX',slith_pix,FORMAT='(G0.5)', $
           ' Slit height in pixels'
  fxaddpar,hdr,'SLTH_ARC',slith_arc,FORMAT='(G0.5)', $
           ' Slit height in arcseconds'
  fxaddpar,hdr,'SLTW_PIX',slitw_pix,FORMAT='(G0.5)', $
           ' Slit width in pixels'
  fxaddpar,hdr,'SLTW_ARC',slitw_arc,FORMAT='(G0.5)', $
           ' Slit width in arcseconds'
  fxaddpar,hdr,'RP',fix(rp), ' Resolving power'
  
  for i = 0, norders-1 do begin
     
     name = 'APPOSO'+string(orders[i],format='(i2.2)')
     comment = ' Aperture positions (arcsec) for order '+ $
               string(orders[i],FORMAT='(i2.2)')
     fxaddpar,hdr,name,strjoin(strtrim(mc_sigfig(appos[*,i],3),2),','), $
              comment
     
  endfor

  if n_elements(PSFRADIUS) ne 0 then $
     fxaddpar,hdr,'PSFRAD',psfradius, $
              ' Optimal extraction PSF radius in arcseconds',FORMAT='(f4.2)'

  for i =0,n_elements(apradius)-1 do begin

     fxaddpar,hdr,'AP'+string(i+1,FORMAT='(i2.2)')+'RAD', $
              apradius[i],' Aperture radius in arcseconds',FORMAT='(f4.2)'

  endfor

  if n_elements(PSBGINFO) ne 0 then begin

     fxaddpar,hdr,'BGSTART',psbginfo[0], $
              ' Background start radius in arcseconds',FORMAT='(f4.2)'

     fxaddpar,hdr,'BGWIDTH',psbginfo[1],' Background width in arcseconds', $
              FORMAT='(G0.5)'

     fxaddpar,hdr,'BGORDER',psbginfo[2],' Background polynomial fit degree',$
              FORMAT='(I1.1)'

  endif

  if n_elements(XSBGINFO) ne 0 then begin
     
     fxaddpar,hdr,'BGR',xsbginfo[0],' Background regions in arcseconds'

     fxaddpar,hdr,'BGORDER',fix(xsbginfo[1]),' Background polynomial fit degree'

  endif

  fxaddpar,hdr,'XUNITS',xunits, ' Units of the X axis'
  fxaddpar,hdr,'YUNITS',yunits, ' Units of the Y axis'
  fxaddpar,hdr,'XTITLE',xtitle, ' IDL X title'
  fxaddpar,hdr,'YTITLE',ytitle, ' IDL Y title'
  
  fxaddpar,hdr,'VERSION',version, ' Spextool version'
  
  if n_elements(RES) ne 0 then fxaddpar,hdr,'RES',res, $
                                        ' Average spectral resolving power'
  
  if n_elements(DISP) ne 0 then begin
     
     for j = 0, norders-1 do begin
        
        name = 'DISPO'+string(orders[j],format='(i2.2)')
        comment = ' Dispersion ('+xunits+' pixel-1) for order '+ $
                  string(orders[j],FORMAT='(i2.2)')
        sxaddpar,hdr,name,disp[j],comment
        
     endfor
     
  endif
  
  if keyword_set(EXT2D) then begin
     
     history = 'The orders and apertures can be accessed as follows:  If the'+$ 
               ' user desires aperture Y in order X then lambda = [*,0,( X - '+$
               'min(orders))*naps + Y-1], flux = array[*,1:nspat,( X - '+$
               'min(orders))*naps + Y-1], error = array[*,(nspat+1):*,( X - '+$
               'min(orders))*naps + Y-1] where nspat=(NAXIS2-1)/2.'
     
  endif else begin
     
     
     history = 'The output FITS files contain a 3-D array of data ' + $
               'consisting of sets of triplet arrays of data for each ' + $
               'aperture and each order, where each triplet is composed ' + $
               'of an array for the wavelength, an array for the flux, and ' + $
               'an array for the error. The triplets for each aperture are ' + $
               'stacked behind one another, followed by the triplets for ' + $
               'the next order, etc. If no orders have been skipped or ' + $
               'deslected in the extraction process, the contents of ' + $
               'aperture Y in order X can be found as follows:' + $
               '' + $
               'lambda = array[*,0,( X - (smallest order number))*naps ' + $
               '+ (Y-1)], flux   = array[*,1,( X - (smallest order ' + $
               'number))*naps + (Y-1)], error  = array[*,2,( X - (smallest ' + $
               'order number))*naps + (Y-1)]' + $
               '' + $
               '  For example, for an SXD file with two apertures, the ' + $
               'wavelength array for aperture 1 in order 3 is located in ' + $
               'array [*,0,0], the flux is in array [*,1,0] and the error ' + $
               'is in array [*,2,0]. For aperture 2, the wavelength is in ' + $
               'array [*,0,1], the flux is in array [*,1,1] and the error ' + $
               'is in array [*,2,1]. For order 4, the flux for aperture 1 ' + $
               'is in array [*,1,2], while the flux for aperture 2 is in ' + $
               'array [*,1,3]. For order 5, the fluxes for the two ' + $
               'apertures are in arrays [*,1,4] and [*,1,5], etc.'
     
  endelse
  
  if n_elements(RMS2D) ne 0 then begin
     
     history =history+'  The RMS errors in angstroms of the wavelength ' + $
              'calibration is '+strjoin(strcompress(rms2d*10^4,/RE),', ')+'.'
     
  endif

  if n_elements(RMS1D) ne 0 then begin
     
     history =history+'  The RMS errors in angstroms of the wavelength ' + $
              'calibrations are '+strjoin(strcompress(rms1d*10^4,/RE),', ')+'.'
     
  endif
  
  if keyword_set(LINEARITY) then history = history+'  The spectra have been '+$
                                           'corrected for non-linearity.'

  hdr = mc_addhistlabel(hdr,'Xspextool History',CANCEL=cancel)
  if cancel then return
  
  history = mc_splittext(history,70,CANCEL=cancel)
  if cancel then return
  sxaddhist,history,hdr
  
  writefits,fullpath,spectra,hdr
  

end

