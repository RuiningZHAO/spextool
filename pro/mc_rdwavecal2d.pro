;+
; NAME:
;     mc_rdwavecal2d
;
; PURPOSE:
;     To read a Spextool 2D wavecal file.
;
; CALLING SEQUENCE:
;     mc_rdwavecal2d,ifile,wavecal,spatcal,wctype,xy2w,xy2s,wxdeg,wydeg, $
;                    sxdeg,sydeg,wdisp,ROTATE=rotate,CANCEL=cancel
;
; INPUTS:
;     ifile - The name of a Spextool 2D wavecal file.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     ROTATE - If given, the wavecal and spatcal images will be
;              rotated by this value (see IDL ROTATE).
;     CANCEL - Set on return if there is a problem.
;
; OUTPUTS:
;     wavecal - The 2D wavelength calibration image.  Each pixel in an
;               order is set to its wavelength.
;     spatcal - The 2D spatial calibration image.  Each pixel in an
;               order is set to its spatial coordinate.
;     wctype  - The type of wavelength calibration, e.g., 2D or 1D.
;               1D implies that wavelength=columns.
;     xy2w    - A [(wxdeg+1)*(wydeg+1),norders] array given the
;               coefficients to convert a pixel position to a
;               wavelength.
;     xy2s    - A [(sxdeg+1)*(sydeg+1),norders] array given the
;               coefficients to convert a pixel position to a spatial
;               value.
;     wxdeg   - The X polynomial degree for the wavelength
;               calibration.
;     wydeg   - The Y polynomial degree for the wavelength
;               calibration.
;     sxdeg   - The X polynomial degree for the spatial calibration.
;     sydeg   - The Y polynomial degree for the spatial calibration.
;     wdisp   - The approximate dispersion in microns per pixel for
;               each order.
;
; OPTIONAL OUTPUTS:
;     All
;
; COMMON BLOCKS:
;     None
;
; RESTRICTIONS:
;     Must be used on a Spextool wavecal file.
;
; DEPENDENCIES:
;     redfits() [astron]
;     fxpar() [astron]
;
; PROCEDURE:
;     Just read
;
; EXAMPLES:
;     eh
;
; MODIFICATION HISTORY:
;     2009-12-03 - Written by M. Cushing, NASA JPL
;     2014-05-28 - Modified to read new MEF files.
;     2014-08-06 - Added wdisp output.
;     2022-12-25 - Modified to be compatible with paltspec by RNZ, NAOC
;-
pro mc_rdwavecal2d,ifile,wavecal,spatcal,wctype,xo2w,wdeg,odeg,homeorder, $
                   xy2w,xy2s,wxdeg,wydeg,sxdeg,sydeg,wdisp,$
                   ROTATE=rotate,CANCEL=cancel

  cancel = 0
  
  if n_params() lt 1 then begin

     print, 'Syntax - mc_rdwavecal2d,ifile,wavecal,spatcal,wctype,xy2w,$'
     print, '                        xy2s,wxdeg,wydeg,sxdeg,sydeg,$'
     print, '                        wdisp,ROTATE=rotate,CANCEL=cancel'
     cancel = 1
     return

  endif

  cancel = mc_cpar('mc_rdwavecal2d',ifile,1,'Input Filename',7,0)
  if cancel then return

  tmp = mrdfits(ifile,0,hdr,/SILENT)
;
;  Since the wavecal file generated for paltspec data has only one 
;  extension, the code below (trying to access the 2nd and the 3rd 
;  extensions) may lead to an EOF error. Moved to the if-construct 
;  below.
;  (2022-12-25 by RNZ)
;
;  wavecal = mrdfits(ifile,1,/SILENT)
;  spatcal = mrdfits(ifile,2,/SILENT)
;
;  if n_elements(ROTATE) ne 0 then begin
;  
;     wavecal = rotate(temporary(wavecal),rotate)
;     spatcal = rotate(temporary(spatcal),rotate)
;     
;  endif
;
  wctype = strtrim(fxpar(hdr,'WCTYPE'),2)

;  Get XD 1D wavelength solution 

  wdeg = fxpar(hdr,'WDEG')
  odeg = fxpar(hdr,'ODEG')
  homeorder = fxpar(hdr,'HOMEORDR')

  if wdeg+odeg gt 0 then xo2w = fxpar(hdr,'1DWC*')

;  Get 2D calibration

  wxdeg = fxpar(hdr,'WXDEG')
  wydeg = fxpar(hdr,'WYDEG')

  sxdeg = fxpar(hdr,'SXDEG')
  sydeg = fxpar(hdr,'SYDEG')

  norders = fxpar(hdr,'NORDERS')
  orders  = long( strsplit( fxpar(hdr,'ORDERS'), ',', /EXTRACT) )
;
;  The wavecal file generated for paltspec data does not have keywords 
;  'DISP*'. Moved to the if-construct below.
;  (2022-12-25 by RNZ)
;
;  wdisp = mc_fxpar(hdr,'DISP*')
;
  if wxdeg+wydeg+sxdeg+sydeg gt 0 then begin
;
;  For paltspec data, both wavecal and spatcal arrays are stored in the 1st
;  extension. And they have already been loaded above into tmp.
;  (2022-12-25 by RNZ)
;
     wavecal = reform(tmp(*, *, 0))
     spatcal = reform(tmp(*, *, 1))

     wdisp = dblarr(norders,/NOZERO)
     
     xy2w = dblarr((wxdeg+1)*(wydeg+1),norders,/NOZERO)
     xy2s = dblarr((sxdeg+1)*(sydeg+1),norders,/NOZERO)
     
     for i = 0,norders-1 do begin
;
;  For paltspec data, keywords 'DISP*' do not exist. Use fxpar to avoid 
;  halt and use /NAN to get !values.f_nan as return.
;  (2022-12-25 by RNZ)
;
        dname = 'DISPO'+string(orders[i],format='(i2.2)')
        wdisp[i] = fxpar(hdr,dname,/NAN)

        wname = 'OR'+string(orders[i],FORMAT='(i2.2)')+'WC*'
        sname = 'OR'+string(orders[i],FORMAT='(i2.2)')+'SC*'
        
        xy2w[*,i] = fxpar(hdr,wname)
        xy2s[*,i] = fxpar(hdr,sname)
        
     endfor

  endif else begin
;
;  For uspex data, wavecal and spatcal arrays are stored in the 2nd and 
;  the 3rd extentions respectively. And keywords 'DISP*' exist. 
;  (2022-12-25 by RNZ)
;
     wavecal = mrdfits(ifile,1,/SILENT)
     spatcal = mrdfits(ifile,2,/SILENT)

     wdisp = mc_fxpar(hdr,'DISP*')

     xy2w = 0
     xy2s = 0

  endelse

;
;  For both cases, wavecal and spatcal arrays are loaded properly so far.
;  (2022-12-25 by RNZ)
;
  if n_elements(ROTATE) ne 0 then begin
    
    wavecal = rotate(temporary(wavecal),rotate)
    spatcal = rotate(temporary(spatcal),rotate)
    
  endif
  
end
