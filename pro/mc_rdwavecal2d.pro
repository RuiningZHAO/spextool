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
  wavecal = mrdfits(ifile,1,/SILENT)
  spatcal = mrdfits(ifile,2,/SILENT)

  if n_elements(ROTATE) ne 0 then begin

     wavecal = rotate(temporary(wavecal),rotate)
     spatcal = rotate(temporary(spatcal),rotate)

  endif

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

  wdisp = mc_fxpar(hdr,'DISP*')

  if wxdeg+wydeg+sxdeg+sydeg gt 0 then begin

     xy2w = dblarr((wxdeg+1)*(wydeg+1),norders,/NOZERO)
     xy2s = dblarr((sxdeg+1)*(sydeg+1),norders,/NOZERO)
     
     for i = 0,norders-1 do begin
        
        wname = 'OR'+string(orders[i],FORMAT='(i2.2)')+'WC*'
        sname = 'OR'+string(orders[i],FORMAT='(i2.2)')+'SC*'
        
        xy2w[*,i] = fxpar(hdr,wname)
        xy2s[*,i] = fxpar(hdr,sname)
        
     endfor

  endif else begin

     xy2w = 0
     xy2s = 0

  endelse
  
end
