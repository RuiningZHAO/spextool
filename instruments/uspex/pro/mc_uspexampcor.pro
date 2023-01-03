;+
; NAME:
;     mc_uspexampcor
;
; PURPOSE:
;     To correct for amplifier noise in a (upgraded) SpeX image.
;
; CALLING SEQUENCE:
;     result = mc_uspexampcor(img,CANCEL=cancel)
;
; INPUTS:
;     img - A 2048 x 2048 uSpeX image in the default orientation.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     SILENT - Set to keep the program quite.
;     CANCEL - Set on return if there is a problem.
;
; OUTPUTS:
;     A 2048 x 2048 image where the median of the reference pixels (at
;     the bottom of the image)) for a given amplifier is subtracted
;     off all of the pixels read by an amplifier.
;
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     None
;
; RESTRICTIONS:
;     None
;
; DEPENDENCIES:
;     Requires the Spxtool library.
;
; PROCEDURE:
;     Just simple math.
;
; EXAMPLES:
;     duh
;
; MODIFICATION HISTORY:
;     2014-08-07 - Written by M. Cushing, University of Toledo
;     2014-08-08 - SILENT keyword added, W. Vacca, SOFIA
;-
function mc_uspexampcor,img,IVAR=ivar,OVAR=ovar,SILENT=silent,CANCEL=cancel

  cancel = 0
  
  if n_params() ne 1 then begin

     print, 'Syntax - result = mc_uspexampcor(img,SILENT=silent,CANCEL=cancel)'
     cancel = 1
     return, -1

  endif

  cancel = mc_cpar('mc_uspexampcor',img,1,'img',[2,3,4,5],[2])
  if cancel then return, -1

  tmp = img

  for i = 0,31 do begin

     xl = 0+64*i
     xr = xl+63
     
     med = median(img[xl:xr,2044:2047],/EVEN)
     tmp[xl:xr,*] = tmp[xl:xr,*]-med

     if not(keyword_set(SILENT)) then begin

        var = (1.4826*median( abs(img[xl:xr,2044:2047]-med), /EVEN))^2
        print, 'Median = ',med,' sigma = ', sqrt(var)
     
     endif

  endfor

  return, tmp

end
