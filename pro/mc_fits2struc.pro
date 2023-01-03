;+
; NAME:
;     mc_fits2struc
;
; PURPOSE:
;     To read a Spextool FITS file into a structure.
;
; CALLING SEQUENCE:
;     result = mc_fits2struc(array,norders,naps,CANCEL=cancel)
;
; INPUTS:
;     array - 
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;
; OUTPUTS:
;
;
; OPTIONAL OUTPUTS:
;
;
; COMMON BLOCKS:
;
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
;
;-
function mc_fits2struc,spec,norders,naps,CANCEL=cancel

  cancel = 0

  l = 0

  for i = 0,norders-1 do begin

     for j = 0,naps-1 do begin

        key = 'ODR'+string(i+1,FORMAT='(I3.3)')+ $
              'AP'+string(j+1,FORMAT='(I3.3)')

        if l eq 0 then begin

           struc = create_struct(key,spec[*,*,i*naps+j])
           l = l+1

        endif else begin

           struc = create_struct(struc,key,spec[*,*,i*naps+j])
           
        endelse
        
     endfor

  endfor

  return, struc

end
