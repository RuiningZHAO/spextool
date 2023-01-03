;+
; NAME:
;     mc_findprefixes
;
; PURPOSE:
;     To identify the file prefixes of (gzipped) FITS files.
;
; CALLING SEQUENCE:
;     result = mc_findspexprefixes(dir,nint,nsuffix,WIDGET_ID=widget_id,$
;                                  SILENT=silent,CANCEL=cancel)
;
; INPUTS:
;     dir     - A string scalar giving the directory to be searched.
;     nint    - Number of integer positions in the file name, for ;
;               spc0025.a.fits, nint=4.
;     nsuffix - Number of positions in the suffix, for 
;              spc0025.a.fits, nsuffix=6.
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
function mc_findprefixes,dir,nint,nsuffix,WIDGET_ID=widget_id, $
                         SILENT=silent,CANCEL=cancel

  cancel = 0

  files = file_search(dir,'*.fits*',COUNT=cnt)
  if cnt eq 0 then begin

     if keyword_set(SILENT) then begin

        cancel = 1
        return, -1

     endif
     
     message = 'No files found in '+dir+'.'
     message = mc_splittext(message,40,CANCEL=cancel)
     if cancel then return, -1

     if n_elements(WIDGET_ID) then begin

        ok = dialog_message(message,/ERROR,DIALOG_PARENT=widget_id)
        cancel = 1
        return, -1
        
     endif else begin

        print
        print, message
        print
        cancel = 1
        return,-1

     endelse
     
  endif

  
  base = strarr(cnt)
  mask = intarr(cnt)+1
  
  for i = 0,cnt-1 do begin

     tmp = file_basename(files[i])

;  Check to see if it ends with .gz.

     offset = (strmid(tmp,1,2,/REVERSE_OFFSET) eq 'gz') ? 3:0

;  Now extract the suffix
     
     tmp2 = strmid(tmp,0,strlen(tmp)-nint-nsuffix-1-offset)
     base[i] = tmp2
     if strlen(tmp2) eq 0 then mask[i] = 0
     
  endfor

  z = where(mask eq 1,cnt)
  if cnt ne 0 then begin

     base = base[z]
     return, base[uniq(base)]

  endif else begin

     if keyword_set(SILENT) then begin

        cancel = 1
        return, -1
        
     endif
     
     message = 'No files found in '+dir+'.'
     message = mc_splittext(message,40,CANCEL=cancel)
     if cancel then return, -1

     if n_elements(WIDGET_ID) then begin

        ok = dialog_message(message,/ERROR,DIALOG_PARENT=widget_id)
        cancel = 1
        return, -1
        
     endif else begin

        print
        print, message
        print
        cancel = 1
        return,-1

     endelse
     
  endelse
  

end
