;+
; NAME:
;     mc_rdohll
;
; PURPOSE:
;     Reads the OH line list and prunes it into memory.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     mc_rdarll,filename,lines,types,WRANGE=wrange,RES=RES,THRESH=thresh,$
;               CANCEL=cancel
;
; INPUTS:
;     filename - A string containg the full path of a file containing
;                the line list in units of microns.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     RES    - If the resolving power is given, the list is pruned to avoid
;              lines that are too close together.
;     WRANGE - Range of wavelengths in microns to return.
;     THRESH - The number of sigma a to check around a line to see if
;              there is a "blended" line
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     lines - An array of the lines which conform to the keywords. 
;     types - An array of the line types.
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
;     The lines are read in and then pruned based on the resolution
;     and wavelength range
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2008-08-01 - Written by M. Cushing, Institute for Astronomy, UH
;-
pro mc_rdohll,filename,lines,WRANGE=wrange,RES=res,THRESH=thresh,$
              CANCEL=cancel

cancel = 0

;  Check parameters

if n_params() lt 1 then begin
    
    print, 'Syntax - mc_rdohll,filename,lines,WRANGE=wrange,RES=res,$'
    print, '                   THRESH=thresh,CANCEL=cancel'
    cancel = 1
    return

endif
cancel = mc_cpar('mc_rdohll',filename,1,'Filename',7, 0)
if cancel then return

;  Read entire file.

readcol,filename,lines,FORMAT='D',/SILENT

;  Prune based on the WRANGE

if n_elements(WRANGE) ne 0 then begin

    z = where(lines ge wrange[0] and lines le wrange[1])
    lines = lines[z]

endif

;  Prune the line list based on the resolution R.

if n_elements(RES) ne 0 then begin

    if n_elements(THRESH) eq 0 then thresh = 1.

    nlines  = n_elements(lines)
    goodbad = intarr(nlines)+1

    for i = 1,nlines-2 do begin

        delta = lines[i]/float(RES)
        z = where(lines ge lines[i]-thresh*delta/2.534 and $
                  lines le lines[i]+thresh*delta/2.534,count)
        if count gt 1 then goodbad[z] = 0

    endfor
    good = where(goodbad eq 1)
    lines = lines[good]

endif 

end


