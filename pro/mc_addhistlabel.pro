;+
; NAME:
;     mc_addhistlabel
;
; PURPOSE:
;     To include a easily visible history label in a FITS header
;
; CALLING SEQUENCE:
;     result = mc_addhistlabel(hdr,name,CANCEL=cancel)
;
; INPUTS:
;     hdr   - A FITS header.
;     label - The label to be included.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on retun if there is a problem.
;
; OUTPUTS:
;     A FITS header with the HISTORY label added.
;
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     None
;
; RESTRICTIONS:
;     Must be a FITS header.
;
; DEPENDENCIES:
;     Requires the astronomy User's library and the Spextool library.
;
; PROCEDURE:
;     Just string parsing.
;
; EXAMPLES:
;     nhdr = mc_addhistlabel(ohdr,'Xspextool History',CANCEL=cancel)
;
; MODIFICATION HISTORY:
;     2014-05-29 - Written by M. Cushing, Univesrity of Toledo
;-
function mc_addhistlabel,hdr,label,CANCEL=cancel

  cancel = 0

  if n_params() ne 2 then begin

     print, 'Syntax - result = mc_addhistlabel(hdr,label,CANCEL=cancel)'
     cancel = 1
     return, -1
     
  endif
  
  cancel = mc_cpar('mc_addhistlabel',hdr,'Hdr',1,7,[0,1])
  if cancel then return, -1
  cancel = mc_cpar('mc_addhistlabel',label,'label',2,7,0)
  if cancel then return, -1

  tmp = hdr

  tot = floor((80-(strlen(label)+2))/2)

  string = strjoin(replicate('#',tot))+' '+label+' '+strjoin(replicate('#',tot))

  sxaddhist,' ',tmp,/BLANK
  sxaddhist,string,tmp,/BLANK
  sxaddhist,' ',tmp,/BLANK
  
  return, tmp

end
