;+
; NAME:
;     mc_addhdrinfo
;
; PURPOSE:
;     To add additional keywords to the hdrinfo structure.
;
; CATEGORY:
;     FITS 
;
; CALLING SEQUENCE:
;     result = mc_addhdrinfo(hdrinfo,keyword,value,[comment],CANCEL=cancel)
;
; INPUTS:
;     hdrinfo - A hdrinfo structure obtained from the gethdrinfo
;               program.
;     keyword - A string giving a tag name.
;     value   - The value
;
; OPTIONAL INPUTS:
;     comment - An optional comment.
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem.
;
; OUTPUTS:
;     A new hdrinfo structure with the keyword added.
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
;     Requires Spextool.
;
; PROCEDURE:
;     Duh.
;
; EXAMPLE:
;     result = mc_addhdrinfo(hdrinfo,'test',1,' A test value')
;     print, result.vals.test -> 1
;     print, result.coms.test -> A test value
;
; MODIFICATION HISTORY:
;     2008-02-28 - Written by M. Cushing, Institute for Astronomy,
;                  University of Hawaii
;-
function mc_addhdrinfo,hdrinfo,keyword,value,comment,CANCEL=cancel

  cancel = 0

  if n_params() lt 3 then begin

     print, 'Syntax - result = mc_addhdrinfo(hdrinfo,keyword,value,[comment],$'
     print, '                                CANCEL=cancel)'
     cancel = 1
     return, -1

  endif
  cancel = mc_cpar('mc_addhdrinfo',hdrinfo,1,'Hdrinfo',8,[0,1])
  if cancel then return,-1
  cancel = mc_cpar('mc_addhdrinfo',keyword,2,'Keyword',7,0)
  if cancel then return,-1
  cancel = mc_cpar('mc_addhdrinfo',value,3,'Keyword',[0,1,2,3,4,5,6,7],0)
  if cancel then return,-1
  cancel = mc_cpar('mc_addhdrinfo',value,3,'Keyword',[0,1,2,3,4,5,6,7],0)
  if cancel then return,-1
  if n_params() eq 4 then begin
     
     cancel = mc_cpar('mc_addhdrinfo',comment,4,'Comment',7,0)
     if cancel then return,-1
     
  endif else comment = ' '

  vals = hdrinfo.vals
  coms = hdrinfo.coms

  vals = create_struct(vals,keyword,value)
  coms = create_struct(coms,keyword,comment)

  return, {vals:vals,coms:coms}

end
