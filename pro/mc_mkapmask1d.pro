;+
; NAME:
;     mc_mkapmask1d
;
; PURPOSE:
;     Constructs an 1D aperture mask.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = mc_mkapmask1d(slit,positions,apradius,PSBGINFO=bginfo,$
;                            XSBGINFO=xsbginfo,WIDGET_ID=widget_id,$
;                            CANCEL=cancel)
;
; INPUTS: 
;     slit       - 1D array of slit position values (e.g. pixels, arcseconds) 
;     positions  - Array of apertures positions in units of slit 
;     apwidth    - Aperture radius in units of slit
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     PSBGINFO  - 2 element array [bgstart,bgwidth] giving the radius 
;                 at which to start the background and the width of 
;                 the background in units of slit.
;     XSBGINFO  - (Background Regions) Array of background regions 
;                 in arcseconds ([[1,2],[13-15]]).
;     WIDGET_ID - If given, an pop-up error message will appear over
;                 the widget
;     CANCEL    - Set on return if there is a problem.
;
; OUTPUTS:
;     Returns a 1D mask where:
;        0+ap < value < 1+ap - aperture
;        value = -1          - background
;        0                   - nothing
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
;      First the backgrounds are set to -1 without regard to the
;      apertures.  Then from appos[i]-bgstart to appos[i]+bgstart is         
;      cleared out and set to 0.  Then the apertures are indexed to            
;      apnum.  Finally the edge pixels are set to reflect their
;      fractional pixel values.  To determine where aperture n is, 
;      z = where(mask gt n-1 and mask le n).  If BGWIDTH is not given, 
;      then the background regions are not defined.
;
; EXAMPLE:
;      result = mc_mkapmask(findgen(16),[3,8],0.5)
;
; MODIFICATION HISTORY:
;      2000-08-06 - Written by M. Cushing, Institute for Astronomy, UH
;                   Combined mkmask_ps and mkmask_xs into a single
;                   routine.
;-
function mc_mkapmask1d,slit,positions,apradius,PSBGINFO=psbginfo,$
                       XSBGINFO=xsbginfo,WIDGET_ID=widget_id,$
                       CANCEL=cancel

  cancel = 0

  if n_params() lt 3 then begin
     
     print, 'Syntax - result = mc_mkapmask1d(slit,positions,apradius,$'
     print, '                                PSBGINFO=bginfo,$'
     print, '                                XSBGINFO=xsbginfo,$'
     print, '                                WIDGET_ID=widget_id,CANCEL=cancel)'
     cancel = 1
     return, -1
     
  endif
  cancel = mc_cpar('mc_mkapmask1d',slit,1,'Slit',[2,3,4,5],1)
  if cancel then return, -1
  cancel = mc_cpar('mc_mkapmask1d',positions,2,'Positions',[2,3,4,5],[0,1])
  if cancel then return, -1
  cancel = mc_cpar('mc_mkapmask1d',apradius,3,'Apradius',[2,3,4,5],[0,1])
  if cancel then return, -1
  
  npix = n_elements(slit)
  naps = n_elements(positions)
  mask = fltarr(npix)

  ;  Check to make sure the apertures don't overlap 

  for i = 0, naps-1 do begin
     
     ap  = [(positions[i]-float(apradius[i])),(positions[i]+float(apradius[i]))]
     tabinv, slit, ap, ap_idx
     mask[ap_idx[0]:ap_idx[1]] = mask[ap_idx[0]:ap_idx[1]] + 1
     
  endfor
  z = where(mask gt 1,cnt)
  if cnt gt 0 then begin
     
     result = dialog_message([['The extraction apertures overlap.  Please '],$
                              ['lower the aperture radii.']],/ERROR,$
                             DIALOG_PARENT=widget_id)
     cancel = 1
     return, -1
     
  endif

;  Reset and create mask

  mask[*] = 0
  
;  First determine background regions around each peak.
  
  if n_elements(PSBGINFO) ne 0 then begin
     
     bgstart = psbginfo[0]
     bgwidth = psbginfo[1]
     
     for i = 0, naps-1 do begin
        
        botbg = [(positions[i]-bgstart-bgwidth),(positions[i]-bgstart)]
        topbg = [(positions[i]+bgstart),(positions[i]+bgstart+bgwidth)]
        
        tabinv, slit, botbg, bot_idx
        tabinv, slit, topbg, top_idx
        
        mask[bot_idx[0]:bot_idx[1]] = -1
        mask[top_idx[0]:top_idx[1]] = -1
        
     endfor
     
     for i =0, naps-1 do begin
        
;  Now clear out from -bgstart to bgstart around each peak.
        
        clr = [(positions[i]-bgstart),(positions[i]+bgstart)]
        tabinv, slit, clr, clr_idx
        mask[clr_idx[0]:clr_idx[1]] = 0
        
     endfor
     
  endif    

  if n_elements(XSBGINFO) ne 0 then begin
     
     s = size(XSBGINFO)
     nbg = (s[0] eq 1) ? 1:s[2]
     for i = 0, nbg-1 do begin
        
        tabinv, slit, reform(xsbginfo[*,i]), idx
        mask[idx[0]:idx[1]]   = -1
        
     endfor
     
  endif
  
  for i = 0,naps-1 do begin
     
;  Define aperture
     
     ap  = [(positions[i]-float(apradius[i])),(positions[i]+float(apradius[i]))]
     tabinv, slit, ap, ap_idx
     mask[ap_idx[0]:ap_idx[1]] = i+1
     
;  Fix endpoints to reflect fractional pixels.
     
     if ap_idx[0]-floor(ap_idx[0]) ge 0.5 then begin
        
        mask[ap_idx[0]]   = 0
        mask[ap_idx[0]+1] = (0.5 + round(ap_idx[0])-ap_idx[0]) + i
        
     endif else begin
        
        mask[ap_idx[0]] = (0.5 - (ap_idx[0]-floor(ap_idx[0]) ) ) + i
        
     endelse

     if ap_idx[1]-floor(ap_idx[1]) ge 0.5 then begin
        
        mask[ap_idx[1]+1] = (0.5 - (round(ap_idx[1])-ap_idx[1]) ) + i
        
        
     endif else begin
        
        mask[ap_idx[1]] = ( 0.5 + (ap_idx[1]-round(ap_idx[1])) ) + i
        
     endelse
     
  endfor
  
  return, mask


end
