;+
; NAME:
;     mc_nantrim
;
; PURPOSE:
;     To remove NaNs from a spectrum.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = mc_nantrim(array,[flag],CANCEL=cancel)
;
; INPUTS:
;     array - The 1D array to be trimed.
;
; OPTIONAL INPUTS:
;     flag - 0: The trailing NaNs are removed (default).
;            1: The leading NaNs are removed.
;            2: Both leading and trailing NaNs are removed.
;            3: All NaNs are removed.
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem.
;
; OUTPUTS:
;     result - A 1D array giving the index values to the original
;              array that are consistent with the flag.
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
;     NA
;
; EXAMPLE:
;     Let x = [!values.f_nan,2,3,4,!values.f_nan,7,89,90,!values.f_nan]
;
;     x[mc_nantrim(x,0)] = [!values.f_nan,2,3,4,!values.f_nan,7,89,90]
;     x[mc_nantrim(x,1)] = [2,3,4,!values.f_nan,7,89,90,!values.f_nan]
;     x[mc_nantrim(x,2)] = [2,3,4,!values.f_nan,7,89,90]
;     x[mc_nantrim(x,3)] = [2,3,4,7,89,90]
;
; MODIFICATION HISTORY:
;     2007-07-03 - Written by M. Cushing, Steward Observatory,
;                  University of Arizona
;-
function mc_nantrim,array,flag,CANCEL=cancel

  cancel = 0

  if n_params() lt 1 then begin
     
     print, 'Syntax - mc_nantrim(array,[flag],CANCEL=cancel)'
     cancel = 1
     return,-1
     
  endif
  
  cancel = mc_cpar('mc_nantrim',array,'Array',1,[1,2,3,4,5],1)
  if cancel then return,-1
  
  if n_params() eq 2 then begin
     
     cancel = mc_cpar('mc_nantrim',flag,'Flag',2,[1,2,3],0)
     if cancel then return,-1
     
  endif
  
  ndat = n_elements(array)
  nan = where(finite(array) eq 0,cnt)
  if cnt eq 0 then return, findgen(ndat)
  
  if n_elements(flag) eq 0 then flag = 0
  
  case flag of 
     
     0: begin
        
        tmp = reverse(array)
        cum = total(tmp,/NAN,/CUM)
        z = where(cum ne 0,cnt)
        uidx = (ndat-1)-z[0]
        
        return, findgen(uidx+1)
        
     end
     
     1: begin
        
        
        cum = total(array,/NAN,/CUM)
        z = where(cum ne 0,cnt)
        
        return, findgen(ndat-1-z[0]+1)+z[0]
        
     end
     
     2: begin
        
        tmp = reverse(array)
        cum = total(tmp,/NAN,/CUM)
        z = where(cum ne 0,cnt)
        uidx = (ndat-1)-z[0]
        
        cum = total(array,/NAN,/CUM)
        z = where(cum ne 0,cnt)
        
        return, findgen(uidx-z[0]+1)+z[0]
        
     end
     
     3: begin
        
        z = where(finite(array) eq 1)
        return, z
        
     end
     
     else:  begin
        
        print, 'Flag unknown.'
        cancel = 1
        return, -1
        
     endelse
     
  endcase
  
end

