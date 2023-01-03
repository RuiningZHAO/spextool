;+
; NAME:
;     mc_getfunits
;
; PURPOSE:
;     To return strings for the flux units of spectra

; CATEGORY:
;     Plotting
;
; CALLING SEQUENCE:
;     result - mc_getfunits(type,funits,pfunits,CANCEL=cancel)
;
; INPUTS:
;     type - The input flux density unit
;             0 = W m-2 um-1
;             1 = ergs s-1 cm-2 A-1
;             2 = W m-2 Hz-1
;             3 = ergs s-1 cm-2 Hz-1
;             4 = Jy      
;             5 = W m-2
;             6 = ergs s-1 cm-2 A-1
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     funits  - A string with the units in readable form
;     pfunits - A string that can be passed to YTITLE 
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
;
;
; MODIFICATION HISTORY:
;     2005-03-10 - Written by M. Cushing, Steward Observatory,
;                  University of Arizona
;     2010-01-13 - Turned into a function, added PS and EXP keywords.
;-
function mc_getfunits,type,PS=ps,EXP=exp,UNITS=units,CANCEL=cancel

  cancel = 0

  if n_params() lt 1 then begin
     
     print, 'Syntax - result = mc_getfunits(type,PS=ps,EXP=exp,UNITS=units,$'
     print, '                               CANCEL=cancel)'
     cancel = 1
     return,-1
     
  endif
  cancel = mc_cpar('mc_getfunits',type,'Type',1,[1,2,3,4,5],0)
  if cancel then return,-1

  tmp = keyword_set(EXP) ? '10!U'+string(exp,FORMAT='(i3)')+'!N ':''

  tang = '!5!sA!r!u!9 %!5!n'
  case type of 
     
     0: begin
        
        units  = 'W m-2 um-1'
        funit = '!5f!D!7k!N!5 ('+tmp+'W m!U-2!N !7l!5m!U-1!N)'
        psfunit = 'f!D!9l!N!X ('+tmp+'W m!U-2!N !9m!Xm!U-1!N)'
        
     end
     
     1: begin
        
        units  = 'ergs s-1 cm-2 A-1'
        funit = '!5f!D!7k!N!5 ('+tmp+'ergs s!U-1!N cm!U-2!N '+tang+'!U-1!N)'
        psfunit = 'f!D!9l!N!X ('+tmp+'ergs s!U-1!N cm!U-2!N '+string(197B)+ $
                  '!U-1!N)'
        
     end
     
     2: begin
        
        units  = 'W m-2 Hz-1'
        funit = '!5f!D!7m!N!5 ('+tmp+'W m!U-2!N Hz!U-1!N)'
        psfunit = 'f!D!9n!N!X ('+tmp+'W m!U-2!N Hz!U-1!N)'
        
     end
     
     3: begin
        
        units  = 'ergs s-1 cm-2 Hz-1'
        funit = '!5f!D!7m!N!5 ('+tmp+'ergs s!U-1!N cm!U-2!N Hz!U-1!N)'
        psfunit = 'f!D!9n!N!X ('+tmp+'ergs s!U-1!N cm!U-2!N Hz!U-1!N)'
        
     end
     
     4: begin
        
        units  = 'Jy'
        funit = '!5f!D!7m!N!5 ('+tmp+'Jy)'
        psfunit = 'f!D!9n!N!X ('+tmp+'Jy)'
        
     end
     
     
     5: begin
        
        units  = 'W m-2'
        funit = '!5f ('+tmp+'W m!U-2!N)'
        psfunit = 'f ('+tmp+'W m!U-2!N)'
        
     end
     
     
     6: begin
        
        units  = 'ergs s-1 cm-2'
        funit = '!5f ('+tmp+'ergs s!U-1!N cm!U-2!N)'
        psfunit = 'f ('+tmp+'ergs s!U-1!N cm!U-2!N)'
        
     end
     
     else: begin
        
        units  = ''
        funit = ''
        
     end
     
  endcase

  if keyword_set(PS) then return, psfunit else return, funit
  
end

