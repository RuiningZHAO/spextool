;+
; NAME:
;     mc_getwunits
;
; PURPOSE:
;     To return strings for the xunits of spectra

; CATEGORY:
;     Plotting
;
; CALLING SEQUENCE:
;     mc_getpwunits,type,wunits,pwunits,CANCEL=cancel
;
; INPUTS:
;     type - The input wavelength unit
;            0 = microns
;            1 = nanometers
;            2 = Angstroms 
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     wunits  - A string with the units in readable form
;     pwunits - A string that can be passed to XTITLE 
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
;     IDL> mc_getfunits,3,wunit,pwunit,CANCEL=cancel
;     IDL> print, wunit,'  ', pwunit
;     A  !7k!5 (!5!sA!r!u!9 %!5!n)
;     IDL> 
;
; MODIFICATION HISTORY:
;     2005-03-10 - Written by M. Cushing, Steward Observatory,
;                  University of Arizona
;-
pro mc_getwunits,type,wunit,pwunit,CANCEL=cancel

cancel = 0

if n_params() lt 1 then begin

    print, 'Syntax - mc_getwunits,type,wunit,pwunit,CANCEL=cancel'
    cancel = 1
    return

endif
cancel = mc_cpar('mc_getwunits',type,'Type',1,[1,2,3,4,5],0)
if cancel then return

case type of 

    0: begin

        wunit  = 'um'
        pwunit = '!7k!5 (!7l!5m)'
 
    end

    1: begin

        wunit  = 'nm'
        pwunit = '!7k!5 (nm)'
 
    end

    2: begin

        wunit  = 'A'
        pwunit = '!7k!5 (!5!sA!r!u!9 %!5!n)'
 
    end

    else: begin

        wunit  = ''
        pwunit = ''
 
    end

endcase

end

