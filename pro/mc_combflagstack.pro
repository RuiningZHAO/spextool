;+
; NAME:
;     mc_combflagstack
;
; PURPOSE:
;     To combine bit-set flag arrays together.
;
; CALLING SEQUENCE:
;     result = mc_combflagspec(stack,[nbits],CANCEL=cancel)
;
; INPUTS:
;     stack - The stack of bit-set flag arrays to combine.  The stack
;             can either be a stack of spectra [ndat,nspec] or a stack
;             of images [nx,ny,nimg].   
;
; OPTIONAL INPUTS:
;     nbits - The number of bits that can potentially be set.  This
;             routine assumes the bits are set sequentially, starting
;             with the zeroth bit.  So if nbits is 2, then it will
;             check the 0th and 1st bit.  The default is to check all
;             eight bits.
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem.
;
; OUTPUTS:
;     A bit-set flag array that reflects the bit-set flags from all of
;     the spectra or images.
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
;     Requires the Spextool library (cpar.pro).
;
; PROCEDURE:
;     Just some math.
;
; EXAMPLES:
;
;
; MODIFICATION HISTORY:
;     2015-01-02 - Written by M. Cushing, University of Toledo
;-
function mc_combflagstack,stack,nbits,CANCEL=cancel
  
  cancel = 0

  if n_params() lt 1 then begin
     
     print, 'Syntax - result = mc_combflagstack(stack,[nbits],CANCEL=cancel)'
     cancel = 1
     return, -1
     
  endif
  cancel = mc_cpar('mc_combflagstack',stack,1,'stack',[1,2,3],[1,2,3])
  if cancel then return,-1

  if n_params() eq 2 then begin

     cancel = mc_cpar('mc_combflagstack',nbits,2,'Nbits',[1,2,3,12,13],0)
     if cancel then return,-1

  endif else nbits = 8
  
  s = size(stack)
  
  case s[0] of

     2: begin

        comb = bytarr(s[1])
        sumdimen = 2
        
     end

     3: begin

        comb = bytarr(s[1],s[2])
        sumdimen = 3
        
     end

  endcase

  for i = 0,nbits-1 do begin

     set = mc_bitset(stack,i,CANCEL=cancel)
     if cancel then return,-1
     comb = comb + (total(set,sumdimen,/PRESERVE) gt 0)*2B^i 

  endfor

  return, comb
  
end
