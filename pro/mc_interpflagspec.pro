;+
; NAME:
;     mc_interpflagspec
;
; PURPOSE:
;     Peforms a linear interpolation on a bit-set flag array.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     mc_interpflagspec,ix,iy,ox,oy,NBITS=nbits,CANCEL=cancel
;
; INPUTS:
;     ix    - The independent values of spectrum
;     iy    - The dependent values of the bit-set flag array.
;     ox    - The new independent values of the bit-set flag array.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     NBITS  - The number of bits to scan through.  The assumption is
;              that they are sequentual starting with the first bit.
;              Defaults to 8.
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     oy - The new dependent values of the bit-set flag array.
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
; DEPENDENCIES:
;     mc_cpar.pro (Spextool)     
;
; PROCEDURE:
;     Later
;
; EXAMPLE:
;    
;
; MODIFICATION HISTORY:
;     Written 2015-01-02 by M. Cushing, University of Toledo
;-
pro mc_interpflagspec,ix,iy,ox,oy,NBITS=nbits,CANCEL=cancel

  cancel = 0
  
  if n_params() lt 4 then begin
     
     print, 'Syntax - mc_interpflagspec,ix,iy,ox,oy,CANCEL=cancel'
     cancel = 1
     return
     
  endif
  cancel = mc_cpar('mc_interpflagspec',ix,1,'IX',[2,3,4,5],1)
  if cancel then return
  cancel = mc_cpar('mc_interpflagspec',iy,2,'IY',[1,2],1)
  if cancel then return
  cancel = mc_cpar('mc_interpflagspec',ox,3,'OX',[2,3,4,5],[0,1])
  if cancel then return

  if n_elements(NBITS) eq 0 then nbits = 8
  
;  Remove NaNs from ix and possibly iy array.

  zci = where(finite(ix) eq 1)
  cix = ix[zci]
  ciy = iy[zci]

;  Remove NaNs from ox array
  
  zco = where(finite(ox) eq 1)
  cox = ox[zco]

;  We are now working with "c"lean ix and ox arrays.

;  Create output "c"lean y and  arrays.

  ndat = n_elements(cox)
  coy = bytarr(ndat)

;  Determine index of cox on cix.
  
  mc_findidx, cix,cox,idx
  
  zgood = where(finite(idx) eq 1, c_idx)   
  
  if c_idx eq 0 then begin
     
     print, ' '
     print, 'Cannot interpolate x on xa.'
     print, ' '
     cancel = 1
     return
     
  endif
  
;  Determine the top and bot index of idx and "t"rim the x array
  
  idx = idx[zgood]
  bot = floor(idx)
  top = ceil(idx)
  
  tcox = cox[zgood]

  for i = 0,nbits-1 do begin

     set = mc_bitset(ciy,i,CANCEL=cancel)
     if cancel then return

     tcoy = fltarr(c_idx)
     
;  See which x points land on xa points
  
     same = where(bot-top eq 0,c_same)
     if c_same ne 0 then tcoy[same] = set[bot[same]]
     
;  Perform interpolation for those points that do not
;  y = y_1 + m * dx where m = (y2-y1)/(x2-x1), dx=x-x1
     
     if c_idx ne c_same then begin
        
        dif       = where(bot-top ne 0,c_dif)
        m         = (set[top[dif]]-set[bot[dif]]) / (cix[top[dif]]-cix[bot[dif]])
        tcoy[dif] = set[bot[dif]]+ m*(tcox[dif]-cix[bot[dif]])
        
     endif
     
     coy[zgood] = coy[zgood] + ceil(tcoy)*2^(i)
     
  endfor
  
;  Now fill back into the full size oy array.

  oy = fltarr(n_elements(ox))
  oy[zco] = coy

  oy = byte(oy)
  
end
