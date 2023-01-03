;+
; NAME:
;     mc_speccor
;
; PURPOSE:
;     To correct a stack of spectra for shape differences
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = mc_speccor(stack,fwidth,IERRSTACK=ierrstack,
;                         OERRSTACK=oerrstack,SPECMASK=mask,PIXMASK=pixmask,$
;                         REFSPEC=refspec,CORRECTIONS=corrections,$
;                         CANCEL=cancel)
;
; INPUTS:
;     stack  - A [ncols,nspec] array of spectra.  This program assumes
;              that the only difference between the spectra is due to
;              either 1) shapre variations due presumablyn to
;              heterochromatic slit losses and 2), random noise.
;     fwidth - 
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     IERRSTACK   - A [ncols,nspec] array of errors corresponding to
;                   stack.  If given, the corrected error array is given
;                   in OERRSTACK.
;     OERRSTACK   - A [ncols,nspec] array of corrected error spectra
;     SPECMASK    - An [nspec] array denoting which spectra to use to
;                   determine the reference spectrum, 0 - bad,
;                   1 - good.  Note that all the spectra in the stack
;                   are scaled to the reference spectrum.  
;     PIXMASK     - An [ncols,nspec] array of giving which pixels are
;                   good, 0-bad, 1-good.  In order to compute the
;                   shape correction, all pixels set to 0 will be
;                   replaced with the scaled median of the stack.  
;     REFSPEC     - A reference spectrum [ncols] that the spectra in
;                   stack are corrected to.

;
; OUTPUTS:
;     Returns a [ncols,nspec] array of spectra corrected to the
;     reference spectrum
;
; OPTIONAL OUTPUTS:
;     CORRECTIONS - An [ncols,nspec] array of corrections scale factors.
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
;     Later
;
; EXAMPLE:
;     Later
;
; MODIFICATION HISTORY:
;     2005-10-12 - Written by Bill Vacca, SOFIA, NASA Ames Research
;                  Center
;     2014-12-01 - Modifed to deal with large groups of bad pixels.
;-
function mc_speccor,stack,fwidth,IERRSTACK=ierrstack,OERRSTACK=oerrstack, $
                    SPECMASK=specmask,PIXMASK=pixmask,REFSPEC=irefspec,$
                    CORRECTIONS=corrections,CANCEL=cancel
  
  cancel = 0
  
  if n_params() ne 2 then begin
     
     print, 'Syntax - result = mc_speccor(stack,fwidth,IERRSTACK=ierrstack,$'
     print, '                          OERRSTACK=oerrstack,MASK=mask,$'
     print, '                          REFSPEC=irefspec,$'
     print, '                          CANCEL=cancel)'
     cancel = 1
     return, -1
     
  endif
  
  cancel = mc_cpar('mc_speccor',stack,'Stack',1,[2,3,4,5],2)
  if cancel then return,-1
  cancel = mc_cpar('mc_speccor',fwidth,'Fwidth',2,[2,3,4,5],0)
  if cancel then return,-1
  
  if n_elements(IERRSTACK) ne 0 then begin
     
     cancel = mc_cpar('mc_speccor',ierrstack,'IERRSTACK',3,[2,3,4,5],2)
     if cancel then return,-1
     
  endif
  
  if n_elements(SPECMASK) ne 0 then begin
     
     cancel = mc_cpar('mc_speccor',specmask,'SPECMASK',4,[2,3],1)
     if cancel then return,-1
     
  endif 

  if n_elements(PIXMASK) ne 0 then begin
     
     cancel = mc_cpar('mc_speccor',pixmask,'PIXMASK',4,[2,3],2)
     if cancel then return,-1
     
  endif 
  
;  Get the number of columns and the number of spectra in the stack.

  s = size(stack,/DIMEN)
  ncols = s[0]
  nspec = s[1]

;  Do we do errors?

  doerr = (n_elements(IERRSTACK) ne 0) ? 1:0
  if doerr then oerrstack = ierrstack

;  Load blank masks if necessary

  specmask = (n_elements(SPECMASK) eq 0) ?  fltarr(nspec)+1:float(specmask)
  pixmask  = (n_elements(PIXMASK) eq 0) ?  fltarr(ncols,nspec)+1:float(pixmask)

;  Find outliers in the stack.

  z = where(specmask eq 1)

  mc_meancomb,stack[*,z],meanspec,MASK=pixmask,OGOODBAD=npixmask,ROBUST=3, $
              /SILENT,CANCEL=cancel

;  Now replace those outliers with the values of the mean spectrum.

  tstack = stack
  for i = 0,nspec-1 do begin

     if ~specmask[i] then continue
     
     bad = where(npixmask[*,i] eq 0,cnt)
     if cnt ne 0 then tstack[bad,i] = meanspec[bad]

  endfor
    
;  Smooth to remove any missed bad pixels that screw with the FFT

  x = findgen(ncols)
  for i =0,nspec-1 do begin
     
     test = reform((mc_robustsg(x,reform(tstack[*,i]),10,5,0.1))[*,1])    
     tstack[*,i] = test
          
  endfor

;  Create filter
  
  y = (ncols mod 2) ? [findgen(ncols/2+1),reverse(findgen(ncols/2))]:$
      [findgen(ncols/2),reverse(findgen(ncols/2))]
  filter = 1.0/(1+(y/fwidth)^10)
  
;  Get shape reference spectrum
  
  if n_elements(REFSPEC) ne 0 then begin
     
     cancel = mc_cpar('mc_speccor',irefspec,'refspec',6,[2,3,4,5],1)
     if cancel then return,-1    
     if n_elements(refspec) ne ncols then begin
        
        cancel = 1
        return, -1
        
     endif
     refspec = irefspec[goodspec]
     
  endif else refspec = meanspec
 
  goodpix = where(finite(meanspec) eq 1)

;  Filter reference spectrum
  
  ref_lofreq = fft(fft(refspec[goodpix])*filter,/INVERSE)
   
;  Correct each spectrum
  
  corrections = fltarr(ncols,nspec)
  cstack = fltarr(ncols,nspec)

  for i =0,nspec-1 do begin
     
     lofreq = fft(fft(tstack[goodpix,i])*filter,/INVERSE)
     cstack[goodpix,i] = reform(stack[goodpix,i]*(ref_lofreq/lofreq))
     corrections[goodpix,i] = ref_lofreq/lofreq
     
     if doerr then oerrstack[goodpix,i] = ierrstack[goodpix,i] $
                                          *(ref_lofreq/lofreq)
     
  endfor

  return, cstack
  


end
