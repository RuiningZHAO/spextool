;+
; NAME:
;     mc_bytsclimg
;
; PURPOSE:
;     To byte scale an image using the requested method.
;
; CATEGORY:
;     Imaging
;
; CALLING SEQUENCE:
;     result = mc_bytsclimg(image,type,offset,MIN=min,MAX=max,CANCEL=cancel)
;
; INPUTS:
;     image - A 2D image.
;     type  - The requested scaling type.
;             0 - Linear
;             1 - Sqrt
;             2 - Squared
;             3 - Log
;             4 - Hist Eq
;             5 - Asinh
;     cbot  - The index value of the first value in the color table
;             to use.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     MIN    - The value of the image that will correspond to the
;              bottom of the color table.
;     MAX    - The value of the image that will correspond to the
;              top of the color table.
;     CANCEL - Set on return if there is a problem.
;
; OUTPUTS:
;     Returns a byte-scaled image that can be displayed with tv.
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
;     Later
;
; MODIFICATION HISTORY:
;     2009-05-06 - Written by M. Cushing, Institute for Astronomy,
;                  University of Hawaii
;-
function mc_bytsclimg,image,type,cbot,MIN=min,MAX=max,CANCEL=cancel

  cancel = 0
  
  if n_params() ne 3 then begin

     print
     print, 'Syntax - mc_bytsclimg,image,type,offset,MIN=min,MAX=max,$'
     print, '                      CANCEL=cancel'
     print
     cancel = 1
     return,-1

  endif

  cancel = mc_cpar('mc_bytsclimg',image,1,'Image',[2,3,4,5],2)
  if cancel then return, -1
  cancel = mc_cpar('mc_bytsclimg',type,2,'Type',[1,2,3,4],0)
  if cancel then return, -1
  cancel = mc_cpar('mc_bytsclimg',cbot,3,'Cbot',[1,2,3,4],0)
  if cancel then return, -1

  if n_elements(MIN) eq 0 then min = min(image,/NAN)
  if n_elements(MAX) eq 0 then max = max(image,/NAN)
  
  
  ncolors = !d.n_colors < 255
  ncolors = ncolors-cbot
  except = !except
  !except = 0  

  case type of 

     0: begin  ;  Linear

        img = bytscl(image,/NAN,MIN=min,MAX=max,TOP=ncolors-1) + cbot

     end

     1: begin  ; Sqrt
        
        offset = -1*min
        fmin = (min+offset)^0.5
        fmax = (max+offset)^0.5
        img = bytscl((image+offset)^0.5,/NAN, $
                     MIN=fmin,MAX=fmax,TOP=ncolors-1) + cbot

     end
     
     2: begin  ;  Squared
        
        offset = -1*min
        fmin = (min+offset)^2
        fmax = (max+offset)^2
        
        img = bytscl((image+offset)^2,/NAN, $
                     MIN=fmin,MAX=fmax,TOP=ncolors-1) + cbot

     end

     3: begin  ;  Log
        
        offset = (max-min)*0.01-min
        
        fmin = alog10(min+offset)
        fmax = alog10(max+offset)
        
        img = bytscl(alog10(img+offset),MIN=fmin,MAX=fmax,/NAN,TOP=ncolors-1)+ $
              cbot

     end

     4: begin  ;  Hist Eq
        
        img = bytscl(hist_equal(image,MINV=min,MAXV=max),/NAN,TOP=ncolors-1)+ $
              cbot
        
     end

     5: begin  ; Asinh
        
;  Project against negative numbers
        
        offset = -1*min
        fmin = asinh((min+offset)/beta)
        fmax = asinh((max+offset)/beta)
        
        img = bytscl(asinh((image-min)/state.beta),/NAN,MIN=fmin,MAX=fmax, $
                     TOP=ncolors-1) + cbot
        
     end
     
     else: begin

        print
        print, 'Unknown scale code.'
        print, '0 - Linear'
        print, '1 - Sqrt'
        print, '2 - Squared'
        print, '3 - Log'
        print, '4 - Hist Eq'
        print, '5 - Asinh'
        print
        cancel = 1
        return,-1

     end
        
  endcase

  junk = check_math()
  !except = except
  
  return,img
  
end
