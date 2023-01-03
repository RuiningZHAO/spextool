;+
; NAME:
;     mc_mixorders
;
; PURPOSE:
;     Concatinates an arc image and sky image to create a super-arc.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = mc_mixorders(image1,image2,omask,orders,orders1,orders2,$
;                           CANCEL=cancel)
;
; INPUTS:
;     image1     - Image 1
;     image2     - Image 2
;     order      - An order mask where pixels in an order are set to
;                  the order number.
;     orders     - An array [norders] of the order numbers
;     orders1    - The orders to take from image1
;     orders2    - The orders to take from image2
;     
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     Returns and image with orders1 and orders2.  The rest of the
;     image is set to 1.
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
;     Uses the omask to extract orders1 from image1 and order2 
;     from image2 and create the returned image.
;
; EXAMPLE:
;     arc = mc_mixorders(arc,sky,edgecoeffs,omask,orders,$
;                      [5,7,8,9,10],[6])
;
; MODIFICATION HISTORY:
;     2001-02-25 - written M. Cushing, Institute for Astronomy, UH
;     2001-10-11 - Added xranges input and removed start and stop
;                  inputs
;     2015-01-13 - Replaced edgecoeffs and xranges with omask
;-
function mc_mixorders,image1,image2,omask,orders,orders1,orders2, $
                      BACKGROUND=background,CANCEL=cancel

  cancel = 0
  
;  Check parameters
  
  if n_params() lt 6 then begin
     
     print, 'Syntax - result = mc_mixorders(image1,image2,omask,orders,orders1,$'
     print, '                               orders2,CANCEL=cancel'
     cancel = 1
     return, -1
     
  endif
  cancel = mc_cpar('mc_mixorders', image1,1,'Image1',[1,2,3,4,5],2)
  if cancel then return,-1
  cancel = mc_cpar('mc_mixorders', image2,2,'Image2',[1,2,3,4,5],2)
  if cancel then return,-1
  cancel = mc_cpar('mc_mixorders', omask,3,'Omask',[2,3,4,5],2)
  if cancel then return,-1
  cancel = mc_cpar('mc_mixorders', orders,4,'Orders',[2,3,4,5],[0,1])
  if cancel then return,-1
  cancel = mc_cpar('mc_mixorders', orders1,5,'Orders1',[2,3,4,5],[0,1])
  if cancel then return,-1
  cancel = mc_cpar('mc_mixorders', orders2,6,'Orders2',[2,3,4,5],[0,1]) 
  if cancel then return,-1
  
;  Get setup info
  
  norders = n_elements(orders)

  s = size(image1,/DIMEN)
  ncols = s[0]
  nrows = s[1]

;  Make sure input arrays are of the same type.

  if size(image1,/TYPE) ne size(image2,/TYPE) then begin

     print
     print, 'Images are not of the same type.'
     print
     cancel = 1
     return, -1
     
  endif
  
  concat = make_array(ncols,nrows,TYPE=size(image1,/TYPE))
  
  for i = 0, norders-1 do begin

;  Figure out which image to take from.
     
     z = where(orders1 eq orders[i],count1)
     z = where(orders2 eq orders[i],count2)

;  Now do the swap

     z = where(omask eq orders[i])
     
     if count1 ne 0 then begin

        concat[z] = image1[z]

     endif

     if count2 ne 0 then begin
        
        concat[z] = image2[z]
        
     endif 
            
  endfor
  
  return, concat
  
end
