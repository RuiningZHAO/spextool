;+
; NAME:
;     mc_scalespecsky
;
; PURPOSE:
;     Scales spectral sky images to the same sky level.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = mc_scalespecsky(data,var,omask,orders,UPDATE=update,$
;                              WIDGEt_ID=widget_id,CANCEL=cancel)
;
; INPUTS:
;     data   - 3-D Data cube of sky images.
;     var    - The variance cube
;     omask   - A 2D array where each pixel value is set to the 
;               order number.
;     orders - A 1D vector giving the order numbers.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     The data cube where individual orders in each image are scaled
;     to the median sky background level.
;
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     NOne
;
; SIDE EFFECTS:
;     None
;
; RESTRICTIONS:
;     None
;
; PROCEDURE:
;     The background level is then calculated
;     for each image.  The median background level is determined for
;     the order and then every order is scaled to make its background
;     level equal to the median background level of orders.  The
;     errors are then propagated.
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2000-08-25 - Written by M. Cushing, Institute for Astronomy, UH
;     2001-10-02 - Added the xranges input
;     2015-01-05 - Replaced edgecoeffs and xranges inputs with omask
;                  and orders.
;-
pro mc_scalespecsky,data,var,omask,orders,UPDATE=update,WIDGET_ID=widget_id, $
                    CANCEL=cancel

  cancel = 0
  
;  Check parameters
  
  if n_params() lt 4 then begin
     
     cancel = 1
     print, 'Syntax - mc_scalespecsky,data,var,omask,orders,$'
     print, '                         UPDATE=update,WIDGEt_ID=widget_id,$'
     print, '                         CANCEL=cancel'
     return
     
  endif
  
  cancel = mc_cpar('mc_scalespecsky',data,1,'Data',[2,3,4,5],3)
  if cancel then return
  cancel = mc_cpar('mc_scalespecsky',var,2,'Var',[2,3,4,5],3)
  if cancel then return
  cancel = mc_cpar('mc_scalespecsky',omask,3,'Omask',[2,3,4,5],2)
  if cancel then return
    cancel = mc_cpar('mc_scalespecsky',orders,4,'orders',[2,3,4,5],1)
  if cancel then return
  
;  Get sizes

  
  s       = size(data)
  ncols   = s[1]
  nrows   = s[2]
  nimages = s[3]
  norders = n_elements(orders)

  
;  Set up the Fanning showprogress object if requested.
  
  if keyword_set(UPDATE) then begin
     cancelbutton = (n_elements(WIDGET_ID) ne 0) ? 1:0
     progressbar = obj_new('SHOWPROGRESS',widget_id,color=2,$
                           CANCELBUTTON=cancelbutton,title='Spextool',$
                           MESSAGE='Scaling the Images...')
     progressbar -> start
     
  endif
  
  for i = 0, norders-1 do begin

     stat = fltarr(nimages)
     z    = where(omask eq orders[i])
     for j = 0, nimages-1 do begin
        
        tmp_image = reform(data[*,*,j])
        stat[j] = median(tmp_image[z],/EVEN)

     endfor
     med   = median(stat,/EVEN)
     scale = med/stat

     z = where(finite(scale) eq 0,cnt)
     if cnt ne 0 then scale[z] = 1.0
     
     for j = 0, nimages-1 do begin
        
        tmp_image = reform(data[*,*,j])
        tmp_image[z] = tmp_image[z]*scale[j]
        data[*,*,j] = tmp_image
        tmp_image = reform(var[*,*,j])
        tmp_image[z] = tmp_image[z]*scale[j]^2
        var[*,*,j] = tmp_image
        
     endfor
     
     if keyword_set(UPDATE) then begin
        
        if cancelbutton then begin
           
           cancel = progressBar->CheckCancel()
           if cancel then begin
              
              progressBar->Destroy
              obj_destroy, progressbar
              cancel = 1
              return
              
           endif
           
        endif
        percent = (i+1)*(100./float(norders))
        progressbar->update, percent
        
     endif
     
  endfor
  
  if keyword_set(UPDATE) then begin
     
     progressbar-> destroy
     obj_destroy, progressbar
     
  endif

end
