;+
; NAME:
;     mc_sumextspec1d
;
; PURPOSE:
;     To preform a sum extraction in 1D.
;
; CALLING SEQUENCE:
;     result = mc_sumextspec1D(img,var,omask,orders,wavecal,spatcal,apsign, $
;                              TRACECOEFFS=tracecoeffs,APRADII=apradii, $
;                              APRANGES=apranges,UPDATE=update, $
;                              WIDGET_ID=widget_id,CANCEL=cancel)
;
; INPUTS:
;     img     - A 2D image.
;     var     - A 2D variance image.
;     omask   - A 2D array where each pixel value is set to the 
;               order number.
;     orders  - An norders array 
;     wavecal - A 2D image where the value of each pixel is its
;               wavelength.
;     spatcal - A 2D image where the value of each pixel is its
;               position along the slit (spatial coordinate).
;     apsign  - Array of 1s and -1s indicating which apertures
;               are positive and which are negative (for IR pair
;               subtraction). 
;
; OPTIONAL INPUTS:
;     See keywords
;
; KEYWORD PARAMETERS:
;     TRACECOEFFS - Array [fitdegree+1,naps*norders] of polynomial 
;                   coefficients of the traces of the apertures.
;                   The coefficients should be indexed starting with 
;                   the bottom order and looping through the apertures
;                   in that order.
;     APRADII     - An [naps,norders] array giving the aperture radius
;                   for each aperture.
;     APRANGES    - An [naps*2,norders] array giving the left and
;                   right edges of the apertures.
;     UPDATE      - If set, the program will launch the Fanning
;                   showprogress widget.
;     WIDGET_ID   - If given, a cancel button is added to the Fanning
;                   showprogress routine.  The widget blocks the
;                   WIDGET_ID, and checks for for a user cancel
;                   command.
;     CANCEL      - Set on return if there is a problem.
;
; OUTPUTS:
;     
;
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     None
;
; RESTRICTIONS:
;     Lots
;
; DEPENDENCIES:
;     
;
; PROCEDURE:
;
;
; EXAMPLES:
;
;
; MODIFICATION HISTORY:
;     2009-12-18 - Written by M. Cushing, NASA JPL
;-
function mc_sumextspec1d,img,var,omask,orders,wavecal,spatcal,apsign, $
                         TRACECOEFFS=tracecoeffs,APRADII=apradii, $
                         APRANGES=apranges,UPDATE=update, $
                         WIDGET_ID=widget_id,CANCEL=cancel
  
  cancel = 0

;  Get necessary info

  norders = n_elements(orders)

  if keyword_set(TRACECOEFFS) then naps = n_elements(APRADII[*,0])

  if keyword_set(APRANGES) then naps = n_elements(APRANGES[*,0])/2

  s     = size(img,/DIMEN)
  ncols = s[0]
  nrows = s[1]

  l = 0

  if keyword_set(UPDATE) then begin

     if keyword_set(WIDGET_ID) then begin

        cancelbutton = (n_elements(WIDGET_ID) ne 0) ? 1:0
        progressbar = obj_new('SHOWPROGRESS',widget_id,COLOR=2,$
                              CANCELBUTTON=cancelbutton,$
                              MESSAGE='Extracting Spectra')
        progressbar -> start
     
     endif

  endif

;  Loop over each order

  for i = 0,norders-1 do begin

     z = where(omask eq orders[i])

     xrange = [min(z mod ncols,MAX=max),max]
     nx     = xrange[1]-xrange[0]+1

     owave  = fltarr(nx)*!values.f_nan
     ofspec = fltarr(nx)*!values.f_nan
     ovspec = fltarr(nx)*!values.f_nan

     for j = 0,naps-1 do begin
        
        for k = xrange[0],xrange[1] do begin

           colmsk = reform(omask[k,*])
           z = where(colmsk eq orders[i],cnt)
           
           slitimg  = reform(img[k,z])
           slitvar  = reform(var[k,z])
           slitspat = reform(spatcal[k,z])
           slitwave = reform(wavecal[k,z])
           slitidx  = findgen(cnt)
           apmask   = intarr(cnt)

;           plot,slitspat,slitimg,/XSTY,/YSTY,TITLE=k

           if n_elements(APRANGES) ne 0 then begin
              
              aprange = [apranges[j*2,i],apranges[j*2+1,i]]
              
           endif else begin
              
              cen = poly(slitspat[cnt/2],tracecoeffs[*,i*naps+j])
              aprange = [cen-apradii[j,i],cen+apradii[j,i]]
              
           endelse

           tabinv,slitspat,aprange,apidx

           apmask[apidx[0]:apidx[1]] = 1
           
;  Fix endpoints to reflect fractional pixels.
     
           if apidx[0]-floor(apidx[0]) ge 0.5 then begin
              
              apmask[apidx[0]]   = 0
              apmask[apidx[0]+1] = (0.5 + round(apidx[0])-apidx[0]) 
              
           endif else begin
              
              apmask[apidx[0]] = (0.5 - (apidx[0]-floor(apidx[0]) ) ) 
              
           endelse

           if apidx[1]-floor(apidx[1]) ge 0.5 then begin
              
              apmask[apidx[1]+1] = (0.5 - (round(apidx[1])-apidx[1]) ) 
              
              
           endif else begin
              
              apmask[apidx[1]] = ( 0.5 + (apidx[1]-round(apidx[1])) ) 
              
           endelse
           
;  Sum the pixels and create the spectrum
           
           z = where(apmask gt 0)
           owave[k-xrange[0]]  = slitwave[cnt/2]       
           ofspec[k-xrange[0]] = total(slitimg[z])
           ovspec[k-xrange[0]] = total(slitvar[z])
                      
        endfor
        
;  Store the results

        name = 'ORD'+string(orders[i],FORMAT='(I2.2)')+ $
               'AP'+string(j+1,FORMAT='(I2.2)')

        ofspec = float(apsign[j])*temporary(ofspec)
        
        struc = (l eq 0) ? create_struct(name,[[owave],[ofspec],[ovspec]]):$
                create_struct(struc,name,[[owave],[ofspec],[ovspec]])

        l = l + 1
             
     endfor
     
;  Do the update stuff

     if keyword_set(UPDATE) then begin
        
        if keyword_set(WIDGET_ID) then begin
           
           if cancelbutton then begin
              
              cancel = progressBar->CheckCancel()
              if cancel then begin
                 
                 progressBar->Destroy
                 obj_destroy, progressbar
                 cancel = 1
                 return, -1
                 
              endif
              
           endif
           percent = (i+1)*(100./float(norders))
           progressbar->update, percent
           
        endif else begin
           
           if norders gt 1 then mc_loopprogress,i,0,norders-1
           
        endelse
        
     endif
          
  endfor
  
  if keyword_set(UPDATE) and keyword_set(WIDGET_ID) then begin
     
     progressbar-> destroy
     obj_destroy, progressbar
     
  endif

  return, struc

end
