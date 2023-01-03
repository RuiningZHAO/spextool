;+
; NAME:
;     mc_imgrange
;
; PURPOSE:
;     To compute the values of an array that fall within some fraction
;     of the total pixels.
;
; CATEGORY:
;     Imaging
;
; CALLING SEQUENCE:
;     mc_imgrange,img,min,max,ZSCALE=zscale,RANGE=range,CANCEL=cancel
;
; INPUTS:
;     img   - A 2D image.     
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     RANGE  - A range value between 0 and 1.
;     ZSCALE - Set to run IRAF's zscale.
;     CANCEL - Set on return if there is a problem.
;
; OUTPUTS:
;     min - The minimum value corresponding to 1-range of the pixels.
;     max - The minimum value corresponding to range of the pixels.
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
;     
;
; EXAMPLE:
;
;
; MODIFICATION HISTORY:
;        2008-08-24 - Written by M. Cushing, Insititute for Astronomy,
;                     University of Hawaii
;        2011-06-26 - Made range input a keyword and added IRAF ZSCALE
;                     keyword and scaling, M. Cushing, NASA JPL.
;        2013-10-08 - Added the min > and max < catches for zscale as
;                     it was giving values outside of the image
;                     range.  Found this here:
;                     http://stsdas.stsci.edu/stsci_python_epydoc/numdisplay/numdisplay.zscale-pysrc.html#zscale
;-
pro mc_imgrange,img,min,max,RANGE=range,ZSCALE=zscale,CANCEL=cancel

  cancel = 0

  min = min(img,MAX=max,/NAN)

;  Sample the image if it is larger than 500 x 500 pixels.

  ndat = n_elements(img)

  if n_elements(img) gt 500 then begin

     delidx = floor(ndat/500.0)
     
     z = findgen(500)*delidx

     simg = img[z]

  endif else simg = reform(img,ndat)

  if keyword_set(RANGE) then begin
     
     if range eq 1.0 then return
     
     z = where(finite(simg) eq 1,ngood)
     
     if ngood ne 0 then begin
        
;  Get bin size
        
;     med = median(simg[z],/EVEN)
;     mad = 1.4826*median(abs(simg[z]-med), /EVEN)
;
;     binsize = 3.49*mad*ngood^(-1/3.)
;     binsize = 1
;
;     mc_histogram,simg[z],histx,histy,BINSIZE=binsize,CANCEL=cancel
;     if cancel then return
;
;     mc_plothist,histx,histy,/XSTY,/YSTY,XRANGE=[-100,1000]
;
;     histy = histy/int_tabulated(histx,float(histy))
;
;     cummulative = total(histy,/CUM)
;     plot,histx,cummulative,/XSTY,/YSTY,XRANGE=[-100,1000]
;
;     tabinv,cummulative,[1.0-range,range],idx
;
;     min = histx[idx[0]]
;     max = histx[idx[1]]
;
;     plots,[min,min],!y.crange,COLOR=2
;     plots,[max,max],!y.crange,COLOR=2

        vals  = simg[z[sort(simg[z])]]
        idxs = findgen(ngood)/(ngood-1)
        
        tabinv,idxs,1-range,idx
        min = vals[idx]
        
        tabinv,idxs,range,idx
        max = vals[idx]
        
     endif else begin
        
        print, 'All values are NaN.'
        cancel = 1
        return
        
     endelse

  endif

  if keyword_set(ZSCALE) then begin

     z = where(finite(float(simg)) eq 1)
     simg = simg[z]
     nsimg = n_elements(simg)

     smin = min(simg,MAX=smax,/NAN)

     s = sort(simg)
     simg = simg[s]
     mid = nsimg/2
     x = findgen(nsimg)-mid

     coeff = ladfit(x,simg)     

;     window, 2
;     plot,x,simg,/XSTY,/YSTY
;     oplot,x,poly(x,coeff),COLOR=3

     min = smin > (simg[mid]+coeff[1]/0.25*(0-mid))
     max = smax < (simg[mid]+coeff[1]/0.25*(nsimg-mid))

  endif

end
