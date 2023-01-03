;+
; NAME:
;     mc_tracetoap2
;
; PURPOSE:
;     To convert a tracecoeff array to an aperture position and x,y
;     positions on the array.
;
; CALLING SEQUENCE:
;     mc_tracetoap2,omask,wavecal,spatcal,tracecoeffs,appos,plotstruc,$
;     CANCEL=cancel
;
; INPUTS:
;
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;
; OUTPUTS:
;
;
; OPTIONAL OUTPUTS:
;
;
; COMMON BLOCKS:
;
;
; RESTRICTIONS:
;
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
;
;-
pro mc_tracetoap2d,omask,wavecal,spatcal,tri,tracecoeffs,naps,orders,doorders, $
                   appos,struc,CANCEL=cancel

  cancel = 0

  cancel = mc_cpar('mc_tracetoap2d',omask,1,'Omask',[2,3,4,5],2)
  if cancel then return
  cancel = mc_cpar('mc_tracetoap2d',wavecal,2,'Wavecal',[2,3,4,5],2)
  if cancel then return
  cancel = mc_cpar('mc_tracetoap2d',spatcal,3,'Spatcal',[2,3,4,5],2)
  if cancel then return
  cancel = mc_cpar('mc_tracetoap2d',tracecoeffs,5,'Tracecoeffs',[2,3,4,5],[1,2])
  if cancel then return
  cancel = mc_cpar('mc_tracetoap2d',orders,6,'Orders',[2,3,4,5],[1])
  if cancel then return
  cancel = mc_cpar('mc_tracetoap2d',naps,7,'Naps',[2,3,4,5],0)
  if cancel then return
  cancel = mc_cpar('mc_tracetoap2d',doorders,8,'doorders',[2,3],0)
  if cancel then return

;  Get size of image

  s = size(omask,/DIMEN)
  ncols = s[0]
  nrows = s[1]

;  Get order numbers

  norders = n_elements(orders)

  appos = make_array(norders,naps,/FLOAT,VALUE=!values.f_nan)

  xx = rebin(indgen(ncols),ncols,nrows)
  yy = rebin(reform(indgen(nrows),1,nrows),ncols,nrows)

  l = 0
  for i = 0,norders-1 do begin

     if doorder[i] eq 0 then continue

     for j = 0,naps-1 do begin

        z = where(omask eq orders[i])
        
        wmax = max(wavecal[z],MIN=wmin)
        smax = max(spatcal[z],MIN=smin)
        xmax = max(xx[z],MIN=xmin)
        
        dw = (wmax-wmin)/(xmax-xmin)
        wave = (findgen(xmax-xmin-1)+1)*dw+wmin

        trace_ap = poly(wave,tracecoeffs[*,l])
        appos[i,j] = mean(trace_ap)

        ix = trigrid(wavecal[z],spatcal[z],xx[z],tri.(i),XOUT=wave,YOUT=yout, $
                     MISSING=!values.f_nan)
        
        iy = trigrid(wavecal[z],spatcal[z],yy[z],tri.(i),XOUT=wave,YOUT=yout, $
                     MISSING=!values.f_nan)
        
        array = [[ix[*,0]],[iy[*,0]]]
        name = 'AP'+string(l+1,FORMAT='(I2.2)')
        
        struc = (l eq 0) ? $
                create_struct(name,array):create_struct(struc,name,array)
        
        l = l + 1        

     endfor

  endfor
  
  

end
