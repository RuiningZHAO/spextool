function mc_normspecflat2d,img,var,edgecoeffs,xranges,ybuffer,bkspace,$
                           NPOLY=npoly,NDEG=ndeg,RMS=rms,MODEL=model, $
                           CANCEL=cancel
  
  cancel = 0

;  Determine the size of the image, and number of orders

  s       = size(img)
  ncols   = s[1]
  nrows   = s[2]
  s       = size(edgecoeffs)
  norders = (s[0] eq 2) ? 1:s[3]

;  Initialize arrays.

  nflat = fltarr(ncols,nrows)+1.
  if arg_present(RMS) then rms = fltarr(norders)
  
  x     = rebin(indgen(ncols),ncols,nrows)
  y     = rebin(reform(indgen(nrows),1,nrows),ncols,nrows)
  model = fltarr(ncols,nrows)+1
  
  for i =0,norders-1 do begin
   
;  Create order mask
     
     omask = intarr(ncols,nrows)
     
     xx      = indgen(xranges[1,i]-xranges[0,i]+1)+xranges[0,i]
     botedge = poly1d(xx,edgecoeffs[*,0,i])
     topedge = poly1d(xx,edgecoeffs[*,1,i])
     
     for j = 0,xranges[1,i]-xranges[0,i] do begin
        
        if topedge[j] gt nrows-0.5 or botedge[j] lt -0.5 then continue
        omask[xx[j],botedge[j]:topedge[j]] = 1
        
     endfor
     
;  Do the fit with a bspline
     
     z = where(omask eq 1,cnt)

     sset = bspline_iterfit(float(x[z]),float(img[z]),X2=float(y[z]), $
                            NPOLY=4,BKSPACE=bkspace,NORD=3,$
                            YFIT=yfit,MAX=0)
     help, sset, /STRUC
     print, sset.coeff

     cancel = 1

     return, -1     

     model[z] = yfit
     nflat[z] = img[z]/yfit

     ximgtool,model,nflat
     cancel = 1
     return, -1

     
     if arg_present(RMS) ne 0 then begin
        
        z = where(omask eq 1)
        mc_moments,nflat[z],mean,var,stddev,ROBUST=5,/SILENT,CANCEL=cancel
        if cancel then return,-1
        rms[i] = stddev
        
     endif
     
  endfor
  
;  Protect against zeros for whatever reason
  
  z = where(nflat le 0.0,count)
  if count ne 0 then nflat[z] = 1.0
  
  ximgtool,model
  cancel = 1
  return, -1
;  return, nflat
  
end





