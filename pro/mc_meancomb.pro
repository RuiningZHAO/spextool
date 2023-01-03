;+
; NAME:
;     mc_meancomb
;
; PURPOSE:
;     (Robustly) averages a 1D, 2D, or 3D dataset using a (weighted) mean.
;
; CATEGORY:
;     Image Manipulation
;
; CALLING SEQUENCE:
;     mc_meancomb,data,mean,mvar,MASK=mask,DATAVAR=datavar,NAN=nan,$
;                 ROBUST=robust,RMS=rms,SILENT=silent,UPDATE=update,$
;                 CANCEL=cancel
;
; INPUTS:
;     data - Data array (1, 2 or 3D).
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     MASK       - An optional mask of the same size as data
;                  identifying pixels to use in the combination.
;                  0=bad,1=good
;     DATAVAR    - The variances of the data points.  If given, a
;                  weighted mean is performed.
;     NAN        - Set to ignore NaN values.  Same as passing a MASK
;                  with NaN pixels identified as 0.
;     ROBUST     - Set to the sigma threshold to throw bad data out.
;                  A data point is identified as an outlier if
;                  |x_i - x_med|/MAD > thresh where x_med is the
;                  median and MAD is the median absolute deviation
;                  defined as 1.482*median(|x_i-x_med|).
;     RMS        - Set to return the RMS error instead of the error on
;                  the mean.  This parameter is ignored if DATAVAR are
;                  given.
;     SILENT     - Set to suppress messages.
;     UPDATE     - If set, the program will launch the Fanning
;                  showprogress widget.
;     CANCEL     - Set on return if there is a problem
;    
; OUTPUTS:
;     mean - The mean array.
;     mvar - The "variance of the mean", sigma_mu^2, array.
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
;     If the data is 2D, then the combining happens in the row
;     direction.  Used to combine spectra.
;
; PROCEDURE: 
;     This routine will combine either 1, 2 or 3-D data using
;     either a straight mean or weighted mean.  If DATAVAR are not
;     given, then a straight mean, <x>, and square of the standard
;     error of the mean, sigma_mu^2 = sigma^2/N are computed.  If
;     DATAVAR are given, then a weighted mean and corresponding
;     variance on the mean are computed.
;
; EXAMPLE:
; 
; MODIFICATION HISTORY:
;     2001-04-22 - Written by M. Cushing, Institute for Astronomy, UH
;     2003-04-10 - Changed the 3D weighted average.
;                  Removed Fanning showprogress popup.
;     2003-05-27 - Added the MASK keyword, M. Cushing
;     2004-10-10 - Added the NAN keyword
;     2005-04-08 - Added the RMS keyword
;     2008-01-25 - Added NaN check on the datavar array.
;     2009-06-24 - Changed name to mc_meancomb
;     2011-04-04 - Added the ROBUST and SILENT keyword.  
;     2014-12-02 - Added OGOODBAD keyword.
;     2015-01-07 - Added UPDATE keyword.
;     2015-07-05 - Fixed a bug whereby the the NaNs were not being
;                  ignored in the weighted 2D and 3D cases because
;                  NaN*0 = NaN, not 0.
;-
pro mc_meancomb,data,mean,mvar,MASK=mask,DATAVAR=datavar,NAN=nan,$
                ROBUST=robust,RMS=rms,OGOODBAD=ogoodbad,$
                UPDATE=update,SILENT=silent, CANCEL=cancel
  
  cancel = 0
  
;  Check parameters
  
  if n_params() lt 1 then begin
     
     print, 'Syntax - mc_meancomb,data,mean,mvar,MASK=mask,DATAVAR=datavar,$'
     print, '                     NAN=nan,ROBUST=robust,RMS=rms,$'
     print, '                     OGOODBAD=ogoodbad,SILENT=silent,$'
     print, '                     CANCEL=cancel'
     cancel = 1
     return
     
  endif
  cancel = mc_cpar('mc_meancomb',data,1,'Data',[2,3,4,5],[1,2,3])
  if cancel then return

  idata = data

;  Get size of data array and create internal mask (imask). 

  s = size(idata)

  case s[0] of 
     
     1: begin
        
        narr  = n_elements(idata)
        imask = (n_elements(MASK) eq 0) ? intarr(narr)+1:mask

     end
     
     2: begin
        
        ncols = s[1]
        narr  = s[2]
        imask = (n_elements(MASK) eq 0) ? intarr(ncols,narr)+1:mask
        
     end
     
     3: begin
        
        ncols = s[1]
        nrows = s[2]
        narr  = s[3]
        mean  = dblarr(ncols,nrows,/NOZERO)
        mvar  = dblarr(ncols,nrows,/NOZERO)
        imask = (n_elements(MASK) eq 0) ? intarr(ncols,nrows,narr)+1:mask

     end
     
  endcase
    
;  Check for user inputs.

  weighted  = (n_elements(DATAVAR) ne 0) ?  1:0
  sigmaclip = (n_elements(ROBUST) ne 0) ?  1:0

  if keyword_set(NAN) then begin

     z1 = where(finite(idata) eq 0,cnt)
     if cnt ne 0 then imask[z1] = 0
     
     if weighted then begin
        
        z2 = where(finite(datavar) eq 0,cnt)
        if cnt ne 0 then imask[z2] = 0
        
     endif
         
  endif

;  Set up the Fanning showprogress object if requested.
  
  if keyword_set(UPDATE) and sigmaclip then begin

     progressbar = obj_new('SHOWPROGRESS',widget_id,COLOR=2,$
                           TITLE='mc_meancomb',$
                           MESSAGE='  Identifying outliers...  ')
     progressbar -> start
     
  endif


  
;  Perform combination.

  if s[0] eq 1 then begin  ;  1D

     if sigmaclip then begin

        rmask = mc_findoutliers(idata,robust,SILENT=silent,CANCEL=cancel)
        if cancel then return

        z = where(rmask eq 0,cnt)
        if cnt ne 0 then imask[z] = 0

     endif
     
     if weighted then begin
        
        z    = where(imask eq 1)
        mvar = 1./total(1./datavar[z],/DOUBLE)
        mean = total(idata[z]/datavar[z],/DOUBLE)*mvar
        
     endif else begin
        
        z    = where(imask eq 1,cnt)
        mean = total(idata[z],/DOUBLE)/cnt
        mvar = ( total(double(data[z])^2)-cnt*mean^2 )/(cnt-1)
        if not keyword_set(RMS) then mvar=mvar/cnt
        
     endelse
     
  endif
  
  if s[0] eq 2 then begin  ;  2D

     if sigmaclip then begin

        for i = 0,ncols-1 do begin

           rmask = mc_findoutliers(idata[i,*],robust,SILENT=silent, $
                                   CANCEL=cancel)
           if cancel then return
           
           z = where(rmask eq 0,cnt)
           if cnt ne 0 then imask[i,z] = 0
           
        endfor

     endif

     if weighted then begin

        tmp = 1./datavar
        z = where(imask eq 0,cnt)
        if cnt ne 0 then tmp[z] = 0
        
        mvar = 1./total(tmp,2,/DOUBLE)

        tmp = idata/datavar
        z = where(imask eq 0,cnt)
        if cnt ne 0 then tmp[z] = 0

        mean = total(tmp,2,/DOUBLE)*mvar
                
     endif else begin
        
        z = where(imask eq 0,cnt)
        if cnt ne 0 then idata[z] = 0.0
        ndat = total(imask,2)
        
        mean = total(idata,2,/DOUBLE)/ndat
        mvar = ( total(double(idata)^2,2)-ndat*mean^2 )/(ndat-1)
        if not keyword_set(RMS) then mvar=temporary(mvar)/ndat
        
     endelse
     
  endif
  
  if s[0] eq 3 then begin  ;  3D
     
     if sigmaclip then begin
        
        for i = 0,ncols-1 do begin
           
           for j=0,nrows-1 do begin

              tmp = idata[i,j,*]
              med = median(tmp,/EVEN)
              mad = 1.482*median(abs(tmp-med),/EVEN)
              imask[i,j,*] = ~(abs((tmp-med)/mad) gt robust)
              
           endfor

           if keyword_set(UPDATE) then begin
              
              percent = (i+1)*(100./float(ncols-1))
              progressbar->update, percent
              
           endif    
           
        endfor
       
     endif

     if weighted then begin

        tmp = 1./datavar
        z = where(imask eq 0,cnt)
        if cnt ne 0 then tmp[z] = 0
        
        mvar = 1./total(tmp,3,/DOUBLE)

        tmp = idata/datavar
        z = where(imask eq 0,cnt)
        if cnt ne 0 then tmp[z] = 0

        mean = total(tmp,3,/DOUBLE)*mvar
        
     endif else begin


        z = where(imask eq 0,cnt)
        if cnt ne 0 then idata[z] = 0.0
        
        ndat = total(imask,3)
        
        mean = total(idata,3,/DOUBLE)/ndat
        mvar = ( total(double(idata)^2,3)-ndat*mean^2 )/(ndat-1)
        if not keyword_set(RMS) then mvar=temporary(mvar)/ndat
        
     endelse
     
  endif
  
  mean = float(mean)
  mvar = float(mvar)
  ogoodbad = imask

  if keyword_set(UPDATE) and sigmaclip then begin
     
     progressbar-> destroy
     obj_destroy, progressbar
     
  endif
  
  
end




