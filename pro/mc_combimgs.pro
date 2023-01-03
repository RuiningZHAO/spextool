;+
; NAME:
;     mc_combimgs
;
; PURPOSE:
;     To combine a stack of 2D images together.
;
; CATEGORY:
;     Data Reduction
;
; CALLING SEQUENCE:
;     mc_combimgs,dcube,type,mean,var,IVCUBE=ivcube,THRESH=thresh,
;                 UPDATE=update,OGOODBAD=ogoodbad,CANCEL=cancel
;
; INPUTS: 
;     dcube - The data 3D data cube.  
;     type  - The combination statistic,
;             0: Robust Weighted Mean
;
;                 A sigma clip algorithm is used to identify outliers.
;                 The value at each pixel is then the weighted average
;                 of the good pixels and the error is given by the
;                 propagated variance.
;
;             1: Robust Mean (RMS)
; 
;                 A sigma clip algorithm is used to identify
;                 outliers. The value at each pixel is the mean of the
;                 good pixels and the error is given by rms deviation of
;                 the pixels.
;
;             2: Robust Mean (Mean Error)
;
;                 A sigma clip algorithm is used to identify
;                 outliers.  The value at each pixel is the mean of the
;                 good pixels and the error is given by the error on
;                 the mean (rms deviation of the pixels divided by root
;                 N.)
;
;             3: Weighted Mean
;
;                 A weighted mean and error are given at each pixels.
;
;             4: Mean (RMS)
;
;                 The value at each pixel is given by the mean and the
;                 rms deviation of the pixels.
;
;             5: Mean (Mean Error)
;
;                 The value at each pixel is the mean of the pixel
;                 values and the error is given by the error on the 
;                 mean (rms deviation of the pixels divided by sqrt(N).)
;
;             6: Median (MAD)
;
;                 The value at each pixel is given by the median
;                 of the pixels.  The error is given by the Median Absolute
;                 Deviation (MAD) which is given by,
;                 MAD=1.4826 * median( abs(x_i -med) ).  
;                 The constant 1.4826 is defined such at MAD = sigma
;                 if the random variable x follows a normal
;                 distribution and the sample is large.
;
;             7: Median (Median Error)
;
;                 The value at each pixel is given by the median
;                 of the pixels.  The error is given by the MAD/sqrt(N).
;
;             8: Sum
;
;                The value at each pixel is given the sum of the
;                pixels and the error is given by the sum of the 
;                variances.
;
; OPTIONAL INPUTS:
;     thresh - The robust threshold used to identify bad pixels if a
;              robust statistic is chosen (see below).
;
; KEYWORD PARAMETERS:
;     IVCUBE - The variance 3D cube.  
;     OVIMG  - The output variance image
;     UPDATE - Set to run an update widget (used only for robust
;              stats)
;     CANCEL - Set on return if there is a problem.
;
; OUTPUTS:
;     mean - The combined image.
;     var  - The variance image.
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
;     A data point is identified as an outlier if 
;     |x_i - x_med|/MAD > thresh where x_med is the median and MAD 
;     is the median absolute deviation defined as 
;     1.482*median(|x_i-x_med|).
;
; EXAMPLE:
;     Later
;
; MODIFICATION HISTORY:
;     2007-07-12 - Written by M. Cushing, Steward Observatory,
;                  University of Arizona
;     2015-01-04 - Added OGOODBAD keyword.
;-
pro mc_combimgs,dcube,type,mean,var,IVCUBE=ivcube,THRESH=thresh,$
                OGOODBAD=ogoodbad,UPDATE=update,CANCEL=cancel

  cancel = 0

  if n_params() lt 2 then begin
     
     print, 'Syntax - mc_combimgs,dcube,type,mean,var,IVCUBE=ivcube,$'
     print, '                     THRESH=thresh,UPDATE=update,CANCEL=cancel'
     cancel = 1
     return
     
  endif
  
  cancel = mc_cpar('mc_combimgs',dcube,1,'Dcube',[1,2,3,4,5],3)
  if cancel then return
  cancel = mc_cpar('mc_combimgs',type,2,'Type',[1,2,3,4,5],0)
  if cancel then return
  
  if n_elements(IVCUBE) ne 0 then $
     cancel = mc_cpar('mc_combimgs',ivcube,99,'Cube',[1,2,3,4,5],3)
  if cancel then return
  
  case type of
     
     0: begin

        mc_meancomb,dcube,mean,var,DATAVAR=ivcube,ROBUST=thresh, $
                    OGOODBAD=ogoodbad,CANCEL=cancel
        if cancel then return
        
;        robustmeancomb,dcube,thresh,mean,var,DATAVAR=ivcube,UPDATE=update, $
;                       CANCEL=cancel

        
     end
     
     1: begin

        mc_meancomb,dcube,mean,var,/RMS,ROBUST=thresh, $
                    OGOODBAD=ogoodbad,CANCEL=cancel
        if cancel then return
        
;        robustmeancomb,dcube,thresh,mean,var,/RMS,UPDATE=update,CANCEL=cancel

        
     end
     
     2: begin

        mc_meancomb,dcube,mean,var,ROBUST=thresh,$
                    OGOODBAD=ogoodbad,CANCEL=cancel
        if cancel then return
        
;        robustmeancomb,dcube,thresh,mean,var,UPDATE=update,CANCEL=cancel
        
     end
     
     3: begin
        
        mc_meancomb,dcube,mean,var,DATAVAR=ivcube,CANCEL=cancel
        if cancel then return
        
     end
     
     4: begin
        
        mc_meancomb,dcube,mean,var,/RMS,CANCEL=cancel
        if cancel then return
        
     end
     
     5: begin
        
        mc_meancomb,dcube,mean,var,CANCEL=cancel
        if cancel then return
        
     end
     
     6: begin
        
        mc_medcomb,dcube,mean,var,/MAD,CANCEL=cancel
        if cancel then return
        
     end
     
     7: begin
        
        mc_medcomb,dcube,mean,var,CANCEL=cancel
        if cancel then return
        
     end
     
     8: begin
        
        
        mean = total(dcube,3,/NAN)
        var = total(ivcube,3,/NAN)
        
     end
     
  endcase
  
end
