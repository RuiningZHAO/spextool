;+
; NAME:
;     mc_moments
;
; PURPOSE:
;     Computes statistics on a data set avoiding deviant points if requested.
;
; CATEGORY:
;     Statistics
;
; CALLING SEQUENCE:
;     mc_moments,data,mean,var,stddev,stderror,skew,kurt,ROBUST=robust,
;                DOUBLE=double,IGOODBAD=igoodbad,OGOODBAD=ogoodbad,$
;                SILENT=silent,CANCEL=cancel
;
; INPUTS:
;     data   - Vector of data values
;     thresh - Sigma threshold over which values are identified as outliers
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     ROBUST   - Set to the sigma level at which to throw data away.
;                See below.
;     DOUBLE   - Set for double precision.
;     IGOODBAD - An input goodbad array.  0=bad, 1=good, 2=nan
;     OGOODBAD - An output goodbad array
;     SILENT   - Set to supress printing results to the screen
;     CANCEL   - Set on return if there is a problem
;
; OUTPUTS:
;     mean     - Mean
;     var      - Variance
;     stddev   - Standard deviation 
;     stderror - The standard error on the mean, stddev/sqrt(n).
;     skew     - Skewness
;     kurt     - Kurtosis	
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
;     The mean, variance, standard deviation, skew, kurtosis
;     and standard error of the mean (stddev/sqrt(n)) is computed for
;     a given set of data.  If a value is passed to the ROBUST
;     keyword, then the dataset is searched for outliers.  A data
;     point is identified as an outlier if |x_i - x_med|/MAD > thresh,
;     where x_med is the median, MAD is the median absolute
;     deviation defined as 1.482*median(|x_i-x_med|), and thresh is
;     the value passed to the ROBUST keyword.
;     
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     1999-10-02 - Written by M. Cushing, Institute for Astronomy, UH
;     2000-12-08 - Fixed bug in first loop.  Too hard to explain, and
;                  not really important, but a bug nevertheless.
;     2001-07-11 - Now uses the median and median absolute deviation 
;                  to determine outliers.  As a result, the parameter
;                  eps has been removed.
;     2008-02-14 - Renamed robuststats to mc_moments and turned the
;                  robust part into a keyword.  Remove moments.pro
;                  from the Spextool library.  Added DOUBLE keyword
;                  and removed EXCLUDE keyword.  Added stderror
;                  output.
;-
pro mc_moments,data,mean,var,stddev,stderror,skew,kurt,ROBUST=robust, $
               DOUBLE=double,IGOODBAD=igoodbad,OGOODBAD=ogoodbad, $
               SILENT=silent,FAST=fast,CANCEL=cancel

  cancel = 0

; Check parameters

  if n_params() lt 1 then begin
     
     print, 'Syntax - mc_moments,data,mean,var,stddev,stderror,skew,kurt,$'
     print, '                    ROBUST=robust,DOUBLE=double,$'
     print, '                    IGOODBAD=igoodbad,OGOODBAD=ogoodbad,$'
     print, '                    SILENT=silent,CANCEL=cancel'
     cancel = 1
     return
     
  endif
  cancel = mc_cpar('mc_moments',data,1,'Data',[1,2,3,4,5,12],[1,2,3,4,5,6])
  if cancel then return

;  Check for an input goodbad array.
 
  if n_elements(igoodbad) eq 0 then igoodbad = replicate(1,n_elements(data))

;  Check for NaNs

  nan = where(finite(data) eq 0, cnt_nan)    
  if cnt_nan ne 0 then igoodbad[nan] = 2
  
;  Find good data.

  z = where(igoodbad eq 1,count_initgood)
  
  if count_initgood le 1 then begin
     
     mean   = !values.f_nan
     var    = !values.f_nan
     stddev = !values.f_nan
     skew   = !values.f_nan
     kurt   = !values.f_nan
     stderr = !values.f_nan
     print, 'No good data points found.'
     print, ' '
     cancel = 1
     return
     
  endif
  
; datas    = data[z]
  ogoodbad = igoodbad
 
 if n_elements(ROBUST) ne 0 then begin

    if keyword_set(FAST) then begin

       idx = randomu(seed,(count_initgood-1)/fast)*count_initgood
       med = median(data[z[idx]],/EVEN)
       mad = 1.482*median(abs(data[z[idx]]-med), /EVEN)

    endif else begin

       med = median(data[z],/EVEN)
       mad = 1.482*median(abs(data[z]-med), /EVEN)
       
    endelse

    gooddatas = where(abs( (data[z]-med)/mad ) le robust,cnt)
    if cnt eq 0 then gooddatas = lindgen(n_elements(data[z]))
    
 endif else gooddatas = lindgen(n_elements(data[z]))

;  Compute statistics

 if keyword_set(FAST) then begin

    idx = randomu(seed,(n_elements(gooddatas)-1)/fast)*n_elements(gooddatas)
    result = moment(data[z[gooddatas[idx]]],/NAN,DOUBLE=double)
 
 endif else begin

    result = moment(data[z[gooddatas]],/NAN,DOUBLE=double)
;    result = moment(data,/NAN,DOUBLE=double)

 endelse

 mean     = result[0]
 var      = result[1]
 stddev   = sqrt(var)
 stderror = sqrt(var/n_elements(gooddatas))
 skew     = result[2]
 kurt     = result[3]

; Reconstruct goodbad array
 
 good_bad               = replicate(0,count_initgood)
 good_bad[z[gooddatas]] = 1
 ogoodbad[z]            = good_bad


;  Print out results it SILENT not set.
 
 if not keyword_set(SILENT) then begin
    
    z = where(ogoodbad eq 0,nbad) 
    print, ' '
    print,'Results:'
    print, ' '
    print, 'Total number of data points: ',strtrim(n_elements(data),2)
    print, '             Number of NaNs: ',strtrim(cnt_nan,2)
    print, '       Number of bad points: ',strtrim(nbad,2)
    print, ' '
    print, '              Mean: ',strtrim(mean,2)
    print, '          Variance: ',strtrim(var,2)
    print, 'Standard Deviation: ',strtrim(stddev,2)
    print, '    Standard Error: ',strtrim(stderror,2)
    print, '          Skewness: ',strtrim(skew,2)
    print, '          Kurtosis: ',strtrim(kurt,2)
    
 endif
 
end









