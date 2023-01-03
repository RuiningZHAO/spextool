;+
; NAME:
;     mc_readspexfits
;
; PURPOSE:
;     Reads SpeX FITS images.
;
; CATEGORY:
;     File I/O
;
; CALLING SEQUENCE:
;     result = mc_readspexfits,files,data,[hdrinfo],[var],KEYWORDS=keywords,$
;                               PAIR=PAIR,ROTATE=rotate,LINCORR=lincorr,$
;                               BITINFO=bitinfo,BITMASK=bitmask,$
;                               NIMAGES=nimages,MASK=mask,ITOT=itot,$
;                               WIDGET_ID=widget_id,CANCEL=cancel
;
; INPUTS:
;     files - A string of (fullpath) file names.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     KEYWORDS   - A string array of keywords to extract from the hdrs. 
;     PAIR       - Set to pair subtract.  Must be even number of input files.
;     ROTATE     - Set to the desired IDL rotation command (ROTATE).
;     LINCORR    - Set to correct each image for non-linearity
;     BITINFO   - A structure giving various values with which to
;                 create a bitmask.  In this case, the values should
;                 be: {lincormax:lincormax,lincormaxbit:lincormaxbit}.  
;     BITMASK   - If BITINFO is given, a bit-set array according to
;                 the BITINFO values.
;     NIMAGES    - The number of images in the output data
;     ITOT       - An array giving the total integration time for each
;                  frame, ITIME*CO_ADDS.
;     MASK       - If SATURATION is given, returns a integer array of
;                  masks where 0=good 1=saturated 
;     WIDGET_ID  - If given, an pop-up error message will appear over
;                  the widget.
;     CANCEL     - Will be set on return if there is a problem
;
; OUTPUTS:
;     Returns a floating array of images
;
; OPTIONAL OUTPUTS:
;     hdrinfo - If KEYWORDS are given, an array of structures of the 
;               requested hdr keywords for each image.  If no
;               keywords are given, then all the keywords are
;               returned in a structure.
;     var     - A floating array of variance images is returned
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
;     Images are read in, pair subtracted if requested, and
;     variances images are computed if requested.
;
; EXAMPLE:
;     None
;
; MODIFICATION HISTORY:
;     2007-07-24 - Written by M. Cushing, Steward Observatory,
;                  University of Arizona.  Based on the old Spextool
;                  <v4.0 program readmfits.pro.
;     2007-09-14 - Further hardwired this for SpeX by setting the gain
;                  and single read readnoise explicitly
;     2010-08-28 - Added ITOT keyword.
;     2014-07-29 - Removed the SLOWCNT parameter from the code.
;     2015-01-04 - Removed the SATURATION keyword and added the
;                  BITINFO and BITMASK keywords.
;-
pro mc_readspexfits,files,data,hdrinfo,var,KEYWORDS=keywords,PAIR=PAIR, $
                    ROTATE=rotate,LINCORR=lincorr,BITINFO=bitinfo,BITMASK=bitmask, $
                    NIMAGES=nimages,ITOT=itot,WIDGET_ID=widget_id,CANCEL=cancel

  cancel  = 0

;  Check parameters

  if n_params() lt 1 then begin
     
     cancel = 1
     print, 'Syntax - mc_readspexfits,files,data,[hdrinfo],[var],$'
     print, '                         KEYWORDS=keywords,PAIR=PAIR,ROTATE=rotate,$'
     print, '                         LINCORR=lincorr,BITINFO=bitinfo,BITMASK=bitmask,$'
     print, '                         NIMAGES=nimages,ITOT=itot,$'
     print, '                         WIDGET_ID=widget_id,CANCEL=cancel'
     return
     
  endif
  cancel = mc_cpar('mc_readspexfits',files,1,'Files',7,[0,1])
  if cancel then return
  
;  Get setup info.
  
  dovar = (arg_present(var) eq 1) ? 1:0
  dolinmax = (n_elements(BITINFO) ne 0) ? 1:0
  rot   = (n_elements(ROTATE) ne 0) ? rotate:0

;  Correct for non-linearity?
  
  if keyword_set(LINCORR) then begin
     
     restore,filepath('lc_coeff.sav', $
                      ROOT=file_dirname(file_which('spex.dat'),/MARK))
     
  endif
  
  readnoise = 50.0  ;  per single read
  gain = 13         ;  electrons per DN
  

;  Get number of images and check to make sure even number if /PAIR.

  nfiles = n_elements(files)

  if keyword_set(PAIR) then begin
     
     result = mc_crange(nfiles,0,'Number of Files',/EVEN,WIDGET_ID=widget_id,$
                     CANCEL=cancel)
     if cancel then return
     nimages = nfiles/2
     
  endif else nimages = nfiles
  
;  Make data arrays.

  NAXIS1 = 1024
  NAXIS2 = 1024

  data    = fltarr(NAXIS1,NAXIS2,nimages)
  hdrinfo = replicate(mc_gethdrinfo(headfits(files[0]),keywords),nfiles)
  itot    = fltarr(nfiles)
  
  if dovar then var  = fltarr(NAXIS1,NAXIS2,nimages)
  if dolinmax then bitmask = bytarr(NAXIS1,NAXIS2,nimages)
  
  if keyword_set(PAIR) then begin
     
     for i = 0, nimages-1 do begin

;  Read images and get header info
        
        Aimg = readfits(files[i*2],A_hdr,/SILENT) 

        AITIME    = float(fxpar(A_hdr,'ITIME'))
        ACOADDS   = float(fxpar(A_hdr,'CO_ADDS'))
        ANDRS     = float(fxpar(A_hdr,'NDR'))
        AREADTIME = fxpar(A_hdr,'TABLE_MS')/1000.
        ADIVISOR  = float(fxpar(A_hdr,'DIVISOR'))

        ardvar   = (2.*readnoise^2)/ANDRS/ACOADDS/AITIME^2/gain^2
        acrtn    = (1.0 - AREADTIME*(ANDRS^2 -1.0)/3./AITIME/ANDRS)

        itot[i*2] = AITIME*ACOADDS
        
        Bimg = readfits(files[i*2+1],B_hdr,/SILENT)

        BITIME    = float(fxpar(B_hdr,'ITIME'))
        BCOADDS   = float(fxpar(B_hdr,'CO_ADDS'))
        BNDRS     = float(fxpar(B_hdr,'NDR'))
        BREADTIME = fxpar(B_hdr,'TABLE_MS')/1000.
        BDIVISOR  = float(fxpar(B_hdr,'DIVISOR'))

        brdvar   = (2.*readnoise^2)/BNDRS/BCOADDS/BITIME^2/gain^2
        bcrtn    = (1.0 - BREADTIME*(BNDRS^2 -1.0)/3./BITIME/BNDRS)
        
        itot[i*2+1] = BITIME*BCOADDS

;  Check for saturation

        if dolinmax then begin

           Amask = (Aimg/ADIVISOR gt double(bitinfo.lincormax))* $
                   2B^(bitinfo.lincormaxbit)
           Bmask = (Bimg/BDIVISOR gt double(bitinfo.lincormax))* $
                   2B^(bitinfo.lincormaxbit)

           msk = mc_combflagstack([[[Amask]],[[Bmask]]],3,CANCEL=cancel)
           if cancel then return
           
           bitmask[*,*,i] = rotate(msk,rot)
           
        endif
        
;  Correct for linearity

        if keyword_set(LINCORR) then begin

           aslowcnt = float(fxpar(A_hdr,'SLOWCNT'))
           bslowcnt = float(fxpar(B_hdr,'SLOWCNT'))
           
           Aimg = mc_spexlincor(Aimg/adivisor,aitime,aslowcnt,andrs,lc_coeff)*$
                  adivisor
           Bimg = mc_spexlincor(Bimg/bdivisor,bitime,bslowcnt,bndrs,lc_coeff)*$
                  bdivisor
           
        endif 
        
;  Rotate arrays

        Aimg = rotate(temporary(Aimg),rot)
        Bimg = rotate(temporary(Bimg),rot)

;  Store data and variance if requested

        data[*,*,i] = Aimg/ADIVISOR/AITIME - Bimg/BDIVISOR/BITIME
        
        if dovar then begin

           Avar = abs(Aimg)*acrtn/ANDRS/(ACOADDS^2)/(AITIME^2)/gain + ardvar
           Bvar = abs(Bimg)*bcrtn/BNDRS/(BCOADDS^2)/(BITIME^2)/gain + brdvar

           var[*,*,i] = Avar+Bvar
        
        endif

;  Store header information

        copy_struct_inx,mc_gethdrinfo(A_hdr,keywords),hdrinfo,index_to=i*2
        copy_struct_inx,mc_gethdrinfo(B_hdr,keywords),hdrinfo,index_to=i*2+1
        
     endfor
     
  endif
  
  if not keyword_set(PAIR) then begin
     
     for i = 0, nimages-1 do begin
        
        img = readfits(files[i],hdr,/SILENT)

        ITIME    = float(fxpar(hdr,'ITIME'))
        COADDS   = float(fxpar(hdr,'CO_ADDS'))
        NDRS     = float(fxpar(hdr,'NDR'))
        READTIME = fxpar(hdr,'TABLE_MS')/1000.
        DIVISOR  = float(fxpar(hdr,'DIVISOR'))

        rdvar   = (2.*readnoise^2)/NDRS/COADDS/ITIME^2/gain^2
        crtn    = (1.0 - READTIME*(NDRS^2 -1.0)/3./ITIME/NDRS)

        itot[i] = ITIME*COADDS

;  Check for saturation

        if dolinmax then begin

           msk = (img/DIVISOR gt double(bitinfo.lincormax))* $
                 2B^(bitinfo.lincormaxbit)
           bitmask[*,*,i] = rotate(msk,rot)
           
        endif

;  Correct for linearity

        if keyword_set(LINCORR) then begin

           slowcnt = float(fxpar(hdr,'SLOWCNT'))
           img = mc_spexlincor(img/divisor,itime,slowcnt,ndrs,lc_coeff)*divisor
           
        endif 

;  Rotate arrays

        img = rotate(temporary(img),rot)

;  Store data and variance if requested

        data[*,*,i] = img/DIVISOR/ITIME
        
        if dovar then begin

           var[*,*,i] = abs(img)*crtn/NDRS/(COADDS^2)/(ITIME^2)/gain + rdvar
        
        endif        

;  Store header information
        
        copy_struct_inx,mc_gethdrinfo(hdr,keywords),hdrinfo,index_to=i
        
     endfor
     
  endif
  
end







