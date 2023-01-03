;+
; NAME:
;     mc_readpaltspecfits
;
; PURPOSE:
;     Reads multiple Palomar TSpec FITS images into memory.
;
; CATEGORY:
;     File I/O
;
; CALLING SEQUENCE:
;     mc_readpaltspecfits,files,data,hdrinfo,var,KEYWORDS=keywords,$
;                         ROTATE=rotate,PAIR=PAIR,LINCORR=lincorr,$
;                         WIDGET_ID=widget_id,NIMAGES=nimages,$
;                         SATURATION=saturation,MASK=mask,_EXTRA=extra,$
;                         CANCEL=cancel
; INPUTS:
;     files - A string of (fullpath) file names.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     KEYWORDS   - A string array of keywords to extract from the hdrs. 
;     ROTATE     - Set to the desired IDL rotation command (ROTATE).
;     PAIR       - Set to pair subtract.  Must be even number of input files.
;     LINCORR     - Not functional, just there for consistency with the
;                  SpeX version.
;     WIDGET_ID  - If given, an pop-up error message will appear over
;                  the widget.
;     NIMAGES    - The number of images in the output data
;     SATURATION - The saturation level in DN
;     MASK       - If SATURATION is given, returns a integer array of
;                  masks where 0=good 1=saturated 
;     _EXTRA     - Set to subtract bias correction on pair subtractions.
;     CANCEL     - Will be set on return if there is a problem
;
; OUTPUTS:
;     Returns a floating array of images
;
; OPTIONAL OUTPUTS:
;     hdrinfo - If KEYWORDS are given, an array of structures of the 
;               requested hdr keywords for each image.  If no
;               keywords are given, then all the keywords are
;               returned in an array of structure.
;     var     - A floating array of variance images is returned.
;
; COMMON BLOCKS:
;     None
;
; SIDE EFFECTS:
;     None
;
; RESTRICTIONS:
;     Error propagation is hardwired for Palomar TripleSpec.
;
; PROCEDURE:
;     Images are read in, pair subtracted if requested, and
;     variances images are computed if requested.
;
; EXAMPLE:
;     None
;
; MODIFICATION HISTORY:
;     Written by M. Cushing, NASA JPL
;     2011-06-18 - Added _EXTRA keyword.
;
;-
pro mc_readpaltspecfits,files,data,hdrinfo,var,KEYWORDS=keywords,ROTATE=rotate,$
                        PAIR=PAIR,LINCORR=lincorr,WIDGET_ID=widget_id,$
                        NIMAGES=nimages,ITOT=itot,BITINFO=bitinfo, $
                        BITMASK=bitmask,_EXTRA=extra,CANCEL=cancel

  cancel  = 0
  
;  Check parameters
  
  if n_params() lt 1 then begin
     
     print, 'Syntax - mc_readpaltspecfits,files,data,hdrinfo,var,$'
     print, '                             KEYWORDS=keywords,ROTATE=rotate,$'
     print, '                             PAIR=PAIR,LINCORR=lincorr,$'
     print, '                             WIDGET_ID=widget_id,NIMAGES=nimages,$'
     print, '                             BITINFO=bitinfo,BITMASK=bitmask,$'
     print, '                             _EXTRA=extra,CANCEL=cancel'
     cancel = 1
     return
     
  endif
  cancel = mc_cpar('mc_readpaltspecfits',files,1,'Files',7,[0,1])
  if cancel then return
  
;  Get setup info.
  
  dovar = (arg_present(var) eq 1) ? 1:0
  dolinmax = (n_elements(BITINFO) ne 0) ? 1:0
  rot   = (n_elements(ROTATE) ne 0) ? rotate:0
  
;  Get number of images and check to make sure even number if /PAIR.
  
  nfiles = n_elements(files)
  if keyword_set(PAIR) then begin
     
     result = mc_crange(nfiles,0,'Number of Files',/EVEN, $
                     WIDGET_ID=widget_id,CANCEL=cancel)
     if cancel then return
     nimages = nfiles/2
     
  endif else nimages = nfiles

;  Make data arrays.

  NAXIS1 = 2048
  NAXIS2 = 1024

  data    = fltarr(NAXIS1,NAXIS2,nimages,/NOZERO)
  hdrinfo = replicate(mc_gethdrinfo(headfits(files[0]),keywords),nfiles)
  itot    = fltarr(nfiles)
   
  if dovar then var  = fltarr(NAXIS1,NAXIS2,nimages)
  if dolinmax then bitmask = intarr(NAXIS1,NAXIS2,nimages)
  
;  Get size of images, itime, coadds, and ndrs from header.
  
  readnoise = 10.23
  READTIME = 1.271  
  READTIME = 3.813  
  
;  Read images
  
  if keyword_set(PAIR) then begin

;  Read images and get header info
     
     for i = 0, nimages-1 do begin

        Aimg = readfits(files[i*2],A_hdr,/SILENT) 
;        Aimg = tspec_clean(files[i*2],A_hdr)
        
        AITIME    = float(fxpar(A_hdr,'EXPTIME'))
        ACOADDS   = float(fxpar(A_hdr,'COADDS'))
        ANDRS     = float(fxpar(A_hdr,'FSAMPLE'))
        AGAIN     = fxpar(A_hdr,'GAIN')
        AREADTIME = READTIME
        ADIVISOR  = ACOADDS*ANDRS

        ardvar   = (2.*readnoise^2)/ANDRS/ACOADDS/AITIME^2/AGAIN^2
        acrtn    = (1.0 - AREADTIME*(ANDRS^2 -1.0)/3./AITIME/ANDRS)

        itot[i*2] = AITIME*ACOADDS

        Bimg = readfits(files[i*2+1],B_hdr,/SILENT) 
;        Bimg = tspec_clean(files[i*2+1],B_hdr)        

        BITIME    = float(fxpar(B_hdr,'EXPTIME'))
        BCOADDS   = float(fxpar(B_hdr,'COADDS'))
        BNDRS     = float(fxpar(B_hdr,'FSAMPLE'))
        BGAIN     = fxpar(B_hdr,'GAIN')
        BREADTIME = READTIME
        BDIVISOR  = BCOADDS*BNDRS

        brdvar   = (2.*readnoise^2)/BNDRS/BCOADDS/BITIME^2/BGAIN^2
        bcrtn    = (1.0 - BREADTIME*(BNDRS^2 -1.0)/3./BITIME/BNDRS)
        
        itot[i*2+1] = BITIME*BCOADDS

;  Check for saturation        

        if dolinmax then begin

           amask = (Aimg gt bitinfo.lincormax)*2^(bitinfo.lincormaxbit)
           bmask = (Bimg gt bitinfo.lincormax)*2^(bitinfo.lincormaxbit)

           msk = mc_combflagstack([[[amask]],[[bmask]]], $
                                  bitinfo.lincormaxbit+1,CANCEL=cancel)
           
           bitmask[*,*,i] = rotate(msk,rot)
           
        endif

;  Rotate images
        
        Aimg = rotate(temporary(Aimg),rot)
        Bimg = rotate(temporary(Bimg),rot)

;  Subtract bias drift

        dif = Aimg - Bimg

;  Correct for bias drifts
        
        if keyword_set(extra.test) then begin

           for j = 0,7 do begin
              

              histy = histogram(dif[0:1023,(j*128):(j+1)*128-1],BINSIZE=1,$
                                 LOCATIONS=histx)
              
              max = max(histy,idx)
              mode = histx[idx]
              med = median(dif[0:1023,(j*128):(j+1)*128-1],/EVEN)
              
;              mc_plothist,histx,histy,/XSTY,/YSTY,XRANGE=[-50,50], $
;                          TITLE=string((j*128))+string((j+1)*128-1)
;              plots,[mode,mode],!y.mc_crange,COLOR=2
;              plots,[med,med],!y.mc_crange,COLOR=3
;              xyouts,0.25,0.5,mode,/NORM,COLOR=2
;              xyouts,0.25,0.4,med,/NORM,COLOR=3
;              re =  ' '
;              read, re
              
              dif[0:1023,(j*128):(j+1)*128-1] = $
                 dif[0:1023,(j*128):(j+1)*128-1] - med
              
           endfor

        endif

;  Store data and variance if requested (assumes the ITIMES are the same!)

;        dif[0:1024,255] = !values.f_nan
;        dif[0:1024,383] = !values.f_nan
;        dif[0:1024,511] = !values.f_nan
        data[*,*,i] = dif/float(AITIME)


;        data[*,*,i] = dif/float(AITIME)


        if dovar then begin
           
           Avar = abs(Aimg*ADIVISOR)*acrtn/ANDRS/(ACOADDS^2)/(AITIME^2)/AGAIN+$
                  ardvar
           Bvar = abs(Bimg*BDIVISOR)*bcrtn/BNDRS/(BCOADDS^2)/(BITIME^2)/BGAIN+$
                  brdvar

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
        
        ITIME    = float(fxpar(hdr,'EXPTIME'))
        COADDS   = float(fxpar(hdr,'COADDS'))
        NDRS     = float(fxpar(hdr,'FSAMPLE'))
        GAIN     = fxpar(hdr,'GAIN')
        READTIME = READTIME
        DIVISOR  = COADDS*NDRS

        rdvar   = (2.*readnoise^2)/NDRS/COADDS/ITIME^2/gain^2
        crtn    = (1.0 - READTIME*(NDRS^2 -1.0)/3./ITIME/NDRS)

        itot[i] = ITIME*COADDS

;  Check for saturation        

        if dolinmax then begin

           msk = (img gt bitinfo.lincormax)*2^(bitinfo.lincormaxbit)
           bitmask[*,*,i] = rotate(msk,rot)          
           
        endif

;  Rotate images
        
        img = rotate(temporary(img),rot)

;  Store data and variance if requested

        data[*,*,i] = img/float(ITIME)
        
        if dovar then begin

           var[*,*,i] = abs(img*DIVISOR)*crtn/NDRS/(COADDS^2)/(ITIME^2)/gain+$
                        rdvar
        
        endif

;  Store header information
        
        copy_struct_inx,mc_gethdrinfo(hdr,keywords),hdrinfo,index_to=i
        
     endfor
     
  endif
  
end







