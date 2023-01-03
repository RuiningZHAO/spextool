;+
; NAME:
;     mc_readapotspecfits
;
; PURPOSE:
;     Reads multiple APO TSpec FITS images into memory.
;
; CATEGORY:
;     File I/O
;
; CALLING SEQUENCE:
;     mc_readapotspecfits,files,data,hdrinfo,var,KEYWORDS=keywords,$
;                         ROTATE=rotate,PAIR=PAIR,LINCORR=lincorr,$
;                         WIDGET_ID=widget_id,NIMAGES=nimages,$
;                         SATURATION=saturation,MASK=mask,CANCEL=cancel
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
;
;     MASK       - If SATURATION is given, returns a integer array of
;                  masks where 0=good 1=saturated 
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
;     2008 - Written by M. Cushing, Institute for Astronomy,
;            University of Hawaii
;
;-
pro mc_readapotspecfits,files,data,hdrinfo,var,KEYWORDS=keywords,ROTATE=rotate,$
                        PAIR=PAIR,LINCORR=lincorr,WIDGET_ID=widget_id,$
                        NIMAGES=nimages,ITOT=itot,BITINFO=bitinfo, $
                        BITMASK=bitmask,CANCEL=cancel

  cancel  = 0
  
;  Check parameters
  
  if n_params() lt 1 then begin
     
     print, 'Syntax - mc_readapotspecfits,files,data,hdrinfo,var,$'
     print, '                             KEYWORDS=keywords,ROTATE=rotate,$'
     print, '                             PAIR=PAIR,LINCORR=lincorr,$'
     print, '                             WIDGET_ID=widget_id,NIMAGES=nimages,$'
     print, '                             SATURATION=saturation,MASK=mask,$'
     print, '                             CANCEL=cancel'
     cancel = 1
     return
     
  endif
  cancel = mc_cpar('mc_readapotspecfits',files,1,'Files',7,[0,1])
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
  
  readnoise = 18.0
  coadds = 1.0
  gain = 3.5
  
;  Read images
  
  if keyword_set(PAIR) then begin

;  Read images and get header info
     
     for i = 0, nimages-1 do begin

        Aimg = readfits(files[i*2],A_hdr,/SILENT) 
        
        AITIME    = float(fxpar(A_hdr,'EXPTIME'))
        ACOADDS   = coadds
        ANDRS     = float(fxpar(A_hdr,'FSAMPLE'))
        AREADTIME = fxpar(A_hdr,'INTOFF')/1000.
        ADIVISOR  = ACOADDS*ANDRS

        ardvar   = (2.*readnoise^2)/ANDRS/ACOADDS/AITIME^2/gain^2
        acrtn    = (1.0 - AREADTIME*(ANDRS^2 -1.0)/3./AITIME/ANDRS)

        itot[i*2] = AITIME*coadds

        Bimg = readfits(files[i*2+1],B_hdr,/SILENT) 

        BITIME    = float(fxpar(B_hdr,'EXPTIME'))
        BCOADDS   = coadds
        BNDRS     = float(fxpar(B_hdr,'NFOWLER'))
        BREADTIME = fxpar(B_hdr,'INTOFF')/1000.
        BDIVISOR  = BCOADDS*BNDRS

        brdvar   = (2.*readnoise^2)/BNDRS/BCOADDS/BITIME^2/gain^2
        bcrtn    = (1.0 - BREADTIME*(BNDRS^2 -1.0)/3./BITIME/BNDRS)
        
        itot[i*2+1] = BITIME*coadds

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

;  Store data and variance if requested (assumes the ITIMES are the same!)

        data[*,*,i] = dif/float(AITIME)


        if dovar then begin
           
           Avar = abs(Aimg*ADIVISOR)*acrtn/ANDRS/(coadds^2)/(AITIME^2)/gain+$
                  ardvar
           Bvar = abs(Bimg*BDIVISOR)*bcrtn/BNDRS/(coadds^2)/(BITIME^2)/gain+$
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
        NDRS     = float(fxpar(hdr,'NFOWLER'))
        READTIME = fxpar(hdr,'INTOFF')/1000.
        DIVISOR  = coadds*NDRS

        rdvar   = (2.*readnoise^2)/NDRS/coadds/ITIME^2/gain^2
        crtn    = (1.0 - READTIME*(NDRS^2 -1.0)/3./ITIME/NDRS)

        itot[i] = ITIME*coadds

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

           var[*,*,i] = abs(img*DIVISOR)*crtn/NDRS/(coadds^2)/(ITIME^2)/gain+$
                        rdvar
        
        endif

;  Store header information
        
        copy_struct_inx,mc_gethdrinfo(hdr,keywords),hdrinfo,index_to=i
        
     endfor
     
  endif

  var[*] = 1.0
  
end







