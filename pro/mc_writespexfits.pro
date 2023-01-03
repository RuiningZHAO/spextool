;+
; NAME:
;     mc_writespexfits
;
; PURPOSE:
;     To write a Spetool FITS file given a wavelength and flux array
;
; CATEGORY:
;     Spectroscopy     
;
; CALLING SEQUENCE:
;     mc_writespexfits,wave,flux,oname,UNC=unc,FLAGS=flags,$
;                      XUNITS=xunits,YUNITS=yunits,PXTITLE=pxtitle,$
;                      PYTITLE=pytitle,WAVETYPE=wavetype,EXTRA=extra,$
;                      COMMENTS=comments,CANCEL=cancel
;
; INPUTS:
;     wave  - An [ndat] array giving the wavelengths.
;     flux  - An [ndat] array giving the flux values.
;     oname - A string scalar of the output file name.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     UNC      - An [ndat] array of the uncertainty values.
;     FLAGS    - An [ndat] array giving possible flags.
;                Any format is possible, but for use with xvspec:
;                0 bit set:  value contains a pixel in the bad pixel mask
;                1 bit set:  value contains a pixel identified as bad
;                2 bit set:  value contains a pixel identified as
;                suturated.
;     XUNITS   - A string scalar giving the wavelength units.
;     YUNITS   - A string scalar giving the flux units.
;     PXTITLE  - A string scalar giving the XTITLE that can be used in IDL.
;     PYTITLE  - A string scalar giving the YTITLE that can be used in IDL.
;     WAVETYPE - If the xunits are wavelengths, the type of
;                wavelength, i.e. air/vacuum.
;     EXTRA    - A structure with two tags, vals (values) and coms
;                (comments), that has additional keywords and comments.
;                EXTRA.vals is a structure where the tag name is the
;                FITS keyword and the value is the value of the FITS
;                keyword.  EXTRA.coms is a structure where the tag
;                name is the FITS keyword and the value is the comment
;                for the FITS keyword.
;                
;                e.g., 
;
;                vals = {name:'img001.fits'}
;                coms = {name:' FITS file name'}
;                extra = {vals:vals,coms:coms}
;
;     COMMENTS - A string scalar giving comments.
;     CANCEL   - Set on return if there is a problem
;
; OUTPUTS:
;      Writes a Spextool FITS file to disk.
;
; OPTIONAL OUTPUTS:
;      None
;
; COMMON BLOCKS:
;      None
;
; SIDE EFFECTS:
;      None
;
; RESTRICTIONS:
;      None
;
; PROCEDURE:
;      Creates a header and writes a FITS file to disk
;
; EXAMPLE:
;      writespexfits,wave,flux,'test.fits',error,'um','Wm-2um-1','Vacuum'
;
; MODIFICATION HISTORY:
;      2004-02-25 - Written by M. Cushing, NASA Ames Research Center
;      2004-02-26 - Added the xunits and yunits inputs
;      2004-11-19 - Added the error and wavetype inputs
;      2005-02-22 - Added EXTRA and COMMENTS keyword
;      2015-01-03 - Added the FLAGS keyword.
;
;-
pro mc_writespexfits,wave,flux,oname,ERROR=error,FLAGS=flags,XUNITS=xunits,$
                     YUNITS=yunits,PXTITLE=pxtitle,PYTITLE=pytitle,$
                     WAVETYPE=wavetype,EXTRA=extra,COMMENTS=comments,$
                     CANCEL=cancel

  cancel = 0

  if n_params() lt 3 then begin
     
     print, 'Syntax - mcc_writespexfits,wave,flux,oname,ERROR=error,$'
     print, '                           FLAGS=flags,XUNITS=xunits,$'
     print, '                           YUNITS=yunits,PXTITLE=pxtitle,$'
     print, '                           PYTITLE=pytitle,WAVETYPE=wavetype,$'
     print, '                           EXTRA=extra,COMMENTS=comments,$'
     print, '                           CANCEL=cancel'
     cancel = 1
     return
     
  endif
  
  cancel = mc_cpar('mc_writespexfits',wave,1,'Wave',[2,3,4,5],1)
  if cancel then return
  cancel = mc_cpar('mc_writespexfits',flux,2,'Flux',[2,3,4,5],1)
  if cancel then return
  cancel = mc_cpar('mc_writespexfits',oname,3,'Oname',7,0)
  if cancel then return
  
  if n_elements(ERROR) ne 0 then begin
     
     cancel = mc_cpar('mc_writespexfits',error,4,'Error',[2,3,4,5],[0,1])
     if cancel then return
     
  endif

  if n_elements(Flags) ne 0 then begin
     
     cancel = mc_cpar('mc_writespexfits',flags,5,'Flags',[2,3,4,5],[0,1])
     if cancel then return
     
  endif
  
  if n_elements(XUNITS) ne 0 then begin
     
     cancel = mc_cpar('mc_writespexfits',xunits,6,'Xunits',7,0)
     if cancel then return
     
  endif
  
  if n_elements(YUNITS) ne 0 then begin
     
     cancel = mc_cpar('mc_writespexfits',yunits,7,'Yunits',7,0)
     if cancel then return
     
  endif
  
  if n_elements(PXTITLE) ne 0 then begin
     
     cancel = mc_cpar('mc_writespexfits',pxtitle,8,'Xtitle',7,0)
     if cancel then return
     
  endif
  
  if n_elements(PYTITLE) ne 0 then begin
     
     cancel = mc_cpar('mc_writespexfits',pytitle,9,'Ytitle',7,0)
     if cancel then return
     
  endif
  
  if n_elements(WAVETYPE) ne 0  then begin
     
     cancel = mc_cpar('mc_writespexfits',wavetype,10,'Wavetype',7,0)
     if cancel then return
     
  endif
  
 ;  Create uncertainty and flag arrays if necessary
  
  if n_elements(error) eq 0 then error = replicate(1,n_elements(wave))
  if n_elements(flags) eq 0 then flags = replicate(0,n_elements(wave))

;  Create array
  
  array = [[wave],[flux],[error],[flags]]

;  Create the FITS header
  
  mkhdr,hdr,array
  
;  Do EXTRA keywords if given
  
  if n_elements(EXTRA) ne 0 then begin
     
     ntags = n_tags(EXTRA.vals)
     names = tag_names(EXTRA.vals)
     
     for i =0, ntags-1 do fxaddpar,hdr,names[i],extra.vals.(i),extra.coms.(i)
     
  endif
  
  fxaddpar,hdr,'NORDERS',1,' Number of Orders'
  fxaddpar,hdr,'ORDERS','1',' Order Numbers'
  fxaddpar,hdr,'NAPS',1,' Number of Apertures'
  
  if n_elements(xunits) ne 0 then fxaddpar,hdr,'XUNITS',xunits,$
                                           ' Wavelength Units'
  
  if n_elements(yunits) ne 0 then fxaddpar,hdr,'YUNITS',yunits,' Flux Units'
  
  if n_elements(wavetype) ne 0  then fxaddpar,hdr,'WAVETYPE',wavetype,$
     ' Wavelength Type'
  
  if n_elements(pxtitle) ne 0 then fxaddpar,hdr,'XTITLE',pxtitle,$
                                            ' IDL X Title'
  
  if n_elements(pytitle) ne 0 then fxaddpar,hdr,'YTITLE',pytitle,$
                                            ' IDL Y Title'
  
  if n_elements(COMMENTS) ne 0 then begin
     
     fxaddpar,hdr,'COMMENT',' '
     text = mc_splittext(comments,69)
     for i = 0,n_elements(text)-1 do fxaddpar,hdr,'COMMENT',text[i]
     
  endif

;  Write file to disk  
  
  writefits,oname,array,hdr
  
end
