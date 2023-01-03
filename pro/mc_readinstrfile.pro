;+
; NAME:
;     mc_readinstrfile
;
; PURPOSE:
;     Reads a Spextool instrument calibration file.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     result = mc_readinstrfile(filename,CANCEL=cancel)
;
; INPUTS:
;     filename - The name of a Spextool calibration file.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem
;
; OUTPUTS:
;     Later
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
;     Must be a Spextool instrument calibration file
;
; PROCEDURE:
;     Easy
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     2001-05-10 - Written by M. Cushing, Institute for Astronomy, UH
;     2003-03-27 - Added slowcnt input
;     2003-04-10 - Added readtime input
;     2007-08-xx - Huge rewrite.
;     2007-09-14 - Removed gain and readnoise outputs.
;     2008-02-28 - Added plotwinsize input.
;     2008-08-01 - Added flatmodule input.
;     2010-08-17 - Converted to a function.
;     2011-06-18 - Removed the combimgsub variable.
;     2014-06-18 - Added the possibility to freeze certain widgets.
;     2014-08-06 - Added the ampcorrect parameter.
;     2015-01-03 - Removed the plotsaturation parameter.
;     2015-01-04 - Changed saturation to lincormax.
;-
function mc_readinstrfile,filename,CANCEL=cancel

  cancel = 0

;  Check parameters

  if n_params() lt 1 then begin
    
     print, 'Syntax - result = mc_readinstfile(filename,CANCEL=cancel)'
     cancel = 1
     return,-1
     
  endif
  cancel = mc_cpar('mc_readinstrfile',filename,1,'Filename',7,0)
  if cancel then return,-1
  
  readcol,filename,type,val,COMMENT='#',DELIMITER='=',FORMAT='A,A',/SILENT
  
  i = 0
  instr = strtrim(val[0],2)
  i = i+1

  ncols = long(val[i])
  i = i+1

  nrows = long(val[i])
  i = i+1

  stdimage = long(val[i])
  i = i+1

  plotwinsize = long(strsplit(val[i],' ',/EXTRACT))
  i = i+1

  irafname = strtrim(val[i],2)
  i = i+1
    
  exptime = strtrim(val[i],2)
  i = i+1

  time = strtrim(val[i],2)
  i = i+1  

  posangle = strtrim(val[i],2)
  i = i+1

  ha = strtrim(val[i],2)
  i = i+1

  airmass = strtrim(val[i],2)
  i = i+1

  nint = fix(val[i])
  i = i+1

  nsuffix = fix(val[i])
  i = i+1  

  bdpxmk = strtrim(val[i],2)
  i = i+1

  calmodule = strtrim(val[i],2)
  i = i+1

  filereadmode = strtrim(val[i],2)
  i = i+1

  iprefix = strtrim(val[i],2)
  i = i+1  

  oprefix = strtrim(val[i],2)
  i = i+1  

  suffix = strtrim(val[i],2)
  if suffix eq 'None' then suffix = ''
  i = i+1

  fitsreadprogram = strtrim(val[i],2)
  i = i+1

  reductionmode = strtrim(val[i],2)
  i = i+1

  combimgmode = strtrim(val[i],2)
  i = i+1  

  combimgstat = strtrim(val[i],2)
  i = i+1

  combimgthresh = strtrim(val[i],2)
  i = i+1

  combimgdir = strtrim(val[i],2)
  i = i+1

  skystat = strtrim(val[i],2)
  i = i+1  

  skythresh = strtrim(val[i],2)
  i = i+1

  ybuffer = strtrim(val[i],2)
  i = i+1  

  oversamp = strtrim(val[i],2)
  i = i+1  

  atmosthresh = strtrim(val[i],2)
  i = i+1  

  psnaps = fix(val[i])
  i = i+1    

  pspsfrad = strtrim(val[i],2)
  i = i+1  
    
  psaprad = strtrim(val[i],2)
  i = i+1  

  psbgsub = fix(val[i])
  i = i+1  
  
  psbgstart = strtrim(val[i],2)
  i = i+1  

  psbgwidth = strtrim(val[i],2)
  i = i+1  
  
  psbgdeg = strtrim(val[i],2)
  i = i+1  

  xsbgsub = fix(val[i])
  i = i+1  

  xsbgreg = strtrim(val[i],2)
  i = i+1  
  
  xsbgdeg = strtrim(val[i],2)
  i = i+1    

  tracedeg = fix(val[i])
  i = i+1  

  tracestepsize = fix(val[i])
  i = i+1  

  tracesumap = fix(val[i])
  i = i+1  

  tracesigthresh = fix(val[i])
  i = i+1  

  tracewinthresh = fix(val[i])
  i = i+1  

  bdpixthresh = float(val[i])
  i = i+1  

  lincormax = long(val[i])
  i = i+1  

  checkseeing = fix(val[i])
  i = i+1  

  seeingthresh = float(val[i])
  i = i+1  

  ampcorrect = fix(strsplit(val[i],' ',/EXTRACT))
  i = i+1  

  lincorrect = fix(strsplit(val[i],' ',/EXTRACT))
  i = i+1  

  errorprop = fix(strsplit(val[i],' ',/EXTRACT))
  i = i+1  

  flatfield = fix(strsplit(val[i],' ',/EXTRACT))
  i = i+1  

  fixbdpx = fix(strsplit(val[i],' ',/EXTRACT))
  i = i+1  

  optextract = fix(strsplit(val[i],' ',/EXTRACT))
  i = i+1  

  ntot = n_elements(type)

  keywords = strtrim(val[i],2)
  i = i+1

  while i ne ntot do begin

     keywords = [keywords,strtrim(val[i],2)]
     i = i+1
     
  endwhile

  struc = {instr:instr,nrows:nrows,ncols:ncols,stdimage:stdimage, $
           plotwinsize:plotwinsize,irafname:irafname,exptime:exptime,$
           time:time,posangle:posangle,ha:ha,airmass:airmass,nint:nint,$
           nsuffix:nsuffix,bdpxmk:bdpxmk,calmodule:calmodule, $
           filereadmode:filereadmode,iprefix:iprefix,oprefix:oprefix, $
           suffix:suffix,fitsreadprogram:fitsreadprogram, $
           reductionmode:reductionmode,combimgmode:combimgmode, $
           combimgstat:combimgstat,combimgthresh:combimgthresh, $
           combimgdir:combimgdir,skystat:skystat,skythresh:skythresh, $
           ybuffer:ybuffer,oversamp:oversamp,atmosthresh:atmosthresh, $
           psnaps:psnaps,pspsfrad:pspsfrad,psaprad:psaprad,psbgsub:psbgsub,$
           psbgstart:psbgstart,psbgwidth:psbgwidth,psbgdeg:psbgdeg, $
           xsbgsub:xsbgsub,xsbgreg:xsbgreg,xsbgdeg:xsbgdeg,tracedeg:tracedeg,$
           tracestepsize:tracestepsize,tracesumap:tracesumap, $
           tracesigthresh:tracesigthresh,tracewinthresh:tracewinthresh, $
           bdpxthresh:bdpixthresh,lincormax:lincormax, $
           checkseeing:checkseeing,seeingthresh:seeingthresh, $
           ampcorrect:ampcorrect,lincorrect:lincorrect,errorprop:errorprop, $
           flatfield:flatfield,fixbdpx:fixbdpx,optextract:optextract, $
           keywords:keywords}

  return, struc

end
