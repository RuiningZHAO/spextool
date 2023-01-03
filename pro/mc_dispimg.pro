;+
; NAME:
;     mc_dispimg
;
; PURPOSE:
;     To do everything necessary to display an image (range, scaling, etc.)
;
; CALLING SEQUENCE:
;     
;
; INPUTS:
;
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;     RANGE - 99
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
pro mc_dispimg,img,RANGE=range,ZRANGE=zrange,SCALING=scaling,CMAP=cmap, $
               XRANGE=xrange,YRANGE=yrange,POSITION=position, $
               PLOTAXES=plotaxes,_EXTRA=_extra,INVERT=invert, $
               KEEP_ASPECT_RATIO=keep_aspect_ratio,CANCEL=cancel

  cancel = 0
  
  s = size(img,/DIMEN)
  ncols = s[0]
  nrows = s[1]

  if n_elements(RANGE) eq 0 then range = 1
  if n_elements(SCALING) eq 0 then scaling = 0
  if n_elements(CMAP) eq 0 then cmap = 0
  if n_elements(XRANGE) eq 0 then xrange = [-0.5,ncols-0.5]
  if n_elements(YRANGE) eq 0 then yrange = [-0.5,nrows-0.5]
  if n_elements(POSITION) eq 0 then position = [0,0,1,1]

  mc_mkct, cmap, BOT=bot,INVERT=invert

  if n_elements(ZRANGE) eq 0 then begin
     
     mc_imgrange,img,min,max,RANGE=range,CANCEL=cancel
     if cancel then return
     
  endif else begin
     
     min = zrange[0]
     max = zrange[1]

  endelse

  tvimage,mc_bytsclimg(img,scaling,bot,MIN=min,MAX=max),POSITION=position,$
          /NOINTERP,KEEP_ASPECT_RATIO=keep_aspect_ratio
  
  style = (keyword_set(PLOTAXES) eq 1) ? 1:5

  plot,[1],[1],/NODATA,/NOERASE,XSTY=style,YSTY=style, $
       XRANGE=xrange,YRANGE=yrange,XMARGIN=[0,0],YMARGIN=[0,0], $
       NOCLIP=0,POSITION=position,XMINOR=0,YMINOR=0,_EXTRA=_extra, $
       COLOR=6
  
end
