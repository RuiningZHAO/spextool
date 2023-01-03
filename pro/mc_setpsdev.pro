;+
; NAME:
;     mc_setpsdev
;
; PURPOSE:
;     To set the Postscript Device.
;
; CATEGORY:
;     Plotting
;
; CALLING SEQUENCE:
;     mc_setpsdev,filename,xsize,ysize,LANDSCAPE=landscape,CLOSE=close,$
;                 _EXTRA=_extra,CONVERT=convert,ERASE=erase,CANCEL=cancel
;
; INPUTS:
;     None
;
; OPTIONAL INPUTS:
;     file  - A string giving the postscript file name
;     xsize - The xsize of the figure in inches.
;     ysize - The ysize of the figure in inches.
;
; KEYWORD PARAMETERS:
;     LANDSCAPE  - Set to plot in landscape mode 
;     CLOSE      - Set to close the postscript device
;                  useful for making figures for talks.
;     _EXTRA     - See manual for additional keywords to the DEVICE procedure
;     CONVERT    - Set to convert to a pdf file.  You must have the
;                  ps2pdf script in your path.
;     ERASE      - Set to delete the postscript file.
;     CANCEL     - Set on return if there is a problem
;
; OUTPUTS:
;     None
;
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     info
;
; SIDE EFFECTS:
;     None
;
; RESTRICTIONS:
;     Units are in inches.     
;     Assumes a paper size of 8x11 (via getpsoffset)
;     Selects a Helvetica font.
;
; PROCEDURE:
;     Sets the postscipt device following the user inputs. 
;
; EXAMPLE:
;     mc_setpsdev,'idl.ps',10,7.5,/LANDSCAPE
;     plot,x,y,/XSTY,/YSTY,/NODATA
;     oplot,x,y,COLOR=2
;     mc_setpsdev,/CLOSE
;
;     This will create a file called idl.ps with a plot with a colored line
;     in the landscape mode.  
;
; MODIFICATION HISTORY:
;     2008-01-21 - Written by M. Cushing, Institute for Astronomy, 
;                  University of Hawaii
;     2014-03-01 - Added the CONVERT and ERASE keywords.
;     2014-10-17 - Let the device get set back to whatever it was
;                  before the ps switch (required for some windows
;                  machines it seems).
;     2014-11-26 - Fixed a bug where the converted pdf file was not
;                  being written to the correct directory.
;-
;===========================================================================
;    
pro getpsoffset,xsize,ysize,xoffset,yoffset,LANDSCAPE=landscape,CANCEL=cancel

  papersize = [8.5,11.0]

  if keyword_set(LANDSCAPE) then begin
     
     xoffset = (papersize[0]-ysize)/2.
     yoffset = papersize[1]-(papersize[1]-xsize)/2.
     
  endif else begin
     
     xoffset = (papersize[0]-xsize)/2.
     yoffset = (papersize[1]-ysize)/2.
     
  endelse
  
end
;
;==============================================================================
;
pro mc_setpsdev,filename,xsize,ysize,CLOSE=close,LANDSCAPE=landscape, $
                FONT_SIZE=font_size,_EXTRA=_extra,CONVERT=convert,ERASE=erase,$
                CANCEL=cancel

  common info,fname,odevice
  
  cancel = 0

  if keyword_set(CLOSE) then begin
     
     device, /CLOSE
     set_plot,odevice

     if keyword_set(CONVERT) then begin

        eps = (strlen(fname)-strpos(fname,'.',/REVERSE_SEARCH) eq 4) ? 1:0
        suffix = (eps eq 1) ? '.eps':'.ps'
        file = file_basename(fname,suffix)

        dir = file_dirname(fname,/MARK)
        spawn, 'ps2pdf -dEPSCrop '+fname+' '+dir+file+'.pdf',result,err

        if keyword_set(ERASE) then spawn, 'rm '+fname
     
     endif
     return
     
  endif else begin

     fname = filename

     
     getpsoffset,xsize,ysize,xoffset,yoffset,LANDSCAPE=landscape,CANCEL=cancel
     if cancel then return

     if n_elements(FONT_SIZE) eq 0 then font_size=10
     
     odevice = !d.name
     set_plot,'ps' 
     
     device, FILENAME=filename,LANDSCAPE=landscape,XSIZE=xsize,YSIZE=ysize,$
             XOFFSET=xoffset,YOFFSET=yoffset,/INCHES,_EXTRA=_extra,/COLOR,$
             /HELVETICA,/ISOLATIN1,FONT_SIZE=font_size
     
  endelse

end
