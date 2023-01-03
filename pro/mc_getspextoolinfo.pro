;+
; NAME:
;     mc_getspextoolinfo
;
; PURPOSE:
;     To obtain basic spextool information like paths and instrument information
;
; CALLING SEQUENCE:
;     mc_getspextoolinfo,spextoolpath,packagepath,instr,notspex,version,$
;                        INSTRUMENT=instrument,CANCEL=cancel
;
; INPUTS:
;     NA
;
; OPTIONAL INPUTS:
;     NA
;
; KEYWORD PARAMETERS:
;     INSTRUMENT - A string 
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

pro mc_getspextoolinfo,spextoolpath,packagepath,instr,notspex,version, $
                       INSTRUMENT=instrument,CANCEL=cancel

  cancel = 0

;  Get Spextool path

  spextoolpath = file_dirname(file_dirname($
                 file_which('Spextool_Instruments.dat')),/MARK)
  
;  Check to see if the instrument was passed in.
  
  if ~keyword_set(INSTRUMENT) then begin
     
     readcol,filepath('Spextool_Instruments.dat',ROOT_DIR=spextoolpath, $
                      SUBDIR='data'),instrument,COMMENT='#',FORMAT='A'
     
     instrument = strlowcase(instrument[0])
     instrfile = strcompress(instrument,/RE)+'.dat'
     
  endif else begin
     
     instrument = strlowcase(instrument)
     instrfile = instrument+'.dat'
     
  endelse
  
;  Check to make sure the instrument file exists.

  check = file_which(instrfile)

  if check eq '' then begin

     print
     print, 'Error:  Spextool does not recognize the instrument '+instrument+'.'
     print
     cancel = 1
     return

  endif

;  Note whether it is spex or not.
  
  notspex = (instrument[0] ne 'spex' and instrument[0] ne 'uspex') ? 1:0

;  Get package path.

  packagepath = file_dirname(file_dirname(check),/MARK)
  
;  Get instrument info and default settings
  
  instr = mc_readinstrfile(filepath(instrfile, $
                                    ROOT_DIR=packagepath,SUBDIR='data'), $
                           CANCEL=cancel)
  if cancel then return

;  Get version number

  readcol,file_which('Version.dat'),version,FORMAT='A'
  

end
