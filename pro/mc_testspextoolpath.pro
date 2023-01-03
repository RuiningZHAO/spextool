;+
; NAME:
;     mc_testspextoolpath
;
; PURPOSE:
;     To determine whether the Spextool directory and IDL directories
;     have been properly included in the IDL_PATH variable.
;
; CALLING SEQUENCE:
;     mc_testspextoolpath,CANCEL=cancel
;
; INPUTS:
;     None
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     CANCEL - Set on return if there is a problem.
;
; OUTPUTS:
;     None
;
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     None
;
; RESTRICTIONS:
;     Can only be used with Spextool.
;
; DEPENDENCIES:
;     Spextool 
;
; PROCEDURE:
;     Attempts to run a program to test whether the IDL routines are
;     in the path and if so, attempts to find two files in the
;     Spextool directories to make sure the Spextool path is right.
;
; EXAMPLES:
;     mc_testspextoolpath
;
; MODIFICATION HISTORY:
;     2014-09-12 - Written by M. Cushing, University of Toledo
;-
pro mc_testspextoolpath,CANCEL=cancel

  cancel = 0

;  Check main IDL libraries.  It will simply break if it doesn't work.

  file = file_which('xloadct.pro')

;  Check Spextool path

  file1 = file_which('xspextool.pro')
  file2 = file_which('uspex.dat')

  if file1 eq '' or file2 eq '' then begin

     cancel = 1
     print
     print, 'The Spextool directories are not in your IDL path.' 
     return

  endif else begin

     readcol,file_which('Version.dat'),version,FORMAT='A'
     print
     print, 'Spextool version '+version[0]+' loaded.'

  endelse

  file = file_which('sixty.pro')

  if file eq '' then begin

     cancel = 1
     print
     print, "The Astronomy User's Library directories are not in your IDL path."
     return

  endif else begin

     print
     print, "The Astronomy User's Library directories are in your IDL path."
     
  endelse

  file = file_which('mpfitfun.pro')

  if file eq '' then begin

     cancel = 1
     print
     print, "The Markwardt MPFIT Library directories are not in your IDL path."
     return

  endif else begin

     print
     print, "The Markwardt MPFIT Library directories are in your IDL path."

  endelse

  print
  print, 'The IDL paths are set correctly.'

end
