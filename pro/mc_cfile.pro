;+
; NAME:
;     mc_cfile 
;
; PURPOSE:
;     Determines if a file exists.
;
; CATEGORY:
;     File I/O
;
; CALLING SEQUENCE:
;     result = mc_cfile(filename,WIDGET_ID=widget_id,SILENT=silent,CANCEL=cancel)
;
; INPUTS:
;     filename - String name of a filename to check
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     WIDGET_ID - If given, an pop-up error message will appear over
;                 the widget
;     SILENT    - Set to supress error message.
;     CANCEL    - Set on return if the file does not exist or if there 
;                 is a problem
;
; OUTPUTS:
;     If the file is good, the filename is return.  If not, -1 is
;     return.
;    
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     None
;
; SIDE EFFECTS:
;     A pop-up widget will appear if there is a problem with the file
;     AND if WIDGET_ID is given.
;
; RESTRICTIONS:
;     None
;
; PROCEDURE:
;     Uses the file_info routine.    
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;     Written 2000-13-07 by M. Cushing, Institute for Astronomy, UH
;             2007-02-20 - Added SILENT keyword, M. Cushing,
;                          Steward Observatory, University of Arizona
;             2008-03-06 - Now uses the file_info program.
;             2014-05-16 - Renamed mc_cfile.
;             2014-10-09 - Modified to use file_search and return the
;                          fullpath without wildcards because
;                          file_info doesn't work on windows with a wildcard. 
;-
function mc_cfile,filename,WIDGET_ID=widget_id,SILENT=silent,CANCEL=cancel

  cancel = 0

;  Check parameters

  if n_params() lt 1 then begin
     
     print, 'Syntax -  result = mc_cfile(filename,WIDGET_ID=widget_id,$'
     print, '                            SILENT=silent,CANCEL=cancel)'
     cancel = 1
     return, -1
     
  endif
  cancel = mc_cpar('mc_cfile',filename,1,'File name',7,0)
  if cancel then return,-1
  
  
  widget = (n_elements(widget_id) ne 0) ? 1:0
  mess = 'File '+filename+' does not exist.'

  file = file_search(filename,COUNT=cnt)
  if cnt eq 0 then begin
     
     if not keyword_set(SILENT) then begin
        
        if widget then ok = dialog_message(mess,/ERROR,DIALOG_PARENT=widget_id)
        if not widget then begin

           print
           print, mess
           print

        endif
     endif
     
     cancel=1
     return, -1
     
  endif 
  return, file[0]

end        

