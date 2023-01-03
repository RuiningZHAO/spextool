;+
; NAME:
;     mc_checkdups
;
; PURPOSE:
;     Check for files that have the same index number.
;
; CALLING SEQUENCE:
;     result = mc_checkdups(path,files,ni,WIDGET_ID=widget_id,CANCEL=cancel)
;
; INPUTS:
;     path   - A scalar string giving the directory with the data.
;     files - A vector string of file index numbers.
;     ni    - A scalar integer giving the Number of integer positions, e.g.
;             for spc0025.fits, ni=4
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     WIDGET_ID - If given, an pop-up error message will appear over
;                 the widget. 
;     CANCEL    - Set on return if there is a problem.
;
; OUTPUTS:
;      The program will pop-up and error message describing which
;      file numbers have duplicate files.
;
; OPTIONAL OUTPUTS:
;      None
;
; COMMON BLOCKS:
;      None
;
; RESTRICTIONS:
;      Requires the Spextool package.
;
; DEPENDENCIES:
;      cpar
;
; PROCEDURE:
;      Just some error checking.
;
; EXAMPLES:
;      result = mc_checkdups('/data/raw/',['1','2'],4,CANCEL=cancel)
;
; MODIFICATION HISTORY:
;      2014-06-16 - Written by M. Cushing, University of Toledo
;-
function mc_checkdups,path,files,ni,WIDGET_ID=widget_id,CANCEL=cancel

  cancel = 0

  ;  Check parameters
  
  if n_params() lt 3 then begin
     
     cancel = 1
     print, 'Syntax - result = mc_checkdups(path,files,ni,WIDGET_ID=widget_id,$'
     print, '                               CANCEL=cancel)'
     return, -1  
     
  endif
  
  cancel = mc_cpar('mc_checkdups',path,1,'Path',7,0)
  if cancel then return,-1
  cancel = mc_cpar('mc_checkdups',files,2,'Files',7,[0,1])
  if cancel then return,-1
  cancel = mc_cpar('mc_checkdups',ni,3,'Ni',[1,2,3],0)
  if cancel then return,-1

  widget = (n_elements(WIDGET_ID) ne 0) ? 1:0

  nfiles = n_elements(files) 

  for i = 0,nfiles-1 do begin

     fmt= '(i'+strtrim(ni,2)+'.'+strtrim(ni,2)+')'
     numstring = string(files[i],FORMAT=fmt)
     result = file_search(path,'*'+numstring+'*',COUNT=cnt)

     if cnt gt 1 then begin

        cancel = 1
        mess = [['Two files with "'+numstring+'" in their names ' + $
                 'exist in the directory '],$
                [path],$
               ['Each file must have a unique index number.']]
        if widget then ok = dialog_message(mess,/ERROR,$
                                           DIALOG_PARENT=widget_id)
        if not widget then print, mess
        
        return, -1

     endif

  endfor

  return, 0

end
