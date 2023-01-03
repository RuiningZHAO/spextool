;+
; NAME:
;     mc_loopprogress
;
; PURPOSE:
;     To update the progress of a for loop on the command line, ala FTP.
;
; CALLING SEQUENCE:
;     mc_loopprogress,idx,loopbot,looptop,MESSAGE=message,MODVAL=modval,$
;     CANCEL=cancel
;
; INPUTS:
;     idx     - The current index value of the loop.
;     loopbot - The bottom loop value
;     looptop - The top loop value
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     MESSAGE - An optional message printed above the timer.
;     MODVAL  - Will only run if not idx mod MODVAL.
;     CANCEL  - Set on return if there is a problem.
;
; OUTPUTS:
;     Writes the progress to the command line.
;
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     None
;
; RESTRICTIONS:
;     Must be an increasing loop number and a delta of 1 (will fix later).
;
; DEPENDENCIES:
;     None
;
; PROCEDURE:
;     Just a fancy string.
;
; EXAMPLES:
;
;     If embedded in a for loop such as:
;
;       for i = 10,100 do begin
;
;          mc_loopprogress,i,10,100
;
;       endfor
;
;     It will return the follow:
;
;     22% |****************                                         |
;
; MODIFICATION HISTORY:
;     2009-05-29 - Written by M. Cushing, Institute for Astronomy,
;                  University of Hawaii
;-
pro mc_loopprogress,idx,loopbot,looptop,MODVAL=modval,MESSAGE=message,$
                    CANCEL=cancel

  cancel = 0
  
  !except = 0
  if n_elements(MODVAL) eq 0 then modval = 0
  !except = 1  

;  if modval mod idx ne 0 then return

  frac = float(idx+1-loopbot)/(looptop-loopbot+1)
  
  if keyword_set(MESSAGE) and idx eq loopbot then begin

     print
     print, message

  endif
  print, strjoin(replicate(string(8B),77)), $
         round(frac*100.), $
         strjoin(replicate('*',(round(frac*70) > 1))) ,$
         FORMAT="($,A77,I3,'%',1x,'|',A-70,'|')"
  
   if idx ge (looptop-modval) then print


end
