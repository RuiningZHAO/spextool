;+
; NAME:
;     mc_rotcoord
;
; PURPOSE:
;     To rotate and unrotate coordinates that are transformed using
;     the IDL rotate function.
;
; CATEGORY:
;     Image analysis
;
; CALLING SEQUENCE:
;     mc_rotcoord,x,y,ncols,nrows,rotation,nx,ny,ROTATE=rotate,$
;                 UNROTATE=unrotate,CANCEL=cancel
;
; INPUTS:
;
;
;
; OPTIONAL INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;
;
;
; OUTPUTS:
;
;
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS:
;
;
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;
;
;
; PROCEDURE:
;
;
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
;-


pro mc_rotcoord,x,y,ncols,nrows,rotation,nx,ny,ROTATE=rotate, $
                UNROTATE=unrotate,CANCEL=cancel

  cancel = 0

  if keyword_set(UNROTATE) then begin

     case rotation of 

        0: begin

           nx = x
           ny = y
           
           end

        1: begin

           nx = y
           ny = -1*x + ncols-1

        end

        2: begin

           nx = -1*x + ncols - 1
           ny = -1*y + nrows - 1

        end

        3: begin
           
           nx = -1*y + nrows-1
           ny = x

        end

        4: begin

           nx = y
           ny = x

        end

        5: begin

           nx = ncols-x-1
           ny = y

        end

        6: begin
           
           nx = -1*y + nrows - 1
           ny = -1*x + ncols - 1

        end

        7: begin

           nx = x
           ny = -1*y + nrows -1

        end

     end

  endif else begin

     case rotation of 

        0: begin

           nx = x
           ny = y

        end
        
        1: begin

           nx = nrows-y-1
           ny = x

        end

        2: begin

           nx = ncols-x-1
           ny = nrows-y-1

        end

        3: begin

           nx = y
           ny = ncols-x-1

        end

        4: begin

           nx = y
           ny = x

        end

        5: begin

           nx = ncols-x-1
           ny = y

        end

        6: begin

           nx = nrows-y-1
           ny = ncols-x-1

        end

        7: begin

           nx = x
           ny = nrows-y-1

        end

     endcase
     
  endelse

end
