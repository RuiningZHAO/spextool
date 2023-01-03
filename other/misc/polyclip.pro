;+
; NAME:
;
;    POLYCLIP
;
; DESCRIPTION:
;
;    Clips a polygon to a square unit pixel, using the
;    Sutherland-Hodgman polygon clipping algorithm.
;
; CATEGORY:
;
;    GRAPHICS, REGION OF INTEREST
;
; CALLING SEQUENCE:
;
;    polyclip,i,j,x,y
;
; INPUTS:
;
;    i,j: The pixel coordinates to which to clip.
;
;    x,y: The vectors containing the x and y subscripts of the
;       polygon.  May be in fractional units; pixel centers for pixel
;       (i,j) is at (x,y)=(i+.5,j+.5).  Modified on output
;
; OUTPUTS:
;
;    x,y: X and Y coordinates of the clipped polygon vertices,
;       modified on output.  If a pixel is fully outside the specified
;       polygon, x=-1 is returned.
;
; EXAMPLE:
;
;    x=[1.2,3,5.3,3.2] & y=[1.3,6.4,4.3,2.2]
;    polyclip,5,4,x,y
;
; MODIFICATION HISTORY:
;
;
;       2002-08-27 (J.D. Smith): Migrated from SMART codebase
;       2002-07-28 (J.D. Smith): Optimized, inlining clip_*
;       2001-09-26 (J.D. Smith): Written and documented
;-
;    $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2001,2002 J.D. Smith
;
;  This file is free software; you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published
;  by the Free Software Foundation; either version 2, or (at your
;  option) any later version.
;  
;  This file is distributed in the hope that it will be useful, but
;  WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;  General Public License for more details.
;  
;  You should have received a copy of the GNU General Public License
;  along with this file; see the file COPYING.  If not, write to the
;  Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;  Boston, MA 02110-1301, USA.
;
;##############################################################################

pro polyclip,i,j,pol_x,pol_y
  ;; Sutherland-Hodgman's polygon-clipping algorithm for a square unit pixel
  ;;       A polygon edge looks like:
  ;;                 s-------->p
  np=n_elements(pol_x) 
  nout=np+4L
  py_out=(px_out=fltarr(nout,/nozero)) ;room for full clipped (convex) polygon
  clip_vals=[i,i+1,j+1,j]
  
  for ctype=0b,3b do begin      ;clip left, right, top, bottom
     case ctype of              ;which points are inside this clipping line?
        0b: in=pol_x gt i       ;  left
        1b: in=pol_x lt i+1     ;  right
        2b: in=pol_y lt j+1     ;  top
        3b: in=pol_y gt j       ;  bottom
     endcase 
     
     ;; entirely inside..... no change to polygon
     if array_equal(in,1b) then continue
     
     cv=clip_vals[ctype]
     
     ;; does the segment cross the clipping line?
     crosses=shift(in,1) XOR in
     pind=0L                    ;the index into the vectors
     for k=0L,np-1L do begin
        px=pol_x[k] & py=pol_y[k]
        if crosses[k] then begin ; in->out or out->in, add intersection
           ;; Put the intersection point on the list
           ind=k eq 0?np-1:k-1
           sx=pol_x[ind] & sy=pol_y[ind]
           switch ctype of
              0b:               ;left
              1b: begin         ;or right
                 px_out[pind]=cv
                 py_out[pind]=sy+(py-sy)/(px-sx)*(cv-sx)
                 break
              end
              2b:               ;top
              3b: begin         ;or bottom
                 px_out[pind]=sx+(px-sx)/(py-sy)*(cv-sy)
                 py_out[pind]=cv
                 break
              end
           endswitch
           pind++
        endif
        if in[k] then begin ; out->in or in->in, add 2nd point
           px_out[pind]=px & py_out[pind]=py
           pind++
        endif 
        if pind ge nout-2 then begin ;ran out of room, increase size
           px_out=[px_out,fltarr(nout,/NOZERO)]
           py_out=[py_out,fltarr(nout,/NOZERO)]
           nout*=2L
        endif 
     endfor 
     if pind eq 0 then begin   ; polygon is entirely outside this line
        pol_x=-1
        return 
     endif 
     np=pind & pol_x=px_out[0:np-1] & pol_y=py_out[0:np-1] ;swap
  endfor
end
