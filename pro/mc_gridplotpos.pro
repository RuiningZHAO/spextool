;+
; NAME:
;     mc_gridplotpos
;
; PURPOSE: 
;     To generate the normalized positions for a grid of plots.
;
; CATEGORY:
;     Plotting
;
; CALLING SEQUENCE:
;     result = mc_gridplotpos(plotpos,layout,spacing,SQUARE=square,$
;                             COLUMN=column,CANCEL=cancel)
;
; INPUTS:
;     plotpos - The normalized coordinates [x0,y0,x1,y1] of the entire grid of
;               plots, .e.g., [0.05,0.05,0.95,0.95]
;     layout  - A 2 element array [ncol,nrow] giving the number of
;               columns and rows.
;     spacing - The space between the plots in normalized coordinates.
;               If 1 element, the same spacing is used for both x and
;               y.  If 2 element, then [xspacing,yspacing].
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     SQUARE - Set to require that the plots are square.
;     COLUMN - Set to return the plots in column order instead of row order.
;     CANCEL - Set on return if there is a problem.
;
; OUTPUTS: 
;        An array [4,ncol*nrow] of normalized coordinates for each
;        plot.  The plotting starts at the upper left, and proceeds along
;        each row.  If COLUMN is set, then the plotting starts at the
;        upper left and proceeds down each column.
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
;     Uses mc_cpar.pro from the Spextool package.
;
;     ****MUST BE CALLED AFTER THE DEVICE IS SET UP***
;
; PROCEDURE:
;     
;
; EXAMPLE:
;     positions = mc_gridplotpos([0.05,0.05,0.95,0.95],[2,3],0.02)
;     for i = 0,5 do plot,[1],[1],POSITION=position[*,i],NOERASE=i,TITLE='i'
;
;     Will create a 2x3 grid of plots starting at the upper right and
;     moving along each row.
;
; MODIFICATION HISTORY:
;     2007-11-18 - Written by M. Cushing, Institute for Astronomy,
;                  University of Hawai'i.  Adapted from code by
;                  K. Cruz.
;     2008-08-04 - Added the COLUMN and SQUARE keyword.
;
;-
function mc_gridplotpos,plotpos,layout,spacing,SQUARE=square,COLUMN=column, $
                        CANCEL=cancel

  cancel = 0

  if n_params() ne 3 then begin
     
     print, 'Sytax-result = mc_gridplotpos(plotpos,layout,spacing,' + $
            '                              SQUARE=square,COLUMN=column,$'
     print, '                              CANCEL=cancel)'
     cancel = 1
     return, -1
     
  endif
  
  cancel = mc_cpar('mc_gridplotpos',plotpos,'Plotpos',1,[1,2,3,4,5],1)
  if cancel then return,-1
  cancel = mc_cpar('mc_gridplotpos',layout,'Layout',2,[1,2,3,4,5],1)
  if cancel then return,-1
  cancel = mc_cpar('mc_gridplotpos',spacing,'Spacing',3,[1,2,3,4,5],[0,1])
  if cancel then return,-1

  aratio = !d.y_size/float(!d.x_size)

;  Scale spacing for the aspect ratio of the plotting device

  if n_elements(spacing) eq 1 then begin
     
     xspacing = spacing
     yspacing = spacing

     if aratio gt 1 then xspacing = xspacing*aratio else $
        yspacing = yspacing/aratio
     
  endif else begin
     
     xspacing = spacing[0]
     yspacing = spacing[1]
     
  endelse

;  Determine the plot sizes in normalized coordinates

  xtot = (plotpos[2]-plotpos[0]-(layout[0]-1)*xspacing)/float(layout[0])
  ytot = (plotpos[3]-plotpos[1]-(layout[1]-1)*yspacing)/float(layout[1])

  tplotpos = plotpos

  if keyword_set(SQUARE) then begin

     if xtot/aratio gt ytot then $
        xtot = ytot*aratio else ytot = xtot/aratio

     delx = (plotpos[2]-plotpos[0]-xtot*layout[0] - $
             (xspacing*(layout[0]-1)))/2.
     
     tplotpos[[0,2]] = [plotpos[0]+delx,plotpos[2]-delx]
     
     dely = (plotpos[3]-plotpos[1]-ytot*layout[1] - $
             (yspacing*(layout[1]-1)))/2.
     
     tplotpos[[1,3]] = [plotpos[1]+dely,plotpos[3]-dely]
     
  endif 

  position = fltarr(4,layout[0]*layout[1])
  
  l = 0
  for i =0,layout[1]-1 do begin
     
     for j = 0,layout[0]-1 do begin
        
        position[1,l] = tplotpos[3]-(i+1)*ytot-i*yspacing
        position[3,l] = tplotpos[3]-i*ytot-i*yspacing
        
        position[0,l] = tplotpos[0]+j*xtot+j*xspacing
        position[2,l] = tplotpos[0]+(j+1)*xtot+j*xspacing
        
        l = l+1
        
     endfor
     
  endfor
  
  if keyword_set(COLUMN) then begin

     idx = reform(transpose(indgen(layout[0],layout[1])),layout[0]*layout[1])
     position = position[*,idx]

  endif

  return, position

end
