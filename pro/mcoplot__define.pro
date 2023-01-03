function mcoplot::init
  
  void={mcoplot, $
        label:ptr_new(),$
        dolabel:0,$
        x:ptr_new(),$
        y:ptr_new(),$
        extra:ptr_new()}


  self.x = ptr_new(/allocate)
  self.y = ptr_new(/allocate)
  self.extra = ptr_new(/allocate)
  self.label = ptr_new(/allocate)

  return,1
  
end
;
;=============================================================================
;
pro mcoplot::set,x,y,label,_EXTRA=extra


  *self.x = x
  *self.y = y

  if n_params() gt 2 then begin
     
     *self.label = label
     self.dolabel = 1

  endif

  if n_elements(EXTRA) ne 0 then *self.extra = extra

  return 
  
end
;
;=============================================================================
;
pro mcoplot::plot

  
  if self.dolabel then begin
     
     for i = 0,n_elements(*self.x)-1 do xyouts,(*self.x)[i],(*self.y)[i],$
                                               (*self.label)[i], $
                                               ALIGNMENT=0.5,_EXTRA=*self.extra


  endif else begin

     oplot,*self.x,*self.y,_EXTRA=*self.extra


  endelse
  
  return
  
end
;
;=============================================================================
;
function mcoplot::cleanup

;-- free memory allocated to pointer when destroying object

  ptr_free, self.x
  ptr_free, self.y
  ptr_free, self.extra
  ptr_free, self.label

 return,1

end 

;
;=============================================================================
;
pro mcoplot__define
 
  void={mcoplot, $
        label:ptr_new(),$
        dolabel:0,$
        x:ptr_new(),$
        y:ptr_new(),$
        extra:ptr_new()}


  return 
 
end
