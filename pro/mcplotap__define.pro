function mcplotap::init
  
  void={mcplotap, $
        x:ptr_new(),$
        color:ptr_new()}


  self.x = ptr_new(/allocate)
  self.color = ptr_new(/allocate)

  return,1
  
end
;
;=============================================================================
;
pro mcplotap::set,struc,color


  *self.x = struc
  *self.color = color


  return 
  
end
;
;=============================================================================
;
pro mcplotap::plot

  ntags = n_tags(*self.x)

  for i = 0,ntags-1 do begin

     
     arr = (*self.x).(i)
     oplot,arr[*,0],arr[*,1],COLOR=*self.color
  
  endfor

  return
  
end
;
;=============================================================================
;
function mcplotap::cleanup

;-- free memory allocated to pointer when destroying object

  ptr_free, self.x
  ptr_free, self.color

 return,1

end 

;
;=============================================================================
;
pro mcplotap__define
 
  void={mcplotap, $
        x:ptr_new(),$
        color:ptr_new()}


  return 
 
end
