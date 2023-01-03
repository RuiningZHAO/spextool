function mcoplotedge::init
  
  self.edgecoeffs = ptr_new(/allocate)
  self.xranges = ptr_new(/allocate)

  return,1
  
end
;
;=============================================================================
;
pro mcoplotedge::set,xranges,edgecoeffs

  *self.edgecoeffs = edgecoeffs
  *self.xranges = xranges

  return 
  
end
;
;=============================================================================
;
pro mcoplotedge::plot

  s = size(*self.edgecoeffs)
  norders = (s[0] eq 2) ? 1:s[3]
  

  for i = 0,norders-1 do begin
     
     del = (*self.xranges)[1,i]-(*self.xranges)[0,i]+1
     x = findgen(del)+(*self.xranges)[0,i]
     
     oplot,x,poly(x,(*self.edgecoeffs)[*,0,i]),COLOR=3
     oplot,x,poly(x,(*self.edgecoeffs)[*,1,i]),COLOR=3
     
  endfor
     
  return
  
end
;
;=============================================================================
;
function mcoplotedge::cleanup

;-- free memory allocated to pointer when destroying object

 ptr_free,self.edgecoeffs
 ptr_free,self.xranges

 return,1

end 

;
;=============================================================================
;
pro mcoplotedge__define
 
  void={mcoplotedge, $
        xranges:ptr_new(),$
        edgecoeffs:ptr_new()}
  return 
 
end
