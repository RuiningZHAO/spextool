;+
; NAME:
;     mc_mergespec
;
; PURPOSE:
;     To combine two spectra into a single spectrum.
;
; CATEGORY:
;     Spectroscopy
;
; CALLING SEQUENCE:
;     mc_mergespec,w1,f1,w2,f2,owave,flux,[overlap],E1=e1,E2=e2,OERROR=oerror,$
;                  BF1=bf1,BF2=bf2,OBF=obf,OSUM=sum,CANCEL=cancel
;
; INPUTS:
;     w1 - The 1st wavelength array. 
;     f1 - The 1st flux array. This is the anchor spectrum onto which
;          w2/f2 are interpolated onto. 
;     w2 - The 2nd wavelength array 
;     f2 - The 2nd flux array. This is the add spectrum which is
;          interpolated onto w1/f1.
;
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     E1     - The error for f1
;     E2     - The error for f2
;     OERROR - The output error array for flux.
;     BF1    - The bitflag for f1
;     BF2    - The bitflag for f2
;     OBF    - The output bitflag array.
;     SUM    - Set to sum the flux instead of averaging the flux.
;     CANCEL - Set on return if there is a problem.
;
; OUTPUTS:
;     owave - The merged wavelength array.
;     oflux - The merged flux array.
;
; OPTIONAL OUTPUTS:
;     overlap - The wavelength range over which the spectra overlap.
;
; COMMON BLOCKS:
;     None
;
; SIDE EFFECTS:
;     If the two spectra do not overlap, then the two edge pixels are
;     set to NaN for IDL plotting purposes.
;
; RESTRICTIONS:
;     None
;
; PROCEDURE:
;     Later
;
; EXAMPLE:
;     Later
;
; MODIFICATION HISTORY:
;     2007-07-03 - Written by M. Cushing, Steward Observatory,
;                  University of Arizona
;     2008-05-02 - Added the SUM keyword.
;     2009-09-22 - Added the OVERLAP output.
;     2015-01-08 - Added the BF1, BF2, and OBF keywords.
;     2015-07-05 - Fixed a bug whereby the the merging was failing if
;                  there were NaNs sitting in the middle of the spectrum.
;-
pro mc_mergespec,w1,f1,w2,f2,owave,oflux,overlap,E1=e1,E2=e2,OERROR=oerror, $
                 BF1=bf1,BF2=bf2,OBITFLAG=obitflag,SUM=sum,CANCEL=cancel

  cancel = 0

  if n_params() lt 4 then begin
     
     print, 'Syntax - mc_mergespec,w1,f1,w2,f2,owave,oflux,E1=e1,E2=e2,$', $
     print, '                      OERROR=oerror,BF1=bf1,BF2=bf2,OBF=obf,$'
     print, '                      SUM=sum,CANCEL=cancel'
     cancel = 0
     return
     
  endif
  cancel = mc_cpar('mc_mergespec',w1,1,'W1',[2,3,4,5],1)
  if cancel then return
  cancel = mc_cpar('mc_mergespec',f1,2,'F1',[2,3,4,5],1)
  if cancel then return
  cancel = mc_cpar('mc_mergespec',w2,3,'W2',[2,3,4,5],1)
  if cancel then return
  cancel = mc_cpar('mc_mergespec',f2,4,'F2',[2,3,4,5],1)
  if cancel then return
  
;  Check for errors
  
  if n_elements(E1) eq 0 and n_elements(E2) eq 0 then begin
     
     e1 = f1
     e1[*] = 1.0
     
     e2 = f2
     e2[*] = 1.0
     
  endif

  if n_elements(BF1) eq 0 and n_elements(BF1) eq 0 then begin
     
     bf1 = byte(f1)
     bf1[*] = 0

     bf2 = byte(f2)
     bf1[*] = 0
         
  endif
  
  if n_elements(E1) eq 0 and n_elements(E2) gt 0 then begin
     
     print, 'An error array was not passed into E1.'
     cancel = 1
     return
     
  endif
  
  if n_elements(E1) gt 0 and n_elements(E2) eq 0 then begin
     
     print, 'An error array was not passed into E2.'
     cancel = 1
     return
     
  endif
  
;  Trim NaNs from the beginning and ends of the spectra.

  idx = mc_nantrim(f1,2,CANCEL=cancel)
  if cancel then return
  nw1  = w1[idx]
  nf1  = f1[idx]
  ne1  = e1[idx]
  nbf1 = byte(bf1[idx])
  n1 = n_elements(nw1)
  
  idx = mc_nantrim(f2,2,CANCEL=cancel)
  if cancel then return
  nw2  = w2[idx]
  nf2  = f2[idx]
  ne2  = e2[idx]
  nbf2 = byte(bf2[idx])
  n2 = n_elements(nw2)
  
;  Get min and max wavelengths
  
  min1 = min(nw1,MAX=max1)
  min2 = min(nw2,MAX=max2)

;  Find position of w2/f2 with respect to w1/f1.
  
  if min2 lt min1 then pos = 'Left' else $
     if max2 gt max1 then pos = 'Right' else pos = 'Inside'

  case pos of 

     'Right': begin

;  Do they overlap?
        
        if min2 gt max1 then begin
         
;  Set the two edge pixels to NaN for plotting purposes.
           
           owave    = [nw1,nw2]
           oflux    = [nf1[0:(n1-2)],!values.f_nan,!values.f_nan,nf2[1:*]]
           oerror   = [ne1[0:(n1-2)],!values.f_nan,!values.f_nan,ne2[1:*]]
           obitflag = [nbf1[0:(n1-2)],0,0,nbf2[1:*]]
           overlap  = [!values.f_nan,!values.f_nan]
                
        endif else begin
           
;  Nope, start the merging.  Get left part first.
           
           z = where(nw1 lt min2,cnt)
           
           lw   = nw1[z[0:*]]
           lf   = nf1[z[0:*]]
           lerr = ne1[z[0:*]]
           lbf  = nbf1[z[0:*]]    
           
;  Get middle (overlap) region
;  Deal with the flux
           
           mc_interpspec,nw2,nf2,nw1,nnf2,nne2,IYERROR=ne2,/LEAVENANS,$
                         CANCEL=cancel
           if cancel then return

;  Find the pixels that are in the overlap region by exploiting the
;  fact that pixels outside the range are set to NaN.
           
           idx = mc_nantrim(nnf2,1,CANCEL=cancel)
           if cancel then return
           
           if keyword_set(SUM) then begin

              nnf2 = total([[nf1],[nnf2]],2)
              nnv2 = total([[ne1],[nne2]]^2,2)
              
           endif else begin

              
              mc_meancomb,[[nf1],[nnf2]],nnf2,nnv2,DATAVAR=[[ne1],[nne2]]^2, $
                          /NAN,CANCEL=cancel
              if cancel then return
              
           endelse

;  Now the flags
           
           mc_interpflagspec,nw2,nbf2,nw1,nnbf2,CANCEL=cancel
           if cancel then return

           nnbf2 = mc_combflagstack([[nbf1],[nnbf2]],CANCEL=cancel)
           if cancel then return

;  Trim NaNs
           
           mw = nw1[idx]
           mf = nnf2[idx]
           merr = sqrt(nnv2[idx])
           mbf = nnbf2[idx]
           
;  Get right part

           z = where(nw2 ge max(mw),cnt)
           
           rw   = nw2[z[1:*]]
           rf   = nf2[z[1:*]]
           rerr = ne2[z[1:*]]
           rbf  = nbf2[z[1:*]]
           
           owave = [lw,mw,rw]
           oflux = [lf,mf,rf]
           oerror = [lerr,merr,rerr]
           obitflag = [lbf,mbf,rbf]
           
           overlap = [min2,max(mw)]

        endelse
        
     end
     
     'Left': begin

;  Do they overlap?
        
        if min1 gt max2 then begin
           
           ;  Set the two inner edge pixels to NaN for plotting purposes.
           
           owave    = [nw2,nw1]
           oflux    = [nf2[0:(n2-2)],!values.f_nan,!values.f_nan,nf1[1:*]]
           oerror   = [ne2[0:(n2-2)],!values.f_nan,!values.f_nan,ne1[1:*]]
           obitflag = [nbf2[0:(n2-2)],0,0,nbf1[1:*]]
           overlap  = [!values.f_nan,!values.f_nan]

        endif else begin
           
;  Nope, so start the merging process.  Get left part
           
           z = where(nw2 le min1,cnt)
           lw = nw2[z[0:cnt-2]]
           lf = nf2[z[0:cnt-2]]
           lerr = ne2[z[0:cnt-2]]
           lbf = nbf2[z[0:cnt-2]]           
           
;  Get (middle) overlap region
;  Deal with the flux
           
           mc_interpspec,nw2,nf2,nw1,nnf2,nne2,IYERROR=ne2,/LEAVENAN, $
                         CANCEL=cancel
           if cancel then return

;  Find the pixels that are in the overlap region by exploiting the
;  fact that pixels outside the range are set to NaN.
           
           idx = mc_nantrim(nnf2,0,CANCEL=cancel)
           if cancel then return
                    
           if keyword_set(SUM) then begin

              nnf2 = total([[nf1],[nnf2]],2)
              nnv2 = total([[ne1],[nne2]]^2,2)
              
           endif else begin

              mc_meancomb,[[nf1],[nnf2]],nnf2,nnv2,DATAVAR=[[ne1],[nne2]]^2, $
                          /NAN,CANCEL=cancel
              if cancel then return
              
           endelse

;  Now the flags

           mc_interpflagspec,nw2,nbf2,nw1,nnbf2,CANCEL=cancel
           if cancel then return
           
           nnbf2 = mc_combflagstack([[nbf1],[nnbf2]],CANCEL=cancel)
           if cancel then return
           
;  Trim NaNs
           
           mw = nw1[idx]
           mf = nnf2[idx]
           merr = sqrt(nnv2[idx])
           mbf = nnbf2[idx]

;  Get right part

           z = where(w1 gt max(mw),cnt)
           rw = nw1[z]
           rf = nf1[z]
           rerr = ne1[z]
           rbf = nbf1[z]
           
           owave = [lw,mw,rw]
           oflux = [lf,mf,rf]
           oerror = [lerr,merr,rerr]
           obitflag = [lbf,mbf,rbf]
           
           overlap = [min1,max(mw)]

        endelse
        
     end
     
     'Inside': begin
        
;  Get left part
        
        z = where(w1 le min2,cnt)
        lw = nw1[z[0:cnt-2]]
        lf = nf1[z[0:cnt-2]]
        lerr = ne1[z[0:cnt-2]]
        lbf = nbf1[z[0:cnt-2]]
        
;  Get right part
        
        z = where(nw1 gt max2,cnt)
        rw = nw1[z]
        rf = nf1[z]
        rerr = ne1[z]
        rbf = nbf1[z]
        
;  Get middle part

        z = where(nw2 gt min1 and nw2 lt max1,cnt)

;  Deal with the flux
        
        mc_interpspec,nw2[z],nf2[z],nw1,nnf2,nne2,IYERROR=ne2,/LEAVENANS,$
                      CANCEL=cancel
        if cancel then return
        
        if keyword_set(SUM) then begin

           nnf2 = total([[nf1],[nnf2]],2)
           nnv2 = total([[ne1],[nne2]]^2,2)
           
        endif else begin

           mc_meancomb,[[nf1],[nnf2]],nnf2,nnv2,DATAVAR=[[ne1],[nne2]]^2, $
                       /NAN,CANCEL=cancel
           if cancel then return
        
        endelse

;  Now the flags

        mc_interpflagspec,nw2[z],nbf2[z],nw1,nnbf2,CANCEL=cancel
        if cancel then return

        nnbf2 = mc_combflagstack([[nbf1],[nnbf2]],CANCEL=cancel)
        if cancel then return
        
;  Trim NaNs
        
        idx = mc_nantrim(nnf2,2,CANCEL=cancel)
        if cancel then return
        
        mw = nw1[idx]
        mf = nnf2[idx]
        merr = sqrt(nnv2[idx])
        mbf = nnbf2[idx]
        
        owave = [lw,mw,rw]
        oflux = [lf,mf,rf]
        oerror = [lerr,merr,rerr]
        obitflag = [lbf,mbf,rbf]
        
        overlap = [min1,max2]

     end
     
  endcase
  
end
