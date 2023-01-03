;+
; NAME:
;     xmc_scalelines
;    
; PURPOSE:
;     Scales the EW of the absorption lines in the Vega spectrum.    
;
; CATEGORY:
;     Widget
;
; CALLING SEQUENCE:
;     xmc_scalelines,std,stdorders,stdmag,stdbmv,wvega,fvega,fcvega,fc2vega,$
;                 kernels,vshift,obj,objorders,objnaps,awave,atrans,$
;                 hlines,hnames,initscale,scales,vrot,PARENT=parent,$
;                 XPUTS=xputs,XTITLE=xtitle,YTITLE=ytitle,CANCEL=cancel
;    
; INPUTS:
;     std        - The standard spectra
;     stdorders  - The order numbers of the standard spectra
;     stdmag     - The V magnitude of the standard
;     stdbmv     - The (B-V) color of the standard
;     wvega      - The wavelength array of the Vega model
;     fvega      - The flux array of the Vega model
;     fcvega     - The continuum array of the Vega model
;     fc2vega    - The fit to the continuum array of the Vega model
;     kernels    - A structure [ntags] of kernels to convolve the Vega model 
;     vshift     - The velocity shift of Vega
;     obj        - The object spectra
;     objorders  - The orders of the object
;     objnaps    - The number of apertures in the object spectra
;     awave      - The atmospheric transmission wavelength array
;     atrans     - The atmopsheric transmission flux array
;     hlines     - An array of hydrogren lines
;     hnames     - A string array of the names of the hydrogen lines
;     initscales - The initial scale factor for the EWs
;     
; OPTIONAL INPUTS:
;     None
;
; KEYWORD PARAMETERS:
;     PARENT   - If given, the widget belonging to PARENT is greyed
;                out
;     XPUTS    - An [*,2] array giving the wavelength and transmission
;                of the throughput of the instrument.  If given, the
;                it is multiplied by the atmospheric transmission.
;     XTITLE   - A string of the x-axis title
;     YTITLE   - A string of the y-axis title
;     CANCEL   - Set on return if there is a problem
;     
; OUTPUTS:
;     vshift - The new velocity shift of Vega
;     scales - An array [*,norders] of scale factors for each order
;     
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     xmc_scalelines_state
;
; SIDE EFFECTS:
;     Greys out PARENT
;
; RESTRICTIONS:
;     None
;
; PROCEDURE:
;     The telluric correction spectrum is displayed for each order.
;     The user then adjusts the scale factor for each line until it disappears.
;
; EXAMPLE:
;     
; MODIFICATION HISTORY:
;     2002 - Written by M. Cushing, Institute for Astronomy, UH
;     2005-08-04 - Changed the XUNITS and YUNITS keywords to XTITLE
;                  and YTITLE
;     2005-10-03 - Fixed a bug where you couldn't turn off the
;                  atmosphere and throughputs
;-
;
;******************************************************************************
;
;---------------------------- Support Procedures ----------------------------
;
;******************************************************************************
;
pro xmc_scalelines_cleanup,base

  widget_control, base, GET_UVALUE = state, /NO_COPY
  if n_elements(state) ne 0 then begin

     ptr_free, state.r.cpoints
     ptr_free, state.r.scalespec

     ptr_free, state.d.deltascales
     ptr_free, state.d.tellspec
     ptr_free, state.d.wave
     ptr_free, state.d.flux
     ptr_free, state.d.error

     ptr_free, state.p.spec

  endif
  state = 0B
  
end
;
;==============================================================================
;
pro xmc_scalelines_initcommon,std,stdorders,stdmag,stdbmv,wvega,fvega,fcvega,$
                           fc2vega,kernels,vshift,obj,objorders,objnaps,awave,$
                           atrans,hlines,hnames,initscale,scales,$
                           XPUTS=xputs,VROT=vrot,XTITLE=xtitle,YTITLE=ytitle

cleanplot,/SILENT

if n_elements(XPUTS) ne 0 then begin

    ixputs = xputs
    doxputs = 1

endif else begin

    ixputs = 0
    doxputs = 0

endelse 

xtitle = n_elements(XTITLE) eq 0 ? '':xtitle
ytitle = n_elements(YTITLE) eq 0 ? '':ytitle

;  Create cut region structure

norders = n_elements(stdorders)

for i = 0,norders-1 do begin

    key = string(i)
    value = replicate(!values.f_nan,1)
    cutreg=(i eq 0) ? create_struct(key,ptr_new(value)):$
      create_struct(cutreg,key,ptr_new(value))

endfor


common xmc_scalelines_state, state

;  Build three structures which will hold important info.

w = {keyboard:0L,$
     message:0L,$
     order_dl:0L,$
     plotwin1:0,$
     plotwin2:0,$
     scaleatmos_fld:[0L,0L],$
     scalezero_fld:[0L,0L],$
     slider:0L,$
     subregscale_fld:[0L,0L],$
     tension_fld:[0L,0L],$
     vshift_fld:[0L,0L],$
     xmc_scalelines_base:0L,$
     xmin_fld:[0L,0L],$
     xmax_fld:[0L,0L],$
     ymin_fld:[0L,0L],$
     ymax_fld:[0L,0L]}

r = {cancel:0,$
     cpoints:ptr_new(fltarr(2)),$
     cursormode:'None',$
     cutreg:cutreg,$
     ereg:[!values.f_nan,!values.f_nan],$
     plotatmos:1,$
     plotxputs:doxputs,$
     scalespec:ptr_new(fltarr(2)),$
     spectype:'Telluric',$
     stdidx:0,$
     stdbmv:stdbmv,$
     stdmag:stdmag,$
     tension:10.,$
     vshift:vshift}

d = {atrans:[[awave],[atrans]],$
     deltascales:ptr_new(fltarr(2)),$
     hlines:hlines,$
     hnames:hnames,$
     initscale:initscale,$
     kernels:kernels,$
     norders:n_elements(stdorders),$
     obj:obj,$
     objorders:objorders,$
     objnaps:objnaps,$
     ofcvega:fcvega,$
     ofc2vega:fc2vega,$
     ofvega:fvega,$
     owvega:wvega,$
     std:std,$
     stdorders:stdorders,$
     tellspec:ptr_new(fltarr(2)),$
     wave:ptr_new(2),$
     flux:ptr_new(2),$
     error:ptr_new(2),$
     xputs:ixputs}

p = {absxrange2:[0.,0.],$
     absyrange2:[0.,0.],$     
     buffer:[0.,0.],$
     cursor:0,$
     modcpt:-1,$
     pixmap1_wid:0L,$
     pixmap2_wid:0L,$
     plot1size:[770,200],$
     plot2size:[770,280],$
     plot1scale:0.,$
     plot2scale:0.,$
     plotwin1_wid:0L,$
     plotwin2_wid:0L,$
     pscale1:!p,$
     pscale2:!p,$
     spec:ptr_new(2),$
     reg:[[!values.f_nan,!values.f_nan],$
          [!values.f_nan,!values.f_nan]],$
     xrange2:[0.,0.],$
     xscale1:!x,$
     xscale2:!x,$
     xtitle:xtitle,$
     yrange1:[0.,0.],$
     yrange2:[0.,0.],$
     yscale1:!y,$
     yscale2:!y,$
     ytitle:[ytitle,'!5Arbitrary Flux']}

p.plot1scale = float(p.plot1size[1])/(p.plot1size[1]+p.plot2size[1])
p.plot2scale = float(p.plot2size[1])/(p.plot1size[1]+p.plot2size[1])

;  Load the three structures in the state structure.

state = {w:w,r:r,d:d,p:p}

;  Load up the initial control points

for i = 0,state.d.norders-1 do begin

    min   = min(state.d.std[*,0,i],MAX=max,/NAN)
    array = [min,initscale]
    z     = where(state.d.hlines gt min and state.d.hlines lt max,count)
    
    if count ne 0 then begin
    
        hlines = state.d.hlines[z]

        for j = 0,count-1 do begin
        
            array = [[array],[hlines[j],initscale]]
            
        endfor

    endif
    array = [[array],[max,initscale]]
    s = sort(array[0,*])
    array = array[*,s]
    
    key = 'Order'+string(i,FORMAT='(i2.2)')
    *state.r.cpoints = (i eq 0) ? create_struct(key,array):$
      create_struct(*state.r.cpoints,key,array)

endfor

end
;
;******************************************************************************
;
pro xmc_scalelines_estimatescale

common xmc_scalelines_state
widget_control, /HOURGLASS

;std_wave = state.d.std[*,0,state.r.stdidx]
;std_flux = state.d.std[*,1,state.r.stdidx]
;std_err  = state.d.std[*,2,state.r.stdidx]

std_wave = *state.d.wave
std_flux = *state.d.flux
std_err  = *state.d.error

tel_flux = 1./*state.d.tellspec

z = where(std_wave gt state.r.ereg[0] and std_wave lt state.r.ereg[1])

;  Get estimates for the peak finding routine

idx = where(state.d.hlines gt state.r.ereg[0] and $
            state.d.hlines lt state.r.ereg[1],count)
if count eq 0 then return
wguess = state.d.hlines[idx]


tabinv,std_wave,wguess,idx
tabinv,std_wave,state.r.ereg[0],idlo
tabinv,std_wave,state.r.ereg[1],idhi
fguess = tel_flux[idx] - 0.5*(tel_flux[idlo] + tel_flux[idhi])
sguess = (state.r.ereg[1]-state.r.ereg[0])/5.
cguess = poly_fit1d(std_wave[z],tel_flux[z],1,/SILENT)
mguess = (tel_flux[idlo] - tel_flux[idhi])/(std_wave[idlo]-std_wave[idhi])

result = mpfitpeak(std_wave[z],tel_flux[z],gcoeff,NTERMS=5,/LORENTZIAN,$
                   ESTIMATES=[fguess,wguess,sguess,cguess,mguess])

wline = gcoeff[1]

wvega = state.d.owvega*(1+state.r.vshift/2.99792458E5)

;  Determine the range over which to convolve the Vega model

wmin = min(std_wave,/NAN,MAX=wmax)
zv   = where(wvega gt wmin and wvega lt wmax,count)

nkern = n_elements(state.d.kernels.(state.r.stdidx))
nadd  = round(nkern/2.)

idx = findgen(count+2*nadd)+(zv[0]-nadd)

;  Do the convolution

rvred    = 3.10
vegabmv  = 0.00
vegamag  = 0.03
magscale = 10.0^(-0.4*(state.r.stdmag-vegamag))
ebmv     = (state.r.stdbmv - vegabmv) > 0.0 

; to prevent de-reddening the spectrum

avred    = rvred*ebmv
redfact  = 10.0^(0.4*avred)
magfact  = replicate(redfact*magscale,n_elements(std_wave))

nfvconv = convol((state.d.ofvega[idx]/state.d.ofcvega[idx]-1.0), $
                 state.d.kernels.(state.r.stdidx))

linterp,wvega[idx],nfvconv,std_wave,rnfvconv
linterp,wvega,state.d.ofcvega,std_wave,rfcvega

;  Redden the convolved Vega model

mc_redden,std_wave,rfcvega,ebmv,rrfcvega

; Determine the scale factor

sgwid = 5
;sgdeg = 2
s = (poly_smooth(std_flux,sgwid)/$
     (magfact*poly(std_wave,gcoeff[3:4])*rrfcvega))- 1.0
s = temporary(s)/rnfvconv

z = where((*state.r.cpoints).(state.r.stdidx)[0,*] gt state.r.ereg[0] and $
          (*state.r.cpoints).(state.r.stdidx)[0,*] lt state.r.ereg[1],count)
if count eq 0 then return
wline = (*state.r.cpoints).(state.r.stdidx)[0,z]

tabinv,std_wave,wline,idx
(*state.r.cpoints).(state.r.stdidx)[1,z] = s[idx]

end
;
;******************************************************************************
;
pro xmc_scalelines_getminmax

common xmc_scalelines_state

state.p.xrange2 = [min((*state.p.spec)[*,0],MAX=max),max]
state.p.yrange2 = [min((*state.p.spec)[*,1],MAX=max,/NAN),max]

state.p.absxrange2 = state.p.xrange2
state.p.absyrange2 = state.p.yrange2

end
;
;******************************************************************************
;
pro xmc_scalelines_makescale

common xmc_scalelines_state

if n_elements((*state.r.cpoints).(state.r.stdidx)[0,*]) eq 2 then begin

   scalespec = replicate(1.0,n_elements(*state.d.wave))
   
endif else begin

   scalespec = spline((*state.r.cpoints).(state.r.stdidx)[0,*],$
                      (*state.r.cpoints).(state.r.stdidx)[1,*],$
                      *state.d.wave,state.r.tension)
   
endelse

*state.r.scalespec = scalespec

end
;
;******************************************************************************
;
pro xmc_scalelines_makespec

common xmc_scalelines_state

case state.r.spectype of

    'Telluric': begin

        wave = *state.d.wave
        spec = *state.d.tellspec
        *state.p.spec = [[wave],[1./spec]]

    end

    'Object': begin

         idx = mc_nantrim(state.d.obj[*,0,state.d.objnaps*state.r.stdidx],2)
         wave = state.d.obj[idx,0,state.d.objnaps*state.r.stdidx]
         spec = state.d.obj[idx,1,state.d.objnaps*state.r.stdidx]

         mc_interpspec,*state.d.wave,*state.d.tellspec,wave,tellspec, $
                       CANCEL=cancel
         if cancel then return
         
         spec = temporary(spec)*tellspec
         *state.p.spec = [[wave],[spec]]
         
      end
    
endcase

end
;
;******************************************************************************
;
pro xmc_scalelines_maketelluric

common xmc_scalelines_state

mc_mktellspec, *state.d.wave,*state.d.flux,*state.d.error, $
               state.r.stdmag,state.r.stdbmv, $
               state.d.kernels.(state.r.stdidx),*state.r.scalespec, $
               state.d.owvega,state.d.ofvega,state.d.ofcvega,$
               state.d.ofc2vega,state.r.vshift,tellcor,CANCEL=cancel
if cancel then return

;  Perform interpolations if necessary

ndat = n_elements(*state.r.cutreg.(state.r.stdidx))

if ndat ne 1 then begin

    nreg = ndat/2
    for i = 0, nreg-1 do begin

        xrange = reform((*state.r.cutreg.(state.r.stdidx))[(i*2):(i*2+1)])
                tabinv,*state.d.wave,xrange,idx
                idx = round(idx)
                
                x = [(*state.d.wave)[idx[0]],(*state.d.wave)[idx[1]]]

                y = [tellcor[idx[0]],tellcor[idx[1]]]
                coeff = poly_fit1d(x,y,1,/SILENT)

                tellcor[idx[0]:idx[1]]=$
                   poly((*state.d.wave)[idx[0]:idx[1]],coeff)

    endfor


endif

*state.d.tellspec = tellcor

end
;
;******************************************************************************
;
pro xmc_scalelines_plotscale

  common xmc_scalelines_state
  
  plotsym,8,1.3,/FILL
  if state.p.modcpt eq -1 then begin
     
     min = min(*state.r.scalespec,max=max)
     del = (max-min)*0.3
     if del lt 1e-2 then del = 0.5
     state.p.yrange1 = [min-del,max+del]
     
  endif
  
  plot,(*state.p.spec)[*,0],*state.r.scalespec,/XSTY,/YSTY,$
       YRANGE=state.p.yrange1,XRANGE=state.p.xrange2,YTITLE='!5Scale Factor',$
       XTITLE=state.p.xtitle,PSYM=10,CHARSIZE=1.5
  
  ncpts = n_elements((*state.r.cpoints).(state.r.stdidx)[0,*])
  
  for i = 0, ncpts-1 do begin
     
     if (*state.r.cpoints).(state.r.stdidx)[0,i] ge state.p.xrange2[0] and $
        (*state.r.cpoints).(state.r.stdidx)[0,i] le state.p.xrange2[1] then $
           plots,[(*state.r.cpoints).(state.r.stdidx)[0,i],$
                  (*state.r.cpoints).(state.r.stdidx)[1,i]],PSYM=8,COLOR=3
     
  endfor
  
  state.p.pscale1 = !p
  state.p.xscale1 = !x
  state.p.yscale1 = !y

end
;
;******************************************************************************
;
pro xmc_scalelines_plotspec

  common xmc_scalelines_state
  
  scale = mc_cfld(state.w.scaleatmos_fld,4,/EMPTY,CANCEL=cancel)
  if cancel then return
  
  z = where(state.d.atrans[*,0] lt state.p.absxrange2[1] and $
            state.d.atrans[*,0] gt state.p.absxrange2[0],count)
  
  wtrans = state.d.atrans[z,0] 
  spec   = state.d.atrans[z,1] 
  spec   = (temporary(spec)-1.0)*scale+1.0
  
  if state.r.plotatmos then begin
     
     z = where( wtrans lt state.p.xrange2[1] and $
                wtrans gt state.p.xrange2[0],count)
     
     if count ne 0 then begin
        
        plot, wtrans,spec,COLOR=5,YRANGE=[0,1],YSTYLE=5,XSTYLE=5,$
              XRANGE=state.p.xrange2,PSYM=10,CHARSIZE=1.5
        ticks = string(findgen(11)*.1,FORMAT='(f3.1)')
        axis,YAXIS=1,YTICKS=10,YTICKNAME='!5'+ticks,YMINOR=1,COLOR=5
                
     endif
     ystyle = 9
     noerase = 1
     
  endif else begin
     
     ystyle=1
     noerase = 0
     
  endelse
  
  case state.r.spectype of
     
     'Telluric': ytitle = state.p.ytitle[0]
     
     'Object': ytitle = state.p.ytitle[1]
     
  endcase
  
  plot,(*state.p.spec)[*,0],(*state.p.spec)[*,1],$
       XTITLE=state.p.xtitle,YTITLE=ytitle,/XSTY,YSTYLE=ystyle,$
       YRANGE=state.p.yrange2,XRANGE=state.p.xrange2,$
       NOERASE=noerase,PSYM=10,CHARSIZE=1.5
  
;  Label H lines
  
  z = where(state.d.hlines lt state.p.xrange2[1] and $
            state.d.hlines gt state.p.xrange2[0],count)
  
  for i =0, count-1 do begin
     
     tabinv,(*state.p.spec)[*,0],state.d.hlines[z[i]],idx
     
     linenorm = convert_coord(state.d.hlines[z[i]],(*state.p.spec)[idx,1], $
                              /DATA,/TO_NORM)

     plotnorm = convert_coord(!x.crange[0],!y.crange[1],/DATA,/TO_NORM)



     ytop = (linenorm[1]+0.2) < (plotnorm[1]-0.03)

     plots,[linenorm[0],linenorm[0]],[linenorm[1],ytop],LINESTYLE=1,COLOR=3, $
           THICK=2,/NORM
     
     xyouts, linenorm[0],ytop+0.005,'!5'+state.d.hnames[z[i]], $
             ORIENTATION=90,/NORM,COLOR=3
     
  endfor
  
  state.p.pscale2 = !p
  state.p.xscale2 = !x
  state.p.yscale2 = !y
  
end
;
;******************************************************************************
;
pro xmc_scalelines_plotupdate1

  common xmc_scalelines_state
  
  wset, state.p.pixmap1_wid
  erase
  xmc_scalelines_plotscale
  
  wset, state.p.plotwin1_wid
  device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
                state.p.pixmap1_wid]

end
;
;******************************************************************************
;
pro xmc_scalelines_plotupdate2

  common xmc_scalelines_state
  
  wset, state.p.pixmap2_wid
  erase
  xmc_scalelines_plotspec 
  
  wset, state.p.plotwin2_wid
  device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,0,$
                state.p.pixmap2_wid]

end
;
;==============================================================================
;
pro xmc_scalelines_selectspec

  common xmc_scalelines_state
  
  idx = mc_nantrim(state.d.std[*,0,state.r.stdidx],2)
  *state.d.wave = reform(state.d.std[idx,0,state.r.stdidx])
  *state.d.flux = reform(state.d.std[idx,1,state.r.stdidx])
  *state.d.error = reform(state.d.std[idx,2,state.r.stdidx])

end
;
;******************************************************************************
;
pro xmc_scalelines_setminmax

common xmc_scalelines_state

widget_control, state.w.xmin_fld[1],SET_VALUE=strtrim(state.p.xrange2[0],2)
widget_control, state.w.xmax_fld[1],SET_VALUE=strtrim(state.p.xrange2[1],2)
widget_control, state.w.ymin_fld[1],SET_VALUE=strtrim(state.p.yrange2[0],2)
widget_control, state.w.ymax_fld[1],SET_VALUE=strtrim(state.p.yrange2[1],2)

end
;
;******************************************************************************
;
pro xmc_scalelines_undocut

common xmc_scalelines_state

ndat = n_elements(*state.r.cutreg.(state.r.stdidx))
if ndat ne 1 then begin

    if ndat eq 2 then *state.r.cutreg.(state.r.stdidx) = !values.f_nan
    if ndat gt 2 then *state.r.cutreg.(state.r.stdidx) = $
      (*state.r.cutreg.(state.r.stdidx))[0:ndat-3]

endif

xmc_scalelines_maketelluric
xmc_scalelines_makespec
xmc_scalelines_plotupdate2   

end
;
;******************************************************************************
;
pro xmc_scalelines_whichpoint,idx

common xmc_scalelines_state

idx = -1
del = state.p.xrange2[1]-state.p.xrange2[0]
ncpts = n_elements((*state.r.cpoints).(state.r.stdidx)[0,*])

for i = 0,ncpts-1 do begin

    if state.p.reg[0,0] gt (*state.r.cpoints).(state.r.stdidx)[0,i]-$
      (del*0.005) and $
      state.p.reg[0,0] lt (*state.r.cpoints).(state.r.stdidx)[0,i]+$
      (del*0.005) then $
      idx = i

endfor

end
;
;******************************************************************************
;
pro xmc_scalelines_zoom,IN=in,OUT=out

common xmc_scalelines_state

delabsx = state.p.absxrange2[1]-state.p.absxrange2[0]
delx    = state.p.xrange2[1]-state.p.xrange2[0]

delabsy = state.p.absyrange2[1]-state.p.absyrange2[0]
dely    = state.p.yrange2[1]-state.p.yrange2[0]
    
xcen = state.p.xrange2[0]+delx/2.
ycen = state.p.yrange2[0]+dely/2.
    
case state.r.cursormode of 
        
    'XZoom': begin
        
        z = alog10(delabsx/delx)/alog10(2)
        if keyword_set(IN) then z = z+1 else z=z-1
        hwin = delabsx/2.^z/2.
        state.p.xrange2 = [xcen-hwin,xcen+hwin]
        xmc_scalelines_plotupdate1
        xmc_scalelines_plotupdate2
        xmc_scalelines_setminmax
        
    end
    
    'YZoom': begin
        
        z = alog10(delabsy/dely)/alog10(2)
        if keyword_set(IN) then z = z+1 else z=z-1
        hwin = delabsy/2.^z/2.
        state.p.yrange2 = [ycen-hwin,ycen+hwin]
        xmc_scalelines_plotupdate2
        xmc_scalelines_setminmax
        
    end
    
    'Zoom': begin
        
        z = alog10(delabsx/delx)/alog10(2)
        if keyword_set(IN) then z = z+1 else z=z-1
        hwin = delabsx/2.^z/2.
        state.p.xrange2 = [xcen-hwin,xcen+hwin]
        
        z = alog10(delabsy/dely)/alog10(2)
        if keyword_set(IN) then z = z+1 else z=z-1
        hwin = delabsy/2.^z/2.
        state.p.yrange2 = [ycen-hwin,ycen+hwin]
        
        xmc_scalelines_plotupdate1
        xmc_scalelines_plotupdate2
        xmc_scalelines_setminmax
        
    end
    
    else:
    
endcase

end
;  
;******************************************************************************
;
;-----------------------------Event Handlers----------------------------------
;
;******************************************************************************
;
pro xmc_scalelines_event,event

common xmc_scalelines_state
widget_control, event.id,  GET_UVALUE = uvalue
widget_control, /HOURGLASS

case uvalue of

    'Accept': widget_control, event.top, /DESTROY

    'Atmosphere': begin

        state.r.plotatmos = event.select
;        widget_control, state.w.scaleatmos_fld[0],SENSITIVE=event.select

        xmc_scalelines_plotupdate2


    end

    'Cancel': begin

        state.r.cancel = 1
        widget_control, event.top, /DESTROY

    end

    'Keyboard': begin

        case strtrim(event.ch,2) of 

            'c': begin

;                if state.r.cursormode eq 'Select Region' then begin
;                 
;                    z = where(finite(state.r.scalereg) eq 1,count)
;                    if count eq 0 then begin
;
;                        xmc_scalelines_modoffsets,$
;                          (*state.r.scalespec)[state.r.scalereg[0,0]-1]
;                        state.r.scalereg[*] = !values.f_nan
;                        widget_control, state.w.regionscale_base, MAP=0
;                        xmc_scalelines_makescale
;                        xmc_scalelines_maketelluric
;                        xmc_scalelines_makespec
;                        
;                    endif else state.r.scalereg[*] = !values.f_nan
;
;                endif

                state.r.cursormode = 'None'
                state.p.reg = !values.f_nan
                xmc_scalelines_plotupdate1
                xmc_scalelines_plotupdate2


            end

            'e': begin

                state.r.cursormode = 'Estimate Scale'
                state.r.ereg = !values.f_nan

            end

            'f': begin

                state.r.cursormode='Fix'
                state.p.reg = !values.f_nan

                
            end

            'i': xmc_scalelines_zoom,/IN

            'o': xmc_scalelines_zoom,/OUT

            'w': begin

                state.p.xrange2 = state.p.absxrange2
                state.p.yrange2 = state.p.absyrange2
                xmc_scalelines_setminmax
                xmc_scalelines_plotupdate1
                xmc_scalelines_plotupdate2

            end

            'u': xmc_scalelines_undocut

            'x': begin 

                state.r.cursormode = 'XZoom'
                state.p.reg = !values.f_nan

            end

            'y': begin 

                state.r.cursormode = 'YZoom'
                state.p.reg = !values.f_nan

            end

            'z': begin

                state.r.cursormode = 'Zoom'
                state.p.reg = !values.f_nan
                
            end
            
            else:

        endcase

    end

    'Order': begin

        state.r.stdidx = event.index
        xmc_scalelines_selectspec
        xmc_scalelines_makescale
        xmc_scalelines_maketelluric
        xmc_scalelines_makespec
        xmc_scalelines_getminmax
        xmc_scalelines_setminmax
        xmc_scalelines_plotupdate1
        xmc_scalelines_plotupdate2        
        
    end

    'Reset Control Points': begin

        (*state.r.cpoints).(state.r.stdidx)[1,*] = state.d.initscale
        xmc_scalelines_makescale
        xmc_scalelines_maketelluric
        xmc_scalelines_makespec
        xmc_scalelines_plotupdate1
        xmc_scalelines_plotupdate2                

    end

    'Scale Atmosphere': xmc_scalelines_plotupdate2

    'Spectrum Type': begin

        state.r.spectype = event.value
        xmc_scalelines_makespec
        xrange = state.p.xrange2
        xmc_scalelines_getminmax
        state.p.xrange2 = xrange
        z = where((*state.p.spec)[*,0] gt state.p.xrange2[0] and $
                  (*state.p.spec)[*,0] lt state.p.xrange2[1])
        state.p.yrange2 = [min((*state.p.spec)[z,1],max=max,/NAN),max]
        xmc_scalelines_setminmax
        xmc_scalelines_plotupdate2


    end

    'Vrot': begin

        val = mc_cfld(state.w.vrot_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then goto, cont
        state.r.vrot = val
        xmc_scalelines_maketelluric
        xmc_scalelines_makespec
        xmc_scalelines_plotupdate2       

    end

    'Vshift': begin
        
        val = mc_cfld(state.w.vshift_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then goto, cont
        state.r.vshift = val
        xmc_scalelines_maketelluric
        xmc_scalelines_makespec
        xmc_scalelines_plotupdate2       

    end

    'Throughputs': begin

        state.r.plotxputs = event.select
        xmc_scalelines_plotupdate2
        
    end

    else:

endcase

cont: 

end
;
;******************************************************************************
;
pro xmc_scalelines_minmaxevent,event

common xmc_scalelines_state
widget_control, event.id,  GET_UVALUE = uvalue

case uvalue of 

    'X Min': begin

        xmin = mc_cfld(state.w.xmin_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then return
        xmin2 = mc_crange(xmin,state.p.xrange2[1],'X Min',/KLT,$
                       WIDGET_ID=state.w.xmc_scalelines_base,CANCEL=cancel)
        if cancel then begin
            
            widget_control, state.w.xmin_fld[0],SET_VALUE=state.p.xrange2[0]
            return
            
        endif else state.p.xrange2[0] = xmin2

    end
    'X Max': begin

        xmax = mc_cfld(state.w.xmax_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then return
        xmax2 = mc_crange(xmax,state.p.xrange2[0],'X Max',/KGT,$
                       WIDGET_ID=state.w.xmc_scalelines_base,CANCEL=cancel)
        if cancel then begin
            
            widget_control, state.w.xmax_fld[0],SET_VALUE=state.p.xrange2[1]
            return
            
        endif else state.p.xrange2[1] = xmax2

    end
    'Y Min': begin

        ymin = mc_cfld(state.w.ymin_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then return
        ymin2 = mc_crange(ymin,state.p.yrange2[1],'Y Min',/KLT,$
                       WIDGET_ID=state.w.xmc_scalelines_base,CANCEL=cancel)
        if cancel then begin
            
            widget_control,state.w.ymin_fld[0],SET_VALUE=state.p.yrange2[0]
            return
            
        endif else state.p.yrange2[0] = ymin2
        
    end
    'Y Max': begin

        ymax = mc_cfld(state.w.ymax_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then return
        ymax2 = mc_crange(ymax,state.p.yrange2[0],'Y Max',/KGT,$
                       WIDGET_ID=state.w.xmc_scalelines_base,CANCEL=cancel)
        if cancel then begin
            
            widget_control,state.w.ymax_fld[0],SET_VALUE=state.p.yrange2[1]
            return
            
        endif else state.p.yrange2[1] = ymax2
        
    end
    
endcase

xmc_scalelines_plotupdate1
xmc_scalelines_plotupdate2

end

;
;******************************************************************************
;
pro xmc_scalelines_plotwin1event,event

common xmc_scalelines_state
widget_control, event.id,  GET_UVALUE = uvalue

;  Check to see if it is a TRACKING event.

if strtrim(tag_names(event,/STRUCTURE_NAME),2) eq 'WIDGET_TRACKING' then begin

    if event.enter eq 0 then begin

        widget_control, state.w.keyboard, SENSITIVE=0
        wset, state.p.plotwin1_wid
        device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
                      state.p.pixmap1_wid]
        wset, state.p.plotwin2_wid
        device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,0,$
                      state.p.pixmap2_wid]
        
    endif
    goto, cont
    
endif else widget_control, state.w.keyboard, /INPUT_FOCUS, /SENSITIVE

wset, state.p.plotwin1_wid

;  Load up plot scales.

!p = state.p.pscale1
!x = state.p.xscale1
!y = state.p.yscale1

;  Determine the wavelength and flux values for the event.

x  = event.x/float(state.p.plot1size[0])  
y  = event.y/float(state.p.plot1size[1])
xy = convert_coord(x,y,/NORMAL,/TO_DATA)

case event.type of 
    
    0: begin
        
        state.p.reg[*,0] = xy[0:1]
        xmc_scalelines_whichpoint,idx
        state.p.modcpt = idx
        
    end
    
    1: begin

        if state.p.modcpt ne -1 then begin

            widget_control, /HOURGLASS
            tmp = state.p.modcpt
            state.p.modcpt = -1
            xmc_scalelines_maketelluric
            xmc_scalelines_makespec
            xmc_scalelines_plotupdate1
            xmc_scalelines_plotupdate2
        
        endif
    end
    
    2: begin
        
        if state.p.modcpt ne -1 then begin
            
            (*state.r.cpoints).(state.r.stdidx)[1,state.p.modcpt] = xy[1]
            xmc_scalelines_makescale
            xmc_scalelines_plotupdate1
            
        endif
        
    end
    
endcase

;  Copy the pixmap and draw the cross hair.

wset, state.p.plotwin1_wid
device, copy=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
              state.p.pixmap1_wid]

wset, state.p.plotwin2_wid
device, copy=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,0,$
              state.p.pixmap2_wid]

wset, state.p.plotwin1_wid
plots, [event.x,event.x],[0,state.p.plot1size[1]],COLOR=2,/DEVICE
plots, [0,state.p.plot1size[0]],[event.y,event.y],COLOR=2,/DEVICE


tabinv, (*state.p.spec)[*,0],xy[0],idx
idx = round(idx)
label = 'Cursor X: '+strtrim(xy[0],2)+', Y: '+strtrim(xy[1],2)
label = label+'   Scale X: '+strtrim((*state.p.spec)[idx,0],2)+$
  ', Scale Y: '+strtrim((*state.r.scalespec)[idx],2)
widget_control,state.w.message,SET_VALUE=label

cont:

end
;
;******************************************************************************
;
pro xmc_scalelines_plotwin2event,event

common xmc_scalelines_state
widget_control, event.id,  GET_UVALUE = uvalue

;  Check to see if it is a TRACKING event.

if strtrim(tag_names(event,/STRUCTURE_NAME),2) eq 'WIDGET_TRACKING' then begin

    if event.enter eq 0 then begin

        widget_control, state.w.keyboard, SENSITIVE=0
        wset, state.p.plotwin1_wid
        device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
                      state.p.pixmap1_wid]
        wset, state.p.plotwin2_wid
        device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,0,$
                      state.p.pixmap2_wid]
        
    ENDIF
    goto, cont
    
endif else widget_control, state.w.keyboard, /INPUT_FOCUS, /SENSITIVE

wset, state.p.plotwin2_wid

;  Load up plot scales.

!p = state.p.pscale2
!x = state.p.xscale2
!y = state.p.yscale2

;  Determine the wavelength and flux values for the event.

x  = event.x/float(state.p.plot2size[0])  
y  = event.y/float(state.p.plot2size[1])
xy = convert_coord(x,y,/NORMAL,/TO_DATA)

if event.type eq 1 then begin

    case state.r.cursormode of 

        'Estimate Scale': begin
            
            z = where(finite(state.r.ereg) eq 1,count)
            if count eq 0 then begin
                
                state.r.ereg[0] = xy[0]
                wset, state.p.pixmap2_wid
                plots, [event.x,event.x],[0,state.p.plot2size[1]],COLOR=7,$
                  /DEVICE,LINESTYLE=2
               wset, state.p.plotwin2_wid
               device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,$
                             0,state.p.pixmap2_wid]
               
            endif else begin

                state.r.ereg[1] = xy[0]
                xmc_scalelines_estimatescale
                xmc_scalelines_makescale
                xmc_scalelines_maketelluric
                xmc_scalelines_makespec
                xmc_scalelines_plotupdate1
                xmc_scalelines_plotupdate2
                state.r.cursormode = 'None'
                
            endelse

        end

        'Fix': begin

            z = where(finite(state.p.reg) eq 1,count)
            if count eq 0 then begin
                
                state.p.reg[*,0] = xy[0:1] 

                wset, state.p.plotwin2_wid
                plots,[xy[0],xy[0]],(state.p.yscale2).crange,LINESTYLE=2,$
                  COLOR=7
                wset, state.p.pixmap2_wid
                plots,[xy[0],xy[0]],(state.p.yscale2).crange,LINESTYLE=2,$
                  COLOR=7
                
            endif else begin
                
                state.p.reg[*,1] = xy[0:1]

                ndat = n_elements(*state.r.cutreg.(state.r.stdidx))
                xx = reform(state.p.reg[0,*])
                xx = xx[sort(xx)]

                if ndat eq 1 then *state.r.cutreg.(state.r.stdidx) = xx $
                else *state.r.cutreg.(state.r.stdidx) = $
                   [*state.r.cutreg.(state.r.stdidx),xx]
                
                state.p.reg = !values.f_nan
                xmc_scalelines_maketelluric
                xmc_scalelines_makespec
                xmc_scalelines_plotupdate2     
                state.r.cursormode = 'None'

            endelse

        end


        'XZoom': begin

            z = where(finite(state.p.reg) eq 1,count)
            if count eq 0 then begin
                
                state.p.reg[*,0] = xy[0:1]
                wset, state.p.pixmap2_wid
                plots, [event.x,event.x],[0,state.p.plot2size[1]],COLOR=2,$
                  /DEVICE,LINESTYLE=2
               wset, state.p.plotwin2_wid
               device, copy=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,$
                             0,state.p.pixmap2_wid]
               
            endif else begin

               state.p.reg[*,1] = xy[0:1]                
                state.p.xrange2 = [min(state.p.reg[0,*],MAX=m),m]
                state.r.cursormode = 'None'
                state.p.reg = !values.f_nan
                xmc_scalelines_plotupdate1
                xmc_scalelines_plotupdate2
                xmc_scalelines_setminmax
                
            endelse


        end

        'YZoom': begin

            z = where(finite(state.p.reg) eq 1,count)
            if count eq 0 then begin
                
                state.p.reg[*,0] = xy[0:1]
                wset, state.p.pixmap2_wid
                plots, [0,state.p.plot2size[0]],[event.y,event.y],COLOR=2,$
                  /DEVICE,LINESTYLE=2

                wset, state.p.plotwin2_wid
                device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],$
                              0,0,state.p.pixmap2_wid]
               
            endif else begin

                state.p.reg[*,1] = xy[0:1]
                state.p.yrange2 = [min(state.p.reg[1,*],MAX=m),m]
                state.r.cursormode = 'None'
                state.p.reg = !values.f_nan
                xmc_scalelines_plotupdate1
                xmc_scalelines_plotupdate2
                xmc_scalelines_setminmax
                
            endelse

        end
        
       'Zoom': begin
       
           z = where(finite(state.p.reg) eq 1,count)
           if count eq 0 then state.p.reg[*,0] = xy[0:1] else begin

               state.p.reg[*,1] = xy[0:1]
               state.p.xrange2 = [min(state.p.reg[0,*],MAX=max),max]
               state.p.yrange2 = [min(state.p.reg[1,*],MAX=max),max]
               state.r.cursormode = 'None'
               state.p.reg = !values.f_nan
               xmc_scalelines_plotupdate1
               xmc_scalelines_plotupdate2
               xmc_scalelines_setminmax
               
           endelse

        end

        else:

    endcase

endif

;  Copy the pixmap and draw the cross hair.

wset, state.p.plotwin1_wid
device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
              state.p.pixmap1_wid]

wset, state.p.plotwin2_wid
device, COPY=[0,0,state.p.plot2size[0],state.p.plot2size[1],0,0,$
              state.p.pixmap2_wid]

case state.r.cursormode of 

    'XZoom': begin

        wset, state.p.plotwin2_wid
        plots, [event.x,event.x],[0,state.p.plot2size[1]],color=2,/DEVICE
        wset, state.p.plotwin1_wid
        plots, [event.x,event.x],[0,state.p.plot1size[1]],COLOR=2,/DEVICE
        
    end

    'YZoom': plots, [0,state.p.plot2size[0]],[event.y,event.y],COLOR=2,/DEVICE

    'Zoom': begin

        plots, [event.x,event.x],[0,state.p.plot2size[1]],COLOR=2,/DEVICE
        plots, [0,state.p.plot2size[0]],[event.y,event.y],COLOR=2,/DEVICE
        xy = convert_coord(event.x,event.y,/DEVICE,/TO_DATA)
        plots,[state.p.reg[0,0],state.p.reg[0,0]],[state.p.reg[1,0],xy[1]],$
          LINESTYLE=2,COLOR=2
        plots, [state.p.reg[0,0],xy[0]],[state.p.reg[1,0],state.p.reg[1,0]],$
          LINESTYLE=2,COLOR=2
        
    end

    else: begin
        
        wset, state.p.plotwin2_wid
        plots, [event.x,event.x],[0,state.p.plot2size[1]],COLOR=2,/DEVICE
        plots, [0,state.p.plot2size[0]],[event.y,event.y],COLOR=2,/DEVICE
        wset, state.p.plotwin1_wid
        plots, [event.x,event.x],[0,state.p.plot1size[1]],COLOR=2,/DEVICE

    end

endcase

;  Update cursor position

;tabinv, (*state.p.spec)[*,0],xy[0],idx
;idx = round(idx)
label = 'Cursor X: '+strtrim(xy[0],2)+', Y: '+strtrim(xy[1],2)
;label = label+'   Spectrum X: '+strtrim( (*state.p.spec)[idx,0],2)+$
;  ', Y: '+strtrim( (*state.p.spec)[idx,1],2)+', Scale: '+$
;  strtrim((*state.r.scalespec)[idx],2)
widget_control,state.w.message,SET_VALUE=label

cont:
end
;
;******************************************************************************
;
pro xmc_scalelines_resizeevent, event

  common xmc_scalelines_state

  widget_control, state.w.xmc_scalelines_base, TLB_GET_SIZE = size

;  Get new plot sizes

  state.p.plot1size[0]=size[0]-state.p.buffer[0]
  state.p.plot1size[1]=(size[1]-state.p.buffer[1])*state.p.plot1scale
  
  state.p.plot2size[0]=size[0]-state.p.buffer[0]
  state.p.plot2size[1]=(size[1]-state.p.buffer[1])*state.p.plot2scale

;  Resize windows
  
  widget_control, state.w.plotwin1, UPDATE=0
  widget_control, state.w.plotwin2, UPDATE=0

  widget_control, state.w.plotwin1, DRAW_XSIZE=state.p.plot1size[0]
  widget_control, state.w.plotwin1, DRAW_YSIZE=state.p.plot1size[1]

  widget_control, state.w.plotwin2, DRAW_XSIZE=state.p.plot2size[0]
  widget_control, state.w.plotwin2, DRAW_YSIZE=state.p.plot2size[1]

  widget_control, state.w.plotwin1, UPDATE=1
  widget_control, state.w.plotwin2, UPDATE=1
  
;  Redo pixel maps

  wdelete,state.p.pixmap1_wid
  window, /FREE, /PIXMAP,XSIZE=state.p.plot1size[0],YSIZE=state.p.plot1size[1]
  state.p.pixmap1_wid = !d.window
  
  wset, state.p.plotwin1_wid
  device, COPY=[0,0,state.p.plot1size[0],state.p.plot1size[1],0,0,$
                state.p.pixmap1_wid]

  wdelete,state.p.pixmap2_wid
  window, /FREE, /PIXMAP,XSIZE=state.p.plot2size[0],YSIZE=state.p.plot2size[1]
  state.p.pixmap2_wid = !d.window
  

  xmc_scalelines_plotupdate1
  xmc_scalelines_plotupdate2

end
;
;******************************************************************************
;
;-----------------------------Main Program------------------------------------
;
;******************************************************************************
;
pro xmc_scalelines,std,stdorders,stdmag,stdbmv,wvega,fvega,fcvega,fc2vega,$
                kernels,vshift,obj,objorders,objnaps,awave,atrans,hlines,$
                hnames,initscale,scales,cutreg,PARENT=parent,XPUTS=xputs,$
                XTITLE=xtitle,YTITLE=ytitle,CANCEL=cancel

 common xmc_scalelines_state

 mc_mkct

 if not xregistered('xmc_scalelines') then begin
    
    xmc_scalelines_initcommon,std,stdorders,stdmag,stdbmv,wvega,fvega,fcvega,$
                           fc2vega,kernels,vshift,obj,objorders,objnaps,awave, $
                           atrans,hlines,hnames,initscale,scales,XPUTS=xputs, $
                           XTITLE=xtitle,YTITLE=ytitle
    
    if n_elements(PARENT) ne 0 then widget_control, parent, SENSITIVE=0

;  Build the widget.

    mc_getfonts,buttonfont,textfont
    
    state.w.xmc_scalelines_base = widget_base(TITLE='Xmc_Scalelines', $
                                          /COLUMN,$
                                          /TLB_SIZE_EVENTS)

       quit_button = widget_button(state.w.xmc_scalelines_base,$
                                   FONT=buttonfont,$
                                   EVENT_PRO='xmc_scalelines_event',$
                                   VALUE='Cancel',$
                                   UVALUE='Cancel')

       state.w.message = widget_text(state.w.xmc_scalelines_base, $
                                     VALUE='',$
                                     YSIZE=1)

       state.w.keyboard = widget_text(state.w.xmc_scalelines_base, $
                                      /ALL_EVENTS, $
                                      SCR_XSIZE=1, $
                                      SCR_YSIZE=1, $
                                      UVALUE='Keyboard', $
                                      EVENT_PRO='xmc_scalelines_event',$
                                      VALUE= '')

    row = widget_base(state.w.xmc_scalelines_base,$
                      EVENT_PRO='xmc_scalelines_event',$
                      /ROW,$
                      FRAME=2,$
                      /BASE_ALIGN_CENTER)

       v = coyote_field2(row,$
                         LABELFONT=buttonfont,$
                         FIELDFONT=textfont,$
                         TITLE='Vshift (km/s) :',$
                         UVALUE='Vshift',$
                         VALUE=strtrim(state.r.vshift,2),$
                         XSIZE=8,$
                         EVENT_PRO='xmc_scalelines_event',$
                         /CR_ONLY,$
                         TEXTID=textid)
       state.w.vshift_fld = [v,textid]

       button = widget_button(row,$
                              VALUE='Reset Control Points',$
                              FONT=buttonfont,$
                              UVALUE='Reset Control Points')    



    state.w.plotwin1 = widget_draw(state.w.xmc_scalelines_base,$
                                   XSIZE=state.p.plot1size[0],$
                                   YSIZE=state.p.plot1size[1],$
                                   /TRACKING_EVENTS,$
                                   /BUTTON_EVENTS,$
                                   /MOTION_EVENTS,$
                                   EVENT_PRO='xmc_scalelines_plotwin1event',$
                                   UVALUE='Plot Window 1')


    row = widget_base(state.w.xmc_scalelines_base,$
                      EVENT_PRO='xmc_scalelines_event',$
                      FRAME=2,$
                      /ROW,$
                      /BASE_ALIGN_CENTER)

       bg = cw_bgroup(row,$
                      FONT=buttonfont,$
                      ['Telluric','Object'],$
                      /ROW,$
                      /RETURN_NAME,$
                      /NO_RELEASE,$
                      /EXCLUSIVE,$
                      LABEL_LEFT='Spectrum:',$
                      UVALUE='Spectrum Type',$
                      SET_VALUE=0)

       state.w.order_dl = widget_droplist(row,$
                                          FONT=buttonfont,$
                                          TITLE='Order:',$
                                          VALUE=string(state.d.stdorders,$
                                                       FORMAT='(i2.2)'),$
                                          UVALUE='Order')

       atmos_bg = cw_bgroup(row,$
                            ['Atmosphere'],$
                            FONT=buttonfont,$
                            UVALUE='Atmosphere',$
                            SET_VALUE=[1],$
                            /NONEXCLUSIVE)

       
       fld  = coyote_field2(row,$
                            LABELFONT=buttonfont,$
                            FIELDFONT=textfont,$
                            TITLE='Scale:',$
                            UVALUE='Scale Atmosphere',$
                            XSIZE=4,$
                            VALUE=1.0,$
                            EVENT_PRO='xmc_scalelines_event',$
                            /CR_ONLY,$
                            TEXTID=textid)
       state.w.scaleatmos_fld = [fld,textid]


       if n_elements(XPUTS) ne 0 then begin

           xput_bg = cw_bgroup(row,$
                               ['Throughputs'],$
                               FONT=buttonfont,$
                               UVALUE='Throughputs',$
                               SET_VALUE=[1],$
                               /NONEXCLUSIVE)
           
       endif
       state.w.plotwin2 = widget_draw(state.w.xmc_scalelines_base,$
                                      XSIZE=state.p.plot2size[0],$
                                      YSIZE=state.p.plot2size[1],$
                                      /TRACKING_EVENTS,$
                                      /BUTTON_EVENTS,$
                                      /MOTION_EVENTS,$
                                      EVENT_PRO='xmc_scalelines_plotwin2event',$
                                      UVALUE='Plot Window 2')
    
    row_base = widget_base(state.w.xmc_scalelines_base,$
                           EVENT_PRO='xmc_scalelines_event',$
                           /ROW)
         
       xmin = coyote_field2(row_base,$
                            LABELFONT=buttonfont,$
                            FIELDFONT=textfont,$
                            TITLE='X Min:',$
                            UVALUE='X Min',$
                            XSIZE=12,$
                            EVENT_PRO='xmc_scalelines_minmaxevent',$
                            /CR_ONLY,$
                            TEXTID=textid)
       state.w.xmin_fld = [xmin,textid]
       
       xmax = coyote_field2(row_base,$
                            LABELFONT=buttonfont,$
                            FIELDFONT=textfont,$
                            TITLE='X Max:',$
                            UVALUE='X Max',$
                            XSIZE=12,$
                            EVENT_PRO='xmc_scalelines_minmaxevent',$
                            /CR_ONLY,$
                            TEXTID=textid)
       state.w.xmax_fld = [xmax,textid]
       
       ymin = coyote_field2(row_base,$
                            LABELFONT=buttonfont,$
                            FIELDFONT=textfont,$
                            TITLE='Y Min:',$
                            UVALUE='Y Min',$
                            XSIZE=12,$
                            EVENT_PRO='xmc_scalelines_minmaxevent',$
                            /CR_ONLY,$
                            TEXTID=textid)
       state.w.ymin_fld = [ymin,textid]
       
       ymax = coyote_field2(row_base,$
                            LABELFONT=buttonfont,$
                            FIELDFONT=textfont,$
                            TITLE='Y Max:',$
                            UVALUE='Y Max',$
                            XSIZE=12,$
                            EVENT_PRO='xmc_scalelines_minmaxevent',$
                            /CR_ONLY,$
                            TEXTID=textid)
       state.w.ymax_fld = [ymax,textid]
       
    accept_button = widget_button(state.w.xmc_scalelines_base,$
                                  FONT=buttonfont,$
                                  EVENT_PRO='xmc_scalelines_event',$
                                  VALUE='Accept',$
                                  UVALUE='Accept')
    
; Get things running.  Center the widget using the Fanning routine.
      
    cgcentertlb,state.w.xmc_scalelines_base
    widget_control, state.w.xmc_scalelines_base, /REALIZE
    
;  Get plotwin ids
    
    widget_control, state.w.plotwin1, GET_VALUE=x
    state.p.plotwin1_wid = x

    widget_control, state.w.plotwin2, GET_VALUE=x
    state.p.plotwin2_wid = x
    
    window, /FREE, /PIXMAP,XSIZE=state.p.plot1size[0],$
      YSIZE=state.p.plot1size[1]
    state.p.pixmap1_wid = !d.window

    window, /FREE, /PIXMAP,XSIZE=state.p.plot2size[0],$
      YSIZE=state.p.plot2size[1]
    state.p.pixmap2_wid = !d.window

;  Get sizes for things.
    
    widget_geom = widget_info(state.w.xmc_scalelines_base, /GEOMETRY)
    state.p.buffer[0]=widget_geom.xsize-state.p.plot1size[0]
    state.p.buffer[1]=widget_geom.ysize-state.p.plot1size[1]-$
      state.p.plot2size[1]

;  Get things running

    xmc_scalelines_selectspec
    xmc_scalelines_makescale
    xmc_scalelines_maketelluric
    xmc_scalelines_makespec
    xmc_scalelines_getminmax
    xmc_scalelines_setminmax
    xmc_scalelines_plotupdate1
    xmc_scalelines_plotupdate2

; Start the Event Loop. This will be a non-blocking program.
    
    XManager, 'xmc_scalelines', $
              state.w.xmc_scalelines_base, $
              EVENT_HANDLER='xmc_scalelines_resizeevent',$
              CLEANUP='xmc_scalelines_cleanup'
    
    if n_elements(PARENT) ne 0 then widget_control, parent, SENSITIVE=1

;  Now return required info

    scales = fltarr(n_elements(state.d.std[*,0,0]),state.d.norders)
    for i = 0, state.d.norders-1 do begin

       if n_elements((*state.r.cpoints).(i)[0,*]) eq 2 then begin

          scales[*,i] = 1.0
          
       endif else begin
          
          scales[*,i] = spline((*state.r.cpoints).(i)[0,*],$
                               (*state.r.cpoints).(i)[1,*],$
                               state.d.std[*,0,i],state.r.tension)
          

       endelse
       
        
    endfor
    cancel = state.r.cancel
    vshift = state.r.vshift 
    cutreg = state.r.cutreg

    ptr_free, state.r.scalespec
    ptr_free, state.d.deltascales
    ptr_free, state.d.tellspec

    state = 0B

endif

end

