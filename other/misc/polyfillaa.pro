;+
; NAME:
;
;    POLYFILLAA
;
; DESCRIPTION:
;
;    Finds the fractional area of all pixels at least partially inside
;    a specified polygon, or multiple polygons.
;
; CATEGORY:
;
;    GRAPHICS, REGION OF INTEREST
;
; CALLING SEQUENCE:
;
;    inds=polyfillaa(px,py,sx,sy,[POLY_INDICES=,
;                    AREAS=,POLYGONS=,/NO_COMPILED,/RECOMPILE])
;
; INPUT PARAMETERS:
;
;    px,py: The vectors containing the x and y subscripts of the
;       polygon.  May be in fractional units.  These lists can contain
;       multiple polygons concatenated together, which will all be
;       clipped at once.  If multiple polygons are passed,
;       POLY_INDICES must be set to separate inputs and outputs (which
;       see).
;
;    sx,sy: The size of the pixel grid on which the polygon is
;       superposed.  
;
; INPUT KEYWORD PARAMETERS:
;
;    NO_COMPILED: If set, the IDL-native method will be used to
;       compute the clipped polygons, regardless of whether
;       compilation of the external C version succeeded.  Otherwise,
;       POLYFILLAA will attempt to compile a C version of the
;       Sutherland Hodgemand algorithm found in the file "polyclip.c".
;       This version is ~50x faster, in particular when clipping
;       multiple polygons at once.
;
;    RECOMPILE: If set, recompile the C version, even if it has
;      already been compiled.  Note: if compilation has already
;      succeeded once, to actually link to the recompiled version you
;      must unload the old version.
;
;
; OUTPUT KEYWORD PARAMETERS:
;
;    AREAS: For each pixel index returned, the fractional area of that
;       pixel contained inside the polygon, between 0 and 1.
;
;    POLYGONS: A structure of the form {INDS: inds, POLYS: polys}
;       where POLYS is a 2xn array containing the polygon vertex
;       information (as columns x,y), and INDS is the reverse index of
;       length n_poly_out + 1 (where n_poly_out is the total number of
;       resulting clipped polygons).  POLYGONS can only be returned
;       when clipping single polygons, and so cannot be used with
;       POLY_INDICES.
;
;
; INPUT/OUTPUT KEYWORD PARAMETERS:
;
;    POLY_INDICES: Only used when passing multiple encoded polygons in
;      px and py, this keyword serves two purpose.  On input, it
;      should be a "reverse index" style vector of length n_polygons +
;      1, where vec[i]:vec[i+1]-1 gives the index range in px and py
;      corresponding to polygon i.  On output, it similarly lists the
;      indices into inds and areas, with vec[i]:vec[i+1] giving the
;      range of pixel indices and areas corresponding to input polygon
;      i.  Note that POLYGONS is not an allowed output with multiple
;      polygons.
;
; OUTPUTS:
;
;    inds: The indices of all pixels at least partially inside the
;       polygon.
;
; PROCEDURES:
;
;    polyclip
;
; NOTES:
;
;    POLYFILLAA attempts to auto-compile a C-language version of the
;    resource-intensive clipping algorithm, found in polyclip.c.  In
;    order for this compilation to succeed, a compiler which IDL
;    recognizes must be installed.  See documentation for MAKE_DLL and
;    the !MAKE_DLL system variable for more information.  The
;    resulting compiled shared library is located in, e.g. (modify for
;    architecture and version):
;
;      ~/.idl/rsi/compile_dir-118-idl_6_3-linux-x86-m32-f64/polyclip.so
;
;    This library is loaded with CALL_EXTERNAL automatically.
;
; RESTRICTIONS:
;
;    When passing multiple polygons on input for simultaneous
;    clipping, only the areas and pixel indices clipped can be
;    returned (not POLYGONS).
;
; EXAMPLE:
;
;    inds=polyfillaa([1.2,3,5.3,3.2],[1.3,6.4,4.3,2.2],10,10,AREAS=areas)
;
; MODIFICATION HISTORY:
;
;       2009-04-22 (J.D. Smith): Correctly estimate maximum number of
;          resulting clipped polygons for multiple input polygons.
;
;       2007-01-11 (J.D. Smith): Major rewrite to accommodate the new
;          multi-polygon clipper polyclip.c, greatly improving speed
;          when only the clipped areas are needed.  Has two
;          entrypoints into polyclip: polyclip_single and
;          polyclip_multi.
;
;       2003-01-03 (J.D. Smith): Substantial rewrite to use external C
;          code (if possible) for a significant performance
;          improvement.
;
;       2001-09-26 (J.D. Smith): Written.  Initial documentation.
;-
;   $Id$
;##############################################################################
; 
; LICENSE
;
;  Copyright (C) 2001-2007 J.D. Smith
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

function polyfillaa, px,py,sx,sy, AREAS=areas, POLYGONS=polys,NO_COMPILED=nc, $
                     RECOMPILE=rc, POLY_INDICES=poly_inds
  common polyfillaa_external,polyclip_compiled,polyclip_path,polyclip_version
  
  ;; Compile the C clipper, if needed
  if n_elements(polyclip_compiled) eq 0 || keyword_set(rc) then begin 
     catch, err
     if err ne 0 then begin   ; any failure in compiling, just use the IDL vers
        message, $
           /CONTINUE, $
           'DLM clipper failed -- reverting to internal version. '+ $
           'Error: '+!ERROR_STATE.MSG
        polyclip_compiled=0
     endif else begin 

spextoolpath = file_dirname(file_dirname(file_which('Spextool_Instruments.dat'),/MARK))
        path = filepath(ROOT=spextoolpath,'other/misc')

;        @cubism_dir
;        path=filepath(ROOT=cubism_dir,'calib')
        reuse=~keyword_set(rc)  ; Don't reuse if re-compiling
        done=0
        i=1
        while ~done && i le 2 do begin 
           make_dll,'polyclip', $
                    ['polyclip_test','polyclip_single','polyclip_multi'], $
                    INPUT_DIRECTORY=path,$
                    DLL_PATH=polyclip_path,REUSE_EXISTING=reuse, $
                    EXTRA_CFLAGS=!VERSION.OS_FAMILY eq 'unix'?'-O2':''
                                ;,EXTRA_CFLAGS='-DDEBUG'
           
           ;; Test for a correctly compiled library
           tmp=call_external(polyclip_path,'polyclip_test',/B_VALUE, $
                             UNLOAD=keyword_set(rc))
           
           switch tmp[0] of
              43b:
              42b: begin 
                 message, /CONTINUE,'Outdated clipper detected, recompiling.'
                 reuse=0b       ;old version, recompile it
                 tmp=call_external(polyclip_path,'polyclip_test',/B_VALUE, $
                                   /UNLOAD) ; Unload it for next round compile
                 break
              end 
           
              44b: begin 
                 done=1         ;correct version, continue
                 break
              end 
              
              else: message,'Testing clipper DLM: Incorrect value returned.', $
                            /NONAME
           endswitch 
           i++
        endwhile 
        if ~done then message,'Clipper version test failed.',/NONAME 
        polyclip_compiled=1
        polyclip_version=tmp[0]
     endelse 
     catch,/cancel 
  endif
  
  app=arg_present(polys)
  
  ;; See if we have multiple polygons passed
  if n_elements(poly_inds) ne 0 then begin 
     n_poly=n_elements(poly_inds)-1L
     multi=1b
     if app then $
        message,'Clipped polygon output not supported for multiple poly inputs.'
  endif else n_poly=1
  
  sx=long(sx) & sy=long(sy)
  
  ;; Count up the pixels from the various polygon's bounding boxes
  if keyword_set(multi) then begin 
     npix=0L                   ;use histogram over poly length
     h=histogram(poly_inds[1:*]-poly_inds,OMIN=om,REVERSE_INDICES=ri)
     bottom=lonarr(n_poly,/NOZERO) & top=lonarr(n_poly,/NOZERO)
     left=lonarr(n_poly,/NOZERO) & right=lonarr(n_poly,/NOZERO)
     
     for i=0,n_elements(h)-1 do begin 
        if ri[i] eq ri[i+1] then continue
        targ=[h[i],i+om]
        set=ri[ri[i]:ri[i+1]-1] ;the polys of this length
        take=rebin(poly_inds[set],targ,/SAMPLE) + $ ;all the poly indices
             rebin(lindgen(1,i+om),targ,/SAMPLE)
        left[set]=floor(min(px[take],DIMENSION=2,MAX=maxx))>0L
        right[set]=floor(maxx)<(sx-1L)
        bottom[set]=floor(min(py[take],DIMENSION=2,max=maxy))>0L
        top[set]=floor(maxy)<(sy-1L)
     endfor
     nx=right-left+1L
     ny=top-bottom+1L
     npix=total(nx*ny,/PRESERVE_TYPE)
  endif else begin              ; Single polygon
     ;; Clip grid to the enclosing box
     left=floor(min(px,max=maxx))>0L
     right=floor(maxx)<(sx-1L)
     bottom=floor(min(py,max=maxy))>0L
     top=floor(maxy)<(sy-1L)
     nx=right-left+1L & ny=top-bottom+1L
     if nx lt 1 || ny lt 1 then return,-1L
     npix=nx*ny
  endelse 
  if npix le 0L then return,-1L
  
  ;; npix is the maximum possible number of clipped polys
  nverts=n_elements(px)         
  apa=arg_present(areas)
  
  if keyword_set(nc) || polyclip_compiled eq 0 then begin 
     ;; --- pure IDL version
     ind=0L
     if apa then areas=fltarr(npix,/NOZERO) 
     
     ret=lonarr(npix,/NOZERO)
     beg=0
     for p=0L,n_poly-1 do begin 
        nclip_poly=0L
        if keyword_set(multi) then begin 
           px1=px[beg:poly_inds[p+1]-1]
           py1=py[beg:poly_inds[p+1]-1]
        endif else begin 
           px1=px
           py1=py
        endelse 
        
        for j=bottom[p],top[p] do begin 
           for i=left[p],right[p] do begin
              px_out=px1 & py_out=py1
              polyclip,i,j,px_out,py_out
              if px_out[0] eq -1 then continue
              ret[ind]=i+j*sx
              if apa then $
                 areas[ind]=abs(total(double(px_out)* $
                                      shift(double(py_out),-1)- $
                                      double(py_out)* $
                                      shift(double(px_out),-1))/2.)
              if app then begin 
                 if n_elements(polys_out_poly) eq 0 then begin
                    polys_out_poly=[transpose(px_out),transpose(py_out)]
                    polys_out_ind=[0,n_elements(px_out)]
                 endif else begin 
                    polys_out_poly=[[polys_out_poly], $
                                [transpose(px_out),transpose(py_out)]]
                    polys_out_ind=[polys_out_ind, $
                                   polys_out_ind[n_elements(polys_out_ind)-1]+$
                                   n_elements(px_out)]
                 endelse 
              endif 
              ind++
              nclip_poly++
           endfor
        endfor
        if keyword_set(multi) then begin
           beg=poly_inds[p+1]
           poly_inds[p+1]=poly_inds[p] + nclip_poly
        endif
     endfor 
     if app then polys=create_struct('POLYS',temporary(polys_out_poly), $
                                     'INDS',temporary(polys_out_ind))
     if apa then areas=areas[0:ind-1L]
     ret=ret[0:ind-1L]
  endif else begin     
     ;; --- Compiled version, use call_external and the shared lib
     if size(px,/TYPE) eq 5 then begin ; No double please
        px=float(px) & py=float(py)
     endif

     nclip_poly=0L              ;actual number of clipped polygons
     inds=lonarr(2,npix)        ;Output x,y indices which clipped the poly(s)
     
     areas=fltarr(npix,/NOZERO) 
     if keyword_set(multi) then begin 
        ;; Multiple polys input: no output polygons; poly_inds is
        ;; input/output for area & inds
        if size(poly_inds,/TYPE) ne 13 then poly_inds=ulong(poly_inds) 
        tmp=call_external(polyclip_path,'polyclip_multi',$
                          VALUE= $
                          [0b,0b,0b,0b,$
                           0b,0b, $
                           1b,   0b, $
                           0b, $
                           0b, $
                           0b], $
                          left,right,bottom,top, $ ; bounding box arrays
                          px,py, $            ; input polygon indices
                          n_poly,poly_inds, $ ; indices per poly into px,py
                          inds, $             ; OUT: x,y inds within array
                          nclip_poly, $       ; OUT: final number clipped polys
                          areas)              ; OUT: output areas
        if nclip_poly eq 0L then return,-1L
     endif else begin 
        ;; Single poly input: ri_out is for output clipped polygons
        ri_out=lonarr(npix+1)      
        py_out=(px_out=fltarr((nverts+24)*npix,/NOZERO)) ; at most 24 new legs
        
        tmp=call_external(polyclip_path,'polyclip_single',$
                          VALUE= $
                          [1b, 1b,   1b,    1b,  $
                           0b,0b,1b, $
                           0b, $
                           0b, $
                           0b, $
                           0b,0b,0b], $
                          left,right,bottom,top, $ ; bound-box to consider
                          px,py,nverts, $          ; input polygon indices
                          inds, $                  ; OUT: x,y inds within array
                          nclip_poly, $       ; OUT: final number clipped polys
                          areas, $            ; OUT: output areas
                          px_out,py_out,ri_out) ; OUT: clipped poly verts
        
        if nclip_poly eq 0L then return,-1L
        if app then begin 
           pmax=ri_out[nclip_poly]-1 ;max index into p[xy]_out
           polys=create_struct('POLYS', $
                               [transpose(px_out[0:pmax]), $
                                transpose(py_out[0:pmax])],$
                               'INDS',ri_out[0:nclip_poly])
        endif 
     endelse
     areas=areas[0:nclip_poly-1]
     ret=reform(inds[0,0:nclip_poly-1]+sx*inds[1,0:nclip_poly-1],/OVERWRITE)
  endelse 
  
  return,ret
end
