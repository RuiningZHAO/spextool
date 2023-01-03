;+
; NAME:
;     ximgtool
;
; PURPOSE:
;     To display FITS images in a fashion similar to ximtool.
;
; CALLING SEQUENCE:
;     ximgtool,[img1],[img2],[img3],[img4],[img5],$
;              EXT=ext,BUFFER=buffer,ROTATION=rotation,ZOOM=zoom,$
;              ZTOFIT=ztofit,RANGE=range,ZRANGE=zrange,SCALE=scale,$
;              COLORMAP=colormap,INVERT=invert,NOUPDATE=noupdate,OPLOT=oplot,$
;              BITMASK=bitmask,ZUNITS=zunits,$
;              DIVISORS=divisors,FILENAME=filename,HDR=hdr,$
;              LOCKIMAGES=lockimages,STDIMAGE=stdimage,MAGNIFIER=magnifier,$
;              PANNER=panner,PLOTWINSIZE=plotwinsize,POSITION=position,$
;              WID=wid,GROUP_LEADER=group_leader,CANCEL=cancel
;
; INPUTS:
;     none
;
; OPTIONAL INPUTS:
;     img1 - A string scale given the name (or fullpath) of a FITS
;            images or an 2D array.  The image will be loaded in
;            buffer 1.
;     img2 - A string scale given the name (or fullpath) of a FITS
;            images or an 2D array.  The image will be loaded in
;            buffer 2.
;     img3 - A string scale given the name (or fullpath) of a FITS
;            images or an 2D array.  The image will be loaded in
;            buffer 3.
;     img4 - A string scale given the name (or fullpath) of a FITS
;            images or an 2D array.  The image will be loaded in
;            buffer 4.
;     img5 - A string scale given the name (or fullpath) of a FITS
;            images or an 2D array.  The image will be loaded in
;            buffer 5.

;
; KEYWORD PARAMETERS:
;     EXT          - The FITS extension number to load if the input is
;                    the name of a file.
;     BUFFER       - The buffer to load the image into (1-5).  Useful, if
;                    you are loading in masks or OPLOT objects one at a time.
;     ROTATION     - The IDL ROTATION function input number.
;     ZOOM         - Set to the requested zoom level, e.g. 2, 3, etc.
;     ZTOFIT       - Set to zoom the image to fit the display window.
;     RANGE        - A string scalar giving the name of a preset range
;                    of z values to display.
;
;                      'MinMax', '99.5%', '99%', '98%', '95%', '90%', 'ZScale'
;
;                    The default is 'ZScale'.
;     ZRANGE       - The z value range to display.
;     SCALE        - A string scalar giving the name of a preset
;                    scaling type.
;
;                      'Linear','Sqrt','Squared','Log','Hist Eq'
;
;                    The default is Linear.
;
;     COLORMAP     - A string scalar giving the name of a preset
;                    colormap.
;
;                      'Grey','Blue','Rainbow','Heat','Green'
;                   
;                    The default is Grey.
;     INVERT       - Set to invert the color map.
;     NOUPDATE     - Set to use the same scaling, color map, etc. used
;                    in the current buffer.     
;     OPLOT        - An object that will simply execute each time the
;                    display is updated.  Useful to over plot data onto
;                    the display.  Note:  The plotting must be done in
;                    pixel coordiates, not WCS coordinates.  Ximgtool
;                    assumes the center of the LLH pixel is (0,0) not
;                    (0.5,0.5).  
;     MASK         - A an array of the same size as the input image
;                    that can be used as a mask.  Pixels set to >1
;                    will be colored red in the display. 
;     ZUNITS       - A string scalar giving the units of the image.  If
;                    given, the results will be displayed next to the z
;                    values of the pixels.
;     DIVISORS     - A string scalar giving FITS header keywords
;                    separated by commas the values of which will be 
;                    divided into the data before display.  Useful if 
;                    the user wants to display the image rate (e.g. 
;                    counts per second) instead of just counts.  
;     FILENAME     - If the user is passing a 2D image, but would like
;                    to associate a filename with the image to be
;                    displayed at the top of the widget, given a string
;                    scalar of the filename.
;     HDR          - If the user is passing a 2D image, but would like
;                    to pass a FITS header that can be viewed with
;                    ximgtool, then pass the FITS header array through
;                    this keyword.
;     LOCKIMAGES   - If each buffer has the same size image (in pixels),
;                    setting this keyword will force the images in each
;                    buffer to zoom and pan together.
;     STDIMAGE     - An IRAF-like parameter giving the "deafult" image
;                    size such that STDIMAGExSTDIMAGE pixels are display
;                    at a zoom level of 1.
;     MAGNIFIER    - Set to show the magnifier window (default is to show it).
;     PANNER       - Set to show the panner window (default is to show
;                    it).
;     PLOTWINSIZE  - A two element vector giving the size of the
;                    display window in device pixels.
;     POSITION     - A two-element vector in normalized coordinates
;                    giving the position of the ximgtool.  [1,0] is
;                    the upper left hand corner of the display.
;     WID          - The plot window ID.
;     GROUP_LEADER - The IDL widget group leader.
;     CANCEL       - Set on return if there is a problem.
;
; OUTPUTS:
;     None
;
; OPTIONAL OUTPUTS:
;     Can write FITS or TIFF files to disk.   
;
; COMMON BLOCKS:
;     ximgtool_state
;
; RESTRICTIONS:
;     Esh.
;
; DEPENDENCIES:
;     The Spextool Library and the Astronomy User's Library.
;
; PROCEDURE:
;     ximgtool can be called in a number of ways.  
;
;       1) IDL> ximgtool
;
;     will simply launch the GUI and the user can interact with it via
;     the menus.
;
;       2) IDL> ximgtool 'file1.fits','file2.fits', etc.
;
;     ximgtool will be launched with the files loaded into the
;     buffers.
;
;       3) ximgtool,'file1.fits',BUFFER=1
;          ximgtool,'file2.fits',BUFFER=2
;          ximgtool,'file2.fits',BUFFER=3
;
;     By calling ximgtool in a program, the user can load each buffer
;     separately and pass masks, oplot objects, etc.
;
; EXAMPLES:
;     
;
; MODIFICATION HISTORY:
;     2010 - Written by M. Cushing, Institute for Astronomy, UH.
;            There have been several incarnations but this version is
;            the one that looks like the old ximtool.
;-
;
;===========================================================================
;
pro ximgtool_startup,DIVISORS=divisors,STDIMAGE=stdimage, $
                     PLOTWINSIZE=plotwinsize,POSITION=position,$
                     GROUP_LEADER=group_leader


  common ximgtool_state, state,buffers

  cleanplot,/SILENT

;  Get Spextool path

  spextoolpath = file_dirname(file_dirname( $
                 file_which('Spextool_Instruments.dat')),/MARK)

;  Get user inputs

  stdimage = (n_elements(stdimage) eq 0) ? 1024:stdimage


  if n_elements(PLOTWINSIZE) eq 0 then plotwinsize = [700,512]	 else $
     plotwinsize = [plotwinsize[0] > 512, plotwinsize[1] > 512]

;  Get fonts

  mc_getfonts,buttonfont,textfont,CANCEL=cancel
  if cancel then return

;  Load color table
  
  device, RETAIN=2
  mc_mkct,BOT=offset,BLUE=blue,GREEN=green,RED=red

;  DIVISORS

  if n_elements(DIVISORS) ne 0 then begin

     tmpdivisors = strtrim(strsplit(divisors,',',/EXTRACT),2)
     tmpdivisorson = 1
     
  endif else begin

     tmpdivisors = ''
     tmpdivisorson = 0

  endelse

  blankbuffer = {filename:'',img:ptr_new(2),hdr:ptr_new(''),rotation:0, $
                 rotimg:ptr_new(2),bytimg:ptr_new(2),ncols:0,nrows:0, $
                 imgrange:'',min:0.0,max:0.0,imgscaling:'', $
                 imgcmap:'Blue',red:red,green:green,blue:blue,invert:0, $
                 bricon:[0.5,0.5],cenpix:[0.,0.],zoom:0.0, $
                 bitmask:ptr_new(2),plotbitmask:0,$
                 pixmap_wid:0,xscale:!x,yscale:!y,pscale:!p, $
                 plotimg:ptr_new(2),offset:[0.0,0.0], $
                 position:[0,0,0,0],xrange:[0.0,0.0],yrange:[0.0,0.0], $
                 oplot:ptr_new(2),zunits:'',infosize:0,path:'', $
                 phdr:ptr_new(''),wcsinfo:ptr_new(2), $
                 pwcsinfo:ptr_new(2),equinox:'',pixdisp:[0L,0L], $
                 panimg:ptr_new(2),divisorslabel:'',divisorssize:0,title:''}
  
  state = {blankbuffer:blankbuffer,$
           blinkbuffer:[1,0,0,0,0],$
           buffer:0,$
           buffermathpanel:0,$
           bufferblinkpanel:0,$
           buttonfont:buttonfont,$
           but_buffer:[0L,0L,0L,0L,0L],$
           but_blinkbuf:[0,0,0,0,0],$
           but_blink:0L,$
           but_color:[0L,0L,0L,0L,0L],$
           but_cursor:[0L,0L,0L,0L],$
           but_decdeg:0L,$
           but_invert:0L,$
           but_lock:0L,$
           but_magnifier:0L,$
           but_mask:0L,$
           but_overplot:0L,$
           but_panner:0L,$
           but_range:[0L,0L,0L,0L,0L,0L,0L,0L],$
           but_rot:[0L,0L,0L,0L,0L,0L,0L,0L,0L],$
           but_scale:[0L,0L,0L,0L,0L],$
           but_sexagesimal:0L,$
           color_bottom:offset,$
           cursormode:['None','None'],$
           cursorpix:[0.0,0.0],$
           cursorpos:[0.0,0.0],$
           dec_fld:[0L,0L],$
           decdeg:0,$
           delay:0.2,$
           delay_fld:[0L,0L],$
           devpos:[0,0],$
           dev2img:0.0,$
           divisors:ptr_new(tmpdivisors),$
           divisorson:tmpdivisorson,$
           divisorspanel:0L,$
           divisorson_bg:0L,$
           divisors_fld:[0L,0L],$
           distregcolor:6,$
           imgmin_fld:[0L,0L],$
           imgmax_fld:[0L,0L],$
           invert:0,$
           lineregcolor:7,$
           loadedbuffer:[1,0,0,0,0],$
           lock:0,$
           magnifierpos:[0L,0L],$
           movie:0,$
           ncolors:!d.n_colors<256.0,$
           oplot:1,$
           panel:0L,$
           panner:1,$
           pannermod:0,$
           pannerpos:[0L,0L],$
           plotwin:0L,$
           plotwin_wid:0L,$
           plotwinsize:plotwinsize,$
           pstart:[0.0,0.0],$
           ra_fld:[0L,0L],$
           rangeboxcolor:2,$
           rangepanel:0,$
           region:[[!values.f_nan,!values.f_nan], $
                   [!values.f_nan,!values.f_nan]],$
           sexagesimal:1,$
           packagepath:spextoolpath,$
           statboxcolor:5,$
           stdimage:stdimage,$
           textfont:textfont,$
           trackwinsize:131,$
           magnifier:1,$
           magnifier_size:15,$
           mbuf1:0,$
           mbuf2:1,$
           mbuf3:2,$
           mop:1,$
           winbuffer:[0,0],$
           ximgtool_base:0L}

  buffers = {buffer1:{filename:'',img:ptr_new(2),hdr:ptr_new(''),rotation:0, $
                      rotimg:ptr_new(2),bytimg:ptr_new(2),ncols:0,nrows:0, $
                      imgrange:'',min:0.0,max:0.0,imgscaling:'', $
                      imgcmap:'Blue',red:red,green:green,blue:blue,invert:0, $
                      bricon:[0.5,0.5],cenpix:[0.,0.],zoom:0.0, $
                      bitmask:ptr_new(2),plotbitmask:0,$
                      pixmap_wid:0,xscale:!x,yscale:!y,pscale:!p, $
                      plotimg:ptr_new(2),offset:[0.0,0.0], $
                      position:[0,0,0,0],xrange:[0.0,0.0],yrange:[0.0,0.0], $
                      oplot:ptr_new(2),zunits:'',infosize:0,path:'', $
                      phdr:ptr_new(''),wcsinfo:ptr_new(2), $
                      pwcsinfo:ptr_new(2),equinox:'',pixdisp:[0L,0L], $
                      panimg:ptr_new(2),divisorslabel:'',divisorssize:0,$
                      title:''},$
             buffer2:{filename:'',img:ptr_new(2),hdr:ptr_new(''),rotation:0, $
                      rotimg:ptr_new(2),bytimg:ptr_new(2),ncols:0,nrows:0, $
                      imgrange:'',min:0.0,max:0.0,imgscaling:'', $
                      imgcmap:'Blue',red:red,green:green,blue:blue,invert:0, $
                      bricon:[0.5,0.5],cenpix:[0.,0.],zoom:0.0, $
                      bitmask:ptr_new(2),plotbitmask:0,$
                      pixmap_wid:0,xscale:!x,yscale:!y,pscale:!p, $
                      plotimg:ptr_new(2),offset:[0.0,0.0], $
                      position:[0,0,0,0],xrange:[0.0,0.0],yrange:[0.0,0.0], $
                      oplot:ptr_new(2),zunits:'',infosize:0,path:'', $
                      phdr:ptr_new(''),wcsinfo:ptr_new(2), $
                      pwcsinfo:ptr_new(2),equinox:'',pixdisp:[0L,0L], $
                      panimg:ptr_new(2),divisorslabel:'',divisorssize:0,$
                      title:''},$
             buffer3:{filename:'',img:ptr_new(2),hdr:ptr_new(''),rotation:0, $
                      rotimg:ptr_new(2),bytimg:ptr_new(2),ncols:0,nrows:0, $
                      imgrange:'',min:0.0,max:0.0,imgscaling:'', $
                      imgcmap:'Blue',red:red,green:green,blue:blue,invert:0, $
                      bricon:[0.5,0.5],cenpix:[0.,0.],zoom:0.0, $
                      bitmask:ptr_new(2),plotbitmask:0,$
                      pixmap_wid:0,xscale:!x,yscale:!y,pscale:!p, $
                      plotimg:ptr_new(2),offset:[0.0,0.0], $
                      position:[0,0,0,0],xrange:[0.0,0.0],yrange:[0.0,0.0], $
                      oplot:ptr_new(2),zunits:'',infosize:0,path:'', $
                      phdr:ptr_new(''),wcsinfo:ptr_new(2), $
                      pwcsinfo:ptr_new(2),equinox:'',pixdisp:[0L,0L], $
                      panimg:ptr_new(2),divisorslabel:'',divisorssize:0,$
                      title:''},$
             buffer4:{filename:'',img:ptr_new(2),hdr:ptr_new(''),rotation:0, $
                      rotimg:ptr_new(2),bytimg:ptr_new(2),ncols:0,nrows:0, $
                      imgrange:'',min:0.0,max:0.0,imgscaling:'', $
                      imgcmap:'Blue',red:red,green:green,blue:blue,invert:0, $
                      bricon:[0.5,0.5],cenpix:[0.,0.],zoom:0.0, $
                      bitmask:ptr_new(2),plotbitmask:0,$
                      pixmap_wid:0,xscale:!x,yscale:!y,pscale:!p, $
                      plotimg:ptr_new(2),offset:[0.0,0.0], $
                      position:[0,0,0,0],xrange:[0.0,0.0],yrange:[0.0,0.0], $
                      oplot:ptr_new(2),zunits:'',infosize:0,path:'', $
                      phdr:ptr_new(''),wcsinfo:ptr_new(2), $
                      pwcsinfo:ptr_new(2),equinox:'',pixdisp:[0L,0L], $
                      panimg:ptr_new(2),divisorslabel:'',divisorssize:0,$
                      title:''},$
             buffer5:{filename:'',img:ptr_new(2),hdr:ptr_new(''),rotation:0, $
                      rotimg:ptr_new(2),bytimg:ptr_new(2),ncols:0,nrows:0, $
                      imgrange:'',min:0.0,max:0.0,imgscaling:'', $
                      imgcmap:'Blue',red:red,green:green,blue:blue,invert:0, $
                      bricon:[0.5,0.5],cenpix:[0.,0.],zoom:0.0, $
                      bitmask:ptr_new(2),plotbitmask:0,$
                      pixmap_wid:0,xscale:!x,yscale:!y,pscale:!p, $
                      plotimg:ptr_new(2),offset:[0.0,0.0], $
                      position:[0,0,0,0],xrange:[0.0,0.0],yrange:[0.0,0.0], $
                      oplot:ptr_new(2),zunits:'',infosize:0,path:'', $
                      phdr:ptr_new(''),wcsinfo:ptr_new(2), $
                      pwcsinfo:ptr_new(2),equinox:'',pixdisp:[0L,0L], $
                      panimg:ptr_new(2),divisorslabel:'',divisorssize:0,$
                      title:''}}
             
  state.dev2img    = state.stdimage/512.

  state.magnifierpos = [state.plotwinsize[0]-10-state.trackwinsize,$
                        state.plotwinsize[1]-10-state.trackwinsize]

  state.pannerpos  = [10,state.plotwinsize[1]-10-state.trackwinsize]
  

  state.ximgtool_base = widget_base(TITLE='Ximgtool',$
                                    /COLUMN,$
                                    GROUP_LEADER=group_leader,$
                                    MBAR=mbar,$
                                    /TLB_SIZE_EVENTS)

  file = widget_button(mbar,$
                       EVENT_PRO='ximgtool_menuevent',$
                       VALUE='File',$
                       FONT=buttonfont,$
                       /MENU)

     button = widget_button(file,$
                            VALUE='Load FITS',$
                            UVALUE='Load FITS',$
                            FONT=buttonfont)                            

     button = widget_button(file,$
                            VALUE='New Frame',$
                            UVALUE='New Frame',$
                            FONT=buttonfont)                            

     button = widget_button(file,$
                            VALUE='Divisors',$
                            UVALUE='Divisors',$
                            FONT=buttonfont)                                 


     button = widget_button(file,$
                            VALUE='Clear Frame',$
                            UVALUE='Clear Frame',$
                            FONT=buttonfont)                            

     button = widget_button(file,$
                            VALUE='View Header',$
                            UVALUE='View Header',$
                            FONT=buttonfont)                            

;     button = widget_button(file,$
;                            VALUE='Find Position',$
;                            UVALUE='Find Position',$
;                            FONT=buttonfont)                            

     button = widget_button(file,$
                            VALUE='Write TIFF',$
                            UVALUE='Write TIFF',$
                            FONT=buttonfont)
     
     button = widget_button(file,$
                            VALUE='Write FITS',$
                            UVALUE='Write FITS',$
                            FONT=buttonfont)

     button = widget_button(file,$
                            VALUE='Quit',$
                            UVALUE='Quit',$
                            FONT=buttonfont)                            

  view = widget_button(mbar,$
                       EVENT_PRO='ximgtool_menuevent',$
                       VALUE='View',$
                       FONT=buttonfont,$
                       /MENU)

     state.but_magnifier = widget_button(view,$
                                         VALUE='Magnifier',$
                                         UVALUE='Magnifier',$
                                         /CHECKED_MENU,$
                                         FONT=buttonfont)    
     widget_control, state.but_magnifier, SET_BUTTON=state.magnifier

     state.but_panner = widget_button(view,$
                                      VALUE='Panner',$
                                      UVALUE='Panner',$
                                      /CHECKED_MENU,$
                                      FONT=buttonfont)                          
     widget_control, state.but_panner, SET_BUTTON=state.panner

     state.but_overplot = widget_button(view,$
                                        VALUE='Over Plot',$
                                        UVALUE='Over Plot',$
                                        /CHECKED_MENU,$
                                        FONT=buttonfont)          
     widget_control, state.but_overplot, SET_BUTTON=state.oplot

     state.but_mask = widget_button(view,$
                                    VALUE='Mask',$
                                    UVALUE='Mask',$
                                    /CHECKED_MENU,$
                                    FONT=buttonfont)          

     state.but_lock = widget_button(view,$
                                    VALUE='Lock Images',$
                                    UVALUE='Lock Images',$
                                    /CHECKED_MENU,$
                                    FONT=buttonfont)                            
     widget_control, state.but_lock, SET_BUTTON=state.lock


     wcs = widget_button(view,$
                         VALUE='WCS',$
                         UVALUE='WCS',$
                         /MENU,$
                         FONT=buttonfont)                            
     
     state.but_sexagesimal = widget_button(wcs,$
                                           VALUE='Sexagesimal',$
                                           UVALUE='Sexagesimal',$
                                           /CHECKED_MENU,$
                                           FONT=buttonfont)                   
     widget_control, state.but_sexagesimal, SET_BUTTON=state.sexagesimal

     state.but_decdeg = widget_button(wcs,$
                                      VALUE='Decimal Degrees',$
                                      UVALUE='Decimal Degrees',$
                                      /CHECKED_MENU,$
                                      FONT=buttonfont)
     widget_control, state.but_decdeg, SET_BUTTON=state.decdeg



  zoom = widget_button(mbar,$
                       VALUE='Zoom',$
                       EVENT_PRO='ximgtool_menuevent',$
                       FONT=buttonfont,$
                       /MENU)

     button = widget_button(zoom,$
                            VALUE='Center Image',$
                            UVALUE='Center Image',$
                            FONT=buttonfont)

     button = widget_button(zoom,$
                            VALUE='Zoom In',$
                            UVALUE='Zoom In',$
                            /SEPARATOR,$
                            FONT=buttonfont)

     button = widget_button(zoom,$
                            VALUE='Zoom Out',$
                            UVALUE='Zoom Out',$
                            FONT=buttonfont)

     button = widget_button(zoom,$
                            VALUE='Zoom To Fit',$
                            UVALUE='Zoom to Fit',$
                            FONT=buttonfont)

     
     state.but_rot[0] = widget_button(zoom,$
                                      VALUE='None (0)',$
                                      UVALUE='Rotation None',$
                                      EVENT_PRO='ximgtool_rotevent',$
                                      /CHECKED_MENU,$
                                      /SEPARATOR,$
                                      FONT=buttonfont)

     state.but_rot[1] = widget_button(zoom,$
                                      VALUE='Flip X (5)',$
                                      UVALUE='Rotation Flip X',$
                                      EVENT_PRO='ximgtool_rotevent',$
                                      /CHECKED_MENU,$
                                      FONT=buttonfont)

     state.but_rot[2] = widget_button(zoom,$
                                      VALUE='Flip Y (7)',$
                                      UVALUE='Rotation Flip Y',$
                                      EVENT_PRO='ximgtool_rotevent',$
                                      /CHECKED_MENU,$
                                      FONT=buttonfont)

     state.but_rot[3] = widget_button(zoom,$
                                      VALUE='-90 (1)',$
                                      UVALUE='Rotation -90',$
                                      EVENT_PRO='ximgtool_rotevent',$
                                      /CHECKED_MENU,$
                                      FONT=buttonfont)

     state.but_rot[4] = widget_button(zoom,$
                                      VALUE='-180 (2)',$
                                      UVALUE='Rotation -180',$
                                      EVENT_PRO='ximgtool_rotevent',$
                                      /CHECKED_MENU,$
                                      FONT=buttonfont)

     state.but_rot[5] = widget_button(zoom,$
                                      VALUE='-270 (3)',$
                                      UVALUE='Rotation -270',$
                                      EVENT_PRO='ximgtool_rotevent',$
                                      /CHECKED_MENU,$
                                      FONT=buttonfont)

     state.but_rot[6] = widget_button(zoom,$
                                      VALUE='Flip +XY (4)',$
                                      UVALUE='Rotation Flip +XY',$
                                      EVENT_PRO='ximgtool_rotevent',$
                                      /CHECKED_MENU,$
                                      FONT=buttonfont)

     state.but_rot[7] = widget_button(zoom,$
                                      VALUE='Flip -XY (6)',$
                                      UVALUE='Rotation Flip -XY',$
                                      EVENT_PRO='ximgtool_rotevent',$
                                      /CHECKED_MENU,$
                                      FONT=buttonfont)

     state.but_rot[8] = widget_button(zoom,$
                                      VALUE='N up E left',$
                                      UVALUE='N up E left',$
                                      EVENT_PRO='ximgtool_rotevent',$
                                      /CHECKED_MENU,$
                                      FONT=buttonfont)


  range = widget_button(mbar,$
                        VALUE='Range',$
                        EVENT_PRO='ximgtool_rangebuttonevent',$
                        FONT=buttonfont,$
                        /MENU)

     state.but_range[0] = widget_button(range,$
                                        VALUE='Min Max',$
                                        UVALUE='Min Max',$
                                        /CHECKED_MENU,$
                                        FONT=buttonfont)

     state.but_range[1] = widget_button(range,$
                                        VALUE='99.5%',$
                                        UVALUE='99.5%',$
                                        /CHECKED_MENU,$
                                        FONT=buttonfont)

     state.but_range[2] = widget_button(range,$
                                        VALUE='99%',$
                                        UVALUE='99%',$
                                        /CHECKED_MENU,$
                                        FONT=buttonfont)

     state.but_range[3] = widget_button(range,$
                                        VALUE='98%',$
                                        UVALUE='98%',$
                                        /CHECKED_MENU,$
                                        FONT=buttonfont)

     state.but_range[4] = widget_button(range,$
                                        VALUE='95%',$
                                        UVALUE='95%',$
                                        /CHECKED_MENU,$
                                        FONT=buttonfont)

     state.but_range[5] = widget_button(range,$
                                        VALUE='90%',$
                                        UVALUE='90%',$
                                        /CHECKED_MENU,$
                                        FONT=buttonfont)

     state.but_range[6] = widget_button(range,$
                                        VALUE='ZScale',$
                                        UVALUE='ZScale',$
                                        /CHECKED_MENU,$
                                        FONT=buttonfont)

     state.but_range[7] = widget_button(range,$
                                        VALUE='User',$
                                        UVALUE='User',$
                                        /CHECKED_MENU,$
                                        FONT=buttonfont)



  scale = widget_button(mbar,$
                        VALUE='Scale',$
                        FONT=buttonfont,$
                        EVENT_PRO='ximgtool_scaleevent',$
                        /MENU)

     state.but_scale[0] = widget_button(scale,$
                                        VALUE='Linear',$
                                        UVALUE='Linear',$
                                        /CHECKED_MENU,$
                                        FONT=buttonfont)

     state.but_scale[1] = widget_button(scale,$
                                        VALUE='Sqrt',$
                                        UVALUE='Sqrt',$
                                        /CHECKED_MENU,$
                                        FONT=buttonfont)

     state.but_scale[2] = widget_button(scale,$
                                        VALUE='Squared',$
                                        UVALUE='Squared',$
                                        /CHECKED_MENU,$
                                        FONT=buttonfont)

     state.but_scale[3] = widget_button(scale,$
                                        VALUE='Log',$
                                        UVALUE='Log',$
                                        /CHECKED_MENU,$
                                        FONT=buttonfont)

     state.but_scale[4] = widget_button(scale,$
                                        VALUE='Hist Eq',$
                                        UVALUE='Hist Eq',$
                                        /CHECKED_MENU,$
                                        FONT=buttonfont)

  color = widget_button(mbar,$
                        VALUE='ColorMap',$
                        FONT=buttonfont,$
                        EVENT_PRO='ximgtool_colorevent',$
                        /MENU)

     state.but_color[0] = widget_button(color,$
                                        VALUE='Grey',$
                                        UVALUE='Grey',$
                                        /CHECKED_MENU,$
                                        FONT=buttonfont)

     state.but_color[1] = widget_button(color,$
                                        VALUE='Blue',$
                                        UVALUE='Blue',$
                                        /CHECKED_MENU,$
                                        FONT=buttonfont)

     state.but_color[2] = widget_button(color,$
                                        VALUE='Rainbow',$
                                        UVALUE='Rainbow',$
                                        /CHECKED_MENU,$
                                        FONT=buttonfont)

     state.but_color[3] = widget_button(color,$
                                        VALUE='Heat',$
                                        UVALUE='Heat',$
                                        /CHECKED_MENU,$
                                        FONT=buttonfont)

     state.but_color[4] = widget_button(color,$
                                        VALUE='Green',$
                                        UVALUE='Green',$
                                        /CHECKED_MENU,$
                                        FONT=buttonfont)

     state.but_invert = widget_button(color,$
                                      VALUE='Invert',$
                                      UVALUE='Invert',$
                                      /CHECKED_MENU,$
                                        FONT=buttonfont)

  buffer = widget_button(mbar,$
                         VALUE='Buffer',$
                         FONT=buttonfont,$
                         EVENT_PRO='ximgtool_bufferbuttonevent',$
                         /MENU)

     state.but_buffer[0] = widget_button(buffer,$
                                        VALUE='Buffer 1',$
                                        UVALUE='Buffer 1',$
                                        /CHECKED_MENU,$
                                        FONT=buttonfont)

     state.but_buffer[1] = widget_button(buffer,$
                                        VALUE='Buffer 2',$
                                        UVALUE='Buffer 2',$
                                        /CHECKED_MENU,$
                                        FONT=buttonfont)

     state.but_buffer[2] = widget_button(buffer,$
                                        VALUE='Buffer 3',$
                                        UVALUE='Buffer 3',$
                                        /CHECKED_MENU,$
                                        FONT=buttonfont)

     state.but_buffer[3] = widget_button(buffer,$
                                        VALUE='Buffer 4',$
                                        UVALUE='Buffer 4',$
                                        /CHECKED_MENU,$
                                        FONT=buttonfont)

     state.but_buffer[4] = widget_button(buffer,$
                                        VALUE='Buffer 5',$
                                        UVALUE='Buffer 5',$
                                        /CHECKED_MENU,$
                                        FONT=buttonfont)

     button = widget_button(buffer,$
                            VALUE='Buffer Math',$
                            UVALUE='Buffer Math',$
                            EVENT_PRO='ximgtool_menuevent',$
                            /SEPARATOR,$
                            /CHECKED_MENU,$
                            FONT=buttonfont)

     button = widget_button(buffer,$
                            VALUE='Blink Buffers',$
                            UVALUE='Buffer Blink',$
                            EVENT_PRO='ximgtool_menuevent',$
                            /CHECKED_MENU,$
                            FONT=buttonfont)

  tmp = widget_button(mbar,$
                         VALUE='Cursor',$
                         FONT=buttonfont,$
                         EVENT_PRO='ximgtool_cursorevent',$
                         /MENU)

     state.but_cursor[0] = widget_button(tmp,$
                                         VALUE='Zoom',$
                                         UVALUE='Zoom Mode',$
                                         /CHECKED_MENU,$
                                         FONT=buttonfont)

     state.but_cursor[1] = widget_button(tmp,$
                                         VALUE='Stretch',$
                                         UVALUE='Stretch Mode',$
                                         /CHECKED_MENU,$
                                         FONT=buttonfont)

     state.but_cursor[2] = widget_button(tmp,$
                                         VALUE='Range',$
                                         UVALUE='Range Mode',$
                                         /CHECKED_MENU,$
                                         FONT=buttonfont)

     state.but_cursor[3] = widget_button(tmp,$
                                         VALUE='Image Exam',$
                                         UVALUE='Image Exam Mode',$
                                         /CHECKED_MENU,$
                                         FONT=buttonfont)

  help = widget_button(mbar,$
                       VALUE='Help',$
                       FONT=buttonfont,$
                       EVENT_PRO='ximgtool_menuevent',$
                       /HELP,$
                       /MENU)

     button = widget_button(help,$
                            VALUE='Ximgtool Help',$
                            UVALUE='Ximgtool Help',$
                            FONT=buttonfont)

  row = widget_base(state.ximgtool_base,$
                    EVENT_PRO='ximgtool_menuevent',$
                    /ROW,$
                    XPAD=0,$
                    YPAD=0,$
                    /BASE_ALIGN_CENTER)

     button = widget_button(row,$
                            VALUE='LoadFITS',$
                            UVALUE='Load FITS',$
                            FONT=buttonfont)                            

     button = widget_button(row,$
                            VALUE='NewFrame',$
                            UVALUE='New Frame',$
                            FONT=buttonfont)                            

     button = widget_button(row,$
                            VALUE='Center',$
                            UVALUE='Center Image',$
                            FONT=buttonfont)

     button = widget_button(row,$
                            VALUE='ZoomIn',$
                            UVALUE='Zoom In',$
                            /SEPARATOR,$
                            FONT=buttonfont)

     button = widget_button(row,$
                            VALUE='ZoomOut',$
                            UVALUE='Zoom Out',$
                            FONT=buttonfont)

     button = widget_button(row,$
                            VALUE='Zoom to Fit',$
                            UVALUE='Zoom to Fit',$
                            FONT=buttonfont)

     button = widget_button(row,$
                            VALUE='Quit',$
                            UVALUE='Quit',$
                            FONT=buttonfont)                            

  state.plotwin = widget_draw(state.ximgtool_base,$
                              XSIZE=state.plotwinsize[0],$
                              YSIZE=state.plotwinsize[1],$
                              EVENT_PRO='ximgtool_plotwinevent',$
                              /TRACKING_EVENTS,$
                              /KEYBOARD_EVENTS,$
                              /MOTION_EVENTS,$
                              /BUTTON_EVENTS)


; Get things running.

   if n_elements(POSITION) eq 0 then position = [0.5,0.5]
   cgcentertlb,state.ximgtool_base,position[0],position[1]

  widget_control, state.ximgtool_base, /REALIZE

;  Get plotwin ids

   widget_control, state.plotwin, GET_VALUE=x
   state.plotwin_wid=x

;  Do pixmaps for main window 

  for i = 0,4  do begin
     
     window, /FREE, /PIXMAP,XSIZE=state.plotwinsize[0], $
             YSIZE=state.plotwinsize[1]
     buffers.(i).pixmap_wid = !d.window
     
  endfor

;  Get sizes for thing state.

   widget_geom = widget_info(state.ximgtool_base, /GEOMETRY)

   state.winbuffer[0]=widget_geom.xsize-state.plotwinsize[0]
   state.winbuffer[1]=widget_geom.ysize-state.plotwinsize[1]


   XManager, 'ximgtool', $
             state.ximgtool_base, $
             EVENT_HANDLER='ximgtool_resizeevent',$
             CLEANUP='ximgtool_cleanupevent',$
             NO_BLOCK=1


end
;
;******************************************************************************
;
pro ximgtool_blink,BACKWARD=backward,FORWARD=forward

  common ximgtool_state
  
  nbuffer = n_elements(state.loadedbuffer) 
  idx = state.buffer
  
  for i = 0,nbuffer-1 do begin
     
     if keyword_set(FORWARD) then begin
        
        idx = idx+1
        if idx gt nbuffer-1 then idx = 0
        
     endif

     if keyword_set(BACKWARD) then begin
        
        idx = idx-1
        if idx lt 0 then idx = nbuffer-1
        
     endif

     if state.blinkbuffer[idx] eq 1 then begin
        
        state.buffer = idx
        ximgtool_chbuffer
        goto, cont
        
     endif
     
  endfor

cont:
  
end
;
;==============================================================================
;
pro ximgtool_bytsclimg

  common ximgtool_state
  
  ncolors = state.ncolors-(state.color_bottom+1)
  
  min = buffers.(state.buffer).min
  max = buffers.(state.buffer).max
  
;  Scale image and also determine where to place the tick marks in the
;  color bar.

  except = !except
  !except = 0  
  case buffers.(state.buffer).imgscaling of 
     
     'Asinh': begin
        
;  Project against negative numbers
        
        offset = -1*min
        fmin = asinh((min+offset)/state.beta)
        fmax = asinh((max+offset)/state.beta)
        
        img = bytscl(asinh((*buffers.(state.buffer).rotimg-min)/state.beta), $
                     /NAN,MIN=fmin,MAX=fmax,TOP=ncolors) + $
              state.color_bottom      
                
     end
     
     'Linear': begin
        
        img = bytscl(*buffers.(state.buffer).rotimg,/NAN,$
                     MIN=min,MAX=max,TOP=ncolors) + state.color_bottom
        
     end
     
     'Sqrt': begin
        
        offset = -1*min
        fmin = (min+offset)^0.5
        fmax = (max+offset)^0.5
        img = bytscl((*buffers.(state.buffer).rotimg+offset)^0.5,/NAN,$
                     MIN=fmin,MAX=fmax,TOP=ncolors) + state.color_bottom
        
     end
     
     'Squared': begin
        
        offset = -1*min
        fmin = (min+offset)^2
        fmax = (max+offset)^2
        
        img = bytscl((*buffers.(state.buffer).rotimg+offset)^2,/NAN,$
                     MIN=fmin,MAX=fmax,TOP=ncolors) + state.color_bottom
        
     end
     
     'Log': begin
        
        offset = (max-min)*0.01-min
        
        fmin = alog10(min+offset)
        fmax = alog10(max+offset)
        
        img = bytscl(alog10(*buffers.(state.buffer).rotimg+offset), $
                     MIN=fmin,MAX=fmax,/NAN,TOP=ncolors)+state.color_bottom
        
     end
     
     'Hist Eq': begin
        
        img = bytscl(hist_equal(*buffers.(state.buffer).rotimg, $
                                MINV=min,MAXV=max),/NAN,TOP=ncolors)+ $
              state.color_bottom
                
     end
     
  endcase
  junk = check_math()
  !except = except

  if buffers.(state.buffer).plotbitmask then begin

     ntags = n_tags(*buffers.(state.buffer).bitmask)
     
     bitmask = (*buffers.(state.buffer).bitmask).(0)
     for i = 1,ntags-1 do begin

        info = (*buffers.(state.buffer).bitmask).(i)
        mask = mc_bitset(bitmask,info[0],CANCEL=cancel)
        z = where(mask eq 1,cnt)
        if cnt ne 0 then img[z] = info[1]

     endfor
  
  endif

  *buffers.(state.buffer).bytimg = img

end
;
;===============================================================================
;
pro ximgtool_chbuffer

  common ximgtool_state
  
  tvlct,buffers.(state.buffer).red,buffers.(state.buffer).green,$
        buffers.(state.buffer).blue,state.color_bottom
  
  ximgtool_stretchct,BRICON=buffers.(state.buffer).bricon

  state.region = !values.f_nan
  state.cursormode = ['Zoom Mode','None']

  ximgtool_plotupdate
  
;  Update title bar
    
  widget_control, state.ximgtool_base,TLB_SET_TITLE=buffers.(state.buffer).title
  
;  Update menus
  
  ximgtool_setminmax
  ximgtool_updatebuttons
  
;  Find cursor position
  
  pix  = (convert_coord(state.devpos[0],state.devpos[1], $
                        /DEVICE,/TO_DATA))[0:1]
  state.cursorpos = pix
  zpix = mc_roundgt(pix)
  state.cursorpix = zpix
  
end
;
;==============================================================================
;
pro ximgtool_chcmap

  common ximgtool_state

  case buffers.(state.buffer).imgcmap of 
     
     'Grey': mc_mkct,0,RED=red,GREEN=green,BLUE=blue
     
     'Blue': mc_mkct,1,RED=red,GREEN=green,BLUE=blue
     
     'Rainbow': mc_mkct,34,RED=red,GREEN=green,BLUE=blue
     
     'Heat': mc_mkct,3,RED=red,GREEN=green,BLUE=blue
     
     'Green': mc_mkct,8,RED=red,GREEN=green,BLUE=blue
     
  endcase
  
  if buffers.(state.buffer).invert then begin
     
     red   = reverse(temporary(red))
     green = reverse(temporary(green))
     blue  = reverse(temporary(blue))
     
  endif
  
  buffers.(state.buffer).red   = red
  buffers.(state.buffer).green = green
  buffers.(state.buffer).blue  = blue
  
  tvlct,buffers.(state.buffer).red,buffers.(state.buffer).green,$
        buffers.(state.buffer).blue,state.color_bottom
  
  ximgtool_stretchct,BRICON=buffers.(state.buffer).bricon
  
end
;
;=============================================================================
;
pro ximgtool_clearframes,frames,CANCEL=cancel

  cancel = 0

  common ximgtool_state

  tmp = indgen(5)

  for i = 0,n_elements(frames)-1 do begin

     buffer = frames[i]-1
     
     zz = where(tmp ne buffer and state.loadedbuffer eq 1,cnt)
     
     if cnt eq 0 then begin
        
        ok = dialog_message('Cannot clear when only ' + $
                            'one buffer loaded.',/INFO, $
                            DIALOG_PARENT=state.ximgtool_base)
        return
        
     endif
     
     state.loadedbuffer[buffer] = 0
     state.blinkbuffer[buffer] = 0
     
     if state.bufferblinkpanel then begin
        
        if buffer eq 0 then widget_control, state.but_blinkbuf[0], $
                                            SENSITIVE=0
        if buffer eq 1 then widget_control, state.but_blinkbuf[1], $
                                            SENSITIVE=0
        if buffer eq 2 then widget_control, state.but_blinkbuf[2], $
                                            SENSITIVE=0
        if buffer eq 3 then widget_control, state.but_blinkbuf[3], $
                                            SENSITIVE=0
        if buffer eq 4 then widget_control, state.but_blinkbuf[4], $
                                            SENSITIVE=0
        
     endif
     
     wid = buffers.(buffer).pixmap_wid
     buffers.(buffer) = state.blankbuffer
     buffers.(buffer).pixmap_wid = wid
     state.buffer = tmp[zz[0]]
     ximgtool_chbuffer

  endfor
     
end
;
;=============================================================================
;
pro ximgtool_findposition

  common ximgtool_state

  ra = mc_cfld(state.ra_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return

  dec = mc_cfld(state.dec_fld,7,/EMPTY,CANCEL=cancel)
  if cancel then return
  
;  Check to see if it is sexagesimal or decimal degrees

  ra = strtrim(ra,2)
  dec = strtrim(dec,2)

  rasep = strsplit(ra,' ',/EXTRACT)
  if n_elements(rasep) gt 1 then begin

     ra = 15D*ten(rasep[0],rasep[1],rasep[2])

  endif else ra = double(ra)

  decsep = strsplit(dec,' ',/EXTRACT)

  if n_elements(decsep) gt 1 then begin

     dec = ten(decsep[0],decsep[1],decsep[2])

  endif else dec = double(dec)

  
  

  






end
;
;=============================================================================
;
pro ximgtool_imgrange

  common ximgtool_state

;  Full image or box image?

  box = state.region

  if finite(box[0]) eq 1 then begin

     xy1 = box[*,0]
     xy2 = box[*,1]
     
     x = [min([xy1[0],xy2[0]],MAX=max),max]
     y = [min([xy1[1],xy2[1]],MAX=max),max]
     
     x = 0.0 > (x < (buffers.(state.buffer).ncols-1))
     y = 0.0 > (y < (buffers.(state.buffer).nrows-1))
     
     img = (*buffers.(state.buffer).rotimg)[x[0]:x[1],y[0]:y[1]]
     
  endif else img = (*buffers.(state.buffer).rotimg) 
  
  s = size(img,/DIMEN)
  ncols = s[0]
  nrows = s[1]
  
  case buffers.(state.buffer).imgrange of 
     
     'MinMax': begin

        mc_imgrange,img,min,max,RANGE=1.0,CANCEL=cancel
        if cancel then return
     
     end

     '99.5%': begin

        mc_imgrange,img,min,max,RANGE=0.995,CANCEL=cancel
        if cancel then return     

     end

     '99%': begin

        mc_imgrange,img,min,max,RANGE=0.99,CANCEL=cancel
        if cancel then return     

     end

     '98%': begin

        mc_imgrange,img,min,max,RANGE=0.98,CANCEL=cancel
        if cancel then return
        
     end

     '95%': begin

        mc_imgrange,img,min,max,RANGE=0.95,CANCEL=cancel
        if cancel then return

     end

     '90%': begin

        mc_imgrange,img,min,max,RANGE=0.90,CANCEL=cancel
        if cancel then return
     
     end

     'ZScale': begin

        mc_imgrange,img,min,max,/ZSCALE,CANCEL=cancel
        if cancel then begin

           print, 'Unable to zscale, defaulting to 99.5%.'

           mc_imgrange,img,min,max,RANGE=0.995,CANCEL=cancel
           if cancel then return
           
           buffers.(state.buffer).imgrange ='99.5%'

        endif

     end
     
     else: return
     
  endcase
  
  if min eq max then begin
     
     min = min-1
     max = max+1
     
  endif
  
  buffers.(state.buffer).min = min
  buffers.(state.buffer).max = max
  
end
;
;******************************************************************************
;
pro ximgtool_imgmath

  common ximgtool_state

;  Check to make sure the image size agree

  s1 = size(*buffers.(state.mbuf1).rotimg)
  ncols1 = s1[1]
  nrows1 = s1[2]

  s2 = size(*buffers.(state.mbuf2).rotimg)
  ncols2 = s2[1]
  nrows2 = s2[2]
  
  if s1[0] ne s2[0] or s1[1] ne s2[1] then begin
     
     ok = dialog_message('Images sizes do not match.',/ERROR,$
                         DIALOG_PARENT=state.ximgtool_base)
     
     return
     
  endif
  
  case state.mop of 
     
     0: tmp = *buffers.(state.mbuf1).rotimg + *buffers.(state.mbuf2).rotimg
     
     1: tmp = *buffers.(state.mbuf1).rotimg - *buffers.(state.mbuf2).rotimg
     
     2: tmp = *buffers.(state.mbuf1).rotimg * $
              float(*buffers.(state.mbuf2).rotimg)
     
     3: tmp = *buffers.(state.mbuf1).rotimg / $
              float(*buffers.(state.mbuf2).rotimg)
     
  endcase
  
  ximgtool_loadimage,tmp,BUFFER=state.mbuf3+1
  
end
;
;===============================================================================
;
pro ximgtool_linecut

  common ximgtool_state
  
  p1 = state.region[*,0]
  p2 = state.region[*,1]

  if total(p1-p2) eq 0 then return

  nPoints = ABS(p2[0]-p1[0]+1) > ABS(p2[1]-p1[1]+1)
  
  xloc = p1[0] + (p2[0] - p1[0]) * Findgen(nPoints) / (nPoints - 1)
  yloc = p1[1] + (p2[1] - p1[1]) * Findgen(nPoints) / (nPoints - 1)
  
  z = where(xloc gt 0 and yloc gt 0 and xloc lt buffers.(state.buffer).ncols $
            and yloc lt buffers.(state.buffer).nrows)
  
  
  profile = Interpolate(*buffers.(state.buffer).rotimg, xloc, yloc)
  
  xzoomplot,findgen(n_elements(profile)),float(profile)
  
;  Reload plotting parameters
  
  wset, state.plotwin_wid
  !x = buffers.(state.buffer).xscale
  !y = buffers.(state.buffer).yscale
  !p = buffers.(state.buffer).pscale
  
end
;
;==============================================================================
;
pro ximgtool_loadimage,img,EXT=ext,ZRANGE=zrange,SCALE=scale, $
                       ROTATION=rotation,ZOOM=zoom,ZTOFIT=ztofit,$
                       XYPOS=xypos,COLORMAP=colormap,INVERT=invert, $
                       NOUPDATE=noupdate,BITMASK=bitmask,$
                       BUFFER=buffer,RANGE=range,STDIMAGE=stdimage, $
                       OPLOT=oplot,ZUNITS=zunits,FILENAME=filename,HDR=hdr, $
                       NODISPLAY=nodisplay,CANCEL=cancel

  common ximgtool_state

  cancel = 0

;  setup std image size if need be
  
  if keyword_set(STDIMAGE) then begin
     
     state.stdimage = stdimage
     state.dev2img = state.stdimage/512.
     
  endif
  
;  Get image and header
  
  if n_elements(img) eq 0 then begin

;  The second header is the one that is modified as the image is
;  rotated.

     timg = intarr(512,512)+27
     zrange = [0,255]
     hdr = ''
     phdr = ''
     
  endif

;  Pick Buffer
  
  state.buffer = 0
  
  if keyword_set(BUFFER) then state.buffer = buffer-1

  if n_elements(img) gt 0 then begin

;  Load the image if passed

     if size(img,/TYPE) eq 7 then begin

        img = mc_cfile(img,CANCEL=cancel)
        if cancel then return

        buffers.(state.buffer).filename= $
           strmid(img,strpos(img,'/',/REVERSE_S)+1)
        
        fits_open,img,fcb
        if fcb.nextend gt 0 then begin

           if n_elements(ext) eq 0 then begin
           
              ext = xmc_select(strtrim(indgen(fcb.nextend+1),2), $
                              'Please select an extension number.','Ext.: ',$
                               GROUP_LEADER=state.ximgtool_base,$
                               /BLOCK,CANCEL=cancel)
              if cancel then return
           
           endif 

        endif else ext = 0
        
        fits_read,fcb,timg,hdr,EXTEN_NO=total(ext)
        fits_close,fcb

        if state.divisorson and *state.divisors ne '' then begin

           for i = 0,n_elements(*state.divisors)-1 do begin
              
              val = fxpar(hdr,(*state.divisors)[i])
              if val eq 0 then begin

                 result = dialog_message('Divisor keyword "'+ $
                                         (*state.divisors[i])+$
                                         '" not found in header.',/ERROR, $
                                         DIALOG_PARENT=state.ximgtool_base)
                 cancel = 1
                 *state.divisors = ''
                 state.divisorson = 0
                 timg = intarr(512,512)+27
                 zrange = [0,255]
                 hdr = ''
                 phdr = ''

                 goto, continue
                                  
              endif
              timg = temporary(timg)/float(val)

           endfor
           buffers.(state.buffer).divisorslabel = strjoin(*state.divisors,',')

           
        endif else buffers.(state.buffer).divisorslabel = ''
        
     endif else begin

        buffers.(state.buffer).filename=''
        *(buffers.(state.buffer).wcsinfo) = ptr_new()
        *(buffers.(state.buffer).pwcsinfo) = ptr_new()
        buffers.(state.buffer).equinox = ''     
        
        timg = img
        
     endelse
     
;  Try and get WCS info

     if size(img,/TYPE) eq 7 or n_elements(hdr) gt 1 then begin

        extast,hdr,astr,noparms
        
        if noparms gt 0 then begin
           
           *(buffers.(state.buffer).wcsinfo) = astr
           *(buffers.(state.buffer).pwcsinfo) = astr
           equ = get_equinox(hdr, code)
           if code ne -1 then begin
              
              case equ of 
                 
                 2000.0: buffers.(state.buffer).equinox = 'J2000'
                 
                 1950.0: buffers.(state.buffer).equinox = 'B1950'
                 
                 else: buffers.(state.buffer).equinox = string(equ, $
                                                               FORMAT='(F6.1)')
                 
              endcase
              
           endif
           
        endif else begin
           
           *(buffers.(state.buffer).wcsinfo) = ptr_new()
           *(buffers.(state.buffer).pwcsinfo) = ptr_new()
           buffers.(state.buffer).equinox = ''
           
        endelse
        
     endif
     
     if n_elements(FILENAME) ne 0 then $
        buffers.(state.buffer).filename=filename

;  Check to make sure it is 2D
     
     s = size(timg)
     
     if s[0] ne 2 then begin
        
        result = dialog_message('FITS array must be 2D.',/ERROR, $
                                DIALOG_PARENT=state.ximgtool_base)
        cancel = 1
        return
        
     endif

;  If NOUPDATE, check to see if the image is the same size as the
;  current buffer image.

     if keyword_set(NOUPDATE) then begin
        
        s = size(timg,/DIMEN)
        if s[0] ne buffers.(state.buffer).ncols or $
           s[1] ne buffers.(state.buffer).nrows then begin 

           print, 'Cannot use current settings.  Defaulting.'
           noupdate = 0
           
        endif
        
     endif

;  Check to see if it is a byte image.

     if size(timg,/TYPE) eq 1 then timg = fix(temporary(timg))

     if n_elements(hdr) eq 0 then hdr = ''

  endif

  continue:

;  Update title

  if not keyword_set(NOUPDATE) then begin
     
     title = 'Ximgtool ['+string(state.plotwinsize[0],FORMAT='(I4)')+','+ $
             string(state.plotwinsize[1],FORMAT='(I4)')+ $
             '] ('+strtrim(state.buffer+1,2)+')'

     if buffers.(state.buffer).filename ne '' then title = title+' - '+$
        strtrim(buffers.(state.buffer).filename,2)

     buffers.(state.buffer).title = title
     
  endif
          
;  Update buffer info
  
  state.loadedbuffer[state.buffer] = 1         
  z = where(state.loadedbuffer eq 1,cnt)
  state.blinkbuffer[z] = 1      

  if state.bufferblinkpanel then begin

     if state.buffer eq 0 then widget_control, state.but_blinkbuf[0], /SENSITIVE
     if state.buffer eq 1 then widget_control, state.but_blinkbuf[1], /SENSITIVE
     if state.buffer eq 2 then widget_control, state.but_blinkbuf[2], /SENSITIVE
     if state.buffer eq 3 then widget_control, state.but_blinkbuf[3], /SENSITIVE
     if state.buffer eq 4 then widget_control, state.but_blinkbuf[4], /SENSITIVE

  endif

;  Load data
  
  *buffers.(state.buffer).img = timg
  *buffers.(state.buffer).hdr = hdr
  *buffers.(state.buffer).phdr = hdr
  
;  Rotate image
  
  if n_elements(ROTATION) ne 0 then begin

     if rotation gt 7 then begin

        print, 'Rotation unknown, defaulting to 0'
        rotation = 0

     endif 

  endif else rotation = 0

  if not keyword_set(NOUPDATE) then buffers.(state.buffer).rotation = rotation

  *buffers.(state.buffer).rotimg=rotate(timg,buffers.(state.buffer).rotation)
  
;  Zero things

  state.region     = !values.f_nan
  state.pstart     = !values.f_nan
  state.cursormode = ['Zoom Mode','None']

;  Check for image keywords

  if not keyword_set(NOUPDATE) then begin
     
;  Center and zero the zoom level
     
     s = size(*buffers.(state.buffer).rotimg)
     buffers.(state.buffer).ncols  = s[1]
     buffers.(state.buffer).nrows  = s[2]
     
     if keyword_set(ZTOFIT) then begin

        iaratio = buffers.(state.buffer).ncols/ $
                  float(buffers.(state.buffer).nrows)
  
        daratio = state.plotwinsize[0]/ $
                  float(state.plotwinsize[1])

        if daratio gt iaratio then begin
           
           zoom_factor = float(state.plotwinsize[1])/ $
                         buffers.(state.buffer).nrows*state.dev2img
           
        endif else begin
           
           zoom_factor = float(state.plotwinsize[0])/ $
                         buffers.(state.buffer).ncols*state.dev2img
           
        endelse
        
        buffers.(state.buffer).zoom = alog10(zoom_factor)/alog10(2)
        
        buffers.(state.buffer).cenpix = $
           [buffers.(state.buffer).ncols, $
            buffers.(state.buffer).nrows]/2.-0.5
        
     endif else begin

        buffers.(state.buffer).cenpix = (n_elements(XYPOS) eq 0) ? $
                                        [buffers.(state.buffer).ncols, $
                                         buffers.(state.buffer).nrows]/2. $
                                        -0.5:xypos
        state.cursorpix = buffers.(state.buffer).cenpix 
        buffers.(state.buffer).zoom   = (n_elements(ZOOM) eq 0) ? 0.0:zoom
     
     endelse

;  Check image range
     
     if n_elements(RANGE) ne 0 then begin
        
        ranges = ['MinMax','99.5%','99%','98%','95%','90%','ZScale']
        z = where(strlowcase(ranges) eq strlowcase(range),cnt)
        if cnt eq 0 then begin
           
           print, 'Range unknown, defaulting to MinMax.'
           buffers.(state.buffer).imgrange = 'MinMax'
           
        endif else buffers.(state.buffer).imgrange = ranges[z]
        
     endif else buffers.(state.buffer).imgrange = 'ZScale'
     
;  Zrange takes precedence over range keywords
     
     if n_elements(ZRANGE) gt 0 then begin
        
        buffers.(state.buffer).min = zrange[0]
        buffers.(state.buffer).max = zrange[1]
        buffers.(state.buffer).imgrange = 'User'
        
     endif

;  Check image scaling

     if n_elements(SCALE) ne 0 then begin

        scalings = ['Linear','Sqrt','Squared','Log','Asinh','Hist Eq']
        z = where(strcompress(strlowcase(scalings),/RE) eq $
                  strcompress(strlowcase(scale),/RE),cnt)
        if cnt eq 0 then begin

           print, 'Scaling unknown, defaulting to Linear.'
           buffers.(state.buffer).imgscaling = 'Linear'
        
        endif else buffers.(state.buffer).imgscaling = scalings[z]
        
     endif else buffers.(state.buffer).imgscaling = 'Linear'
     
;  load color map
     
     if n_elements(COLORMAP) ne 0 then begin

        colors = ['Grey','Blue','Rainbow','Heat','Green']
        z = where(strlowcase(colors) eq strlowcase(colormap),cnt)
        if cnt eq 0 then begin

           print, 'Color Map unknown, defaulting to Grey.'
           buffers.(state.buffer).imgcmap='Grey'

        endif else buffers.(state.buffer).imgcmap=colors[z]
        
     endif else buffers.(state.buffer).imgcmap='Grey'

;  Invert?

     buffers.(state.buffer).invert = keyword_set(INVERT)
     widget_control, state.but_invert, $
                     SET_BUTTON=buffers.(state.buffer).invert
     
;  Load Mask

     if n_elements(BITMASK) eq 0 then begin

        buffers.(state.buffer).plotbitmask = 0
        *buffers.(state.buffer).bitmask = intarr(buffers.(state.buffer).ncols,$
                                                 buffers.(state.buffer).nrows)
        
     endif else begin
        
        *buffers.(state.buffer).bitmask = bitmask
        buffers.(state.buffer).plotbitmask = 1
        widget_control, state.but_mask, $
                        SET_BUTTON=buffers.(state.buffer).plotbitmask

     endelse

     ximgtool_imgrange
     ximgtool_setminmax
     ximgtool_chcmap

;  Store the zunits
  
     buffers.(state.buffer).zunits = n_elements(ZUNITS) ? zunits:''
  
;  Figure out size of labels, XYZ first 
  
     imgmin = min(*buffers.(state.buffer).img,MAX=imgmax,/NAN)
     
     maxtext = 'X: '+strtrim(string(buffers.(state.buffer).ncols, $
                                    FORMAT='(f7.2)'),2)+ $
               ',  Y: '+strtrim(string(buffers.(state.buffer).nrows $
                                       ,FORMAT='(f7.2)'),2)+$
               ',  Z: '+strtrim(string(imgmax) ,2)+' '+ $
               buffers.(state.buffer).zunits
     
     mintext = 'X: '+strtrim(string(buffers.(state.buffer).ncols, $
                                    FORMAT='(f7.2)'),2)+ $
               ',  Y: '+strtrim(string(buffers.(state.buffer).nrows $
                                       ,FORMAT='(f7.2)'),2)+$
               ',  Z: '+strtrim(string(imgmin) ,2)+' '+ $
               buffers.(state.buffer).zunits+' '
     
     
     window, XSIZE=10,YSIZE=10,/FREE,/PIXMAP
     
     xyouts,0.5,0.5,maxtext,FONT=0,WIDTH=maxwidth
     xyouts,0.5,0.5,mintext,FONT=0,WIDTH=minwidth
     
     wdelete,!d.window
     buffers.(state.buffer).infosize = (maxwidth > minwidth)*10
     
;  Now do the divisors label

     if (*state.divisors) ne '' then begin
        
        text = 'Divisors: '+buffers.(state.buffer).divisorslabel+' '
        window, XSIZE=10,YSIZE=10,/FREE,/PIXMAP
        
        xyouts,0.5,0.5,text,FONT=0,WIDTH=width
        wdelete,!d.window
        buffers.(state.buffer).divisorssize = width*10
        
     endif
          
  endif 

;  Load the overplot

  *buffers.(state.buffer).oplot = (n_elements(OPLOT) ne 0) ? $
                                  oplot:!values.f_nan
  
;  Create the image

  ximgtool_bytsclimg
  ximgtool_stretchct,BRICON=buffers.(state.buffer).bricon 
  ximgtool_mkplotimg,/PANIMG

;  if keyword_set(ROTATION) then ximgtool_rotateimg,rotation

;  Display the image

  if not keyword_set(NODISPLAY) then begin

     widget_control, state.ximgtool_base,TLB_SET_TITLE=buffers.(state.buffer).title
     ximgtool_plotupdate
     ximgtool_trackwin
     ximgtool_updatebuttons

  endif

;  unlock all images

  widget_control,  state.but_lock, SET_BUTTON=0 
  state.lock = 0
  

end
;
;=============================================================================
;
pro ximgtool_lockimages,CENTER=center

  
  common ximgtool_state

  currentbuffer = state.buffer

  for i = 0,4 do begin
     
     if state.loadedbuffer[i] eq 1 then begin
        
        state.buffer = i
        !x = buffers.(i).xscale
        !y = buffers.(i).yscale
        !p = buffers.(i).pscale
        buffers.(i).zoom   = buffers.(currentbuffer).zoom
        buffers.(i).cenpix = buffers.(currentbuffer).cenpix
        ximgtool_chcmap
        ximgtool_mkplotimg
        ximgtool_plotupdate,RELOAD=(i eq currentbuffer) ? 0:1,CENTER=center

     endif
     
  endfor

  state.buffer = currentbuffer
  ximgtool_chcmap
  ximgtool_plotupdate,RELOAD=1,CENTER=center
  !x = buffers.(state.buffer).xscale
  !y = buffers.(state.buffer).yscale
  !p = buffers.(state.buffer).pscale
              
end
;
;===============================================================================
;
pro ximgtool_mkplotimg,PANIMG=panimg

  common ximgtool_state

;  Get the size of the image that is displayed given the zoom level (zwin_size)
;  Get the size of the plot window area (pwin_size)

  zoom_factor = 2.^buffers.(state.buffer).zoom
  zwin_size   = state.plotwinsize / zoom_factor * state.dev2img
  dev_offset  = (zwin_size-floor(zwin_size)) * zoom_factor / $
                state.dev2img / 2.
  
  zwin_size   = floor(zwin_size)
  pwin_size   = round(state.plotwinsize-dev_offset)

  
;  Given the zwin_size and the center pixel value, what is the FULL
;  range of pixels values that will be shown.
  
  xyll        = mc_roundgt(buffers.(state.buffer).cenpix- zwin_size/2. )
  xyur        = xyll+zwin_size-1

  buffers.(state.buffer).pixdisp = [xyur[0]-xyll[0],xyur[1]-xyll[1]]

;  Now trim these ranges to be between the image size limits
  
  xyll_img    = xyll > 0.
  xyur_img    = fltarr(2)
  
  xyur_img[0] = xyur[0] < (buffers.(state.buffer).ncols-1)
  xyur_img[1] = xyur[1] < (buffers.(state.buffer).nrows-1)
  
  
;  Get the actually size of the part of the image that will be
;  displayed and the offset from the lower left hand corder.
  
  zwin_imgsize = xyur_img-xyll_img+1
  zwin_offset  = xyll_img-xyll
  
;  Now compute the image in display coordinates and the offset.
  
  pwin_imgsize = round( (pwin_size/float(zwin_size))*zwin_imgsize) > 1
  pwin_offset  = round( (pwin_size/float(zwin_size))*zwin_offset)

  img = *buffers.(state.buffer).bytimg

  *buffers.(state.buffer).plotimg = congrid(img[xyll_img[0]:xyur_img[0],$
                                                xyll_img[1]:xyur_img[1]],$
                                            pwin_imgsize[0],pwin_imgsize[1])

  buffers.(state.buffer).offset = pwin_offset+dev_offset

  
  buffers.(state.buffer).xrange   = xyll_img[0]+[0,zwin_imgsize[0]]-0.5

  buffers.(state.buffer).yrange   = xyll_img[1]+[0,zwin_imgsize[1]]-0.5

  buffers.(state.buffer).position = [buffers.(state.buffer).offset[0], $
                                       buffers.(state.buffer).offset[1],$
                                       buffers.(state.buffer).offset[0]+ $
                                       pwin_imgsize[0],$
                                       buffers.(state.buffer).offset[1]+ $
                                       pwin_imgsize[1]]

  if keyword_set(PANIMG) then begin


     *buffers.(state.buffer).panimg =  intarr(state.trackwinsize, $
                                              state.trackwinsize)+20
  
     aratio = buffers.(state.buffer).ncols/float(buffers.(state.buffer).nrows)
     
     if aratio gt 1 then begin
        
        xsize = state.trackwinsize > 1 
        ysize = state.trackwinsize/aratio > 1
        
     endif else begin
        
        xsize = state.trackwinsize*aratio > 1
        ysize = state.trackwinsize > 1
        
     endelse

     nx = congrid(*buffers.(state.buffer).bytimg,xsize,ysize)
     offset = ([state.trackwinsize,state.trackwinsize]-[xsize,ysize])/2.
     (*buffers.(state.buffer).panimg)[offset[0],offset[1]] = nx
     
  endif
  
end
;
;===============================================================================
;
pro ximgtool_moments

  common ximgtool_state

  xy1 = state.region[*,0]
  xy2 = state.region[*,1]
  
  x = [min([xy1[0],xy2[0]],MAX=max),max]
  y = [min([xy1[1],xy2[1]],MAX=max),max]
  
  x = 0.0 > (x < buffers.(state.buffer).ncols)
  y = 0.0 > (y < buffers.(state.buffer).nrows)
  
  img = (*buffers.(state.buffer).img)[x[0]:x[1],y[0]:y[1]]
  
  imgsize = [x[1]-x[0]+1,y[1]-y[0]+1]
  
  npts = n_elements(img)
  min = min(img,MAX=max,/NAN)
  
  mc_moments,img,mean,var,stddev,stderror,skew,kurt,/SILENT,CANCEL=cancel
  if cancel then return
  
  med = median(img,/EVEN)
  MAD = 1.482*median( abs(img-med),/EVEN)
  
  total = total(img,/NAN)
  z = where(finite(img) eq 0,cnt)
  
  
  arr = [['               '],$
         ['   Xrange : ('+strjoin([strtrim(fix(x),2)],', ')+')'],$
         ['   Yrange : ('+strjoin([strtrim(fix(y),2)],', ')+')'],$
         [' Box Size : '+strtrim(fix(imgsize[0]),2)+' x '+ $
          strtrim(fix(imgsize[1]),2)],$
         [' # Points : '+strtrim(npts,2)],$
         ['                '],$
         ['   #  NaN : '+strtrim(cnt,2)],$
         ['      Min : '+strtrim(min,2)],$
         ['      Max : '+strtrim(max,2)],$
         ['    Total : '+strtrim(total,2)],$
         ['          '],$
         ['     Mean : '+strtrim(mean,2)],$
         [' Variance : '+strtrim(var,2)],$
         ['  Std Dev : '+strtrim(stddev,2)],$
         ['Std Error : '+strtrim(stddev/sqrt(npts),2)],$
         ['     Skew : '+strtrim(skew,2)],$
         [' Kurtosis : '+strtrim(kurt,2)],$
         ['          '],$
         ['   Median : '+strtrim(med,2)],$
         ['      MAD : '+strtrim(mad,2)]]
  
  xmc_displaytext,arr,TITLE='Moments',WSIZE=[250,320], $
                  GROUP_LEADER=state.ximgtool_base
  
;  Move the cursor just to re-focus the widget
  
  widget_control, state.plotwin, /INPUT_FOCUS
  
  wset, state.plotwin_wid
  tvcrs,state.cursorpix[0],state.cursorpos[1]+1,/DATA
  
end
;
;===============================================================================
;
pro ximgtool_plotupdate,CENTER=center,RELOAD=reload

  common ximgtool_state

;  Display main image

  wset, buffers.(state.buffer).pixmap_wid
  erase
  polyfill,[0,0,1,1,0],[0,1,1,0,0], COLOR=20,/NORM
  
  tv, *buffers.(state.buffer).plotimg,buffers.(state.buffer).offset[0], $
      buffers.(state.buffer).offset[1],/DEVICE

;  Set up coordinate system

  style = 5  ;  Set to 1 for debugging
  
  plot,indgen(10),/NODATA,/NOERASE,XSTY=style,YSTY=style, $
       XRANGE=buffers.(state.buffer).xrange,$
       YRANGE=buffers.(state.buffer).yrange,XMARGIN=[0,0],YMARGIN=[0,0], $
       COLOR=2,NOCLIP=0,/DEVICE,POSITION=buffers.(state.buffer).position

  buffers.(state.buffer).xscale = !x
  buffers.(state.buffer).yscale = !y
  buffers.(state.buffer).pscale = !p

;  Overplot any stuff requested

  if size(*buffers.(state.buffer).oplot,/TYPE) eq 11 and $
     state.oplot then begin

     for i = 0,n_elements(*buffers.(state.buffer).oplot)-1 do $
        (*buffers.(state.buffer).oplot)[i]->plot
  
  endif

;  Plot Pan Window

;  Messed up with small images.

  if state.panner then begin

     tv,*buffers.(state.buffer).panimg, $
        state.pannerpos[0],state.pannerpos[1],/DEVICE
     
     plots,[state.pannerpos[0],state.pannerpos[0], $
            state.pannerpos[0]+state.trackwinsize,$
            state.pannerpos[0]+state.trackwinsize,state.pannerpos[0]],$
           [state.pannerpos[1],state.pannerpos[1]+state.trackwinsize,$
            state.pannerpos[1]+state.trackwinsize,state.pannerpos[1],$
            state.pannerpos[1]],/DEVICE,COLOR=3,THICK=2

     aratio = buffers.(state.buffer).ncols/float(buffers.(state.buffer).nrows)
     
     if aratio gt 1 then begin
        
        offset = (buffers.(state.buffer).ncols-buffers.(state.buffer).nrows)/2
        
        ll = [buffers.(state.buffer).cenpix[0]- $
              buffers.(state.buffer).pixdisp[0]/2,$
              buffers.(state.buffer).cenpix[1]- $
              buffers.(state.buffer).pixdisp[1]/2+offset]/ $
             buffers.(state.buffer).ncols*float(state.trackwinsize) > 0
        
        ur = [buffers.(state.buffer).cenpix[0]+ $
              buffers.(state.buffer).pixdisp[0]/2,$
              buffers.(state.buffer).cenpix[1]+ $
              buffers.(state.buffer).pixdisp[1]/2+offset]/ $
             buffers.(state.buffer).ncols*float(state.trackwinsize) < $
             (state.trackwinsize)
               
     endif else begin
        
        offset = (buffers.(state.buffer).nrows-buffers.(state.buffer).ncols)/2

        ll = [buffers.(state.buffer).cenpix[0]- $
              buffers.(state.buffer).pixdisp[0]/2.+offset,$
              buffers.(state.buffer).cenpix[1]- $
              buffers.(state.buffer).pixdisp[1]/2.]-0.5 > (-0.5)

        ll = ll/(buffers.(state.buffer).nrows-1)*float(state.trackwinsize) > 0

        ur = [buffers.(state.buffer).cenpix[0]+ $
              buffers.(state.buffer).pixdisp[0]/2.+offset,$
              buffers.(state.buffer).cenpix[1]+ $
              buffers.(state.buffer).pixdisp[1]/2.]+0.5 < (buffers.(state.buffer).ncols+0.5)

        ur = ur/(buffers.(state.buffer).nrows-1)*float(state.trackwinsize) < $
             (state.trackwinsize)

     endelse     
     
     plots,state.pannerpos[0]+[ll[0],ll[0],ur[0],ur[0],ll[0]],$
           state.pannerpos[1]+[ll[1],ur[1],ur[1],ll[1],ll[1]], $
           COLOR=3,/DEVICE
  
     if n_tags(*buffers.(state.buffer).pwcsinfo) gt 0 then $
        arrows,*buffers.(state.buffer).phdr, $
            state.pannerpos[0]+state.trackwinsize/2,$
               state.pannerpos[1]+state.trackwinsize/2,COLOR=5,FONT=0, $
               CHARSIZE=1.5,THICK=1.5
        
  endif

;  Now overplot anything else we need.

  if not keyword_set(RELOAD) then begin

     wset, state.plotwin_wid
     device, COPY=[0,0,state.plotwinsize[0],state.plotwinsize[1],0,0,$
                   buffers.(state.buffer).pixmap_wid]

;  Plot the range box.

     if state.cursormode[0] eq 'Range Mode' then begin

        box = state.region
        
        plots,[box[0,0]-0.5,box[0,0]-0.5,box[0,1]+0.5, $
               box[0,1]+0.5,box[0,0]-0.5],$
              [box[1,0]+0.5,box[1,1]-0.5,box[1,1]-0.5, $
               box[1,0]+0.5,box[1,0]+0.5],$
              COLOR=state.rangeboxcolor,THICK=2
        
        plots,box[0,1]+0.5,box[1,1]-0.5,PSYM=8,COLOR=state.rangeboxcolor    

     endif

     if state.cursormode[0] eq 'Stats Mode' then begin
        
        box = state.region
        
        plots,[box[0,0]-0.5,box[0,0]-0.5,box[0,1]+0.5, $
               box[0,1]+0.5,box[0,0]-0.5],$
              [box[1,0]+0.5,box[1,1]-0.5,box[1,1]-0.5, $
               box[1,0]+0.5,box[1,0]+0.5],$
              COLOR=state.statboxcolor,THICK=2
        
        plotsym,0,1,/FILL
        plots,box[0,1]+0.5,box[1,1]-0.5,PSYM=8,COLOR=state.statboxcolor

        
     endif

     if state.cursormode[0] eq 'Line Cut Mode' then begin


        plots,[state.region[0,0],state.region[0,1]],$
              [state.region[1,0],state.region[1,1]], $
              COLOR=state.lineregcolor,THICK=2 

        plots,state.region[0,0],state.region[1,0],PSYM=6, $
              COLOR=state.lineregcolor

        plots,state.region[0,1],state.region[1,1],PSYM=8, $
              COLOR=state.lineregcolor
          
     endif

     if state.cursormode[0] eq 'Distance Mode' then begin

        plots,[state.region[0,0],state.region[0,1]],$
              [state.region[1,0],state.region[1,1]], $
              COLOR=state.distregcolor,THICK=2 

        plots,state.region[0,0],state.region[1,0],PSYM=6, $
              COLOR=state.distregcolor

        plots,state.region[0,1],state.region[1,1],PSYM=8, $
              COLOR=state.distregcolor

        dy = state.region[1,1]-state.region[1,0]
        dx = state.region[0,1]-state.region[0,0]
        d  = sqrt(dy^2 + dx^2)

        !except=0
        string = strtrim(fix(dx),2)+', '+strtrim(fix(dy),2)+', '+strtrim(d,2)
        del = (dy gt 0.0) ? 10:-20
        junk = check_math()

        xy = convert_coord(state.region[0,1],state.region[1,1], $
                           /DATA,/TO_DEVICE)
        xyouts,xy[0],xy[1]+del,string,/DEVICE,COLOR=5,FONT=0,CHARSIZE=12, $
               ALIGNMENT=0.5
                
     endif

     if keyword_set(CENTER) then $
        tvcrs,mc_roundgt(buffers.(state.buffer).cenpix[0]),$
              mc_roundgt(buffers.(state.buffer).cenpix[1]),/DATA
     
  endif
  
end
;
;******************************************************************************
;
pro ximgtool_rotateimg,rotation

  common ximgtool_state

;;  Rotate the range box if necessary
;
;  xr = reform(buffers.(state.buffer).rangebox[0,*])
;  yr = reform(buffers.(state.buffer).rangebox[1,*])
;
;  xs = reform(buffers.(state.buffer).statbox[0,*])
;  ys = reform(buffers.(state.buffer).statbox[1,*])
;
;  case buffers.(state.buffer).rotation of 
;     
;     0: begin
;        
;        xxr = xr
;        yyr = yr
;
;        xxs = xs
;        yys = ys
;
;     end
;
;     1: begin
;
;        xxr = [xr[0],xr[1]]
;        yyr = [yr[1],yr[0]]
;
;        xxs = [xs[0],xs[1]]
;        yys = [ys[1],ys[0]]
;
;     end
;
;     2: begin
;
;        xxr = [xr[1],xr[0]]
;        yyr = [yr[0],yr[1]]
;
;        xxs = [xs[1],xs[0]]
;        yys = [ys[0],ys[1]]
;
;     end
;
;     3: begin
;
;        xxr = [xr[1],xr[0]]
;        yyr = [yr[0],yr[1]]
;
;        xxs = [xs[1],xs[0]]
;        yys = [ys[0],ys[1]]
;
;     end
;
;     4: begin
;        
;        xxr = [xr[1],xr[0]]
;        yyr = [yr[1],yr[0]]
;
;        xxs = [xs[1],xs[0]]
;        yys = [ys[1],ys[0]]
;
;     end
;
;     5: begin
;
;        xxr = [xr[1],xr[0]]
;        yyr = [yr[0],yr[1]]
;
;        xxs = [xs[1],xs[0]]
;        yys = [ys[0],ys[1]]
;
;     end
;
;     6: begin
;
;        xxr = [xr[0],xr[1]]
;        yyr = [yr[0],yr[1]]
;
;        xxs = [xs[0],xs[1]]
;        yys = [ys[0],ys[1]]
;
;     end
;
;     7: begin
;
;        xxr = [xr[0],xr[1]]
;        yyr = [yr[1],yr[0]]
;
;        xxs = [xs[0],xs[1]]
;        yys = [ys[1],ys[0]]
;
;     end
;
;  endcase
;
  if rotation ne 8 then begin
;
;
;     mc_rotcoord,xxr,yyr,buffers.(state.buffer).ncols, $
;                 buffers.(state.buffer).nrows, $
;                 buffers.(state.buffer).rotation,tx,ty,/UNROTATE,CANCEL=cancel
;     if cancel then return
;     
;     ixr = [min(tx,MAX=max),max]
;     iyr = [max(ty,MIN=min),min]
;     
;     mc_rotcoord,xxs,yys,buffers.(state.buffer).ncols, $
;                 buffers.(state.buffer).nrows, $
;                 buffers.(state.buffer).rotation,tx,ty,/UNROTATE,CANCEL=cancel
;     if cancel then return
;     
;     ixs = [min(tx,MAX=max),max]
;     iys = [max(ty,MIN=min),min]
;     
;     s = size(*buffers.(state.buffer).img,/DIMEN)
;     ncols = s[0]
;     nrows = s[1]
;     
;     mc_rotcoord,ixr,iyr,ncols,nrows,rotation,tx,ty,CANCEL=cancel
;     fxr = [min(tx,MAX=max),max]
;     fyr = [max(ty,MIN=min),min]  
;     
;     buffers.(state.buffer).rangebox = [[fxr[0],fyr[0]],[fxr[1],fyr[1]]]
;     
;     mc_rotcoord,ixs,iys,ncols,nrows,rotation,tx,ty,CANCEL=cancel
;     fxr = [min(tx,MAX=max),max]
;     fyr = [max(ty,MIN=min),min]  
;
;     buffers.(state.buffer).statbox = [[fxr[0],fyr[0]],[fxr[1],fyr[1]]]

     if n_tags(*buffers.(state.buffer).pwcsinfo) gt 0 then begin 
        
        hrotate,*buffers.(state.buffer).img,*buffers.(state.buffer).hdr,$
                newimg,newhdr,rotation
        
        *buffers.(state.buffer).rotimg = newimg
        *buffers.(state.buffer).phdr = newhdr
        
        extast,newhdr,astr,noparms
        *(buffers.(state.buffer).pwcsinfo) = astr
        
     endif else begin
     
        *buffers.(state.buffer).rotimg = rotate(*buffers.(state.buffer).img,$
                                                rotation)
        
     endelse

;  Rotate the raw image and optional mask.
  
     *buffers.(state.buffer).bitmask = rotate(*buffers.(state.buffer).bitmask, $
                                              rotation)

  endif else begin

     getrot,*buffers.(state.buffer).hdr,angle,/SILENT

     hrot,*buffers.(state.buffer).img,*buffers.(state.buffer).hdr,newimg, $
          newhdr,angle,-1,-1,2

     *buffers.(state.buffer).rotimg = newimg
     *buffers.(state.buffer).phdr = newhdr
     
     extast,newhdr,astr,noparms
     *(buffers.(state.buffer).pwcsinfo) = astr
     
  endelse

;  Update the ncols, nrows, and central pixel of the image.

  s = size(*buffers.(state.buffer).rotimg,/DIMEN)
  buffers.(state.buffer).ncols = s[0]
  buffers.(state.buffer).nrows = s[1]
  
  buffers.(state.buffer).cenpix = [buffers.(state.buffer).ncols,$
                                   buffers.(state.buffer).nrows]/2.-0.5

  buffers.(state.buffer).rotation = rotation

end
;
;==============================================================================
;
pro ximgtool_setminmax

  common ximgtool_state

  if state.rangepanel then begin

     widget_control, state.imgmin_fld[1], $
                     SET_VALUE=strtrim(buffers.(state.buffer).min,2)
     widget_control, state.imgmax_fld[1], $
                     SET_VALUE=strtrim(buffers.(state.buffer).max,2)
  
  endif

end
;
;==============================================================================
;
pro ximgtool_stretchct,event,BRICON=bricon

  common ximgtool_state

;  Taken from ATV
;  http://www.physics.uci.edu/~barth/atv/

  ncolors = state.ncolors-state.color_bottom-1
  
  if n_params() eq 1 then bricon = [event.y/float(state.plotwinsize[1]),$
                                    1-event.x/float(state.plotwinsize[0])]
  
  x = bricon[0]*(ncolors-1)
  y = bricon[1]*(ncolors-1) > 2 ; Minor change by AJB 
  high = x+y & low = x-y
  diff = (high-low) > 1
  
  slope = float(ncolors-1)/diff ;Range to range of 0 : nc-1
  intercept = -slope*low
  idx = long(findgen(ncolors)*slope+intercept) ;subscripts to select
  
  tvlct, (buffers.(state.buffer).red)[idx],$
         (buffers.(state.buffer).green)[idx],$
         (buffers.(state.buffer).blue)[idx],state.color_bottom
  
  buffers.(state.buffer).bricon = bricon
  
end
;
;==============================================================================
;
pro ximgtool_trackwin

  common ximgtool_state

  if state.magnifier then begin
  
     if state.magnifierpos[0] eq 0 then return
     
     del = state.magnifier_size

     x = state.cursorpix[0]
     y = state.cursorpix[1]
     
     xrange = [mc_roundgt(x)-del,mc_roundgt(x)+del]
     yrange = [mc_roundgt(y)-del,mc_roundgt(y)+del]
     
     cxrange = 0 > xrange < (buffers.(state.buffer).ncols-1)
     cyrange = 0 > yrange < (buffers.(state.buffer).nrows-1)
     
     xoffset = cxrange[0]-xrange[0]
     yoffset = cyrange[0]-yrange[0]
     
     subimg = bytarr(2*del+1,2*del+1)+20
     
     if xoffset le 2*del and xoffset ge 0 and $
        yoffset le 2*del and yoffset ge 0 then begin
        
        subimg[xoffset,yoffset] = $
           (*buffers.(state.buffer).bytimg)[cxrange[0]:cxrange[1], $
                                              cyrange[0]:cyrange[1]]
        
     endif 
     
     tv,congrid(subimg,state.trackwinsize,state.trackwinsize), $
        state.magnifierpos[0],state.magnifierpos[1],/DEVICE
     
     plots,[state.magnifierpos[0],state.magnifierpos[0], $
            state.magnifierpos[0]+state.trackwinsize,$
            state.magnifierpos[0]+state.trackwinsize,$
            state.magnifierpos[0]],$
           [state.magnifierpos[1],state.magnifierpos[1]+state.trackwinsize,$
            state.magnifierpos[1]+state.trackwinsize,state.magnifierpos[1],$
            state.magnifierpos[1]],/DEVICE,COLOR=3,THICK=2
     
     cen = state.magnifierpos+state.trackwinsize/2
     plots,cen[0],cen[1],PSYM=6,COLOR=3,/DEVICE,THICK=1
  
  endif 


end
;
;==============================================================================
;
pro ximgtool_updatebuttons

  common ximgtool_state

  buf = state.buffer

;  Cursor

  widget_control, state.but_cursor[0],SET_BUTTON=0
  widget_control, state.but_cursor[1],SET_BUTTON=0
  widget_control, state.but_cursor[2],SET_BUTTON=0
  widget_control, state.but_cursor[3],SET_BUTTON=0

  case state.cursormode[0] of 

     'Zoom Mode': widget_control, state.but_cursor[0],/SET_BUTTON
     
     'Color Mode': widget_control, state.but_cursor[1],/SET_BUTTON

     'Distance Mode': widget_control, state.but_cursor[3],/SET_BUTTON
     
     'Range Mode': widget_control, state.but_cursor[2],/SET_BUTTON

     'Line Cut Mode': widget_control, state.but_cursor[3],/SET_BUTTON

     'Stats Mode': widget_control, state.but_cursor[3],/SET_BUTTON

     'Stretch Mode': widget_control, state.but_cursor[3],/SET_BUTTON

  endcase


;  Rotation

  for i = 0,8 do widget_control, state.but_rot[i],SET_BUTTON=0

  case buffers.(state.buffer).rotation of 

     0: widget_control, state.but_rot[0],/SET_BUTTON

     1: widget_control, state.but_rot[3],/SET_BUTTON

     2: widget_control, state.but_rot[4],/SET_BUTTON

     3: widget_control, state.but_rot[5],/SET_BUTTON

     4: widget_control, state.but_rot[6],/SET_BUTTON

     5: widget_control, state.but_rot[1],/SET_BUTTON

     6: widget_control, state.but_rot[7],/SET_BUTTON

     7: widget_control, state.but_rot[2],/SET_BUTTON

     8: widget_control, state.but_rot[8],/SET_BUTTON

  endcase

;  Image Range

  for i = 0,7 do widget_control, state.but_range[i],SET_BUTTON=0

  case buffers.(state.buffer).imgrange of

     'MinMax': widget_control, state.but_range[0],/SET_BUTTON

     '99.5%': widget_control, state.but_range[1],/SET_BUTTON

     '99%': widget_control, state.but_range[2],/SET_BUTTON

     '98%': widget_control, state.but_range[3],/SET_BUTTON

     '95%': widget_control, state.but_range[4],/SET_BUTTON

     '90%': widget_control, state.but_range[5],/SET_BUTTON
 
     'ZScale': widget_control, state.but_range[6],/SET_BUTTON
    
     'User': widget_control, state.but_range[7],/SET_BUTTON

  endcase

; Image Scale

  for i = 0,4 do widget_control, state.but_scale[i],SET_BUTTON=0

  case buffers.(state.buffer).imgscaling of 

     'Linear': widget_control, state.but_scale[0],/SET_BUTTON

     'Log': widget_control, state.but_scale[3],/SET_BUTTON

     'Sqrt': widget_control, state.but_scale[1],/SET_BUTTON

     'Squared': widget_control, state.but_scale[2],/SET_BUTTON

     'Hist Eq': widget_control, state.but_scale[4],/SET_BUTTON

  endcase

;  Color Map

  for i = 0,4 do widget_control, state.but_color[i],SET_BUTTON=0

  case buffers.(state.buffer).imgcmap of 

     'Grey': widget_control, state.but_color[0],/SET_BUTTON

     'Blue': widget_control, state.but_color[1],/SET_BUTTON

     'Rainbow': widget_control, state.but_color[2],/SET_BUTTON

     'Heat': widget_control, state.but_color[3],/SET_BUTTON

     'Green': widget_control, state.but_color[4],/SET_BUTTON

  endcase

;  Buffers

  widget_control, state.but_buffer[0],SET_BUTTON=0
  widget_control, state.but_buffer[1],SET_BUTTON=0
  widget_control, state.but_buffer[2],SET_BUTTON=0
  widget_control, state.but_buffer[3],SET_BUTTON=0
  widget_control, state.but_buffer[4],SET_BUTTON=0
  widget_control, state.but_buffer[state.buffer],/SET_BUTTON

  widget_control, state.but_buffer[0], SENSITIVE=state.loadedbuffer[0]

  widget_control, state.but_buffer[1], SENSITIVE=state.loadedbuffer[1]
  widget_control, state.but_buffer[2], SENSITIVE=state.loadedbuffer[2]
  widget_control, state.but_buffer[3], SENSITIVE=state.loadedbuffer[3]
  widget_control, state.but_buffer[4], SENSITIVE=state.loadedbuffer[4]

;  Lock button

  widget_control, state.but_lock,SET_BUTTON=state.lock

;  Invert Color Map

  widget_control, state.but_invert, SET_BUTTON=buffers.(state.buffer).invert

;  Zoom and Pan Win

  widget_control, state.but_magnifier,SET_BUTTON=state.magnifier
  widget_control, state.but_panner,SET_BUTTON=state.panner

;  Mask Button

  widget_control, state.but_mask, SET_BUTTON=buffers.(state.buffer).plotbitmask

end
;
;==============================================================================
;
pro ximgtool_updateinfobar

  common ximgtool_state
 
  xy = state.cursorpix

; Do XYZ first

  if xy[0] lt -0.5 or $
     xy[0] gt buffers.(state.buffer).ncols-0.5 or $
     xy[1] lt -0.5 or $
     xy[1] gt buffers.(state.buffer).nrows-0.5 then $
        z = !values.f_nan else $
           z = (*buffers.(state.buffer).rotimg)[xy[0],xy[1]]

  val = 'X: '+strtrim(string(state.cursorpos[0],FORMAT='(f7.2)'),2)+ $
        ',  Y: '+strtrim(string(state.cursorpos[1],FORMAT='(f7.2)'),2)+$
        ',  Z: '+strtrim(string(z) ,2)+' '+buffers.(state.buffer).zunits+' '
  
  size = buffers.(state.buffer).infosize
  polyfill, [state.plotwinsize[0]-12-size,state.plotwinsize[0]-12-size,$
             state.plotwinsize[0]-12,state.plotwinsize[0]-12,$
             state.plotwinsize[0]-12-size],$
            [12,12+15,12+15,12,12],COLOR=0,/DEVICE
  
  xyouts,state.plotwinsize[0]-3,16,val,/DEVICE,COLOR=5, $
         CHARSIZE=10,FONT=0,ALIGNMENT=1

;  Do Divisors if needed.

  if buffers.(state.buffer).divisorslabel ne '' then begin

     text = 'Divisors: '+buffers.(state.buffer).divisorslabel+'  '
     
     size = buffers.(state.buffer).divisorssize
     polyfill, [state.plotwinsize[0]-12-size,state.plotwinsize[0]-12-size,$
               state.plotwinsize[0]-12,state.plotwinsize[0]-12,$
               state.plotwinsize[0]-12-size],$
               [12,12+15,12+15,12,12]+18,COLOR=0,/DEVICE
     
     xyouts,state.plotwinsize[0]-3,16+18,text,COLOR=5,/DEVICE,ALIGNMENT=1,FONT=0


  endif
 
;  Do WCS if needed

  if n_tags(*buffers.(state.buffer).pwcsinfo) gt 0 then begin

     pane = 0
     xy2ad, xy[0],xy[1], *buffers.(state.buffer).pwcsinfo, ra,dec

     if state.sexagesimal then begin

        radec, ra, dec, ihr, imin, xsec, ideg, imn, xsc

        pm = (ideg lt 0 or imn lt 0 or xsc lt 0) ? '-':'+'
        
        string = string(ihr,imin,xsec,pm,abs(ideg),abs(imn),abs(xsc),$
                        FORMAT='(I2.2,":",I2.2,":",F06.3," ",A1,I2.2,":",' + $
                        'I2.2,":",F05.2," ")')+buffers.(state.buffer).equinox

        polyfill,[5,5,205,205,5]+12,[12,12+15,12+15,12,12],/DEVICE,COLOR=0

        xyouts,17,16,' '+string,COLOR=5,/DEVICE,ALIGNMENT=0,FONT=0

        pane = 1

     endif

     if state.decdeg then begin

        string = string(ra,dec,$
                        FORMAT='(D11.7,", ",D11.7," ")')+$
                        buffers.(state.buffer).equinox

        polyfill,[5,5,205,205,5]+12,[12,12+15,12+15,12,12]+18*pane,/DEVICE,COLOR=0

        xyouts,17,16+18*pane,' '+string,COLOR=5,/DEVICE,ALIGNMENT=0,FONT=0

        pane = 2

     endif


  endif


end
;
;==============================================================================
;
pro ximgtool_zoomtofit

  common ximgtool_state

;  crap.  depends on both what the ratios are, and whether one is
;  greater than the other.

  iaratio = buffers.(state.buffer).ncols/float(buffers.(state.buffer).nrows)
  
  daratio = state.plotwinsize[0]/float(state.plotwinsize[1])

  if daratio gt iaratio then begin

     zoom_factor = float(state.plotwinsize[1])/ $
                   buffers.(state.buffer).nrows*state.dev2img
     
  endif else begin

     zoom_factor = float(state.plotwinsize[0])/ $
                   buffers.(state.buffer).ncols*state.dev2img

  endelse

  buffers.(state.buffer).zoom = alog10(zoom_factor)/alog10(2)

  buffers.(state.buffer).cenpix = $
     [buffers.(state.buffer).ncols, $
      buffers.(state.buffer).nrows]/2.-0.5
  
  if state.lock then ximgtool_lockimages,/CENTER else begin
     
     state.cursormode = ['Zoom Mode','None']
     ximgtool_mkplotimg
     ximgtool_plotupdate
     ximgtool_trackwin
     
  endelse
  
end
;
;==============================================================================
;
pro ximgtool_bufferbuttonevent,event

  common ximgtool_state
  
  widget_control, event.id, GET_VALUE=uvalue

  case uvalue of 

     'Buffer 1': state.buffer = 0
     'Buffer 2': state.buffer = 1
     'Buffer 3': state.buffer = 2
     'Buffer 4': state.buffer = 3
     'Buffer 5': state.buffer = 4

  endcase
  
  ximgtool_chbuffer
  ximgtool_updatebuttons
  

end
;
;=============================================================================
;
pro ximgtool_cleanupevent,event

  common ximgtool_state


  for i = 0,4 do begin

     ptr_free, buffers.(i).img
     ptr_free, buffers.(i).bytimg
     ptr_free, buffers.(i).panimg
     ptr_free, buffers.(i).hdr
     ptr_free, buffers.(i).phdr
     ptr_free, buffers.(i).wcsinfo
     ptr_free, buffers.(i).pwcsinfo
     ptr_free, buffers.(i).bitmask
     
  endfor

  for i = 0,n_elements(*buffers.(state.buffer).oplot)-1 do begin

     
     if size((*buffers.(state.buffer).oplot)[i],/TYPE) eq 11 then $
        obj_destroy,(*buffers.(state.buffer).oplot)[i]

  endfor

  ptr_free, state.divisors

  state = 0B

end
;
;==============================================================================
;
pro ximgtool_colorevent,event

  common ximgtool_state

  widget_control, event.id, GET_VALUE=uvalue

  if uvalue eq 'Invert' then begin

     buffers.(state.buffer).invert = ~buffers.(state.buffer).invert
     widget_control, state.but_invert, SET_BUTTON=buffers.(state.buffer).invert

  endif else buffers.(state.buffer).imgcmap = uvalue

  ximgtool_chcmap
  ximgtool_plotupdate
  ximgtool_trackwin
  ximgtool_updatebuttons
  
end
;
;==============================================================================
;
pro ximgtool_cursorevent,event

  common ximgtool_state
  
  widget_control, event.id, GET_VALUE=uvalue

  state.cursormode = [uvalue+' Mode','None']
  ximgtool_plotupdate
  ximgtool_trackwin               

  for i = 0,3 do widget_control, state.but_cursor[i],SET_BUTTON=0

  case uvalue of

     'Zoom': widget_control, state.but_cursor[0],/SET_BUTTON

     'Stretch': widget_control, state.but_cursor[1],/SET_BUTTON

     'Range': widget_control, state.but_cursor[2],/SET_BUTTON

     'Image Exam': widget_control, state.but_cursor[3],/SET_BUTTON

  endcase

end
;
;=============================================================================
;
pro ximgtool_distanceevent,event


  common ximgtool_state

  if event.ch ne 0 then begin
   
     if string(event.ch) eq 'c' then begin
        
        state.region = !values.f_nan
        ximgtool_plotupdate         
        
     endif
     
     goto, cont

  endif

  if event.press eq 1 or event.press eq 2 or event.press eq 4 then begin

     case event.press of 

        1: state.cursormode[1] = 'Diagonal' 
        
        2: state.cursormode[1] = 'Column' 
        
        4: state.cursormode[1] = 'Row' 
        
        else:
        
     endcase

;  Check for a resize request

     if total((state.cursorpos- $
               reform(state.region[*,1]))^2) lt 15 then $
        begin
        state.pstart = reform(state.region[*,0])
        
     endif else begin
     
;  If not, then it must be a draw request

     state.pstart = state.cursorpos
     
     endelse

     state.region = !values.f_nan
     ximgtool_plotupdate

  endif

  if state.cursormode[1] ne 'None' then begin
 
     ximgtool_plotupdate

     case state.cursormode[1] of 

        'Diagonal': begin

           xend = state.cursorpos[0]
           yend = state.cursorpos[1]

        end

        'Column': begin

           xend = state.pstart[0]
           yend = state.cursorpos[1]

        end

        'Row': begin

           xend = state.cursorpos[0]
           yend = state.pstart[1]

        end

     endcase

     plots,[state.pstart[0],xend],[state.pstart[1],yend], $
           COLOR=state.distregcolor,THICK=2
     plots,state.pstart[0],state.pstart[1],PSYM=6,COLOR=state.distregcolor
     plotsym,0,1,/FILL
     plots,xend,yend,PSYM=8,COLOR=state.distregcolor
     
     dy = yend-state.pstart[1]
     dx = xend-state.pstart[0]
     d = sqrt(dx^2+dy^2)
     
     string = strtrim(fix(dx),2)+', '+strtrim(fix(dy),2)+', '+strtrim(d,2)
     del = (dy gt 0) ? 10:-20
     xy = convert_coord(xend,yend,/DATA,/TO_DEVICE)

     xyouts,xy[0],xy[1]+del,string,/DEVICE,COLOR=5,FONT=0,CHARSIZE=12, $
            ALIGNMENT=0.5
     
     
  endif

  if event.release eq 1 or event.release eq 2 or event.release eq 4 then begin

     case state.cursormode[1] of 

        'Diagonal': begin

           xend = state.cursorpos[0]
           yend = state.cursorpos[1]

        end

        'Column': begin

           xend = state.pstart[0]
           yend = state.cursorpos[1]

        end

        'Row': begin

           xend = state.cursorpos[0]
           yend = state.pstart[1]

        end

     endcase

     state.region = [[state.pstart],[xend,yend]]
     state.cursormode[1] = 'None'
     
  endif 
  

  cont:

end
;
;==============================================================================
;
pro ximgtool_menuevent,event

  common ximgtool_state

  widget_control, event.id, GET_UVALUE=uvalue

  case uvalue of 

     'Blink': begin

        IF TAG_NAMES(event, /STRUCTURE_NAME) eq 'WIDGET_TIMER' then begin

           if not state.movie then return
           ximgtool_blink,/FORWARD
           widget_control,state.but_blink,TIMER=state.delay
           
        endif else begin

           delay = mc_cfld(state.delay_fld,4,/EMPTY,CANCEL=cancel)
           if cancel then begin

              widget_control, state.delay_fld[1], $
                              SET_VALUE=strtrim(state.delay,2)
              widget_control, state.but_blink, SET_BUTTON=0
              return
           
           endif
           delay = mc_crange(delay,0.1,'Delay time',/KGE, $
                          WIDGET_ID=state.ximgtool_base,CANCEL=cancel)
           if cancel then begin
              
              widget_control, state.delay_fld[1], $
                              SET_VALUE=strtrim(state.delay,2)
              widget_control, state.but_blink, SET_BUTTON=0
              
           endif else begin
              
              state.movie = event.select
              state.delay = delay
              widget_control,state.delay_fld[0],SENSITIVE=abs(event.select-1)
              if state.movie then widget_control,state.but_blink, $
                                                   TIMER=delay
           endelse

        endelse

     end

     'Blink Buffer 1': state.blinkbuffer[0] = event.select

     'Blink Buffer 2': state.blinkbuffer[1] = event.select

     'Blink Buffer 3': state.blinkbuffer[2] = event.select

     'Blink Buffer 4': state.blinkbuffer[3] = event.select

     'Blink Buffer 5': state.blinkbuffer[4] = event.select

     'Buffer Blink': begin

        state.bufferblinkpanel = 1

        widget_control, state.ximgtool_base,UPDATE=0
        widget_control, state.plotwin,/DESTROY

        state.panel = widget_base(state.ximgtool_base,$
                                  EVENT_PRO='ximgtool_menuevent',$
                                  /BASE_ALIGN_CENTER,$
                                  FRAME=2,$
                                  /ROW)

           row = widget_base(state.panel,$
                             /NONEXCLUSIVE,$
                             /TOOLBAR,$
                             /ROW)

              state.but_blinkbuf[0] = widget_button(row,$
                                                    FONT=state.buttonfont,$
                                                    VALUE='Buffer 1',$
                                                    UVALUE='Blink Buffer 1')
              widget_control, state.but_blinkbuf[0],/SET_BUTTON, $
                              SENSITIVE=state.loadedbuffer[0]
              
              state.but_blinkbuf[1] = widget_button(row,$
                                                    FONT=state.buttonfont,$
                                                    VALUE='Buffer 2',$
                                                    UVALUE='Blink Buffer 2')
              widget_control, state.but_blinkbuf[1],/SET_BUTTON,$
                              SENSITIVE=state.loadedbuffer[1]
              
              state.but_blinkbuf[2] = widget_button(row,$
                                                    FONT=state.buttonfont,$
                                                    VALUE='Buffer 3',$
                                                    UVALUE='Blink Buffer 3')
              widget_control, state.but_blinkbuf[2],/SET_BUTTON,$
                              SENSITIVE=state.loadedbuffer[2]
              
              state.but_blinkbuf[3] = widget_button(row,$
                                                    FONT=state.buttonfont,$
                                                    VALUE='Buffer 4',$
                                                    UVALUE='Blink Buffer 4')
              widget_control, state.but_blinkbuf[3],/SET_BUTTON,$
                              SENSITIVE=state.loadedbuffer[3]
           
              state.but_blinkbuf[4] = widget_button(row,$
                                                    FONT=state.buttonfont,$
                                                    VALUE='Buffer 5',$
                                                    UVALUE='Blink Buffer 5')
              widget_control, state.but_blinkbuf[4],/SET_BUTTON,$
                              SENSITIVE=state.loadedbuffer[4]
              
           row = widget_base(state.panel,$
                             /NONEXCLUSIVE,$
                             /ROW,$
                             /TOOLBAR)
           
              state.but_blink = widget_button(row,$
                                              FONT=state.buttonfont,$
                                              TOOLTIP='Blick Buffers',$
                                              VALUE='Blink',$
                                              UVALUE='Blink')
              
           fld = coyote_field2(state.panel,$
                               LABELFONT=state.buttonfont,$
                               FIELDFONT=state.textfont,$
                               TITLE='Delay: ',$
                               UVALUE='Blink Delay',$
                               XSIZE=3,$
                               VALUE=state.delay,$
                               EVENT_PRO='ximgtool_submenuevent',$
                               /CR_ONLY,$
                               TEXTID=textid)
           state.delay_fld = [fld,textid]
              
              
              
           button = widget_button(state.panel,$
                                  FONT=state.buttonfont,$
                                  VALUE='Done',$
                                  UVALUE='Done Buffer Blink')

           state.plotwin = widget_draw(state.ximgtool_base,$
                                       XSIZE=state.plotwinsize[0],$
                                       YSIZE=state.plotwinsize[1],$
                                       EVENT_PRO='ximgtool_plotwinevent',$
                                       /TRACKING_EVENTS,$
                                       /KEYBOARD_EVENTS,$
                                       /MOTION_EVENTS,$
                                       /BUTTON_EVENTS)
           
        widget_geom = widget_info(state.ximgtool_base, /GEOMETRY)
        
        state.winbuffer[0]=widget_geom.xsize-state.plotwinsize[0]
        state.winbuffer[1]=widget_geom.ysize-state.plotwinsize[1]
        
        widget_control, state.ximgtool_base,UPDATE=1
        ximgtool_plotupdate        

     end

     'Buffer Math': begin

        if state.buffermathpanel then return
        state.buffermathpanel = 1

        widget_control, state.ximgtool_base,UPDATE=0
        widget_control, state.plotwin,/DESTROY

        state.panel = widget_base(state.ximgtool_base,$
                                  /BASE_ALIGN_CENTER,$
                                  EVENT_PRO='ximgtool_menuevent',$
                                  FRAME=2,$
                                  /ROW)
        
           bufs = string(findgen(n_elements(state.loadedbuffer))+1, $
                         FORMAT='(i2.2)')
           
           dl = widget_droplist(state.panel,$
                                FONT=state.buttonfont,$
                                VALUE=bufs,$
                                UVALUE='Math Buffer 1')
           widget_control,dl,SET_DROPLIST_SELECT=state.mbuf1
           
           dl = widget_droplist(state.panel,$
                                FONT=state.buttonfont,$
                                VALUE=['+','-','*','/'],$
                                UVALUE='Math Operation')
           widget_control,dl,SET_DROPLIST_SELECT=state.mop
           
           dl = widget_droplist(state.panel,$
                                FONT=state.buttonfont,$
                                VALUE=bufs,$
                                UVALUE='Math Buffer 2')
           widget_control,dl,SET_DROPLIST_SELECT=state.mbuf2
           
           label = widget_label(state.panel,$
                                FONT=state.buttonfont,$
                                VALUE='=')
           
           dl = widget_droplist(state.panel,$
                                FONT=state.buttonfont,$
                                VALUE=bufs,$
                                UVALUE='Math Buffer 3')
           widget_control,dl,SET_DROPLIST_SELECT=state.mbuf3
           
           button = widget_button(state.panel,$
                                  FONT=state.buttonfont,$
                                  VALUE='Compute',$
                                  EVENT_PRO='ximgtool_menuevent',$
                                  UVALUE='Compute Math')
           
           button = widget_button(state.panel,$
                                  FONT=state.buttonfont,$
                                  EVENT_PRO='ximgtool_menuevent',$
                                  VALUE=' Done ',$
                                  UVALUE='Done Buffer Math')

        state.plotwin = widget_draw(state.ximgtool_base,$
                                    XSIZE=state.plotwinsize[0],$
                                    YSIZE=state.plotwinsize[1],$
                                    EVENT_PRO='ximgtool_plotwinevent',$
                                    /TRACKING_EVENTS,$
                                    /KEYBOARD_EVENTS,$
                                    /MOTION_EVENTS,$
                                    /BUTTON_EVENTS)

        widget_geom = widget_info(state.ximgtool_base, /GEOMETRY)
        
        state.winbuffer[0]=widget_geom.xsize-state.plotwinsize[0]
        state.winbuffer[1]=widget_geom.ysize-state.plotwinsize[1]

        widget_control, state.ximgtool_base,UPDATE=1
        ximgtool_plotupdate        

     end

     'Center Image': begin

        buffers.(state.buffer).cenpix = [buffers.(state.buffer).ncols,$
                                         buffers.(state.buffer).nrows]/2. $
                                        -0.5        
        
        if state.lock then ximgtool_lockimages,/CENTER else begin
           
           state.cursormode = ['Zoom Mode','None']
           ximgtool_mkplotimg
           ximgtool_plotupdate
           ximgtool_trackwin
           
        endelse

     end

     'Clear Frame': begin

        ximgtool_clearframes,state.buffer,CANCEL=cancel

     end

     'Compute Math': ximgtool_imgmath

     'Decimal Degrees': begin

        state.decdeg = ~state.decdeg
        ximgtool_plotupdate
        ximgtool_trackwin
        widget_control, state.but_decdeg,SET_BUTTON=state.decdeg

     end

     'Divisors': begin

        state.divisorspanel = 1
        
        widget_control, state.ximgtool_base,UPDATE=0
        widget_control, state.plotwin,/DESTROY
        
        state.panel = widget_base(state.ximgtool_base,$
                                  /BASE_ALIGN_CENTER,$
                                  EVENT_PRO='ximgtool_menuevent',$
                                  FRAME=2,$
                                  /ROW)
        
           fld = coyote_field2(state.panel,$
                               LABELFONT=state.buttonfont,$
                               FIELDFONT=state.textfont,$
                               TITLE='Divisors:',$
                               UVALUE='Divisors Field',$
                               VALUE=strjoin(*state.divisors,','),$
                               XSIZE=40,$
                               EVENT_PRO='ximgtool_menuevent',$
                               /CR_ONLY,$
                               TEXTID=textid)
           state.divisors_fld = [fld,textid]

            state.divisorson_bg = cw_bgroup(state.panel,$
                                            FONT=state.buttonfont,$
                                            ['On'],$
                                            /ROW,$
                                            /NONEXCLUSIVE,$
                                            SET_VALUE=state.divisorson,$
                                            UVALUE='Divisors On')
           
           button = widget_button(state.panel,$
                                  FONT=state.buttonfont,$
                                  EVENT_PRO='ximgtool_menuevent',$
                                  VALUE=' Done ',$
                                  UVALUE='Done Divisors')

        state.plotwin = widget_draw(state.ximgtool_base,$
                                    XSIZE=state.plotwinsize[0],$
                                    YSIZE=state.plotwinsize[1],$
                                    EVENT_PRO='ximgtool_plotwinevent',$
                                    /TRACKING_EVENTS,$
                                    /KEYBOARD_EVENTS,$
                                    /MOTION_EVENTS,$
                                    /BUTTON_EVENTS)

        widget_geom = widget_info(state.ximgtool_base, /GEOMETRY)
        
        state.winbuffer[0]=widget_geom.xsize-state.plotwinsize[0]
        state.winbuffer[1]=widget_geom.ysize-state.plotwinsize[1]

        widget_control, state.ximgtool_base,UPDATE=1
        ximgtool_plotupdate

     end

     'Divisors On': state.divisorson = event.select

     'Divisors Field': begin

        if state.divisorson then begin
           
           divisors = mc_cfld(state.divisors_fld,7,/EMPTY,CANCEL=cancel)
           if cancel then return
           *state.divisors = strtrim(strsplit(divisors,',',/EXTRACT),2)
           
        endif else *state.divisors = ''

     end

     'Done Buffer Blink': begin

        state.bufferblinkpanel = 0

        widget_control, state.ximgtool_base,UPDATE=0
        widget_control, state.panel,/DESTROY
        widget_control, state.plotwin,/DESTROY

        state.plotwin = widget_draw(state.ximgtool_base,$
                                    XSIZE=state.plotwinsize[0],$
                                    YSIZE=state.plotwinsize[1],$
                                    EVENT_PRO='ximgtool_plotwinevent',$
                                    /TRACKING_EVENTS,$
                                    /KEYBOARD_EVENTS,$
                                    /MOTION_EVENTS,$
                                    /BUTTON_EVENTS)

        widget_geom = widget_info(state.ximgtool_base, /GEOMETRY)
        
        state.winbuffer[0]=widget_geom.xsize-state.plotwinsize[0]
        state.winbuffer[1]=widget_geom.ysize-state.plotwinsize[1]

        
        widget_control, state.ximgtool_base,UPDATE=1
        ximgtool_plotupdate

     end

     'Done Buffer Math': begin

        state.buffermathpanel = 0

        widget_control, state.ximgtool_base,UPDATE=0
        widget_control, state.panel,/DESTROY
        widget_control, state.plotwin,/DESTROY

        state.plotwin = widget_draw(state.ximgtool_base,$
                                    XSIZE=state.plotwinsize[0],$
                                    YSIZE=state.plotwinsize[1],$
                                    EVENT_PRO='ximgtool_plotwinevent',$
                                    /TRACKING_EVENTS,$
                                    /KEYBOARD_EVENTS,$
                                    /MOTION_EVENTS,$
                                    /BUTTON_EVENTS)

        widget_geom = widget_info(state.ximgtool_base, /GEOMETRY)
        
        state.winbuffer[0]=widget_geom.xsize-state.plotwinsize[0]
        state.winbuffer[1]=widget_geom.ysize-state.plotwinsize[1]
        
        widget_control, state.ximgtool_base,UPDATE=1
        ximgtool_plotupdate        

     end

     'Done Divisors': begin

        state.divisorspanel = 0

        widget_control, state.ximgtool_base,UPDATE=0
        widget_control, state.panel,/DESTROY
        widget_control, state.plotwin,/DESTROY

        state.plotwin = widget_draw(state.ximgtool_base,$
                                    XSIZE=state.plotwinsize[0],$
                                    YSIZE=state.plotwinsize[1],$
                                    EVENT_PRO='ximgtool_plotwinevent',$
                                    /TRACKING_EVENTS,$
                                    /KEYBOARD_EVENTS,$
                                    /MOTION_EVENTS,$
                                    /BUTTON_EVENTS)

        widget_geom = widget_info(state.ximgtool_base, /GEOMETRY)
        
        state.winbuffer[0]=widget_geom.xsize-state.plotwinsize[0]
        state.winbuffer[1]=widget_geom.ysize-state.plotwinsize[1]
        
        widget_control, state.ximgtool_base,UPDATE=1
        ximgtool_plotupdate        

     end
     
;     'Done Find Position': begin
;
;        state.rangepanel = 0
;
;        widget_control, state.ximgtool_base,UPDATE=0
;        widget_control, state.panel,/DESTROY
;        widget_control, state.plotwin,/DESTROY
;
;        state.plotwin = widget_draw(state.ximgtool_base,$
;                                    XSIZE=state.plotwinsize[0],$
;                                    YSIZE=state.plotwinsize[1],$
;                                    EVENT_PRO='ximgtool_plotwinevent',$
;                                    /TRACKING_EVENTS,$
;                                    /KEYBOARD_EVENTS,$
;                                    /MOTION_EVENTS,$
;                                    /BUTTON_EVENTS)
;        
;        widget_geom = widget_info(state.ximgtool_base, /GEOMETRY)
;        
;        state.winbuffer[0]=widget_geom.xsize-state.plotwinsize[0]
;        state.winbuffer[1]=widget_geom.ysize-state.plotwinsize[1]
;
;        
;        widget_control, state.ximgtool_base,UPDATE=1
;        ximgtool_plotupdate        
;
;     end

     'Done Range User Defined': begin

        state.rangepanel = 0

        widget_control, state.ximgtool_base,UPDATE=0
        widget_control, state.panel,/DESTROY
        widget_control, state.plotwin,/DESTROY

        state.plotwin = widget_draw(state.ximgtool_base,$
                                    XSIZE=state.plotwinsize[0],$
                                    YSIZE=state.plotwinsize[1],$
                                    EVENT_PRO='ximgtool_plotwinevent',$
                                    /TRACKING_EVENTS,$
                                    /KEYBOARD_EVENTS,$
                                    /MOTION_EVENTS,$
                                    /BUTTON_EVENTS)
        
        widget_geom = widget_info(state.ximgtool_base, /GEOMETRY)
        
        state.winbuffer[0]=widget_geom.xsize-state.plotwinsize[0]
        state.winbuffer[1]=widget_geom.ysize-state.plotwinsize[1]

        
        widget_control, state.ximgtool_base,UPDATE=1
        ximgtool_plotupdate        

     end

     'Find Position': begin

        widget_control, state.ximgtool_base,UPDATE=0
        widget_control, state.plotwin,/DESTROY

        state.panel = widget_base(state.ximgtool_base,$
                                  /BASE_ALIGN_CENTER,$
                                  EVENT_PRO='ximgtool_menuevent',$
                                  FRAME=2,$
                                  /ROW)
        
           fld = coyote_field2(state.panel,$
                               LABELFONT=state.buttonfont,$
                               FIELDFONT=state.textfont,$
                               TITLE='RA:',$
                               UVALUE='RA',$
                               XSIZE=20,$
                               EVENT_PRO='ximgtool_menuevent',$
                               /CR_ONLY,$
                               TEXTID=textid)
           state.ra_fld = [fld,textid]

           fld = coyote_field2(state.panel,$
                               LABELFONT=state.buttonfont,$
                               FIELDFONT=state.textfont,$
                               TITLE='Dec:',$
                               UVALUE='Dec',$
                               XSIZE=20,$
                               EVENT_PRO='ximgtool_menuevent',$
                               /CR_ONLY,$
                               TEXTID=textid)
           state.dec_fld = [fld,textid]

           button = widget_button(state.panel,$
                                  FONT=state.buttonfont,$
                                  EVENT_PRO='ximgtool_menuevent',$
                                  VALUE=' Find ',$
                                  UVALUE='Plot Position')

           button = widget_button(state.panel,$
                                  FONT=state.buttonfont,$
                                  EVENT_PRO='ximgtool_menuevent',$
                                  VALUE=' Done ',$
                                  UVALUE='Done Find Position')

        

        state.plotwin = widget_draw(state.ximgtool_base,$
                                    XSIZE=state.plotwinsize[0],$
                                    YSIZE=state.plotwinsize[1],$
                                    EVENT_PRO='ximgtool_plotwinevent',$
                                    /TRACKING_EVENTS,$
                                    /KEYBOARD_EVENTS,$
                                    /MOTION_EVENTS,$
                                    /BUTTON_EVENTS)

        widget_geom = widget_info(state.ximgtool_base, /GEOMETRY)
        
        state.winbuffer[0]=widget_geom.xsize-state.plotwinsize[0]
        state.winbuffer[1]=widget_geom.ysize-state.plotwinsize[1]

        widget_control, state.ximgtool_base,UPDATE=1
        ximgtool_plotupdate        

     end

     'Ximgtool Help': begin
     
        xmc_displaytext,filepath('ximgtool_helpfile.txt', $
                                 ROOT_DIR=state.packagepath, $
                                 SUBDIR='helpfiles'),WSIZE=[600,400]

     end

     'Img Min/Max': begin

        min = mc_cfld(state.imgmin_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then return
        max = mc_cfld(state.imgmax_fld,4,/EMPTY,CANCEL=cancel)
        if cancel then return
        
        junk = mc_crange(min,max,'Min',/KLT,$
                      WIDGET_ID=state.ximgtool_base,CANCEL=cancel)
        
        if cancel then begin
           
           widget_control, state.imgmin_fld[0], $
                           SET_VALUE=buffers.(state.buffer).min
           widget_control, state.imgmax_fld[0], $
                           SET_VALUE=buffers.(state.buffer).max
           return
           
        endif else begin
           
           buffers.(state.buffer).min = min
           buffers.(state.buffer).max = max
           buffers.(state.buffer).imgrange = 'User'
           ximgtool_updatebuttons
           
        endelse
        
        state.region = !values.f_nan
        ximgtool_bytsclimg
        ximgtool_mkplotimg,/PANIMG
        ximgtool_plotupdate
        ximgtool_trackwin
        
     end

     'Load FITS': begin

        fullpath=dialog_pickfile(DIALOG_PARENT=state.ximgtool_base,$
                                 FILTER=['*.fits;*fits.gz;*FTS'],$
                                 GET_PATH=newpath,$
                                 /MUST_EXIST,PATH=buffers.(state.buffer).path,$
                                 /MULTIPLE_FILES)
        if fullpath[0] ne '' then begin
           
           buffers.(state.buffer).path = newpath
           
           if n_elements(fullpath) gt 1 then begin
              
              for i =0,n_elements(fullpath)-1 do begin

                 ximgtool_loadimage,fullpath[i],BUFFER=i+1,/NODISPLAY
                 
              endfor

              state.buffer = 0
              ximgtool_chbuffer
              ximgtool_trackwin

           endif else ximgtool_loadimage,fullpath[0],BUFFER=state.buffer+1
           
        endif

     end

     'Lock Images': begin

        state.lock = ~state.lock
        if state.lock then begin
           
           state.cursormode = ['Zoom Mode','None']
           ximgtool_lockimages

        end
        ximgtool_updatebuttons
          
     end

     'Magnifier': begin

        state.magnifier = ~state.magnifier
        ximgtool_plotupdate
        ximgtool_trackwin
        widget_control, state.but_magnifier,SET_BUTTON=state.magnifier

     end

     'Mask': begin

        buffers.(state.buffer).plotbitmask = ~buffers.(state.buffer).plotbitmask

        cbuffer = state.buffer
        for i = 0,4 do begin
           
           if state.loadedbuffer[i] eq 0 then continue
           state.buffer = i
           ximgtool_bytsclimg
           ximgtool_mkplotimg,/PANIMG
           
        endfor
        state.buffer = cbuffer
        ximgtool_plotupdate
        ximgtool_trackwin
        widget_control, state.but_mask, $
                        SET_BUTTON=buffers.(state.buffer).plotbitmask 


     end

     'Math Buffer 1': state.mbuf1 = event.index
     
     'Math Buffer 2': state.mbuf2 = event.index
     
     'Math Buffer 3': state.mbuf3 = event.index
     
     'Math Operation': state.mop = event.index

     'New Frame': begin

        z = where(state.loadedbuffer ne 1,cnt)
        
        if cnt eq 0 then begin
           
           ok = dialog_message('No Free Buffers.',/INFO, $
                               DIALOG_PARENT=state.ximgtool_base)

        endif else begin

           state.buffer = z[0]

           fullpath=dialog_pickfile(DIALOG_PARENT=state.ximgtool_base,$
                                    FILTER=['*.fits;*fits.gz;*FTS'],$
                                    GET_PATH=newpath,$
                                    /MUST_EXIST, $
                                    PATH=buffers.(state.buffer).path)
           if fullpath[0] ne '' then begin
              
              buffers.(state.buffer).path = newpath
              ximgtool_loadimage,fullpath[0],BUFFER=state.buffer+1
              
           endif else ximgtool_loadimage,BUFFER=state.buffer+1
           
        endelse

     end

     'Over Plot': begin

        state.oplot = ~state.oplot
        ximgtool_plotupdate
        ximgtool_trackwin
        widget_control, state.but_overplot,SET_BUTTON=state.oplot

     end

     'Panner': begin

        state.panner = ~state.panner
        ximgtool_plotupdate
        ximgtool_trackwin
        widget_control, state.but_panner,SET_BUTTON=state.panner

     end

     'Plot Position': ximgtool_findposition
     
     'Quit': begin

        widget_control, event.top, /DESTROY
        return
        
     end

     'Range User Defined': begin

     end

     'Sexagesimal': begin

        state.sexagesimal = ~state.sexagesimal
        ximgtool_plotupdate
        ximgtool_trackwin
        widget_control, state.but_sexagesimal,SET_BUTTON=state.sexagesimal

     end

     'View Header': begin
        
        if n_elements(*buffers.(state.buffer).hdr) gt 1 then begin
           
           xmc_displaytext,*buffers.(state.buffer).hdr,$
                           TITLE='FITS Header for Buffer '+ $
                           string(state.buffer+1,FORMAT='(i1.1)'),$
                           GROUP_LEADER=state.ximgtool_base
           
        endif else ok = dialog_message('No FITS header.',/INFO, $
                                       DIALOG_PARENT=state.ximgtool_base)
        
     end

     'Write TIFF': begin

;  Taken from A. Barth's atv 
        
        filename=dialog_pickfile(DIALOG_PARENT=state.ximgtool_base,$
                                 FILTER='*.tiff',/WRITE,$
                                 FILE='img.tiff')
        
        if filename ne '' then  begin
           
           tmp_img = tvrd(/TRUE)
           tmp_img = reverse(tmp_img, 3)
           write_tiff, filename, tmp_img, 1, /planarconfig
           
        endif              
        
     end

     'Write FITS': begin

        
        filename=dialog_pickfile(DIALOG_PARENT=state.ximgtool_base,$
                                 FILTER='*.fits',/WRITE,$
                                 FILE='img.fits')
        
        if filename ne '' then begin
           
           writefits,filename,*buffers.(state.buffer).rotimg
           
        endif
        
     end

     'Zoom In': begin

        buffers.(state.buffer).zoom = buffers.(state.buffer).zoom + 1
        if state.lock then ximgtool_lockimages,/CENTER else begin
           
           state.cursormode = ['Zoom Mode','None']
           ximgtool_mkplotimg
           ximgtool_plotupdate
           ximgtool_trackwin
           
        endelse

     end

     'Zoom Out': begin

        buffers.(state.buffer).zoom = buffers.(state.buffer).zoom - 1
        if state.lock then ximgtool_lockimages,/CENTER else begin
           
           state.cursormode = ['Zoom Mode','None']
           ximgtool_mkplotimg
           ximgtool_plotupdate
           ximgtool_trackwin
           
        endelse

     end

     'Zoom to Fit': ximgtool_zoomtofit

  endcase

end
;
;==============================================================================
;
pro ximgtool_momentsevent,event

  common ximgtool_state

  if event.ch ne 0 then begin
     
;     if event.press eq 1 then goto, cont

     if string(event.ch) eq 'c' then begin

        state.region = !values.f_nan
        ximgtool_imgrange
        ximgtool_bytsclimg
        ximgtool_mkplotimg
        ximgtool_plotupdate         
        
     endif

     goto, cont

  endif

;  Do the box

  box = state.region
  
  if event.press eq 1 then begin
           
;  Check for a Move request

     if state.cursorpix[0] gt box[0,0] and $
        state.cursorpix[0] lt box[0,1] and $
        state.cursorpix[1] gt box[1,1] and $
        state.cursorpix[1] lt box[1,0] then begin
        
        state.cursormode[1] = 'Move'
        state.pstart = state.cursorpix
        goto, next1

     endif 
     
;  Check for a resize request

     if total((state.cursorpix-reform(box[*,1]))^2) lt 15 then begin
        
        state.cursormode[1] = 'Draw' 
        state.pstart = reform(box[*,0])
        goto, next1
        
     endif
     
;  If neither, then it must be a draw request

     box[*] = !values.f_nan
     state.cursormode[1] = 'Draw' 
     state.pstart = state.cursorpix
     
  endif

  next1:
  
  if event.release eq 1 then begin
           
     case state.cursormode[1] of
        
        'Draw': begin
           
;  First a quick catch for a mistaken click and release.

           if state.pstart[0] eq state.cursorpix[0] and $
              state.pstart[1] eq state.cursorpix[1] then begin
              
              state.cursormode[1] = 'None'
              box[*] = !values.f_nan
              goto, cont                        
              
           endif
           
           state.cursormode[1] = 'None'
        
        end

         'Move': state.cursormode[1] = 'None'

         else:

      endcase

     ximgtool_moments
;     return

   endif

 ;  Now do the drawing

   case state.cursormode[1] of 

      'Draw': begin

         x   = state.cursorpix[0] > (state.pstart[0]+1)
         y   = state.cursorpix[1] < (state.pstart[1]-1)
         box = [[state.pstart],[[x,y]]]

        wset, state.plotwin_wid
        device, COPY=[0,0,state.plotwinsize[0],$
                      state.plotwinsize[1],0,0, $
                      buffers.(state.buffer).pixmap_wid]

        plots,[box[0,0]-0.5,box[0,0]-0.5,box[0,1]+0.5,box[0,1]+0.5, $
               box[0,0]-0.5],$
              [box[1,0]+0.5,box[1,1]-0.5,box[1,1]-0.5,box[1,0]+0.5,$
               box[1,0]+0.5],COLOR=state.statboxcolor,THICK=2

           plotsym,0,1,/FILL
           plots,box[0,1]+0.5,box[1,1]-0.5,PSYM=8,COLOR=state.statboxcolor    
        
     end
     
     'Move': begin
        
        del = state.cursorpix-state.pstart
        box[0,*] = box[0,*]+del[0]
        box[1,*] = box[1,*]+del[1]
        
        wset, state.plotwin_wid
        device, COPY=[0,0,state.plotwinsize[0],$
                      state.plotwinsize[1],0,0, $
                      buffers.(state.buffer).pixmap_wid]
        
        plots,[box[0,0]-0.5,box[0,0]-0.5,box[0,1]+0.5,box[0,1]+0.5, $
               box[0,0]-0.5],$
              [box[1,0]+0.5,box[1,1]-0.5,box[1,1]-0.5,box[1,0]+0.5,$
               box[1,0]+0.5],COLOR=state.statboxcolor,THICK=2

           plotsym,0,1,/FILL
           plots,box[0,1]+0.5,box[1,1]-0.5,PSYM=8,COLOR=state.statboxcolor    

           state.pstart = state.cursorpix
           
        end
     
     else:
     
  endcase
  
   state.region = box

cont:

end
;
;=============================================================================
;
pro ximgtool_linecutevent,event


  common ximgtool_state

  if event.ch ne 0 then begin
   
     if string(event.ch) eq 'c' then begin
        
        state.region = !values.f_nan
        ximgtool_plotupdate         
        
     endif
     
     goto, cont

  endif

  if event.press eq 1 or event.press eq 2 or event.press eq 4 then begin

     case event.press of 

        1: state.cursormode[1] = 'Diagonal' 
        
        2: state.cursormode[1] = 'Column' 
        
        4: state.cursormode[1] = 'Row' 
        
        else:
        
     endcase

;  Check for a resize request

     if total((state.cursorpos-reform(state.region[*,1]))^2) lt 15 then $
        begin
        state.pstart = reform(state.region[*,0])
        
     endif else begin
     
;  If not, then it must be a draw request

     state.pstart = state.cursorpos
     
     endelse
     state.region = !values.f_nan
     ximgtool_plotupdate

  endif

  if state.cursormode[1] ne 'None' then begin
     
     ximgtool_plotupdate

     case state.cursormode[1] of 

        'Diagonal': begin

           xend = state.cursorpos[0]
           yend = state.cursorpos[1]

        end

        'Column': begin

           xend = state.pstart[0]
           yend = state.cursorpos[1]

        end

        'Row': begin

           xend = state.cursorpos[0]
           yend = state.pstart[1]

        end

     endcase

     plots,[state.pstart[0],xend],[state.pstart[1],yend], $
           COLOR=state.lineregcolor,THICK=2
     plots,state.pstart[0],state.pstart[1],PSYM=6,COLOR=state.lineregcolor
     plotsym,0,1,/FILL
     plots,xend,yend,PSYM=8,COLOR=state.lineregcolor

  endif

  if event.release eq 1 or event.release eq 2 or event.release eq 4 then begin

     case state.cursormode[1] of 

        'Diagonal': begin

           xend = state.cursorpos[0]
           yend = state.cursorpos[1]

        end

        'Column': begin

           xend = state.pstart[0]
           yend = state.cursorpos[1]

        end

        'Row': begin

           xend = state.cursorpos[0]
           yend = state.pstart[1]

        end

     endcase


     state.region = [[state.pstart],[xend,yend]]
     state.cursormode[1] = 'None'
     ximgtool_linecut
          
  endif 
  
  cont:

end
;
;==============================================================================
;
pro ximgtool_pannerevent,event

  common ximgtool_state

  if event.type eq 1 then begin
     
     if state.lock then ximgtool_lockimages else begin
        
        ximgtool_mkplotimg
        ximgtool_plotupdate
        
     endelse
     state.pannermod = 0
     
     return

  endif

  state.cursormode = ['Zoom Mode','None']

     tv,*buffers.(state.buffer).panimg, $
        state.pannerpos[0],state.pannerpos[1],/DEVICE
     
     plots,[state.pannerpos[0],state.pannerpos[0], $
            state.pannerpos[0]+state.trackwinsize,$
            state.pannerpos[0]+state.trackwinsize,state.pannerpos[0]],$
           [state.pannerpos[1],state.pannerpos[1]+state.trackwinsize,$
            state.pannerpos[1]+state.trackwinsize,state.pannerpos[1],$
            state.pannerpos[1]],/DEVICE,COLOR=3,THICK=2

    aratio = buffers.(state.buffer).ncols/float(buffers.(state.buffer).nrows)
  
  if aratio gt 1 then begin
     
     delx = event.x-(state.pannerpos[0]+state.trackwinsize/2.)
     dely = event.y-(state.pannerpos[1]+state.trackwinsize/2.)
     
     x = delx/float(state.trackwinsize)*buffers.(state.buffer).ncols+ $
         buffers.(state.buffer).ncols/2.

     y = dely/float(state.trackwinsize)*buffers.(state.buffer).nrows* $
         aratio+buffers.(state.buffer).nrows/2.

     if x lt 0 or x gt buffers.(state.buffer).ncols-1 or $
        y lt 0 or y gt buffers.(state.buffer).nrows-1 then return

     buffers.(state.buffer).cenpix[0] = x
     buffers.(state.buffer).cenpix[1] = y

     offset = (buffers.(state.buffer).ncols-buffers.(state.buffer).nrows)/2
     
     ll = [buffers.(state.buffer).cenpix[0]- $
           buffers.(state.buffer).pixdisp[0]/2,$
           buffers.(state.buffer).cenpix[1]- $
           buffers.(state.buffer).pixdisp[1]/2+offset]/ $
          buffers.(state.buffer).ncols*float(state.trackwinsize) > 0
     
     ur = [buffers.(state.buffer).cenpix[0]+ $
           buffers.(state.buffer).pixdisp[0]/2,$
           buffers.(state.buffer).cenpix[1]+ $
           buffers.(state.buffer).pixdisp[1]/2+offset]/ $
          buffers.(state.buffer).ncols*float(state.trackwinsize) < $
          (state.trackwinsize)

     
  endif else begin
     
     delx = event.x-(state.pannerpos[0]+state.trackwinsize/2.)
     dely = event.y-(state.pannerpos[1]+state.trackwinsize/2.)

     x = delx/float(state.trackwinsize)*buffers.(state.buffer).ncols/ $
         aratio+buffers.(state.buffer).ncols/2.

     y = dely/float(state.trackwinsize)*buffers.(state.buffer).nrows+ $
         buffers.(state.buffer).nrows/2.

     if x lt 0 or x gt buffers.(state.buffer).ncols-1 or $
        y lt 0 or y gt buffers.(state.buffer).nrows-1 then return

     buffers.(state.buffer).cenpix[0] = x
     buffers.(state.buffer).cenpix[1] = y

     offset = (buffers.(state.buffer).nrows-buffers.(state.buffer).ncols)/2
     
     ll = [buffers.(state.buffer).cenpix[0]- $
           buffers.(state.buffer).pixdisp[0]/2+offset,$
           buffers.(state.buffer).cenpix[1]- $
           buffers.(state.buffer).pixdisp[1]/2]/ $
          buffers.(state.buffer).nrows*float(state.trackwinsize) > 0
     
     ur = [buffers.(state.buffer).cenpix[0]+ $
           buffers.(state.buffer).pixdisp[0]/2+offset,$
           buffers.(state.buffer).cenpix[1]+ $
           buffers.(state.buffer).pixdisp[1]/2]/ $
          buffers.(state.buffer).nrows*float(state.trackwinsize) < $
          (state.trackwinsize)
         
  endelse     

  plots,state.pannerpos[0]+[ll[0],ll[0],ur[0],ur[0],ll[0]],$
        state.pannerpos[1]+[ll[1],ur[1],ur[1],ll[1],ll[1]], $
        COLOR=3,/DEVICE
  
end
;
;==============================================================================
;
pro ximgtool_plotwinevent,event

  common ximgtool_state

  if state.loadedbuffer[state.buffer] eq 0 then return

  wset, state.plotwin_wid
  !x = buffers.(state.buffer).xscale
  !y = buffers.(state.buffer).yscale
  !p = buffers.(state.buffer).pscale
  
;  Check for tracking and give focus to the plot window if necessary
  
  if strtrim(tag_names(event,/STRUCTURE_NAME),2) eq 'WIDGET_TRACKING' then begin
     
     widget_control, state.plotwin, INPUT_FOCUS=event.enter
     
     mc_mkct
     tvlct,buffers.(state.buffer).red,buffers.(state.buffer).green,$
           buffers.(state.buffer).blue,state.color_bottom
     ximgtool_stretchct,BRICON=buffers.(state.buffer).bricon
;     ximgtool_plotupdate
     ximgtool_trackwin
     goto, out
     
  endif

;  Check to see if it is a pan image click

  if state.panner and $
     event.x gt state.pannerpos[0] and $
     event.x lt state.pannerpos[0]+state.trackwinsize and $
     event.y gt state.pannerpos[1] and $
     event.x lt state.pannerpos[1]+state.trackwinsize then begin

     if event.type eq 0 then begin

        state.pannermod = 1
        goto, out

     endif

;     if event.type eq 0 or event.type eq 2 then goto, out
     if state.pannermod then ximgtool_pannerevent,event

     goto, cont


  endif

;  Find cursor position
  
  state.devpos = [event.x,event.y]

  pix  = (convert_coord(event.x,event.y,/DEVICE,/TO_DATA))[0:1]
  state.cursorpos = pix
  zpix = mc_roundgt(pix)
  state.cursorpix = zpix

; Check for keyboard input
  
  if event.ch ne 0 and event.release then begin

     case string(event.ch) of 

        'd': begin

           state.cursormode = ['Distance Mode','None']
           state.region = !values.f_nan
           polyfill,[state.plotwinsize[0]/2-50,$
                     state.plotwinsize[0]/2-50,$
                     state.plotwinsize[0]/2+50,$
                     state.plotwinsize[0]/2+50,$
                     state.plotwinsize[0]/2-50],$
                    [state.plotwinsize[1]/2-8,$
                     state.plotwinsize[1]/2+14,$
                     state.plotwinsize[1]/2+14,$
                     state.plotwinsize[1]/2-8,$
                     state.plotwinsize[1]/2-8],/DEVICE,COLOR=0
           
           xyouts,state.plotwinsize[0]/2,state.plotwinsize[1]/2,$
                  'Image Exam Mode',COLOR=5,/DEVICE,ALIGNMENT=0.5,FONT=0
           wait,0.25
           ximgtool_updatebuttons

        end

        'z': begin

           state.cursormode = ['Zoom Mode','None']
           state.region = !values.f_nan
           polyfill,[state.plotwinsize[0]/2-35,$
                     state.plotwinsize[0]/2-35,$
                     state.plotwinsize[0]/2+35,$
                     state.plotwinsize[0]/2+35,$
                     state.plotwinsize[0]/2-35],$
                    [state.plotwinsize[1]/2-8,$
                     state.plotwinsize[1]/2+14,$
                     state.plotwinsize[1]/2+14,$
                     state.plotwinsize[1]/2-8,$
                     state.plotwinsize[1]/2-8],/DEVICE,COLOR=0

           xyouts,state.plotwinsize[0]/2,state.plotwinsize[1]/2,$
                  'Zoom Mode',COLOR=5,/DEVICE,ALIGNMENT=0.5,FONT=0

           wait,0.25
           ximgtool_updatebuttons

        end

        'r': begin

           state.cursormode = ['Range Mode','None']
           state.region = !values.f_nan
           polyfill,[state.plotwinsize[0]/2-35,$
                     state.plotwinsize[0]/2-35,$
                     state.plotwinsize[0]/2+35,$
                     state.plotwinsize[0]/2+35,$
                     state.plotwinsize[0]/2-35],$
                    [state.plotwinsize[1]/2-8,$
                     state.plotwinsize[1]/2+14,$
                     state.plotwinsize[1]/2+14,$
                     state.plotwinsize[1]/2-8,$
                     state.plotwinsize[1]/2-8],/DEVICE,COLOR=0

           xyouts,state.plotwinsize[0]/2,state.plotwinsize[1]/2,$
                  'Range Mode',COLOR=5,/DEVICE,ALIGNMENT=0.5,FONT=0

           wait,0.25
           ximgtool_updatebuttons

        end

        'l': begin

           state.cursormode = ['Line Cut Mode','None']
           state.region = !values.f_nan
           polyfill,[state.plotwinsize[0]/2-50,$
                     state.plotwinsize[0]/2-50,$
                     state.plotwinsize[0]/2+50,$
                     state.plotwinsize[0]/2+50,$
                     state.plotwinsize[0]/2-50],$
                    [state.plotwinsize[1]/2-8,$
                     state.plotwinsize[1]/2+14,$
                     state.plotwinsize[1]/2+14,$
                     state.plotwinsize[1]/2-8,$
                     state.plotwinsize[1]/2-8],/DEVICE,COLOR=0
           
           xyouts,state.plotwinsize[0]/2,state.plotwinsize[1]/2,$
                  'Image Exam Mode',COLOR=5,/DEVICE,ALIGNMENT=0.5,FONT=0

           wait,0.25
           ximgtool_updatebuttons

        end

        'm': begin

           state.cursormode = ['Stats Mode','None']
           state.region = !values.f_nan
           polyfill,[state.plotwinsize[0]/2-50,$
                     state.plotwinsize[0]/2-50,$
                     state.plotwinsize[0]/2+50,$
                     state.plotwinsize[0]/2+50,$
                     state.plotwinsize[0]/2-50],$
                    [state.plotwinsize[1]/2-8,$
                     state.plotwinsize[1]/2+14,$
                     state.plotwinsize[1]/2+14,$
                     state.plotwinsize[1]/2-8,$
                     state.plotwinsize[1]/2-8],/DEVICE,COLOR=0
           
           xyouts,state.plotwinsize[0]/2,state.plotwinsize[1]/2,$
                  'Image Exam Mode',COLOR=5,/DEVICE,ALIGNMENT=0.5,FONT=0
           
           wait,0.25           
           ximgtool_updatebuttons
           
        end

        'a': begin

           xmc_phot,*buffers.(state.buffer).rotimg,$
                    state.cursorpix[0],state.cursorpix[1], $
                    GROUP_LEADER=state.ximgtool_base
          
        end

        's': begin

           state.cursormode = ['Stretch Mode','None']
           state.region = !values.f_nan
           polyfill,[state.plotwinsize[0]/2-70,$
                     state.plotwinsize[0]/2-70,$
                     state.plotwinsize[0]/2+70,$
                     state.plotwinsize[0]/2+70,$
                     state.plotwinsize[0]/2-70],$
                    [state.plotwinsize[1]/2-8,$
                     state.plotwinsize[1]/2+14,$
                     state.plotwinsize[1]/2+14,$
                     state.plotwinsize[1]/2-8,$
                     state.plotwinsize[1]/2-8],/DEVICE,COLOR=0
           
           xyouts,state.plotwinsize[0]/2,state.plotwinsize[1]/2,$
                  'Color Map Stretch Mode',COLOR=5,/DEVICE,ALIGNMENT=0.5,FONT=0

           wait,0.25
           ximgtool_updatebuttons

        end

        'b': begin

           ximgtool_blink,/BACK
           goto, cont

        end
           
        'f': begin

           ximgtool_blink,/FORWARD
           goto, cont

        end

        'q': begin

           widget_control,event.top,/DESTROY
           return
           
        end
        

        'p': begin

;           printf, 2, strtrim(state.cursorpos[0],2),'  ', $
;                   strtrim(state.cursorpos[1],2)

;           print, strtrim(state.cursorpos[0],2),'  ', $
;                   strtrim(state.cursorpos[1],2)

           boxsize = 9
           gcntrd,*buffers.(state.buffer).rotimg,state.cursorpos[0],$
                  state.cursorpos[1],xcm,ycm,3

           print
           print, 'Cursor Position (X,Y)   : ',strtrim(state.cursorpos[0],2),$
                  '  ',strtrim(state.cursorpos[1],2)

           if n_tags(*buffers.(state.buffer).pwcsinfo) gt 0 then begin 

              xy2ad,state.cursorpos[0],state.cursorpos[1], $
                    *buffers.(state.buffer).pwcsinfo, ra,dec
              print, 'Cursor Position (RA,Dec): ', $
                     string(ra,dec,FORMAT='(D11.7,"  ",D11.7," ")')

           endif
           print

           print, 'Centroid (X,Y)   : ',strtrim(xcm,2),'  ',strtrim(ycm,2)

           if n_tags(*buffers.(state.buffer).pwcsinfo) gt 0 then begin 

              xy2ad,xcm,ycm,*buffers.(state.buffer).pwcsinfo, ra,dec
              print, 'Centroid (RA,Dec): ', $
                     string(ra,dec,FORMAT='(D11.7,"  ",D11.7," ")')

           endif
           print

        end

        't': ximgtool_zoomtofit
        
        else:

     endcase
     ximgtool_plotupdate
     goto, cont
  
  endif

;  Launch to specific cursor mode event handler

  case state.cursormode[0] of 

     'Zoom Mode': ximgtool_zoomevent,event

     'Stretch Mode': ximgtool_stretchevent,event

     'Line Cut Mode': ximgtool_linecutevent,event

     'Distance Mode': ximgtool_distanceevent,event

     'Range Mode': ximgtool_rangeboxevent,event

     'Stats Mode': ximgtool_momentsevent,event
  
     else:

  endcase

  cont:
  
  ximgtool_trackwin  
  ximgtool_updateinfobar
    
  out:
  
  buffers.(state.buffer).xscale = !x
  buffers.(state.buffer).yscale = !y
  buffers.(state.buffer).pscale = !p



end
;
;==============================================================================
;
pro ximgtool_rangeboxevent,event

  common ximgtool_state

  if event.ch ne 0 then begin
     
;     if event.press eq 1 then goto, cont
     
     if string(event.ch) eq '=' then event.ch = 43B
     case string(event.ch) of 
        
        '-': begin
           
           val = (*buffers.(state.buffer).rotimg)[state.cursorpix[0],$
                                                   state.cursorpix[1]]
           if val ge buffers.(state.buffer).max then begin
              
              ok = dialog_message('Value greater than current maximum.',/ERROR,$
                                  DIALOG_PARENT=state.ximgtool_base)   
              goto, cont
           endif else begin
              
              widget_control,state.but_range[7],/SET_BUTTON
              buffers.(state.buffer).min = val
              buffers.(state.buffer).imgrange = 'User'
              state.region = !values.f_nan
              ximgtool_setminmax
              ximgtool_bytsclimg
              ximgtool_mkplotimg,/PANIMG
              ximgtool_plotupdate
              ximgtool_updatebuttons
              
           endelse
           
        end
        
        '+': begin
           
           val = (*buffers.(state.buffer).rotimg)[state.cursorpix[0],$
                                                   state.cursorpix[1]]
           if val le buffers.(state.buffer).min then begin
              
              ok = dialog_message('Value less than current minimum.',/ERROR,$
                                  DIALOG_PARENT=state.ximgtool_base)
              goto,cont
              
           endif else begin

              widget_control,state.but_range[7],/SET_BUTTON
              buffers.(state.buffer).max = val
              buffers.(state.buffer).imgrange = 'User'
              state.region = !values.f_nan
              ximgtool_setminmax
              ximgtool_bytsclimg
              ximgtool_mkplotimg,/PANIMG
              ximgtool_plotupdate         
              ximgtool_updatebuttons
              
           endelse

        end

        'c': begin

           state.region = !values.f_nan
           ximgtool_imgrange
           ximgtool_bytsclimg
           ximgtool_mkplotimg,/PANIMG
           ximgtool_plotupdate      
           ximgtool_updatebuttons   

        end

        else: goto, cont

     endcase
     goto, cont

  endif
  
;  No do the box

  box = state.region

  if event.press eq 1 then begin
           
;  Check for a Move request

     if state.cursorpix[0] gt box[0,0] and $
        state.cursorpix[0] lt box[0,1] and $
        state.cursorpix[1] gt box[1,1] and $
        state.cursorpix[1] lt box[1,0] then begin
        
        state.cursormode[1] = 'Move'
        state.pstart = state.cursorpix
        goto, next1

     endif 
     
;  Check for a resize request

     if total((state.cursorpix-reform(box[*,1]))^2) lt 15 then begin
        
        state.cursormode[1] = 'Draw' 
        state.pstart = reform(box[*,0])
        goto, next1
        
     endif
     
;  If neither, then it must be a draw request

     box[*] = !values.f_nan
     state.cursormode[1] = 'Draw' 
     state.pstart = state.cursorpix
     
  endif

  next1:
  
  if event.release eq 1 then begin
           
     case state.cursormode[1] of
        
        'Draw': begin
           
;  First a quick catch for a mistaken click and release.

           if state.pstart[0] eq state.cursorpix[0] and $
              state.pstart[1] eq state.cursorpix[1] then begin
              
              state.cursormode[1] = 'None'
              box[*] = !values.f_nan
              goto, cont                        
              
           endif
           
           state.cursormode[1] = 'None'
        
        end

         'Move': state.cursormode[1] = 'None'

         else:

      endcase

     ximgtool_imgrange
     ximgtool_bytsclimg
     ximgtool_mkplotimg,/PANIMG
     ximgtool_plotupdate

   endif

 ;  Now do the drawing

   case state.cursormode[1] of 

      'Draw': begin

         x   = state.cursorpix[0] > (state.pstart[0]+1)
         y   = state.cursorpix[1] < (state.pstart[1]-1)
         box = [[state.pstart],[[x,y]]]

        wset, state.plotwin_wid
        device, COPY=[0,0,state.plotwinsize[0],$
                      state.plotwinsize[1],0,0, $
                      buffers.(state.buffer).pixmap_wid]

        plots,[box[0,0]-0.5,box[0,0]-0.5,box[0,1]+0.5,box[0,1]+0.5, $
               box[0,0]-0.5],$
              [box[1,0]+0.5,box[1,1]-0.5,box[1,1]-0.5,box[1,0]+0.5,$
               box[1,0]+0.5],COLOR=state.rangeboxcolor,THICK=2

           plotsym,0,1,/FILL
           plots,box[0,1]+0.5,box[1,1]-0.5,PSYM=8,COLOR=state.rangeboxcolor
        
     end
     
     'Move': begin
        
        del = state.cursorpix-state.pstart
        box[0,*] = box[0,*]+del[0]
        box[1,*] = box[1,*]+del[1]
        
        wset, state.plotwin_wid
        device, COPY=[0,0,state.plotwinsize[0],$
                      state.plotwinsize[1],0,0, $
                      buffers.(state.buffer).pixmap_wid]
        
        plots,[box[0,0]-0.5,box[0,0]-0.5,box[0,1]+0.5,box[0,1]+0.5, $
               box[0,0]-0.5],$
              [box[1,0]+0.5,box[1,1]-0.5,box[1,1]-0.5,box[1,0]+0.5,$
               box[1,0]+0.5],COLOR=state.rangeboxcolor,THICK=2

           plotsym,0,1,/FILL
           plots,box[0,1]+0.5,box[1,1]-0.5,PSYM=8,COLOR=state.rangeboxcolor

           state.pstart = state.cursorpix
           
        end
     
     else:
     
  endcase
  
   state.region = box

cont:

  ximgtool_setminmax

end
;
;==============================================================================
;
pro ximgtool_rangebuttonevent,event

  common ximgtool_state

  widget_control, event.id, GET_UVALUE=uvalue

  case uvalue of 

     'Min Max': buffers.(state.buffer).imgrange = 'MinMax'

     '99.5%': buffers.(state.buffer).imgrange = '99.5%'

     '99%': buffers.(state.buffer).imgrange = '99%'

     '98%': buffers.(state.buffer).imgrange = '98%'

     '95%': buffers.(state.buffer).imgrange = '95%'

     '90%': buffers.(state.buffer).imgrange = '90%'

     'ZScale': buffers.(state.buffer).imgrange = 'ZScale'

     'User': begin

        buffers.(state.buffer).imgrange = 'User'
        
        state.rangepanel = 1
        
        widget_control, state.ximgtool_base,UPDATE=0
        widget_control, state.plotwin,/DESTROY
        
        state.panel = widget_base(state.ximgtool_base,$
                                  /BASE_ALIGN_CENTER,$
                                  EVENT_PRO='ximgtool_menuevent',$
                                  FRAME=2,$
                                  /ROW)
        
           fld = coyote_field2(state.panel,$
                               LABELFONT=state.buttonfont,$
                               FIELDFONT=state.textfont,$
                               TITLE='Min:',$
                               UVALUE='Img Min/Max',$
                               XSIZE=15,$
                               EVENT_PRO='ximgtool_menuevent',$
                               /CR_ONLY,$
                               TEXTID=textid)
           state.imgmin_fld = [fld,textid]
           
           fld = coyote_field2(state.panel,$
                               LABELFONT=state.buttonfont,$
                               FIELDFONT=state.textfont,$
                               TITLE='Max:',$
                               UVALUE='Img Min/Max',$
                               XSIZE=15,$
                               EVENT_PRO='ximgtool_menuevent',$
                               /CR_ONLY,$
                               TEXTID=textid)
           state.imgmax_fld = [fld,textid]

           button = widget_button(state.panel,$
                                  FONT=state.buttonfont,$
                                  EVENT_PRO='ximgtool_menuevent',$
                                  VALUE=' Done ',$
                                  UVALUE='Done Range User Defined')

        state.plotwin = widget_draw(state.ximgtool_base,$
                                    XSIZE=state.plotwinsize[0],$
                                    YSIZE=state.plotwinsize[1],$
                                    EVENT_PRO='ximgtool_plotwinevent',$
                                    /TRACKING_EVENTS,$
                                    /KEYBOARD_EVENTS,$
                                    /MOTION_EVENTS,$
                                    /BUTTON_EVENTS)

        widget_geom = widget_info(state.ximgtool_base, /GEOMETRY)
        
        state.winbuffer[0]=widget_geom.xsize-state.plotwinsize[0]
        state.winbuffer[1]=widget_geom.ysize-state.plotwinsize[1]


        widget_control, state.ximgtool_base,UPDATE=1

     end

     else:

  endcase

        
  ximgtool_imgrange
  ximgtool_setminmax
  ximgtool_bytsclimg
  ximgtool_mkplotimg,/PANIMG
  ximgtool_plotupdate
  ximgtool_trackwin
  ximgtool_updatebuttons


end
;
;==============================================================================
;
pro ximgtool_resizeevent,event

  common ximgtool_state

  widget_control, state.ximgtool_base, TLB_GET_SIZE=size
  
  state.plotwinsize[0]=size[0]-state.winbuffer[0] > 512
  state.plotwinsize[1]=size[1]-state.winbuffer[1]
  
  widget_control, state.plotwin,UPDATE=0
  widget_control, state.plotwin, DRAW_XSIZE=state.plotwinsize[0]
  widget_control, state.plotwin, DRAW_YSIZE=state.plotwinsize[1]
  widget_control, state.plotwin,UPDATE=1

  for i = 0,4  do begin
     
     wdelete,buffers.(i).pixmap_wid
     window, /FREE, /PIXMAP,XSIZE=state.plotwinsize[0],$
             YSIZE=state.plotwinsize[1]
     buffers.(i).pixmap_wid = !d.window

  endfor

;  Relocate the zoom window
  
  state.magnifierpos = [state.plotwinsize[0]-10-state.trackwinsize,$
                        state.plotwinsize[1]-10-state.trackwinsize]
  
  state.pannerpos = [10,state.plotwinsize[1]-10-state.trackwinsize]
  
;  Redisplay everything

  currentbuffer = state.buffer

  for i = 0,4 do begin
     
     if state.loadedbuffer[i] eq 1 then begin
        
        state.buffer = i
        ximgtool_chcmap
        ximgtool_mkplotimg
        ximgtool_plotupdate,RELOAD=(i eq currentbuffer) ? 0:1
        if i eq currentbuffer then ximgtool_trackwin
        
     endif
     state.buffer = currentbuffer
     
  endfor
  
  title = 'Ximgtool ['+string(state.plotwinsize[0],FORMAT='(I4)')+','+ $
          string(state.plotwinsize[1],FORMAT='(I4)')+ $
          '] ('+strtrim(state.buffer+1,2)+')'
  
  if buffers.(state.buffer).filename ne '' then title = title+' - '+$
     strtrim(buffers.(state.buffer).filename,2)

  buffers.(state.buffer).title = title
  
  widget_control, state.ximgtool_base,TLB_SET_TITLE=title

end
;
;==============================================================================
;
pro ximgtool_rotevent,event

  common ximgtool_state

  widget_control, event.id, GET_UVALUE=uvalue

  case uvalue of 

     'Rotation None': rot = 0

     'Rotation Flip X': rot = 5
     
     'Rotation Flip Y': rot = 7
 
     'Rotation -90': rot = 1

     'Rotation -180': rot = 2
                            
     'Rotation -270': rot = 3

     'Rotation Flip +XY': rot = 4
                                 
     'Rotation Flip -XY': rot = 6

     'N up E left': rot = 8

     else:  

  end

  state.cursormode = ['Zoom Mode','None']
  
  if state.lock then begin
     
     cbuffer = state.buffer
     for i = 0,4 do begin
        
        if state.loadedbuffer[i] eq 0 then continue
        state.buffer = i
        ximgtool_rotateimg,rot
        ximgtool_bytsclimg
        ximgtool_mkplotimg,/PANIMG
        
     endfor
     state.buffer = cbuffer
     
  endif else begin
     
     ximgtool_rotateimg,rot
     ximgtool_bytsclimg
     ximgtool_mkplotimg,/PANIMG
     
  endelse
  ximgtool_plotupdate
  ximgtool_trackwin
  
  ximgtool_updatebuttons
   
end
;
;==============================================================================
;
pro ximgtool_scaleevent,event

  common ximgtool_state

  widget_control, event.id, GET_VALUE=uvalue

  buffers.(state.buffer).imgscaling = uvalue
  ximgtool_bytsclimg
  ximgtool_mkplotimg,/PANIMG
  ximgtool_plotupdate
  ximgtool_trackwin
  ximgtool_updatebuttons

end
;
;==============================================================================
;
pro ximgtool_stretchevent,event

  common ximgtool_state

  if event.ch ne 0 then return

  if event.press ge 1 then state.cursormode[1] = 'Active'
  
  if event.release ge 1 then state.cursormode[1] = 'None'

  if state.cursormode[1] eq 'Active' then begin

     ximgtool_stretchct,event
     ximgtool_plotupdate
     
  endif
  
end
;
;==============================================================================
;
pro ximgtool_zoomevent,event

  common ximgtool_state

;  First check for arrow keys

  if event.key ne 0 then begin

     if event.release then goto, cont
     case event.key of 
        
        5: del = [-1.,0.]
        
        6: del = [1.,0.]
        
        7: del = [0.,1.]
        
        8: del = [0.,-1.]
        
        else:
        
     endcase
     
     del = del * (state.dev2img / 2.^buffers.(state.buffer).zoom > 1)

; Test to make sure that it will stay on the display
     
     tmppix = state.cursorpix+del
     xydev = convert_coord(tmppix[0],tmppix[1],/DATA,/TO_DEVICE)
     
     if xydev[0] lt 0 or xydev[0] gt state.plotwinsize[0] or $
        xydev[1] lt 0 or xydev[1] gt state.plotwinsize[1] then begin
        
        buffers.(state.buffer).cenpix = buffers.(state.buffer).cenpix + del
        center = 0
        goto, reload
        
     endif else begin
        
        state.cursorpix = state.cursorpix+del
        state.cursorpos = state.cursorpix

        tvcrs,state.cursorpix[0],state.cursorpos[1],/DATA

        goto, cont
        
     endelse
     
  endif

;  Now check for keyboard inputs

  center = 1
  if event.ch ne 0 then begin
     
     case string(event.ch) of 
        
        'c': begin
           
           buffers.(state.buffer).cenpix = $
              [buffers.(state.buffer).ncols, $
               buffers.(state.buffer).nrows]/2.-0.5

           goto, reload

        end

        'i': begin

           buffers.(state.buffer).zoom = buffers.(state.buffer).zoom + 1
           goto, reload

        end

        'o': begin

           buffers.(state.buffer).zoom = buffers.(state.buffer).zoom - 1
           goto, reload

        end

        else: return

     endcase

  endif

  case event.release of 

     0: return
     
     1:  begin

        xy = (convert_coord(event.x,event.y,/DEVICE,/TO_DATA))[0:1]
        if xy[0] lt 0 or xy[0] gt buffers.(state.buffer).ncols then return
        if xy[1] lt 0 or xy[1] gt buffers.(state.buffer).nrows then return
        
        buffers.(state.buffer).zoom = buffers.(state.buffer).zoom+1
        buffers.(state.buffer).cenpix=xy

     end

     2: begin

        xy = (convert_coord(event.x,event.y,/DEVICE,/TO_DATA))[0:1]
        if xy[0] lt 0 or xy[0] gt buffers.(state.buffer).ncols then return
        if xy[1] lt 0 or xy[1] gt buffers.(state.buffer).nrows then return
        
        buffers.(state.buffer).cenpix=xy

     end

     4: begin

        xy = (convert_coord(event.x,event.y,/DEVICE,/TO_DATA))[0:1]
        if xy[0] lt 0 or xy[0] gt buffers.(state.buffer).ncols then return
        if xy[1] lt 0 or xy[1] gt buffers.(state.buffer).nrows then return
        
        buffers.(state.buffer).zoom = buffers.(state.buffer).zoom-1
        buffers.(state.buffer).cenpix=xy

     end
     
  endcase
  
  reload:
  
  if state.lock then ximgtool_lockimages,CENTER=center else begin
     
     ximgtool_mkplotimg
     ximgtool_plotupdate,CENTER=center
     ximgtool_trackwin
     
  endelse
  
  cont:

end
;
;==============================================================================
;
pro ximgtool,img1,img2,img3,img4,img5,$
             EXT=ext,BUFFER=buffer,ROTATION=rotation,ZOOM=zoom,$
             ZTOFIT=ztofit,RANGE=range,ZRANGE=zrange,SCALE=scale,$
             COLORMAP=colormap,INVERT=invert,NOUPDATE=noupdate,OPLOT=oplot,$
             BITMASK=bitmask,ZUNITS=zunits,DIVISORS=divisors,$
             FILENAME=filename,HDR=hdr,LOCKIMAGES=lockimages,STDIMAGE=stdimage,$
             MAGNIFIER=magnifier,PANNER=panner,PLOTWINSIZE=plotwinsize,$
             POSITION=position,WID=wid,GROUP_LEADER=group_leader,$
             NODISPLAY=nodisplay,CLEARFRAMES=clearframes,QUIT=quit,CANCEL=cancel

  canel = 0

  common ximgtool_state

  ;  Start widget if necessary

  if xregistered('ximgtool') and n_elements(CLEARFRAMES) ne 0 then begin

     ximgtool_clearframes,clearframes,CANCEL=cancel
    
  endif

  if xregistered('ximgtool') and keyword_set(QUIT) then begin

     widget_control, state.ximgtool_base,/DESTROY
     return
     
  endif

  
  if not xregistered('ximgtool') then begin


     ximgtool_startup,DIVISORS=divisors,STDIMAGE=stdimage,$
                      PLOTWINSIZE=plotwinsize,POSITION=position, $
                      GROUP_LEADER=group_leader
     
     
  endif

;  Get user inputs

  state.magnifier = (n_elements(MAGNIFIER) ne 0) ? keyword_set(MAGNIFIER):1

  state.panner = (n_elements(PANNER) ne 0) ? keyword_set(PANNER):1

;  DIVISORS

  if n_elements(DIVISORS) ne 0 then begin

     *state.divisors = strtrim(strsplit(divisors,',',/EXTRACT),2)
     state.divisorson = 1
     if state.divisorspanel then begin

        widget_control, state.divisors_fld[1], SET_VALUE=divisors
        widget_control, state.divisorson_bg,SET_VALUE=1

     endif

  endif else begin

     *state.divisors = ''
     state.divisorson = 0

     if state.divisorspanel then begin

        widget_control, state.divisors_fld[1],SET_VALUE=''
        widget_control, state.divisorson_bg,SET_VALUE=0
        
     endif

  endelse

;  Load images

  if n_params() le 1 then begin

     ximgtool_loadimage,img1,EXT=ext,SCALE=scale,ROTATION=rotation, $
                        ZOOM=zoom,ZTOFIT=ztofit,XYPOS=xycenter, $
                        COLORMAP=colormap,INVERT=invert,NOUPDATE=noupdate, $
                        BITMASK=bitmask,BUFFER=buffer,$
                        RANGE=range,ZRANGE=zrange,STDIMAGE=stdimage, $
                        OPLOT=oplot,ZUNITS=zunits,FILENAME=filename,HDR=hdr, $
                        NODISPLAY=nodisplay,CANCEL=cancel
     
     if cancel then return

  endif else begin

     for i = 0,n_params()-1 do begin

        case i of 
           
           0: img = img1
           1: img = img2
           2: img = img3
           3: img = img4
           4: img = img5
           
        endcase

        ximgtool_loadimage,img,EXT=ext,SCALE=scale,ROTATION=rotation, $
                           ZOOM=zoom,ZTOFIT=ztofit,XYPOS=xycenter, $
                           COLORMAP=colormap,INVERT=invert, $
                           NOUPDATE=noupdate,BITMASK=bitmask,$
                           BUFFER=i+1,RANGE=range,ZRANGE=zrange, $
                           STDIMAGE=stdimage,OPLOT=oplot,ZUNITS=zunits, $
                           FILENAME=filename,HDR=hdr,NODISPLAY=i,CANCEL=cancel
        
        if cancel then return
        
     endfor 
     
  endelse


  if keyword_set(LOCKIMAGES) then begin
     
     state.lock = 1
     widget_control, state.but_lock,/SET_BUTTON
     ximgtool_lockimages
     ximgtool_updatebuttons
     
  endif

  if n_params() gt 1 then begin

     state.buffer = 0
     ximgtool_chbuffer

  endif
  
  
  wid = state.plotwin_wid


end
