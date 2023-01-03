;+
; NAME:
;     xmc_select
;
; PURPOSE:
;     To allow the user to select amoungst various choices
;
; CATEGORY:
;     widgets
;
; CALLING SEQUENCE:
;     result = xmc_select(values,message,[label],NONEXCLUSIVE=nonexclusive, $
;                         GROUP_LEADER=group_leader,BLOCK=block,CANCEL=cancel)
;
; INPUTS:
;     values  - An array of possible choices
;     message - A text message.
;
; OPTIONAL INPUTS:
;     label   - A label for the values.  Defaults to 'Value: '
;
; KEYWORD PARAMETERS:
;     NONEXCLUSIVE = Set this keyword to cause buttons to be placed 
;                    in an non-exclusive base, in which any number of 
;                    buttons can be selected at once.
;     GROUP_LEADER - The group leader if called from another widget.  
;     BLOCK        - Set to block the group_leader
;     CANCEL       - Set on return if there is a problem.
;
; OUTPUTS:
;     The index values for the selected values
;
; OPTIONAL OUTPUTS:
;     None
;
; COMMON BLOCKS:
;     xmc_select_common
;
; SIDE EFFECTS:
;     None
;
; RESTRICTIONS:
;     None
;
; PROCEDURE:
;     None
;
; EXAMPLE:
;     result = xmc_select(['1','2','3'],'Please select a number.')
;
; MODIFICATION HISTORY:
;     2008-06-07 - Written by M. Cushing, Inistitute for Astronomy,
;                  University of Hawaii
;     2009-03-02 - Modified to include CANCEL button.
;-
;
;*******************************************************************************
;
pro xmc_select_init,values

  common xmc_select_state,state

  state = {select_base:0L,$
           idx:intarr(n_elements(values)),$
           cancel:0}


end
;
;*******************************************************************************
;
pro xmc_select_event,event

  common xmc_select_state
  widget_control, event.id,  GET_UVALUE = uvalue
  
  case uvalue of 

     'Select': state.idx[event.value] = event.select

     'Accept': begin

        z = where(state.idx eq 1,cnt)
        if cnt ne 0 then widget_control,event.top,/DESTROY

     end

     'Cancel': begin

        state.cancel = 1
        widget_control,event.top,/DESTROY


     end

  endcase

end
;
;*******************************************************************************
;
function xmc_select,values,message,label,NONEXCLUSIVE=nonexclusive, $
                    GROUP_LEADER=group_leader,BLOCK=block,CANCEL=cancel

  cancel = 0

  if n_params() lt 3 then begin

     print, 'Syntax - result = xmc_select(values,message,$'
     print, '                             NONEXCLUSIVE=nonexclusive,$'
     print, '                             GROUP_LEADER=group_leader,$'
     print, '                             BLOCK=block,CANCEL=cancel)'
     cancel = 1
     return, -1

  endif

  common xmc_select_state

  if keyword_set(BLOCK) then widget_control, group_leader, SENSITIVE=0


  mc_getfonts,buttonfont,textfont,CANCEL=cancel
  if cancel then return,-1

  xmc_select_init,values


  state.select_base = widget_base(GROUP_LEADER=group_leader, $
                                  /COLUMN,$
                                  FLOATING=keyword_set(GROUP_LEADER),$
                                  TITLE='Pick Value')
  
     lbl = widget_label(state.select_base,$
                        VALUE=message,$
                        FONT=textfont)

     setvalue = (keyword_set(NONEXCLUSIVE) eq 1) ? $
                [1,replicate(0,n_elements(values)-1)]: 0
     state.idx[0] = 1
     
     bg = cw_bgroup(state.select_base,$
                    values,$
                    FONT=buttonfont,$
                    NONEXCLUSIVE=keyword_set(NONEXCLUSIVE),$
                    EXCLUSIVE=(keyword_set(NONEXCLUSIVE) eq 1) ? 0:1,$
                    /RETURN_INDEX,$
                     LABEL_LEFT=label,$
                    SET_VALUE=setvalue,$
                    /ROW,$
                    UVALUE='Select')




     accept = widget_button(state.select_base,$
                            VALUE='Accept',$
                            UVALUE='Accept',$
                            FONT=buttonfont)

     button = widget_button(state.select_base,$
                            VALUE='Cancel',$
                            UVALUE='Cancel',$
                            FONT=buttonfont)

  widget_control, state.select_base, /REALIZE

  widget_control, accept,/INPUT_FOCUS

  XManager, 'xmc_select', $
            state.select_base
  cancel = state.cancel

  if keyword_set(BLOCK) then widget_control, group_leader, SENSITIVE=1

  return, where(state.idx eq 1,cnt)

end
