CLASS z2ui5_cl_pop_layout DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_serializable_object.
    INTERFACES z2ui5_if_app.

    TYPES:
      BEGIN OF fixvalue,
        low        TYPE string,
        high       TYPE string,
        option     TYPE string,
        ddlanguage TYPE string,
        ddtext     TYPE string,
      END OF fixvalue.
    TYPES fixvalues TYPE STANDARD TABLE OF fixvalue WITH DEFAULT KEY.

    TYPES BEGIN OF ty_s_layo.
    INCLUDE TYPE z2ui5_t_11.
    TYPES   selkz TYPE abap_bool.
    TYPES END OF ty_s_layo.
    TYPES ty_t_layo TYPE STANDARD TABLE OF ty_s_layo WITH DEFAULT KEY.

    DATA mo_layout           TYPE REF TO z2ui5_cl_layout.
    DATA mt_controls         TYPE z2ui5_cl_layout=>ty_t_controls.
    DATA mt_Head             TYPE ty_t_layo.
    DATA mv_descr            TYPE string.
    DATA mv_layout           TYPE string.
    DATA mv_def              TYPE abap_bool.
    DATA mv_usr              TYPE abap_bool.
    DATA mv_open             TYPE abap_bool.
    DATA mv_delete           TYPE abap_bool.
    DATA mt_halign           TYPE fixvalues.
    DATA mt_importance       TYPE fixvalues.
    DATA mv_active_subcolumn TYPE string.
    DATA mv_rerender         TYPE abap_bool.

    CLASS-METHODS on_event_layout
      IMPORTING
        !client TYPE REF TO z2ui5_if_client
        !layout TYPE REF TO z2ui5_cl_layout.

    CLASS-METHODS render_layout_function
      IMPORTING
        !xml          TYPE REF TO z2ui5_cl_xml_view
        !client       TYPE REF TO z2ui5_if_client
        !layout       TYPE REF TO z2ui5_cl_layout
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_xml_view.

    CLASS-METHODS factory
      IMPORTING
        !layout       TYPE REF TO z2ui5_cl_layout
        open_layout   TYPE abap_bool OPTIONAL
        delete_layout TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_pop_layout.

  PROTECTED SECTION.
    DATA client  TYPE REF TO z2ui5_if_client.
    DATA mv_init TYPE abap_bool.

    METHODS render_edit.
    METHODS on_event.
    METHODS render_save.
    METHODS save_layout.
    METHODS get_layouts.
    METHODS init_edit.
    METHODS render_delete.
    METHODS render_add_subcolumn.
    METHODS on_event_subcoloumns.
    METHODS check_rerender_necessary.

    METHODS on_init
      IMPORTING
        !control TYPE clike.

    METHODS render_open
      IMPORTING
        external_call TYPE abap_bool OPTIONAL.

    METHODS get_selected_layout
      RETURNING
        VALUE(result) TYPE ty_s_layo.

    METHODS delete_selected_layout
      IMPORTING
        !Head TYPE ty_s_layo.

    METHODS set_selected_layout
      IMPORTING
        !Head TYPE ty_s_layo.

    METHODS check_width_unit
      IMPORTING
        !width        TYPE z2ui5_t_12-width
      RETURNING
        VALUE(result) TYPE z2ui5_t_12-width.

    CLASS-METHODS get_relative_name_of_table
      IMPORTING
        !table        TYPE any
      RETURNING
        VALUE(result) TYPE string.

ENDCLASS.


CLASS z2ui5_cl_pop_layout IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.

    IF mv_init = abap_false.
      mv_init = abap_true.

      on_init( mo_layout->ms_layout-s_head-control ).

      init_edit( ).

      render_edit( ).

      client->popup_model_update( ).

    ENDIF.

    on_event( ).

  ENDMETHOD.

  METHOD on_init.
        DATA temp1 TYPE z2ui5_cl_pop_layout=>fixvalues.
        DATA temp2 LIKE LINE OF temp1.
        DATA temp3 TYPE z2ui5_cl_pop_layout=>fixvalues.
        DATA temp4 LIKE LINE OF temp3.
    DATA temp5 TYPE z2ui5_cl_pop_layout=>fixvalues.
    DATA temp6 LIKE LINE OF temp5.

    CASE Control.
      WHEN z2ui5_cl_layout=>m_table.
        
        CLEAR temp1.
        
        temp2-low = 'Begin'.
        temp2-ddtext = 'Locale-specific positioning at the beginning of the line'.
        INSERT temp2 INTO TABLE temp1.
        temp2-low = 'Center'.
        temp2-ddtext = 'Centered text alignment'.
        INSERT temp2 INTO TABLE temp1.
        temp2-low = 'End'.
        temp2-ddtext = 'Locale-specific positioning at the end of the line'.
        INSERT temp2 INTO TABLE temp1.
        temp2-low = 'Initial'.
        temp2-ddtext = 'Sets no text align, so the browser default is used'.
        INSERT temp2 INTO TABLE temp1.
        temp2-low = 'Left'.
        temp2-ddtext = 'Hard option for left alignment'.
        INSERT temp2 INTO TABLE temp1.
        temp2-low = 'Right'.
        temp2-ddtext = 'Hard option for right alignment'.
        INSERT temp2 INTO TABLE temp1.
        mt_halign = temp1.

      WHEN z2ui5_cl_layout=>ui_table.
        
        CLEAR temp3.
        
        temp4-low = 'Begin'.
        temp4-ddtext = 'Locale-specific positioning at the beginning of the line'.
        INSERT temp4 INTO TABLE temp3.
        temp4-low = 'Center'.
        temp4-ddtext = 'Centered text alignment'.
        INSERT temp4 INTO TABLE temp3.
        temp4-low = 'End'.
        temp4-ddtext = 'Locale-specific positioning at the end of the line'.
        INSERT temp4 INTO TABLE temp3.
        temp4-low = 'Left'.
        temp4-ddtext = 'Hard option for left alignment'.
        INSERT temp4 INTO TABLE temp3.
        temp4-low = 'Right'.
        temp4-ddtext = 'Hard option for right alignment'.
        INSERT temp4 INTO TABLE temp3.
        mt_halign = temp3.
    ENDCASE.

    
    CLEAR temp5.
    
    temp6-low = 'High'.
    temp6-ddtext = 'High priority'.
    INSERT temp6 INTO TABLE temp5.
    temp6-low = 'Low'.
    temp6-ddtext = 'Low priority'.
    INSERT temp6 INTO TABLE temp5.
    temp6-low = 'Medium'.
    temp6-ddtext = 'Medium priority'.
    INSERT temp6 INTO TABLE temp5.
    temp6-low = 'None'.
    temp6-ddtext = 'Default, none priority'.
    INSERT temp6 INTO TABLE temp5.
    mt_importance = temp5.

    mt_controls = z2ui5_cl_layout=>get_controls( ).

  ENDMETHOD.

  METHOD render_edit.

    DATA popup TYPE REF TO z2ui5_cl_xml_view.
    DATA dialog TYPE REF TO z2ui5_cl_xml_view.
    DATA tab TYPE REF TO z2ui5_cl_xml_view.
    DATA list TYPE REF TO z2ui5_cl_xml_view.
    DATA cells TYPE REF TO z2ui5_cl_xml_view.
    DATA columns TYPE REF TO z2ui5_cl_xml_view.
    DATA lt_comp TYPE abap_component_tab.
    DATA col TYPE REF TO z2ui5_cl_xml_view.
    DATA temp7 LIKE LINE OF lt_comp.
    DATA comp LIKE REF TO temp7.
      DATA control TYPE z2ui5_cl_layout=>ty_s_controls.
          DATA temp8 TYPE string_table.
    popup = z2ui5_cl_xml_view=>factory_popup( ).

    
    dialog = popup->dialog( title        = 'Edit Layout'
*                                  stretch      = abap_true
                                  contentwidth = '90%'
                                  afterclose   = client->_event( 'CLOSE' ) ).

    
    tab = dialog->table( growing          = abap_true
                               growingthreshold = '80'
                               items            = client->_bind_edit( mo_layout->ms_layout-t_layout ) ).

    
    list = tab->column_list_item( ).

    
    cells = list->cells( ).

    
    columns = tab->columns( ).

    
    lt_comp = z2ui5_cl_util=>rtti_get_t_attri_by_any( mo_layout->ms_layout-t_layout ).

    
    col = columns->column( '15rem' )->header( `` ).
    col->text( `Row` ).

    
    
    LOOP AT lt_comp REFERENCE INTO comp.

      
      READ TABLE mt_controls INTO control WITH KEY control   = mo_layout->ms_layout-s_head-control
                                                         attribute = comp->name
                                                         active    = abap_true.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CASE control-attribute.
        WHEN 'VISIBLE'.
          col = columns->column( '4.5rem' )->header( `` ).
          col->text( 'Visible' ).
        WHEN 'MERGE'.
          col = columns->column( '4.5rem' )->header( `` ).
          col->text( 'Merge' ).
        WHEN 'HALIGN'.
          col = columns->column( )->header( `` ).
          col->text( 'Align' ).
        WHEN 'IMPORTANCE'.
          col = columns->column( )->header( `` ).
          col->text( 'Importance' ).
        WHEN 'WIDTH'.
          col = columns->column( `7rem` )->header( `` ).
          col->text( 'Width in rem' ).
        WHEN 'SEQUENCE'.
          col = columns->column( `5rem` )->header( `` ).
          col->text( 'Sequence' ).
        WHEN 'ALTERNATIVE_TEXT'.
          col = columns->column( )->header( `` ).
          col->text( 'Alternative Text' ).
        WHEN 'REFERENCE_FIELD'.
          col = columns->column( )->header( `` ).
          col->text( 'Reference Field' ).
        WHEN 'SUBCOLUMN'.
          col = columns->column( )->header( `` ).
          col->text( 'Subcolumn' ).
      ENDCASE.

    ENDLOOP.

    LOOP AT lt_comp REFERENCE INTO comp.

      IF comp->name = 'FNAME'.
        cells->text( |\{{ comp->name }\} { cl_abap_char_utilities=>cr_lf } \{TLABEL\} | ).
      ENDIF.

      READ TABLE mt_controls INTO control WITH KEY control   = mo_layout->ms_layout-s_head-control
                                                   attribute = comp->name
                                                   active    = abap_true.

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CASE comp->name.

        WHEN 'VISIBLE' OR 'MERGE'.

          cells->switch( type  = 'AcceptReject'
                         state = |\{{ comp->name }\}|     ).

        WHEN 'HALIGN'.

          cells->combobox( selectedkey = |\{{ comp->name }\}|
                           items       = client->_bind_local( mt_halign )
                        )->item( key  = '{LOW}'
                                 text = '{LOW} - {DDTEXT}' ).

        WHEN 'IMPORTANCE'.

          cells->combobox( selectedkey = |\{{ comp->name }\}|
                           items       = client->_bind_local( mt_importance )
                        )->item( key  = '{LOW}'
                                 text = '{LOW} - {DDTEXT}' ).

        WHEN 'WIDTH'.

          cells->input( value     = |\{{ comp->name }\}|
                        maxLength = `7` ).

        WHEN 'SEQUENCE'.

          cells->input( value     = |\{{ comp->name }\}|
                        maxLength = `5`
                        width     = `3rem` ).

        WHEN 'ALTERNATIVE_TEXT'.

          cells->input( |\{{ comp->name }\}| ).

        WHEN 'SUBCOLUMN'.

          
          CLEAR temp8.
          INSERT `${FNAME}` INTO TABLE temp8.
          cells->button( text  = |\{{ comp->name }\}|
                         icon  = `sap-icon://add`
                         press = client->_event( val   = 'CALL_SUBCOLUMN'
                                                 t_arg = temp8 ) ).

        WHEN 'REFERENCE_FIELD'.

          cells->combobox( selectedkey = |\{{ comp->name }\}|
                           items       = client->_bind_edit( mo_layout->ms_layout-t_layout )
                        )->item( key  = '{FNAME}'
                                 text = '{FNAME} - {TLABEL}' ).

      ENDCASE.

    ENDLOOP.

    dialog->buttons(
          )->button( press = ''
                     icon  = 'sap-icon://edit'
                     type  = 'Emphasized'
          )->button( press = client->_event( 'LAYOUT_LOAD' )
                     icon  = 'sap-icon://open-folder'
                     type  = 'Ghost'
          )->button( press = client->_event( 'LAYOUT_DELETE' )
                     icon  = 'sap-icon://delete'
                     type  = 'Ghost'
          )->button( type    = 'Transparent'
                     enabled = abap_false
                     text    = `               `
         )->button( text  = 'Close'
                    icon  = 'sap-icon://sys-cancel-2'
                    press = client->_event( 'CLOSE' )
         )->button( text  = 'Okay'
                    icon  = 'sap-icon://accept'
                    press = client->_event( 'EDIT_OKAY' )
         )->button( text  = 'Save'
                    press = client->_event( 'EDIT_SAVE' )
                    icon  = 'sap-icon://save'
                    type  = 'Emphasized' ).

    client->popup_display( popup->get_root( )->xml_get( ) ).

  ENDMETHOD.

  METHOD on_event.
        DATA temp10 LIKE LINE OF mo_layout->ms_layout-t_layout.
        DATA layout LIKE REF TO temp10.

    CASE client->get( )-event.

      WHEN 'LAYOUT_EDIT'.

        init_edit( ).

        render_edit( ).

      WHEN 'LAYOUT_LOAD'.

        get_layouts( ).

        render_open( ).

      WHEN 'LAYOUT_DELETE'.

        get_layouts( ).

        render_delete( ).

      WHEN 'EDIT_OKAY'.

        
        
        LOOP AT mo_layout->ms_layout-t_layout REFERENCE INTO layout.
          layout->tlabel           = mo_layout->set_text( layout->* ).
          layout->alternative_text = to_upper( layout->alternative_text ).
        ENDLOOP.

        mo_layout->ms_layout-t_layout = mo_layout->sort_by_seqence( mo_layout->ms_layout-t_layout ).

        check_rerender_necessary( ).

        client->popup_destroy( ).

        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN 'CLOSE'.

        client->popup_destroy( ).

        mo_layout->ms_layout = mo_layout->ms_layout_tmp.

        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN 'EDIT_SAVE'.

        render_save( ).

      WHEN 'SAVE_CLOSE'.

        client->popup_destroy( ).

        render_edit( ).

      WHEN 'SAVE_SAVE'.

        save_layout( ).

        mv_rerender = abap_true.

        client->popup_destroy( ).

        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN 'OPEN_SELECT'.

        set_selected_layout( get_selected_layout( ) ).

        mv_rerender = abap_true.

        client->popup_destroy( ).

        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN 'DELETE_SELECT'.

        delete_selected_layout( get_selected_layout( ) ).

        DELETE mt_head WHERE selkz = abap_true.

        client->popup_model_update( ).

      WHEN OTHERS.

        on_event_subcoloumns( ).

    ENDCASE.

  ENDMETHOD.

  METHOD factory.

    CREATE OBJECT result.

    result->mo_layout = layout.

    result->mv_open   = open_layout.
    result->mv_delete = delete_layout.

    result->mo_layout->ms_layout_tmp = result->mo_layout->ms_layout.

  ENDMETHOD.

  METHOD render_layout_function.

    result = xml.

    result->button( icon  = 'sap-icon://action-settings'
                    press = client->_event( layout->ms_layout-s_head-guid ) ).

  ENDMETHOD.

  METHOD render_save.

    DATA popup TYPE REF TO z2ui5_cl_xml_view.
    DATA dialog TYPE REF TO z2ui5_cl_xml_view.
    DATA form TYPE REF TO z2ui5_cl_xml_view.
    popup = z2ui5_cl_xml_view=>factory_popup( ).

    
    dialog = popup->dialog( title        = 'Save'
                                  contentwidth = '80%'
                                  afterclose   = client->_event( 'SAVE_CLOSE' ) ).

    
    form = dialog->content( )->simple_form( title                   = 'Layout'
                                                  editable                = abap_true
                                                  labelspanxl             = `4`
                                                  labelspanl              = `4`
                                                  labelspanm              = `4`
                                                  labelspans              = `4`
                                                  adjustlabelspan         = abap_false
                                                  emptySpanXL             = `0`
                                                  emptySpanL              = `0`
                                                  emptySpanM              = `0`
                                                  emptySpanS              = `0`
                                                  columnsXL               = `2`
                                                  columnsL                = `2`
                                                  columnsM                = `2`
                                                  singleContainerFullSize = `true` ).

    form->toolbar( )->title( 'Layout' ).

    form->content( 'form'
                           )->label( 'Layout'
                           )->input( client->_bind_edit( mv_layout )
                           )->label( 'Description'
                           )->input( client->_bind_edit( mv_descr ) ).

    form->toolbar( )->title( `Save Options` ).

    form->content( 'form'
                           )->label( 'Default Layout'
                           )->switch( type  = 'AcceptReject'
                                      state = client->_bind_edit( mv_def )
                           )->label( 'User specific'
                           )->switch( type  = 'AcceptReject'
                                      state = client->_bind_edit( mv_usr ) ).

    dialog->buttons( )->button( text  = 'Back'
                                icon  = 'sap-icon://nav-back'
                                press = client->_event( 'SAVE_CLOSE' )
          )->button( text  = 'Save'
                     press = client->_event( 'SAVE_SAVE' )
                     type  = 'Success'
                     icon  = 'sap-icon://save' ).

    client->popup_display( popup->get_root( )->xml_get( ) ).

  ENDMETHOD.

  METHOD save_layout.

    DATA line      TYPE z2ui5_t_12.
    TYPES temp1 TYPE STANDARD TABLE OF z2ui5_t_12 WITH DEFAULT KEY.
DATA Positions TYPE temp1.
      DATA user LIKE sy-uname.
    DATA temp11 TYPE z2ui5_t_11.
    DATA Head LIKE temp11.
    DATA layout LIKE LINE OF mo_layout->ms_layout-t_layout.
TYPES BEGIN OF temp12.
TYPES guid TYPE z2ui5_t_11-guid.
TYPES END OF temp12.
    TYPES temp2 TYPE STANDARD TABLE OF temp12 WITH DEFAULT KEY.
DATA t_heads TYPE temp2.
TYPES BEGIN OF temp13.
TYPES mandt TYPE z2ui5_t_12-mandt.
TYPES guid TYPE z2ui5_t_12-guid.
TYPES pos_guid TYPE z2ui5_t_12-pos_guid.
TYPES END OF temp13.
        TYPES temp3 TYPE STANDARD TABLE OF temp13 WITH DEFAULT KEY.
DATA t_del TYPE temp3.
            DATA temp14 LIKE LINE OF Positions.
            DATA r_pos LIKE REF TO temp14.
        DATA pos LIKE LINE OF positions.

    IF mv_layout IS INITIAL.
      client->message_toast_display( 'Layoutname missing.' ).
      RETURN.
    ENDIF.

    IF mv_usr = abap_true.
      
      user = sy-uname.
    ENDIF.

    
    CLEAR temp11.
    temp11-guid = mo_layout->ms_layout-s_head-guid.
    temp11-layout = mv_layout.
    temp11-control = mo_layout->ms_layout-s_head-control.
    temp11-handle01 = mo_layout->ms_layout-s_head-handle01.
    temp11-handle02 = mo_layout->ms_layout-s_head-handle02.
    temp11-handle03 = mo_layout->ms_layout-s_head-handle03.
    temp11-handle04 = mo_layout->ms_layout-s_head-handle04.
    temp11-descr = mv_descr.
    temp11-def = mv_def.
    temp11-uname = user.
    
    Head = temp11.

    
    LOOP AT mo_layout->ms_layout-t_layout INTO layout.

      CLEAR line.

      MOVE-CORRESPONDING mo_layout->ms_layout-s_head TO line.
      MOVE-CORRESPONDING layout TO line.
      line-layout = mv_layout.

      line-width  = check_width_unit( line-width ).

      APPEND line TO Positions.

    ENDLOOP.

    " Does a matching Layout exist?
    
    

    SELECT guid FROM z2ui5_t_11 INTO TABLE t_heads
      WHERE layout   = Head-layout
        AND control  = Head-control
        AND handle01 = Head-handle01
        AND handle02 = Head-handle02
        AND handle03 = Head-handle03
        AND handle04 = Head-handle04
      .

    IF sy-subrc = 0.

      IF t_heads IS NOT INITIAL.

        
        

        SELECT mandt guid pos_guid FROM z2ui5_t_12 INTO TABLE t_del
          FOR ALL ENTRIES IN t_heads
          WHERE guid = t_heads-guid
          .

        IF sy-subrc = 0.
          " if structure was changed we do not want any dead entries ...
          DELETE z2ui5_t_12 FROM TABLE t_del.
          COMMIT WORK AND WAIT.
        ENDIF.

      ENDIF.
    ELSE.

      " guid already taken
      SELECT guid FROM z2ui5_t_11 INTO TABLE t_heads
        WHERE guid = head-guid
        .

      IF sy-subrc = 0.
        " Layout changed and saved under new name -> new Guid needed
        TRY.
            Head-guid = cl_system_uuid=>create_uuid_c32_static( ).

            
            
            LOOP AT Positions REFERENCE INTO r_pos.
              r_pos->guid = Head-guid.
            ENDLOOP.

          CATCH cx_root.
        ENDTRY.

      ENDIF.

    ENDIF.

    MODIFY z2ui5_t_11 FROM Head.
    IF sy-subrc = 0.

      MODIFY z2ui5_t_12 FROM TABLE Positions.

      IF sy-subrc = 0.

        COMMIT WORK AND WAIT.

        client->message_toast_display( 'Data saved.' ).

        mo_layout->ms_layout-s_head = Head.

        CLEAR mo_layout->ms_layout-t_layout.

        
        LOOP AT positions INTO pos.
          CLEAR layout.
          MOVE-CORRESPONDING pos TO layout.
          layout-tlabel = mo_layout->set_text( layout ).
          APPEND layout TO mo_layout->ms_layout-t_layout.
        ENDLOOP.

        mo_layout->ms_layout-t_layout = mo_layout->sort_by_seqence( mo_layout->ms_layout-t_layout ).
        mo_layout->ms_layout-t_layout = mo_layout->set_sub_columns( mo_layout->ms_layout-t_layout ).
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD render_delete.

    DATA popup TYPE REF TO z2ui5_cl_xml_view.
    DATA dialog TYPE REF TO z2ui5_cl_xml_view.
    popup = z2ui5_cl_xml_view=>factory_popup( ).

    
    dialog = popup->dialog( title        = 'Delete Layout'
                                  contentwidth = '90%'
                                  afterclose   = client->_event( 'CLOSE' ) ).

    dialog->table( mode  = 'SingleSelectLeft'
                   items = client->_bind_edit( mt_head )
                )->columns(
                    )->column( )->text( 'Layout' )->get_parent(
                    )->column( )->text( 'Description'
                    )->get_parent( )->get_parent(
                )->items(
                    )->column_list_item( selected = '{SELKZ}'
                        )->cells(
                            )->text( '{LAYOUT}'
                            )->text( '{DESCR}' ).

    dialog->buttons(
          )->button( press = client->_event( 'LAYOUT_EDIT' )
                     icon  = 'sap-icon://edit'
                     type  = 'Ghost'
          )->button( press = client->_event( 'LAYOUT_LOAD' )
                     icon  = 'sap-icon://open-folder'
                     type  = 'Ghost'
          )->button( press = ''
                     icon  = 'sap-icon://delete'
                     type  = 'Emphasized'
          )->button( type    = 'Transparent'
                     enabled = abap_false
                     text    = `               `
         )->button( text  = 'Close'
                    icon  = 'sap-icon://sys-cancel-2'
                    press = client->_event( 'CLOSE' )
         )->button( text  = 'Delete'
                    icon  = 'sap-icon://delete'
                    press = client->_event( 'DELETE_SELECT' )
                    type  = 'Reject' ).

    client->popup_display( popup->stringify( ) ).

  ENDMETHOD.

  METHOD render_open.
    " TODO: parameter EXTERNAL_CALL is never used (ABAP cleaner)

    DATA popup TYPE REF TO z2ui5_cl_xml_view.
    DATA dialog TYPE REF TO z2ui5_cl_xml_view.
    popup = z2ui5_cl_xml_view=>factory_popup( ).

    
    dialog = popup->dialog( title        = 'Select Layout'
                                  contentwidth = '90%'
                                  afterclose   = client->_event( 'CLOSE' ) ).

    dialog->table( mode  = 'SingleSelectLeft'
                   items = client->_bind_edit( mt_head )
                )->columns(
                    )->column( )->text( 'Layout' )->get_parent(
                    )->column( )->text( 'Description' )->get_parent(
                    )->column( )->text( 'Default' )->get_parent(
                    )->get_parent(
                )->items(
                    )->column_list_item( selected = '{SELKZ}'
                        )->cells(
                            )->text( '{LAYOUT}'
                            )->text( '{DESCR}'
                            )->text( '{DEF}' ).

    dialog->buttons(
          )->button( press = client->_event( 'LAYOUT_EDIT' )
                     icon  = 'sap-icon://edit'
                     type  = 'Ghost'
          )->button( press = ''
                     icon  = 'sap-icon://open-folder'
                     type  = 'Emphasized'
          )->button( press = client->_event( 'LAYOUT_DELETE' )
                     icon  = 'sap-icon://delete'
                     type  = 'Ghost'
          )->button( type    = 'Transparent'
                     enabled = abap_false
                     text    = `               `
         )->button( text  = 'Close'
                    icon  = 'sap-icon://sys-cancel-2'
                    press = client->_event( 'CLOSE' )
         )->button( text  = 'OK'
                    icon  = 'sap-icon://accept'
                    press = client->_event( 'OPEN_SELECT' )
                    type  = 'Emphasized' ).

    client->popup_display( popup->stringify( ) ).

  ENDMETHOD.

  METHOD get_selected_layout.

    DATA temp15 TYPE z2ui5_cl_pop_layout=>ty_s_layo.
    DATA temp16 TYPE z2ui5_cl_pop_layout=>ty_s_layo.
    CLEAR temp15.
    
    READ TABLE mt_head INTO temp16 WITH KEY selkz = abap_true.
    IF sy-subrc = 0.
      temp15 = temp16.
    ENDIF.
    result = temp15.

  ENDMETHOD.

  METHOD set_selected_layout.

*    IF Head IS INITIAL.
*      RETURN.
*    ENDIF.
*
*    SELECT SINGLE guid,
*                  layout,
*                  control,
*                  handle01,
*                  handle02,
*                  handle03,
*                  handle04,
*                  descr,
*                  def,
*                  uname
*      FROM z2ui5_layo_t_01
*      WHERE guid = @Head-guid
*      INTO CORRESPONDING FIELDS OF @mo_layout->ms_layout-s_head ##SUBRC_OK.
*
*    SELECT guid,
*           layout,
*           control,
*           handle01,
*           handle02,
*           handle03,
*           handle04,
*           fname,
*           rollname,
*           visible,
*           merge,
*           halign,
*           importance,
*           width,
*           sequence,
*           alternative_text,
*           subcolumn,
*           reference_field
*      FROM z2ui5_layo_t_02
*      WHERE guid = @Head-guid
*      INTO CORRESPONDING FIELDS OF TABLE @mo_layout->ms_layout-t_layout  ##SUBRC_OK.

    mo_layout = z2ui5_cl_layout=>factory_by_guid( layout_guid = head-guid ).


  ENDMETHOD.

  METHOD get_layouts.
      FIELD-SYMBOLS <temp17> TYPE z2ui5_cl_pop_layout=>ty_s_layo.
DATA Head LIKE REF TO <temp17>.
        FIELD-SYMBOLS <temp18> TYPE z2ui5_cl_pop_layout=>ty_s_layo.

    mt_head = mo_layout->select_layouts( control  = mo_layout->ms_layout-s_head-control
                                         handle01 = mo_layout->ms_layout-s_head-handle01
                                         handle02 = mo_layout->ms_layout-s_head-handle02
                                         handle03 = mo_layout->ms_layout-s_head-handle03
                                         handle04 = mo_layout->ms_layout-s_head-handle04 ).

    IF mt_head IS NOT INITIAL.

      
      READ TABLE mt_head WITH KEY layout = mo_layout->ms_layout-s_head-layout ASSIGNING <temp17>.
IF sy-subrc <> 0.
  ASSERT 1 = 0.
ENDIF.

GET REFERENCE OF <temp17> INTO Head.
      IF Head IS BOUND.
        Head->selkz = abap_true.
        RETURN.
      ELSE.
        
        READ TABLE mt_head INDEX 1 ASSIGNING <temp18>.
IF sy-subrc <> 0.
  ASSERT 1 = 0.
ENDIF.
GET REFERENCE OF <temp18> INTO Head.
        Head->selkz = abap_true.
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD init_edit.
    DATA temp1 TYPE xsdboolean.

    mv_layout = mo_layout->ms_layout-s_head-layout.
    mv_descr  = mo_layout->ms_layout-s_head-descr.
    mv_def    = mo_layout->ms_layout-s_head-def.

    
    temp1 = boolc( mo_layout->ms_layout-s_head-uname IS NOT INITIAL ).
    mv_usr    = temp1.

  ENDMETHOD.

  METHOD on_event_layout.

    IF layout IS NOT BOUND.
      RETURN.
    ENDIF.

    IF layout->ms_layout IS INITIAL.
      RETURN.
    ENDIF.

    IF client->get( )-event = layout->ms_layout-s_head-guid.
      client->nav_app_call( factory( layout = layout ) ).
    ENDIF.

  ENDMETHOD.

  METHOD delete_selected_layout.

    DELETE FROM z2ui5_t_11 WHERE guid = Head-guid.

    DELETE FROM z2ui5_t_12 WHERE guid = Head-guid.

    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDMETHOD.

  METHOD get_relative_name_of_table.

    FIELD-SYMBOLS <table> TYPE any.
        DATA typedesc TYPE REF TO cl_abap_typedescr.
            DATA temp19 TYPE REF TO cl_abap_tabledescr.
            DATA tabledesc LIKE temp19.
            DATA temp20 TYPE REF TO cl_abap_structdescr.
            DATA structdesc LIKE temp20.

    TRY.
        
        typedesc = cl_abap_typedescr=>describe_by_data( table ).

        CASE typedesc->kind.

          WHEN cl_abap_typedescr=>kind_table.
            
            temp19 ?= typedesc.
            
            tabledesc = temp19.
            
            temp20 ?= tabledesc->get_table_line_type( ).
            
            structdesc = temp20.
            result = structdesc->get_relative_name( ).
            RETURN.

          WHEN typedesc->kind_ref.

            ASSIGN table->* TO <table>.
            result = get_relative_name_of_table( <table> ).

        ENDCASE.
      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.

  METHOD check_width_unit.

    IF width IS INITIAL.
      RETURN.
    ENDIF.

    result = width.

    IF width CO '0123456789., '.
      REPLACE ALL OCCURRENCES OF ` ` IN result WITH ``.
      result = |{ result }rem|.
    ENDIF.

  ENDMETHOD.

  METHOD render_add_subcolumn.

    DATA lo_popup TYPE REF TO z2ui5_cl_xml_view.
    DATA vbox TYPE REF TO z2ui5_cl_xml_view.
    DATA item TYPE REF TO z2ui5_cl_xml_view.
    DATA temp21 TYPE string_table.
    lo_popup = z2ui5_cl_xml_view=>factory_popup( ).

    lo_popup = lo_popup->dialog( afterclose   = client->_event( 'SUBCOLUMN_CANCEL' )
                                 contentwidth = `50%`
                                 title        = 'Define Sub Coloumns' ).

    
    vbox = lo_popup->vbox( justifycontent = 'SpaceBetween' ).

    
    item = vbox->list( nodata          = `no Subcolumns defined`
                             items           = client->_bind_edit( mo_layout->mt_sub_cols )
                             selectionchange = client->_event( 'SELCHANGE' )
                )->custom_list_item( ).

    
    CLEAR temp21.
    INSERT `${KEY}` INTO TABLE temp21.
    item->combobox( selectedkey = `{FNAME}`
                    items       = client->_bind( mo_layout->mt_comps  )
                   )->item( key  = '{FNAME}'
                            text = '{FNAME} {TLABEL}'
             )->get_parent(
             )->button( icon  = 'sap-icon://decline'
                        type  = `Transparent`
                        press = client->_event( val   = `SUBCOLUMN_DELETE`
                                                t_arg = temp21 ) ).

    lo_popup->buttons(
        )->button( text  = `Delete All`
                   icon  = 'sap-icon://delete'
                   type  = `Transparent`
                   press = client->_event( val = `SUBCOLUMN_DELETE_ALL` )
        )->button( text  = `Add Item`
                   icon  = `sap-icon://add`
                   press = client->_event( val = `SUBCOLUMN_ADD` )
       )->button( text  = 'Cancel'
                  press = client->_event( 'SUBCOLUMN_CANCEL' )
       )->button( text  = 'OK'
                  press = client->_event( 'SUBCOLUMN_CONFIRM' )
                  type  = 'Emphasized' ).

    client->popup_display( lo_popup->stringify( ) ).

  ENDMETHOD.

  METHOD on_event_subcoloumns.
        DATA arg TYPE string_table.
        DATA temp23 TYPE string.
        DATA temp24 TYPE string.
        DATA layout TYPE REF TO z2ui5_cl_layout=>ty_s_positions.
        DATA temp25 LIKE LINE OF mo_layout->mt_sub_cols.
        DATA line LIKE REF TO temp25.
        DATA temp26 TYPE z2ui5_cl_layout=>ty_s_sub_columns.
        DATA lt_event TYPE string_table.
        DATA temp27 LIKE LINE OF lt_event.
        DATA temp28 LIKE sy-tabix.
        DATA temp29 TYPE z2ui5_cl_layout=>ty_t_sub_columns.

    CASE client->get( )-event.

      WHEN 'CALL_SUBCOLUMN'.

        
        arg = client->get( )-t_event_arg.
        
        CLEAR temp23.
        
        READ TABLE arg INTO temp24 INDEX 1.
        IF sy-subrc = 0.
          temp23 = temp24.
        ENDIF.
        mv_active_subcolumn = temp23.

        
        READ TABLE mo_layout->ms_layout-t_layout REFERENCE INTO layout WITH KEY fname = mv_active_subcolumn.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        mo_layout->mt_comps        = mo_layout->ms_layout-t_layout.
        mo_layout->mt_sub_cols     = layout->t_sub_col.
        mo_layout->mt_sub_cols_tmp = mo_layout->mt_sub_cols.

        render_add_subcolumn( ).

      WHEN `SUBCOLUMN_CONFIRM`.

        READ TABLE mo_layout->ms_layout-t_layout REFERENCE INTO layout WITH KEY fname = mv_active_subcolumn.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        CLEAR layout->subcolumn.

        
        
        LOOP AT mo_layout->mt_sub_cols REFERENCE INTO line.
          layout->subcolumn = |{ layout->subcolumn } { line->fname }|.
        ENDLOOP.
        SHIFT layout->subcolumn LEFT DELETING LEADING space.

        layout->t_sub_col = mo_layout->mt_sub_cols.

        client->popup_destroy( ).

        init_edit( ).
        render_edit( ).

      WHEN `SUBCOLUMN_CANCEL`.

        init_edit( ).
        render_edit( ).

      WHEN `SUBCOLUMN_ADD`.
        
        CLEAR temp26.
        temp26-key = z2ui5_cl_util=>uuid_get_c32( ).
        INSERT temp26 INTO TABLE mo_layout->mt_sub_cols.
        client->popup_model_update( ).

      WHEN `SUBCOLUMN_DELETE`.
        
        lt_event = client->get( )-t_event_arg.
        
        
        temp28 = sy-tabix.
        READ TABLE lt_event INDEX 1 INTO temp27.
        sy-tabix = temp28.
        IF sy-subrc <> 0.
          ASSERT 1 = 0.
        ENDIF.
        DELETE mo_layout->mt_sub_cols WHERE key = temp27.
        client->popup_model_update( ).

      WHEN `SUBCOLUMN_DELETE_ALL`.
        
        CLEAR temp29.
        mo_layout->mt_sub_cols = temp29.
        client->popup_model_update( ).

    ENDCASE.

  ENDMETHOD.

  METHOD check_rerender_necessary.
    DATA layout LIKE LINE OF mo_layout->ms_layout-t_layout.
      DATA layout_tmp TYPE z2ui5_cl_layout=>ty_s_positions.

    CLEAR mv_rerender.

    " Sequence and SubCols need rerendering
    
    LOOP AT mo_layout->ms_layout-t_layout INTO layout.

      
      READ TABLE mo_layout->ms_layout_tmp-t_layout INTO layout_tmp
           WITH KEY guid     = layout-guid
                    pos_guid = layout-pos_guid.

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF layout-sequence <> layout_tmp-sequence.
        mv_rerender = abap_true.
        RETURN.
      ENDIF.

      IF layout-t_sub_col <> layout_tmp-t_sub_col.
        mv_rerender = abap_true.
        RETURN.
      ENDIF.

      IF layout-reference_field <> layout_tmp-reference_field.
        mv_rerender = abap_true.
        RETURN.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
