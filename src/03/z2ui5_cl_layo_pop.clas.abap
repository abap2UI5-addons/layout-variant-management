CLASS z2ui5_cl_layo_pop DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    TYPES:
      BEGIN OF ty_s_sorting,
        sorting TYPE string,
        descr   TYPE string,
      END OF ty_s_sorting.
    TYPES ty_t_sorting TYPE STANDARD TABLE OF ty_s_sorting WITH EMPTY KEY.

    TYPES BEGIN OF ty_s_layo.
            INCLUDE TYPE z2ui5_t_11.
    TYPES   selkz  TYPE abap_bool.
    TYPES   active TYPE c length 1.
    TYPES END OF ty_s_layo.
    TYPES ty_t_layo TYPE STANDARD TABLE OF ty_s_layo WITH EMPTY KEY.

    DATA mo_layout      TYPE REF TO z2ui5_cl_layo_manager.
    DATA mt_controls    TYPE z2ui5_cl_layo_manager=>ty_t_controls.
    DATA mt_layout      TYPE z2ui5_cl_layo_manager=>ty_t_positions.

    DATA mt_head        TYPE ty_t_layo.
    DATA mv_descr       TYPE string.
    DATA mv_layout      TYPE string.
    DATA mv_def         TYPE abap_bool.
    DATA mv_usr         TYPE abap_bool.
    DATA mv_open        TYPE abap_bool.
    DATA mv_delete      TYPE abap_bool.
    DATA mt_sorting     TYPE ty_t_sorting.
    DATA mv_active_line TYPE string.
    DATA mv_rerender    TYPE abap_bool.

    DATA mv_xl_label    TYPE int4.
    DATA mv_xl_value    TYPE int4.
    DATA mv_l_label     TYPE int4.
    DATA mv_l_value     TYPE int4.
    DATA mv_m_label     TYPE int4.
    DATA mv_m_value     TYPE int4.
    DATA mv_s_label     TYPE int4.
    DATA mv_s_value     TYPE int4.

    CLASS-METHODS on_event_layout
      IMPORTING
        !client TYPE REF TO z2ui5_if_client
        !layout TYPE REF TO z2ui5_cl_layo_manager.

    CLASS-METHODS render_layout_function
      IMPORTING
        !xml          TYPE REF TO z2ui5_cl_xml_view
        !client       TYPE REF TO z2ui5_if_client
        !layout       TYPE REF TO z2ui5_cl_layo_manager
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_xml_view.

    CLASS-METHODS factory
      IMPORTING
        !layout       TYPE REF TO z2ui5_cl_layo_manager
        open_layout   TYPE abap_bool OPTIONAL
        delete_layout TYPE abap_bool OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_layo_pop.

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

    METHODS on_init.

    METHODS render_open
      IMPORTING
        external_call TYPE abap_bool OPTIONAL.

    METHODS get_selected_layout
      RETURNING
        VALUE(result) TYPE ty_s_layo.

    METHODS delete_selected_layout
      IMPORTING
        !head TYPE ty_s_layo.

    METHODS set_selected_layout
      IMPORTING
        !head TYPE ty_s_layo.

    METHODS check_width_unit
      IMPORTING
        !width        TYPE z2ui5_t_12-width
      RETURNING
        VALUE(result) TYPE z2ui5_t_12-width.

    METHODS on_event_gridlayout.
    METHODS render_add_gridlayout.
    METHODS update_values.

    CLASS-METHODS get_relative_name_of_table
      IMPORTING
        !table        TYPE any
      RETURNING
        VALUE(result) TYPE string.

  PRIVATE SECTION.
    METHODS check_grid_sum
      IMPORTING
        !value        TYPE int4
        !type         TYPE string
      RETURNING
        VALUE(result) TYPE abap_bool.

ENDCLASS.


CLASS z2ui5_cl_layo_pop IMPLEMENTATION.

  METHOD z2ui5_if_app~main.

    me->client = client.

    IF mv_init = abap_false.
      mv_init = abap_true.

      on_init( ).

      init_edit( ).

      render_edit( ).

      client->popup_model_update( ).

    ENDIF.

    update_values( ).

    on_event( ).

  ENDMETHOD.

  METHOD on_init.

    IF mt_controls IS INITIAL.
      mt_controls = z2ui5_cl_layo_manager=>get_controls( ).

      mt_sorting = VALUE #( ( sorting = 'ASCENDING' descr = 'Ascending'  )
                            ( sorting = 'DESCENDING' descr = 'Descending'  )
                            ( sorting = `` descr = ``  ) ).

    ENDIF.

  ENDMETHOD.

  METHOD render_edit.

    DATA(popup) = z2ui5_cl_xml_view=>factory_popup( ).

    DATA(dialog) = popup->dialog( title         = 'Edit Layout'
                                  contentwidth  = '80%'
                                  contentheight = '80%'
                                  afterclose    = client->_event( 'CLOSE' ) ).

    DATA(tab) = dialog->table( growing          = abap_true
                               growingthreshold = '80'
                               sticky           = `ColumnHeaders`
                               items            = client->_bind_edit( mt_layout ) ).

    tab->header_toolbar(
                  )->overflow_toolbar(
                     )->toolbar_spacer(
                    )->search_field(
                        width       = `17.5rem`
                        placeholder = |{ z2ui5_cl_util=>rtti_get_data_element_texts( 'ROLLNAME' )-short }/{ z2ui5_cl_util=>rtti_get_data_element_texts(
                                                                                                                'NAME_FELD' )-short }|
                        livechange  = client->_event( val    = 'BUTTON_SEARCH'
                                                      t_arg  = VALUE #( ( `${$source>/value}` ) )
                                                      s_ctrl = VALUE #( check_allow_multi_req = abap_true ) )  ).

    DATA(list) = tab->column_list_item( ).

    DATA(cells) = list->cells( ).

    DATA(columns) = tab->columns( ).

    DATA(t_layout) = mo_layout->ms_layout-t_layout.

    SORT t_layout BY visible DESCENDING
                     fname ASCENDING.

    DATA(lt_comp) = z2ui5_cl_util=>rtti_get_t_attri_by_any( t_layout ).

    LOOP AT mt_controls REFERENCE INTO DATA(control) WHERE control = mo_layout->ms_layout-s_head-control.

      READ TABLE lt_comp INTO DATA(comp) WITH KEY name = control->attribute.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CASE control->attribute.
        WHEN 'TLABEL'.
          DATA(col) = columns->column( '7rem' )->header( `` ).
          col->text( `Row` ).
        WHEN 'VISIBLE'.
          col = columns->column( '3.5rem' )->header( `` ).
          col->text( 'Visible' ).
        WHEN 'MERGE'.
          col = columns->column( '3.5rem' )->header( `` ).
          col->text( 'Merge' ).
        WHEN 'WIDTH'.
          col = columns->column( `3.5rem` )->header( `` ).
          col->text( 'Width in rem' ).
        WHEN 'SEQUENCE'.
          col = columns->column( `3.5rem` )->header( `` ).
          col->text( 'Sequence' ).
        WHEN 'ALTERNATIVE_TEXT'.
          col = columns->column( `7rem` )->header( `` ).
          col->text( 'Alternative Text' ).
        WHEN 'REFERENCE_FIELD'.
          col = columns->column( `7rem` )->header( `` ).
          col->text( 'Reference Field' ).
        WHEN 'SUBCOLUMN'.
          col = columns->column( `3.5rem` )->header( `` ).
          col->text( 'Subcolumn' ).
        WHEN 'GRID_LAYOUT'.
          col = columns->column( `7rem` )->header( `` ).
          col->text( 'Layout' ).
        WHEN 'NO_LEADING_ZERO'.
          col = columns->column( `5rem` )->header( `` ).
          col->text( 'no Leading Zeros' ).
        WHEN 'SORTING'.
          col = columns->column( `5rem` )->header( `` ).
          col->text( 'Sorting' ).
      ENDCASE.

    ENDLOOP.

    LOOP AT mt_controls REFERENCE INTO control WHERE control = mo_layout->ms_layout-s_head-control.

      READ TABLE lt_comp INTO comp WITH KEY name = control->attribute.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      CASE comp-name.
        WHEN 'TLABEL'.

          cells->text( |\{FNAME\} { cl_abap_char_utilities=>cr_lf } \{TLABEL\} | ).

        WHEN 'VISIBLE' OR 'MERGE'.

          cells->switch( type  = 'AcceptReject'
                         state = |\{{ comp-name }\}| ).

        WHEN 'NO_LEADING_ZERO'.

          cells->vbox( visible = |\{SHOW_NO_ZEROS\}|

          )->switch(   type    = 'AcceptReject'
                     state   = |\{{ comp-name }\}|
                     enabled = |\{SHOW_NO_ZEROS\}| ).

        WHEN 'WIDTH'.

          cells->input( value     = |\{{ comp-name }\}|
                        maxlength = `6`
                        width     = `4rem` ).

        WHEN 'SEQUENCE'.

          cells->input( value     = |\{{ comp-name }\}|
                        maxlength = `3`
                        width     = `3rem`
                        type      = `Number` ).

        WHEN 'ALTERNATIVE_TEXT'.

          cells->input( |\{{ comp-name }\}| ).

        WHEN 'SUBCOLUMN'.

          cells->button( text  = |\{{ comp-name }\}|
                         icon  = `sap-icon://add`
                         width       = '5rem'
                         press = client->_event( val   = 'CALL_SUBCOLUMN'
                                                 t_arg = VALUE #( ( `${FNAME}` ) ) ) ).

        WHEN 'SORTING'.

          cells->combobox( selectedkey = |\{{ comp-name }\}|
                           items       = client->_bind_edit( mt_sorting )
                           width       = '5rem'
                        )->item( key  = '{SORTING}'
                                 text = '{DESCR}' ).

        WHEN 'REFERENCE_FIELD'.

          cells->combobox( selectedkey = |\{{ comp-name }\}|
                           items       = client->_bind_edit( mo_layout->ms_layout-t_layout )
                           width       = '10rem'
                        )->item( key  = '{FNAME}'
                                 text = '{FNAME} - {TLABEL}' ).

        WHEN 'GRID_LAYOUT'.

          cells->button( text  = |\{{ comp-name }\}|
                         icon  = `sap-icon://grid`
                           width       = '5rem'
                         press = client->_event( val   = 'CALL_GRIDLAYOUT'
                                                 t_arg = VALUE #( ( `${FNAME}` ) ) ) ).

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

    FIELD-SYMBOLS <row> TYPE any.

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

        LOOP AT mo_layout->ms_layout-t_layout REFERENCE INTO DATA(layout).
          layout->tlabel           = mo_layout->set_text( layout->* ).
          layout->alternative_text = to_upper( layout->alternative_text ).
          layout->width            = check_width_unit( layout->width ).
        ENDLOOP.

        mo_layout->ms_layout-t_layout = mo_layout->sort_by_seqence( mo_layout->ms_layout-t_layout ).

        check_rerender_necessary( ).

        client->popup_destroy( ).

        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN 'BUTTON_SEARCH'.

        mt_layout = mo_layout->ms_layout-t_layout.

        LOOP AT mt_layout ASSIGNING <row>.
          DATA(lv_row) = ``.

          ASSIGN COMPONENT 'FNAME' OF STRUCTURE <row> TO FIELD-SYMBOL(<fname>).
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.
          ASSIGN COMPONENT 'ROLLNAME' OF STRUCTURE <row> TO FIELD-SYMBOL(<rollname>).
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.
          ASSIGN COMPONENT 'TLABEL' OF STRUCTURE <row> TO FIELD-SYMBOL(<tlabel>).
          IF sy-subrc <> 0.
            EXIT.
          ENDIF.

          lv_row = lv_row && <fname> && <rollname> && <tlabel>.

          IF lv_row NS client->get_event_arg( 1 ).
            DELETE mt_layout.
          ENDIF.

        ENDLOOP.

        client->popup_model_update( ).

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

        on_event_gridlayout( ).

    ENDCASE.

  ENDMETHOD.

  METHOD factory.

    result = NEW #( ).

    result->mo_layout = layout.

    result->mt_layout = layout->ms_layout-t_layout.

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

    DATA(popup) = z2ui5_cl_xml_view=>factory_popup( ).

    DATA(dialog) = popup->dialog( title        = 'Save'
                                  contentwidth = '80%'
                                  afterclose   = client->_event( 'SAVE_CLOSE' ) ).

    DATA(form) = dialog->content( )->simple_form( title                   = 'Layout'
                                                  editable                = abap_true
                                                  labelspanxl             = `4`
                                                  labelspanl              = `4`
                                                  labelspanm              = `4`
                                                  labelspans              = `4`
                                                  adjustlabelspan         = abap_false
                                                  emptyspanxl             = `0`
                                                  emptyspanl              = `0`
                                                  emptyspanm              = `0`
                                                  emptyspans              = `0`
                                                  columnsxl               = `2`
                                                  columnsl                = `2`
                                                  columnsm                = `2`
                                                  singlecontainerfullsize = `true` ).

    form->toolbar( )->title( 'Layout' ).

    form->content( 'form'
                           )->label( 'Layout'
                           )->input( value     = client->_bind_edit( mv_layout )
                                     maxlength = '10'
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

    DATA position  TYPE z2ui5_t_12.
    DATA positions TYPE STANDARD TABLE OF z2ui5_t_12 WITH EMPTY KEY.

    IF mv_layout IS INITIAL.
      client->message_toast_display( 'Layoutname missing.' ).
      RETURN.
    ENDIF.

    IF mv_usr = abap_true.
      DATA(user) = sy-uname.
    ENDIF.

    DATA(head) = VALUE z2ui5_t_11( guid     = mo_layout->ms_layout-s_head-guid
                                   layout   = mv_layout
                                   control  = mo_layout->ms_layout-s_head-control
                                   handle01 = mo_layout->ms_layout-s_head-handle01
                                   handle02 = mo_layout->ms_layout-s_head-handle02
                                   handle03 = mo_layout->ms_layout-s_head-handle03
                                   handle04 = mo_layout->ms_layout-s_head-handle04
                                   descr    = mv_descr
                                   def      = mv_def
                                   uname    = user ).

    SELECT SINGLE guid,
                  layout,
                  control,
                  handle01,
                  handle02,
                  handle03,
                  handle04
      FROM z2ui5_t_11
      WHERE guid = @head-guid
      INTO @DATA(head_db).

    IF sy-subrc = 0.

      " found entry.
      IF     head_db-layout   = head-layout
         AND head_db-control  = head-control
         AND head_db-handle01 = head-handle01
         AND head_db-handle02 = head-handle02
         AND head_db-handle03 = head-handle03
         AND head_db-handle04 = head-handle04.
        " Save Changes

      ELSE.

        " Save New Layout - new Guid
        TRY.
            head-guid = cl_system_uuid=>create_uuid_c32_static( ).
          CATCH cx_root.
        ENDTRY.

      ENDIF.

    ENDIF.

    LOOP AT mo_layout->ms_layout-t_layout REFERENCE INTO DATA(r_layout).
      r_layout->guid = head-guid.

      MOVE-CORRESPONDING r_layout->* TO position.
      position-width = check_width_unit( position-width ).

      " only visible/ref_fields/SubCols should be saved.
      IF r_layout->visible = abap_true.
        APPEND position TO positions.
        CONTINUE.
      ENDIF.

      IF line_exists( mo_layout->ms_layout-t_layout[ reference_field = r_layout->fname ] ).
        APPEND position TO positions.
        CONTINUE.
      ENDIF.

      LOOP AT mo_layout->ms_layout-t_layout INTO DATA(layout) WHERE t_sub_col IS NOT INITIAL.
        IF line_exists( layout-t_sub_col[ fname = r_layout->fname ] ).
          APPEND position TO positions.
          CONTINUE.
        ENDIF.
      ENDLOOP.

    ENDLOOP.

    MODIFY z2ui5_t_11 FROM @head.

    IF sy-subrc = 0.

      DELETE FROM z2ui5_t_12 WHERE guid = @head-guid.

      MODIFY z2ui5_t_12 FROM TABLE @positions.

      IF sy-subrc = 0.

        COMMIT WORK AND WAIT.

        client->message_toast_display( 'Data saved.' ).

      ENDIF.
    ENDIF.

    " Check Default
    UPDATE z2ui5_t_11 SET def = @abap_false       WHERE control   = @mo_layout->ms_layout-s_head-control
                                                    AND handle01  = @mo_layout->ms_layout-s_head-handle01
                                                    AND handle02  = @mo_layout->ms_layout-s_head-handle02
                                                    AND handle03  = @mo_layout->ms_layout-s_head-handle03
                                                    AND handle04  = @mo_layout->ms_layout-s_head-handle04
                                                    AND def       = @abap_true
                                                    AND uname     = @user
                                                    AND guid     <> @head-guid.
    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDMETHOD.

  METHOD render_delete.

    DATA(popup) = z2ui5_cl_xml_view=>factory_popup( ).

    DATA(dialog) = popup->dialog( title         = 'Delete Layout'
                                  contentwidth  = '80%'
                                  contentheight = '80%'
                                  afterclose    = client->_event( 'CLOSE' ) ).

    dialog->table( mode  = 'SingleSelectLeft'
                   items = client->_bind_edit( mt_head )
                )->columns(
                    )->column( )->text( 'Layout' )->get_parent(
                    )->column( )->text( 'Description' )->get_parent(
                    )->column( )->text( 'Active'
                    )->get_parent( )->get_parent(
                )->items(
                    )->column_list_item( selected = '{SELKZ}'
                        )->cells(
                            )->text( '{LAYOUT}'
                            )->text( '{DESCR}'
                            )->text( '{ACTIVE}' ).

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

    DATA(popup) = z2ui5_cl_xml_view=>factory_popup( ).

    DATA(dialog) = popup->dialog( title         = 'Select Layout'
                                  contentwidth  = '80%'
                                  contentheight = '80%'
                                  afterclose    = client->_event( 'CLOSE' ) ).

    dialog->table( mode  = 'SingleSelectLeft'
                   items = client->_bind_edit( mt_head )
                )->columns(
                    )->column( )->text( 'Layout' )->get_parent(
                    )->column( )->text( 'Description' )->get_parent(
                    )->column( )->text( 'Active' )->get_parent(
                    )->column( )->text( 'Default' )->get_parent(
                    )->get_parent(
                )->items(
                    )->column_list_item( selected = '{SELKZ}'
                        )->cells(
                            )->text( '{LAYOUT}'
                            )->text( '{DESCR}'
                            )->text( '{ACTIVE}'
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

    result = VALUE #( mt_head[ selkz = abap_true ] OPTIONAL ).

  ENDMETHOD.

  METHOD set_selected_layout.

    mo_layout = z2ui5_cl_layo_manager=>factory_by_guid( layout_guid = head-guid
                                                        t_comps     = mo_layout->ms_layout-t_layout ).

  ENDMETHOD.

  METHOD get_layouts.

    mt_head = mo_layout->select_layouts( control  = mo_layout->ms_layout-s_head-control
                                         handle01 = mo_layout->ms_layout-s_head-handle01
                                         handle02 = mo_layout->ms_layout-s_head-handle02
                                         handle03 = mo_layout->ms_layout-s_head-handle03
                                         handle04 = mo_layout->ms_layout-s_head-handle04 ).

    IF mt_head IS INITIAL.
      RETURN.
    ENDIF.

    DATA(head) = REF #( mt_head[ layout = mo_layout->ms_layout-s_head-layout ] OPTIONAL ).
    IF head IS BOUND.
      head->selkz  = abap_true.
      head->active = abap_true.
      RETURN.
    ELSE.
      head = REF #( mt_head[ 1 ] OPTIONAL ).
      head->selkz = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD init_edit.

    mv_layout = mo_layout->ms_layout-s_head-layout.
    mv_descr  = mo_layout->ms_layout-s_head-descr.
    mv_def    = mo_layout->ms_layout-s_head-def.

    mv_usr    = xsdbool( mo_layout->ms_layout-s_head-uname IS NOT INITIAL ).

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

    DELETE FROM z2ui5_t_11 WHERE guid = @head-guid.

    DELETE FROM z2ui5_t_12 WHERE guid = @head-guid.

    IF sy-subrc = 0.
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDMETHOD.

  METHOD get_relative_name_of_table.

    FIELD-SYMBOLS <table> TYPE any.

    TRY.
        DATA(typedesc) = cl_abap_typedescr=>describe_by_data( table ).

        CASE typedesc->kind.

          WHEN cl_abap_typedescr=>kind_table.
            DATA(tabledesc) = CAST cl_abap_tabledescr( typedesc ).
            DATA(structdesc) = CAST cl_abap_structdescr( tabledesc->get_table_line_type( ) ).
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

    IF width CA '.'.
      FIND REGEX '([0-9]{1,4}.[0-9]{1})' IN width SUBMATCHES result.
    ELSE.
      FIND REGEX '([0-9]{1,4})' IN width SUBMATCHES result.
    ENDIF.

    IF result CO '0123456789. '.
      result = |{ result }rem|.
    ENDIF.

  ENDMETHOD.

  METHOD render_add_subcolumn.

    DATA(lo_popup) = z2ui5_cl_xml_view=>factory_popup( ).

    lo_popup = lo_popup->dialog( afterclose   = client->_event( 'SUBCOLUMN_CANCEL' )
                                 contentwidth = `50%`
                                 title        = 'Define Sub Coloumns' ).

    DATA(vbox) = lo_popup->vbox( justifycontent = 'SpaceBetween' ).

    DATA(item) = vbox->list( nodata          = `no Subcolumns defined`
                             items           = client->_bind_edit( mo_layout->mt_sub_cols )
                             selectionchange = client->_event( 'SELCHANGE' )
                )->custom_list_item( ).

    item->combobox( selectedkey = `{FNAME}`
                    items       = client->_bind( mo_layout->mt_comps  )
                   )->item( key  = '{FNAME}'
                            text = '{FNAME} {TLABEL}'
             )->get_parent(
             )->button( icon  = 'sap-icon://decline'
                        type  = `Transparent`
                        press = client->_event( val   = `SUBCOLUMN_DELETE`
                                                t_arg = VALUE #( ( `${KEY}` ) ) ) ).

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

    CASE client->get( )-event.

      WHEN 'CALL_SUBCOLUMN'.

        DATA(arg) = client->get( )-t_event_arg.
        mv_active_line = VALUE #( arg[ 1 ] OPTIONAL ).

        READ TABLE mt_layout REFERENCE INTO DATA(layout) WITH KEY fname = mv_active_line.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        mo_layout->mt_comps    = mo_layout->ms_layout-t_layout.   " Components for DropDownList
        mo_layout->mt_sub_cols = layout->t_sub_col.               " Defined Sub Col´s

        render_add_subcolumn( ).

      WHEN `SUBCOLUMN_CONFIRM`.

        READ TABLE mt_layout REFERENCE INTO layout WITH KEY fname = mv_active_line.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        CLEAR layout->subcolumn.

        LOOP AT mo_layout->mt_sub_cols REFERENCE INTO DATA(line).
          layout->subcolumn = |{ layout->subcolumn } { line->fname }|.
        ENDLOOP.
        SHIFT layout->subcolumn LEFT DELETING LEADING space.

        layout->t_sub_col = mo_layout->mt_sub_cols.

        client->popup_destroy( ).

        update_values( ).
        mt_layout = mo_layout->ms_layout-t_layout.

        init_edit( ).
        render_edit( ).

      WHEN `SUBCOLUMN_CANCEL`.

        update_values( ).
        mt_layout = mo_layout->ms_layout-t_layout.

        init_edit( ).
        render_edit( ).

      WHEN `SUBCOLUMN_ADD`.
        INSERT VALUE #( key = z2ui5_cl_util=>uuid_get_c32( ) ) INTO TABLE mo_layout->mt_sub_cols.
        client->popup_model_update( ).

      WHEN `SUBCOLUMN_DELETE`.
        DATA(lt_event) = client->get( )-t_event_arg.
        DELETE mo_layout->mt_sub_cols WHERE key = lt_event[ 1 ].
        client->popup_model_update( ).

      WHEN `SUBCOLUMN_DELETE_ALL`.
        mo_layout->mt_sub_cols = VALUE #( ).
        client->popup_model_update( ).

    ENDCASE.

  ENDMETHOD.

  METHOD check_rerender_necessary.

    CLEAR mv_rerender.

    " Sequence and SubCols need rerendering
    LOOP AT mo_layout->ms_layout-t_layout INTO DATA(layout).

      READ TABLE mo_layout->ms_layout_tmp-t_layout INTO DATA(layout_tmp)
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

      IF layout-no_leading_zero <> layout_tmp-no_leading_zero.
        mv_rerender = abap_true.
        RETURN.
      ENDIF.

      IF layout-no_leading_zero <> layout_tmp-no_leading_zero.
        mv_rerender = abap_true.
        RETURN.
      ENDIF.

      IF    layout-grid_value_xl <> layout_tmp-grid_value_xl
         OR layout-grid_value_l  <> layout_tmp-grid_value_l
         OR layout-grid_value_m  <> layout_tmp-grid_value_m
         OR layout-grid_value_s  <> layout_tmp-grid_value_s.
        mv_rerender = abap_true.
        RETURN.
      ENDIF.

      IF    layout-grid_label_xl <> layout_tmp-grid_label_xl
         OR layout-grid_label_l  <> layout_tmp-grid_label_l
         OR layout-grid_label_m  <> layout_tmp-grid_label_m
         OR layout-grid_label_s  <> layout_tmp-grid_label_s.
        mv_rerender = abap_true.
        RETURN.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD on_event_gridlayout.

    CASE client->get( )-event.

      WHEN 'CALL_GRIDLAYOUT'.

        DATA(arg) = client->get( )-t_event_arg.
        mv_active_line = VALUE #( arg[ 1 ] OPTIONAL ).

        READ TABLE mo_layout->ms_layout-t_layout REFERENCE INTO DATA(layout) WITH KEY fname = mv_active_line.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        mv_xl_value = layout->grid_value_xl.
        mv_l_value = layout->grid_value_l.
        mv_m_value = layout->grid_value_m.
        mv_s_value = layout->grid_value_s.

        mv_xl_label = layout->grid_label_xl.
        mv_l_label  = layout->grid_label_l.
        mv_m_label  = layout->grid_label_m.
        mv_s_label  = layout->grid_label_s.

        render_add_gridlayout( ).

      WHEN `GRIDLAYOUT_CONFIRM`.

        IF    check_grid_sum( value = mv_xl_label + mv_xl_value
                              type  = 'XL' )
           OR check_grid_sum( value = mv_l_label + mv_l_value
                              type  = 'L' )
           OR check_grid_sum( value = mv_m_label + mv_m_value
                              type  = 'M' )
           OR check_grid_sum( value = mv_s_label + mv_s_value
                              type  = 'S' ) = abap_true.

        ELSE.

          READ TABLE mo_layout->ms_layout-t_layout REFERENCE INTO layout WITH KEY fname = mv_active_line.
          IF sy-subrc <> 0.
            RETURN.
          ENDIF.

          layout->grid_value_xl = mv_xl_value.
          layout->grid_value_l  = mv_l_value.
          layout->grid_value_m  = mv_m_value.
          layout->grid_value_s  = mv_s_value.

          layout->grid_label_xl = mv_xl_label.
          layout->grid_label_l  = mv_l_label.
          layout->grid_label_m  = mv_m_label.
          layout->grid_label_s  = mv_s_label.

          init_edit( ).
          render_edit( ).

        ENDIF.

      WHEN `GRIDLAYOUT_CANCEL`.

        init_edit( ).
        render_edit( ).

    ENDCASE.

  ENDMETHOD.

  METHOD check_grid_sum.

    IF ( value ) > 12.
 DATA(msg) = z2ui5_cl_util=>msg_get_by_msg(
           id     = '/scmtms/common'
           no     = '154'
           v1     = '12'
       ).
      result = abap_true.
    ENDIF.

    client->message_toast_display( msg-text ).

  ENDMETHOD.

  METHOD render_add_gridlayout.

    TYPES: BEGIN OF ty_s_col,
             col TYPE c length 2,
           END OF ty_s_col.

    DATA t_col TYPE STANDARD TABLE OF ty_s_col.

    t_col = VALUE #( ( col = 1  )
                     ( col = 2  )
                     ( col = 3  )
                     ( col = 4  )
                     ( col = 5  )
                     ( col = 6  )
                     ( col = 7  )
                     ( col = 8  )
                     ( col = 9  )
                     ( col = 10  )
                     ( col = 11  )
                     ( col = 12  ) ).

    DATA(lo_popup) = z2ui5_cl_xml_view=>factory_popup( ).

    lo_popup = lo_popup->dialog( afterclose   = client->_event( 'GRIDLAYOUT_CANCEL' )
                                 contentwidth = `140px`
                                 title        = 'Grid Layout' ).

    DATA(form) = lo_popup->simple_form( editable = abap_true
                                        title    = 'Define Label and Value Span' )->content( ns = `form` ).

    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA(line) = form->label( text = 'XL'
                 )->combobox( selectedkey = client->_bind_edit( mv_xl_label )
                              width       = `7rem`
                              items       = client->_bind_local( t_col  )
                   )->item( key  = '{COL}'
                            text = '{COL} Label Span' ).

    form->combobox( selectedkey = client->_bind_edit( mv_xl_value )
                    width       = `7rem`
                    items       = client->_bind_local( t_col  )
)->item( key  = '{COL}'
         text = '{COL} Value Span' ).

    line = form->label( text = 'L'
                  )->combobox( selectedkey = client->_bind_edit( mv_l_label )
                               width       = `7rem`
                               items       = client->_bind_local( t_col  )
                    )->item( key  = '{COL}'
                             text = '{COL} Label Span' ).
    form->combobox( selectedkey = client->_bind_edit( mv_l_value )

                    items       = client->_bind_local( t_col  )
)->item( key  = '{COL}'
         text = '{COL} Value Span' ).

    line = form->label( text = 'M'
                  )->combobox( selectedkey = client->_bind_edit( mv_m_label )
                               width       = `7rem`
                               items       = client->_bind_local( t_col  )
                    )->item( key  = '{COL}'
                             text = '{COL} Label Span' ).

    form->combobox( selectedkey = client->_bind_edit( mv_m_value )
                    width       = `7rem`
                    items       = client->_bind_local( t_col  )
      )->item( key  = '{COL}'
               text = '{COL} Value Span' ).

    line = form->label( text = 'S'
                  )->combobox( selectedkey = client->_bind_edit( mv_s_label )
                               width       = `7rem`
                               items       = client->_bind_local( t_col  )
                    )->item( key  = '{COL}'
                             text = '{COL} Label Span' ).

    form->combobox( selectedkey = client->_bind_edit( mv_s_value )
                    width       = `7rem`
                    items       = client->_bind_local( t_col  )
      )->item( key  = '{COL}'
               text = '{COL} Value Span' ).

    lo_popup->buttons(
       )->button( text  = 'Cancel'
                  press = client->_event( 'GRIDLAYOUT_CANCEL' )
       )->button( text  = 'OK'
                  press = client->_event( 'GRIDLAYOUT_CONFIRM' )
                  type  = 'Emphasized' ).

    client->popup_display( lo_popup->stringify( ) ).

  ENDMETHOD.

  METHOD update_values.

    LOOP AT mo_layout->ms_layout-t_layout REFERENCE INTO DATA(line).

      DATA(layout) = VALUE #( mt_layout[ pos_guid = line->pos_guid ] OPTIONAL ).
      IF layout IS NOT INITIAL.
        line->* = layout.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
