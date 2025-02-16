CLASS z2ui5add_cl_var_selscreen DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES z2ui5_if_app.

    DATA mv_tabname TYPE string.

    CLASS-METHODS factory
      IMPORTING
        val             TYPE z2ui5_cl_util=>ty_t_filter_multi
      RETURNING
        VALUE(r_result) TYPE REF TO z2ui5add_cl_var_selscreen.

    TYPES:
      BEGIN OF ty_s_result,
        t_filter        TYPE z2ui5_cl_util=>ty_t_filter_multi,
        check_confirmed TYPE abap_bool,
      END OF ty_s_result.

    DATA ms_result TYPE ty_s_result.
    DATA mv_popup_name TYPE string.

    METHODS result
      RETURNING
        VALUE(result) TYPE ty_s_result.

    METHODS set_output
      IMPORTING
        client2 TYPE REF TO z2ui5_if_client
        view    TYPE REF TO z2ui5_cl_xml_view.

    METHODS set_output2
      IMPORTING
        client2  TYPE REF TO z2ui5_if_client
        t_filter TYPE z2ui5_cl_util=>ty_t_filter_multi
        view     TYPE REF TO z2ui5_cl_xml_view.

    METHODS on_event
      IMPORTING
        i_client TYPE REF TO z2ui5_if_client.

  PROTECTED SECTION.
    DATA client                 TYPE REF TO z2ui5_if_client.
    DATA check_initialized      TYPE abap_bool.

    METHODS popup_display.
    METHODS init.

  PRIVATE SECTION.
ENDCLASS.



CLASS z2ui5add_cl_var_selscreen IMPLEMENTATION.

  METHOD factory.

    CREATE OBJECT r_result.
    r_result->ms_result-t_filter = val.

  ENDMETHOD.


  METHOD init.

    popup_display( ).

  ENDMETHOD.


  METHOD popup_display.

    DATA lo_popup TYPE REF TO z2ui5_cl_xml_view.
    lo_popup = z2ui5_cl_xml_view=>factory_popup( ).
    lo_popup = lo_popup->dialog( afterclose    = client->_event( 'BUTTON_CANCEL' )
                                 contentheight = `50%`
                                 contentwidth  = `50%`
                                 title         = 'Define Filter Conditons' ).

    set_output(
         client2 = client
         view = lo_popup ).


    client->popup_display( lo_popup->stringify( ) ).

  ENDMETHOD.


  METHOD result.
    result = ms_result.
  ENDMETHOD.


  METHOD z2ui5_if_app~main.
      DATA temp1 TYPE REF TO z2ui5_cl_pop_get_range.
      DATA lo_popup LIKE temp1.
        FIELD-SYMBOLS <tab> TYPE z2ui5_cl_util=>ty_s_filter_multi.

    me->client = client.
    IF check_initialized = abap_false.
      check_initialized = abap_true.
      init( ).
      RETURN.
    ENDIF.

    IF client->get( )-check_on_navigated = abap_true.

      
      temp1 ?= client->get_app( client->get( )-s_draft-id_prev_app ).
      
      lo_popup = temp1.
      IF lo_popup->result( )-check_confirmed = abap_true.
        
        READ TABLE ms_result-t_filter WITH KEY name = mv_popup_name ASSIGNING <tab>.
        <tab>-t_range = lo_popup->result( )-t_range.
        <tab>-t_token = z2ui5_cl_util=>filter_get_token_t_by_range_t( <tab>-t_range ).
      ENDIF.
      popup_display( ).

    ENDIF.

    on_event( client ).

  ENDMETHOD.


  METHOD set_output.

    DATA vbox TYPE REF TO z2ui5_cl_xml_view.
    DATA item TYPE REF TO z2ui5_cl_xml_view.
    DATA grid TYPE REF TO z2ui5_cl_xml_view.
    DATA temp2 TYPE string_table.
    DATA temp4 TYPE string_table.
    DATA temp6 TYPE string_table.
    vbox = view->vbox( height         = `100%`
                                 justifycontent = 'SpaceBetween' ).

    
    item = vbox->list( nodata          = `no conditions defined`
                             items           = client2->_bind( ms_result-t_filter )
                             selectionchange = client2->_event( 'SELCHANGE' )
                )->custom_list_item( ).

    
    grid = item->grid( class = `sapUiSmallMarginTop sapUiSmallMarginBottom sapUiSmallMarginBegin` ).
    grid->text( `{NAME}` ).

    
    CLEAR temp2.
    INSERT `${NAME}` INTO TABLE temp2.
    grid->multi_input( tokens = `{T_TOKEN}`
        enabled               = abap_false
             valuehelprequest = client2->_event( val = `LIST_OPEN` t_arg = temp2 )
            )->tokens(
                 )->token( key      = `{KEY}`
                           text     = `{TEXT}`
                           visible  = `{VISIBLE}`
                           selected = `{SELKZ}`
                           editable = `{EDITABLE}` ).

    
    CLEAR temp4.
    INSERT `${NAME}` INTO TABLE temp4.
    grid->button( text  = `Select`
                  press = client2->_event( val = `LIST_OPEN` t_arg = temp4 ) ).
    
    CLEAR temp6.
    INSERT `${NAME}` INTO TABLE temp6.
    grid->button( icon  = 'sap-icon://delete'
                  type  = `Transparent`
                  text  = `Clear`
                  press = client2->_event( val = `LIST_DELETE` t_arg = temp6 ) ).

    view->buttons(
        )->button( text  = `Clear All`
                   icon  = 'sap-icon://delete'
                   type  = `Transparent`
                   press = client2->_event( val = `POPUP_DELETE_ALL` )
       )->button( text  = 'Cancel'
                  press = client2->_event( 'BUTTON_CANCEL' )
       )->button( text  = 'OK'
                  press = client2->_event( 'BUTTON_CONFIRM' )
                  type  = 'Emphasized' ).

  ENDMETHOD.

  METHOD set_output2.

*    DATA(vbox) = view->vbox( height         = `100%`
*                                 justifycontent = 'SpaceBetween' ).

*    DATA(item) = view->list( "nodata          = `no conditions defined`
*                             items           = client2->_bind( t_filter )
*                             selectionchange = client2->_event( 'SELCHANGE' )
*                )->custom_list_item( ).

    DATA tab TYPE REF TO z2ui5_cl_xml_view.
  DATA cells TYPE REF TO z2ui5_cl_xml_view.
DATA temp8 TYPE string_table.
DATA temp10 TYPE string_table.
DATA temp12 TYPE string_table.
    tab = view->table( "nodata          = `no conditions defined`
                             items           = client2->_bind( t_filter )
                             selectionchange = client2->_event( 'SELCHANGE' )
                ).
*                ->custom_list_item( ).
*    tab->header_toolbar(
*         )->toolbar(
*             )->input(
*         value = client->_bind_edit( mv_tabname )
*         description = `Tablename`
*      )->button(
*                 text  = 'letf side button'
*                 icon  = 'sap-icon://account'
*                 press = client->_event( 'BUTTON_SORT' )

*         ).

    tab->columns(
         )->column(
             )->text( 'Name' )->get_parent(
         )->column(
             )->text( 'Options' )->get_parent(
         )->column(
             )->text( 'Select' )->get_parent(
         )->column(
             )->text( 'Clear' )->get_parent(
              ).
  
  cells =  tab->items( )->column_list_item( )->cells( ).
cells->text( text = `{NAME}` ).

CLEAR temp8.
INSERT `${NAME}` INTO TABLE temp8.
cells->multi_input( tokens = `{T_TOKEN}`
 enabled               = abap_false
      valuehelprequest = client2->_event( val = `LIST_OPEN` t_arg = temp8 )
     )->tokens(
          )->token( key      = `{KEY}`
                    text     = `{TEXT}`
                    visible  = `{VISIBLE}`
                    selected = `{SELKZ}`
                    editable = `{EDITABLE}` ).

CLEAR temp10.
INSERT `${NAME}` INTO TABLE temp10.
cells->button( text  = `Select`
           press = client2->_event( val = `LIST_OPEN` t_arg = temp10 ) ).

CLEAR temp12.
INSERT `${NAME}` INTO TABLE temp12.
cells->button( icon  = 'sap-icon://delete'
           type  = `Transparent`
           text  = `Clear`
           press = client2->_event( val = `LIST_DELETE` t_arg = temp12 )
 ).


*    data(vl) = item->horizontal_layout(
*      EXPORTING
*        class   = 'sapUiContentPadding equalColumns'
*        width   = '100%'
*        enabled =
*        visible =
*        id      =
*      RECEIVING
*        result  =
*    ).
*    DATA(grid) = item->grid( class = `sapUiSmallMarginTop sapUiSmallMarginBottom sapUiSmallMarginBegin` ).
*    DATA(grid) = item. "->hbox( ). "->flex_box( alignItems = 'Start' ). "( class = `sapUiSmallMarginTop sapUiSmallMarginBottom sapUiSmallMarginBegin` ).
*    grid->text( text = `{NAME}` width = `20%` ).

*    grid->multi_input( tokens = `{T_TOKEN}`
*        enabled               = abap_false
*             valuehelprequest = client2->_event( val = `LIST_OPEN` t_arg = VALUE #( ( `${NAME}` ) ) )
*            )->tokens(
*                 )->token( key      = `{KEY}`
*                           text     = `{TEXT}`
*                           visible  = `{VISIBLE}`
*                           selected = `{SELKZ}`
*                           editable = `{EDITABLE}` ).
*
*    grid->button( text  = `Select`
*                  press = client2->_event( val = `LIST_OPEN` t_arg = VALUE #( ( `${NAME}` ) ) ) ).
*    grid->button( icon  = 'sap-icon://delete'
*                  type  = `Transparent`
*                  text  = `Clear`
*                  press = client2->_event( val = `LIST_DELETE` t_arg = VALUE #( ( `${NAME}` ) ) ) ).

    view->hbox(
        )->button( text  = `Clear All`
                   icon  = 'sap-icon://delete'
                   type  = `Transparent`
                   press = client2->_event( val = `POPUP_DELETE_ALL` )
       )->button( text  = 'Cancel'
                  press = client2->_event( 'BUTTON_CANCEL' )
       )->button( text  = 'OK'
                  press = client2->_event( 'BUTTON_CONFIRM' )
                  type  = 'Emphasized' ).

  ENDMETHOD.


  METHOD on_event.

    FIELD-SYMBOLS <tab> TYPE z2ui5_cl_util=>ty_s_filter_multi.
        DATA lt_event TYPE string_table.
        DATA temp1 LIKE LINE OF lt_event.
        DATA temp2 LIKE sy-tabix.
        DATA temp14 LIKE LINE OF lt_event.
        DATA temp15 LIKE sy-tabix.
        DATA ls_sql TYPE z2ui5_cl_util=>ty_s_filter_multi.
        DATA temp3 LIKE LINE OF ms_result-t_filter.
        DATA temp4 LIKE sy-tabix.
        DATA temp16 LIKE LINE OF ms_result-t_filter.
        DATA lr_sql LIKE REF TO temp16.

    CASE i_client->get( )-event.

      WHEN 'LIST_DELETE'.
        
        lt_event = i_client->get( )-t_event_arg.
        
        
        temp2 = sy-tabix.
        READ TABLE lt_event INDEX 1 INTO temp1.
        sy-tabix = temp2.
        IF sy-subrc <> 0.
          ASSERT 1 = 0.
        ENDIF.
        READ TABLE ms_result-t_filter WITH KEY name = temp1 ASSIGNING <tab>.
        CLEAR <tab>-t_token.
        CLEAR <tab>-t_range.
        i_client->popup_model_update( ).

      WHEN 'LIST_OPEN'.
        lt_event = i_client->get( )-t_event_arg.
        
        
        temp15 = sy-tabix.
        READ TABLE lt_event INDEX 1 INTO temp14.
        sy-tabix = temp15.
        IF sy-subrc <> 0.
          ASSERT 1 = 0.
        ENDIF.
        mv_popup_name = temp14.
        
        
        
        temp4 = sy-tabix.
        READ TABLE ms_result-t_filter WITH KEY name = mv_popup_name INTO temp3.
        sy-tabix = temp4.
        IF sy-subrc <> 0.
          ASSERT 1 = 0.
        ENDIF.
        ls_sql = temp3.
        i_client->nav_app_call( z2ui5_cl_pop_get_range=>factory( ls_sql-t_range ) ).

      WHEN `BUTTON_CONFIRM`.
        ms_result-check_confirmed = abap_true.
        i_client->popup_destroy( ).
        i_client->nav_app_leave( i_client->get_app( i_client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN `BUTTON_CANCEL`.
        i_client->popup_destroy( ).
        i_client->nav_app_leave( i_client->get_app( i_client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN `POPUP_DELETE_ALL`.
        
        
        LOOP AT ms_result-t_filter REFERENCE INTO lr_sql.
          CLEAR lr_sql->t_range.
          CLEAR lr_sql->t_token.
        ENDLOOP.
        i_client->popup_model_update( ).

    ENDCASE.

  ENDMETHOD.

ENDCLASS.
