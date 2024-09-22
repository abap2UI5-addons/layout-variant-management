CLASS z2ui5_cl_selscreen DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_serializable_object.
    DATA mv_tabname TYPE string.
    DATA mv_popup_name TYPE string.

    METHODS paint
      IMPORTING
        view   TYPE REF TO z2ui5_cl_xml_view
        client TYPE REF TO z2ui5_if_client.

    METHODS on_event
      IMPORTING
        client TYPE REF TO z2ui5_if_client.

    DATA mt_filter_tab TYPE z2ui5_cl_util=>ty_t_filter_multi.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS z2ui5_cl_selscreen IMPLEMENTATION.

  METHOD on_event.

    CASE client->get( )-event.

      WHEN `UPDATE_TOKENS`.
        mt_filter_tab = z2ui5_cl_util=>filter_update_tokens(
            val    = mt_filter_tab
            name   = client->get_event_arg( ) ).
        client->view_model_update( ).

      WHEN 'LIST_OPEN'.
        DATA(ls_sql) = mt_filter_tab[ name = client->get_event_arg( ) ].
        client->nav_app_call( z2ui5_cl_pop_get_range=>factory( ls_sql-t_range ) ).
        RETURN.

    ENDCASE.

  ENDMETHOD.

  METHOD paint.

    DATA lr_table TYPE REF TO data.
    CREATE DATA lr_table TYPE STANDARD TABLE OF (mv_tabname) WITH EMPTY KEY.
    mt_filter_tab = z2ui5_cl_util=>filter_get_multi_by_data( lr_table->* ).

    DATA(tab) = view->table(
                             id = `tab`
                             items           = client->_bind_edit( mt_filter_tab )
                             selectionchange = client->_event( 'SELCHANGE' ) ).

    DATA(headder) = tab->header_toolbar(
               )->overflow_toolbar(
               )->input( value = client->_bind_edit( mv_tabname ) description = `Tablename` width = '40%'  submit = client->_event( `POST` )
                 )->toolbar_spacer(
                   )->button(  text = `Load Select Options` press = client->_event( `POST` )   ).

    tab->columns(
         )->column(
             )->text( 'Name' )->get_parent(
         )->column(
             )->text( 'Options' )->get_parent(
         )->column(
             )->text( 'Select' )->get_parent(
         )->column(
             )->text( 'Clear' )->get_parent( ).

    DATA(cells) =  tab->items( )->column_list_item( id = `items` )->cells(  ).
    cells->text( text = `{NAME}` ).
    DATA(multi) = cells->multi_input( tokens = `{T_TOKEN}`
             name = '{NAME}'
          valuehelprequest = client->_event( val = `LIST_OPEN` t_arg = VALUE #( ( `${NAME}` ) ) )
         ).
    multi->tokens(
         )->token(

                   key      = `{KEY}`
                   text     = `{TEXT}`
                   visible  = `{VISIBLE}`
                   selected = `{SELKZ}`
                   editable = `{EDITABLE}` ).

    cells->button( text  = `Select`
               press = client->_event( val = `LIST_OPEN` t_arg = VALUE #( ( `${NAME}` ) ) ) ).
    cells->button( icon  = 'sap-icon://delete'
               type  = `Transparent`
               text  = `Clear`
               press = client->_event( val = `LIST_DELETE` t_arg = VALUE #( ( `${NAME}` ) ) )
     ).

    LOOP AT mt_filter_tab REFERENCE INTO DATA(lr_token).
      DATA(lv_tabix) = sy-tabix.

      view->_z2ui5( )->multiinput_ext(
                 multiinputname = client->_bind_edit( val = lr_token->name tab = mt_filter_tab tab_index = lv_tabix )
                 addedtokens      = client->_bind_edit( val = lr_token->t_token_added tab = mt_filter_tab tab_index =  lv_tabix )
                 removedtokens    = client->_bind_edit( val = lr_token->t_token_removed  tab = mt_filter_tab tab_index =  lv_tabix )
                 change           = client->_event( val = 'UPDATE_TOKENS'  t_arg = VALUE #( ( lr_token->name ) ) )
                   ).

    ENDLOOP.


  ENDMETHOD.
ENDCLASS.
