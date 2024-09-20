 CLASS z2ui5_cl_se16 DEFINITION PUBLIC.

   PUBLIC SECTION.

     INTERFACES z2ui5_if_app.

     DATA:
       BEGIN OF ms_sel,
         tabname        TYPE string,
         number_entries TYPE i,
         name_layout    TYPE string,
         check_autoload TYPE abap_bool,
       END OF ms_sel.
     DATA mt_filter_tab TYPE z2ui5_cl_util=>ty_t_filter_multi.

     DATA mv_popup_name TYPE string.
     DATA ms_layout TYPE z2ui5_cl_pop_display_layout=>ty_s_layout.

   PROTECTED SECTION.
     DATA client TYPE REF TO z2ui5_if_client.
     DATA mv_check_initialized TYPE abap_bool.
     METHODS on_event.
     METHODS view_display.
   PRIVATE SECTION.
     DATA: mo_multiselect TYPE REF TO z2ui5add_cl_var_selscreen.

 ENDCLASS.



 CLASS z2ui5_cl_se16 IMPLEMENTATION.


   METHOD on_event.
     TRY.
         CASE client->get( )-event.

           WHEN `UPDATE_TOKENS`.
             DATA(lt_arg) = client->get( )-t_event_arg.
             mt_filter_tab = z2ui5_cl_util=>filter_update_tokens(
                 val    = mt_filter_tab
                 name   = lt_arg[ 1 ] ).
             client->view_model_update( ).

           WHEN 'LIST_OPEN'.
             DATA(lt_event) = client->get( )-t_event_arg.
             mv_popup_name = lt_event[ 1 ].
             DATA(ls_sql) = mt_filter_tab[ name = mv_popup_name ].
             client->nav_app_call( z2ui5_cl_pop_get_range=>factory( ls_sql-t_range ) ).
             RETURN.

           WHEN `POST`.

             DATA lr_table TYPE REF TO data.
             CREATE DATA lr_table TYPE STANDARD TABLE OF (ms_sel-tabname) WITH EMPTY KEY.
             mt_filter_tab = z2ui5_cl_util=>filter_get_multi_by_data( lr_table->* ).
             view_display( ).

           WHEN `PREVIEW_FILTER`.
             client->nav_app_call( z2ui5_cl_pop_get_range_m=>factory( mt_filter_tab ) ).

           WHEN 'BACK'.
             client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).
         ENDCASE.

       CATCH cx_root INTO DATA(x).
         client->message_box_display(
           text              = x->get_text( )
           type              = `error` ).
     ENDTRY.
   ENDMETHOD.


   METHOD view_display.

     DATA(view) = z2ui5_cl_xml_view=>factory( ).

     DATA(page) = view->shell( )->page( id = `page_main`
              title          = 'abap2UI5 - SE16 CLOUD - Start'
              navbuttonpress = client->_event( 'BACK' )
              shownavbutton = xsdbool( client->get( )-s_draft-id_prev_app_stack IS NOT INITIAL )
           ).

     IF ms_sel-tabname IS NOT INITIAL.

       DATA(tab) = page->table( "nodata          = `no conditions defined`
                                id = `tab`
                                items           = client->_bind_edit( mt_filter_tab )
                                selectionchange = client->_event( 'SELCHANGE' ) ).

       DATA(headder) = tab->header_toolbar(
                  )->overflow_toolbar(
                  )->input( value = client->_bind_edit( ms_sel-tabname ) description = `Tablename` width = '40%'  submit = client->_event( `POST` )
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
                )->text( 'Clear' )->get_parent(
                 ).


       DATA(cells) =  tab->items( )->column_list_item( id = `items` )->cells(  ).
       cells->text( text = `{NAME}` ).
       DATA(multi) = cells->multi_input( tokens = `{T_TOKEN}`
                name = '{NAME}'
*        enabled               = abap_false
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

         page->_z2ui5( )->multiinput_ext(
                    multiinputname = client->_bind_edit( val = lr_token->name tab = mt_filter_tab tab_index = lv_tabix )
                    addedtokens      = client->_bind_edit( val = lr_token->t_token_added tab = mt_filter_tab tab_index =  lv_tabix )
                    removedtokens    = client->_bind_edit( val = lr_token->t_token_removed  tab = mt_filter_tab tab_index =  lv_tabix )
                    change           = client->_event( val = 'UPDATE_TOKENS'  t_arg = VALUE #( ( lr_token->name ) ) )
*                    multiinputid     = '__input5-mainView--tab-' &&  z2ui5_cl_util=>c_trim( lv_tabix - 1 )
                      ).

*         EXIT.
       ENDLOOP.
*
     ENDIF.

     page->footer( )->overflow_toolbar(
         )->button( text = `Clear` press = client->_event( `CLEAR` )
*
         )->checkbox( selected = client->_bind_edit( ms_sel-check_autoload )  text = `A`
         )->input( value = client->_bind_edit( ms_sel-name_layout ) width = `20%` description = `Layout`
                )->toolbar_spacer(
         )->input( value = client->_bind_edit( ms_sel-number_entries ) width = `10%` description = `Number`
         )->button( text = `Count Entries` press = client->_event( `GO` )
         )->button( text = `Go` type = `Emphasized` press = client->_event( `GO` ) ).

     client->view_display( view->stringify( ) ).

   ENDMETHOD.


   METHOD z2ui5_if_app~main.

     me->client = client.

     IF mv_check_initialized = abap_false.
       mv_check_initialized = abap_true.
       ms_sel-tabname =  `USR01`.
       ms_sel-number_entries = 100.
       view_display( ).
       RETURN.

     ENDIF.

     IF client->get( )-check_on_navigated = abap_true.
       TRY.
           DATA(lo_popup) = CAST z2ui5_cl_pop_get_range( client->get_app( client->get( )-s_draft-id_prev_app ) ).
           IF lo_popup->result( )-check_confirmed = abap_true.
             ASSIGN mt_filter_tab[ name = mv_popup_name ] TO FIELD-SYMBOL(<tab>).
             <tab>-t_range = lo_popup->result( )-t_range.
             <tab>-t_token = z2ui5_cl_util=>filter_get_token_t_by_range_t( <tab>-t_range ).
             client->view_model_update( ).
           ENDIF.
         CATCH cx_root.
       ENDTRY.
       RETURN.
     ENDIF.

     IF client->get( )-event IS NOT INITIAL.
       on_event( ).
     ENDIF.

   ENDMETHOD.

 ENDCLASS.
