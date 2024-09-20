 CLASS z2ui5_cl_se16_tab_view DEFINITION PUBLIC.

   PUBLIC SECTION.

     INTERFACES z2ui5_if_app.

     DATA mo_variant TYPE REF TO z2ui5add_cl_var_db_api.
     data mo_layout type ref to z2ui5_cl_pop_display_layout.

     DATA ms_layout TYPE z2ui5_cl_pop_display_layout=>ty_s_layout.
     DATA mv_tabname TYPE string VALUE 'T100'.
     DATA mr_table TYPE REF TO data.
     DATA mt_filter TYPE z2ui5_cl_util=>ty_t_filter_multi.

   PROTECTED SECTION.
     DATA client TYPE REF TO z2ui5_if_client.
     DATA mv_check_initialized TYPE abap_bool.
     METHODS on_event.
     METHODS view_display.
     METHODS set_data.

   PRIVATE SECTION.
     DATA: mo_multiselect TYPE REF TO z2ui5add_cl_var_selscreen.
 ENDCLASS.



 CLASS z2ui5_cl_se16_tab_view IMPLEMENTATION.


   METHOD on_event.

     CASE client->get( )-event.

       WHEN `BUTTON_START`.
         set_data( ).
         view_display( ).

       WHEN 'BACK'.
         client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).
     ENDCASE.

   ENDMETHOD.

   METHOD set_data.

    "Variante lesen
    "SQL Select machen

     DATA lv_result TYPE string.

     SELECT FROM (mv_tabname)
      FIELDS
      *
      WHERE (lv_result)
      INTO TABLE @mr_table->*
      UP TO 100 ROWS.


   ENDMETHOD.


   METHOD view_display.

     DATA(view) = z2ui5_cl_xml_view=>factory( ).

     view = view->shell( )->page( id = `page_main`
              title          = 'abap2UI5 - SE16-CLOUD - ' && mv_tabname
              navbuttonpress = client->_event( 'BACK' )
              floatingfooter = abap_true
              shownavbutton = xsdbool( client->get( )-s_draft-id_prev_app_stack IS NOT INITIAL )
           ).

     FIELD-SYMBOLS <tab> TYPE data.
     ASSIGN mr_table->* TO <tab>.


     DATA(table) = view->table( "growing = 'true'
                                width   = 'auto'
                                items   = client->_bind_edit( val = <tab> ) ).

     DATA(headder) = table->header_toolbar(
                )->overflow_toolbar(
                  )->toolbar_spacer(
                    )->button(  text = `Go` press = client->_event( `BUTTON_START` ) type = `Emphasized` ).

     headder = z2ui5_cl_pop_display_layout=>render_layout_function( xml    = headder
                                                               client = client ).

     DATA(columns) = table->columns( ).

     IF <tab> IS NOT INITIAL.
       ms_layout = z2ui5_cl_pop_display_layout=>init_layout( control  = z2ui5_cl_pop_display_layout=>m_table
                                                   data     = mr_table
*                                                     handle01 = CONV #( class )
*                                                     handle02 = CONV #( 'z2ui5_t_01' )
*                                                     handle03 = ''
*                                                     handle04 = ''
                                                    ).
     ENDIF.

     LOOP AT ms_layout-t_layout REFERENCE INTO DATA(layout).
       DATA(lv_index) = sy-tabix.

       columns->column( visible         = client->_bind( val       = layout->visible
                                                         tab       = ms_layout-t_layout
                                                         tab_index = lv_index )
                        halign          = client->_bind( val       = layout->halign
                                                         tab       = ms_layout-t_layout
                                                         tab_index = lv_index )
                        importance      = client->_bind( val       = layout->importance
                                                         tab       = ms_layout-t_layout
                                                         tab_index = lv_index )
                        mergeduplicates = client->_bind( val       = layout->merge
                                                         tab       = ms_layout-t_layout
                                                         tab_index = lv_index )
                        width           = client->_bind( val       = layout->width
                                                         tab       = ms_layout-t_layout
                                                         tab_index = lv_index )

        )->text( layout->tlabel ).

     ENDLOOP.

     DATA(cells) = columns->get_parent( )->items(
                                        )->column_list_item( valign = 'Middle'
                                                             type   = 'Navigation'

                                        )->cells( ).

     " Subcolumns require new rendering....
     LOOP AT ms_layout-t_layout REFERENCE INTO layout.

       IF layout->t_sub_col IS NOT INITIAL.

         DATA(sub_col) = ``.
         DATA(index) = 0.
         LOOP AT layout->t_sub_col INTO DATA(subcol).

           index = index + 1.

           READ TABLE ms_layout-t_layout INTO DATA(line) WITH KEY fname = subcol-fname.

           IF index = 1.
             sub_col = |{ line-tlabel }: \{{ subcol-fname }\}|.
           ELSE.
             sub_col = |{ sub_col }{ cl_abap_char_utilities=>cr_lf } { line-tlabel }: \{{ subcol-fname }\}|.
           ENDIF.

         ENDLOOP.

         cells->object_identifier( title = |\{{ layout->fname }\}|
                                   text  = sub_col ).

       ELSE.
         cells->object_identifier( text = |\{{ layout->fname }\}| ).
       ENDIF.
     ENDLOOP.

     view->footer( )->overflow_toolbar(
         )->button( text = `Back` press = client->_event( `BACK` )
         )->toolbar_spacer(
         )->button( text = `Refresh` press = client->_event( `REFRESH` ) ).

     client->view_display( view->stringify( ) ).

   ENDMETHOD.


   METHOD z2ui5_if_app~main.

     me->client = client.

     IF mv_check_initialized = abap_false.
       mv_check_initialized = abap_true.
       CREATE DATA mr_table TYPE STANDARD TABLE OF (mv_tabname) WITH EMPTY KEY.
       set_data( ).
       view_display( ).
       RETURN.

     ENDIF.

     IF client->get( )-event IS NOT INITIAL.
       on_event( ).
     ENDIF.

   ENDMETHOD.
 ENDCLASS.
