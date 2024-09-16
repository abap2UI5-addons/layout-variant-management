 CLASS z2ui5_cl_se16 DEFINITION PUBLIC.

   PUBLIC SECTION.

     INTERFACES z2ui5_if_app.

     TYPES:
       BEGIN OF ty_s_tab,
         selkz            TYPE abap_bool,
         product          TYPE string,
         create_date      TYPE string,
         create_by        TYPE string,
         storage_location TYPE string,
         quantity         TYPE i,
       END OF ty_s_tab.
     TYPES ty_t_table TYPE STANDARD TABLE OF ty_s_tab WITH EMPTY KEY.

     DATA ms_layout TYPE z2ui5_cl_pop_display_layout=>ty_s_layout.

     DATA mv_tabname TYPE string.
     DATA mr_table TYPE REF TO data.
     DATA mt_filter TYPE z2ui5_cl_util=>ty_t_filter_multi.

   PROTECTED SECTION.
     DATA client TYPE REF TO z2ui5_if_client.
     DATA mv_check_initialized TYPE abap_bool.
     METHODS on_event.

     METHODS z_build_where_clause
*        importing
*        it_range_table type rs_t_rscedst
        returning
        value(result) type string.

     METHODS view_display.
     METHODS set_data.

   PRIVATE SECTION.
     DATA: mo_multiselect TYPE REF TO z2ui5add_cl_var_selscreen.
 ENDCLASS.



 CLASS z2ui5_cl_se16 IMPLEMENTATION.


   METHOD on_event.


     IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*   WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
     ENDIF.

     CASE client->get( )-event.

       WHEN 'LIST_OPEN'.
         mo_multiselect = z2ui5add_cl_var_selscreen=>factory( mt_filter ).
         mo_multiselect->on_event( client ).
         RETURN.

       WHEN `BUTTON_START`.
         set_data( ).
         view_display( ).

       WHEN `PREVIEW_FILTER`.
         client->nav_app_call( z2ui5_cl_pop_get_range_m=>factory( mt_filter ) ).

       WHEN 'BACK'.
         client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).
     ENDCASE.

   ENDMETHOD.

method z_build_where_clause.
*
*
*  DATA: lt_where_clause TYPE TABLE OF string,
*        lv_where_clause TYPE string,
*        lv_temp_clause TYPE string,
*        lv_operator TYPE string.
*
*  LOOP AT it_range_table INTO DATA(ls_range).
*
*    CLEAR: lv_temp_clause, lv_operator.
*
*    " Determine the SQL operator based on the 'option' field
*    CASE ls_range-option.
*      WHEN 'EQ'. lv_operator = ' = '.
*      WHEN 'NE'. lv_operator = ' <> '.
*      WHEN 'GT'. lv_operator = ' > '.
*      WHEN 'LT'. lv_operator = ' < '.
*      WHEN 'GE'. lv_operator = ' >= '.
*      WHEN 'LE'. lv_operator = ' <= '.
*      WHEN 'BT'. lv_operator = ' BETWEEN '.
*      WHEN 'NB'. lv_operator = ' NOT BETWEEN '.
*      " Add more options if necessary
*      WHEN OTHERS.
*        CONTINUE. " Skip unsupported options
*    ENDCASE.
*
*    " Build clause based on 'sign'
*    IF ls_range-sign = 'I'.
*      IF ls_range-option = 'BT' OR ls_range-option = 'NB'.
*        lv_temp_clause = |{ ls_range-fnam } { lv_operator } '{ ls_range-low }' AND '{ ls_range-high }'|.
*      ELSE.
*        lv_temp_clause = |{ ls_range-fnam } { lv_operator } '{ ls_range-low }'|.
*      ENDIF.
*    ELSEIF ls_range-sign = 'E'.
*      " Exclude condition (Negate the condition)
*      IF ls_range-option = 'BT' OR ls_range-option = 'NB'.
*        lv_temp_clause = |{ ls_range-fnam } NOT { lv_operator } '{ ls_range-low }' AND '{ ls_range-high }'|.
*      ELSE.
*        lv_temp_clause = |{ ls_range-fnam } NOT { lv_operator } '{ ls_range-low }'|.
*      ENDIF.
*    ENDIF.
*
*    " Append to the where clause table
*    APPEND lv_temp_clause TO lt_where_clause.
*
*  ENDLOOP.
*
*  " Join the WHERE conditions with 'AND' to create the full clause
*  lv_where_clause = REDUCE string( INIT x = ''
*                         FOR wa IN lt_where_clause
*                         NEXT x = x && COND string( WHEN x IS NOT INITIAL THEN ' AND ' ELSE '' ) && wa ).
*
*  " Export the final WHERE clause
*  result = lv_where_clause.

endmethod.


   METHOD set_data.

*     DATA lt_range TYPE rs_t_rscedst.

*     LOOP AT mt_filter INTO DATA(ls_filter).
*       LOOP AT ls_filter-t_range INTO DATA(ls_range).
*
*         INSERT VALUE #(
*             fnam = ls_filter-name
*             sign = ls_range-sign
*             option = ls_range-option
*             low = ls_range-low
*             high = ls_range-high
*          ) INTO TABLE lt_range.
*
*       ENDLOOP.
*     ENDLOOP.

     DATA lv_result TYPE string.

*     lv_result = z_build_where_clause( lt_range ).
*     data lt_where type rsdmd_t_where.
*     CALL FUNCTION 'RSDS_RANGE_TO_WHERE'
*       EXPORTING
*         i_t_range = lt_range
**        i_th_range     =
**        i_r_renderer   =
*       IMPORTING
*         e_where   = lv_result
**        e_t_where = lt_where
**  EXCEPTIONS
**        internal_error = 1
**        others    = 2
*       .


     SELECT FROM (mv_tabname)
      FIELDS
      *
      WHERE (lv_result)
      INTO TABLE @mr_table->*
      UP TO 100 ROWS.

*     mt_table = VALUE #(
*         ( product = 'table'    create_date = `01.01.2023` create_by = `Peter` storage_location = `AREA_001` quantity = 400 )
*         ( product = 'chair'    create_date = `01.01.2023` create_by = `Peter` storage_location = `AREA_001` quantity = 400 )
*         ( product = 'sofa'     create_date = `01.01.2023` create_by = `Peter` storage_location = `AREA_001` quantity = 400 )
*         ( product = 'computer' create_date = `01.01.2023` create_by = `Peter` storage_location = `AREA_001` quantity = 400 )
*         ( product = 'oven'     create_date = `01.01.2023` create_by = `Peter` storage_location = `AREA_001` quantity = 400 )
*         ( product = 'table2'   create_date = `01.01.2023` create_by = `Peter` storage_location = `AREA_001` quantity = 400 )
*     ).

*     z2ui5_cl_util=>filter_itab(
*       EXPORTING
*         filter = mt_filter
*       CHANGING
*         val    = mt_table
*     ).

   ENDMETHOD.


   METHOD view_display.

     DATA(view) = z2ui5_cl_xml_view=>factory( ).

     view = view->shell( )->page( id = `page_main`
              title          = 'abap2UI5 - Select-Options'
              navbuttonpress = client->_event( 'BACK' )
              shownavbutton = xsdbool( client->get( )-s_draft-id_prev_app_stack IS NOT INITIAL )
           ).

*     DATA(vbox) = view->vbox( )->input(
*        value = client->_bind_edit( mv_tabname )
*        description = `Tablename`
*     ).

*     vbox->button( text = `GO` press = client->_event( 'TAB' ) ).

    data(cont) = view->scroll_container( height   = '30%'
                                              vertical = abap_true
                                               ).

     DATA(lo_multiselect) = z2ui5add_cl_var_selscreen=>factory( mt_filter ).

     lo_multiselect->set_output2(
         t_filter = mt_filter
       client2 = client
       view    = cont
     ).

*     DATA(tab) = vbox->table(
*         items = client->_bind( val = mr_table->* )
*            )->header_toolbar(
*              )->overflow_toolbar(
*                  )->toolbar_spacer(
**                 )->button( text = `Filter` press = client->_event( `PREVIEW_FILTER` ) icon = `sap-icon://filter`
*            )->button(  text = `Go` press = client->_event( `BUTTON_START` ) type = `Emphasized`
*             )->get_parent( )->get_parent( ).

     FIELD-SYMBOLS <tab> TYPE data.
     ASSIGN mr_table->* TO <tab>.

* data(cont2) = vbox->scroll_container( height   = '30%'
*                                               vertical = abap_true ).

     DATA(table) = view->table( growing = 'true'
                                width   = 'auto'
                                items   = client->_bind_edit( val = <tab> ) ).

     " TODO: variable is assigned but never used (ABAP cleaner)
     DATA(headder) = table->header_toolbar(
                )->overflow_toolbar(
                  )->toolbar_spacer(
                    )->button(  text = `Go` press = client->_event( `BUTTON_START` ) type = `Emphasized` ).

     headder = z2ui5_cl_pop_display_layout=>render_layout_function( xml    = headder
                                                               client = client ).

     DATA(columns) = table->columns( ).

    if <tab> is not initial.
         ms_layout = z2ui5_cl_pop_display_layout=>init_layout( control  = z2ui5_cl_pop_display_layout=>m_table
                                                     data     = mr_table
*                                                     handle01 = CONV #( class )
*                                                     handle02 = CONV #( 'z2ui5_t_01' )
*                                                     handle03 = ''
*                                                     handle04 = ''
                                                      ).
    endif.

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

     client->view_display( view->stringify( ) ).

   ENDMETHOD.


   METHOD z2ui5_if_app~main.

     me->client = client.

     IF mv_check_initialized = abap_false.
       mv_check_initialized = abap_true.
       mv_tabname = `USR01`.

       CREATE DATA mr_table TYPE STANDARD TABLE OF (mv_tabname) WITH EMPTY KEY.
       mt_filter = z2ui5_cl_util=>filter_get_multi_by_data( mr_table->* ).
*       DELETE mt_filter WHERE name = `SELKZ`.
       view_display( ).
       RETURN.
     ENDIF.

     IF client->get( )-check_on_navigated = abap_true.
       TRY.
           DATA(lo_popup) = CAST z2ui5_cl_pop_get_range( client->get_app( client->get( )-s_draft-id_prev_app ) ).
           IF lo_popup->result( )-check_confirmed = abap_true.
             ASSIGN mt_filter[ name = mo_multiselect->mv_popup_name ] TO FIELD-SYMBOL(<tab>).
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
