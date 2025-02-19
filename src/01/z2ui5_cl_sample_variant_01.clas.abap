CLASS z2ui5_cl_sample_variant_01 DEFINITION PUBLIC.

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

    DATA mv_tabname TYPE string.
    DATA mr_table   TYPE REF TO data.
    DATA mt_filter  TYPE z2ui5_cl_util=>ty_t_filter_multi.

  PROTECTED SECTION.
    DATA client               TYPE REF TO z2ui5_if_client.
    DATA mv_check_initialized TYPE abap_bool.

    METHODS on_event.
    METHODS view_display.
    METHODS set_data.

  PRIVATE SECTION.
    DATA mo_multiselect TYPE REF TO z2ui5add_cl_var_selscreen.
ENDCLASS.


CLASS z2ui5_cl_sample_variant_01 IMPLEMENTATION.

  METHOD on_event.

    CASE client->get( )-event.

      WHEN 'LIST_OPEN'.
        mo_multiselect = z2ui5add_cl_var_selscreen=>factory( mt_filter ).
        mo_multiselect->on_event( client ).
        RETURN.

      WHEN `BUTTON_START`.
        set_data( ).
        client->view_model_update( ).

      WHEN `PREVIEW_FILTER`.
        client->nav_app_call( z2ui5_cl_pop_get_range_m=>factory( mt_filter ) ).

      WHEN 'BACK'.
        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).
    ENDCASE.

  ENDMETHOD.

  METHOD set_data.

*     DATA lt_range TYPE rs_t_rscedst.
*
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
*
*     DATA lv_result TYPE string.
**     data lt_where type rsdmd_t_where.
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

    " select from (mv_tabname)
    "  fields
    "  -
    "  where (lv_result)
    "  into table @mr_table->*.

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

    view = view->shell( )->page( id             = `page_main`
                                 title          = 'abap2UI5 - Select-Options'
                                 navbuttonpress = client->_event( 'BACK' )
                                 shownavbutton  = xsdbool( client->get( )-s_draft-id_prev_app_stack IS NOT INITIAL ) ).

    DATA(vbox) = view->vbox( )->input( value       = client->_bind_edit( mv_tabname )
                                       description = `Tablename` ).

    vbox->button( text  = `GO`
                  press = client->_event( 'TAB' ) ).

    DATA(lo_multiselect) = z2ui5add_cl_var_selscreen=>factory( mt_filter ).

    lo_multiselect->set_output2( t_filter = mt_filter
                                 client2  = client
                                 view     = vbox ).

    DATA(tab) = vbox->table( items = client->_bind( val = mr_table->* )
           )->header_toolbar(
             )->overflow_toolbar(
                 )->toolbar_spacer(
*                 )->button( text = `Filter` press = client->_event( `PREVIEW_FILTER` ) icon = `sap-icon://filter`
           )->button( text  = `Go`
                      press = client->_event( `BUTTON_START` )
                      type  = `Emphasized`
            )->get_parent( )->get_parent( ).

    DATA(lo_columns) = tab->columns( ).
    lo_columns->column( )->text( text = `Product` ).
    lo_columns->column( )->text( text = `Date` ).
    lo_columns->column( )->text( text = `Name` ).
    lo_columns->column( )->text( text = `Location` ).
    lo_columns->column( )->text( text = `Quantity` ).

    DATA(lo_cells) = tab->items( )->column_list_item( ).
    lo_cells->text( `{PRODUCT}` ).
    lo_cells->text( `{CREATE_DATE}` ).
    lo_cells->text( `{CREATE_BY}` ).
    lo_cells->text( `{STORAGE_LOCATION}` ).
    lo_cells->text( `{QUANTITY}` ).

    client->view_display( view->stringify( ) ).

  ENDMETHOD.

  METHOD z2ui5_if_app~main.

    me->client = client.

    IF mv_check_initialized = abap_false.
      mv_check_initialized = abap_true.
      mv_tabname = `T100`.

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
