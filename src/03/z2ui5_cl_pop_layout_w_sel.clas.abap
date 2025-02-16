CLASS z2ui5_cl_pop_layout_w_sel DEFINITION
  PUBLIC FINAL
  CREATE PROTECTED.

  PUBLIC SECTION.
    INTERFACES if_serializable_object.
    INTERFACES z2ui5_if_app.

    TYPES:
      BEGIN OF ty_s_result,
        row             TYPE REF TO data,
        check_confirmed TYPE abap_bool,
      END OF ty_s_result.

    DATA ms_result       TYPE ty_s_result.
    DATA mr_tab          TYPE REF TO data.
    DATA mr_out          TYPE REF TO data.
    DATA mr_out_tmp      TYPE REF TO data.

    DATA mo_layout       TYPE REF TO z2ui5_cl_layout.
    DATA mv_search_value TYPE string.

    CLASS-METHODS factory
      IMPORTING
        i_tab              TYPE STANDARD TABLE
        i_title            TYPE clike                   OPTIONAL
        i_sort_field       TYPE clike                   OPTIONAL
        i_descending       TYPE abap_bool               OPTIONAL
        i_contentwidth     TYPE clike                   OPTIONAL
        i_contentheight    TYPE clike                   OPTIONAL
        i_growingthreshold TYPE clike                   OPTIONAL
        i_handle01         TYPE z2ui5_cl_layout=>handle OPTIONAL
        i_handle02         TYPE z2ui5_cl_layout=>handle OPTIONAL
        i_handle03         TYPE z2ui5_cl_layout=>handle OPTIONAL
        i_handle04         TYPE z2ui5_cl_layout=>handle OPTIONAL
      RETURNING
        VALUE(r_result)    TYPE REF TO z2ui5_cl_pop_layout_w_sel.

    METHODS result
      RETURNING
        VALUE(result) TYPE ty_s_result.

  PROTECTED SECTION.
    DATA check_initialized TYPE abap_bool.
    DATA client            TYPE REF TO z2ui5_if_client.
    DATA title             TYPE string.
    DATA sort_field        TYPE string.
    DATA content_width     TYPE string.
    DATA content_height    TYPE string.
    DATA growing_threshold TYPE string.
    DATA descending        TYPE abap_bool.

    METHODS on_event.
    METHODS Render_main.
    METHODS set_output_table.

    METHODS on_event_search.

    METHODS get_comp
      RETURNING
        VALUE(result) TYPE abap_component_tab.

  PRIVATE SECTION.
    METHODS set_row_id.
    METHODS confirm.
    METHODS on_after_layout.

ENDCLASS.



CLASS z2ui5_cl_pop_layout_w_sel IMPLEMENTATION.


  METHOD factory.

    CREATE OBJECT r_result.
    r_result->title             = i_title.
    r_result->sort_field        = i_sort_field.
    r_result->descending        = i_descending.
    r_result->content_height    = i_contentheight.
    r_result->content_width     = i_contentwidth.
    r_result->growing_threshold = i_growingthreshold.

    r_result->mr_tab            = z2ui5_cl_util=>conv_copy_ref_data( i_tab ).

    CREATE DATA r_result->ms_result-row LIKE LINE OF i_tab.

    r_result->mo_layout = z2ui5_cl_layout=>factory( data     = r_result->mr_tab
                                                    control  = z2ui5_cl_layout=>m_table
                                                    handle01 = i_handle01
                                                    handle02 = i_handle02
                                                    handle03 = i_handle03
                                                    handle04 = i_handle04 ).

  ENDMETHOD.


  METHOD Render_main.

    DATA popup TYPE REF TO z2ui5_cl_xml_view.
    DATA temp1 LIKE REF TO mv_search_value.
    popup = z2ui5_cl_xml_view=>factory_popup( )->dialog( title      = title
                                                               afterclose = client->_event( 'CANCEL' )  ).

    
    GET REFERENCE OF mv_search_value INTO temp1.
z2ui5_cl_xml_builder=>xml_build_table( i_data         = mr_out
                                           i_xml          = popup
                                           i_client       = client
                                           i_layout       = mo_layout
                                           i_search_value = temp1
                                           i_col_type     = 'Navigation'
                                           i_col_bind_to  = 'ZZROW_ID' ).

    client->popup_display( popup->stringify( ) ).

  ENDMETHOD.


  METHOD z2ui5_if_app~main.

    me->client = client.

    IF check_initialized = abap_false.
      check_initialized = abap_true.

      set_output_table( ).

      Render_main( ).

      RETURN.

    ENDIF.

    on_event( ).

    on_after_layout( ).

  ENDMETHOD.


  METHOD on_event.

    CASE client->get( )-event.

      WHEN 'ROW_SELECT'.

        confirm( ).

        client->popup_destroy( ).
        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN 'CANCEL'.
        client->popup_destroy( ).
        client->nav_app_leave( client->get_app( client->get( )-s_draft-id_prev_app_stack ) ).

      WHEN 'SEARCH'.
        on_event_search( ).
        client->popup_model_update( ).

      WHEN OTHERS.

        z2ui5_cl_pop_layout=>on_event_layout( client = client
                                                      layout = mo_layout ).

    ENDCASE.

  ENDMETHOD.


  METHOD on_after_layout.
        DATA temp2 TYPE REF TO z2ui5_cl_pop_layout.
        DATA app LIKE temp2.

    IF client->get( )-check_on_navigated = abap_false.
      RETURN.
    ENDIF.

    TRY.

        
        temp2 ?= client->get_app( client->get( )-s_draft-id_prev_app ).
        
        app = temp2.

        mo_layout = app->mo_layout.

        Render_main( ).

      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.


  METHOD confirm.

    FIELD-SYMBOLS <tab> TYPE STANDARD TABLE.
    DATA t_arg TYPE string_table.
    DATA row_clicked LIKE LINE OF t_arg.
    DATA temp1 LIKE LINE OF t_arg.
    DATA temp2 LIKE sy-tabix.
    FIELD-SYMBOLS <line> TYPE ANY.
      FIELD-SYMBOLS <row_id> TYPE any.
        FIELD-SYMBOLS <any> TYPE data.

    ASSIGN mr_out->* TO <tab>.
    
    t_arg = client->get( )-t_event_arg.
    
    
    
    temp2 = sy-tabix.
    READ TABLE t_arg INDEX 1 INTO temp1.
    sy-tabix = temp2.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    row_clicked = temp1.

    
    LOOP AT <tab> ASSIGNING <line>.

      
      ASSIGN COMPONENT 'ZZROW_ID' OF STRUCTURE <line> TO <row_id>.

      IF <row_id> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      IF <row_id> = row_clicked.
        
        ASSIGN ms_result-row->* TO <any>.
        MOVE-CORRESPONDING <line> TO <any>.
        EXIT.
      ENDIF.

    ENDLOOP.

    ms_result-check_confirmed = abap_true.

  ENDMETHOD.


  METHOD result.

    result = ms_result.

  ENDMETHOD.


  METHOD set_output_table.

    DATA t_comp TYPE abap_component_tab.
        DATA new_struct_desc TYPE REF TO cl_abap_structdescr.
        DATA new_table_desc TYPE REF TO cl_abap_tabledescr.
    FIELD-SYMBOLS <in> TYPE data.
    FIELD-SYMBOLS <out> TYPE data.
    FIELD-SYMBOLS <out_tmp> TYPE data.
    t_comp = get_comp( ).
    TRY.

        
        new_struct_desc = cl_abap_structdescr=>create( t_comp ).

        
        new_table_desc = cl_abap_tabledescr=>create( p_line_type  = new_struct_desc
                                                           p_table_kind = cl_abap_tabledescr=>tablekind_std ).

        CREATE DATA mr_out     TYPE HANDLE new_table_desc.
        CREATE DATA mr_out_tmp TYPE HANDLE new_table_desc.

      CATCH cx_root.

    ENDTRY.

    
    ASSIGN mr_tab->* TO <in>.
    
    ASSIGN mr_out->* TO <out>.

    z2ui5_cl_util=>itab_corresponding(
      EXPORTING
        val = <in>
      CHANGING
        tab = <out> ).

    set_row_id( ).

    
    ASSIGN mr_out_tmp->* TO <out_tmp>.
    <out_tmp> = <out>.

  ENDMETHOD.


  METHOD set_row_id.
    FIELD-SYMBOLS <tab>  TYPE STANDARD TABLE.
    FIELD-SYMBOLS <line> TYPE any.
      FIELD-SYMBOLS <row> TYPE any.

    ASSIGN mr_out->* TO <tab>.

    LOOP AT <tab> ASSIGNING <line>.

      
      ASSIGN COMPONENT 'ZZROW_ID' OF STRUCTURE <line> TO <row>.
      IF <row> IS ASSIGNED.
        <row> = sy-tabix.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_comp.
    DATA index TYPE int4.
        DATA comp TYPE abap_component_tab.
        DATA temp3 LIKE sy-subrc.
        DATA temp1 TYPE xsdboolean.
          DATA temp4 TYPE cl_abap_structdescr=>component_table.
          DATA temp5 LIKE LINE OF temp4.
          DATA temp6 TYPE REF TO cl_abap_datadescr.

*    DATA selkz TYPE abap_bool.

    TRY.

        
        comp = z2ui5_cl_util=>rtti_get_t_attri_by_any( mr_tab ).

        
        READ TABLE comp WITH KEY name = 'ZZROW_ID' TRANSPORTING NO FIELDS.
        temp3 = sy-subrc.
        
        temp1 = boolc( temp3 = 0 ).
        IF temp1 = abap_false.
          
          CLEAR temp4.
          
          temp5-name = 'ZZROW_ID'.
          
          temp6 ?= cl_abap_datadescr=>describe_by_data( index ).
          temp5-type = temp6.
          INSERT temp5 INTO TABLE temp4.
          APPEND LINES OF temp4 TO result.
        ENDIF.
*        IF xsdbool( line_exists( comp[ name = 'SELKZ' ] ) ) = abap_false.
*          APPEND LINES OF VALUE cl_abap_structdescr=>component_table(
*                                    ( name = 'SELKZ'
*                                      type = CAST #( cl_abap_datadescr=>describe_by_data( selkz ) ) ) ) TO result.
*
*        ENDIF.

        APPEND LINES OF comp TO result.

      CATCH cx_root.
    ENDTRY.
  ENDMETHOD.


  METHOD on_event_search.

    FIELD-SYMBOLS <tab>     TYPE STANDARD TABLE.
    FIELD-SYMBOLS <tab_tmp> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <f_row> TYPE ANY.
      DATA lv_row TYPE string.
      DATA lv_index TYPE i.
        FIELD-SYMBOLS <field> TYPE any.

    ASSIGN mr_out->* TO <tab>.
    ASSIGN mr_out_tmp->* TO <tab_tmp>.

    IF <tab_tmp> IS NOT INITIAL.
      <tab> = <tab_tmp>.
    ENDIF.

    IF mv_search_value IS INITIAL.
      RETURN.
    ENDIF.

    
    LOOP AT <tab> ASSIGNING <f_row>.
      
      lv_row = ``.
      
      lv_index = 1.
      DO.
        
        ASSIGN COMPONENT lv_index OF STRUCTURE <f_row> TO <field>.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        lv_row = lv_row && <field>.
        lv_index = lv_index + 1.
      ENDDO.

      IF lv_row NS mv_search_value.
        DELETE <tab>.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
