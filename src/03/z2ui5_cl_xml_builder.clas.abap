CLASS z2ui5_cl_xml_builder DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    CLASS-METHODS xml_build_table
      IMPORTING
        i_data         TYPE REF TO data
        i_xml          TYPE REF TO z2ui5_cl_xml_view
        i_client       TYPE REF TO z2ui5_if_client
        i_layout       TYPE REF TO z2ui5_cl_layout
        i_search_value TYPE REF TO data OPTIONAL
        i_Sel_mode     TYPE string      OPTIONAL
        i_sel_BIND_TO  TYPE string      OPTIONAL
        i_col_type     TYPE string      OPTIONAL
        i_col_bind_to  TYPE string      OPTIONAL.

    CLASS-METHODS xml_build_simple_form
      IMPORTING
        i_DATA   TYPE REF TO data
        i_xml    TYPE REF TO z2ui5_cl_xml_view
        i_client TYPE REF TO z2ui5_if_client
        i_layout TYPE REF TO z2ui5_cl_layout
        i_title  TYPE string OPTIONAL.

ENDCLASS.


CLASS z2ui5_cl_xml_builder IMPLEMENTATION.

  METHOD xml_build_simple_form.

    DATA form TYPE REF TO z2ui5_cl_xml_view.
    DATA temp1 LIKE LINE OF i_layout->ms_layout-t_layout.
    DATA layout LIKE REF TO temp1.
      DATA lv_index LIKE sy-tabix.
      FIELD-SYMBOLS <data> TYPE data.
      FIELD-SYMBOLS <value> TYPE any.
      DATA line TYPE REF TO z2ui5_cl_xml_view.
        FIELD-SYMBOLS <ref_value> TYPE any.
        DATA temp2 TYPE z2ui5_cl_layout=>ty_s_positions.
        DATA temp3 TYPE z2ui5_cl_layout=>ty_s_positions.
        DATA ref_field LIKE temp2.
    form = i_xml->simple_form( title                   = i_title
                                     editable                = abap_true
                                     layout                  = `ResponsiveGridLayout`
                                     labelspans              = '3'
                                     labelspanm              = '3'
                                     labelspanl              = '3'
                                     labelspanxl             = '3'
                                     adjustlabelspan         = abap_false
                                     emptyspanxl             = '4'
                                     emptyspanl              = '4'
                                     emptyspanm              = '2'
                                     emptyspans              = '0'
                                     columnsxl               = '1'
                                     columnsl                = '1'
                                     columnsm                = '1'
                                     singlecontainerfullsize = abap_false
                              )->content( ns = `form` ).

    
    
    LOOP AT  i_layout->ms_layout-t_layout REFERENCE INTO layout.

      
      lv_index = sy-tabix.

      
      ASSIGN i_data->* TO <data>.
      
      ASSIGN COMPONENT layout->fname OF STRUCTURE <data> TO <value>.
      IF <value> IS NOT ASSIGNED.
        CONTINUE.
      ENDIF.

      
      line = form->label( text = i_client->_bind( val       = layout->tlabel
                                                        tab       = i_layout->ms_layout-t_layout
                                                        tab_index = lv_index )

             )->input( visible = i_client->_bind( val       = layout->visible
                                                  tab       = i_layout->ms_layout-t_layout
                                                  tab_index = lv_index )
                       value   = i_client->_bind( <value> )
                       enabled = abap_false
                       width   = i_client->_bind( val       = layout->width
                                                  tab       = i_layout->ms_layout-t_layout
                                                  tab_index = lv_index ) ).

      IF layout->reference_field IS NOT INITIAL.

        ASSIGN i_data->* TO <data>.
        
        ASSIGN COMPONENT layout->reference_field OF STRUCTURE <data> TO <ref_value>.
        IF <ref_value> IS NOT ASSIGNED.
          CONTINUE.
        ENDIF.

        
        CLEAR temp2.
        
        READ TABLE i_layout->ms_layout-t_layout INTO temp3 WITH KEY fname = layout->reference_field.
        IF sy-subrc = 0.
          temp2 = temp3.
        ENDIF.
        
        ref_field = temp2.

        line->input( visible = i_client->_bind( val       = layout->visible
                                                tab       = i_layout->ms_layout-t_layout
                                                tab_index = lv_index )
                     value   = i_client->_bind( <ref_value> )
                     enabled = abap_false
                     width   = ref_field-width ).

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD xml_build_table.

    FIELD-SYMBOLS <data> TYPE data.
    DATA temp4 TYPE string.
    DATA table TYPE REF TO z2ui5_cl_xml_view.
    DATA toolbar TYPE REF TO z2ui5_cl_xml_view.
      FIELD-SYMBOLS <search_value> TYPE data.
    DATA columns TYPE REF TO z2ui5_cl_xml_view.
    DATA temp5 LIKE LINE OF i_layout->ms_layout-t_layout.
    DATA layout LIKE REF TO temp5.
      DATA lv_index LIKE sy-tabix.
    DATA temp6 TYPE string_table.
    DATA temp1 TYPE string.
    DATA temp2 TYPE string.
    DATA temp3 TYPE string.
    DATA column_list_item TYPE REF TO z2ui5_cl_xml_view.
    DATA cells TYPE REF TO z2ui5_cl_xml_view.
        DATA sub_col TYPE string.
        DATA index TYPE i.
        DATA subcol LIKE LINE OF layout->t_sub_col.
          DATA line TYPE z2ui5_cl_layout=>ty_s_positions.
            DATA Column TYPE string.
    ASSIGN i_data->* TO <data>.
    
    IF i_sel_mode = space.
      temp4 = `None`.
    ELSE.
      temp4 = i_sel_mode.
    ENDIF.
    
    table = i_xml->table( width           = 'auto'
                                mode            = temp4
                                items           = i_client->_bind_edit( val = <data> )
                                selectionChange = i_client->_event( 'SELECTION_CHANGE' ) ).

    
    toolbar = table->header_toolbar(
                   )->overflow_toolbar(
                   )->toolbar_spacer( ).

    IF i_search_value IS SUPPLIED.
      
      ASSIGN i_search_value->* TO <search_value>.
      toolbar->search_field( value  = i_client->_bind_edit( <search_value> )
                             search = i_client->_event( 'SEARCH' )
                             change = i_client->_event( 'SEARCH' )
                             id     = `SEARCH`
                             width  = '17.5rem' ).
    ENDIF.

    z2ui5_cl_pop_layout=>render_layout_function( client = i_client
                                                         xml    = toolbar
                                                         layout = i_layout ).

    
    columns = table->columns( ).

    
    
    LOOP AT i_layout->ms_layout-t_layout REFERENCE INTO layout.
      
      lv_index = sy-tabix.

      columns->column( visible         = i_client->_bind( val       = layout->visible
                                                          tab       = i_layout->ms_layout-t_layout
                                                          tab_index = lv_index )
                       halign          = i_client->_bind( val       = layout->halign
                                                          tab       = i_layout->ms_layout-t_layout
                                                          tab_index = lv_index )
                       importance      = i_client->_bind( val       = layout->importance
                                                          tab       = i_layout->ms_layout-t_layout
                                                          tab_index = lv_index )
                       mergeduplicates = i_client->_bind( val       = layout->merge
                                                          tab       = i_layout->ms_layout-t_layout
                                                          tab_index = lv_index )
                       width           = i_client->_bind( val       = layout->width
                                                          tab       = i_layout->ms_layout-t_layout
                                                          tab_index = lv_index )

       )->text( layout->tlabel ).

    ENDLOOP.

    
    CLEAR temp6.
    
    IF i_col_BIND_TO = space.
      temp1 = ``.
    ELSE.
      temp1 = |$\{{ i_col_BIND_TO }\}|.
    ENDIF.
    INSERT temp1 INTO TABLE temp6.
    
    IF i_sel_BIND_TO = space.
      temp2 = ``.
    ELSE.
      temp2 = |\{{ i_sel_BIND_TO }\}|.
    ENDIF.
    
    IF i_col_type = space.
      temp3 = `Inactive`.
    ELSE.
      temp3 = i_col_type.
    ENDIF.
    
    column_list_item = columns->get_parent( )->items(
                                       )->column_list_item(
                                           valign   = 'Middle'
                                           selected = temp2
                                           type     = temp3
                                           press    = i_client->_event(
                                               val   = 'ROW_SELECT'
                                               t_arg = temp6 ) ).

    
    cells = column_list_item->cells( ).

    LOOP AT i_layout->ms_layout-t_layout REFERENCE INTO layout.

      IF layout->t_sub_col IS NOT INITIAL.

        
        sub_col = ``.
        
        index = 0.

        
        LOOP AT layout->t_sub_col INTO subcol.

          index = index + 1.

          
          READ TABLE i_layout->ms_layout-t_layout INTO line WITH KEY fname = subcol-fname.

          IF line-reference_field IS INITIAL.
            
            Column = |{ line-tlabel }: \{{ subcol-fname }\}|.
          ELSE.
            column = |{ line-tlabel }: \{{ subcol-fname }\} \{{ line-reference_field }\}|.
          ENDIF.

          IF index = 1.
            sub_col = column.
          ELSE.
            sub_col = |{ sub_col }{ cl_abap_char_utilities=>cr_lf }{ column }|.
          ENDIF.
        ENDLOOP.

        IF layout->reference_field IS NOT INITIAL.
          cells->object_identifier( title = |\{{ layout->fname }\} \{{ layout->reference_field }\}|
                                    text  = sub_col ).
        ELSE.
          cells->object_identifier( title = |\{{ layout->fname }\}|
                                    text  = sub_col ).
        ENDIF.

      ELSE.
        IF layout->reference_field IS NOT INITIAL.
          cells->object_identifier( text = |\{{ layout->fname }\} \{{ layout->reference_field }\}| ).
        ELSE.
          cells->object_identifier( text = |\{{ layout->fname }\}| ).
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
