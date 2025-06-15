CLASS z2ui5_cl_layo_manager DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_serializable_object.

    TYPES handle  TYPE c LENGTH 40.
    TYPES control TYPE c LENGTH 15.

    TYPES:
      BEGIN OF ty_s_controls,
        attribute TYPE string,
        control   TYPE control,
        active    TYPE abap_bool,
        index     TYPE int4,
      END OF ty_s_controls.
    TYPES ty_t_controls TYPE STANDARD TABLE OF ty_s_controls WITH EMPTY KEY.

    CLASS-DATA ui_table      TYPE control VALUE 'UI.TABLE' ##NO_TEXT.
    CLASS-DATA m_table       TYPE control VALUE 'M.TABLE' ##NO_TEXT.
    CLASS-DATA ui_simpleform TYPE control VALUE 'UI.SIMPLEFORM' ##NO_TEXT.
    CLASS-DATA others        TYPE control VALUE '' ##NO_TEXT.

    TYPES ty_s_Head TYPE z2ui5_t_11.
    TYPES ty_t_head TYPE STANDARD TABLE OF ty_s_head WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_s_sub_columns,
        key   TYPE string,
        fname TYPE string,
      END OF ty_s_sub_columns.
    TYPES ty_t_sub_columns TYPE STANDARD TABLE OF ty_s_sub_columns WITH EMPTY KEY.

    TYPES  BEGIN OF ty_s_positions.
    INCLUDE TYPE z2ui5_t_12.
    TYPES: tlabel            TYPE string,
           t_sub_col         TYPE ty_t_sub_columns,
           show_no_zeros     TYPE abap_bool,
           grid_layout       TYPE string,
           grid_layout_label TYPE string,
           END OF ty_s_positions.
    TYPES ty_t_positions TYPE STANDARD TABLE OF ty_s_positions WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_s_layout,
        s_head   TYPE ty_s_head,
        t_layout TYPE ty_t_positions,
      END OF ty_s_layout.

    DATA ms_layout     TYPE ty_s_layout.
    DATA ms_layout_tmp TYPE ty_s_layout.
    DATA mt_comps      TYPE ty_t_positions.
    DATA mt_sub_cols   TYPE ty_t_sub_columns.
    DATA mr_data TYPE REF TO data.

    CLASS-METHODS factory
      IMPORTING
        !data         TYPE REF TO data
        !control      TYPE clike
        handle01      TYPE clike OPTIONAL
        handle02      TYPE clike OPTIONAL
        handle03      TYPE clike OPTIONAL
        handle04      TYPE clike OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_layo_manager.

    CLASS-METHODS factory_by_guid
      IMPORTING
        layout_guid   TYPE clike
        t_comps       TYPE Ty_t_positions
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_layo_manager.

    CLASS-METHODS select_layouts
      IMPORTING
        layout_guid   TYPE clike OPTIONAL
        !control      TYPE clike OPTIONAL
        handle01      TYPE clike OPTIONAL
        handle02      TYPE clike OPTIONAL
        handle03      TYPE clike OPTIONAL
        handle04      TYPE clike OPTIONAL
      RETURNING
        VALUE(result) TYPE  ty_t_head.

    CLASS-METHODS select_layout_components
      IMPORTING
        layout_guid   TYPE clike
      RETURNING
        VALUE(result) TYPE  ty_t_positions.

    CLASS-METHODS set_text
      IMPORTING
        !layout       TYPE  ty_s_positions
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS sort_by_seqence
      IMPORTING
        !Pos          TYPE  ty_t_positions
      RETURNING
        VALUE(result) TYPE  ty_t_positions.

    CLASS-METHODS set_sub_columns
      IMPORTING
        !layout       TYPE  ty_t_positions
      RETURNING
        VALUE(result) TYPE  ty_t_positions.

    CLASS-METHODS get_controls
      RETURNING
        VALUE(result) TYPE  ty_t_controls.

    CLASS-METHODS choose_layout
      IMPORTING
        !control      TYPE control DEFAULT  m_table
        handle01      TYPE clike   OPTIONAL
        handle02      TYPE clike   OPTIONAL
        handle03      TYPE clike   OPTIONAL
        handle04      TYPE clike   OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_Layo_pop_w_sel.

    METHODS sort.

  PRIVATE SECTION.

    CLASS-METHODS create_layout_obj
      IMPORTING
        layout_guid   TYPE clike       OPTIONAL
        !data         TYPE REF TO data OPTIONAL
        !control      TYPE clike       OPTIONAL
        handle01      TYPE clike       OPTIONAL
        handle02      TYPE clike       OPTIONAL
        handle03      TYPE clike       OPTIONAL
        handle04      TYPE clike       OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_layo_manager.

    CLASS-METHODS get_default_layout
      IMPORTING
        handle04      TYPE clike
        handle03      TYPE clike
        handle02      TYPE clike
        handle01      TYPE clike
        layout_guid   TYPE clike
        !head         TYPE  ty_t_head
      RETURNING
        VALUE(result) TYPE  ty_s_head.

    CLASS-METHODS build_default_positions
      IMPORTING
        comp          TYPE REF TO abap_componentdescr
        guid          TYPE sysuuid_c32
        !index        TYPE i
      RETURNING
        VALUE(result) TYPE  ty_s_positions.

    CLASS-METHODS check_zeros_option
      IMPORTING
        i_typekind TYPE abap_typekind
      CHANGING
        c_layout   TYPE  ty_s_positions.

    CLASS-METHODS default_grid_layout
      IMPORTING
        !position     TYPE  ty_s_positions
      RETURNING
        VALUE(result) TYPE  ty_s_positions.

ENDCLASS.


CLASS z2ui5_cl_layo_manager IMPLEMENTATION.

  METHOD get_controls.

    result = VALUE #( active = abap_true
                      ( control =  m_table       index = 1 attribute = 'TLABEL' )
                      ( control =  m_table       index = 2 attribute = 'VISIBLE' )
                      ( control =  m_table       index = 3 attribute = 'MERGE' )
                      ( control =  m_table       index = 6 attribute = 'WIDTH' )
                      ( control =  m_table       index = 7 attribute = 'ALTERNATIVE_TEXT' )
                      ( control =  m_table       index = 8 attribute = 'SEQUENCE' )
                      ( control =  m_table       index = 9 attribute = 'SUBCOLUMN' )
                      ( control =  m_table       index = 10 attribute = 'REFERENCE_FIELD' )
                      ( control =  m_table       index = 11 attribute = 'SORTING' )
                      ( control =  m_table       index = 12 attribute = 'NO_LEADING_ZERO' )
                      ( control =  ui_table      index = 1 attribute = 'TLABEL' )
                      ( control =  ui_table      index = 2 attribute = 'VISIBLE' )
                      ( control =  ui_table      index = 3 attribute = 'ALTERNATIVE_TEXT' )
                      ( control =  ui_table      index = 5 attribute = 'WIDTH' )
                      ( control =  others        index = 1 attribute = 'TLABEL' )
                      ( control =  others        index = 2 attribute = 'VISIBLE' )
                      ( control =  others        index = 3 attribute = 'SEQUENCE' )
                      ( control =  others        index = 4 attribute = 'ALTERNATIVE_TEXT' )
                      ( control =  others        index = 5 attribute = 'REFERENCE_FIELD' )
                      ( control =  others        index = 6 attribute = 'WIDTH' )
                      ( control =  ui_simpleform index = 1 attribute = 'TLABEL' )
                      ( control =  ui_simpleform index = 2 attribute = 'VISIBLE' )
                      ( control =  ui_simpleform index = 3 attribute = 'SEQUENCE' )
                      ( control =  ui_simpleform index = 4 attribute = 'ALTERNATIVE_TEXT' )
                      ( control =  ui_simpleform index = 5 attribute = 'REFERENCE_FIELD' )
                      ( control =  ui_simpleform index = 6 attribute = 'NO_LEADING_ZERO' )
                      ( control =  ui_simpleform index = 7 attribute = 'GRID_LAYOUT' ) ).
  ENDMETHOD.

  METHOD factory.

    result = create_layout_obj( data     = data
                                control  = control
                                handle01 = handle01
                                handle02 = handle02
                                handle03 = handle03
                                handle04 = handle04 ).

    result->mr_data = data.

  ENDMETHOD.

  METHOD select_layouts.

    IF layout_guid IS NOT INITIAL.

      SELECT guid,
             layout,
             control,
             handle01,
             handle02,
             handle03,
             handle04,
             descr,
             def,
             uname
        FROM z2ui5_t_11
        WHERE guid = @layout_guid
        INTO CORRESPONDING FIELDS OF TABLE @result ##SUBRC_OK.

    ELSE.

      SELECT guid,
             layout,
             control,
             handle01,
             handle02,
             handle03,
             handle04,
             descr,
             def,
             uname
        FROM z2ui5_t_11
        WHERE control  = @control
          AND handle01 = @handle01
          AND handle02 = @handle02
          AND handle03 = @handle03
          AND handle04 = @handle04
        INTO CORRESPONDING FIELDS OF TABLE @result ##SUBRC_OK.

    ENDIF.

  ENDMETHOD.

  METHOD select_layout_components.

    SELECT guid,
           pos_guid,
           fname,
           rollname,
           visible,
           merge,
           halign,
           importance,
           width,
           sequence,
           alternative_text,
           reference_field,
           subcolumn,
           grid_label_xl,
           grid_value_xl,
           grid_label_l,
           grid_value_l,
           grid_label_m,
           grid_value_m,
           grid_label_s,
           grid_value_s,
           no_leading_zero,
           sorting
      FROM z2ui5_t_12
      WHERE guid = @layout_guid
      INTO CORRESPONDING FIELDS OF TABLE @result ##SUBRC_OK.

  ENDMETHOD.

  METHOD set_text.

    IF layout-alternative_text IS INITIAL.
      result = z2ui5_cl_util=>rtti_get_data_element_texts( layout-rollname )-short.
    ELSE.
      result = z2ui5_cl_util=>rtti_get_data_element_texts( layout-alternative_text )-short.
    ENDIF.

    IF result IS INITIAL.
      result = layout-fname.
    ENDIF.

  ENDMETHOD.

  METHOD sort_by_seqence.

    " First all wit a seqence then the rest
    DATA(tab) = pos.

    DATA(index) = 0.

    DO 99 TIMES.

      index = index + 1.

      LOOP AT tab INTO DATA(line) WHERE sequence = index.

        APPEND line TO result.
        DELETE tab.

      ENDLOOP.

    ENDDO.

    APPEND LINES OF tab TO result.

  ENDMETHOD.

  METHOD set_sub_columns.

    result = layout.

    LOOP AT result REFERENCE INTO DATA(line) WHERE subcolumn IS NOT INITIAL.

      SPLIT line->subcolumn AT ` ` INTO TABLE DATA(tab).

      line->t_sub_col = VALUE #( FOR t IN tab
                                 ( key = z2ui5_cl_util=>uuid_get_c32( ) fname = t ) ).

    ENDLOOP.

  ENDMETHOD.

  METHOD choose_layout.

    DATA(layouts) = select_layouts( control  = control
                                    handle01 = handle01
                                    handle02 = handle02
                                    handle03 = handle03
                                    handle04 = handle04  ).

    result = z2ui5_cl_Layo_pop_w_sel=>factory( i_tab   = layouts
                                               i_title = 'Layouts' ).

  ENDMETHOD.

  METHOD factory_by_guid.

    result = NEW #( ).

    result->ms_layout-t_layout = t_comps.

    " Select Layout Heads
    SELECT SINGLE guid,
                  layout,
                  control,
                  handle01,
                  handle02,
                  handle03,
                  handle04,
                  descr,
                  def,
                  uname
      FROM z2ui5_t_11
      WHERE guid = @layout_guid
      INTO @DATA(head) ##SUBRC_OK.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(t_pos) = select_layout_components( layout_guid ).

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT result->ms_layout-t_layout REFERENCE INTO DATA(layout).

      IF line_exists( t_pos[ fname = layout->fname ] ).

        DATA(pos) = VALUE #( t_pos[ fname = layout->fname ] OPTIONAL ).
        MOVE-CORRESPONDING pos TO layout->*.

      ELSE.

        DATA(no_zero) = layout->no_leading_zero.
        DATA(fname) = layout->fname.
        DATA(rollname) = layout->rollname.

        CLEAR layout->*.

        layout->no_leading_zero = no_zero.
        layout->fname           = fname.
        layout->rollname        = rollname.

        TRY.
            layout->pos_guid = cl_system_uuid=>create_uuid_c32_static( ).
          CATCH cx_root.
        ENDTRY.

        layout->* = default_grid_layout( position = layout->* ).

      ENDIF.

      layout->guid   = layout_guid.
      layout->tlabel = set_text( layout->* ).

    ENDLOOP.

    result->ms_layout-s_head   = CORRESPONDING #( head ).
    result->ms_layout-t_layout = sort_by_seqence( result->ms_layout-t_layout ).
    result->ms_layout-t_layout = set_sub_columns( result->ms_layout-t_layout ).

  ENDMETHOD.

  METHOD create_layout_obj.

    result = NEW #( ).

    DATA(t_comp) = z2ui5_cl_util=>rtti_get_t_attri_by_any( data ).

    LOOP AT t_comp INTO DATA(comp).
      IF comp-type->type_kind = cl_abap_elemdescr=>typekind_oref.
        DELETE t_comp.
      ENDIF.
    ENDLOOP.

    " Select Layout Heads
    DATA(Head) = select_layouts( layout_guid = layout_guid
                                 control     = control
                                 handle01    = handle01
                                 handle02    = handle02
                                 handle03    = handle03
                                 handle04    = handle04 ).

    DATA(def) = get_default_layout( handle04    = handle04
                                    handle03    = handle03
                                    handle02    = handle02
                                    handle01    = handle01
                                    layout_guid = layout_guid
                                    head        = head ).

    IF def-layout IS NOT INITIAL.

      DATA(t_pos) = select_layout_components( def-guid ).

      " Structure was changed - Field Added
      LOOP AT t_comp REFERENCE INTO DATA(r_comp).

        IF NOT line_exists( t_pos[ fname = r_comp->name ] ).

          APPEND build_default_positions( comp  = r_comp
                                          guid  = def-guid
                                          index = 99 ) TO result->ms_layout-t_layout.

        ELSE.

          DATA(pos) = REF #( t_pos[ fname = r_comp->name ] OPTIONAL ).

          " Structure was changed - Field no longer exists
          IF NOT line_exists( t_comp[ name = pos->fname ] ).
            CONTINUE.
          ENDIF.

          DATA(layout) = VALUE ty_s_positions( ).

          layout = CORRESPONDING #( pos->* ).
          layout-rollname = r_comp->type->get_relative_name( ).
          layout-tlabel   = set_text( layout ).

          DATA(typekind) = t_comp[ name = pos->fname ]-type->type_kind.

          check_zeros_option( EXPORTING i_typekind = typekind
                              CHANGING  c_layout   = layout ).

          APPEND layout TO result->ms_layout-t_layout.

        ENDIF.

      ENDLOOP.

      result->ms_layout-s_head   = CORRESPONDING #( def ).
      result->ms_layout-t_layout = sort_by_seqence( result->ms_layout-t_layout ).
      result->ms_layout-t_layout = set_sub_columns( result->ms_layout-t_layout ).

    ELSE.

      TRY.
          DATA(guid) = cl_system_uuid=>create_uuid_c32_static( ).
        CATCH cx_root.
      ENDTRY.

      " Default Layout
      DATA(index) = 0.

      LOOP AT t_comp REFERENCE INTO r_comp.

        index = index + 1.

        APPEND build_default_positions( comp  = r_comp
                                        guid  = guid
                                        index = index ) TO result->ms_layout-t_layout.

      ENDLOOP.

      result->ms_layout-s_head-guid     = guid.
      result->ms_layout-s_head-layout   = 'DEFAULT'.
      result->ms_layout-s_head-control  = control.
      result->ms_layout-s_head-descr    = 'System generated Layout'.
      result->ms_layout-s_head-def      = abap_true.
      result->ms_layout-s_head-handle01 = handle01.
      result->ms_layout-s_head-handle02 = handle02.
      result->ms_layout-s_head-handle03 = handle03.
      result->ms_layout-s_head-handle04 = handle04.

    ENDIF.

  ENDMETHOD.

  METHOD check_zeros_option.

    IF    i_typekind = cl_abap_elemdescr=>typekind_num
       OR i_typekind = cl_abap_elemdescr=>typekind_char.
      IF z2ui5_cl_util=>boolean_check_by_name( CONV #( c_layout-rollname ) ) = abap_false.
        c_layout-show_no_zeros = abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD build_default_positions.

    result-fname    = comp->name.
    result-rollname = comp->type->get_relative_name( ).
    IF result-rollname   IS INITIAL.
      result-rollname = result-fname.
    ENDIF.

    check_zeros_option( EXPORTING i_typekind = comp->type->type_kind
                        CHANGING  c_layout   = result ).

    TRY.
        DATA(pos_guid) = cl_system_uuid=>create_uuid_c32_static( ).
      CATCH cx_root.
    ENDTRY.

    " Default only 10 rows
    IF index <= 10.
      result-visible = abap_true.
    ENDIF.

    IF    result-fname = 'MANDT'
       OR result-fname = 'ROW_ID'
       OR result-fname = 'SELKZ'.
      result-visible = abap_false.
    ENDIF.

    result-guid     = guid.
    result-pos_guid = pos_guid.

    result = default_grid_layout( result ).

    result-tlabel = set_text( result ).

  ENDMETHOD.

  METHOD default_grid_layout.
    result = position.

    result-grid_label_xl = 2.
    result-grid_label_l  = 2.
    result-grid_label_m  = 2.
    result-grid_label_s  = 3.

    result-grid_value_xl = 4.
    result-grid_value_l  = 4.
    result-grid_value_m  = 4.
    result-grid_value_s  = 9.

  ENDMETHOD.

  METHOD get_default_layout.

    IF head IS INITIAL OR layout_guid IS NOT INITIAL.
      RETURN.
    ENDIF.

    " Default all Handles + User
    result = VALUE #( head[ handle01 = handle01
                            handle02 = handle02
                            handle03 = handle03
                            handle04 = handle04
                            def      = abap_true
                            uname    = sy-uname ] OPTIONAL ).

    IF result IS NOT INITIAL.
      RETURN.
    ENDIF.

    " Default frist 4 Handles + no User
    result = VALUE #( head[ handle01 = handle01
                            handle02 = handle02
                            handle03 = handle03
                            handle04 = handle04
                            def      = abap_true ] OPTIONAL ).

  ENDMETHOD.

  METHOD sort.

    FIELD-SYMBOLS <table> TYPE STANDARD TABLE.

    DATA(sortorder) = VALUE abap_sortorder_tab(
                                FOR layout IN ms_layout-t_layout  WHERE ( sorting <> space )
                                ( descending = COND #( WHEN layout-sorting = 'DESCENDING' THEN abap_true )
                                  name       = layout-fname ) ).

    IF sortorder IS INITIAL.
      RETURN.
    ENDIF.

    TRY.

        ASSIGN mr_data->* TO <table>.

        SORT <table>
             BY (sortorder).
      CATCH cx_sy_dyn_table_ill_comp_val. "##NO_HANDLER
      CATCH cx_root.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.
