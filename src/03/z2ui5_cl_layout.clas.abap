CLASS z2ui5_cl_layout DEFINITION
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
        others    TYPE abap_bool,
      END OF ty_s_controls.
    TYPES ty_t_controls TYPE STANDARD TABLE OF ty_s_controls WITH EMPTY KEY.

    CLASS-DATA ui_table      TYPE control VALUE 'ui.Table' ##NO_TEXT.
    CLASS-DATA m_table       TYPE control VALUE 'm.Table' ##NO_TEXT.
    CLASS-DATA ui_simpleform TYPE control VALUE 'ui.SimpleForm' ##NO_TEXT.
    CLASS-DATA others        TYPE control VALUE '' ##NO_TEXT.

    TYPES ty_s_Head TYPE z2ui5_layo_t_01.
    TYPES ty_t_head TYPE STANDARD TABLE OF ty_s_head WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_s_sub_columns,
        key   TYPE string,
        fname TYPE string,
      END OF ty_s_sub_columns.
    TYPES ty_t_sub_columns TYPE STANDARD TABLE OF ty_s_sub_columns WITH EMPTY KEY.

    TYPES  BEGIN OF ty_s_positions.
             INCLUDE TYPE z2ui5_layo_t_02.
    TYPES:   tlabel            TYPE string,
             t_sub_col         TYPE ty_t_sub_columns,
             grid_layout       TYPE char20,
             grid_layout_label TYPE char20,
           END OF ty_s_positions.
    TYPES ty_t_positions TYPE STANDARD TABLE OF ty_s_positions WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_s_layout,
        s_head   TYPE ty_s_head,
        t_layout TYPE ty_t_positions,
      END OF ty_s_layout.

    DATA ms_layout       TYPE ty_s_layout.
    DATA ms_layout_tmp   TYPE ty_s_layout.
    DATA mt_comps        TYPE ty_t_positions.
    DATA mt_sub_cols     TYPE ty_t_sub_columns.
    DATA mt_sub_cols_tmp TYPE ty_t_sub_columns.

    CLASS-METHODS factory
      IMPORTING
        !data         TYPE REF TO data
        !control      TYPE clike
        handle01      TYPE clike OPTIONAL
        handle02      TYPE clike OPTIONAL
        handle03      TYPE clike OPTIONAL
        handle04      TYPE clike OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_layout.

    CLASS-METHODS factory_by_guid
      IMPORTING
        layout_guid   TYPE clike
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_layout.

    CLASS-METHODS select_layouts
      IMPORTING
        layout_guid   TYPE clike OPTIONAL
        !control      TYPE clike
        handle01      TYPE clike
        handle02      TYPE clike
        handle03      TYPE clike
        handle04      TYPE clike
      RETURNING
        VALUE(result) TYPE z2ui5_cl_layout=>ty_t_head.

    CLASS-METHODS select_layout_components
      IMPORTING
        layout_guid   TYPE clike
      RETURNING
        VALUE(result) TYPE z2ui5_cl_layout=>ty_t_positions.

    CLASS-METHODS set_text
      IMPORTING
        !layout       TYPE z2ui5_cl_layout=>ty_s_positions
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS sort_by_seqence
      IMPORTING
        !Pos          TYPE z2ui5_cl_layout=>ty_t_positions
      RETURNING
        VALUE(result) TYPE z2ui5_cl_layout=>ty_t_positions.

    CLASS-METHODS set_sub_columns
      IMPORTING
        !layout       TYPE z2ui5_cl_layout=>ty_t_positions
      RETURNING
        VALUE(result) TYPE z2ui5_cl_layout=>ty_t_positions.

    CLASS-METHODS get_controls
      RETURNING
        VALUE(result) TYPE z2ui5_cl_layout=>ty_t_controls.

    CLASS-METHODS choose_layout
      IMPORTING
        !control      TYPE z2ui5_cl_layout=>control DEFAULT z2ui5_cl_layout=>m_table
        handle01      TYPE clike                    OPTIONAL
        handle02      TYPE clike                    OPTIONAL
        handle03      TYPE clike                    OPTIONAL
        handle04      TYPE clike                    OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_pop_to_sel_w_layout.

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
        VALUE(result) TYPE REF TO z2ui5_cl_layout.

    CLASS-METHODS get_default_layout
      IMPORTING
        handle04      TYPE clike
        handle03      TYPE clike
        handle02      TYPE clike
        handle01      TYPE clike
        layout_guid   TYPE clike
        !head         TYPE z2ui5_cl_layout=>ty_t_head
      RETURNING
        VALUE(result) TYPE z2ui5_cl_layout=>ty_s_head.

    CLASS-METHODS build_default_positions
      IMPORTING
        !control      TYPE clike
        handle01      TYPE clike
        handle02      TYPE clike
        handle03      TYPE clike
        handle04      TYPE clike
        comp          TYPE REF TO abap_componentdescr
        guid          TYPE sysuuid_c32
        !index        TYPE i
      RETURNING
        VALUE(result) TYPE z2ui5_cl_layout=>ty_s_positions.

ENDCLASS.


CLASS z2ui5_cl_layout IMPLEMENTATION.

  METHOD get_controls.

    result = VALUE #( active = abap_true
                      ( control = z2ui5_cl_layout=>m_table  attribute = 'VISIBLE' )
                      ( control = z2ui5_cl_layout=>m_table  attribute = 'MERGE' )
                      ( control = z2ui5_cl_layout=>m_table  attribute = 'HALIGN' )
                      ( control = z2ui5_cl_layout=>m_table  attribute = 'IMPORTANCE' )
                      ( control = z2ui5_cl_layout=>m_table  attribute = 'WIDTH' )
                      ( control = z2ui5_cl_layout=>m_table  attribute = 'ALTERNATIVE_TEXT' )
                      ( control = z2ui5_cl_layout=>m_table  attribute = 'SEQUENCE' )
                      ( control = z2ui5_cl_layout=>m_table  attribute = 'SUBCOLUMN' )
                      ( control = z2ui5_cl_layout=>m_table  attribute = 'REFERENCE_FIELD' )
                      ( control = z2ui5_cl_layout=>ui_table attribute = 'VISIBLE' )
                      ( control = z2ui5_cl_layout=>ui_table attribute = 'ALTERNATIVE_TEXT' )
                      ( control = z2ui5_cl_layout=>ui_table attribute = 'HALIGN' )
                      ( control = z2ui5_cl_layout=>ui_table attribute = 'WIDTH' )
                      ( control = z2ui5_cl_layout=>others   attribute = 'VISIBLE' )
                      ( control = z2ui5_cl_layout=>others   attribute = 'SEQUENCE' )
                      ( control = z2ui5_cl_layout=>others   attribute = 'ALTERNATIVE_TEXT' )
                      ( control = z2ui5_cl_layout=>others   attribute = 'REFERENCE_FIELD' )
                      ( control = z2ui5_cl_layout=>others   attribute = 'WIDTH' )
                      ( control = z2ui5_cl_layout=>ui_simpleform   attribute = 'VISIBLE' )
                      ( control = z2ui5_cl_layout=>ui_simpleform   attribute = 'SEQUENCE' )
                      ( control = z2ui5_cl_layout=>ui_simpleform   attribute = 'ALTERNATIVE_TEXT' )
                      ( control = z2ui5_cl_layout=>ui_simpleform   attribute = 'REFERENCE_FIELD' )
                      ( control = z2ui5_cl_layout=>ui_simpleform   attribute = 'GRID_LAYOUT' ) ).

  ENDMETHOD.

  METHOD factory.

    result = create_layout_obj(
*                                layout_guid =
                                data     = data
                                control  = control
                                handle01 = handle01
                                handle02 = handle02
                                handle03 = handle03
                                handle04 = handle04 ).

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
        FROM z2ui5_layo_t_01
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
        FROM z2ui5_layo_t_01
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
           layout,
           control,
           handle01,
           handle02,
           handle03,
           handle04,
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
           grid_value_s
      FROM z2ui5_layo_t_02
      WHERE guid = @layout_guid
      INTO CORRESPONDING FIELDS OF TABLE @result ##SUBRC_OK.

  ENDMETHOD.

  METHOD set_text.

    IF layout-alternative_text IS INITIAL.
      result = z2ui5_cl_util=>rtti_get_data_element_texts( CONV #( layout-rollname ) )-short.
    ELSE.
      result = z2ui5_cl_util=>rtti_get_data_element_texts( CONV #( layout-alternative_text ) )-short.
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

    result = z2ui5_cl_pop_to_sel_w_layout=>factory( i_tab   = layouts
                                                    i_title = 'Layouts' ).

  ENDMETHOD.

  METHOD factory_by_guid.

    result = create_layout_obj( layout_guid = layout_guid ).

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

      SELECT guid,
             pos_guid,
             layout,
             control,
             handle01,
             handle02,
             handle03,
             handle04,
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
             grid_value_s
        FROM z2ui5_layo_t_02
        WHERE guid = @def-guid
        INTO TABLE @DATA(t_pos) ##SUBRC_OK.

      LOOP AT t_pos REFERENCE INTO DATA(pos).

        DATA(layout) = VALUE ty_s_positions( ).

        " Structure was changed - Field no longer exists
        IF NOT line_exists( t_comp[ name = pos->fname ] ).
          CONTINUE.
        ENDIF.

        layout = CORRESPONDING #( pos->* ).
        layout-tlabel = set_text( layout ).

        APPEND layout TO result->ms_layout-t_layout.

      ENDLOOP.

      " Structure was changed - Field Added
      LOOP AT t_comp REFERENCE INTO DATA(r_comp).
        IF NOT line_exists( t_pos[ fname = r_comp->name ] ).

          APPEND build_default_positions( control  = control
                                          handle01 = handle01
                                          handle02 = handle02
                                          handle03 = handle03
                                          handle04 = handle04
                                          comp     = r_comp
                                          guid     = def-guid
                                          index    = 99 ) TO result->ms_layout-t_layout.

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

        APPEND build_default_positions( control  = control
                                        handle01 = handle01
                                        handle02 = handle02
                                        handle03 = handle03
                                        handle04 = handle04
                                        comp     = r_comp
                                        guid     = guid
                                        index    = index ) TO result->ms_layout-t_layout.

      ENDLOOP.

      result->ms_layout-s_head-guid     = guid.
      result->ms_layout-s_head-layout   = 'Default'.
      result->ms_layout-s_head-control  = control.
      result->ms_layout-s_head-descr    = 'System generated Layout'.
      result->ms_layout-s_head-def      = abap_true.
      result->ms_layout-s_head-handle01 = handle01.
      result->ms_layout-s_head-handle02 = handle02.
      result->ms_layout-s_head-handle03 = handle03.
      result->ms_layout-s_head-handle04 = handle04.

    ENDIF.

  ENDMETHOD.

  METHOD build_default_positions.

    result-control  = control.
    result-handle01 = handle01.
    result-handle02 = handle02.
    result-handle03 = handle03.
    result-handle04 = handle04.
    result-fname    = comp->name.
    result-rollname = comp->type->get_relative_name( ).

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
      result-width   = '5rem'.
    ENDIF.

    result-guid          = guid.
    result-pos_guid      = pos_guid.
    result-layout        = 'Default'.
    result-control       = control.
    result-halign        = 'Begin'.
    result-importance    = 'None'.
    result-handle01      = handle01.
    result-handle02      = handle02.
    result-handle03      = handle03.
    result-handle04      = handle04.

    result-grid_label_xl = '2'.
    result-grid_label_l  = '2'.
    result-grid_label_m  = '2'.
    result-grid_label_s  = '3'.

    result-grid_value_xl = '4'.
    result-grid_value_l  = '4'.
    result-grid_value_m  = '4'.
    result-grid_value_s  = '9'.

    result-tlabel        = set_text( result ).

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

ENDCLASS.
