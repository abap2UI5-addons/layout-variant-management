CLASS z2ui5_cl_layo_manager DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_serializable_object.

    TYPES handle  TYPE c LENGTH 40.
    TYPES control TYPE c LENGTH 10.

    TYPES:
      BEGIN OF ty_s_controls,
        attribute TYPE string,
        control   TYPE control,
        active    TYPE abap_bool,
        others    TYPE abap_bool,
      END OF ty_s_controls.
    TYPES ty_t_controls TYPE STANDARD TABLE OF ty_s_controls WITH EMPTY KEY.

    CLASS-DATA ui_table TYPE control VALUE 'ui.Table' ##NO_TEXT.
    CLASS-DATA m_table  TYPE control VALUE 'm.Table' ##NO_TEXT.
    CLASS-DATA others   TYPE control VALUE '' ##NO_TEXT.

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
    TYPES: tlabel    TYPE string,
           t_sub_col TYPE ty_t_sub_columns,
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
        data         TYPE REF TO data OPTIONAL
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
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_layo_manager.

    CLASS-METHODS select_layouts
      IMPORTING
        layout_guid   TYPE clike OPTIONAL
        !control      TYPE clike
        handle01      TYPE clike
        handle02      TYPE clike
        handle03      TYPE clike
        handle04      TYPE clike
      RETURNING
        VALUE(result) TYPE ty_t_head.

    CLASS-METHODS select_layout_components
      IMPORTING
        layout_guid   TYPE clike
      RETURNING
        VALUE(result) TYPE ty_t_positions.

    CLASS-METHODS set_text
      IMPORTING
        !layout       TYPE ty_s_positions
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS sort_by_seqence
      IMPORTING
        !Pos          TYPE ty_t_positions
      RETURNING
        VALUE(result) TYPE ty_t_positions.

    CLASS-METHODS set_sub_columns
      IMPORTING
        !layout       TYPE ty_t_positions
      RETURNING
        VALUE(result) TYPE ty_t_positions.

    CLASS-METHODS get_controls
      RETURNING
        VALUE(result) TYPE ty_t_controls.

    CLASS-METHODS default_layout
      IMPORTING
        t_layout      TYPE ty_t_positions
        !control      TYPE clike
        handle01      TYPE clike
        handle02      TYPE clike
        handle03      TYPE clike
        handle04      TYPE clike
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_layo_manager.

    CLASS-METHODS choose_layout
      IMPORTING
        !control      TYPE control DEFAULT m_table
        handle01      TYPE clike                    OPTIONAL
        handle02      TYPE clike                    OPTIONAL
        handle03      TYPE clike                    OPTIONAL
        handle04      TYPE clike                    OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_layo_pop_w_sel.

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

ENDCLASS.


CLASS z2ui5_cl_layo_manager IMPLEMENTATION.

  METHOD get_controls.

    result = VALUE #( active = abap_true
                      ( control = m_table  attribute = 'VISIBLE' )
                      ( control = m_table  attribute = 'MERGE' )
                      ( control = m_table  attribute = 'HALIGN' )
                      ( control = m_table  attribute = 'IMPORTANCE' )
                      ( control = m_table  attribute = 'WIDTH' )
                      ( control = m_table  attribute = 'ALTERNATIVE_TEXT' )
                      ( control = m_table  attribute = 'SEQUENCE' )
                      ( control = m_table  attribute = 'SUBCOLUMN' )
                      ( control = m_table  attribute = 'REFERENCE_FIELD' )
                      ( control = ui_table attribute = 'VISIBLE' )
                      ( control = ui_table attribute = 'ALTERNATIVE_TEXT' )
                      ( control = ui_table attribute = 'HALIGN' )
                      ( control = ui_table attribute = 'WIDTH' )
                      ( control = others   attribute = 'VISIBLE' )
                      ( control = others   attribute = 'SEQUENCE' )
                      ( control = others   attribute = 'ALTERNATIVE_TEXT' )
                      ( control = others   attribute = 'REFERENCE_FIELD' )
                      ( control = others   attribute = 'WIDTH' ) ).

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
*      DATA(lr_guid) = VALUE z2ui5_cl_util=>ty_t_range(
*                                ( CORRESPONDING #( z2ui5_cl_util=>filter_get_range_by_token( |={  layout_guid }| ) ) ) ).
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
           subcolumn
      FROM z2ui5_t_12
      WHERE guid = @layout_guid
      INTO CORRESPONDING FIELDS OF TABLE @result ##SUBRC_OK.

  ENDMETHOD.

  METHOD set_text.
    IF layout-alternative_text IS INITIAL.
*      result = z2ui5_cl_stmpncfctn_api=>rtti_get_data_element_texts( CONV #( layout-rollname ) )-long.
      result = z2ui5_cl_util=>rtti_get_data_element_texts( CONV #( layout-rollname ) )-short.
    ELSE.
*      result = z2ui5_cl_stmpncfctn_api=>rtti_get_data_element_texts( CONV #( layout-alternative_text ) )-long.
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

  METHOD default_layout.

    result = NEW #( ).

    TRY.
        DATA(guid) = cl_system_uuid=>create_uuid_c32_static( ).
      CATCH cx_root.
    ENDTRY.

    result->ms_layout-t_layout = t_layout.

    " Default Layout
    DATA(index) = 0.

    LOOP AT result->ms_layout-t_layout REFERENCE INTO DATA(layout).

      TRY.
          DATA(pos_guid) = cl_system_uuid=>create_uuid_c32_static( ).
        CATCH cx_root.
      ENDTRY.

      index = index + 1.

      " Default only 10 rows
      IF index <= 10.
        layout->visible = abap_true.
      ENDIF.

      IF    layout->fname = 'MANDT'
         OR layout->fname = 'ROW_ID'
         OR layout->fname = 'SELKZ'.
        layout->visible = abap_false.
        layout->width   = '5rem'.
      ENDIF.

      layout->guid       = guid.
      layout->pos_guid   = pos_guid.
      layout->layout     = 'Default'.
      layout->control    = control.
      layout->halign     = 'Begin'.
      layout->importance = 'None'.
      layout->handle01   = handle01.
      layout->handle02   = handle02.
      layout->handle03   = handle03.
      layout->handle04   = handle04.

      layout->tlabel     = set_text( layout->* ).

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

  ENDMETHOD.

  METHOD choose_layout.

    DATA(layouts) = select_layouts( control  = control
                                    handle01 = handle01
                                    handle02 = handle02
                                    handle03 = handle03
                                    handle04 = handle04  ).

    result = z2ui5_cl_layo_pop_w_sel=>factory( i_tab   = layouts
                                                    i_title = 'Layouts' ).

  ENDMETHOD.

  METHOD factory_by_guid.

    result = create_layout_obj( layout_guid = layout_guid ).

  ENDMETHOD.

  METHOD create_layout_obj.

    result = NEW #( ).

    " Select Layout Heads
    DATA(Head) = select_layouts( layout_guid = layout_guid
                                 control     = control
                                 handle01    = handle01
                                 handle02    = handle02
                                 handle03    = handle03
                                 handle04    = handle04 ).

    IF sy-subrc = 0 AND layout_guid IS INITIAL.

      " Default all Handles + User
      DATA(def) = VALUE #( Head[ handle01 = handle01
                                 handle02 = handle02
                                 handle03 = handle03
                                 handle04 = handle04
                                 def      = abap_true
                                 uname    = sy-uname ] OPTIONAL ).

      IF def IS INITIAL.
        " Default frist 3 Handles + User
        def = VALUE #( Head[ handle01 = handle01
                             handle02 = handle02
                             handle03 = handle03
                             def      = abap_true
                             uname    = sy-uname ] OPTIONAL ).
        IF def IS INITIAL.
          " Default frist 2 Handles + User
          def = VALUE #( Head[ handle01 = handle01
                               handle02 = handle02
                               def      = abap_true
                               uname    = sy-uname ] OPTIONAL ).
          IF def IS INITIAL.
            " Default frist 1 Handles + User
            def = VALUE #( Head[ handle01 = handle01
                                 def      = abap_true
                                 uname    = sy-uname ] OPTIONAL ).
          ENDIF.
          IF def IS INITIAL.
            " Default User
            def = VALUE #( Head[ def   = abap_true
                                 uname = sy-uname ] OPTIONAL ).
          ENDIF.
          IF def IS INITIAL.
            " Default User
            def = VALUE #( Head[ def = abap_true ] OPTIONAL ).
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.

      def = VALUE #( head[ 1 ] OPTIONAL ).

    ENDIF.

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
             subcolumn,
             reference_field
        FROM z2ui5_t_12
        WHERE guid = @def-guid
        INTO TABLE @DATA(t_pos) ##SUBRC_OK.

      LOOP AT t_pos REFERENCE INTO DATA(pos).

        DATA(layout) = VALUE ty_s_positions( ).

        layout = CORRESPONDING #( pos->* ).
        layout-tlabel = set_text( layout ).

        APPEND layout TO result->ms_layout-t_layout.

      ENDLOOP.

      result->ms_layout-s_head   = CORRESPONDING #( def ).
      result->ms_layout-t_layout = sort_by_seqence( result->ms_layout-t_layout ).
      result->ms_layout-t_layout = set_sub_columns( result->ms_layout-t_layout ).

      RETURN.

    ENDIF.

    " create the tab first if the db fields were added/deleted
    DATA(t_comp) = z2ui5_cl_util=>rtti_get_t_attri_by_any( data ).

    LOOP AT t_comp INTO DATA(comp).
      IF comp-type->type_kind = cl_abap_elemdescr=>typekind_oref.
        DELETE t_comp.
      ENDIF.
    ENDLOOP.

    LOOP AT t_comp REFERENCE INTO DATA(lr_comp).

      INSERT VALUE #( control  = control
                      handle01 = handle01
                      handle02 = handle02
                      handle03 = handle03
                      handle04 = handle04
                      fname    = lr_comp->name
                      rollname = lr_comp->type->get_relative_name( ) )
             INTO TABLE result->ms_layout-t_layout.
    ENDLOOP.

    result = default_layout( t_layout = result->ms_layout-t_layout
                             control  = control
                             handle01 = handle01
                             handle02 = handle02
                             handle03 = handle03
                             handle04 = handle04 ).

  ENDMETHOD.

ENDCLASS.
