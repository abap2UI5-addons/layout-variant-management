CLASS z2ui5_cl_layout DEFINITION
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
    TYPES ty_t_controls TYPE STANDARD TABLE OF ty_s_controls WITH DEFAULT KEY.

    CLASS-DATA ui_table TYPE control VALUE 'ui.Table' ##NO_TEXT.
    CLASS-DATA m_table  TYPE control VALUE 'm.Table' ##NO_TEXT.
    CLASS-DATA others   TYPE control VALUE '' ##NO_TEXT.

    TYPES ty_s_Head TYPE z2ui5_t_11.
    TYPES ty_t_head TYPE STANDARD TABLE OF ty_s_head WITH DEFAULT KEY.

    TYPES:
      BEGIN OF ty_s_sub_columns,
        key   TYPE string,
        fname TYPE string,
      END OF ty_s_sub_columns.
    TYPES ty_t_sub_columns TYPE STANDARD TABLE OF ty_s_sub_columns WITH DEFAULT KEY.

    TYPES  BEGIN OF ty_s_positions.
    INCLUDE TYPE z2ui5_t_12.
    TYPES: tlabel    TYPE string,
           t_sub_col TYPE ty_t_sub_columns,
           END OF ty_s_positions.
    TYPES ty_t_positions TYPE STANDARD TABLE OF ty_s_positions WITH DEFAULT KEY.

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

    CLASS-METHODS default_layout
      IMPORTING
        t_layout      TYPE z2ui5_cl_layout=>ty_t_positions
        !control      TYPE clike
        handle01      TYPE clike
        handle02      TYPE clike
        handle03      TYPE clike
        handle04      TYPE clike
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_layout.

    CLASS-METHODS choose_layout
      IMPORTING
        !control      TYPE z2ui5_cl_layout=>control DEFAULT z2ui5_cl_layout=>m_table
        handle01      TYPE clike                    OPTIONAL
        handle02      TYPE clike                    OPTIONAL
        handle03      TYPE clike                    OPTIONAL
        handle04      TYPE clike                    OPTIONAL
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_pop_layout_w_sel.

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

ENDCLASS.


CLASS z2ui5_cl_layout IMPLEMENTATION.

  METHOD get_controls.

    DATA temp1 TYPE z2ui5_cl_layout=>ty_t_controls.
    DATA temp2 LIKE LINE OF temp1.
    CLEAR temp1.
    
    temp2-active = abap_true.
    temp2-control = z2ui5_cl_layout=>m_table.
    temp2-attribute = 'VISIBLE'.
    INSERT temp2 INTO TABLE temp1.
    temp2-control = z2ui5_cl_layout=>m_table.
    temp2-attribute = 'MERGE'.
    INSERT temp2 INTO TABLE temp1.
    temp2-control = z2ui5_cl_layout=>m_table.
    temp2-attribute = 'HALIGN'.
    INSERT temp2 INTO TABLE temp1.
    temp2-control = z2ui5_cl_layout=>m_table.
    temp2-attribute = 'IMPORTANCE'.
    INSERT temp2 INTO TABLE temp1.
    temp2-control = z2ui5_cl_layout=>m_table.
    temp2-attribute = 'WIDTH'.
    INSERT temp2 INTO TABLE temp1.
    temp2-control = z2ui5_cl_layout=>m_table.
    temp2-attribute = 'ALTERNATIVE_TEXT'.
    INSERT temp2 INTO TABLE temp1.
    temp2-control = z2ui5_cl_layout=>m_table.
    temp2-attribute = 'SEQUENCE'.
    INSERT temp2 INTO TABLE temp1.
    temp2-control = z2ui5_cl_layout=>m_table.
    temp2-attribute = 'SUBCOLUMN'.
    INSERT temp2 INTO TABLE temp1.
    temp2-control = z2ui5_cl_layout=>m_table.
    temp2-attribute = 'REFERENCE_FIELD'.
    INSERT temp2 INTO TABLE temp1.
    temp2-control = z2ui5_cl_layout=>ui_table.
    temp2-attribute = 'VISIBLE'.
    INSERT temp2 INTO TABLE temp1.
    temp2-control = z2ui5_cl_layout=>ui_table.
    temp2-attribute = 'ALTERNATIVE_TEXT'.
    INSERT temp2 INTO TABLE temp1.
    temp2-control = z2ui5_cl_layout=>ui_table.
    temp2-attribute = 'HALIGN'.
    INSERT temp2 INTO TABLE temp1.
    temp2-control = z2ui5_cl_layout=>ui_table.
    temp2-attribute = 'WIDTH'.
    INSERT temp2 INTO TABLE temp1.
    temp2-control = z2ui5_cl_layout=>others.
    temp2-attribute = 'VISIBLE'.
    INSERT temp2 INTO TABLE temp1.
    temp2-control = z2ui5_cl_layout=>others.
    temp2-attribute = 'SEQUENCE'.
    INSERT temp2 INTO TABLE temp1.
    temp2-control = z2ui5_cl_layout=>others.
    temp2-attribute = 'ALTERNATIVE_TEXT'.
    INSERT temp2 INTO TABLE temp1.
    temp2-control = z2ui5_cl_layout=>others.
    temp2-attribute = 'REFERENCE_FIELD'.
    INSERT temp2 INTO TABLE temp1.
    temp2-control = z2ui5_cl_layout=>others.
    temp2-attribute = 'WIDTH'.
    INSERT temp2 INTO TABLE temp1.
    result = temp1.

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
      SELECT guid
             layout
             control
             handle01
             handle02
             handle03
             handle04
             descr
             def
             uname
        FROM z2ui5_t_11 INTO CORRESPONDING FIELDS OF TABLE result
        WHERE guid = layout_guid
         ##SUBRC_OK.

    ELSE.

      SELECT guid
             layout
             control
             handle01
             handle02
             handle03
             handle04
             descr
             def
             uname
        FROM z2ui5_t_11 INTO CORRESPONDING FIELDS OF TABLE result
        WHERE control  = control
          AND handle01 = handle01
          AND handle02 = handle02
          AND handle03 = handle03
          AND handle04 = handle04
         ##SUBRC_OK.

    ENDIF.

  ENDMETHOD.

  METHOD select_layout_components.

    SELECT guid
           pos_guid
           layout
           control
           handle01
           handle02
           handle03
           handle04
           fname
           rollname
           visible
           merge
           halign
           importance
           width
           sequence
           alternative_text
           reference_field
           subcolumn
      FROM z2ui5_t_12 INTO CORRESPONDING FIELDS OF TABLE result
      WHERE guid = layout_guid
       ##SUBRC_OK.

  ENDMETHOD.

  METHOD set_text.
      DATA temp3 TYPE string.
      DATA temp4 TYPE string.
    IF layout-alternative_text IS INITIAL.
*      result = z2ui5_cl_stmpncfctn_api=>rtti_get_data_element_texts( CONV #( layout-rollname ) )-long.
      
      temp3 = layout-rollname.
      result = z2ui5_cl_util=>rtti_get_data_element_texts( temp3 )-short.
    ELSE.
*      result = z2ui5_cl_stmpncfctn_api=>rtti_get_data_element_texts( CONV #( layout-alternative_text ) )-long.
      
      temp4 = layout-alternative_text.
      result = z2ui5_cl_util=>rtti_get_data_element_texts( temp4 )-short.
    ENDIF.

    IF result IS INITIAL.
      result = layout-fname.
    ENDIF.

  ENDMETHOD.

  METHOD sort_by_seqence.

    " First all wit a seqence then the rest
    DATA tab LIKE pos.
    DATA index TYPE i.
      DATA line LIKE LINE OF tab.
    tab = pos.

    
    index = 0.

    DO 99 TIMES.

      index = index + 1.

      
      LOOP AT tab INTO line WHERE sequence = index.

        APPEND line TO result.
        DELETE tab.

      ENDLOOP.

    ENDDO.

    APPEND LINES OF tab TO result.

  ENDMETHOD.

  METHOD set_sub_columns.
    DATA temp5 LIKE LINE OF result.
    DATA line LIKE REF TO temp5.
      DATA tab TYPE STANDARD TABLE OF string WITH DEFAULT KEY.
      DATA temp6 TYPE z2ui5_cl_layout=>ty_t_sub_columns.
      DATA t LIKE LINE OF tab.
        DATA temp7 LIKE LINE OF temp6.

    result = layout.

    
    
    LOOP AT result REFERENCE INTO line WHERE subcolumn IS NOT INITIAL.

      
      SPLIT line->subcolumn AT ` ` INTO TABLE tab.

      
      CLEAR temp6.
      
      LOOP AT tab INTO t.
        
        temp7-key = z2ui5_cl_util=>uuid_get_c32( ).
        temp7-fname = t.
        INSERT temp7 INTO TABLE temp6.
      ENDLOOP.
      line->t_sub_col = temp6.

    ENDLOOP.

  ENDMETHOD.

  METHOD default_layout.
        DATA guid TYPE sysuuid_c32.
    DATA index TYPE i.
    DATA temp8 LIKE LINE OF result->ms_layout-t_layout.
    DATA layout LIKE REF TO temp8.
          DATA pos_guid TYPE sysuuid_c32.

    CREATE OBJECT result.

    TRY.
        
        guid = cl_system_uuid=>create_uuid_c32_static( ).
      CATCH cx_root.
    ENDTRY.

    result->ms_layout-t_layout = t_layout.

    " Default Layout
    
    index = 0.

    
    
    LOOP AT result->ms_layout-t_layout REFERENCE INTO layout.

      TRY.
          
          pos_guid = cl_system_uuid=>create_uuid_c32_static( ).
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

    DATA layouts TYPE z2ui5_cl_layout=>ty_t_head.
    layouts = select_layouts( control  = control
                                    handle01 = handle01
                                    handle02 = handle02
                                    handle03 = handle03
                                    handle04 = handle04  ).

    result = z2ui5_cl_pop_layout_w_sel=>factory( i_tab   = layouts
                                                    i_title = 'Layouts' ).

  ENDMETHOD.

  METHOD factory_by_guid.

    result = create_layout_obj( layout_guid = layout_guid ).

  ENDMETHOD.

  METHOD create_layout_obj.
    DATA Head TYPE z2ui5_cl_layout=>ty_t_head.
      DATA temp9 TYPE z2ui5_t_11.
      DATA temp10 TYPE z2ui5_t_11.
      DATA def LIKE temp9.
        DATA temp11 TYPE z2ui5_t_11.
        DATA temp12 TYPE z2ui5_t_11.
          DATA temp13 TYPE z2ui5_t_11.
          DATA temp14 TYPE z2ui5_t_11.
            DATA temp15 TYPE z2ui5_t_11.
            DATA temp16 TYPE z2ui5_t_11.
            DATA temp17 TYPE z2ui5_t_11.
            DATA temp18 TYPE z2ui5_t_11.
            DATA temp19 TYPE z2ui5_t_11.
            DATA temp20 TYPE z2ui5_t_11.
      DATA temp21 TYPE z2ui5_t_11.
      DATA temp22 TYPE z2ui5_t_11.
TYPES BEGIN OF temp23.
TYPES guid TYPE z2ui5_t_12-guid.
TYPES pos_guid TYPE z2ui5_t_12-pos_guid.
TYPES layout TYPE z2ui5_t_12-layout.
TYPES control TYPE z2ui5_t_12-control.
TYPES handle01 TYPE z2ui5_t_12-handle01.
TYPES handle02 TYPE z2ui5_t_12-handle02.
TYPES handle03 TYPE z2ui5_t_12-handle03.
TYPES handle04 TYPE z2ui5_t_12-handle04.
TYPES fname TYPE z2ui5_t_12-fname.
TYPES rollname TYPE z2ui5_t_12-rollname.
TYPES visible TYPE z2ui5_t_12-visible.
TYPES merge TYPE z2ui5_t_12-merge.
TYPES halign TYPE z2ui5_t_12-halign.
TYPES importance TYPE z2ui5_t_12-importance.
TYPES width TYPE z2ui5_t_12-width.
TYPES sequence TYPE z2ui5_t_12-sequence.
TYPES alternative_text TYPE z2ui5_t_12-alternative_text.
TYPES subcolumn TYPE z2ui5_t_12-subcolumn.
TYPES reference_field TYPE z2ui5_t_12-reference_field.
TYPES END OF temp23.
      DATA t_pos TYPE STANDARD TABLE OF temp23 WITH DEFAULT KEY.
      DATA temp24 LIKE LINE OF t_pos.
      DATA pos LIKE REF TO temp24.
        DATA temp25 TYPE ty_s_positions.
        DATA layout LIKE temp25.
    DATA t_comp TYPE abap_component_tab.
    DATA comp LIKE LINE OF t_comp.
    DATA temp26 LIKE LINE OF t_comp.
    DATA lr_comp LIKE REF TO temp26.
      DATA temp27 TYPE z2ui5_cl_layout=>ty_s_positions.

    CREATE OBJECT result.

    " Select Layout Heads
    
    Head = select_layouts( layout_guid = layout_guid
                                 control     = control
                                 handle01    = handle01
                                 handle02    = handle02
                                 handle03    = handle03
                                 handle04    = handle04 ).

    IF sy-subrc = 0 AND layout_guid IS INITIAL.

      " Default all Handles + User
      
      CLEAR temp9.
      
      READ TABLE Head INTO temp10 WITH KEY handle01 = handle01 handle02 = handle02 handle03 = handle03 handle04 = handle04 def = abap_true uname = sy-uname.
      IF sy-subrc = 0.
        temp9 = temp10.
      ENDIF.
      
      def = temp9.

      IF def IS INITIAL.
        " Default frist 3 Handles + User
        
        CLEAR temp11.
        
        READ TABLE Head INTO temp12 WITH KEY handle01 = handle01 handle02 = handle02 handle03 = handle03 def = abap_true uname = sy-uname.
        IF sy-subrc = 0.
          temp11 = temp12.
        ENDIF.
        def = temp11.
        IF def IS INITIAL.
          " Default frist 2 Handles + User
          
          CLEAR temp13.
          
          READ TABLE Head INTO temp14 WITH KEY handle01 = handle01 handle02 = handle02 def = abap_true uname = sy-uname.
          IF sy-subrc = 0.
            temp13 = temp14.
          ENDIF.
          def = temp13.
          IF def IS INITIAL.
            " Default frist 1 Handles + User
            
            CLEAR temp15.
            
            READ TABLE Head INTO temp16 WITH KEY handle01 = handle01 def = abap_true uname = sy-uname.
            IF sy-subrc = 0.
              temp15 = temp16.
            ENDIF.
            def = temp15.
          ENDIF.
          IF def IS INITIAL.
            " Default User
            
            CLEAR temp17.
            
            READ TABLE Head INTO temp18 WITH KEY def = abap_true uname = sy-uname.
            IF sy-subrc = 0.
              temp17 = temp18.
            ENDIF.
            def = temp17.
          ENDIF.
          IF def IS INITIAL.
            " Default User
            
            CLEAR temp19.
            
            READ TABLE Head INTO temp20 WITH KEY def = abap_true.
            IF sy-subrc = 0.
              temp19 = temp20.
            ENDIF.
            def = temp19.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.

      
      CLEAR temp21.
      
      READ TABLE head INTO temp22 INDEX 1.
      IF sy-subrc = 0.
        temp21 = temp22.
      ENDIF.
      def = temp21.

    ENDIF.

    IF def-layout IS NOT INITIAL.

      
      
      SELECT guid
             pos_guid
             layout
             control
             handle01
             handle02
             handle03
             handle04
             fname
             rollname
             visible
             merge
             halign
             importance
             width
             sequence
             alternative_text
             subcolumn
             reference_field
        FROM z2ui5_t_12 INTO TABLE t_pos
        WHERE guid = def-guid
         ##SUBRC_OK.

      
      
      LOOP AT t_pos REFERENCE INTO pos.

        
        CLEAR temp25.
        
        layout = temp25.

        MOVE-CORRESPONDING pos->* TO layout.
        layout-tlabel = set_text( layout ).

        APPEND layout TO result->ms_layout-t_layout.

      ENDLOOP.

      MOVE-CORRESPONDING def TO result->ms_layout-s_head.
      result->ms_layout-t_layout = sort_by_seqence( result->ms_layout-t_layout ).
      result->ms_layout-t_layout = set_sub_columns( result->ms_layout-t_layout ).

      RETURN.

    ENDIF.

    " create the tab first if the db fields were added/deleted
    
    t_comp = z2ui5_cl_util=>rtti_get_t_attri_by_any( data ).

    
    LOOP AT t_comp INTO comp.
      IF comp-type->type_kind = cl_abap_elemdescr=>typekind_oref.
        DELETE t_comp.
      ENDIF.
    ENDLOOP.

    
    
    LOOP AT t_comp REFERENCE INTO lr_comp.

      
      CLEAR temp27.
      temp27-control = control.
      temp27-handle01 = handle01.
      temp27-handle02 = handle02.
      temp27-handle03 = handle03.
      temp27-handle04 = handle04.
      temp27-fname = lr_comp->name.
      temp27-rollname = lr_comp->type->get_relative_name( ).
      INSERT temp27
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
