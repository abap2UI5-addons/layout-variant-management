CLASS z2ui5_cl_layout DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_serializable_object.

    TYPES:
      BEGIN OF ty_s_controls,
        attribute TYPE string,
        control   TYPE control,
        active    TYPE abap_bool,
        others    TYPE abap_bool,
      END OF ty_s_controls.
    TYPES ty_t_controls TYPE STANDARD TABLE OF ty_s_controls WITH EMPTY KEY.

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
    TYPES:   tlabel    TYPE string,
             t_sub_col TYPE ty_t_sub_columns,
           END OF ty_s_positions.
    TYPES ty_t_positions TYPE STANDARD TABLE OF ty_s_positions WITH EMPTY KEY.

    TYPES:
      BEGIN OF ty_s_layout,
        s_head   TYPE ty_s_head,
        t_layout TYPE ty_t_positions,
      END OF ty_s_layout.

    TYPES handle  TYPE c LENGTH 40.
    TYPES control TYPE c LENGTH 10.

    CLASS-DATA ui_table TYPE control VALUE 'ui.Table' ##NO_TEXT.
    CLASS-DATA m_table  TYPE control VALUE 'm.Table' ##NO_TEXT.
    CLASS-DATA others   TYPE control VALUE '' ##NO_TEXT.

    DATA ms_layout       TYPE ty_s_layout.
    DATA ms_layout_tmp   TYPE ty_s_layout.
    DATA mt_comps        TYPE ty_t_positions.
    DATA mt_sub_cols     TYPE ty_t_sub_columns.
    DATA mt_sub_cols_tmp TYPE ty_t_sub_columns.

    CLASS-METHODS get_controls
      RETURNING
        VALUE(result) TYPE z2ui5_cl_layout=>ty_t_controls.

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
                      ( control = z2ui5_cl_layout=>others   attribute = 'WIDTH' ) ).

  ENDMETHOD.

ENDCLASS.
