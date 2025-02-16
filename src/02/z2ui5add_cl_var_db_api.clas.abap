CLASS z2ui5add_cl_var_db_api DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS factory
      IMPORTING
        report        TYPE string
      RETURNING
        VALUE(result) TYPE REF TO z2ui5add_cl_var_db_api.

    TYPES ty_s_head TYPE z2ui5_t_13.
    TYPES ty_s_pos TYPE z2ui5_t_14.

    DATA mt_variant TYPE STANDARD TABLE OF ty_S_head WITH DEFAULT KEY.
    METHODS check_default.
    METHODS get_default.

    METHODS db_variant_read
      IMPORTING
        name          TYPE string
      RETURNING
        VALUE(result) TYPE z2ui5_cl_util=>ty_t_filter_multi.

    METHODS db_variant_save
      IMPORTING
        name          TYPE string
      RETURNING
        VALUE(result) TYPE z2ui5_cl_util=>ty_t_filter_multi.

    METHODS db_read.
    METHODS db_save.


    CLASS-METHODS hlp_db_delete_by_handle
      IMPORTING
        uname        TYPE clike OPTIONAL
        handle       TYPE clike OPTIONAL
        handle2      TYPE clike OPTIONAL
        handle3      TYPE clike OPTIONAL
        check_commit TYPE abap_bool DEFAULT abap_true.

    CLASS-METHODS hlp_db_save
      IMPORTING
        uname         TYPE clike OPTIONAL
        handle        TYPE clike OPTIONAL
        handle2       TYPE clike OPTIONAL
        handle3       TYPE clike OPTIONAL
        data          TYPE any
        check_commit  TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(result) TYPE string.

    CLASS-METHODS hlp_db_load_by_id
      IMPORTING
        id            TYPE clike OPTIONAL
      EXPORTING
        VALUE(result) TYPE any.

    CLASS-METHODS hlp_db_load_by_handle
      IMPORTING
        uname         TYPE clike OPTIONAL
        handle        TYPE clike OPTIONAL
        handle2       TYPE clike OPTIONAL
        handle3       TYPE clike OPTIONAL
      EXPORTING
        VALUE(result) TYPE any.


  PROTECTED SECTION.

    DATA:
      BEGIN OF mS_config,
        classname TYPE string,
        name      TYPE string,
        user      TYPE string,
        timestamp TYPE timestamp,
        data      TYPE string,
      END OF mS_config.

    DATA object TYPE REF TO object.
    DATA mt_filter TYPE z2ui5_cl_util=>ty_t_filter_multi.

    METHODS obj_to_filter.
    METHODS filter_to_object.

  PRIVATE SECTION.
ENDCLASS.



CLASS z2ui5add_cl_var_db_api IMPLEMENTATION.



  METHOD hlp_db_delete_by_handle.

    DELETE FROM z2ui5_t_15
        WHERE
           uname = uname
            AND handle = handle
            AND handle2 = handle2
            AND handle3 = handle3.

    IF check_commit = abap_true.
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDMETHOD.


  METHOD hlp_db_load_by_handle.

    DATA lt_db TYPE STANDARD TABLE OF z2ui5_t_15 WITH DEFAULT KEY.
    DATA ls_db LIKE LINE OF lt_db.
    DATA temp1 LIKE LINE OF lt_db.
    DATA temp2 LIKE sy-tabix.

    SELECT data
      FROM z2ui5_t_15 INTO CORRESPONDING FIELDS OF TABLE lt_db
       WHERE
        uname = uname
        AND handle = handle
        AND handle2 = handle2
        AND handle3 = handle3
      .
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE z2ui5_cx_util_error
        EXPORTING
          val = `No entry for handle exists`.
    ENDIF.

    
    
    
    temp2 = sy-tabix.
    READ TABLE lt_db INDEX 1 INTO temp1.
    sy-tabix = temp2.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    ls_db = temp1.

    z2ui5_cl_util=>xml_parse(
      EXPORTING
        xml = ls_db-data
      IMPORTING
        any = result ).

  ENDMETHOD.


  METHOD hlp_db_load_by_id.

    DATA lt_db TYPE STANDARD TABLE OF z2ui5_t_15 WITH DEFAULT KEY.
    DATA ls_db LIKE LINE OF lt_db.
    DATA temp3 LIKE LINE OF lt_db.
    DATA temp4 LIKE sy-tabix.

    SELECT data
      FROM z2ui5_t_15 INTO CORRESPONDING FIELDS OF TABLE lt_db
      WHERE id = id
      .
    ASSERT sy-subrc = 0.

    
    
    
    temp4 = sy-tabix.
    READ TABLE lt_db INDEX 1 INTO temp3.
    sy-tabix = temp4.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.
    ls_db = temp3.

    z2ui5_cl_util=>xml_parse(
      EXPORTING
        xml = ls_db-data
      IMPORTING
        any = result ).

  ENDMETHOD.


  METHOD hlp_db_save.

    DATA lt_db TYPE STANDARD TABLE OF z2ui5_t_15 WITH DEFAULT KEY.
    DATA temp1 TYPE z2ui5_t_15.
    DATA ls_db LIKE temp1.
        DATA temp2 LIKE LINE OF lt_db.
        DATA temp3 LIKE sy-tabix.
    SELECT id
      FROM z2ui5_t_15 INTO CORRESPONDING FIELDS OF TABLE lt_db
       WHERE
        uname = uname
        AND handle = handle
        AND handle2 = handle2
        AND handle3 = handle3
       ##SUBRC_OK.

    
    CLEAR temp1.
    temp1-uname = uname.
    temp1-handle = handle.
    temp1-handle2 = handle2.
    temp1-handle3 = handle3.
    temp1-data = z2ui5_cl_util=>xml_stringify( data ).
    
    ls_db = temp1.

    TRY.
        
        
        temp3 = sy-tabix.
        READ TABLE lt_db INDEX 1 INTO temp2.
        sy-tabix = temp3.
        IF sy-subrc <> 0.
          ASSERT 1 = 0.
        ENDIF.
        ls_db-id = temp2-id.
      CATCH cx_root.
        ls_db-id = z2ui5_cl_util=>uuid_get_c32( ).
    ENDTRY.

    MODIFY z2ui5_t_15 FROM ls_db.
    ASSERT sy-subrc = 0.

    IF check_commit = abap_true.
      COMMIT WORK AND WAIT.
    ENDIF.

    result = ls_db-id.

  ENDMETHOD.




  METHOD obj_to_filter.

  ENDMETHOD.

  METHOD filter_to_object.

  ENDMETHOD.

  METHOD db_read.

  ENDMETHOD.

  METHOD db_save.

  ENDMETHOD.

  METHOD factory.

    CREATE OBJECT result.

    SELECT * FROM z2ui5_t_13 INTO TABLE result->mt_variant
        
        WHERE report = report
        .

  ENDMETHOD.

  METHOD check_default.

  ENDMETHOD.

  METHOD db_variant_read.

  ENDMETHOD.

  METHOD get_default.

  ENDMETHOD.

  METHOD db_variant_save.

  ENDMETHOD.

ENDCLASS.
