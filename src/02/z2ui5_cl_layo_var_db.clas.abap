CLASS z2ui5_cl_layo_var_db DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS factory
      IMPORTING
        report        TYPE string
      RETURNING
        VALUE(result) TYPE REF TO z2ui5_cl_layo_var_db.

    TYPES ty_s_head TYPE z2ui5_t_13.
    TYPES ty_s_pos TYPE z2ui5_t_14.

    DATA mt_variant TYPE STANDARD TABLE OF ty_S_head WITH EMPTY KEY.
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



CLASS z2ui5_cl_layo_var_db IMPLEMENTATION.


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

    SELECT FROM z2ui5_t_13
        FIELDS *
        WHERE report = @report
        INTO TABLE @result->mt_variant.

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
