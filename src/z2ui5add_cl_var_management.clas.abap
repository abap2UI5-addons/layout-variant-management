CLASS z2ui5add_cl_var_management DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS factory
      IMPORTING
        name          TYPE string
        object        TYPE REF TO object
      RETURNING
        VALUE(result) TYPE REF TO z2ui5add_cl_var_management.

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



CLASS z2ui5add_cl_var_management IMPLEMENTATION.

  METHOD obj_to_filter.

  ENDMETHOD.

  METHOD filter_to_object.

  ENDMETHOD.

  METHOD db_read.

  ENDMETHOD.

  METHOD db_save.

  ENDMETHOD.

  METHOD factory.

  ENDMETHOD.

ENDCLASS.
