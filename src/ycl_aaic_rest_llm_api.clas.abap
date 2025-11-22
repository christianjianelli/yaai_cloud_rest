CLASS ycl_aaic_rest_llm_api DEFINITION INHERITING FROM ycl_aaic_rest_resource
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS read REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ycl_aaic_rest_llm_api IMPLEMENTATION.


  METHOD read.

    DATA lt_models TYPE SORTED TABLE OF yaaic_model
       WITH UNIQUE KEY id.

    TYPES: BEGIN OF ty_model_s,
             model         TYPE string,
             default_model TYPE abap_bool,
           END OF ty_model_s,

           ty_model_t TYPE STANDARD TABLE OF ty_model_s WITH EMPTY KEY,

           BEGIN OF ty_api_s,
             id       TYPE string,
             base_url TYPE string,
             models   TYPE ty_model_t,
           END OF ty_api_s,

           ty_api_t TYPE STANDARD TABLE OF ty_api_s WITH EMPTY KEY,

           BEGIN OF ty_response_s,
             apis  TYPE ty_api_t,
             error TYPE string,
           END OF ty_response_s.


    DATA: ls_api      TYPE ty_api_s,
          ls_response TYPE ty_response_s.

    DATA l_json TYPE string.

    DATA(l_id) = to_upper( i_o_request->get_form_field( i_name = 'id' ) ).

    IF l_id IS NOT INITIAL.

      APPEND INITIAL LINE TO ls_response-apis ASSIGNING FIELD-SYMBOL(<ls_api>).

      SELECT SINGLE id, base_url
        FROM yaaic_api
        WHERE id = @l_id
        INTO CORRESPONDING FIELDS OF @<ls_api>.

      SELECT model, default_model
        FROM yaaic_model
        WHERE id = @l_id
        INTO CORRESPONDING FIELDS OF TABLE @<ls_api>-models.

    ELSE.

      SELECT id, base_url
        FROM yaaic_api
        INTO TABLE @DATA(lt_apis).

      SELECT id, model, default_model
        FROM yaaic_model
        ORDER BY PRIMARY KEY
        INTO CORRESPONDING FIELDS OF TABLE @lt_models.

      LOOP AT lt_apis ASSIGNING FIELD-SYMBOL(<ls_api_db>).

        APPEND INITIAL LINE TO ls_response-apis ASSIGNING <ls_api>.

        <ls_api>-id = <ls_api_db>-id.
        <ls_api>-base_url = <ls_api_db>-base_url.

        LOOP AT lt_models ASSIGNING FIELD-SYMBOL(<ls_model_db>)
          WHERE id = <ls_api_db>-id.

          APPEND VALUE #( model = <ls_model_db>-model
                          default_model = <ls_model_db>-default_model ) TO <ls_api>-models.

        ENDLOOP.

      ENDLOOP.

    ENDIF.

    l_json = /ui2/cl_json=>serialize(
     EXPORTING
       data = ls_response
       compress         = abap_false
*       name             =
       pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
*       type_descr       =
*       assoc_arrays     =
*       ts_as_iso8601    =
*       expand_includes  =
*       assoc_arrays_opt =
*       numc_as_string   =
*       name_mappings    =
*       conversion_exits =
    ).

    TRY.

        IF ls_response-apis IS INITIAL.

          i_o_response->set_status(
            EXPORTING
              i_code = 404
          ).

          RETURN.

        ENDIF.

        i_o_response->set_content_type( content_type = 'application/json' ).

        i_o_response->set_text(
          EXPORTING
            i_text = l_json
        ).

      CATCH cx_web_message_error ##NO_HANDLER.

        "TODO: log

    ENDTRY.

  ENDMETHOD.
ENDCLASS.
