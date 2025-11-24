CLASS ycl_aaic_rest_rag_doc DEFINITION INHERITING FROM ycl_aaic_rest_resource
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_response_create_s,
             created TYPE abap_bool,
             id      TYPE string,
             error   TYPE string,
           END OF ty_response_create_s.

    METHODS create REDEFINITION.
    METHODS read REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ycl_aaic_rest_rag_doc IMPLEMENTATION.


  METHOD create.

    DATA: lo_request TYPE REF TO if_web_http_request.

    DATA ls_response TYPE ty_response_create_s.

    DATA l_json TYPE string.

    TRY.

        lo_request = i_o_request->get_multipart( index = 1 ).

        DATA(l_file_content) = lo_request->get_text( ).

        DATA(l_filename) = lo_request->get_header_field( '~content_filename' ).

        DATA(l_description) = i_o_request->get_form_field( i_name = 'description' ).

        DATA(l_keywords) = i_o_request->get_form_field( i_name = 'keywords' ).

        NEW ycl_aaic_rag_db( )->create(
          EXPORTING
            i_filename = l_filename
            i_description = l_description
            i_keywords = l_keywords
            i_content  = l_file_content
          IMPORTING
            e_id = DATA(l_id)
        ).

        IF l_id IS NOT INITIAL.

          ls_response-created = abap_true.
          ls_response-id = l_id.

        ELSE.

          ls_response-created = abap_false.
          ls_response-error = 'An error occurred while saving the document in the database.'.

        ENDIF.

        l_json = /ui2/cl_json=>serialize(
          EXPORTING
            data = ls_response
            compress = abap_false
            pretty_name = /ui2/cl_json=>pretty_mode-camel_case
        ).

        i_o_response->set_content_type( content_type = 'application/json' ).

        i_o_response->set_text(
          EXPORTING
            i_text = l_json
        ).

      CATCH cx_web_message_error ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.

  METHOD read.

    TRY.

        i_o_response->set_content_type( content_type = 'text/plain' ).

        i_o_response->set_text(
          EXPORTING
            i_text = |TODO|
        ).

      CATCH cx_web_message_error ##NO_HANDLER.

    ENDTRY.

  ENDMETHOD.
ENDCLASS.
