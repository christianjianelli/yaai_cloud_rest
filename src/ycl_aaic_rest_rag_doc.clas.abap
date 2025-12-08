CLASS ycl_aaic_rest_rag_doc DEFINITION INHERITING FROM ycl_aaic_rest_resource
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_response_create_s,
             created TYPE abap_bool,
             id      TYPE string,
             error   TYPE string,
           END OF ty_response_create_s,

           BEGIN OF ty_response_read_s,
             document TYPE yif_aaic_rag_db=>ty_document_s,
             error    TYPE string,
           END OF ty_response_read_s,

           BEGIN OF ty_response_query_s,
             documents TYPE yif_aaic_rag_db=>ty_documents_t,
           END OF ty_response_query_s,

           BEGIN OF ty_response_update_s,
             updated TYPE abap_bool,
             id      TYPE string,
             error   TYPE string,
           END OF ty_response_update_s,

           BEGIN OF ty_response_delete_s,
             deleted TYPE abap_bool,
             id      TYPE string,
             error   TYPE string,
           END OF ty_response_delete_s.

    METHODS create REDEFINITION.
    METHODS read REDEFINITION.
    METHODS update REDEFINITION.
    METHODS delete REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS YCL_AAIC_REST_RAG_DOC IMPLEMENTATION.


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

    DATA: ls_response_query TYPE ty_response_query_s,
          ls_response_read  TYPE ty_response_read_s.

    DATA l_json TYPE string.

    DATA(l_id) = i_o_request->get_form_field( i_name = 'id' ).
    DATA(l_filename) = i_o_request->get_form_field( i_name = 'filename' ).
    DATA(l_description) = i_o_request->get_form_field( i_name = 'description' ).
    DATA(l_keywords) = i_o_request->get_form_field( i_name = 'keywords' ).

    IF l_id IS INITIAL.

      NEW ycl_aaic_rag_db( )->query(
        EXPORTING
          i_filename    = l_filename
          i_description = l_description
          i_keywords    = l_keywords
        IMPORTING
          e_t_documents = DATA(lt_documents)
      ).

      ls_response_query-documents = lt_documents.

      l_json = /ui2/cl_json=>serialize(
        EXPORTING
          data = ls_response_query
          compress = abap_false
          pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      ).

      TRY.

          i_o_response->set_content_type( content_type = 'application/json' ).

          i_o_response->set_text(
            EXPORTING
              i_text = l_json
          ).

        CATCH cx_web_message_error ##NO_HANDLER.

      ENDTRY.

      RETURN.

    ENDIF.

    NEW ycl_aaic_rag_db( )->read(
      EXPORTING
        i_id          = CONV #( l_id )
      IMPORTING
        e_filename    = l_filename
        e_description = l_description
        e_keywords    = l_keywords
        e_content     = DATA(l_content)
        e_error       = DATA(l_error)
    ).

    ls_response_read-document = VALUE #( id = l_id
                                         filename = l_filename
                                         description = l_description
                                         keywords = l_keywords
                                         content = l_content ).

    l_json = /ui2/cl_json=>serialize(
      EXPORTING
        data = ls_response_read
        compress = abap_false
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
    ).

    TRY.

        i_o_response->set_content_type( content_type = 'application/json' ).

        i_o_response->set_text(
          EXPORTING
            i_text = l_json
        ).

      CATCH cx_web_message_error ##NO_HANDLER.

    ENDTRY.

  ENDMETHOD.


  METHOD update.

    DATA: lo_request TYPE REF TO if_web_http_request.

    DATA ls_response TYPE ty_response_update_s.

    DATA l_json TYPE string.

    TRY.

        lo_request = i_o_request->get_multipart( index = 1 ).

        DATA(l_file_content) = lo_request->get_text( ).

        DATA(l_id) = i_o_request->get_form_field( i_name = 'id' ).

        DATA(l_description) = i_o_request->get_form_field( i_name = 'description' ).

        DATA(l_keywords) = i_o_request->get_form_field( i_name = 'keywords' ).

        NEW ycl_aaic_rag_db( )->update(
          EXPORTING
            i_id          = CONV #( l_id )
            i_description = l_description
            i_keywords    = l_keywords
            i_content     = l_file_content
          IMPORTING
            e_updated     = ls_response-updated
            e_error       = ls_response-error
        ).

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


  METHOD delete.

    DATA ls_response TYPE ty_response_delete_s.

    DATA l_json TYPE string.

    DATA(l_id) = i_o_request->get_form_field( i_name = 'id' ).

    IF l_id IS INITIAL.
      RETURN.
    ENDIF.

    ls_response-id = l_id.

    NEW ycl_aaic_rag_db( )->delete(
      EXPORTING
        i_id       = CONV #( l_id )
      IMPORTING
        e_deleted  = ls_response-deleted
        e_error    = ls_response-error
    ).

    l_json = /ui2/cl_json=>serialize(
      EXPORTING
        data = ls_response
        compress = abap_false
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
    ).

    TRY.

        i_o_response->set_content_type( content_type = 'application/json' ).

        i_o_response->set_text(
          EXPORTING
            i_text = l_json
        ).

      CATCH cx_web_message_error ##NO_HANDLER.

    ENDTRY.

  ENDMETHOD.
ENDCLASS.
