CLASS ycl_aaic_rest_handler DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_service_extension.

    CONSTANTS: mc_interface_name       TYPE c LENGTH 30 VALUE 'YCL_AAIC_REST_RESOURCE' ##NO_TEXT,
               mc_rest_resource_prefix TYPE c LENGTH 30 VALUE 'YCL_AAIC_REST' ##NO_TEXT,
               mc_create               TYPE string VALUE 'CREATE' ##NO_TEXT,
               mc_read                 TYPE string VALUE 'READ' ##NO_TEXT,
               mc_update               TYPE string VALUE 'UPDATE' ##NO_TEXT,
               mc_delete               TYPE string VALUE 'DELETE' ##NO_TEXT.

    METHODS get_rest_resource_instance
      IMPORTING
        i_resource   TYPE csequence
      EXPORTING
        e_o_resource TYPE REF TO object
        e_error      TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ycl_aaic_rest_handler IMPLEMENTATION.


  METHOD if_http_service_extension~handle_request.

    DATA(l_path_info) = request->get_header_field( i_name = '~path_info' ).

    SPLIT l_path_info AT '/' INTO TABLE DATA(lt_path_info).

    IF lt_path_info[] IS INITIAL.

      TRY.

          response->set_status(
            EXPORTING
              i_code = 404
          ).

        CATCH cx_web_message_error ##NO_HANDLER.
      ENDTRY.

      RETURN.

    ENDIF.

    " Logoff
    IF to_lower( lt_path_info[ 1 ] ) = 'logoff'.

      TRY.

          cl_web_http_server_utility=>logoff(
            EXPORTING
              redirect_url              = '/sap/public/bc/icf/logoff'
              request                   = request
          ).

        CATCH cx_logoff_failed ##NO_HANDLER.
      ENDTRY.

      RETURN.

    ENDIF.

    " Stateless or Statefull session
    cl_web_http_server_utility=>set_session_stateful(
      stateful = COND #( WHEN request->get_form_field( i_name = 'statefull' ) IS NOT INITIAL
                         THEN cl_web_http_server_utility=>co_enabled
                         ELSE cl_web_http_server_utility=>co_disabled )
      path     = ''
      request  = request
    ).

    " Get Resource Instance
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    me->get_rest_resource_instance(
      EXPORTING
        i_resource   = lt_path_info[ 1 ]
      IMPORTING
        e_o_resource = DATA(lo_resource)
    ).

    IF lo_resource IS NOT BOUND.

      TRY.

          response->set_status(
            EXPORTING
              i_code = 404
          ).

        CATCH cx_web_message_error ##NO_HANDLER.
      ENDTRY.

      RETURN.

    ENDIF.

    DATA(l_method) = to_upper( request->get_method( ) ).

    DATA(l_method_name) = COND string( WHEN l_method = 'POST'   THEN mc_create
                                       WHEN l_method = 'GET'    THEN mc_read
                                       WHEN l_method = 'PUT'    THEN mc_update
                                       WHEN l_method = 'PATCH'  THEN mc_update
                                       WHEN l_method = 'DELETE' THEN mc_delete
                                       ELSE space ).

    IF l_method_name = space.

      TRY.

          response->set_status(
            EXPORTING
              i_code = 405
          ).

        CATCH cx_web_message_error ##NO_HANDLER.

      ENDTRY.

      RETURN.

    ENDIF.

    " Call Handler
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    TRY.

        CALL METHOD lo_resource->(l_method_name)
          EXPORTING
            i_o_request  = request
            i_o_response = response.

        RETURN.

      CATCH cx_sy_dyn_call_illegal_method
            cx_sy_dyn_call_illegal_type
            cx_sy_dyn_call_param_missing
            cx_sy_dyn_call_param_not_found
            cx_sy_ref_is_initial INTO DATA(lo_exception).

        DATA(l_error) = lo_exception->get_text( ).

    ENDTRY.

    TRY.

        response->set_status(
          EXPORTING
            i_code   = 500
            i_reason = l_error
        ).

      CATCH cx_web_message_error ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD get_rest_resource_instance.

    CLEAR: e_o_resource,
           e_error.

    DATA(lo_class) = xco_cp_abap=>class( mc_interface_name ).

    DATA(lt_subclasses) = lo_class->subclasses->all->get_names( ).

    LOOP AT lt_subclasses INTO DATA(l_subclass).

      IF l_subclass = |{ mc_rest_resource_prefix }_{ to_upper( i_resource ) }|.

        TRY.

            CREATE OBJECT e_o_resource TYPE (l_subclass).

          CATCH cx_sy_create_object_error
                cx_sy_dyn_call_illegal_class
                cx_sy_dyn_call_illegal_method
                cx_sy_dyn_call_illegal_type
                cx_sy_dyn_call_param_missing
                cx_sy_dyn_call_param_not_found
                cx_sy_ref_is_initial INTO DATA(lo_ex_create_object).

            e_error = lo_ex_create_object->get_text( ).

            RETURN.

        ENDTRY.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
