CLASS ycl_aaic_rest_resource DEFINITION
  PUBLIC
  ABSTRACT.

  PUBLIC SECTION.

    INTERFACES yif_aaic_rest_resource.

    ALIASES: create FOR yif_aaic_rest_resource~create,
             read FOR yif_aaic_rest_resource~read,
             update FOR yif_aaic_rest_resource~update,
             delete FOR yif_aaic_rest_resource~delete.

  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS _method_not_allowed
      IMPORTING
        i_o_request  TYPE REF TO if_web_http_request
        i_o_response TYPE REF TO if_web_http_response.

ENDCLASS.



CLASS ycl_aaic_rest_resource IMPLEMENTATION.

  METHOD yif_aaic_rest_resource~create.

    me->_method_not_allowed(
      i_o_request  = i_o_request
      i_o_response = i_o_response
    ).

  ENDMETHOD.

  METHOD yif_aaic_rest_resource~read.

    me->_method_not_allowed(
      i_o_request  = i_o_request
      i_o_response = i_o_response
    ).

  ENDMETHOD.

  METHOD yif_aaic_rest_resource~update.

    me->_method_not_allowed(
      i_o_request  = i_o_request
      i_o_response = i_o_response
    ).

  ENDMETHOD.

  METHOD yif_aaic_rest_resource~delete.

    me->_method_not_allowed(
      i_o_request  = i_o_request
      i_o_response = i_o_response
    ).

  ENDMETHOD.

  METHOD _method_not_allowed.

    TRY.

        i_o_response->set_status(
          EXPORTING
            i_code   = 405
        ).

      CATCH cx_web_message_error ##NO_HANDLER.

    ENDTRY.

  ENDMETHOD.

ENDCLASS.
