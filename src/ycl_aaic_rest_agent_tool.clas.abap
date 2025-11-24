CLASS ycl_aaic_rest_agent_tool DEFINITION INHERITING FROM ycl_aaic_rest_resource
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS create REDEFINITION.
    METHODS read   REDEFINITION.
    METHODS update REDEFINITION.
    METHODS delete REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ycl_aaic_rest_agent_tool IMPLEMENTATION.


  METHOD create.

    TRY.

        i_o_response->set_content_type( content_type = 'text/plain' ).

        i_o_response->set_text(
          EXPORTING
            i_text = |TODO|
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

  METHOD update.

    TRY.

        i_o_response->set_content_type( content_type = 'text/plain' ).

        i_o_response->set_text(
          EXPORTING
            i_text = |TODO|
        ).

      CATCH cx_web_message_error ##NO_HANDLER.

    ENDTRY.

  ENDMETHOD.

  METHOD delete.

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
