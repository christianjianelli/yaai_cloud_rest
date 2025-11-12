CLASS ycl_aaic_rest_hello_world DEFINITION INHERITING FROM ycl_aaic_rest_resource
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS read REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ycl_aaic_rest_hello_world IMPLEMENTATION.

  METHOD read.

    TRY.

        i_o_response->set_content_type( content_type = 'text/plain' ).

        i_o_response->set_text(
          EXPORTING
            i_text = |{ TEXT-001 }|
        ).

      CATCH cx_web_message_error ##NO_HANDLER.

    ENDTRY.

  ENDMETHOD.

ENDCLASS.
