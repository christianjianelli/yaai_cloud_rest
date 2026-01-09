CLASS ycl_aaic_rest_stats DEFINITION INHERITING FROM ycl_aaic_rest_resource
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS read REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ycl_aaic_rest_stats IMPLEMENTATION.

  METHOD read.

    TYPES: BEGIN OF ty_response_s,
             tools        TYPE i,
             documents    TYPE i,
             agents       TYPE i,
             chats        TYPE i,
             log_messages TYPE i,
             async_tasks  TYPE i,
           END OF ty_response_s.

    DATA ls_response TYPE ty_response_s.

    DATA l_json TYPE string.

    SELECT COUNT( * ) FROM yaaic_tool INTO @ls_response-tools.
    SELECT COUNT( * ) FROM yaaic_rag INTO @ls_response-documents.
    SELECT COUNT( * ) FROM yaaic_agent INTO @ls_response-agents.
    SELECT COUNT( * ) FROM yaaic_chat INTO @ls_response-chats.
    SELECT COUNT( * ) FROM yaaic_log INTO @ls_response-log_messages.
    SELECT COUNT( * ) FROM yaaic_async INTO @ls_response-async_tasks.

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
