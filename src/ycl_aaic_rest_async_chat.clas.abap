CLASS ycl_aaic_rest_async_chat DEFINITION INHERITING FROM ycl_aaic_rest_resource
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_request_create_s,
             chat_id  TYPE string,
             api      TYPE string,
             api_key  TYPE string,
             message  TYPE string,
             context  TYPE string,
             agent_id TYPE string,
             model    TYPE string,
           END OF ty_request_create_s,

           BEGIN OF ty_response_create_s,
             chat_id TYPE string,
             task_id TYPE string,
             created TYPE abap_bool,
             error   TYPE string,
           END OF ty_response_create_s,

           BEGIN OF ty_request_read_s,
             chat_id TYPE string,
             task_id TYPE string,
           END OF ty_request_read_s,

           BEGIN OF ty_response_read_s,
             chat_id TYPE string,
             task_id TYPE string,
             status  TYPE string,
           END OF ty_response_read_s.

    METHODS create REDEFINITION.

    METHODS read REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS:
      _run_openai
        IMPORTING
          i_s_request   TYPE ty_request_create_s
        CHANGING
          ch_s_response TYPE ty_response_create_s,

      _run_anthropic
        IMPORTING
          i_s_request   TYPE ty_request_create_s
        CHANGING
          ch_s_response TYPE ty_response_create_s,

      _run_google
        IMPORTING
          i_s_request   TYPE ty_request_create_s
        CHANGING
          ch_s_response TYPE ty_response_create_s,

      _run_mistral
        IMPORTING
          i_s_request   TYPE ty_request_create_s
        CHANGING
          ch_s_response TYPE ty_response_create_s.

ENDCLASS.



CLASS ycl_aaic_rest_async_chat IMPLEMENTATION.

  METHOD create.

    DATA: lo_aaic_db TYPE REF TO ycl_aaic_db.

    DATA: ls_request  TYPE ty_request_create_s,
          ls_response TYPE ty_response_create_s.

    DATA: l_json    TYPE string,
          l_task_id TYPE yaaic_async-id.

    DATA(l_request_body) = i_o_request->get_text( ).

    /ui2/cl_json=>deserialize(
      EXPORTING
        json        = l_request_body
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      CHANGING
        data        = ls_request
    ).

    ls_request-chat_id = to_upper( ls_request-chat_id ).
    ls_request-api = to_upper( ls_request-api ).

    CASE ls_request-api.

      WHEN yif_aaic_const=>c_openai. " OpenAI

        me->_run_openai(
          EXPORTING
            i_s_request   = ls_request
          CHANGING
            ch_s_response = ls_response
        ).

      WHEN yif_aaic_const=>c_anthropic. " Anthropic

        me->_run_anthropic(
          EXPORTING
            i_s_request   = ls_request
          CHANGING
            ch_s_response = ls_response
        ).

      WHEN yif_aaic_const=>c_google. " Google

        me->_run_google(
          EXPORTING
            i_s_request   = ls_request
          CHANGING
            ch_s_response = ls_response
        ).

      WHEN yif_aaic_const=>c_mistral. " Mistral

        me->_run_mistral(
          EXPORTING
            i_s_request   = ls_request
          CHANGING
            ch_s_response = ls_response
        ).

    ENDCASE.

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

  METHOD read.

    DATA: ls_request  TYPE ty_request_read_s,
          ls_response TYPE ty_response_read_s.

    DATA: l_json TYPE string.

    ls_request-task_id = to_upper( i_o_request->get_form_field( i_name = 'task_id' ) ).

    DATA(lo_async_task) = NEW ycl_aaic_async( ).

    DATA(l_process_state) = lo_async_task->get_process_state(
      EXPORTING
        i_task_id = CONV #( ls_request-task_id )
    ).

    lo_async_task->update_status(
      EXPORTING
        i_task_id       = CONV #( ls_request-task_id )
        i_process_state = l_process_state
    ).

    ls_response-status = lo_async_task->get_status(
      EXPORTING
        i_task_id = CONV #( ls_request-task_id ) ).

    ls_response-task_id = ls_request-task_id.

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

  METHOD _run_anthropic.

    DATA(lo_aaic_db) = NEW ycl_aaic_db( i_api = yif_aaic_const=>c_anthropic
                                        i_id = CONV #( i_s_request-chat_id ) ).

    ch_s_response-chat_id = lo_aaic_db->m_id.

    DATA(lo_aaic_async) = NEW ycl_aaic_async( ).

    DATA(l_task_id) = lo_aaic_async->create(
      EXPORTING
        i_chat_id   = lo_aaic_db->m_id
        i_task_name = 'ANTHROPIC_ASYNC_CHAT'
    ).

    ch_s_response-task_id = l_task_id.

    DATA(lo_async_chat_anthropic) = NEW ycl_aaic_async_chat_anthropic(
      i_task_id  = l_task_id
      i_chat_id  = CONV #( ch_s_response-chat_id )
      i_api_key  = i_s_request-api_key
      i_message  = i_s_request-message
      i_model    = i_s_request-model
      i_agent_id = i_s_request-agent_id
      i_context  = i_s_request-context
    ).

    DATA(l_started) = lo_aaic_async->run(
      EXPORTING
        i_task_id = l_task_id
        i_o_task  = lo_async_chat_anthropic
    ).

    IF l_started = abap_false.
      ch_s_response-error = 'Error while creating the Anthropic Async Chat'.
    ELSE.
      ch_s_response-created = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD _run_google.

    DATA(lo_aaic_db) = NEW ycl_aaic_db( i_api = yif_aaic_const=>c_google
                                        i_id = CONV #( i_s_request-chat_id ) ).

    ch_s_response-chat_id = lo_aaic_db->m_id.

    DATA(lo_aaic_async) = NEW ycl_aaic_async( ).

    DATA(l_task_id) = lo_aaic_async->create(
      EXPORTING
        i_chat_id   = lo_aaic_db->m_id
        i_task_name = 'GOOGLE_ASYNC_CHAT'
    ).

    ch_s_response-task_id = l_task_id.

    DATA(lo_async_chat_google) = NEW ycl_aaic_async_chat_google(
      i_task_id  = l_task_id
      i_chat_id  = CONV #( ch_s_response-chat_id )
      i_api_key  = i_s_request-api_key
      i_message  = i_s_request-message
      i_model    = i_s_request-model
      i_agent_id = i_s_request-agent_id
      i_context  = i_s_request-context
    ).

    DATA(l_started) = lo_aaic_async->run(
      EXPORTING
        i_task_id = l_task_id
        i_o_task  = lo_async_chat_google
    ).

    IF l_started = abap_false.
      ch_s_response-error = 'Error while creating the Google Async Chat'.
    ELSE.
      ch_s_response-created = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD _run_mistral.

    DATA(lo_aaic_db) = NEW ycl_aaic_db( i_api = yif_aaic_const=>c_mistral
                                        i_id = CONV #( i_s_request-chat_id ) ).

    ch_s_response-chat_id = lo_aaic_db->m_id.

    DATA(lo_aaic_async) = NEW ycl_aaic_async( ).

    DATA(l_task_id) = lo_aaic_async->create(
      EXPORTING
        i_chat_id   = lo_aaic_db->m_id
        i_task_name = 'MISTRAL_ASYNC_CHAT'
    ).

    ch_s_response-task_id = l_task_id.

    DATA(lo_async_chat_mistral) = NEW ycl_aaic_async_chat_mistral(
      i_task_id  = l_task_id
      i_chat_id  = CONV #( ch_s_response-chat_id )
      i_api_key  = i_s_request-api_key
      i_message  = i_s_request-message
      i_model    = i_s_request-model
      i_agent_id = i_s_request-agent_id
      i_context  = i_s_request-context
    ).

    DATA(l_started) = lo_aaic_async->run(
      EXPORTING
        i_task_id = l_task_id
        i_o_task  = lo_async_chat_mistral
    ).

    IF l_started = abap_false.
      ch_s_response-error = 'Error while creating the Mistral Async Chat'.
    ELSE.
      ch_s_response-created = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD _run_openai.

    DATA(lo_aaic_db) = NEW ycl_aaic_db( i_api = yif_aaic_const=>c_openai
                                        i_id = CONV #( i_s_request-chat_id ) ).

    ch_s_response-chat_id = lo_aaic_db->m_id.

    DATA(lo_aaic_async) = NEW ycl_aaic_async( ).

    DATA(l_task_id) = lo_aaic_async->create(
      EXPORTING
        i_chat_id   = lo_aaic_db->m_id
        i_task_name = 'OPENAI_ASYNC_CHAT'
    ).

    ch_s_response-task_id = l_task_id.

    DATA(lo_async_chat_openai) = NEW ycl_aaic_async_chat_openai(
      i_task_id  = l_task_id
      i_chat_id  = CONV #( ch_s_response-chat_id )
      i_api_key  = i_s_request-api_key
      i_message  = i_s_request-message
      i_model    = i_s_request-model
      i_agent_id = i_s_request-agent_id
      i_context  = i_s_request-context
    ).

    DATA(l_started) = lo_aaic_async->run(
      EXPORTING
        i_task_id = l_task_id
        i_o_task  = lo_async_chat_openai
    ).

    IF l_started = abap_false.
      ch_s_response-error = 'Error while creating the OpenAI Async Chat'.
    ELSE.
      ch_s_response-created = abap_true.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
