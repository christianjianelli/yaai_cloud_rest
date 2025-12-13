CLASS ycl_aaic_rest_agent DEFINITION INHERITING FROM ycl_aaic_rest_resource
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_tool_s,
             class_name  TYPE string,
             method_name TYPE string,
             proxy_class TYPE string,
             description TYPE string,
           END OF ty_tool_s,

           BEGIN OF ty_model_s,
             api            TYPE string,
             model          TYPE yde_aaic_model,
             temperature    TYPE yde_aaic_temperature,
             verbosity      TYPE yde_aaic_verbosity,
             reasoning      TYPE yde_aaic_reasoning_effort,
             max_tool_calls TYPE yde_aaic_max_tool_calls,
           END OF ty_model_s,

           ty_tool_t  TYPE STANDARD TABLE OF ty_tool_s WITH EMPTY KEY,

           ty_model_t TYPE STANDARD TABLE OF ty_model_s WITH EMPTY KEY,

           BEGIN OF ty_agent_s,
             id              TYPE string,
             name            TYPE yde_aaic_agent_name,
             description     TYPE yde_aaic_description,
             sys_inst_id     TYPE string,
             filename_si     TYPE yde_aaic_filename,
             file_si_descr   TYPE yde_aaic_description,
             rag_ctx_id      TYPE string,
             filename_ctx    TYPE yde_aaic_filename,
             file_ctx_descr  TYPE yde_aaic_description,
             prompt_template TYPE yde_aaic_prompt_template,
             tools           TYPE ty_tool_t,
             models          TYPE ty_model_t,
           END OF ty_agent_s,

           ty_agent_t TYPE STANDARD TABLE OF ty_agent_s WITH EMPTY KEY,

           BEGIN OF ty_response_read_s,
             agent TYPE ty_agent_s,
             error TYPE string,
           END OF ty_response_read_s,

           BEGIN OF ty_response_query_s,
             agents TYPE ty_agent_t,
             error  TYPE string,
           END OF ty_response_query_s,

           ty_request_create_s TYPE ty_agent_s,

           BEGIN OF ty_response_create_s,
             created TYPE abap_bool,
             id      TYPE string,
             error   TYPE string,
           END OF ty_response_create_s,

           ty_request_update_s TYPE ty_agent_s,

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



CLASS ycl_aaic_rest_agent IMPLEMENTATION.

  METHOD create.

    DATA: ls_request  TYPE ty_request_create_s,
          ls_response TYPE ty_response_create_s.

    DATA: l_json TYPE string,
          l_id   TYPE yaaic_agent-id.

    TRY.

        DATA(l_body) = i_o_request->get_text( ).

        /ui2/cl_json=>deserialize(
          EXPORTING
            json        = l_body
            pretty_name = /ui2/cl_json=>pretty_mode-camel_case
          CHANGING
            data        = ls_request
        ).

        DATA(lo_agent) = NEW ycl_aaic_agent_db( ).

        lo_agent->create(
          EXPORTING
            i_s_agent        = CORRESPONDING #( ls_request )
            i_t_agent_tools  = CORRESPONDING #( ls_request-tools )
            i_t_agent_models = CORRESPONDING #( ls_request-models )
          IMPORTING
            e_id             = l_id
            e_error          = ls_response-error
        ).

        ls_response-id = l_id.

        IF ls_response-id IS NOT INITIAL AND NOT ls_response-id CO '0 '.
          ls_response-created = abap_true.
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

    DATA: lt_rng_agent_name  TYPE RANGE OF yaaic_agent-name,
          lt_rng_agent_descr TYPE RANGE OF yaaic_agent-description.

    DATA: ls_response_query TYPE ty_response_query_s,
          ls_response_read  TYPE ty_response_read_s.

    DATA: l_json TYPE string.

    DATA(l_agent_id) = to_upper( i_o_request->get_form_field( i_name = 'id' ) ).

    IF l_agent_id IS NOT INITIAL. " Read

      TRY.

          SELECT SINGLE a~id, a~name, a~description, a~sys_inst_id, b~filename AS filename_si, b~description AS file_si_descr,
                        a~rag_ctx_id, b~filename AS filename_ctx, b~description AS file_ctx_descr, a~prompt_template
            FROM yaaic_agent AS a
            LEFT OUTER JOIN yaaic_rag AS b
            ON a~sys_inst_id = b~id
            LEFT OUTER JOIN yaaic_rag AS c
            ON a~rag_ctx_id = c~id
            WHERE a~id = @l_agent_id
            INTO @DATA(ls_agent).

        CATCH cx_sy_open_sql_data_error INTO DATA(lo_ex_sy_open_sql_data_error).

          ls_response_read-error = lo_ex_sy_open_sql_data_error->get_text( ).

      ENDTRY.

      IF ls_agent IS INITIAL.

        TRY.

            "Not Found
            i_o_response->set_status(
              EXPORTING
                i_code = 404
            ).

          CATCH cx_web_message_error ##NO_HANDLER.
        ENDTRY.

      ENDIF.

      ls_response_read-agent = CORRESPONDING #( ls_agent ).

      SELECT class_name, method_name, proxy_class, description
        FROM yaaic_agent_tool
        WHERE id = @l_agent_id
        INTO CORRESPONDING FIELDS OF TABLE @ls_response_read-agent-tools.

      SELECT api, model, temperature, verbosity, reasoning, max_tool_calls
        FROM yaaic_agent_mdl
        WHERE id = @l_agent_id
        INTO CORRESPONDING FIELDS OF TABLE @ls_response_read-agent-models.

      l_json = /ui2/cl_json=>serialize(
        EXPORTING
          data = ls_response_read
          compress = abap_false
          pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      ).

    ELSE. " Query

      DATA(l_agent_name) = i_o_request->get_form_field( i_name = 'agent_name' ).
      DATA(l_agent_description) = i_o_request->get_form_field( i_name = 'agent_description' ).

      IF l_agent_name IS NOT INITIAL.
        lt_rng_agent_name = VALUE #( ( sign = 'I' option = 'CP' low = |*{ l_agent_name }*| ) ).
      ENDIF.

      IF l_agent_description IS NOT INITIAL.
        lt_rng_agent_descr = VALUE #( ( sign = 'I' option = 'CP' low = |*{ l_agent_description }*| ) ).
      ENDIF.

      TRY.

          SELECT a~id, a~name, a~description, a~sys_inst_id, b~filename AS filename_si, b~description AS file_si_descr,
                 a~rag_ctx_id, b~filename AS filename_ctx, b~description AS file_ctx_descr, a~prompt_template
            FROM yaaic_agent AS a
            LEFT OUTER JOIN yaaic_rag AS b
            ON a~sys_inst_id = b~id
            LEFT OUTER JOIN yaaic_rag AS c
            ON a~rag_ctx_id = c~id
            WHERE a~name IN @lt_rng_agent_name
              AND a~description IN @lt_rng_agent_descr
            INTO TABLE @DATA(lt_agent).

        CATCH cx_sy_open_sql_data_error INTO lo_ex_sy_open_sql_data_error.

          ls_response_query-error = lo_ex_sy_open_sql_data_error->get_text( ).

      ENDTRY.

      IF lt_agent IS NOT INITIAL.
        ls_response_query-agents = CORRESPONDING #( lt_agent ).
      ENDIF.

      l_json = /ui2/cl_json=>serialize(
        EXPORTING
          data = ls_response_query
          compress = abap_false
          pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      ).

    ENDIF.

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

    DATA: ls_request  TYPE ty_request_update_s,
          ls_response TYPE ty_response_update_s.

    DATA: l_json TYPE string,
          l_id   TYPE yaaic_agent-id.

    TRY.

        DATA(l_body) = i_o_request->get_text( ).

        /ui2/cl_json=>deserialize(
          EXPORTING
            json        = l_body
            pretty_name = /ui2/cl_json=>pretty_mode-camel_case
          CHANGING
            data        = ls_request
        ).

        DATA(lo_rag) = NEW ycl_aaic_rag_db( ).

        IF ls_request-filename_si IS NOT INITIAL.

          lo_rag->read(
            EXPORTING
              i_filename    = ls_request-filename_si
            IMPORTING
              e_id          = DATA(l_sys_inst_id)
          ).

          ls_request-sys_inst_id = l_sys_inst_id.

        ENDIF.

        IF ls_request-filename_ctx IS NOT INITIAL.

          lo_rag->read(
            EXPORTING
              i_filename    = ls_request-filename_ctx
            IMPORTING
              e_id          = DATA(l_rag_ctx_id)
          ).

          ls_request-rag_ctx_id = l_rag_ctx_id.

        ENDIF.

        DATA(lo_agent) = NEW ycl_aaic_agent_db( ).

        lo_agent->update(
          EXPORTING
            i_s_agent        = CORRESPONDING #( ls_request )
            i_t_agent_tools  = CORRESPONDING #( ls_request-tools )
            i_t_agent_models = CORRESPONDING #( ls_request-models )
          IMPORTING
            e_updated        = ls_response-updated
            e_error          = ls_response-error
        ).

        ls_response-id = l_id.

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

    DATA ls_response_delete TYPE ty_response_delete_s.

    DATA l_json TYPE string.

    DATA(l_agent_id) = to_upper( i_o_request->get_form_field( i_name = 'id' ) ).

    ls_response_delete-id = l_agent_id.

    DATA(lo_agent) = NEW ycl_aaic_agent_db( ).

    lo_agent->delete(
      EXPORTING
        i_agent_id = CONV #( ls_response_delete-id )
      IMPORTING
        e_deleted  = ls_response_delete-deleted
        e_error    = ls_response_delete-error
    ).


    l_json = /ui2/cl_json=>serialize(
      EXPORTING
        data = ls_response_delete
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
