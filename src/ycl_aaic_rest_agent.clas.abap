CLASS ycl_aaic_rest_agent DEFINITION INHERITING FROM ycl_aaic_rest_resource
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_agent_s,
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
           END OF ty_agent_s,

           ty_agent_t TYPE STANDARD TABLE OF ty_agent_s WITH EMPTY KEY,

           BEGIN OF ty_response_read_s,
             agent TYPE ty_agent_s,
           END OF ty_response_read_s,

           BEGIN OF ty_response_query_s,
             agents TYPE ty_agent_t,
           END OF ty_response_query_s.

    METHODS read REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ycl_aaic_rest_agent IMPLEMENTATION.

  METHOD read.

    DATA: lt_rng_agent_name  TYPE RANGE OF yaaic_agent-name,
          lt_rng_agent_descr TYPE RANGE OF yaaic_agent-description.

    DATA: ls_response_query TYPE ty_response_query_s,
          ls_response_read  TYPE ty_response_read_s.

    DATA: l_json TYPE string.

    DATA(l_agent_id) = to_upper( i_o_request->get_form_field( i_name = 'id' ) ).

    IF l_agent_id IS NOT INITIAL. " Read

      SELECT SINGLE a~id, a~name, a~description, a~sys_inst_id, b~filename as filename_si, b~description as file_si_descr,
                    a~rag_ctx_id, b~filename as filename_ctx, b~description as file_ctx_descr, a~prompt_template
        FROM yaaic_agent AS a
        LEFT OUTER JOIN yaaic_rag AS b
        ON a~sys_inst_id = b~id
        LEFT OUTER JOIN yaaic_rag AS c
        ON a~rag_ctx_id = c~id
        WHERE a~id = @l_agent_id
        INTO @DATA(ls_agent).

      IF sy-subrc <> 0.

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

      l_json = /ui2/cl_json=>serialize(
        EXPORTING
          data = ls_response_read
          compress = abap_false
          pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      ).

    ELSE. " Query

      DATA(l_agent_name) = to_upper( i_o_request->get_form_field( i_name = 'agent_name' ) ).
      DATA(l_agent_description) = to_upper( i_o_request->get_form_field( i_name = 'agent_description' ) ).

      IF l_agent_name IS NOT INITIAL.
        lt_rng_agent_name = VALUE #( ( sign = 'I' option = 'CP' low = |*{ l_agent_name }*| ) ).
      ENDIF.

      IF l_agent_description IS NOT INITIAL.
        lt_rng_agent_descr = VALUE #( ( sign = 'I' option = 'CP' low = |*{ l_agent_description }*| ) ).
      ENDIF.

      SELECT id, name, description, sys_inst_id, rag_ctx_id, prompt_template
        FROM yaaic_agent
        WHERE name IN @lt_rng_agent_name
          AND description IN @lt_rng_agent_descr
        INTO TABLE @DATA(lt_agent).

      IF sy-subrc = 0.
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

ENDCLASS.
