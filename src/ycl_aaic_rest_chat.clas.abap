CLASS ycl_aaic_rest_chat DEFINITION INHERITING FROM ycl_aaic_rest_resource
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_msg_s,
             seqno    TYPE yde_aaic_seqno,
             msg      TYPE yde_aaic_chat_msg,
             msg_date TYPE yde_aaic_msg_date,
             msg_time TYPE yde_aaic_msg_time,
           END OF ty_msg_s,

           ty_msg_t TYPE STANDARD TABLE OF ty_msg_s WITH EMPTY KEY,

           BEGIN OF ty_chat_query_s,
             id         TYPE string,
             api        TYPE yde_aaic_api,
             username   TYPE usnam,
             chat_date  TYPE yde_aaic_chat_date,
             chat_time  TYPE yde_aaic_chat_time,
             max_seq_no TYPE i,
             blocked    TYPE abap_bool,
           END OF ty_chat_query_s,

           ty_chat_t TYPE STANDARD TABLE OF ty_chat_query_s WITH EMPTY KEY,

           BEGIN OF ty_chat_s,
             id         TYPE string,
             api        TYPE yde_aaic_api,
             username   TYPE usnam,
             chat_date  TYPE yde_aaic_chat_date,
             chat_time  TYPE yde_aaic_chat_time,
             max_seq_no TYPE i,
             blocked    TYPE abap_bool,
             messages   TYPE ty_msg_t,
           END OF ty_chat_s,

           BEGIN OF ty_response_read_s,
             chat TYPE ty_chat_s,
           END OF ty_response_read_s,

           BEGIN OF ty_response_query_s,
             chats TYPE ty_chat_t,
           END OF ty_response_query_s,

           BEGIN OF ty_chat_update_s,
             id      TYPE string,
             updated TYPE abap_bool,
             error   TYPE string,
           END OF ty_chat_update_s,

           BEGIN OF ty_chat_delete_s,
             id      TYPE string,
             deleted TYPE abap_bool,
             error   TYPE string,
           END OF ty_chat_delete_s.

    METHODS create REDEFINITION.

    METHODS read REDEFINITION.

    METHODS update REDEFINITION.

    METHODS delete REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ycl_aaic_rest_chat IMPLEMENTATION.

  METHOD create.

    TRY.

        i_o_response->set_content_type( content_type = 'application/json' ).

        i_o_response->set_text(
          EXPORTING
            i_text = CONV #( '{"state":"processing"}' )
        ).

      CATCH cx_web_message_error ##NO_HANDLER.

    ENDTRY.

  ENDMETHOD.

  METHOD read.

    DATA: lt_rng_username  TYPE RANGE OF yaaic_log-username,
          lt_rng_chat_date TYPE RANGE OF yaaic_log-log_date.

    DATA: ls_response_query TYPE ty_response_query_s,
          ls_response_read  TYPE ty_response_read_s.

    DATA: l_chat_date_from TYPE yaaic_log-log_date,
          l_chat_date_to   TYPE yaaic_log-log_date,
          l_json           TYPE string.

    DATA(l_id) = to_upper( i_o_request->get_form_field( i_name = 'id' ) ).

    IF l_id IS NOT INITIAL. " Read

      SELECT SINGLE id ,api ,username ,chat_date ,chat_time, blocked
        FROM yaaic_chat
        WHERE id = @l_id
        INTO @DATA(ls_chat).

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

      ls_response_read-chat = CORRESPONDING #( ls_chat ).

      SELECT id , seqno, msg, msg_date, msg_time
        FROM yaaic_msg
        WHERE id = @l_id
        ORDER BY id, seqno
        INTO TABLE @DATA(lt_msg).

      IF sy-subrc = 0.
        ls_response_read-chat-messages = CORRESPONDING #( lt_msg ).
      ENDIF.

      LOOP AT lt_msg ASSIGNING FIELD-SYMBOL(<ls_msg>).
        ls_response_read-chat-max_seq_no = <ls_msg>-seqno.
      ENDLOOP.

      l_json = /ui2/cl_json=>serialize(
        EXPORTING
          data = ls_response_read
          compress = abap_false
          pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      ).

    ELSE. " Query

      DATA(l_datefrom) = i_o_request->get_form_field( i_name = 'datefrom' ).
      DATA(l_dateto) = i_o_request->get_form_field( i_name = 'dateto' ).
      DATA(l_username) = to_upper( i_o_request->get_form_field( i_name = 'username' ) ).

      IF l_datefrom IS NOT INITIAL AND l_dateto IS NOT INITIAL.
        lt_rng_chat_date = VALUE #( ( sign = 'I' option = 'BT' low = l_datefrom high = l_dateto ) ).
      ELSEIF l_datefrom IS NOT INITIAL AND l_dateto IS INITIAL.
        lt_rng_chat_date = VALUE #( ( sign = 'I' option = 'EQ' low = l_datefrom ) ).
      ENDIF.

      IF l_username IS NOT INITIAL.
        lt_rng_username = VALUE #( ( sign = 'I' option = 'EQ' low = l_username ) ).
      ENDIF.

      SELECT a~id, a~api, a~username, a~chat_date, a~chat_time, a~blocked, MAX( b~seqno ) AS max_seq_no
        FROM yaaic_chat AS a
        LEFT OUTER JOIN yaaic_msg AS b
        ON a~id = b~id
        WHERE chat_date IN @lt_rng_chat_date
        AND username IN @lt_rng_username
        GROUP BY a~id, a~api, a~username, a~chat_date, a~chat_time, a~blocked
        INTO TABLE @DATA(lt_chat).

      IF sy-subrc = 0.
        ls_response_query-chats = CORRESPONDING #( lt_chat ).
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

    DATA ls_response_update TYPE ty_chat_update_s.

    DATA l_json TYPE string.

    ls_response_update-id = to_upper( i_o_request->get_form_field( i_name = 'chat_id' ) ).
    DATA(l_action) = to_upper( i_o_request->get_form_field( i_name = 'action' ) ).

    IF ls_response_update-id IS INITIAL.

      TRY.

          "Not Found
          i_o_response->set_status(
            EXPORTING
              i_code = 404
          ).

        CATCH cx_web_message_error ##NO_HANDLER.
      ENDTRY.

      RETURN.

    ENDIF.

    CASE l_action.

      WHEN 'BLOCK'.

        NEW ycl_aaic_db(
          i_api     = space
          i_id      = CONV #( ls_response_update-id )
        )->block_chat(
          IMPORTING
            e_blocked = ls_response_update-updated
        ).

        IF ls_response_update-updated = abap_false.
          ls_response_update-error = 'Error while trying to block the chat.'.
        ENDIF.

      WHEN 'RELEASE'.

        NEW ycl_aaic_db(
          i_api     = space
          i_id      = CONV #( ls_response_update-id )
        )->release_chat(
          IMPORTING
            e_released = ls_response_update-updated
        ).

        IF ls_response_update-updated = abap_false.
          ls_response_update-error = 'Error while trying to release the chat.'.
        ENDIF.

    ENDCASE.

    l_json = /ui2/cl_json=>serialize(
      EXPORTING
        data = ls_response_update
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

  METHOD delete.

    DATA ls_response_delete TYPE ty_chat_delete_s.

    DATA l_json TYPE string.

    ls_response_delete-id = to_upper( i_o_request->get_form_field( i_name = 'chat_id' ) ).

    IF ls_response_delete-id IS INITIAL.

      TRY.

          "Not Found
          i_o_response->set_status(
            EXPORTING
              i_code = 404
          ).

        CATCH cx_web_message_error ##NO_HANDLER.
      ENDTRY.

      RETURN.

    ENDIF.

    NEW ycl_aaic_db(
      i_api     = space
      i_id      = CONV #( ls_response_delete-id )
    )->delete_chat(
      IMPORTING
        e_deleted = ls_response_delete-deleted
    ).

    IF ls_response_delete-deleted = abap_false.
      ls_response_delete-error = 'Error while trying to delete the chat.'.
    ENDIF.

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
