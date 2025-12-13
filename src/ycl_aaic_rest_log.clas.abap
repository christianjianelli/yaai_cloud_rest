CLASS ycl_aaic_rest_log DEFINITION INHERITING FROM ycl_aaic_rest_resource
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_log_s,
             id       TYPE string,
             seqno    TYPE yde_aaic_seqno,
             message  TYPE yde_aaic_log_message,
             username TYPE usnam,
             log_date TYPE yde_aaic_chat_date,
             log_time TYPE yde_aaic_chat_time,
             msgid    TYPE symsgid,
             msgno    TYPE symsgno,
             msgty    TYPE bapi_mtype,
           END OF ty_log_s,

           ty_log_t TYPE STANDARD TABLE OF ty_log_s WITH EMPTY KEY,

           BEGIN OF ty_response_read_s,
             message TYPE ty_log_s,
           END OF ty_response_read_s,

           BEGIN OF ty_response_query_s,
             messages TYPE ty_log_t,
           END OF ty_response_query_s.

    METHODS read REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ycl_aaic_rest_log IMPLEMENTATION.

  METHOD read.

    DATA: lt_rng_id       TYPE RANGE OF yaaic_log-id,
          lt_rng_log_date TYPE RANGE OF yaaic_log-log_date,
          lt_rng_username TYPE RANGE OF yaaic_log-username.

    DATA: ls_response_read  TYPE ty_response_read_s,
          ls_response_query TYPE ty_response_query_s.

    DATA l_json TYPE string.

    DATA(l_id) = to_upper( i_o_request->get_form_field( i_name = 'id' ) ).
    DATA(l_seqno) = i_o_request->get_form_field( i_name = 'seqno' ).

    IF l_id IS NOT INITIAL AND l_seqno IS NOT INITIAL.

      SELECT SINGLE id, seqno, message, username, log_date, log_time, msgid, msgno, msgty
        FROM yaaic_log
          WHERE id = @l_id
            AND seqno = @l_seqno
            INTO @DATA(ls_log).

      IF sy-subrc = 0.
        ls_response_read-message = CORRESPONDING #( ls_log ).
      ENDIF.

    ELSE.

      DATA(l_datefrom) = i_o_request->get_form_field( i_name = 'datefrom' ).
      DATA(l_dateto) = i_o_request->get_form_field( i_name = 'dateto' ).
      DATA(l_username) = i_o_request->get_form_field( i_name = 'username' ).

      IF l_datefrom IS NOT INITIAL AND l_dateto IS NOT INITIAL.
        lt_rng_log_date = VALUE #( ( sign = 'I' option = 'BT' low = l_datefrom high = l_dateto ) ).
      ELSEIF l_datefrom IS NOT INITIAL AND l_dateto IS INITIAL.
        lt_rng_log_date = VALUE #( ( sign = 'I' option = 'EQ' low = l_datefrom ) ).
      ENDIF.

      IF l_id IS NOT INITIAL.
        lt_rng_id = VALUE #( ( sign = 'I' option = 'EQ' low = l_id ) ).
      ENDIF.

      IF l_username IS NOT INITIAL.
        lt_rng_username = VALUE #( ( sign = 'I' option = 'EQ' low = l_username ) ).
      ENDIF.

      SELECT id, seqno, message, username, log_date, log_time, msgid, msgno, msgty
        FROM yaaic_log
          WHERE id IN @lt_rng_id
            AND log_date IN @lt_rng_log_date
            AND username IN @lt_rng_username
            INTO TABLE @DATA(lt_log).

      IF sy-subrc = 0.
        ls_response_query-messages = CORRESPONDING #( lt_log ).
      ENDIF.

    ENDIF.

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

  ENDMETHOD.
ENDCLASS.
