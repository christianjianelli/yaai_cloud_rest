CLASS ycl_aaic_rest_async_task DEFINITION INHERITING FROM ycl_aaic_rest_resource
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_async_s,
             id         TYPE string,
             chat_id    TYPE string,
             name       TYPE yde_aaic_async_task_name,
             status     TYPE yde_aaic_async_task_status,
             username   TYPE usnam,
             start_date TYPE yde_aaic_async_task_start_date,
             start_time TYPE yde_aaic_async_task_start_time,
             end_date   TYPE yde_aaic_async_task_end_date,
             end_time   TYPE yde_aaic_async_task_end_time,
             response   TYPE yde_aaic_response,
           END OF ty_async_s,

           ty_async_t TYPE STANDARD TABLE OF ty_async_s WITH EMPTY KEY,

           BEGIN OF ty_response_query_s,
             tasks TYPE ty_async_t,
           END OF ty_response_query_s,

           BEGIN OF ty_response_read_s,
             task TYPE ty_async_s,
           END OF ty_response_read_s.

    METHODS read REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ycl_aaic_rest_async_task IMPLEMENTATION.

  METHOD read.

    DATA: lt_rng_username TYPE RANGE OF yaaic_async-username,
          lt_rng_date     TYPE RANGE OF yaaic_log-log_date.

    DATA: ls_response_read  TYPE ty_response_read_s,
          ls_response_query TYPE ty_response_query_s.

    DATA l_json TYPE string.

    DATA(l_id) = to_upper( i_o_request->get_form_field( i_name = 'id' ) ).
    DATA(l_datefrom) = i_o_request->get_form_field( i_name = 'datefrom' ).
    DATA(l_dateto) = i_o_request->get_form_field( i_name = 'dateto' ).
    DATA(l_username) = i_o_request->get_form_field( i_name = 'username' ).

    IF l_id IS NOT INITIAL.

      SELECT SINGLE id, chat_id, name, status, username,
            startdate AS start_date, starttime AS start_time,
            enddate AS end_date, endtime AS end_time, response
        FROM yaaic_async
       WHERE id = @l_id
        INTO @DATA(ls_async).

      ls_response_read-task = CORRESPONDING #( ls_async ).

      l_json = /ui2/cl_json=>serialize(
        EXPORTING
          data = ls_response_read
          compress = abap_false
          pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      ).

    ELSE.

      IF l_username IS NOT INITIAL.
        lt_rng_username = VALUE #( ( sign = 'I' option = 'EQ' low = l_username ) ).
      ENDIF.

      IF l_datefrom IS NOT INITIAL AND l_dateto IS NOT INITIAL.
        lt_rng_date = VALUE #( ( sign = 'I' option = 'BT' low = l_datefrom high = l_dateto ) ).
      ELSEIF l_datefrom IS NOT INITIAL AND l_dateto IS INITIAL.
        lt_rng_date = VALUE #( ( sign = 'I' option = 'EQ' low = l_datefrom ) ).
      ENDIF.

      SELECT id, chat_id, name, status, username,
             startdate AS start_date, starttime AS start_time,
             enddate AS end_date, endtime AS end_time, response
      FROM yaaic_async
        WHERE startdate IN @lt_rng_date
          AND username IN @lt_rng_username
          INTO TABLE @DATA(lt_async).

      ls_response_query-tasks = CORRESPONDING #( lt_async ).

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
