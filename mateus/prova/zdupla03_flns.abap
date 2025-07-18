*&---------------------------------------------------------------------*
*& Report ZDUPLA03_FLNS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdupla03_flns.

TABLES: pernr.
INFOTYPES: 0000,
           0001,
           0002,
           0008.

* Criando os objetos do ALV utilizando a classe da SALV
DATA: go_alv              TYPE REF TO cl_salv_table,
      go_alv_functions    TYPE REF TO cl_salv_functions,
      go_alv_columns      TYPE REF TO cl_salv_columns_table,
      go_alv_column       TYPE REF TO cl_salv_column_table,
      go_alv_sort         TYPE REF TO cl_salv_sorts,
      go_alv_aggregations TYPE REF TO cl_salv_aggregations,
      go_alv_events       TYPE REF TO cl_salv_events_table.

* Criando a estrutura do ALV
TYPES: BEGIN OF ty_employee_data,
         pernr TYPE p0001-pernr,
         bukrs TYPE p0001-bukrs,
         werks TYPE p0001-werks,
         cname TYPE p0002-cname,
         gesch TYPE p0002-gesch,
         gbdat TYPE p0002-gbdat,
         gblnd TYPE p0002-gblnd,
         gbdep TYPE p0002-gbdep,
         gbort TYPE p0002-gbort,
         natio TYPE p0002-natio,
       END OF ty_employee_data.

* Estrutura para rubricas (RT)
TYPES: BEGIN OF ty_rubricas,
         pernr TYPE pa0001-pernr,
         sname TYPE pa0001-sname,
         lgart TYPE pc207-lgart,
         betrg TYPE pc207-betrg,
         anzhl TYPE pc207-anzhl,
         betpe TYPE pc207-betpe,
       END OF ty_rubricas.

DATA: lt_rubricas TYPE TABLE OF ty_rubricas,
      wa_rubricas TYPE          ty_rubricas.


* Criando a tabela interna
DATA: itab_employee_data TYPE TABLE OF ty_employee_data,
      wa_employee_data   TYPE          ty_employee_data.

* Criando uma estrutura BDCDATA para preencher com os dados de entrada.
DATA: lt_bdcdata TYPE TABLE OF bdcdata,
      ls_bdcdata TYPE          bdcdata.

DATA: iv_pernr TYPE pa0001-pernr.


CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS handle FOR EVENT double_click OF cl_salv_events_table IMPORTING row column.
ENDCLASS.

CLASS lcl_handle_events IMPLEMENTATION.
  METHOD handle.

    READ TABLE itab_employee_data INTO DATA(ls_employee_data) INDEX row.

    CLEAR lt_bdcdata.

    ls_bdcdata-program = 'SAPMPZ02'.
    ls_bdcdata-dynpro  = '1000'.
    ls_bdcdata-dynbegin = 'X'.
    APPEND ls_bdcdata TO lt_bdcdata.

    CLEAR ls_bdcdata.
    ls_bdcdata-fnam = 'RP50G-PERNR'.
    ls_bdcdata-fval = iv_pernr.
    APPEND ls_bdcdata TO lt_bdcdata.

    CALL TRANSACTION 'PC_PAYRESULT' USING lt_bdcdata
          MODE 'E'.
  ENDMETHOD.
ENDCLASS.

INITIALIZATION.

START-OF-SELECTION.

GET pernr.

  SELECT pa0001~pernr pa0001~bukrs pa0001~werks pa0002~cname pa0002~gesch pa0002~gbdat pa0002~gblnd pa0002~gbdep pa0002~gbort pa0002~natio
    INTO CORRESPONDING FIELDS OF TABLE itab_employee_data
    FROM pa0001
    INNER JOIN pa0002 ON pa0001~pernr = pa0002~pernr
    WHERE pa0001~mandt = sy-mandt.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF screen-name CS 'stat2'.
      screen-active = '0'. MODIFY SCREEN.
    ELSEIF screen-name CS 'abkrs'.
      screen-active = '0'. MODIFY SCREEN.
    ELSEIF screen-name CS 'werks'.
      screen-active = '0'. MODIFY SCREEN.
    ELSEIF screen-name CS 'btrtl'.
      screen-active = '0'. MODIFY SCREEN.
    ELSEIF screen-name CS 'kostl'.
      screen-active = '0'. MODIFY SCREEN.
    ELSEIF screen-name CS 'persg'.
      screen-active = '0'. MODIFY SCREEN.
    ELSEIF screen-name CS 'persk'.
      screen-active = '0'. MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

END-OF-SELECTION.

  TRY.
      cl_salv_table=>factory(
       IMPORTING r_salv_table    =  go_alv
       CHANGING  t_table         =  itab_employee_data ).

    CATCH cx_salv_msg.
      WRITE: / 'alv error'.

  ENDTRY.

*  IF column-fieldname = 'SNAME'.
*  PERFORM display_rubricas USING iv_pernr.
*  ELSEIF column-fieldname = 'PERNR'.
*    CREATE OBJECT lo_bdc.
*    lo_bdc->call_transaction_pc_payresult( iv_pernr ).
*    ENDIF.
  PERFORM display_rubricas USING iv_pernr.

  go_alv_functions = go_alv->get_functions( ).
  go_alv_functions->set_all( 'x' ).

  go_alv_columns = go_alv->get_columns( ).
  go_alv_columns->set_optimize( 'x' ).

  go_alv_sort = go_alv->get_sorts( ).
  go_alv_sort->add_sort( columnname = 'CNAME' ).

  go_alv_events = go_alv->get_event( ).
  DATA(lo_lcl_handle_events) = NEW lcl_handle_events( ).
  SET HANDLER lo_lcl_handle_events->handle FOR go_alv_events.

  go_alv->display( ).

FORM display_rubricas USING iv_pernr TYPE pa0001-pernr.

  DATA: lo_alv             TYPE REF TO cl_salv_table,
        lo_alv_aggregation TYPE REF TO cl_salv_aggregations,
        lo_alv_functions   TYPE REF TO cl_salv_functions,
        lo_alv_columns     TYPE REF TO cl_salv_columns_table,
        lo_alv_column      TYPE REF TO cl_salv_column_table,
        lo_alv_sorts       TYPE REF TO cl_salv_sorts.

  DATA: rgdir    TYPE TABLE OF pc261,
        number   TYPE pc261-seqnr,
        lv_cl_id TYPE relid_pcl.

  DATA: result TYPE paybr_result,
        wa_rt  TYPE pc207.

*START-OF-SELECTION.

*GET pernr.

  DATA lv_pernr_conv TYPE pernr_d.

  CALL FUNCTION 'CU_READ_RGDIR'
    EXPORTING
      persnr          = pernr-pernr
*    IMPORTING
*     molga           = country
    TABLES
      in_rgdir        = rgdir
    EXCEPTIONS
      no_record_found = 1
      OTHERS          = 2.

  CALL FUNCTION 'CD_READ_LAST'
    EXPORTING
      begin_date      = pn-begda
      end_date        = pn-endda
    IMPORTING
      out_seqnr       = number
    TABLES
      rgdir           = rgdir
    EXCEPTIONS
      no_record_found = 1
      OTHERS          = 2.

  CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
    EXPORTING
*     CLUSTERID                    =
      employeenumber               = wa_rubricas-pernr
      sequencenumber               = number
*     READ_ONLY_INTERNATIONAL      = ' '
    CHANGING
      payroll_result               = result
    EXCEPTIONS
      illegal_isocode_or_clusterid = 1
      error_generating_import      = 2
      import_mismatch_error        = 3
      subpool_dir_full             = 4
      no_read_authority            = 5
      no_record_found              = 6
      versions_do_not_match        = 7
      error_reading_archive        = 8
      error_reading_relid          = 9
      OTHERS                       = 10.

  IF sy-subrc <> 0.
    WRITE: / 'ERRO AO LER RESULTADOS DE PAGAMENTO.'.
  ENDIF.

  LOOP AT result-inter-rt INTO wa_rt.

    MOVE-CORRESPONDING wa_rt TO wa_rubricas.

    wa_rubricas-pernr = pernr-pernr.
    wa_rubricas-sname = pernr-sname.


    APPEND wa_rubricas TO lt_rubricas.
    CLEAR wa_rubricas.

  ENDLOOP.


*END-OF-SELECTION.

  TRY .

      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = lo_alv
        CHANGING
          t_table = lt_rubricas
      ).
    CATCH cx_salv_msg.
      WRITE: / 'alv error'.

  ENDTRY.

  lo_alv_functions = lo_alv->get_functions( ).
  lo_alv_functions->set_all( 'x' ).

  lo_alv_columns = lo_alv->get_columns( ).
  lo_alv_columns->set_optimize( 'x' ).

  lo_alv_column ?= lo_alv_columns->get_column( 'PERNR' ).
  lo_alv_column->set_cell_type( if_salv_c_cell_type=>hotspot ).

  lo_alv->display( ).


ENDFORM.