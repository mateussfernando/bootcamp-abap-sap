REPORT zdupla5_mvs.

TABLES pernr.

INFOTYPES: 0001, 0002.

TYPES: BEGIN OF ty_funcionario,

         pernr TYPE pernr-pernr,
         bukrs TYPE pernr-bukrs,
         werks TYPE pernr-werks,
         cname TYPE p0002-cname,
         gesch TYPE p0002-gesch,
         gbdat TYPE p0002-gbdat,
         gblnd TYPE p0002-gblnd,
         gbdep TYPE p0002-gbdep,
         gbort TYPE p0002-gbort,
         natio TYPE p0002-natio,

       END OF ty_funcionario.


DATA:
  ti_funcionarios TYPE TABLE OF ty_funcionario,
  wa_funcionarios TYPE          ty_funcionario.


DATA:
  go_alv_columns   TYPE REF TO cl_salv_columns_table,
  go_alv_column    TYPE REF TO cl_salv_column_table,
  go_alv_functions TYPE REF TO cl_salv_functions,
  go_alv           TYPE REF TO cl_salv_table,
  go_alv_hotspot   TYPE REF TO cl_salv_events_table,
  go_alv_sort      TYPE REF TO cl_salv_sorts,

  go_alv_hotspot2  TYPE REF TO cl_salv_events_table.



DATA: ti_bdcdata TYPE STANDARD TABLE OF bdcdata WITH HEADER LINE,
      wa_bdcdata TYPE bdcdata.

INITIALIZATION.

*PNPPERNR-SIGN = 'I'.
*PNPPERNR-OPTION = 'EQ'.
*PNPPERNR-LOW = '1'.
*APPEND PNPPERNR.
*
*PNPPERNR-SIGN = 'I'.
*PNPPERNR-OPTION = 'EQ'.
*PNPPERNR-LOW = '50'.
*APPEND PNPPERNR.

AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.

    IF screen-name CS 'STAT2'.
      "screen-invisible = 1
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.

    IF screen-name CS 'ABKRS'.
      "screen-invisible = 1
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.

    IF screen-name CS 'XBWBK'.
      "screen-invisible = 1
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.

    IF screen-name CS 'XPGPK'.
      "screen-invisible = 1
      screen-active = 0.
      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.


START-OF-SELECTION.


CLASS lcl_event_handler DEFINITION.

  PUBLIC SECTION.

    METHODS: on_hotspot_click FOR EVENT link_click OF cl_salv_events_table
      IMPORTING
        row
        column.
ENDCLASS.



DATA: lo_event_handler TYPE REF TO  lcl_event_handler.


CLASS lcl_event_handler IMPLEMENTATION.

  METHOD on_hotspot_click.

    READ TABLE ti_funcionarios INTO wa_funcionarios INDEX row.

    CLEAR wa_bdcdata.
    wa_bdcdata-program = 'H99_DISPLAY_PAYRESULT'.
    wa_bdcdata-dynpro  = '2000'.
    wa_bdcdata-dynbegin = 'X'.
    APPEND wa_bdcdata TO ti_bdcdata.

    CLEAR wa_bdcdata.
    wa_bdcdata-fnam = 'S_PERNR-LOW'.
    wa_bdcdata-fval  = wa_funcionarios-pernr.
    APPEND wa_bdcdata TO ti_bdcdata.

    CLEAR wa_bdcdata.
    wa_bdcdata-fnam = 'BDC_OKCODE'.
    wa_bdcdata-fval = '/00'.
    APPEND wa_bdcdata TO ti_bdcdata.

    CALL TRANSACTION 'PC_PAYRESULT' USING ti_bdcdata MODE 'E'.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_event_handler2 DEFINITION.

  PUBLIC SECTION.

    METHODS: on_hotspot_click2 FOR EVENT double_click OF cl_salv_events_table
      IMPORTING
        row
        column.
ENDCLASS.

DATA: lo_event_handler2 TYPE REF TO  lcl_event_handler2.

CLASS lcl_event_handler2 IMPLEMENTATION.

  METHOD on_hotspot_click2.

    READ TABLE ti_funcionarios INTO wa_funcionarios INDEX row.

    DATA:
      go_alv_columns2   TYPE REF TO cl_salv_columns_table,
      go_alv_column2    TYPE REF TO cl_salv_column_table,
      go_alv2           TYPE REF TO cl_salv_table,
      go_alv_functions2 TYPE REF TO cl_salv_functions,
      go_alv_sort2      TYPE REF TO cl_salv_sorts.


    TYPES: BEGIN OF ty_rubricas_funcionarios,

             pernr TYPE pernr-pernr,
             cname TYPE p0002-cname,
             lgart TYPE pc207-lgart,
             betrg TYPE pc207-betrg,
             anzhl TYPE pc207-anzhl,

           END OF ty_rubricas_funcionarios.

    DATA: "ti_rubricas_funcionarios TYPE TABLE OF ty_rubricas_funcionarios,
          wa_rubricas_funcionarios TYPE          ty_rubricas_funcionarios.

    DATA:
      rgdir        TYPE TABLE OF pc261,
      rt_header    TYPE          pc207,
      numero       TYPE          pc261-seqnr,
      resultado    TYPE          paybr_result,
      ti_resultado TYPE TABLE OF ty_rubricas_funcionarios.


    CALL FUNCTION 'CU_READ_RGDIR'
      EXPORTING
        persnr          = wa_funcionarios-pernr
*       buffer          =
*       no_authority_check = ' '
        "IMPORTING
*       molga           = country
      TABLES
        in_rgdir        = rgdir
      EXCEPTIONS
        no_record_found = 1
        OTHERS          = 2.

    IF sy-subrc <> 0.
      "Implement suitable error handling here
    ENDIF.

    CALL FUNCTION 'CD_READ_LAST'
      EXPORTING
        begin_date      = pn-begda
        end_date        = pn-endda
      IMPORTING
        out_seqnr       = numero
      TABLES
        rgdir           = rgdir
      EXCEPTIONS
        no_record_found = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    CALL FUNCTION 'PYXX_READ_PAYROLL_RESULT'
      EXPORTING
*       clusterid                    = ''
        employeenumber               = wa_funcionarios-pernr
        sequencenumber               = numero
*       READ_ONLY_BUFFER             = ' '
*        read_only_international      = 'X'
*       ARC_GROUP                    = ' '
*       CHECK_READ_AUTHORITY         = 'X'
*       FILTER_CUMULATIONS           = 'X'
*       CLIENT                       =
* IMPORTING
*       VERSION_NUMBER_PAYVN         =
*       VERSION_NUMBER_PCL2          =
      CHANGING
        payroll_result               = resultado
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
* Implement suitable error handling here
    ENDIF.

    LOOP AT resultado-inter-rt INTO rt_header.

      wa_rubricas_funcionarios-lgart = rt_header-lgart.
      wa_rubricas_funcionarios-anzhl = rt_header-anzhl.
      wa_rubricas_funcionarios-betrg = rt_header-betrg.
      wa_rubricas_funcionarios-pernr = wa_funcionarios-pernr.
      wa_rubricas_funcionarios-cname = wa_funcionarios-cname.


      APPEND wa_rubricas_funcionarios TO ti_resultado.
      CLEAR wa_rubricas_funcionarios.

     ENDLOOP.

      TRY .
          cl_salv_table=>factory(
            IMPORTING
              r_salv_table = go_alv2
            CHANGING
              t_table = ti_resultado
          ).
        CATCH cx_salv_msg.
          WRITE: / 'alv error'.
      ENDTRY.


    go_alv_functions2 = go_alv2->get_functions( ).
    go_alv_functions2->set_all( 'x' ).

    go_alv_columns2 = go_alv2->get_columns( ).
    go_alv_columns2->set_optimize( 'x' ).

    go_alv_sort2 = go_alv2->get_sorts( ).
    go_alv_sort2->add_sort( columnname = 'LGART' subtotal = abap_true ).


    go_alv2->display( ).

  ENDMETHOD.

ENDCLASS.


GET pernr.

  PROVIDE cname gesch gbdat gblnd gbdep gbort natio
     FROM p0002
     BETWEEN pn-begda AND pn-endda.

    wa_funcionarios-pernr = pernr-pernr.
    wa_funcionarios-bukrs = pernr-bukrs.
    wa_funcionarios-werks = pernr-werks.
    wa_funcionarios-cname = p0002-cname.
    wa_funcionarios-gesch = p0002-gesch.
    wa_funcionarios-gbdat = p0002-gbdat.
    wa_funcionarios-gblnd = p0002-gblnd.
    wa_funcionarios-gbdep = p0002-gbdep.
    wa_funcionarios-gbort = p0002-gbort.
    wa_funcionarios-natio = p0002-natio.

  ENDPROVIDE.

  APPEND wa_funcionarios TO ti_funcionarios.
  CLEAR wa_funcionarios.


END-OF-SELECTION.

  TRY .
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = go_alv
        CHANGING
          t_table = ti_funcionarios
      ).
    CATCH cx_salv_msg.
      WRITE: / 'alv error'.
  ENDTRY.


  go_alv_functions = go_alv->get_functions( ).
  go_alv_functions->set_all( 'x' ).

  go_alv_columns = go_alv->get_columns( ).
  go_alv_columns->set_optimize( 'x' ).

  go_alv_sort = go_alv->get_sorts( ).
  go_alv_sort->add_sort( columnname = 'CNAME' subtotal = abap_true ).

  go_alv_column ?= go_alv_columns->get_column( 'PERNR' ).
  go_alv_column->set_cell_type( if_salv_c_cell_type=>hotspot ).


  go_alv_hotspot = go_alv->get_event( ).
  CREATE OBJECT lo_event_handler.
  SET HANDLER lo_event_handler->on_hotspot_click FOR go_alv_hotspot.

"REMOVI ESSE TRECHO POIS QUANDO SETA O HOTSPOT EXPLICITO NOS 2 CAMPOS ELE DA CONFLITO DE FUNCIONALIDADES.
"FUNCIONA MESMO SEM ESTAR CETADO NO CAMPO 'CNAME'

*  go_alv_column ?= go_alv_columns->get_column( 'CNAME' ).
*  go_alv_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
*
*  go_alv_hotspot = go_alv->get_event( ).
  CREATE OBJECT lo_event_handler2.
  SET HANDLER lo_event_handler2->on_hotspot_click2 FOR go_alv_hotspot.

  go_alv->display( ).