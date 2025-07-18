*&---------------------------------------------------------------------*
*& Report ZDUPLA03_AMGF
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zdupla03_amgf.

TABLES: pernr.

INFOTYPES: 0000,
           0001,
           0002.


DATA: go_alv              TYPE REF TO cl_salv_table,
      go_alv_functions    TYPE REF TO cl_salv_functions,
      go_alv_columns      TYPE REF TO cl_salv_columns_table,
      go_alv_column       TYPE REF TO cl_salv_column_table,
      go_alv_sort         TYPE REF TO cl_salv_sorts,
      go_alv_aggregations TYPE REF TO cl_salv_aggregations,
      go_alv_events       TYPE REF TO cl_salv_events_table.

TYPES: BEGIN OF ty_saida,
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
       END OF ty_saida.

TYPES: BEGIN OF ty_rubrica,
         lgart TYPE pc207-lgart,
         betrg TYPE pc207-betrg,
         anzhl TYPE pc207-anzhl,
         pernr type pa0001-pernr,
         sname type pa0001-sname,
         "betrg TYPE pc207-betrg,
       END OF ty_rubrica.

DATA gt_saida TYPE TABLE OF ty_saida.
DATA gw_saida TYPE ty_saida.

DATA gt_bdcdata TYPE TABLE OF bdcdata.
DATA gw_bdcdata TYPE          bdcdata.
DATA gt_pernr type pa0001-pernr.

DATA gt_rubrica TYPE TABLE OF ty_rubrica.
DATA gw_rubrica TYPE ty_rubrica.

CLASS lcl_handle_events DEFINITION.
  PUBLIC SECTION.
    METHODS handle FOR EVENT double_click OF cl_salv_events_table IMPORTING row column.
ENDCLASS.

CLASS lcl_handle_events IMPLEMENTATION.
  METHOD handle.

  READ TABLE gt_saida INTO DATA(gw_saida) INDEX row.
  CLEAR gt_bdcdata.

  gw_bdcdata-program = 'SAPMPZ02'.
  gw_bdcdata-dynpro = '1000'.
  gw_bdcdata-dynbegin = 'X'.
  APPEND gw_bdcdata to gt_bdcdata.
  CLEAR gw_bdcdata.

  gw_bdcdata-fnam = 'RP50G-PERNR'.
  gw_bdcdata-fval = gt_pernr.
  APPEND gw_bdcdata to gt_bdcdata.

  CALL TRANSACTION 'PC_PAYRESULT' USING gt_bdcdata
        MODE 'E'.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.

GET pernr.
  PROVIDE cname gesch gbdat gblnd gbdep gbort natio FROM p0002 BETWEEN pn-begda AND pn-endda.
    gw_saida-cname = p0002-cname.
    gw_saida-gesch = p0002-gesch.
    gw_saida-gbdat = p0002-gbdat.
    gw_saida-gblnd = p0002-gblnd.
    gw_saida-gbdep = p0002-gbdep.
    gw_saida-gbort = p0002-gbort.
    gw_saida-natio = p0002-natio.

  ENDPROVIDE.

  gw_saida-pernr = pernr-pernr.
  gw_saida-bukrs = pernr-bukrs.
  gw_saida-werks = pernr-werks.

  APPEND gw_saida TO gt_saida.
  CLEAR gw_saida.



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
      IMPORTING
        r_salv_table = go_alv
      CHANGING
        t_table      = gt_saida
        ).
    CATCH cx_salv_msg.
      WRITE: / 'ALV error'.
  ENDTRY.


  go_alv_functions = go_alv->get_functions( ).
  go_alv_functions->set_all( 'x' ).

  go_alv_columns = go_alv->get_columns( ).
  go_alv_columns->set_optimize( 'x' ).

  go_alv_sort = go_alv->get_sorts( ).
  go_alv_sort->add_sort( columnname = 'CNAME' ).

  go_alv_column ?= go_alv_columns->get_column( 'PERNR' ).
  go_alv_column->set_cell_type( if_salv_c_cell_type=>hotspot ).

  go_alv_events = go_alv->get_event( ).
  DATA(lo_handle_event) = NEW lcl_handle_events( ).
  SET HANDLER lo_handle_event->handle FOR go_alv_events.


  go_alv->display( ).

  FORM display_rubricas USING iv_pernr TYPE pa0001-pernr.

  DATA: go_alv2           TYPE REF TO cl_salv_table,
      go_alv2_functions TYPE REF TO cl_salv_functions,
      go_alv2_columns   TYPE REF TO cl_salv_columns_table.

DATA: rgdir      TYPE TABLE OF pc261,
      lv_cl_id   TYPE relid_pcl,
      number     TYPE pc261-seqnr,
      result     TYPE paybr_result,
      rt_header  TYPE pc207,
      s_rubricas TYPE ty_rubrica,
      ti_result  TYPE TABLE OF ty_rubrica.

  CALL FUNCTION 'CU_READ_RGDIR'
    EXPORTING
      persnr          = pernr-pernr
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
      employeenumber               = pernr-pernr
      sequencenumber               = number
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

  LOOP AT result-inter-rt INTO rt_header.

    MOVE-CORRESPONDING rt_header TO s_rubricas.

    s_rubricas-pernr = pernr-pernr.
    s_rubricas-sname = pernr-sname.

    APPEND s_rubricas TO ti_result.

    CLEAR s_rubricas.




  ENDLOOP.

  LOOP AT result-inter-rt INTO rt_header.
    s_rubricas-betrg = s_rubricas-betrg + rt_header-betrg.
  ENDLOOP.

  APPEND s_rubricas TO ti_result.


  TRY .
      cl_salv_table=>factory(
        IMPORTING
          r_salv_table = go_alv
        CHANGING
          t_table = gt_rubrica
      ).
    CATCH cx_salv_msg.
      WRITE: / 'alv error'.
  ENDTRY.

  go_alv2_functions = go_alv->get_functions( ).
  go_alv2_functions->set_all( 'x' ).

  go_alv2_columns = go_alv->get_columns( ).
  go_alv2_columns->set_optimize( 'x' ).
  go_alv2->display( ).

ENDFORM.