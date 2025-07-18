*&---------------------------------------------------------------------*
*& Report ZALUNO17_EXER5V2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zaluno17_exer5v2.

TYPES: BEGIN OF tipo_estrutura,
         numero_pedido     TYPE ebeln,
         numero_item       TYPE ebelp,
         quantidade_pedido TYPE bstmg,
         texto_item        TYPE char50,
         desc_item         TYPE char50,
       END OF tipo_estrutura.

DATA: tabela_interna TYPE TABLE OF tipo_estrutura,
      tipo_tabela    TYPE tipo_estrutura.

DATA: lt_item       TYPE TABLE OF bapiekpo,
      lt_desc_item  TYPE TABLE OF bapiekpo,
      lt_txt_pedido TYPE TABLE OF bapiekkotx.

DATA: alv           TYPE REF TO cl_salv_table,
      alv_functions TYPE REF TO cl_salv_functions_list.

SELECT-OPTIONS: i_ebeln FOR tipo_tabela-numero_pedido OBLIGATORY,
                i_ebelp FOR tipo_tabela-numero_item.

START-OF-SELECTION.

  SELECT ebeln
         ebelp
         menge
    INTO TABLE tabela_interna
    FROM ekpo
    WHERE ebeln IN i_ebeln
      AND ebelp IN i_ebelp.

  IF sy-subrc <> 0.
    MESSAGE 'Nenhum registro encontrado.' TYPE 'E'.
    EXIT.
  ENDIF.

  LOOP AT tabela_interna INTO tipo_tabela.

    CLEAR lt_item.
    CLEAR lt_desc_item.
    CLEAR lt_txt_pedido.

    CALL FUNCTION 'BAPI_PO_GETDETAIL'
      EXPORTING
        purchaseorder   = tipo_tabela-numero_pedido
        items           = 'X'
        item_texts      = 'X'
        header_texts    = 'X'
      TABLES
        po_items        = lt_item
        po_item_texts   = lt_desc_item
        po_header_texts = lt_txt_pedido.

    LOOP AT lt_txt_pedido INTO DATA(ls_cabecalho).
      tipo_tabela-texto_item = ls_cabecalho-text_line.
      EXIT.
    ENDLOOP.

    LOOP AT lt_item INTO DATA(ls_item).
      IF ls_item-po_item = tipo_tabela-numero_item.
        tipo_tabela-desc_item = ls_item-short_text.
        EXIT.
      ENDIF.
    ENDLOOP.

    MODIFY tabela_interna FROM tipo_tabela
      TRANSPORTING texto_item desc_item
      WHERE numero_pedido = tipo_tabela-numero_pedido
        AND numero_item   = tipo_tabela-numero_item.

  ENDLOOP.

  cl_salv_table=>factory(
    IMPORTING
      r_salv_table = alv
    CHANGING
      t_table      = tabela_interna ).

  alv_functions = alv->get_functions( ).
  alv_functions->set_all( abap_true ).

  DATA: lr_columns TYPE REF TO cl_salv_columns_table,
        lr_column  TYPE REF TO cl_salv_column.

  lr_columns = alv->get_columns( ).

  lr_column ?= lr_columns->get_column( 'TEXTO_ITEM' ).
  lr_column->set_long_text(  'Texto do Pedido' ).
  lr_column->set_medium_text( 'Texto Pedido' ).
  lr_column->set_short_text(  'Texto' ).

  lr_column ?= lr_columns->get_column( 'DESC_ITEM' ).
  lr_column->set_long_text(  'Descrição do Item' ).
  lr_column->set_medium_text( 'Descrição' ).
  lr_column->set_short_text(  'Desc.' ).

  alv->display( ).