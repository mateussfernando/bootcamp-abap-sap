REPORT zaluno17_exer5v4.

"--- Definição de tipos de estrutura para armazenar os dados finais do relatório
TYPES: BEGIN OF tipo_estrutura,
         numero_pedido     TYPE ebeln,     " Número do pedido de compra
         numero_item       TYPE ebelp,     " Número do item do pedido
         texto_item        TYPE char50,    " Texto detalhado do item
         desc_item         TYPE char50,    " Texto do cabeçalho do pedido
         desc_curta_item   TYPE char50,    " Descrição curta do item
         quantidade_pedido TYPE bstmg,     " Quantidade do item
       END OF tipo_estrutura,
       tipo_estrutura_tab TYPE STANDARD TABLE OF tipo_estrutura WITH DEFAULT KEY.

"--- Tipo reduzido apenas com os campos essenciais da tabela EKPO
TYPES: BEGIN OF tipo_ekpo_reduzido,
         ebeln TYPE ebeln,
         ebelp TYPE ebelp,
         menge TYPE bstmg,
       END OF tipo_ekpo_reduzido,
       tipo_ekpo_tab TYPE STANDARD TABLE OF tipo_ekpo_reduzido WITH DEFAULT KEY.

"--- Variável auxiliar para os campos de seleção
DATA: gs_pedido_filtro TYPE tipo_estrutura.

"--- Campos de seleção obrigatórios para o número do pedido e item
SELECT-OPTIONS: s_ebeln FOR gs_pedido_filtro-numero_pedido OBLIGATORY,
                s_ebelp FOR gs_pedido_filtro-numero_item.

"======================================================================
" Definição da classe local LCL_PEDIDO_ALV
"======================================================================
CLASS lcl_pedido_alv DEFINITION.
  PUBLIC SECTION.
    METHODS:
      carregar_dados,
      exibir_alv.

  PRIVATE SECTION.
    DATA: gt_pedidos_detalhados TYPE tipo_estrutura_tab,
          gs_pedido_detalhe     TYPE tipo_estrutura.

    METHODS:
      buscar_detalhes_pedido IMPORTING iv_ebeln TYPE ebeln
                                       iv_ebelp TYPE ebelp.
ENDCLASS.

"======================================================================
" Implementação da classe LCL_PEDIDO_ALV
"======================================================================
CLASS lcl_pedido_alv IMPLEMENTATION.

  METHOD carregar_dados.
    DATA: lt_itens_ekpo TYPE tipo_ekpo_tab,
          ls_item_ekpo  TYPE tipo_ekpo_reduzido.

    SELECT ebeln, ebelp, menge
      INTO TABLE @lt_itens_ekpo
      FROM ekpo
      WHERE ebeln IN @s_ebeln AND ebelp IN @s_ebelp.

    IF sy-subrc <> 0.
      MESSAGE 'Nenhum registro encontrado.' TYPE 'E'.
      RETURN.
    ENDIF.

    LOOP AT lt_itens_ekpo INTO ls_item_ekpo.
      buscar_detalhes_pedido(
        iv_ebeln = ls_item_ekpo-ebeln
        iv_ebelp = ls_item_ekpo-ebelp ).
    ENDLOOP.
  ENDMETHOD.

  METHOD buscar_detalhes_pedido.
    DATA: lt_bapi_itens     TYPE TABLE OF bapiekpo,
          lt_bapi_txt_itens TYPE TABLE OF bapiekpotx,
          lt_bapi_txt_hdr   TYPE TABLE OF bapiekkotx,
          ls_bapi_item      TYPE bapiekpo,
          ls_bapi_txt_item  TYPE bapiekpotx,
          ls_bapi_txt_hdr   TYPE bapiekkotx.

    CALL FUNCTION 'BAPI_PO_GETDETAIL'
      EXPORTING
        purchaseorder   = iv_ebeln
        items           = 'X'
        item_texts      = 'X'
        header_texts    = 'X'
      TABLES
        po_items        = lt_bapi_itens
        po_item_texts   = lt_bapi_txt_itens
        po_header_texts = lt_bapi_txt_hdr.

    LOOP AT lt_bapi_itens INTO ls_bapi_item WHERE po_item = iv_ebelp.
      CLEAR gs_pedido_detalhe.
      gs_pedido_detalhe-numero_pedido     = iv_ebeln.
      gs_pedido_detalhe-numero_item       = ls_bapi_item-po_item.
      gs_pedido_detalhe-quantidade_pedido = ls_bapi_item-quantity.
      gs_pedido_detalhe-desc_curta_item   = ls_bapi_item-short_text.

      LOOP AT lt_bapi_txt_itens INTO ls_bapi_txt_item
           WHERE po_number = iv_ebeln AND po_item = iv_ebelp.
        DATA(ls_pedido_item) = gs_pedido_detalhe.
        ls_pedido_item-texto_item = ls_bapi_txt_item-text_line.
        APPEND ls_pedido_item TO gt_pedidos_detalhados.
      ENDLOOP.

      LOOP AT lt_bapi_txt_hdr INTO ls_bapi_txt_hdr
           WHERE po_number = iv_ebeln.
        DATA(ls_pedido_hdr) = gs_pedido_detalhe.
        ls_pedido_hdr-desc_item = ls_bapi_txt_hdr-text_line.
        APPEND ls_pedido_hdr TO gt_pedidos_detalhados.
      ENDLOOP.

      IF lt_bapi_txt_itens IS INITIAL AND lt_bapi_txt_hdr IS INITIAL.
        APPEND gs_pedido_detalhe TO gt_pedidos_detalhados.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD exibir_alv.
    DATA: lo_alv  TYPE REF TO cl_salv_table,
          lo_cols TYPE REF TO cl_salv_columns_table,
          lo_col  TYPE REF TO cl_salv_column_table.

    cl_salv_table=>factory(
      IMPORTING r_salv_table = lo_alv
      CHANGING  t_table      = gt_pedidos_detalhados ).

    lo_alv->get_functions( )->set_all( abap_true ).
    lo_alv->get_sorts( )->add_sort( columnname = 'NUMERO_PEDIDO' ).
    lo_alv->get_aggregations( )->add_aggregation( columnname = 'QUANTIDADE_PEDIDO' ).
    lo_alv->get_display_settings( )->set_striped_pattern( abap_true ).

    lo_cols = lo_alv->get_columns( ).
    lo_cols->set_optimize( abap_false ).

    lo_col ?= lo_cols->get_column( 'NUMERO_PEDIDO' ).
    lo_col->set_medium_text( 'Nº Pedido' ).
    lo_col->set_output_length( 10 ).

    lo_col ?= lo_cols->get_column( 'NUMERO_ITEM' ).
    lo_col->set_medium_text( 'Item' ).
    lo_col->set_output_length( 5 ).

    lo_col ?= lo_cols->get_column( 'DESC_CURTA_ITEM' ).
    lo_col->set_medium_text( 'Descrição Curta' ).
    lo_col->set_output_length( 25 ).

    lo_col ?= lo_cols->get_column( 'TEXTO_ITEM' ).
    lo_col->set_medium_text( 'Texto Item' ).
    lo_col->set_output_length( 50 ).

    lo_col ?= lo_cols->get_column( 'DESC_ITEM' ).
    lo_col->set_medium_text( 'Texto Cabeçalho' ).
    lo_col->set_output_length( 50 ).

    lo_col ?= lo_cols->get_column( 'QUANTIDADE_PEDIDO' ).
    lo_col->set_medium_text( 'Quantidade' ).
    lo_col->set_output_length( 12 ).

    lo_alv->display( ).
  ENDMETHOD.

ENDCLASS.

START-OF-SELECTION.
  DATA(lo_pedido_alv) = NEW lcl_pedido_alv( ).
  lo_pedido_alv->carregar_dados( ).
  lo_pedido_alv->exibir_alv( ).