*&---------------------------------------------------------------------*
*& Report ZALUNO18_ALVPEDIDO
*&---------------------------------------------------------------------*
*& Relatório ALV para exibição de pedidos de compra com detalhes
*&---------------------------------------------------------------------*
REPORT zaluno18_alvpedido.

TABLES: ekko, ekpo.

" Campos de filtro na tela de entrada
SELECT-OPTIONS: so_po   FOR ekko-ebeln OBLIGATORY,
                so_item FOR ekpo-ebelp.

*&---------------------------------------------------------------------*
*& Include ZALUNO18_ALVPEDIDOCLASS
*&---------------------------------------------------------------------*
*& Classe para processamento e exibição de pedidos de compra em ALV
*&---------------------------------------------------------------------*

CLASS lc_relatorio_pedidos DEFINITION.
  PUBLIC SECTION.
    DATA: lt_pedidos           TYPE ztipotabelaekko,
          lv_dados_encontrados TYPE abap_bool VALUE abap_false.

    METHODS:
      buscar_pedidos,
      exibir_pedidos.
ENDCLASS.

CLASS lc_relatorio_pedidos IMPLEMENTATION.
  METHOD buscar_pedidos.
    DATA: lt_documentos_unicos TYPE TABLE OF ebeln.

    SELECT DISTINCT ebeln
      FROM ekpo
      INTO TABLE @lt_documentos_unicos
     WHERE ebeln IN @so_po
       AND ebelp IN @so_item.

    IF lt_documentos_unicos IS INITIAL.
      MESSAGE 'Nenhum pedido encontrado para os filtros informados.' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

    lv_dados_encontrados = abap_true.
    CLEAR lt_pedidos.

    LOOP AT lt_documentos_unicos INTO DATA(lv_ebeln).
      DATA: lt_item       TYPE TABLE OF bapiekpo,
            lt_desc_item  TYPE TABLE OF bapiekpotx,
            lt_txt_pedido TYPE TABLE OF bapiekkotx.

      CALL FUNCTION 'BAPI_PO_GETDETAIL'
        EXPORTING
          purchaseorder   = lv_ebeln
          items           = abap_true
          item_texts      = abap_true
          header_texts    = abap_true
        TABLES
          po_items        = lt_item
          po_item_texts   = lt_desc_item
          po_header_texts = lt_txt_pedido
        EXCEPTIONS
          OTHERS          = 1.

      IF sy-subrc <> 0.
        MESSAGE |Erro ao obter detalhes do pedido { lv_ebeln }| TYPE 'W'.
        CONTINUE.
      ENDIF.

      DATA lv_header_text TYPE string.
      CLEAR lv_header_text.

      LOOP AT lt_txt_pedido INTO DATA(ls_head).
        CONCATENATE lv_header_text ls_head-text_line INTO lv_header_text SEPARATED BY space.
      ENDLOOP.
      SHIFT lv_header_text LEFT DELETING LEADING space.

      " Primeira passagem: adicionar itens à lt_pedidos com informações básicas
      LOOP AT lt_item INTO DATA(ls_item) WHERE po_item IN so_item.
        DATA ls_pedido TYPE zestruturaekko.

        ls_pedido-pedido      = ls_item-po_number.
        ls_pedido-item_pedido = ls_item-po_item.
        ls_pedido-item_qtd    = ls_item-quantity.
        ls_pedido-desc_item   = ls_item-short_text.
        ls_pedido-header      = lv_header_text.
        ls_pedido-texto_item  = ''. " Inicializa texto_item como vazio

        APPEND ls_pedido TO lt_pedidos.
      ENDLOOP.

      " Segunda passagem: atualizar texto_item usando READ TABLE e MODIFY
      LOOP AT lt_desc_item INTO DATA(ls_text).
        READ TABLE lt_pedidos INTO ls_pedido
          WITH KEY pedido = lv_ebeln item_pedido = ls_text-po_item.
        IF sy-subrc = 0.
          DATA(lv_index) = sy-tabix.
          IF ls_pedido-texto_item IS INITIAL.
            ls_pedido-texto_item = ls_text-text_line.
          ELSE.
            CONCATENATE ls_pedido-texto_item ls_text-text_line
              INTO ls_pedido-texto_item SEPARATED BY space.
          ENDIF.
          MODIFY lt_pedidos FROM ls_pedido INDEX lv_index.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    " Opcional: remover espaços à esquerda após concatenar todos os textos
    LOOP AT lt_pedidos INTO ls_pedido.
      SHIFT ls_pedido-texto_item LEFT DELETING LEADING space.
      MODIFY lt_pedidos FROM ls_pedido.
    ENDLOOP.
  ENDMETHOD.

  METHOD exibir_pedidos.
    data: alv_lo           TYPE REF TO cl_salv_table.
    CHECK lv_dados_encontrados = abap_true.

    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = alv_lo
          CHANGING
            t_table      = lt_pedidos ).

        alv_lo->get_display_settings( )->set_striped_pattern( abap_true ).
        alv_lo->get_display_settings( )->set_list_header( 'Relatório de Pedidos de Compra' ).

        DATA(lo_columns) = alv_lo->get_columns( ).
        lo_columns->set_optimize( abap_true ).

        DATA(lr_column) = lo_columns->get_column( 'PEDIDO' ).
        lr_column->set_short_text( 'Doc.' ).
        lr_column->set_medium_text( 'Documento de Compras' ).

        lr_column = lo_columns->get_column( 'ITEM_PEDIDO' ).
        lr_column->set_short_text( 'Item' ).
        lr_column->set_medium_text( 'Item do Pedido' ).

        lr_column = lo_columns->get_column( 'ITEM_QTD' ).
        lr_column->set_short_text( 'Qtd.Pedido' ).
        lr_column->set_medium_text( 'Quantidade Pedida' ).

        lr_column = lo_columns->get_column( 'TEXTO_ITEM' ).
        lr_column->set_short_text( 'Texto Item' ).
        lr_column->set_medium_text( 'Texto do Item' ).

        lr_column = lo_columns->get_column( 'DESC_ITEM' ).
        lr_column->set_short_text( 'Descrição' ).
        lr_column->set_medium_text( 'Descrição do Item' ).

        lr_column = lo_columns->get_column( 'HEADER' ).
        lr_column->set_short_text( 'Header' ).
        lr_column->set_medium_text( 'Texto Cabeçalho' ).

        alv_lo->get_functions( )->set_all( abap_true ).

        DATA(lo_sorts) = alv_lo->get_sorts( ).
        lo_sorts->add_sort( columnname = 'PEDIDO' ).
        lo_sorts->add_sort( columnname = 'ITEM_PEDIDO' ).

        DATA(lo_aggregations) = alv_lo->get_aggregations( ).
        lo_aggregations->add_aggregation(
            columnname  = 'ITEM_QTD'
            aggregation = if_salv_c_aggregation=>total ).

        alv_lo->display( ).


      CATCH cx_salv_msg INTO DATA(lx_salv_general).
        MESSAGE lx_salv_general->get_text( ) TYPE 'E'.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  DATA(lo_pedido) = NEW lc_relatorio_pedidos( ).
  lo_pedido->buscar_pedidos( ).
  lo_pedido->exibir_pedidos( ).