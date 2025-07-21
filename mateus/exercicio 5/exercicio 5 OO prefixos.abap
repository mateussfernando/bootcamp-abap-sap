REPORT zaluno17_exer5v4.

*---------------------------------------------------------------------*
* Definição de tipos
*---------------------------------------------------------------------*
TYPES: BEGIN OF ty_estrutura,
         numero_pedido     TYPE ebeln,      " Nº do pedido de compras (EKPO-EBELN)
         numero_item       TYPE ebelp,      " Nº do item do pedido (EKPO-EBELP)
         texto_item        TYPE tdline,     " Texto longo do item
         desc_item         TYPE tdline,     " Texto do cabeçalho do pedido
         desc_curta_item   TYPE maktx,      " Descrição curta do item
         quantidade_pedido TYPE bstmg,      " Quantidade solicitada no item
       END OF ty_estrutura,
       ty_estrutura_tab TYPE STANDARD TABLE OF ty_estrutura WITH DEFAULT KEY.

*---------------------------------------------------------------------*
* Parâmetros de seleção (tela de entrada)
*---------------------------------------------------------------------*
SELECT-OPTIONS: 
  so_ebeln FOR ekko-ebeln OBLIGATORY, " Intervalo de pedidos obrigatórios
  so_ebelp FOR ekpo-ebelp.            " Intervalo de itens opcional

*---------------------------------------------------------------------*
* Classe principal do relatório ALV
*---------------------------------------------------------------------*
CLASS lcl_pedido_alv DEFINITION.
  PUBLIC SECTION.
    METHODS:
      carregar_dados,  " Método para buscar e montar os dados
      exibir_alv.      " Método para exibir os dados no ALV

    DATA: 
      gt_pedidos_detalhados TYPE ty_estrutura_tab, " Tabela interna com todos os registros prontos
      gs_pedido_detalhe     TYPE ty_estrutura.     " Estrutura auxiliar para montar linha a linha
ENDCLASS.

*---------------------------------------------------------------------*
* Implementação dos métodos da classe
*---------------------------------------------------------------------*
CLASS lcl_pedido_alv IMPLEMENTATION.

  METHOD carregar_dados.
    " Busca os itens filtrados com base nos parâmetros da tela
    SELECT ebeln, ebelp
      FROM ekpo
      INTO TABLE @DATA(lt_itens_filtrados)
      WHERE ebeln IN @so_ebeln
        AND ebelp IN @so_ebelp.

    IF sy-subrc <> 0.
      " Se nada for encontrado, exibe mensagem de erro
      MESSAGE 'Nenhum registro encontrado.' TYPE 'E'.
      RETURN.
    ENDIF.

    " Para cada item filtrado, buscar os detalhes via BAPI
    LOOP AT lt_itens_filtrados INTO DATA(ls_item).
      " Chamada direta do código de busca (sem método privado)
      DATA: 
        lt_bapi_itens     TYPE TABLE OF bapiekpo,
        lt_bapi_txt_itens TYPE TABLE OF bapiekpotx,
        lt_bapi_txt_hdr   TYPE TABLE OF bapiekkotx.

      CALL FUNCTION 'BAPI_PO_GETDETAIL'
        EXPORTING
          purchaseorder = ls_item-ebeln
          items         = 'X'
          item_texts    = 'X'
          header_texts  = 'X'
        TABLES
          po_items        = lt_bapi_itens
          po_item_texts   = lt_bapi_txt_itens
          po_header_texts = lt_bapi_txt_hdr.
      
      " Processa apenas o item solicitado
      LOOP AT lt_bapi_itens INTO DATA(ls_bapi_item) WHERE po_item = ls_item-ebelp.
        CLEAR gs_pedido_detalhe.
        
        " Preenche dados básicos
        gs_pedido_detalhe-numero_pedido     = ls_item-ebeln.
        gs_pedido_detalhe-numero_item       = ls_bapi_item-po_item.
        gs_pedido_detalhe-quantidade_pedido = ls_bapi_item-quantity.
        gs_pedido_detalhe-desc_curta_item   = ls_bapi_item-short_text.
        
        " Busca texto detalhado do item
        LOOP AT lt_bapi_txt_itens INTO DATA(ls_bapi_txt_item)
          WHERE po_number = ls_item-ebeln AND po_item = ls_item-ebelp.
          gs_pedido_detalhe-texto_item = ls_bapi_txt_item-text_line.
        ENDLOOP.
        
        " Busca texto de cabeçalho
        LOOP AT lt_bapi_txt_hdr INTO DATA(ls_bapi_txt_hdr)
          WHERE po_number = ls_item-ebeln.
          gs_pedido_detalhe-desc_item = ls_bapi_txt_hdr-text_line.
        ENDLOOP.
        
        " Adiciona o registro final na tabela interna
        APPEND gs_pedido_detalhe TO gt_pedidos_detalhados.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
  
  METHOD exibir_alv.
    DATA: 
      lo_alv  TYPE REF TO cl_salv_table,
      lo_cols TYPE REF TO cl_salv_columns_table.
      
    TRY.
      " Cria o ALV com a tabela interna carregada
      cl_salv_table=>factory(
        IMPORTING r_salv_table = lo_alv
        CHANGING  t_table      = gt_pedidos_detalhados ).

      " Ativa todas as funções padrão
      lo_alv->get_functions( )->set_all( abap_true ).

      " Ordenação padrão
      lo_alv->get_sorts( )->add_sort( columnname = 'NUMERO_PEDIDO' ).

      " Soma (agregação) das quantidades
      lo_alv->get_aggregations( )->add_aggregation(
        columnname = 'QUANTIDADE_PEDIDO' ).

      " Exibição com linhas listradas
      lo_alv->get_display_settings( )->set_striped_pattern( abap_true ).

      " Ajuste automático do tamanho das colunas
      lo_cols = lo_alv->get_columns( ).
      lo_cols->set_optimize( abap_true ).

      " Configura textos amigáveis nas colunas
      DATA(lo_col) = lo_cols->get_column( 'NUMERO_PEDIDO' ).
      lo_col->set_medium_text( 'Nº Pedido' ).

      lo_col = lo_cols->get_column( 'NUMERO_ITEM' ).
      lo_col->set_medium_text( 'Item' ).

      lo_col = lo_cols->get_column( 'DESC_CURTA_ITEM' ).
      lo_col->set_medium_text( 'Descrição Curta' ).

      lo_col = lo_cols->get_column( 'TEXTO_ITEM' ).
      lo_col->set_medium_text( 'Texto Item' ).

      lo_col = lo_cols->get_column( 'DESC_ITEM' ).
      lo_col->set_medium_text( 'Texto Cabeçalho' ).

      lo_col = lo_cols->get_column( 'QUANTIDADE_PEDIDO' ).
      lo_col->set_medium_text( 'Quantidade' ).

      " Exibe o ALV
      lo_alv->display( ).
    CATCH cx_salv_msg INTO DATA(lx_error).
      MESSAGE lx_error->get_text( ) TYPE 'E'.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

*---------------------------------------------------------------------*
* Ponto de entrada do programa
*---------------------------------------------------------------------*
START-OF-SELECTION.
  DATA(lo_pedido_alv) = NEW lcl_pedido_alv( ). " Cria a instância da classe
  lo_pedido_alv->carregar_dados( ).            " Busca e monta os dados
  lo_pedido_alv->exibir_alv( ).                " Exibe na tela via ALV