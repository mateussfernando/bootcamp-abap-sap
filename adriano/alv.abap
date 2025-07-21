*&---------------------------------------------------------------------*
*& Report ZALUNO18_ALVPEDIDO
*&---------------------------------------------------------------------*
*& Relatório ALV para exibição de pedidos de compra com detalhes
*&---------------------------------------------------------------------*
REPORT zaluno18_alvpedido. " Declaração do programa ABAP

" Declaração das tabelas transparentes utilizadas
TABLES: ekko, " Tabela de cabeçalho de pedidos de compra
        ekpo. " Tabela de itens de pedidos de compra

" Definição dos parâmetros de seleção na tela de entrada
SELECT-OPTIONS: 
  so_po   FOR ekko-ebeln OBLIGATORY, " Número do pedido (obrigatório)
  so_item FOR ekpo-ebelp.            " Número do item do pedido

*&---------------------------------------------------------------------*
*& Classe para processamento e exibição de pedidos de compra em ALV
*&---------------------------------------------------------------------*
CLASS lc_relatorio_pedidos DEFINITION.
  PUBLIC SECTION.
    " Declaração de variáveis da classe
    DATA: 
      lt_pedidos           TYPE ztipotabelaekko, " Tabela interna para armazenar os pedidos
      lv_dados_encontrados TYPE abap_bool VALUE abap_false. " Flag para verificar se existem dados

    " Declaração dos métodos da classe
    METHODS:
      buscar_pedidos,   " Método para buscar os pedidos
      exibir_pedidos.   " Método para exibir os pedidos em ALV
ENDCLASS.

" Implementação dos métodos da classe
CLASS lc_relatorio_pedidos IMPLEMENTATION.
  " Método para buscar os pedidos de compra
  METHOD buscar_pedidos.
    " Declaração de variáveis locais
    DATA: lt_documentos_unicos TYPE TABLE OF ebeln. " Tabela para números de pedido únicos

    " Busca os números de pedido distintos conforme os filtros informados
    SELECT DISTINCT ebeln
      FROM ekpo
      INTO TABLE @lt_documentos_unicos
     WHERE ebeln IN @so_po    " Filtro por número de pedido
       AND ebelp IN @so_item. " Filtro por item do pedido

    " Verifica se encontrou algum pedido
    IF lt_documentos_unicos IS INITIAL.
      " Exibe mensagem de erro caso não encontre pedidos
      MESSAGE 'Nenhum pedido encontrado para os filtros informados.' TYPE 'S' DISPLAY LIKE 'E'.
      RETURN. " Sai do método
    ENDIF.

    " Marca que existem dados encontrados
    lv_dados_encontrados = abap_true.
    " Limpa a tabela de pedidos
    CLEAR lt_pedidos.

    " Loop pelos pedidos encontrados
    LOOP AT lt_documentos_unicos INTO DATA(lv_ebeln).
      " Declaração de tabelas internas para detalhes do pedido
      DATA: 
        lt_item       TYPE TABLE OF bapiekpo,   " Itens do pedido
        lt_desc_item  TYPE TABLE OF bapiekpotx, " Textos dos itens
        lt_txt_pedido TYPE TABLE OF bapiekkotx. " Textos do cabeçalho

      " Chama BAPI para obter detalhes completos do pedido
      CALL FUNCTION 'BAPI_PO_GETDETAIL'
        EXPORTING
          purchaseorder   = lv_ebeln " Número do pedido
          items           = abap_true " Buscar itens
          item_texts      = abap_true " Buscar textos dos itens
          header_texts    = abap_true " Buscar textos do cabeçalho
        TABLES
          po_items        = lt_item       " Retorna os itens
          po_item_texts   = lt_desc_item  " Retorna textos dos itens
          po_header_texts = lt_txt_pedido " Retorna textos do cabeçalho
        EXCEPTIONS
          OTHERS          = 1.

      " Verifica se houve erro na chamada da BAPI
      IF sy-subrc <> 0.
        " Exibe mensagem de aviso em caso de erro
        MESSAGE |Erro ao obter detalhes do pedido { lv_ebeln }| TYPE 'W'.
        CONTINUE. " Pula para o próximo pedido
      ENDIF.

      " Variável para concatenar os textos do cabeçalho
      DATA lv_header_text TYPE string.
      CLEAR lv_header_text.

      " Concatena todos os textos do cabeçalho em uma única string
      LOOP AT lt_txt_pedido INTO DATA(ls_head).
        CONCATENATE lv_header_text ls_head-text_line INTO lv_header_text SEPARATED BY space.
      ENDLOOP.
      " Remove espaços iniciais
      SHIFT lv_header_text LEFT DELETING LEADING space.

      " Primeira passagem: popula a tabela principal com dados básicos
      LOOP AT lt_item INTO DATA(ls_item) WHERE po_item IN so_item.
        " Estrutura para armazenar os dados do pedido
        DATA ls_pedido TYPE zestruturaekko.

        " Preenche a estrutura com os dados do item
        ls_pedido-pedido      = ls_item-po_number.  " Número do pedido
        ls_pedido-item_pedido = ls_item-po_item.    " Número do item
        ls_pedido-item_qtd    = ls_item-quantity.   " Quantidade
        ls_pedido-desc_item   = ls_item-short_text. " Descrição curta
        ls_pedido-header      = lv_header_text.     " Texto do cabeçalho
        ls_pedido-texto_item  = ''. " Inicializa texto do item como vazio

        " Adiciona o item à tabela principal
        APPEND ls_pedido TO lt_pedidos.
      ENDLOOP.

      " Segunda passagem: atualiza os textos dos itens
      LOOP AT lt_desc_item INTO DATA(ls_text).
        " Localiza o item na tabela principal
        READ TABLE lt_pedidos INTO ls_pedido
          WITH KEY pedido = lv_ebeln item_pedido = ls_text-po_item.
          
        " Se encontrou o item, atualiza o texto
        IF sy-subrc = 0.
          DATA(lv_index) = sy-tabix. " Salva o índice do item
          

          " Atualiza o item na tabela principal
          MODIFY lt_pedidos FROM ls_pedido INDEX lv_index.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    " Remove espaços iniciais dos textos concatenados

  ENDMETHOD.

  " Método para exibir os pedidos em ALV
  METHOD exibir_pedidos.

    method exibir_pedidos.
    " Variável para referência do ALV
    DATA: lo_alv TYPE REF TO cl_salv_table.
    
    " Verifica se existem dados para exibir
    CHECK lv_dados_encontrados = abap_true.

    " Bloco TRY para tratamento de erros
    TRY.
        " Cria o objeto ALV
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_alv " Referência do ALV
          CHANGING
            t_table      = lt_pedidos ). " Tabela de dados

        " Configurações de exibição
        lo_alv->get_display_settings( )->set_striped_pattern( abap_true ). " Listras zebradas
        lo_alv->get_display_settings( )->set_list_header( 'Relatório de Pedidos de Compra' ). " Título

        " Obtém as colunas para configuração
        DATA(lo_columns) = lo_alv->get_columns( ).
        lo_columns->set_optimize( abap_true ). " Otimiza largura das colunas

        " Configuração individual de cada coluna
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
        lr_column->set_medium_text( 'Texto Cabeçalho' );

        " Habilita todas as funções padrão do ALV
        lo_alv->get_functions( )->set_all( abap_true );

        " Configura ordenação padrão
        DATA(lo_sorts) = lo_alv->get_sorts( );
        lo_sorts->add_sort( columnname = 'PEDIDO' );     " Ordena por pedido
        lo_sorts->add_sort( columnname = 'ITEM_PEDIDO' ); " Ordena por item

        " Configura agregação (total) para a coluna de quantidade
        DATA(lo_aggregations) = lo_alv->get_aggregations( );
        lo_aggregations->add_aggregation(
            columnname  = 'ITEM_QTD'
            aggregation = if_salv_c_aggregation=>total );

        " Exibe o ALV
        lo_alv->display( );

      " Tratamento de exceções gerais do SALV
      CATCH cx_salv_msg INTO DATA(lx_salv_general).
        MESSAGE lx_salv_general->get_text( ) TYPE 'E'.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.

" Ponto de entrada principal do programa
START-OF-SELECTION.
  " Cria instância da classe
  DATA(lo_pedido) = NEW lc_relatorio_pedidos( ).
  " Busca os pedidos
  lo_pedido->buscar_pedidos( ).
  " Exibe os pedidos em ALV
  lo_pedido->exibir_pedidos( ).