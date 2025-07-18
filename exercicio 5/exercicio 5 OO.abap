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
DATA: gs_tabela TYPE tipo_estrutura.

"--- Campos de seleção obrigatórios para o número do pedido e item
SELECT-OPTIONS: s_ebeln FOR gs_tabela-numero_pedido OBLIGATORY,
                s_ebelp FOR gs_tabela-numero_item.

"======================================================================
" Definição da classe local LCL_PEDIDO_ALV
"======================================================================
CLASS lcl_pedido_alv DEFINITION.
  PUBLIC SECTION.
    " Métodos públicos acessíveis de fora da classe
    METHODS:
      carregar_dados,  " Responsável por buscar os dados principais
      exibir_alv.      " Exibe os dados no ALV

  PRIVATE SECTION.
    " Atributos e métodos internos da classe
    DATA: gt_final TYPE tipo_estrutura_tab,  " Tabela final com os dados para o ALV
          gs_aux   TYPE tipo_estrutura.      " Estrutura auxiliar para preenchimento

    METHODS:
      buscar_detalhes_pedido IMPORTING iv_ebeln TYPE ebeln
                                       iv_ebelp TYPE ebelp. " Busca detalhes via BAPI
ENDCLASS.

"======================================================================
" Implementação da classe LCL_PEDIDO_ALV
"======================================================================
CLASS lcl_pedido_alv IMPLEMENTATION.

  "--- Método que carrega os dados principais da EKPO
  METHOD carregar_dados.
    DATA: lt_ekpo TYPE tipo_ekpo_tab,
          ls_ekpo TYPE tipo_ekpo_reduzido.

    " Seleciona os pedidos e itens da EKPO conforme os filtros
    SELECT ebeln, ebelp, menge
      INTO TABLE @lt_ekpo
      FROM ekpo
      WHERE ebeln IN @s_ebeln AND ebelp IN @s_ebelp.

    " Se não encontrar registros, exibe mensagem de erro
    IF sy-subrc <> 0.
      MESSAGE 'Nenhum registro encontrado.' TYPE 'E'.
      RETURN.
    ENDIF.

    " Para cada item encontrado, busca os detalhes via BAPI
    LOOP AT lt_ekpo INTO ls_ekpo.
      buscar_detalhes_pedido( iv_ebeln = ls_ekpo-ebeln iv_ebelp = ls_ekpo-ebelp ).
    ENDLOOP.
  ENDMETHOD.

  "--- Método que busca os detalhes do pedido via BAPI
  METHOD buscar_detalhes_pedido.
    DATA: lt_item       TYPE TABLE OF bapiekpo,
          lt_desc_item  TYPE TABLE OF bapiekpotx,
          lt_txt_pedido TYPE TABLE OF bapiekkotx,
          ls_item       TYPE bapiekpo,
          ls_texto_item TYPE bapiekpotx,
          ls_txt_cabec  TYPE bapiekkotx.

    " Chamada da BAPI para obter detalhes do pedido
    CALL FUNCTION 'BAPI_PO_GETDETAIL'
      EXPORTING
        purchaseorder   = iv_ebeln
        items           = 'X'
        item_texts      = 'X'
        header_texts    = 'X'
      TABLES
        po_items        = lt_item
        po_item_texts   = lt_desc_item
        po_header_texts = lt_txt_pedido.

    " Processa os itens retornados pela BAPI
    LOOP AT lt_item INTO ls_item WHERE po_item = iv_ebelp.
      CLEAR gs_aux.
      gs_aux-numero_pedido     = iv_ebeln.
      gs_aux-numero_item       = ls_item-po_item.
      gs_aux-quantidade_pedido = ls_item-quantity.
      gs_aux-desc_curta_item   = ls_item-short_text.

      " Para cada linha de texto do item, cria uma entrada separada
      LOOP AT lt_desc_item INTO ls_texto_item
           WHERE po_number = iv_ebeln AND po_item = iv_ebelp.
        DATA(gs_aux_item) = gs_aux.
        gs_aux_item-texto_item = ls_texto_item-text_line.
        APPEND gs_aux_item TO gt_final.
      ENDLOOP.

      " Para cada linha de texto do cabeçalho, cria uma entrada separada
      LOOP AT lt_txt_pedido INTO ls_txt_cabec
           WHERE po_number = iv_ebeln.
        DATA(gs_aux_cabec) = gs_aux.
        gs_aux_cabec-desc_item = ls_txt_cabec-text_line.
        APPEND gs_aux_cabec TO gt_final.
      ENDLOOP.

      " Se não houver textos, ainda assim adiciona o item básico
      IF lt_desc_item IS INITIAL AND lt_txt_pedido IS INITIAL.
        APPEND gs_aux TO gt_final.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  "--- Método que exibe os dados no ALV
  METHOD exibir_alv.
    DATA: lo_alv  TYPE REF TO cl_salv_table,
          lo_cols TYPE REF TO cl_salv_columns_table,
          lo_col  TYPE REF TO cl_salv_column_table.

    " Cria o ALV com os dados da tabela final
    cl_salv_table=>factory(
      IMPORTING r_salv_table = lo_alv
      CHANGING  t_table      = gt_final ).

    " Ativa todas as funções padrão do ALV (exportar, ordenar, etc.)
    lo_alv->get_functions( )->set_all( abap_true ).

    " Ordena pela coluna número do pedido
    lo_alv->get_sorts( )->add_sort( columnname = 'NUMERO_PEDIDO' ).

    " Soma a quantidade dos itens
    lo_alv->get_aggregations( )->add_aggregation( columnname = 'QUANTIDADE_PEDIDO' ).

    " Ativa o padrão zebrado (linhas alternadas)
    lo_alv->get_display_settings( )->set_striped_pattern( abap_true ).

    " Configurações de colunas
    lo_cols = lo_alv->get_columns( ).
    lo_cols->set_optimize( abap_false ). " Não otimiza automaticamente a largura

    " Define os títulos e larguras das colunas
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

    " Exibe o ALV na tela
    lo_alv->display( ).
  ENDMETHOD.

ENDCLASS.

"--- Ponto de entrada do programa
START-OF-SELECTION.
  " Cria uma instância da classe e executa os métodos
  DATA(lo_relatorio) = NEW lcl_pedido_alv( ).
  lo_relatorio->carregar_dados( ).
  lo_relatorio->exibir_alv( ).