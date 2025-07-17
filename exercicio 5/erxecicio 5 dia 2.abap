REPORT zaluno17_exer5v3.

* Define a estrutura de dados para armazenar as informações dos pedidos
TYPES: BEGIN OF tipo_estrutura,
         numero_pedido     TYPE ebeln,           " Número do pedido de compra
         numero_item       TYPE ebelp,           " Número do item do pedido
         texto_item        TYPE char50,          " Texto do item (descrição detalhada)
         desc_item         TYPE char50,          " Texto do cabeçalho (descrição)
         desc_curta_item   TYPE char50,          " Descrição curta do item
         quantidade_pedido TYPE bstmg,           " Quantidade do item no pedido
       END OF tipo_estrutura,
       tipo_estrutura_tab TYPE STANDARD TABLE OF tipo_estrutura WITH DEFAULT KEY. " Tipo para tabela interna

* Declaração das tabelas internas para armazenar dados
DATA: gt_tabela     TYPE tipo_estrutura_tab,  " Tabela para armazenar os dados finais
      gs_tabela     TYPE tipo_estrutura,      " Variável de trabalho para linha da tabela
      gt_item       TYPE TABLE OF bapiekpo,   " Tabela para armazenar os itens do pedido (do tipo BAPI)
      gt_desc_item  TYPE TABLE OF bapiekpotx,  " Tabela para armazenar os textos dos itens
      gt_txt_pedido TYPE TABLE OF bapiekkotx.  " Tabela para armazenar os textos do cabeçalho do pedido

* Campos de seleção para o número do pedido (EBELN) e número do item (EBELP)
SELECT-OPTIONS: s_ebeln FOR gs_tabela-numero_pedido OBLIGATORY,
                s_ebelp FOR gs_tabela-numero_item.

*----------------------------------------------------------------------*
* INÍCIO DO PROCESSAMENTO
*----------------------------------------------------------------------*
START-OF-SELECTION.

* Recupera os dados dos pedidos de compra (Tabela EKPO)
  SELECT ebeln AS numero_pedido,          " Número do pedido
         ebelp AS numero_item,            " Número do item do pedido
         menge AS quantidade_pedido      " Quantidade do item
    INTO CORRESPONDING FIELDS OF TABLE @gt_tabela  " Preenche a tabela gt_tabela
    FROM ekpo                                    " A partir da tabela EKPO (tabela de itens de pedido)
    WHERE ebeln IN @s_ebeln                     " Filtra pelos números de pedidos selecionados
      AND ebelp IN @s_ebelp.                    " Filtra pelos números de itens selecionados
* Verifica se foram encontrados registros, caso contrário exibe mensagem de erro
  IF sy-subrc <> 0.
    MESSAGE 'Nenhum registro encontrado.' TYPE 'E'.  " Mensagem de erro
    EXIT.  " Encerra o processamento se não encontrar registros
  ENDIF.

* Cria uma tabela interna para armazenar os dados finais
  DATA(gt_final) = VALUE tipo_estrutura_tab( ).

* Loop para processar cada pedido encontrado
  LOOP AT gt_tabela INTO gs_tabela.
   " Limpa as tabelas internas que serão usadas na BAPI
    CLEAR: gt_item, gt_desc_item, gt_txt_pedido.

    "* Chama a BAPI para obter detalhes do pedido de compra
    CALL FUNCTION 'BAPI_PO_GETDETAIL'
      EXPORTING
        purchaseorder   = gs_tabela-numero_pedido  " Número do pedido
        items           = 'X'                      " Indica que deseja obter os itens
        item_texts      = 'X'                      " Indica que deseja obter os textos dos itens
        header_texts    = 'X'                      " Indica que deseja obter os textos do cabeçalho
      TABLES
        po_items        = gt_item                  " Tabela para armazenar os itens retornados pela BAPI
        po_item_texts   = gt_desc_item             " Tabela para armazenar os textos dos itens
        po_header_texts = gt_txt_pedido.          " Tabela para armazenar os textos do cabeçalho do pedido

   " * Loop para processar os itens retornados pela BAPI
    LOOP AT gt_item INTO DATA(gs_item) WHERE po_number = gs_tabela-numero_pedido.
     " * Cria uma estrutura temporária (gs_aux) para armazenar os dados processados
      DATA(gs_aux) = gs_tabela.

      "* Preenche os campos de gs_aux com os dados do item
      gs_aux-numero_item       = gs_item-po_item.    " Atribui o número do item
      gs_aux-quantidade_pedido = gs_item-quantity.    " Atribui a quantidade do item
      gs_aux-desc_curta_item   = gs_item-short_text.  " Atribui a descrição curta do item

     " * Loop para pegar os textos dos itens
      LOOP AT gt_desc_item INTO DATA(gs_texto_item)
           WHERE po_number = gs_aux-numero_pedido
             AND po_item   = gs_aux-numero_item.
        gs_aux-texto_item = gs_texto_item-text_line.  " Atribui o texto do item
      ENDLOOP.

     " * Loop para pegar os textos de cabeçalho do pedido
      LOOP AT gt_txt_pedido INTO DATA(gs_txt_cabec)
           WHERE po_number = gs_aux-numero_pedido.
        gs_aux-desc_item = gs_txt_cabec-text_line.  " Atribui o texto do cabeçalho
      ENDLOOP.

     " * Adiciona os dados de gs_aux à tabela final (gt_final)
      APPEND gs_aux TO gt_final.
    ENDLOOP.
  ENDLOOP.

*----------------------------------------------------------------------*
* EXIBIÇÃO DO ALV COM FORMATAÇÃO DE COLUNAS
*----------------------------------------------------------------------*

 " * Cria a tabela ALV com os dados da tabela final
  cl_salv_table=>factory(
    IMPORTING
      r_salv_table = DATA(go_alv)  " Referência para a tabela ALV
    CHANGING
      t_table      = gt_final ).    " Passa os dados finais para o ALV

 " * Configura as funções do ALV (como sorting, etc.)
  DATA(go_functions) = go_alv->get_functions( ).
  go_functions->set_all( abap_true ).  " Habilita todas as funções do ALV

"  * Configura a ordenação (sorting) do ALV pela coluna 'NUMERO_PEDIDO'
  DATA(go_sorts) = go_alv->get_sorts( ).
  go_sorts->add_sort( columnname = 'NUMERO_PEDIDO' ).  " Ordena pela coluna 'NUMERO_PEDIDO'

"  * Configura a agregação (soma) da coluna 'QUANTIDADE_PEDIDO'
  DATA(go_agg) = go_alv->get_aggregations( ).
  go_agg->add_aggregation( columnname = 'QUANTIDADE_PEDIDO' ).  " Soma da quantidade dos itens

 " * Configura o padrão de exibição (linhas alternadas em cores)
  go_alv->get_display_settings( )->set_striped_pattern( abap_true ).

 " * Configura a otimização das colunas (não otimiza a largura das colunas)
  DATA(go_columns) = go_alv->get_columns( ).
  go_columns->set_optimize( abap_false ).

"  * Formatação das colunas
"  * Formatação do 'NUMERO_PEDIDO'
  DATA(go_col) = go_columns->get_column( 'NUMERO_PEDIDO' ).
  go_col->set_medium_text( 'Nº Pedido' ).  " Título da coluna
  go_col->set_output_length( 10 ).          " Largura da coluna

 " * Formatação do 'NUMERO_ITEM'
  go_col = go_columns->get_column( 'NUMERO_ITEM' ).
  go_col->set_medium_text( 'Item' ).       " Título da coluna
  go_col->set_output_length( 5 ).           " Largura da coluna

 " * Formatação do 'DESC_CURTA_ITEM'
  go_col = go_columns->get_column( 'DESC_CURTA_ITEM' ).
  go_col->set_medium_text( 'Descrição Curta' ).  " Título da coluna
  go_col->set_output_length( 25 ).               " Largura da coluna

 " * Formatação do 'TEXTO_ITEM'
  go_col = go_columns->get_column( 'TEXTO_ITEM' ).
  go_col->set_medium_text( 'Texto Item' ).     " Título da coluna
  go_col->set_output_length( 20 ).              " Largura da coluna

"  * Formatação do 'DESC_ITEM'
  go_col = go_columns->get_column( 'DESC_ITEM' ).
  go_col->set_medium_text( 'Texto Cabeçalho' ).  " Título da coluna
  go_col->set_output_length( 20 ).               " Largura da coluna

"  * Formatação do 'QUANTIDADE_PEDIDO'
  go_col = go_columns->get_column( 'QUANTIDADE_PEDIDO' ).
  go_col->set_medium_text( 'Quantidade' ).    " Título da coluna
  go_col->set_output_length( 12 ).            " Largura da coluna

 " * Exibe a tabela ALV
  go_alv->display( ).