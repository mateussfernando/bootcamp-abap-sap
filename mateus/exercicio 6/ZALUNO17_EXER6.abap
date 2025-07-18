*&---------------------------------------------------------------------*
*& Report ZALUNO17_EXER6
*&---------------------------------------------------------------------*
*& 📌 O que esse programa faz?
*& Esse programa ABAP chamado ZALUNO17_EXER6:
*&
*& 1. Recebe um intervalo de números de pedidos de venda (VBELN) como entrada obrigatória.
*& 2. Busca dados relacionados a esses pedidos nas tabelas VBAK, VBAP e VBPA,
*&    que contêm informações de cabeçalho, itens e parceiros dos pedidos.
*& 3. Organiza esses dados em uma estrutura interna (lt_vend).
*& 4. Exibe os dados em uma tabela ALV (relatório interativo), com funcionalidades
*&    como ordenação, layout zebrado e barra de ferramentas.
*&---------------------------------------------------------------------*
REPORT ZALUNO17_EXER6.

" Declaração das tabelas padrão usadas no SELECT-OPTIONS e nos SELECTs
TABLES: VBAK, VBAP, VBPA.

" Campo de seleção obrigatório para informar os pedidos de venda
SELECT-OPTIONS so_vend FOR vbak-vbeln OBLIGATORY.

" Definição de um tipo de estrutura para armazenar os dados combinados
TYPES: BEGIN OF ty_vend,
         d_vendas TYPE vbeln,      " Número do pedido
         d_doc    TYPE audat,      " Data do documento
         t_doc    TYPE auart,      " Tipo de documento
         o_vendas TYPE vkorg,      " Organização de vendas
         c_distri TYPE vtweg,      " Canal de distribuição
         s_att    TYPE spart,      " Setor de atividade
         i_doc    TYPE posnr,      " Número do item
         n_lote   TYPE charg_d,    " Número do lote
         f_parc   TYPE parvw,      " Função do parceiro
         n_clie   TYPE kunnr,      " Número do cliente
        END OF ty_vend.

" Tabela interna que armazenará os dados finais para exibição
DATA: lt_vend TYPE TABLE OF ty_vend.

" Definição da classe que organiza o processo de seleção e exibição
CLASS lc_vend DEFINITION.
  PUBLIC SECTION.
    METHODS: s_vend,     " Seleciona os pedidos
             s_dados,    " Busca os dados detalhados
             m_tabela.   " Mostra os dados no ALV

    " Tabela interna para armazenar os pedidos selecionados
    DATA: lt_n_vend TYPE TABLE OF vbak-vbeln.
ENDCLASS.

" Implementação da classe
CLASS lc_vend IMPLEMENTATION.

  " Método que seleciona os pedidos com base no intervalo informado
  METHOD s_vend.
    SELECT vbeln
      INTO TABLE @lt_n_vend
      FROM vbak
      WHERE vbeln IN @so_vend.
  ENDMETHOD.

  " Método que busca os dados detalhados de cada pedido
  METHOD s_dados.
    DATA: ls_vend TYPE ty_vend.

    " Loop para cada pedido selecionado
    LOOP AT lt_n_vend INTO DATA(lv_vbeln).

      " Busca dados de cabeçalho do pedido (VBAK)
      SELECT SINGLE vbeln audat auart vkorg vtweg spart
        INTO (ls_vend-d_vendas, ls_vend-d_doc, ls_vend-t_doc,
              ls_vend-o_vendas, ls_vend-c_distri, ls_vend-s_att)
        FROM vbak
        WHERE vbeln = lv_vbeln.

      " Busca dados do item do pedido (VBAP)
      SELECT SINGLE posnr charg
        INTO (ls_vend-i_doc, ls_vend-n_lote)
        FROM vbap
        WHERE vbeln = lv_vbeln.

      " Busca dados do parceiro do pedido (VBPA)
      SELECT SINGLE parvw kunnr
        INTO (ls_vend-f_parc, ls_vend-n_clie)
        FROM vbpa
        WHERE vbeln = lv_vbeln.

      " Adiciona os dados coletados à tabela final
      APPEND ls_vend TO lt_vend.

    ENDLOOP.
  ENDMETHOD.

  " Método que exibe os dados em uma tabela ALV
  METHOD m_tabela.
    " Declaração de objetos para manipular o ALV
    DATA: lo_alv  TYPE REF TO cl_salv_table,
          lo_cols TYPE REF TO cl_salv_columns_table,
          lo_disp TYPE REF TO cl_salv_display_settings,
          lo_func TYPE REF TO cl_salv_functions_list,
          lo_sort TYPE REF TO cl_salv_sorts,
          lo_aggr TYPE REF TO cl_salv_aggregations.

    TRY.
        " Cria a instância do ALV com os dados da tabela lt_vend
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = lo_alv
          CHANGING
            t_table      = lt_vend ).

        " Otimiza o tamanho das colunas automaticamente
        lo_cols = lo_alv->get_columns( ).
        lo_cols->set_optimize( abap_true ).

        " Ativa o padrão zebrado nas linhas
        lo_disp = lo_alv->get_display_settings( ).
        lo_disp->set_striped_pattern( abap_true ).

        " Ativa todas as funções da barra de ferramentas (exportar, ordenar, etc.)
        lo_func = lo_alv->get_functions( ).
        lo_func->set_all( abap_true ).

        " Define ordenação pela coluna D_VENDAS
        lo_sort = lo_alv->get_sorts( ).
        lo_sort->add_sort( columnname = 'D_VENDAS' ).

        " Exibe o ALV na tela
        lo_alv->display( ).

      " Captura e exibe mensagens de erro, se houver
      CATCH cx_salv_msg INTO DATA(lx_msg).
        MESSAGE lx_msg->get_text( ) TYPE 'E'.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.

" Ponto de entrada do programa
START-OF-SELECTION.

" Cria uma instância da classe e executa os métodos
DATA(lo_vend) = NEW lc_vend( ).

lo_vend->s_vend( ).     " Seleciona os pedidos
lo_vend->s_dados( ).    " Busca os dados detalhados
lo_vend->m_tabela( ).   " Exibe os dados no ALV