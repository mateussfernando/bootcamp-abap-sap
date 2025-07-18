*&---------------------------------------------------------------------*
*& Include ZALUNO18_RVENDAS_CLASS2
*&---------------------------------------------------------------------*

CLASS lclass_rel_vendas DEFINITION.
  PUBLIC SECTION.
    DATA:
      " *-- Agora usamos o tipo de tabela global do dicionário de dados.
      lt_vendas            TYPE ztt_ordem_vendas,
      lv_dados_encontrados TYPE abap_bool VALUE abap_false.

    METHODS:
      buscar_rel_vendas,
      exibir_rel_vendas.
ENDCLASS.

CLASS lclass_rel_vendas IMPLEMENTATION.
  METHOD buscar_rel_vendas.
    " O método agora apenas orquestra a chamada para o Módulo de Função e trata o seu retorno.
    CALL FUNCTION 'Z_FM_BUSCAR_DADOS_VENDAS'
      EXPORTING
        it_r_vbeln            = so_venda[] " Passa a tabela de range (corpo do select-option)
        it_r_posnr            = so_item[]  " Passa a tabela de range (corpo do select-option)
      IMPORTING
        et_vendas             = lt_vendas  " Recebe os dados na tabela interna da classe
      EXCEPTIONS
        dados_nao_encontrados = 1 " Se RAISE dados_nao_encontrados, sy-subrc será 1
        OTHERS                = 2. " Para qualquer outro erro

    " Tratamento de possíveis erros de retorno da função com base no sy-subrc.
    CASE sy-subrc.
      WHEN 0. " Sucesso!
        lv_dados_encontrados = abap_true.

      WHEN 1. " Tratamento para a exceção DADOS_NAO_ENCONTRADOS
        MESSAGE 'Nenhum dado encontrado para os filtros informados.' TYPE 'S' DISPLAY LIKE 'E'.
        lv_dados_encontrados = abap_false.

      WHEN 2. " Tratamento para outros erros
        MESSAGE 'Ocorreu um erro inesperado na busca de dados.' TYPE 'E'.
        lv_dados_encontrados = abap_false.
    ENDCASE.

  ENDMETHOD.

  METHOD exibir_rel_vendas.
    " Método responsável por exibir os dados em um ALV (ABAP List Viewer)
    " utilizando a classe CL_SALV_TABLE.

    " Se a flag indicar que nenhum dado foi encontrado na busca, não faz nada.
    IF lv_dados_encontrados = abap_false.
      RETURN.
    ENDIF.

    TRY.
        " 1. Cria a instância do ALV a partir da nossa tabela interna 'lt_vendas'.
        "    O método factory é o ponto de entrada para criar o relatório.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table = DATA(lo_alv) " Declaração inline do objeto ALV
          CHANGING
            t_table      = lt_vendas ).

        " 2. (Opcional) Otimiza a largura das colunas para se ajustarem ao conteúdo.
        DATA(lo_columns) = lo_alv->get_columns( ).
        lo_columns->set_optimize( abap_true ).

        " 3. (Opcional) Habilita as funções padrão da barra de ferramentas do ALV (Exportar, Filtrar, etc).
        DATA(lo_functions) = lo_alv->get_functions( ).
        lo_functions->set_all( abap_true ).

        " 4. (Opcional) Define um título para o relatório.
        DATA(lo_display) = lo_alv->get_display_settings( ).
        lo_display->set_list_header( 'Relatório de Ordens de Venda' ).

        " 5. Exibe o relatório na tela.
        lo_alv->display( ).

      CATCH cx_salv_msg.
        " Em caso de erro na criação ou exibição do ALV, exibe mensagem.
        MESSAGE 'Ocorreu um erro ao tentar exibir o relatório ALV.' TYPE 'E'.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.