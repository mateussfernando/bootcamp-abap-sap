*&---------------------------------------------------------------------*
*& Include ZALUNO17_EXERCIO3V3_CLASS
*&---------------------------------------------------------------------*
" Este include contém a definição e a implementação da classe 'pedido',
" responsável por buscar dados da tabela VBAK e exibir os pedidos.

DATA: todos_os_pedidos TYPE tab_pedido. " Tabela interna que armazena os pedidos buscados

CLASS pedido DEFINITION.
  PUBLIC SECTION. " Seção pública: os métodos e dados abaixo podem ser acessados fora da classe
    METHODS:
      buscar_pedidos,     " Método para buscar pedidos da base de dados (VBAK)
      mostrar_pedidos.    " Método para exibir os pedidos armazenados
ENDCLASS.

CLASS pedido IMPLEMENTATION. " Início da implementação dos métodos da classe
  METHOD buscar_pedidos. " Início do método para buscar pedidos
    SELECT vbeln AS numero_pedido,    " VBELN: número do pedido de venda
           ernam AS vendedor,         " ERNAM: usuário que criou o pedido (vendedor)
           vtweg AS canal_venda       " VTWEG: canal de distribuição
      INTO TABLE @todos_os_pedidos   " Os dados selecionados são armazenados na tabela interna
      FROM vbak.                      " Fonte dos dados: tabela padrão de pedidos de venda (VBAK)
*      UP TO 10 ROWS.                 " Limitaria a seleção aos primeiros 10 registros
  ENDMETHOD.

  METHOD mostrar_pedidos. " Início do método para exibir os pedidos
    DATA: pedido_atual TYPE tipo_pedido. " Declara uma variável para armazenar cada linha da tabela durante o loop

    LOOP AT todos_os_pedidos INTO pedido_atual. " Percorre todos os registros da tabela interna
      WRITE: / 'Pedido:', pedido_atual-numero_pedido,  " Exibe o número do pedido
                'Vendedor:', pedido_atual-vendedor,     " Exibe o nome do vendedor
                'Canal:', pedido_atual-canal_venda.     " Exibe o canal de venda
    ENDLOOP. " Fim do loop
  ENDMETHOD. " Fim do método mostrar_pedidos
ENDCLASS. " Fim da implementação da classe