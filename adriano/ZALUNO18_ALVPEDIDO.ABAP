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

" Importa a classe para execução
INCLUDE zaluno18_alvpedidoclass.

START-OF-SELECTION.
  DATA(lo_pedido) = NEW lc_relatorio_pedidos( ).
  lo_pedido->buscar_pedidos( ).
  lo_pedido->exibir_pedidos( ).