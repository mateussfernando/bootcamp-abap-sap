*&---------------------------------------------------------------------*
*& Include ZALUNO17_EXERCIO3V3_CLASS
*&---------------------------------------------------------------------*

CLASS pedido DEFINITION.
  PUBLIC SECTION.
    METHODS:
      buscar_pedidos,
      mostrar_pedidos.
    DATA:
      todos_os_pedidos TYPE tab_pedido.
ENDCLASS.

CLASS pedido IMPLEMENTATION.

  METHOD buscar_pedidos.

    SELECT vbeln AS numero_pedido,
           ernam AS vendedor,
           vtweg AS canal_venda
      INTO TABLE @todos_os_pedidos
      FROM vbak
      UP TO 10 ROWS.

  ENDMETHOD.

  METHOD mostrar_pedidos.
    DATA: pedido_atual TYPE tipo_pedido.

    LOOP AT todos_os_pedidos INTO pedido_atual.
      WRITE: / 'Pedido:', pedido_atual-numero_pedido,
                'Vendedor:', pedido_atual-vendedor,
                'Canal:', pedido_atual-canal_venda.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.