*&---------------------------------------------------------------------*
*& Include ZALUNO17_EXERCIO3V2_VARS
*&---------------------------------------------------------------------*
*Define uma estrutura do tipo_pedido
*para armazenar dados de um pedido
TYPES: BEGIN OF tipo_pedido,
         numero_pedido      TYPE vbak-vbeln,
         vendedor           TYPE vbak-ernam,
         canal_venda        TYPE vbak-vtweg,
       END OF tipo_pedido.

TYPES: tab_pedido TYPE STANDARD TABLE OF tipo_pedido.
