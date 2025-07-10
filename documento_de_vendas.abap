REPORT zaluno17.

TABLES: vbak.

TYPES: BEGIN OF dv_tab,
  cod_vendas         TYPE vbak-vbeln,
  org_vendas         TYPE vbak-ernam,
  canal_distribuicao TYPE vbak-vtweg,
END OF dv_tab.

DATA: dc_vendas TYPE TABLE OF dv_tab,
      wa_vendas TYPE dv_tab.

SELECT-OPTIONS: pedido FOR vbak-vbeln NO INTERVALS.

START-OF-SELECTION.

SELECT vbeln AS cod_vendas,
       ernam AS org_vendas,
       vtweg AS canal_distribuicao
  INTO TABLE @dc_vendas
  FROM vbak
  WHERE vbeln IN @pedido.

LOOP AT dc_vendas INTO wa_vendas.
  WRITE: / 'Pedido:', wa_vendas-cod_vendas,
           'Org. Vendas:', wa_vendas-org_vendas,
           'Canal:', wa_vendas-canal_distribuicao.
ENDLOOP.
