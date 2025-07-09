REPORT zaluno17.

TABLES: vbak.

TYPES: BEGIN OF dv_tab,
  vbeln TYPE vbak-vbeln,
  vkorg TYPE vbak-ernam,
  statu TYPE vbak-vtweg,
END OF dv_tab.

DATA: dc_vendas TYPE TABLE OF dv_tab,
      wa_vendas TYPE dv_tab.

SELECT-OPTIONS: so_vbeln FOR vbak-vbeln NO INTERVALS.

START-OF-SELECTION.

SELECT vbeln, ernam, vtweg
INTO TABLE @dc_vendas
FROM vbak
WHERE vbeln IN @so_vbeln.
