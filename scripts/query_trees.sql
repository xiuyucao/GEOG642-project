-- Get all trees to be joined to the queried plots (either quried by EVALID or measurement time)
-- Reference FIADB User Guide Phase 2 V 9.0

select
  t.CN as TREE_CN,
  t.PLT_CN,
  t.CONDID,
  t.DIA,
  t.TPA_UNADJ,
  t.SPCD

from
  TREE t
  join PLOT p on t.PLT_CN = p.CN

where
  p.STATECD = 6 
  AND p.MEASYEAR > 2013
  AND t.STATUSCD = 1
