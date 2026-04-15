-- Get all plots in CA
-- Reference FIADB User Guide Phase 2 V 9.0

select
  -- plot
  p.CN as PLT_CN,
  p.LAT,
  p.LON,
  p.STATECD,
  p.COUNTYCD,
  p.INVYR,
  p.MEASYEAR,
  p.MEASMON,
  
  -- condition
  c.CONDID,
  c.COND_STATUS_CD,
  c.CONDPROP_UNADJ,
  c.MICRPROP_UNADJ,
  c.SUBPPROP_UNADJ
  
from
  PLOT p  -- plot table
  join COND c on p.CN = c.PLT_CN  -- condition table

where
  p.STATECD = 6
