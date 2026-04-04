-- Get all trees to be joined to the queried plots (either quried by EVALID or measurement time)
-- Reference FIADB User Guide Phase 2 V 9.0

select
  t.CN as TREE_CN,
  t.PLT_CN,
  t.CONDID,
  t.STATUSCD,
  t.DIA,
  t.HT,
  t.TPA_UNADJ,
  t.DRYBIO_AG,
  t.DRYBIO_FOLIAGE,
  ppsa.EVALID

from
  TREE t
  join PLOT p on t.PLT_CN = p.CN
  join POP_PLOT_STRATUM_ASSGN ppsa on p.CN = ppsa.PLT_CN

where
  ppsa.EVALID in (
          61601,  -- CALIFORNIA 2016: 2007-2016: CURRENT AREA, CURRENT VOLUME
          61701,  -- CALIFORNIA 2017: 2008-2017: CURRENT AREA, CURRENT VOLUME
          61801,  -- CALIFORNIA 2018: 2009-2018: CURRENT AREA, CURRENT VOLUME
          61901,  -- CALIFORNIA 2019: 2010-2019: CURRENT AREA, CURRENT VOLUME
          62001,  -- CALIFORNIA 2020: 2011-2020: CURRENT AREA, CURRENT VOLUME
          62101  -- CALIFORNIA 2021: 2012-2021: CURRENT AREA, CURRENT VOLUME
  )
  OR (p.STATECD = 6 AND p.MEASYEAR > 2013)