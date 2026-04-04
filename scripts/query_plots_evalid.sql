-- Get the plots with CONDition, STRATUM, and ESTN_UNIT (estimation unit) information
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
  c.SUBPPROP_UNADJ,
  
  -- estimation unit
  peu.ESTN_UNIT,
  peu.ESTN_UNIT_DESCR,
  peu.P1PNTCNT_EU,  -- total number of phase 1 pixels in the estimation unit
  
  -- stratum
  ps.EVALID,
  ps.STRATUMCD,
  ps.STRATUM_DESCR,
  ps.P1POINTCNT,  -- number of phase 1 pixels in the stratum
  (ps.P1POINTCNT * 1.0 / peu.P1PNTCNT_EU) as STRATUM_WEIGHT,
  ps.P2POINTCNT,  -- phase 2 plot number
  ps.ADJ_FACTOR_SUBP,
  ps.ADJ_FACTOR_MICR

from
  PLOT p  -- plot table
  join COND c on p.CN = c.PLT_CN  -- condition table
  join POP_PLOT_STRATUM_ASSGN ppsa on p.CN = ppsa.PLT_CN  -- junction table assigning plots to strata
  join POP_STRATUM ps on ppsa.STRATUM_CN = ps.CN  -- strata table
  join POP_ESTN_UNIT peu on ps.ESTN_UNIT_CN = peu.CN  -- estimation unit table

where
  ps.EVALID in (
          61601,  -- CALIFORNIA 2016: 2007-2016: CURRENT AREA, CURRENT VOLUME
          61701,  -- CALIFORNIA 2017: 2008-2017: CURRENT AREA, CURRENT VOLUME
          61801,  -- CALIFORNIA 2018: 2009-2018: CURRENT AREA, CURRENT VOLUME
          61901,  -- CALIFORNIA 2019: 2010-2019: CURRENT AREA, CURRENT VOLUME
          62001,  -- CALIFORNIA 2020: 2011-2020: CURRENT AREA, CURRENT VOLUME
          62101  -- CALIFORNIA 2021: 2012-2021: CURRENT AREA, CURRENT VOLUME
  )