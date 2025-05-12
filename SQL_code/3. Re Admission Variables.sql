WITH weight_kg AS (
-- Select relevant fields
  SELECT subject_id, hadm_id, AVG(valuenum) AS weight_kg
-- MIMIC-IV: ICU charted events (vitals, labs)
  FROM `physionet-data.mimiciv_3_1_icu.chartevents`
  WHERE itemid = 226512
  GROUP BY subject_id, hadm_id
),
weight_lbs AS (
-- Select relevant fields
  SELECT subject_id, hadm_id, AVG(valuenum) AS weight_lbs
-- MIMIC-IV: ICU charted events (vitals, labs)
  FROM `physionet-data.mimiciv_3_1_icu.chartevents`
  WHERE itemid = 226531
  GROUP BY subject_id, hadm_id
),
height_cm AS (
-- Select relevant fields
  SELECT subject_id, hadm_id, AVG(valuenum) AS height_cm
-- MIMIC-IV: ICU charted events (vitals, labs)
  FROM `physionet-data.mimiciv_3_1_icu.chartevents`
  WHERE itemid = 226730
  GROUP BY subject_id, hadm_id
),
height_raw AS (
-- Select relevant fields
  SELECT subject_id, hadm_id, AVG(valuenum) AS height_raw
-- MIMIC-IV: ICU charted events (vitals, labs)
  FROM `physionet-data.mimiciv_3_1_icu.chartevents`
  WHERE itemid = 226707
  GROUP BY subject_id, hadm_id
),
heart_rate AS (
-- Select relevant fields
  SELECT subject_id, hadm_id, AVG(valuenum) AS heart_rate
-- MIMIC-IV: ICU charted events (vitals, labs)
  FROM `physionet-data.mimiciv_3_1_icu.chartevents`
  WHERE itemid = 220045
  GROUP BY subject_id, hadm_id
),
bp_sys AS (
-- Select relevant fields
  SELECT subject_id, hadm_id, AVG(valuenum) AS bp_systolic
-- MIMIC-IV: ICU charted events (vitals, labs)
  FROM `physionet-data.mimiciv_3_1_icu.chartevents`
  WHERE itemid = 220050
  GROUP BY subject_id, hadm_id
),
bp_dia AS (
-- Select relevant fields
  SELECT subject_id, hadm_id, AVG(valuenum) AS bp_diastolic
-- MIMIC-IV: ICU charted events (vitals, labs)
  FROM `physionet-data.mimiciv_3_1_icu.chartevents`
  WHERE itemid = 220051
  GROUP BY subject_id, hadm_id
),
resp_rate AS (
-- Select relevant fields
  SELECT subject_id, hadm_id, AVG(valuenum) AS respiratory_rate
-- MIMIC-IV: ICU charted events (vitals, labs)
  FROM `physionet-data.mimiciv_3_1_icu.chartevents`
  WHERE itemid = 220210
  GROUP BY subject_id, hadm_id
),
spo2 AS (
-- Select relevant fields
  SELECT subject_id, hadm_id, AVG(valuenum) AS oxygen_saturation
-- MIMIC-IV: ICU charted events (vitals, labs)
  FROM `physionet-data.mimiciv_3_1_icu.chartevents`
  WHERE itemid = 228232
  GROUP BY subject_id, hadm_id
), 
sofa_cv AS (
-- Select relevant fields
  SELECT
    icu.subject_id,
    icu.hadm_id,
    CAST(ROUND(AVG(s.cardiovascular_24hours)) AS INT64) AS cardiovascular_sofa
  FROM `physionet-data.mimiciv_3_1_icu.icustays` icu
  JOIN `physionet-data.mimiciv_derived.sofa` s
    ON icu.stay_id = s.stay_id
  GROUP BY icu.subject_id, icu.hadm_id
),
clotting_flag AS (
-- Select relevant fields
  SELECT subject_id, hadm_id, 1 AS had_clotting_event
-- MIMIC-IV: ICD-coded diagnoses
  FROM `physionet-data.mimiciv_3_1_hosp.diagnoses_icd` d
  LEFT JOIN `physionet-data.mimiciv_3_1_hosp.d_icd_diagnoses` dicd
    ON d.icd_code = dicd.icd_code AND d.icd_version = dicd.icd_version
  WHERE LOWER(dicd.long_title) LIKE '%pulmonary embolism%'
     OR LOWER(dicd.long_title) LIKE '%deep vein thrombosis%'
     OR LOWER(dicd.long_title) LIKE '%dvt%'
     OR LOWER(dicd.long_title) LIKE '%venous thrombosis%'
     OR LOWER(dicd.long_title) LIKE '%vein thrombosis%'
  GROUP BY subject_id, hadm_id
), 
gi_bleed_flag AS (
-- Select relevant fields
  SELECT subject_id, hadm_id, 1 AS had_gi_bleed
-- MIMIC-IV: ICD-coded diagnoses
  FROM `physionet-data.mimiciv_3_1_hosp.diagnoses_icd` d
  LEFT JOIN `physionet-data.mimiciv_3_1_hosp.d_icd_diagnoses` dicd
    ON d.icd_code = dicd.icd_code AND d.icd_version = dicd.icd_version
  WHERE LOWER(dicd.long_title) LIKE '%gastrointestinal bleeding%'
     OR LOWER(dicd.long_title) LIKE '%gi bleed%'
     OR LOWER(dicd.long_title) LIKE '%upper gi hemorrhage%'
     OR LOWER(dicd.long_title) LIKE '%lower gi hemorrhage%'
  GROUP BY subject_id, hadm_id
),
other_bleed_flag AS (
-- Select relevant fields
  SELECT subject_id, hadm_id, 1 AS had_other_bleed
-- MIMIC-IV: ICD-coded diagnoses
  FROM `physionet-data.mimiciv_3_1_hosp.diagnoses_icd` d
  LEFT JOIN `physionet-data.mimiciv_3_1_hosp.d_icd_diagnoses` dicd
    ON d.icd_code = dicd.icd_code AND d.icd_version = dicd.icd_version
  WHERE LOWER(dicd.long_title) LIKE '%intracranial hemorrhage%'
     OR LOWER(dicd.long_title) LIKE '%major bleeding%'
     OR LOWER(dicd.long_title) LIKE '%cerebral hemorrhage%'
     OR LOWER(dicd.long_title) LIKE '%hemorrhage%'
  GROUP BY subject_id, hadm_id
), 
cardio_comp_flag AS (
-- Select relevant fields
  SELECT subject_id, hadm_id, 1 AS had_cardio_complication
-- MIMIC-IV: ICD-coded diagnoses
  FROM `physionet-data.mimiciv_3_1_hosp.diagnoses_icd` d
  LEFT JOIN `physionet-data.mimiciv_3_1_hosp.d_icd_diagnoses` dicd
    ON d.icd_code = dicd.icd_code AND d.icd_version = dicd.icd_version
  WHERE LOWER(dicd.long_title) LIKE '%atrial fibrillation%'
     OR LOWER(dicd.long_title) LIKE '%hypertension%'
  GROUP BY subject_id, hadm_id
)
-- Select relevant fields
SELECT DISTINCT
  r.*,
  r.re_admission_id AS hadm_id,
  a.hospital_expire_flag,
  wk.weight_kg,
  wl.weight_lbs,
  hc.height_cm,
  hr.height_raw,
  ht.heart_rate,
  bps.bp_systolic,
  bpd.bp_diastolic,
  rr.respiratory_rate,
  sp.oxygen_saturation,
  DATETIME_DIFF(DATETIME(r.admittime), DATETIME(r.medication_time), HOUR) / 24.0 AS days_to_re_adm,
  sofa.cardiovascular_sofa,
  DATETIME_DIFF(DATETIME(a.dischtime), DATETIME(a.admittime), HOUR) / 24.0 AS los,
  IF(clot.had_clotting_event IS NULL, 0, 1) AS had_clotting_event,
  IF(gi.had_gi_bleed IS NULL, 0, 1) AS had_gi_bleed,
  IF(bleed.had_other_bleed IS NULL, 0, 1) AS had_other_bleed,
  IF(cardio.had_cardio_complication IS NULL, 0, 1) AS had_cardio_complication
-- Custom table: first ICU admissions exposed to medications
FROM `custom.cohort_first_admission_medication_exposed_re_ADR` r
LEFT JOIN `physionet-data.mimiciv_3_1_hosp.admissions` a
  ON r.subject_id = a.subject_id AND r.re_admission_id = a.hadm_id
LEFT JOIN weight_kg AS wk
  ON r.subject_id = wk.subject_id AND r.re_admission_id = wk.hadm_id
LEFT JOIN weight_lbs AS wl
  ON r.subject_id = wl.subject_id AND r.re_admission_id = wl.hadm_id
LEFT JOIN height_cm AS hc
  ON r.subject_id = hc.subject_id AND r.re_admission_id = hc.hadm_id
LEFT JOIN height_raw AS hr
  ON r.subject_id = hr.subject_id AND r.re_admission_id = hr.hadm_id
LEFT JOIN heart_rate AS ht
  ON r.subject_id = ht.subject_id AND r.re_admission_id = ht.hadm_id
LEFT JOIN bp_sys AS bps
  ON r.subject_id = bps.subject_id AND r.re_admission_id = bps.hadm_id
LEFT JOIN bp_dia AS bpd
  ON r.subject_id = bpd.subject_id AND r.re_admission_id = bpd.hadm_id
LEFT JOIN resp_rate AS rr
  ON r.subject_id = rr.subject_id AND r.re_admission_id = rr.hadm_id
LEFT JOIN spo2 AS sp
  ON r.subject_id = sp.subject_id AND r.re_admission_id = sp.hadm_id
LEFT JOIN `physionet-data.mimiciv_3_1_icu.icustays` icu
  ON r.subject_id = icu.subject_id AND r.re_admission_id = icu.hadm_id
LEFT JOIN sofa_cv AS sofa
  ON r.subject_id = sofa.subject_id AND r.re_admission_id = sofa.hadm_id
LEFT JOIN clotting_flag AS clot
  ON r.subject_id = clot.subject_id AND r.re_admission_id = clot.hadm_id
LEFT JOIN gi_bleed_flag AS gi
  ON r.subject_id = gi.subject_id AND r.re_admission_id = gi.hadm_id
LEFT JOIN other_bleed_flag AS bleed
  ON r.subject_id = bleed.subject_id AND r.re_admission_id = bleed.hadm_id
LEFT JOIN cardio_comp_flag AS cardio
  ON r.subject_id = cardio.subject_id AND r.re_admission_id = cardio.hadm_id