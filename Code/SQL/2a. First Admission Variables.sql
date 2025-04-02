WITH weight_kg AS (
    SELECT subject_id, hadm_id, AVG(valuenum) AS weight_kg
    FROM `physionet-data.mimiciv_3_1_icu.chartevents`
    WHERE itemid = 226512
    GROUP BY subject_id, hadm_id
),
weight_lbs AS (
    SELECT subject_id, hadm_id, AVG(valuenum) AS weight_lbs
    FROM `physionet-data.mimiciv_3_1_icu.chartevents`
    WHERE itemid = 226531
    GROUP BY subject_id, hadm_id
),
height_raw AS (
    SELECT subject_id, hadm_id, AVG(valuenum) AS height_raw
    FROM `physionet-data.mimiciv_3_1_icu.chartevents`
    WHERE itemid = 226707
    GROUP BY subject_id, hadm_id
),
height_cm AS (
    SELECT subject_id, hadm_id, AVG(valuenum) AS height_cm
    FROM `physionet-data.mimiciv_3_1_icu.chartevents`
    WHERE itemid = 226730
    GROUP BY subject_id, hadm_id
),
heart_rate AS (
    SELECT subject_id, hadm_id, AVG(valuenum) AS heart_rate
    FROM `physionet-data.mimiciv_3_1_icu.chartevents`
    WHERE itemid = 220045
    GROUP BY subject_id, hadm_id
),
abp_sys AS (
    SELECT subject_id, hadm_id, AVG(valuenum) AS abp_systolic
    FROM `physionet-data.mimiciv_3_1_icu.chartevents`
    WHERE itemid = 220050
    GROUP BY subject_id, hadm_id
),
abp_dia AS (
    SELECT subject_id, hadm_id, AVG(valuenum) AS abp_diastolic
    FROM `physionet-data.mimiciv_3_1_icu.chartevents`
    WHERE itemid = 220051
    GROUP BY subject_id, hadm_id
),
resp_rate AS (
    SELECT subject_id, hadm_id, AVG(valuenum) AS respiratory_rate
    FROM `physionet-data.mimiciv_3_1_icu.chartevents`
    WHERE itemid = 220210
    GROUP BY subject_id, hadm_id
),
spo2 AS (
    SELECT subject_id, hadm_id, AVG(valuenum) AS oxygen_saturation
    FROM `physionet-data.mimiciv_3_1_icu.chartevents`
    WHERE itemid = 228232
    GROUP BY subject_id, hadm_id
),
sofa_cv AS (
    SELECT stay_id, CAST(ROUND(AVG(cardiovascular_24hours)) AS INT64) AS cardiovascular_sofa
    FROM `physionet-data.mimiciv_derived.sofa`
    GROUP BY stay_id
),
warfarin_flag AS (
    SELECT subject_id, hadm_id, 1 AS used_warfarin
    FROM `physionet-data.mimiciv_3_1_hosp.pharmacy`
    WHERE LOWER(medication) = 'warfarin'
    GROUP BY subject_id, hadm_id
),
clopidogrel_flag AS (
    SELECT subject_id, hadm_id, 1 AS used_clopidogrel
    FROM `physionet-data.mimiciv_3_1_hosp.pharmacy`
    WHERE LOWER(medication) = 'clopidogrel'
    GROUP BY subject_id, hadm_id
),
other_statins_flag AS (
    SELECT subject_id, hadm_id, 1 AS used_other_statins
    FROM `physionet-data.mimiciv_3_1_hosp.pharmacy`
    WHERE LOWER(medication) IN (
        'atorvastatin', 'rosuvastatin', 'pravastatin',
        'lovastatin', 'fluvastatin', 'pitavastatin'
    )
    GROUP BY subject_id, hadm_id
),
ppi_flag AS (
    SELECT subject_id, hadm_id, 1 AS used_ppi
    FROM `physionet-data.mimiciv_3_1_hosp.pharmacy`
    WHERE LOWER(medication) IN (
        'omeprazole', 'pantoprazole', 'lansoprazole',
        'esomeprazole', 'rabeprazole', 'dexlansoprazole'
    )
    GROUP BY subject_id, hadm_id
),
nsaid_flag AS (
    SELECT subject_id, hadm_id, 1 AS used_nsaid
    FROM `physionet-data.mimiciv_3_1_hosp.pharmacy`
    WHERE LOWER(medication) IN (
        'ibuprofen', 'naproxen', 'celecoxib', 'meloxicam',
        'diclofenac', 'ketorolac', 'aspirin'
    )
    GROUP BY subject_id, hadm_id
),
anticoag_flag AS (
    SELECT subject_id, hadm_id, 1 AS used_anticoagulant
    FROM `physionet-data.mimiciv_3_1_hosp.pharmacy`
    WHERE LOWER(medication) IN (
        'heparin', 'enoxaparin', 'dalteparin', 'fondaparinux',
        'rivaroxaban', 'apixaban', 'edoxaban', 'dabigatran'
    )
    GROUP BY subject_id, hadm_id
),
ssri_snri_flag AS (
    SELECT subject_id, hadm_id, 1 AS used_ssri_snri
    FROM `physionet-data.mimiciv_3_1_hosp.pharmacy`
    WHERE LOWER(medication) IN (
        'fluoxetine', 'paroxetine', 'sertraline', 'citalopram',
        'escitalopram', 'fluvoxamine', 'venlafaxine',
        'desvenlafaxine', 'duloxetine', 'levomilnacipran'
    )
    GROUP BY subject_id, hadm_id
),
beta_blocker_flag AS (
    SELECT subject_id, hadm_id, 1 AS used_beta_blocker
    FROM `physionet-data.mimiciv_3_1_hosp.pharmacy`
    WHERE LOWER(medication) IN (
        'metoprolol', 'propranolol', 'atenolol'
    )
    GROUP BY subject_id, hadm_id
),
ace_arb_flag AS (
    SELECT subject_id, hadm_id, 1 AS used_ace_arb
    FROM `physionet-data.mimiciv_3_1_hosp.pharmacy`
    WHERE LOWER(medication) IN (
        'lisinopril', 'enalapril', 'losartan', 'valsartan'
    )
    GROUP BY subject_id, hadm_id
),
diuretic_flag AS (
    SELECT subject_id, hadm_id, 1 AS used_diuretic
    FROM `physionet-data.mimiciv_3_1_hosp.pharmacy`
    WHERE LOWER(medication) IN (
        'hydrochlorothiazide', 'furosemide', 'spironolactone'
    )
    GROUP BY subject_id, hadm_id
), 
d_dimer_stats AS (
  SELECT
    l.subject_id,
    l.hadm_id,
    s.starttime,
    AVG(CASE WHEN l.charttime < DATETIME(s.starttime) THEN l.valuenum END) AS avg_d_dimer_before,
    AVG(CASE WHEN l.charttime >= DATETIME(s.starttime) THEN l.valuenum END) AS avg_d_dimer_after
  FROM `physionet-data.mimiciv_3_1_hosp.labevents` l
  JOIN `mimic-2024.ADR_first_medication_tables.simvastatin_HF` s
    ON l.subject_id = s.subject_id AND l.hadm_id = s.hadm_id
  WHERE l.itemid IN (50915, 51196, 52551)
  GROUP BY l.subject_id, l.hadm_id, s.starttime
),
d_dimer_before AS (
  SELECT
    l.subject_id,
    l.hadm_id,
    s.starttime,
    l.valuenum AS last_d_dimer_before
  FROM `physionet-data.mimiciv_3_1_hosp.labevents` l
  JOIN `mimic-2024.ADR_first_medication_tables.simvastatin_HF` s
    ON l.subject_id = s.subject_id AND l.hadm_id = s.hadm_id
  WHERE l.itemid IN (50915, 51196, 52551)
    AND l.charttime < DATETIME(s.starttime)
  QUALIFY ROW_NUMBER() OVER (PARTITION BY l.subject_id, l.hadm_id, s.starttime ORDER BY l.charttime DESC) = 1
),
d_dimer_after AS (
  SELECT
    l.subject_id,
    l.hadm_id,
    s.starttime,
    l.valuenum AS first_d_dimer_after
  FROM `physionet-data.mimiciv_3_1_hosp.labevents` l
  JOIN `mimic-2024.ADR_first_medication_tables.simvastatin_HF` s
    ON l.subject_id = s.subject_id AND l.hadm_id = s.hadm_id
  WHERE l.itemid IN (50915, 51196, 52551)
    AND l.charttime >= DATETIME(s.starttime)
  QUALIFY ROW_NUMBER() OVER (PARTITION BY l.subject_id, l.hadm_id, s.starttime ORDER BY l.charttime ASC) = 1
),
coag_labs_stats AS (
  SELECT
    l.subject_id,
    l.hadm_id,
    s.starttime,

    -- Averages
    AVG(CASE WHEN l.charttime < DATETIME(s.starttime) THEN l.valuenum END) AS avg_coag_before,
    AVG(CASE WHEN l.charttime >= DATETIME(s.starttime) THEN l.valuenum END) AS avg_coag_after

  FROM `physionet-data.mimiciv_3_1_hosp.labevents` l
  JOIN `mimic-2024.ADR_first_medication_tables.simvastatin_HF` s
    ON l.subject_id = s.subject_id AND l.hadm_id = s.hadm_id
  WHERE l.itemid IN (
    51237, 51675, 51265, 53189, 51275, 51274
  )
  GROUP BY l.subject_id, l.hadm_id, s.starttime
),
coag_labs_before AS (
  SELECT
    l.subject_id,
    l.hadm_id,
    s.starttime,
    l.valuenum AS last_coag_before
  FROM `physionet-data.mimiciv_3_1_hosp.labevents` l
  JOIN `mimic-2024.ADR_first_medication_tables.simvastatin_HF` s
    ON l.subject_id = s.subject_id AND l.hadm_id = s.hadm_id
  WHERE l.itemid IN (
    51237, 51675, 51265, 53189, 51275, 51274
  )
    AND l.charttime < DATETIME(s.starttime)
  QUALIFY ROW_NUMBER() OVER (
    PARTITION BY l.subject_id, l.hadm_id, s.starttime
    ORDER BY l.charttime DESC
  ) = 1
),
coag_labs_after AS (
  SELECT
    l.subject_id,
    l.hadm_id,
    s.starttime,
    l.valuenum AS first_coag_after
  FROM `physionet-data.mimiciv_3_1_hosp.labevents` l
  JOIN `mimic-2024.ADR_first_medication_tables.simvastatin_HF` s
    ON l.subject_id = s.subject_id AND l.hadm_id = s.hadm_id
  WHERE l.itemid IN (
    51237, 51675, 51265, 53189, 51275, 51274
  )
    AND l.charttime >= DATETIME(s.starttime)
  QUALIFY ROW_NUMBER() OVER (
    PARTITION BY l.subject_id, l.hadm_id, s.starttime
    ORDER BY l.charttime ASC
  ) = 1
),
blood_lab_stats AS (
  SELECT
    l.subject_id,
    l.hadm_id,
    s.starttime,

    AVG(CASE WHEN l.charttime < DATETIME(s.starttime) THEN l.valuenum END) AS avg_blood_lab_before,
    AVG(CASE WHEN l.charttime >= DATETIME(s.starttime) THEN l.valuenum END) AS avg_blood_lab_after

  FROM `physionet-data.mimiciv_3_1_hosp.labevents` l
  JOIN `mimic-2024.ADR_first_medication_tables.simvastatin_HF` s
    ON l.subject_id = s.subject_id AND l.hadm_id = s.hadm_id
  WHERE l.itemid IN (
    51222, 50811, 51640,      -- Hemoglobin
    51221, 52028, 51638, 51639 -- Hematocrit
  )
  GROUP BY l.subject_id, l.hadm_id, s.starttime
),
blood_lab_before AS (
  SELECT
    l.subject_id,
    l.hadm_id,
    s.starttime,
    l.valuenum AS last_blood_lab_before
  FROM `physionet-data.mimiciv_3_1_hosp.labevents` l
  JOIN `mimic-2024.ADR_first_medication_tables.simvastatin_HF` s
    ON l.subject_id = s.subject_id AND l.hadm_id = s.hadm_id
  WHERE l.itemid IN (
    51222, 50811, 51640,
    51221, 52028, 51638, 51639
  )
    AND l.charttime < DATETIME(s.starttime)
  QUALIFY ROW_NUMBER() OVER (
    PARTITION BY l.subject_id, l.hadm_id, s.starttime
    ORDER BY l.charttime DESC
  ) = 1
),
blood_lab_after AS (
  SELECT
    l.subject_id,
    l.hadm_id,
    s.starttime,
    l.valuenum AS first_blood_lab_after
  FROM `physionet-data.mimiciv_3_1_hosp.labevents` l
  JOIN `mimic-2024.ADR_first_medication_tables.simvastatin_HF` s
    ON l.subject_id = s.subject_id AND l.hadm_id = s.hadm_id
  WHERE l.itemid IN (
    51222, 50811, 51640,
    51221, 52028, 51638, 51639
  )
    AND l.charttime >= DATETIME(s.starttime)
  QUALIFY ROW_NUMBER() OVER (
    PARTITION BY l.subject_id, l.hadm_id, s.starttime
    ORDER BY l.charttime ASC
  ) = 1
)

SELECT
    s.subject_id,
    s.hadm_id,
    s.starttime AS medication_time,
    s.dosage AS medication_dosage,
    p.anchor_age AS age,
    p.gender,
    a.race,
    a.language,
    a.insurance AS insurance_type,
    a.marital_status,
    a.admission_type AS hosp_admission_type,
    a.hospital_expire_flag AS first_expire,
    a.admittime AS first_admittime,
    a.dischtime AS first_dischtime,
    DATETIME_DIFF(DATETIME(a.dischtime), DATETIME(a.admittime), HOUR) / 24.0 AS adm_los,
    icu.first_careunit AS icu_type,
    icu.los AS icu_los,
    wk.weight_kg,
    wl.weight_lbs,
    hr.height_raw,
    hc.height_cm,
    hrx.heart_rate,
    abps.abp_systolic,
    abpd.abp_diastolic,
    rr.respiratory_rate,
    sp.oxygen_saturation,
    sofa.cardiovascular_sofa,
    IF(wf.used_warfarin IS NULL, 0, 1) AS used_warfarin,
    IF(cf.used_clopidogrel IS NULL, 0, 1) AS used_clopidogrel,
    IF(os.used_other_statins IS NULL, 0, 1) AS used_other_statins,
    IF(ppi.used_ppi IS NULL, 0, 1) AS used_ppi,
    IF(nsaid.used_nsaid IS NULL, 0, 1) AS used_nsaid,
    IF(ac.used_anticoagulant IS NULL, 0, 1) AS used_anticoagulant,
    IF(ss.used_ssri_snri IS NULL, 0, 1) AS used_ssri_snri,
    IF(bb.used_beta_blocker IS NULL, 0, 1) AS used_beta_blocker,
    IF(aa.used_ace_arb IS NULL, 0, 1) AS used_ace_arb,
    IF(diur.used_diuretic IS NULL, 0, 1) AS used_diuretic,
    dd.avg_d_dimer_before,
    dd.avg_d_dimer_after,
    ddb.last_d_dimer_before,
    dda.first_d_dimer_after,
    cls.avg_coag_before,
    cls.avg_coag_after,
    clb.last_coag_before,
    cla.first_coag_after,
    bls.avg_blood_lab_before,
    bls.avg_blood_lab_after,
    blb.last_blood_lab_before,
    bla.first_blood_lab_after
FROM `mimic-2024.ADR_first_medication_tables.simvastatin_HF` AS s
LEFT JOIN `physionet-data.mimiciv_3_1_hosp.patients` AS p
    ON s.subject_id = p.subject_id
LEFT JOIN `physionet-data.mimiciv_3_1_hosp.admissions` AS a
    ON s.subject_id = a.subject_id AND s.hadm_id = a.hadm_id
LEFT JOIN `physionet-data.mimiciv_3_1_icu.icustays` AS icu
    ON s.subject_id = icu.subject_id AND s.hadm_id = icu.hadm_id
LEFT JOIN weight_kg AS wk
    ON s.subject_id = wk.subject_id AND s.hadm_id = wk.hadm_id
LEFT JOIN weight_lbs AS wl
    ON s.subject_id = wl.subject_id AND s.hadm_id = wl.hadm_id
LEFT JOIN height_raw AS hr
    ON s.subject_id = hr.subject_id AND s.hadm_id = hr.hadm_id
LEFT JOIN height_cm AS hc
    ON s.subject_id = hc.subject_id AND s.hadm_id = hc.hadm_id
LEFT JOIN heart_rate AS hrx
    ON s.subject_id = hrx.subject_id AND s.hadm_id = hrx.hadm_id
LEFT JOIN abp_sys AS abps
    ON s.subject_id = abps.subject_id AND s.hadm_id = abps.hadm_id
LEFT JOIN abp_dia AS abpd
    ON s.subject_id = abpd.subject_id AND s.hadm_id = abpd.hadm_id
LEFT JOIN resp_rate AS rr
    ON s.subject_id = rr.subject_id AND s.hadm_id = rr.hadm_id
LEFT JOIN spo2 AS sp
    ON s.subject_id = sp.subject_id AND s.hadm_id = sp.hadm_id
LEFT JOIN sofa_cv AS sofa
    ON icu.stay_id = sofa.stay_id
LEFT JOIN warfarin_flag AS wf
    ON s.subject_id = wf.subject_id AND s.hadm_id = wf.hadm_id
LEFT JOIN clopidogrel_flag AS cf
    ON s.subject_id = cf.subject_id AND s.hadm_id = cf.hadm_id
LEFT JOIN other_statins_flag AS os
    ON s.subject_id = os.subject_id AND s.hadm_id = os.hadm_id
LEFT JOIN ppi_flag AS ppi
    ON s.subject_id = ppi.subject_id AND s.hadm_id = ppi.hadm_id
LEFT JOIN nsaid_flag AS nsaid
    ON s.subject_id = nsaid.subject_id AND s.hadm_id = nsaid.hadm_id
LEFT JOIN anticoag_flag AS ac
    ON s.subject_id = ac.subject_id AND s.hadm_id = ac.hadm_id
LEFT JOIN ssri_snri_flag AS ss
    ON s.subject_id = ss.subject_id AND s.hadm_id = ss.hadm_id
LEFT JOIN beta_blocker_flag AS bb
    ON s.subject_id = bb.subject_id AND s.hadm_id = bb.hadm_id
LEFT JOIN ace_arb_flag AS aa
    ON s.subject_id = aa.subject_id AND s.hadm_id = aa.hadm_id
LEFT JOIN diuretic_flag AS diur
    ON s.subject_id = diur.subject_id AND s.hadm_id = diur.hadm_id
LEFT JOIN d_dimer_stats AS dd
  ON s.subject_id = dd.subject_id AND s.hadm_id = dd.hadm_id AND s.starttime = dd.starttime
LEFT JOIN d_dimer_before AS ddb
  ON s.subject_id = ddb.subject_id AND s.hadm_id = ddb.hadm_id AND s.starttime = ddb.starttime
LEFT JOIN d_dimer_after AS dda
  ON s.subject_id = dda.subject_id AND s.hadm_id = dda.hadm_id AND s.starttime = dda.starttime
LEFT JOIN coag_labs_stats AS cls
  ON s.subject_id = cls.subject_id AND s.hadm_id = cls.hadm_id AND s.starttime = cls.starttime
LEFT JOIN coag_labs_before AS clb
  ON s.subject_id = clb.subject_id AND s.hadm_id = clb.hadm_id AND s.starttime = clb.starttime
LEFT JOIN coag_labs_after AS cla
  ON s.subject_id = cla.subject_id AND s.hadm_id = cla.hadm_id AND s.starttime = cla.starttime
LEFT JOIN blood_lab_stats AS bls
  ON s.subject_id = bls.subject_id AND s.hadm_id = bls.hadm_id AND s.starttime = bls.starttime
LEFT JOIN blood_lab_before AS blb
  ON s.subject_id = blb.subject_id AND s.hadm_id = blb.hadm_id AND s.starttime = blb.starttime
LEFT JOIN blood_lab_after AS bla
  ON s.subject_id = bla.subject_id AND s.hadm_id = bla.hadm_id AND s.starttime = bla.starttime
