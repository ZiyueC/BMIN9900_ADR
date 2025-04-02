WITH target_patients AS (
    SELECT DISTINCT subject_id
    FROM `mimic-2024.ADR_first_medication_tables.simvastatin_HF`
)

SELECT 
    a.subject_id, 
    a.hadm_id, 
    a.admittime, 
    a.dischtime, 
    d.icd_code, 
    d.icd_version, 
    dicd.long_title AS icd_description
FROM `physionet-data.mimiciv_3_1_hosp.admissions` a
LEFT JOIN `physionet-data.mimiciv_3_1_hosp.diagnoses_icd` d 
    ON a.subject_id = d.subject_id AND a.hadm_id = d.hadm_id
LEFT JOIN `physionet-data.mimiciv_3_1_hosp.d_icd_diagnoses` dicd 
    ON d.icd_code = dicd.icd_code AND d.icd_version = dicd.icd_version
WHERE a.subject_id IN (SELECT subject_id FROM target_patients)
  AND LOWER(dicd.long_title) LIKE '%heart failure%'
ORDER BY a.subject_id, a.hadm_id, d.icd_code
