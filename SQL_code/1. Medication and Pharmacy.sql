-- Select relevant fields
SELECT *
-- MIMIC-IV: medication orders table
FROM `physionet-data.mimiciv_3_1_hosp.pharmacy`
WHERE LOWER(medication) IN ('clopidogrel', 'warfarin', 'simvastatin');