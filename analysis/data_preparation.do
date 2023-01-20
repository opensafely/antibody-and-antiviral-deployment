********************************************************************************
*
*	Do-file:		data_preparation.do
*
*	Project:		sotrovimab-and-Paxlovid
*
*	Programmed by:	Bang Zheng
*
*	Data used:		output/input.csv
*
*	Data created:	output/main.dta  (main analysis dataset)
*
*	Other output:	logs/data_preparation.log

********************************************************************************

* Open a log file
cap log close
log using ./logs/data_preparation, replace t
clear

* import dataset
shell gunzip ./output/data/input.csv.gz
import delimited ./output/data/input.csv, delimiter(comma) varnames(1) case(preserve) 
describe
codebook

*  Convert strings to dates  *
foreach var of varlist sotrovimab_covid_therapeutics molnupiravir_covid_therapeutics paxlovid_covid_therapeutics remdesivir_covid_therapeutics	///
        casirivimab_covid_therapeutics sotrovimab_covid_approved sotrovimab_covid_complete sotrovimab_covid_not_start sotrovimab_covid_stopped ///
		paxlovid_covid_approved paxlovid_covid_complete paxlovid_covid_not_start paxlovid_covid_stopped covid_test_positive_pre_date ///
        covid_test_positive_date covid_test_positive_date2 covid_symptoms_snomed last_vaccination_date primary_covid_hospital_discharge primary_covid_hospital_admission ///
	   any_covid_hospital_discharge_dat preg_36wks_date death_date dereg_date downs_syndrome_nhsd_snomed downs_syndrome_nhsd_icd10 cancer_opensafely_snomed ///
	   cancer_opensafely_snomed_new haematopoietic_stem_cell_snomed haematopoietic_stem_cell_icd10 haematopoietic_stem_cell_opcs4 ///
	   haematological_malignancies_snom haematological_malignancies_icd1 sickle_cell_disease_nhsd_snomed sickle_cell_disease_nhsd_icd10 ///
	   ckd_stage_5_nhsd_snomed ckd_stage_5_nhsd_icd10 liver_disease_nhsd_snomed liver_disease_nhsd_icd10 immunosuppresant_drugs_nhsd ///
	   oral_steroid_drugs_nhsd immunosupression_nhsd immunosupression_nhsd_new hiv_aids_nhsd_snomed  solid_organ_transplant_nhsd_snom solid_organ_nhsd_snomed_new ///
	   solid_organ_transplant_nhsd_opcs solid_organ_transplant_nhsd solid_organ_transplant_nhsd_new multiple_sclerosis_nhsd_snomed multiple_sclerosis_nhsd_icd10 ///
	   motor_neurone_disease_nhsd_snome motor_neurone_disease_nhsd_icd10 myasthenia_gravis_nhsd_snomed myasthenia_gravis_nhsd_icd10 ///
	   huntingtons_disease_nhsd_snomed huntingtons_disease_nhsd_icd10 bmi_date_measured covid_positive_test_30_days_post ///
	   covid_hosp_outcome_date0 covid_hosp_outcome_date1 covid_hosp_outcome_date2 covid_hosp_discharge_date0 covid_hosp_discharge_date1 covid_hosp_discharge_date2 ///
	   covid_hosp_date_emergency0 covid_hosp_date_emergency1 covid_hosp_date_emergency2 covid_emerg_discharge_date0 covid_emerg_discharge_date1 covid_emerg_discharge_date2 ///
	   covid_hosp_date_mabs_procedure covid_hosp_date_mabs_not_pri covid_hosp_date0_not_primary covid_hosp_date1_not_primary covid_hosp_date2_not_primary ///
	   covid_discharge_date0_not_pri covid_discharge_date1_not_pri covid_discharge_date2_not_pri death_with_covid_on_the_death_ce death_with_covid_underlying_date hospitalisation_outcome_date0 ///
	   hospitalisation_outcome_date1 hospitalisation_outcome_date2 hosp_discharge_date0 hosp_discharge_date1 hosp_discharge_date2 covid_hosp_date_mabs_all_cause date_treated start_date ///
	   downs_syndrome_nhsd haematological_disease_nhsd ckd_stage_5_nhsd liver_disease_nhsd hiv_aids_nhsd  ///
	   multiple_sclerosis_nhsd motor_neurone_disease_nhsd myasthenia_gravis_nhsd huntingtons_disease_nhsd sickle_cell_disease_nhsd advanced_decompensated_cirrhosis decompensated_cirrhosis_icd10 ///
	   ascitic_drainage_snomed ascitic_drainage_snomed_pre ckd_stages_3_5 ckd_primis_stage_date ckd3_icd10 ckd4_icd10 ckd5_icd10 dialysis dialysis_icd10 dialysis_procedure kidney_transplant kidney_transplant_icd10 ///
	   kidney_transplant_procedure RRT RRT_icd10 RRT_procedure creatinine_ctv3_date creatinine_snomed_date creatinine_short_snomed_date eGFR_record_date eGFR_short_record_date ///
	   solid_organ_transplant_snomed drugs_do_not_use drugs_consider_risk  {
  capture confirm string variable `var'
  if _rc==0 {
  rename `var' a
  gen `var' = date(a, "YMD")
  drop a
  format %td `var'
  }
}
*the following date variables had no observation*
*hiv_aids_nhsd_icd10
*transplant_all_y_codes_opcs4
*transplant_thymus_opcs4
*transplant_conjunctiva_y_code_op
*transplant_conjunctiva_opcs4
*transplant_stomach_opcs4
*transplant_ileum_1_Y_codes_opcs4
*transplant_ileum_2_Y_codes_opcs4
*transplant_ileum_1_opcs4
*transplant_ileum_2_opcs4

*check hosp/death event date range*
codebook covid_hosp_outcome_date2 hospitalisation_outcome_date2 death_date

*exclusion criteria*
keep if sotrovimab_covid_therapeutics==start_date | paxlovid_covid_therapeutics==start_date | molnupiravir_covid_therapeutics==start_date
sum age,de
keep if age>=18 & age<110
tab sex,m
keep if sex=="F"|sex=="M"
keep if has_died==0
keep if registered_treated==1
tab covid_test_positive covid_positive_previous_30_days,m
*keep if covid_test_positive==1 & covid_positive_previous_30_days==0
*restrict start_date to 2022Feb10 to now*
*loose this restriction to increase N?*
keep if start_date>=mdy(12,16,2021)&start_date<=mdy(10,01,2022)
*drop if stp==""
*exclude those with multiple therapy*
drop if sotrovimab_covid_therapeutics!=. & ( molnupiravir_covid_therapeutics!=.| remdesivir_covid_therapeutics!=.| casirivimab_covid_therapeutics!=.|paxlovid_covid_therapeutics!=.)
drop if paxlovid_covid_therapeutics!=. & (  molnupiravir_covid_therapeutics!=.| remdesivir_covid_therapeutics!=.| casirivimab_covid_therapeutics!=.|sotrovimab_covid_therapeutics!=. )
drop if molnupiravir_covid_therapeutics!=. & (  paxlovid_covid_therapeutics!=.| remdesivir_covid_therapeutics!=.| casirivimab_covid_therapeutics!=.|sotrovimab_covid_therapeutics!=. )


*define exposure*
describe
gen drug=1 if sotrovimab_covid_therapeutics==start_date
replace drug=0 if paxlovid_covid_therapeutics ==start_date
replace drug=2 if molnupiravir_covid_therapeutics ==start_date
label define drug_Paxlovid 1 "sotrovimab" 0 "Paxlovid" 2 "molnupiravir"
label values drug drug_Paxlovid
tab drug,m


*correcting COVID hosp events: further ignore any day cases or sotro initiators who had COVID hosp record with mab procedure codes on Day 0 or 1 *
count if covid_hosp_outcome_date0!=start_date&covid_hosp_outcome_date0!=.
count if covid_hosp_outcome_date1!=(start_date+1)&covid_hosp_outcome_date1!=.
by drug, sort: count if covid_hosp_outcome_date0==covid_hosp_discharge_date0&covid_hosp_outcome_date0!=.
by drug, sort: count if covid_hosp_outcome_date1==covid_hosp_discharge_date1&covid_hosp_outcome_date1!=.
by drug, sort: count if covid_hosp_outcome_date0==covid_hosp_date_mabs_procedure&covid_hosp_date_mabs_procedure!=.
by drug, sort: count if covid_hosp_outcome_date1==covid_hosp_date_mabs_procedure&covid_hosp_date_mabs_procedure!=.
by drug, sort: count if covid_hosp_outcome_date0==covid_hosp_date_mabs_procedure&covid_hosp_outcome_date0!=.&covid_hosp_outcome_date0==covid_hosp_discharge_date0
by drug, sort: count if covid_hosp_outcome_date1==covid_hosp_date_mabs_procedure&covid_hosp_outcome_date1!=.&covid_hosp_outcome_date1==covid_hosp_discharge_date1
by drug, sort: count if covid_hosp_outcome_date0==covid_hosp_date_mabs_procedure&covid_hosp_outcome_date0!=.&(covid_hosp_discharge_date0 - covid_hosp_outcome_date0)==1
by drug, sort: count if covid_hosp_outcome_date1==covid_hosp_date_mabs_procedure&covid_hosp_outcome_date1!=.&(covid_hosp_discharge_date1 - covid_hosp_outcome_date1)==1
by drug, sort: count if covid_hosp_outcome_date0==covid_hosp_date_mabs_procedure&covid_hosp_outcome_date0!=.&(covid_hosp_discharge_date0 - covid_hosp_outcome_date0)==2
by drug, sort: count if covid_hosp_outcome_date1==covid_hosp_date_mabs_procedure&covid_hosp_outcome_date1!=.&(covid_hosp_discharge_date1 - covid_hosp_outcome_date1)==2
*check if any patient discharged in AM and admitted in PM*
count if primary_covid_hospital_admission!=.&primary_covid_hospital_discharge==.
count if primary_covid_hospital_admission==.&primary_covid_hospital_discharge!=.
count if primary_covid_hospital_admission!=.&primary_covid_hospital_discharge!=.&primary_covid_hospital_admission==primary_covid_hospital_discharge
count if primary_covid_hospital_admission!=.&primary_covid_hospital_discharge!=.&primary_covid_hospital_admission<primary_covid_hospital_discharge
count if primary_covid_hospital_admission!=.&primary_covid_hospital_discharge!=.&primary_covid_hospital_admission>primary_covid_hospital_discharge
*ignore day cases and mab procedures in day 0/1*
replace covid_hosp_outcome_date0=. if covid_hosp_outcome_date0==covid_hosp_discharge_date0&covid_hosp_outcome_date0!=.
replace covid_hosp_outcome_date1=. if covid_hosp_outcome_date1==covid_hosp_discharge_date1&covid_hosp_outcome_date1!=.
*replace covid_hosp_outcome_date0=. if covid_hosp_outcome_date0==covid_hosp_date_mabs_procedure&covid_hosp_date_mabs_procedure!=.&drug==1
*replace covid_hosp_outcome_date1=. if covid_hosp_outcome_date1==covid_hosp_date_mabs_procedure&covid_hosp_date_mabs_procedure!=.&drug==1

gen covid_hospitalisation_outcome_da=covid_hosp_outcome_date2
replace covid_hospitalisation_outcome_da=covid_hosp_outcome_date1 if covid_hosp_outcome_date1!=.
replace covid_hospitalisation_outcome_da=covid_hosp_outcome_date0 if covid_hosp_outcome_date0!=.&drug==0
replace covid_hospitalisation_outcome_da=covid_hosp_outcome_date0 if covid_hosp_outcome_date0!=.&drug==1

gen days_to_covid_admission=covid_hospitalisation_outcome_da-start_date if covid_hospitalisation_outcome_da!=.
by drug days_to_covid_admission, sort: count if covid_hospitalisation_outcome_da!=.

by drug, sort: count if covid_hosp_outcome_date2==covid_hosp_discharge_date2&covid_hosp_outcome_date2!=.&days_to_covid_admission>=2
by drug days_to_covid_admission, sort: count if covid_hosp_outcome_date2==covid_hosp_discharge_date2&covid_hosp_outcome_date2!=.&days_to_covid_admission>=2
by drug, sort: count if covid_hosp_outcome_date2==covid_hosp_date_mabs_procedure&covid_hosp_date_mabs_procedure!=.&days_to_covid_admission>=2
by drug days_to_covid_admission, sort: count if covid_hosp_outcome_date2==covid_hosp_date_mabs_procedure&covid_hosp_date_mabs_procedure!=.&days_to_covid_admission>=2
by drug, sort: count if covid_hosp_outcome_date2==covid_hosp_date_mabs_procedure&covid_hosp_outcome_date2!=.&covid_hosp_outcome_date2==covid_hosp_discharge_date2&days_to_covid_admission>=2
by drug, sort: count if covid_hosp_outcome_date2==covid_hosp_date_mabs_procedure&covid_hosp_outcome_date2!=.&(covid_hosp_discharge_date2 - covid_hosp_outcome_date2)==1&days_to_covid_admission>=2
by drug, sort: count if covid_hosp_outcome_date2==covid_hosp_date_mabs_procedure&covid_hosp_outcome_date2!=.&(covid_hosp_discharge_date2 - covid_hosp_outcome_date2)==2&days_to_covid_admission>=2
*ignore and censor day cases on or after day 2 from this analysis*
*ignore and censor admissions for mab procedure >= day 2 and with same-day or 1-day discharge*
*gen covid_hosp_date_day_cases_mab=covid_hospitalisation_outcome_da if covid_hosp_outcome_date2==covid_hosp_discharge_date2&covid_hosp_outcome_date2!=.&days_to_covid_admission>=2
*replace covid_hosp_date_day_cases_mab=covid_hospitalisation_outcome_da if covid_hosp_outcome_date2==covid_hosp_date_mabs_procedure&covid_hosp_date_mabs_procedure!=.&days_to_covid_admission>=2&(covid_hosp_discharge_date2-covid_hosp_outcome_date2)<=1&drug==1
drop if covid_hosp_outcome_date2==covid_hosp_discharge_date2&covid_hosp_outcome_date2!=.&days_to_covid_admission>=2
*replace covid_hospitalisation_outcome_da=. if covid_hosp_outcome_date2==covid_hosp_date_mabs_procedure&covid_hosp_date_mabs_procedure!=.&days_to_covid_admission>=2&(covid_hosp_discharge_date2-covid_hosp_outcome_date2)<=1&drug==1
*check hosp_admission_method*
tab covid_hosp_admission_method,m
tab drug covid_hosp_admission_method, row chi
*by drug days_to_covid_admission, sort: count if covid_hospitalisation_outcome_da!=covid_hosp_date_emergency&covid_hospitalisation_outcome_da!=.
*capture and exclude COVID-hospital admission/death on the start date
by drug, sort: count if start_date==covid_hospitalisation_outcome_da| start_date==death_with_covid_on_the_death_ce
drop if start_date>=covid_hospitalisation_outcome_da| start_date>=death_with_covid_on_the_death_ce|start_date>=death_date|start_date>=dereg_date


*define outcome and follow-up time*
gen study_end_date=mdy(12,22,2022)
gen start_date_30=start_date+30
by drug, sort: count if covid_hospitalisation_outcome_da!=.
by drug, sort: count if death_with_covid_on_the_death_ce!=.
by drug, sort: count if covid_hospitalisation_outcome_da==.&death_with_covid_on_the_death_ce!=.
by drug, sort: count if death_with_covid_on_the_death_ce==.&covid_hospitalisation_outcome_da!=.
*30-day COVID hosp*
gen covid_hospitalisation_30day=(covid_hospitalisation_outcome_da!=.&covid_hospitalisation_outcome_da<=start_date_30)
tab covid_hospitalisation_30day,m
tab drug covid_hospitalisation_30day,row m
tab drug covid_hospitalisation_30day if start_date>=mdy(12,16,2021)&start_date<=mdy(2,10,2022),row m
tab drug covid_hospitalisation_30day if start_date>=mdy(2,11,2022)&start_date<=mdy(5,31,2022),row m
tab drug covid_hospitalisation_30day if start_date>=mdy(6,1,2022)&start_date<=mdy(10,1,2022),row m
*30-day COVID death*
gen covid_death_30day=(death_with_covid_on_the_death_ce!=.&death_with_covid_on_the_death_ce<=start_date_30)
tab covid_death_30day,m
tab drug covid_death_30day,row m
tab drug covid_death_30day if start_date>=mdy(12,16,2021)&start_date<=mdy(2,10,2022),row m
tab drug covid_death_30day if start_date>=mdy(2,11,2022)&start_date<=mdy(5,31,2022),row m
tab drug covid_death_30day if start_date>=mdy(6,1,2022)&start_date<=mdy(10,1,2022),row m
*30-day all-cause death*
gen death_30day=(death_date!=.&death_date<=start_date_30)
tab death_30day,m
tab drug death_30day,row m
tab drug death_30day if start_date>=mdy(12,16,2021)&start_date<=mdy(2,10,2022),row m
tab drug death_30day if start_date>=mdy(2,11,2022)&start_date<=mdy(5,31,2022),row m
tab drug death_30day if start_date>=mdy(6,1,2022)&start_date<=mdy(10,1,2022),row m


*covariates* 
*10 high risk groups: downs_syndrome, solid_cancer, haematological_disease, renal_disease, liver_disease, imid, 
*immunosupression, hiv_aids, solid_organ_transplant, rare_neurological_conditions, high_risk_group_combined	
tab high_risk_cohort_covid_therapeut,m
by drug,sort: tab high_risk_cohort_covid_therapeut,m
gen downs_therapeutics= 1 if strpos(high_risk_cohort_covid_therapeut, "Downs syndrome")
gen solid_cancer_therapeutics=1 if strpos(high_risk_cohort_covid_therapeut, "solid cancer")
gen haema_disease_therapeutics=1 if strpos(high_risk_cohort_covid_therapeut, "haematological malignancies")
replace haema_disease_therapeutics=1 if strpos(high_risk_cohort_covid_therapeut, "sickle cell disease")
replace haema_disease_therapeutics=1 if strpos(high_risk_cohort_covid_therapeut, "haematological diseases")
replace haema_disease_therapeutics=1 if strpos(high_risk_cohort_covid_therapeut, "stem cell transplant")
gen renal_therapeutics= 1 if strpos(high_risk_cohort_covid_therapeut, "renal disease")
gen liver_therapeutics= 1 if strpos(high_risk_cohort_covid_therapeut, "liver disease")
gen imid_therapeutics= 1 if strpos(high_risk_cohort_covid_therapeut, "IMID")
gen immunosup_therapeutics= 1 if strpos(high_risk_cohort_covid_therapeut, "primary immune deficiencies")
gen hiv_aids_therapeutics= 1 if strpos(high_risk_cohort_covid_therapeut, "HIV or AIDS")
gen solid_organ_therapeutics= 1 if strpos(high_risk_cohort_covid_therapeut, "solid organ recipients")
replace solid_organ_therapeutics= 1 if strpos(high_risk_cohort_covid_therapeut, "solid organ transplant")
gen rare_neuro_therapeutics= 1 if strpos(high_risk_cohort_covid_therapeut, "rare neurological conditions")
*check if all diseases have been captured*
by drug,sort: count if high_risk_cohort_covid_therapeut!=""&high_risk_cohort_covid_therapeut!="other"& ///
   min(downs_therapeutics,solid_cancer_therapeutics,haema_disease_therapeutics,renal_therapeutics,liver_therapeutics,imid_therapeutics,immunosup_therapeutics,hiv_aids_therapeutics,solid_organ_therapeutics,rare_neuro_therapeutics)==.

replace oral_steroid_drugs_nhsd=. if oral_steroid_drug_nhsd_3m_count < 2 & oral_steroid_drug_nhsd_12m_count < 4
gen imid_nhsd=min(oral_steroid_drugs_nhsd, immunosuppresant_drugs_nhsd)
gen rare_neuro_nhsd = min(multiple_sclerosis_nhsd, motor_neurone_disease_nhsd, myasthenia_gravis_nhsd, huntingtons_disease_nhsd)

*gen downs_syndrome=(downs_syndrome_nhsd<=start_date|downs_therapeutics==1)
*gen solid_cancer=(cancer_opensafely_snomed<=start_date|solid_cancer_therapeutics==1)
*gen solid_cancer_new=(cancer_opensafely_snomed_new<=start_date|solid_cancer_therapeutics==1)
*gen haema_disease=( haematological_disease_nhsd <=start_date|haema_disease_therapeutics==1)
*gen renal_disease=( ckd_stage_5_nhsd <=start_date|renal_therapeutics==1)
*gen liver_disease=( liver_disease_nhsd <=start_date|liver_therapeutics==1)
*gen imid=( imid_nhsd <=start_date|imid_therapeutics==1)
*gen immunosupression=( immunosupression_nhsd <=start_date|immunosup_therapeutics==1)
*gen immunosupression_new=( immunosupression_nhsd_new <=start_date|immunosup_therapeutics==1)
*gen hiv_aids=( hiv_aids_nhsd <=start_date|hiv_aids_therapeutics==1)
*gen solid_organ=( solid_organ_transplant_nhsd<=start_date|solid_organ_therapeutics==1)
*gen solid_organ_new=( solid_organ_transplant_nhsd_new<=start_date|solid_organ_therapeutics==1)
*gen rare_neuro=( rare_neuro_nhsd <=start_date|rare_neuro_therapeutics==1)
*gen high_risk_group=(( downs_syndrome + solid_cancer + haema_disease + renal_disease + liver_disease + imid + immunosupression + hiv_aids + solid_organ + rare_neuro )>0)
*tab high_risk_group,m
*gen high_risk_group_new=(( downs_syndrome + solid_cancer_new + haema_disease + renal_disease + liver_disease + imid + immunosupression_new + hiv_aids + solid_organ_new + rare_neuro )>0)
*tab high_risk_group_new,m
*high risk group only based on codelists*
gen downs_syndrome=(downs_syndrome_nhsd<=start_date)
gen solid_cancer=(cancer_opensafely_snomed<=start_date)
gen solid_cancer_new=(cancer_opensafely_snomed_new<=start_date)
gen haema_disease=( haematological_disease_nhsd <=start_date)
gen renal_disease=( ckd_stage_5_nhsd <=start_date)
gen liver_disease=( liver_disease_nhsd <=start_date)
gen imid=( imid_nhsd <=start_date)
gen immunosupression=( immunosupression_nhsd <=start_date)
gen immunosupression_new=( immunosupression_nhsd_new <=start_date)
gen hiv_aids=( hiv_aids_nhsd <=start_date)
gen solid_organ=( solid_organ_transplant_nhsd<=start_date)
gen solid_organ_new=( solid_organ_transplant_nhsd_new<=start_date)
gen rare_neuro=( rare_neuro_nhsd <=start_date)
gen high_risk_group=(( downs_syndrome + solid_cancer + haema_disease + renal_disease + liver_disease + imid + immunosupression + hiv_aids + solid_organ + rare_neuro )>0)
tab high_risk_group,m
gen high_risk_group_new=(( downs_syndrome + solid_cancer_new + haema_disease + renal_disease + liver_disease + imid + immunosupression_new + hiv_aids + solid_organ_new + rare_neuro )>0)
tab high_risk_group_new,m

*Time between positive test and treatment*
gen d_postest_treat=start_date - covid_test_positive_date
tab d_postest_treat,m
replace d_postest_treat=. if d_postest_treat<0|d_postest_treat>7
gen d_postest_treat_g2=(d_postest_treat>=3) if d_postest_treat<=5
label define d_postest_treat_g2_Pax 0 "<3 days" 1 "3-5 days" 
label values d_postest_treat_g2 d_postest_treat_g2_Pax
gen d_postest_treat_missing=d_postest_treat_g2
replace d_postest_treat_missing=9 if d_postest_treat_g2==.
label define d_postest_treat_missing_Pax 0 "<3 days" 1 "3-5 days" 9 "missing" 
label values d_postest_treat_missing d_postest_treat_missing_Pax


*demo*
gen age_group3=(age>=40)+(age>=60)
label define age_group3_Paxlovid 0 "18-39" 1 "40-59" 2 ">=60" 
label values age_group3 age_group3_Paxlovid
tab age_group3,m
egen age_5y_band=cut(age), at(18,25,30,35,40,45,50,55,60,65,70,75,80,85,110) label
tab age_5y_band,m
gen age_50=(age>=50)
gen age_55=(age>=55)
gen age_60=(age>=60)

tab sex,m
rename sex sex_str
gen sex=0 if sex_str=="M"
replace sex=1 if sex_str=="F"
label define sex_Paxlovid 0 "Male" 1 "Female"
label values sex sex_Paxlovid

tab ethnicity,m
rename ethnicity ethnicity_with_missing_str
encode  ethnicity_with_missing_str ,gen(ethnicity_with_missing)
label list ethnicity_with_missing
gen ethnicity=ethnicity_with_missing
replace ethnicity=. if ethnicity_with_missing_str=="Missing"
label values ethnicity ethnicity_with_missing
gen White=1 if ethnicity==6
replace White=0 if ethnicity!=6&ethnicity!=.
gen White_with_missing=White
replace White_with_missing=9 if White==.


tab imd,m
replace imd=. if imd==0
label define imd_Paxlovid 1 "most deprived" 5 "least deprived"
label values imd imd_Paxlovid
gen imd_with_missing=imd
replace imd_with_missing=9 if imd==.

tab region_nhs,m
rename region_nhs region_nhs_str 
encode  region_nhs_str ,gen(region_nhs)
label list region_nhs

tab region_covid_therapeutics ,m
rename region_covid_therapeutics region_covid_therapeutics_str
encode  region_covid_therapeutics_str ,gen( region_covid_therapeutics )
label list region_covid_therapeutics

tab stp ,m
rename stp stp_str
encode  stp_str ,gen(stp)
label list stp

tab rural_urban,m
replace rural_urban=. if rural_urban<1
replace rural_urban=3 if rural_urban==4
replace rural_urban=5 if rural_urban==6
replace rural_urban=7 if rural_urban==8
tab rural_urban,m
gen rural_urban_with_missing=rural_urban
replace rural_urban_with_missing=99 if rural_urban==.
*comor*
tab autism_nhsd,m
tab care_home_primis,m
tab dementia_nhsd,m
tab housebound_opensafely,m
tab learning_disability_primis,m
tab serious_mental_illness_nhsd,m
sum bmi,de
replace bmi=. if bmi<10|bmi>60
rename bmi bmi_all
*latest BMI within recent 10 years*
gen bmi=bmi_all if bmi_date_measured!=.&bmi_date_measured>=start_date-365*10&(age+((bmi_date_measured-start_date)/365)>=18)
gen bmi_5y=bmi_all if bmi_date_measured!=.&bmi_date_measured>=start_date-365*5&(age+((bmi_date_measured-start_date)/365)>=18)
gen bmi_2y=bmi_all if bmi_date_measured!=.&bmi_date_measured>=start_date-365*2&(age+((bmi_date_measured-start_date)/365)>=18)
gen bmi_group4=(bmi>=18.5)+(bmi>=25.0)+(bmi>=30.0) if bmi!=.
label define bmi_Paxlovid 0 "underweight" 1 "normal" 2 "overweight" 3 "obese"
label values bmi_group4 bmi_Paxlovid
gen bmi_g4_with_missing=bmi_group4
replace bmi_g4_with_missing=9 if bmi_group4==.
gen bmi_g3=bmi_group4
replace bmi_g3=1 if bmi_g3==0
label values bmi_g3 bmi_Paxlovid
gen bmi_g3_with_missing=bmi_g3
replace bmi_g3_with_missing=9 if bmi_g3==.
gen bmi_25=(bmi>=25) if bmi!=.
gen bmi_30=(bmi>=30) if bmi!=.

tab diabetes,m
tab chronic_cardiac_disease,m
tab hypertension,m
tab chronic_respiratory_disease,m
*vac and variant*
tab vaccination_status,m
rename vaccination_status vaccination_status_g6
gen vaccination_status=0 if vaccination_status_g6=="Un-vaccinated"|vaccination_status_g6=="Un-vaccinated (declined)"
replace vaccination_status=1 if vaccination_status_g6=="One vaccination"
replace vaccination_status=2 if vaccination_status_g6=="Two vaccinations"
replace vaccination_status=3 if vaccination_status_g6=="Three vaccinations"
replace vaccination_status=4 if vaccination_status_g6=="Four or more vaccinations"
label define vac_Paxlovid 0 "Un-vaccinated" 1 "One vaccination" 2 "Two vaccinations" 3 "Three vaccinations" 4 "Four or more vaccinations"
label values vaccination_status vac_Paxlovid
gen vaccination_3=1 if vaccination_status==3|vaccination_status==4
replace vaccination_3=0 if vaccination_status<3
gen vaccination_g3=vaccination_3 
replace vaccination_g3=2 if vaccination_status==4
gen pre_infection=(covid_test_positive_pre_date<=(covid_test_positive_date - 30)&covid_test_positive_pre_date>mdy(1,1,2020)&covid_test_positive_pre_date!=.)
tab pre_infection,m
tab sgtf,m
tab sgtf_new, m
label define sgtf_new_Paxlovid 0 "S gene detected" 1 "confirmed SGTF" 9 "NA"
label values sgtf_new sgtf_new_Paxlovid
tab variant_recorded ,m
*tab sgtf variant_recorded ,m
*Time between last vaccination and treatment*
gen d_vaccinate_treat=start_date - last_vaccination_date
sum d_vaccinate_treat,de
gen month_after_vaccinate=ceil(d_vaccinate_treat/30)
tab month_after_vaccinate,m
gen week_after_vaccinate=ceil(d_vaccinate_treat/7)
tab week_after_vaccinate,m
*calendar time*
gen month_after_campaign=ceil((start_date-mdy(12,15,2021))/30)
tab month_after_campaign,m
gen week_after_campaign=ceil((start_date-mdy(12,15,2021))/7)
tab week_after_campaign,m
gen day_after_campaign=start_date-mdy(12,15,2021)
sum day_after_campaign,de


*exclude those with contraindications for Pax*
*solid organ transplant*
tab drug if solid_organ==1|solid_organ_therapeutics==1|solid_organ_transplant_snomed<=start_date
tab drug if solid_organ_new==1|solid_organ_therapeutics==1|solid_organ_transplant_snomed<=start_date
*liver*
tab drug if advanced_decompensated_cirrhosis<=start_date
tab drug if decompensated_cirrhosis_icd10<=start_date
tab drug if ascitic_drainage_snomed<=start_date
tab drug if ascitic_drainage_snomed<=start_date&ascitic_drainage_snomed>=(start_date-3*365.25)
tab drug if liver_disease_nhsd_icd10<=start_date
*renal*
tab drug if renal_disease==1|renal_therapeutics==1
tab drug if ckd_stages_3_5<=start_date
tab drug ckd_primis_stage,row
replace ckd_primis_stage=. if ckd_primis_stage_date>start_date
tab drug if ckd3_icd10<=start_date|ckd4_icd10<=start_date|ckd5_icd10<=start_date
tab drug if kidney_transplant<=start_date|kidney_transplant_icd10<=start_date|kidney_transplant_procedure<=start_date
tab drug if dialysis<=start_date|dialysis_icd10<=start_date|dialysis_procedure<=start_date
*egfr: adapted from https://github.com/opensafely/COVID-19-vaccine-breakthrough/blob/updates-feb/analysis/data_process.R*
tab creatinine_operator_ctv3,m
replace creatinine_ctv3 = . if !inrange(creatinine_ctv3, 20, 3000)| creatinine_ctv3_date>start_date
tab creatinine_operator_ctv3 if creatinine_ctv3!=.,m
replace creatinine_ctv3 = creatinine_ctv3/88.4
gen min_creatinine_ctv3=.
replace min_creatinine_ctv3 = (creatinine_ctv3/0.7)^-0.329 if sex==1
replace min_creatinine_ctv3 = (creatinine_ctv3/0.9)^-0.411 if sex==0
replace min_creatinine_ctv3 = 1 if min_creatinine_ctv3<1
gen max_creatinine_ctv3=.
replace max_creatinine_ctv3 = (creatinine_ctv3/0.7)^-1.209 if sex==1
replace max_creatinine_ctv3 = (creatinine_ctv3/0.9)^-1.209 if sex==0
replace max_creatinine_ctv3 = 1 if max_creatinine_ctv3>1
gen egfr_creatinine_ctv3 = min_creatinine_ctv3*max_creatinine_ctv3*141*(0.993^age_creatinine_ctv3) if age_creatinine_ctv3>0&age_creatinine_ctv3<=120
replace egfr_creatinine_ctv3 = egfr_creatinine_ctv3*1.018 if sex==1

tab creatinine_operator_snomed,m
tab creatinine_operator_snomed if creatinine_snomed!=.,m
replace creatinine_snomed = . if !inrange(creatinine_snomed, 20, 3000)| creatinine_snomed_date>start_date
replace creatinine_snomed_date = creatinine_short_snomed_date if missing(creatinine_snomed)
replace creatinine_operator_snomed = creatinine_operator_short_snomed if missing(creatinine_snomed)
replace age_creatinine_snomed = age_creatinine_short_snomed if missing(creatinine_snomed)
replace creatinine_snomed = creatinine_short_snomed if missing(creatinine_snomed)
replace creatinine_snomed = . if !inrange(creatinine_snomed, 20, 3000)| creatinine_snomed_date>start_date
replace creatinine_snomed = creatinine_snomed/88.4
gen min_creatinine_snomed=.
replace min_creatinine_snomed = (creatinine_snomed/0.7)^-0.329 if sex==1
replace min_creatinine_snomed = (creatinine_snomed/0.9)^-0.411 if sex==0
replace min_creatinine_snomed = 1 if min_creatinine_snomed<1
gen max_creatinine_snomed=.
replace max_creatinine_snomed = (creatinine_snomed/0.7)^-1.209 if sex==1
replace max_creatinine_snomed = (creatinine_snomed/0.9)^-1.209 if sex==0
replace max_creatinine_snomed = 1 if max_creatinine_snomed>1
gen egfr_creatinine_snomed = min_creatinine_snomed*max_creatinine_snomed*141*(0.993^age_creatinine_snomed) if age_creatinine_snomed>0&age_creatinine_snomed<=120
replace egfr_creatinine_snomed = egfr_creatinine_snomed*1.018 if sex==1

tab eGFR_operator if eGFR_record!=.,m
tab eGFR_short_operator if eGFR_short_record!=.,m
tab drug if (egfr_creatinine_ctv3<60&creatinine_operator_ctv3!="<")|(egfr_creatinine_snomed<60&creatinine_operator_snomed!="<")|(eGFR_record<60&eGFR_record>0&eGFR_operator!=">"&eGFR_operator!=">=")|(eGFR_short_record<60&eGFR_short_record>0&eGFR_short_operator!=">"&eGFR_short_operator!=">=")

*drug interactions*
tab drug if drugs_do_not_use<=start_date
tab drug if drugs_do_not_use<=start_date&drugs_do_not_use>=(start_date-3*365.25)
tab drug if drugs_do_not_use<=start_date&drugs_do_not_use>=(start_date-365.25)
tab drug if drugs_do_not_use<=start_date&drugs_do_not_use>=(start_date-180)
tab drug if drugs_do_not_use<=start_date&drugs_do_not_use>=(start_date-90)
tab drug if drugs_consider_risk<=start_date
tab drug if drugs_consider_risk<=start_date&drugs_consider_risk>=(start_date-3*365.25)
tab drug if drugs_consider_risk<=start_date&drugs_consider_risk>=(start_date-365.25)
tab drug if drugs_consider_risk<=start_date&drugs_consider_risk>=(start_date-180)
tab drug if drugs_consider_risk<=start_date&drugs_consider_risk>=(start_date-90)
gen drugs_do_not_use_contra=(drugs_do_not_use<=start_date&drugs_do_not_use>=(start_date-180))
gen drugs_consider_risk_contra=(drugs_consider_risk<=start_date&drugs_consider_risk>=(start_date-180))


drop if solid_organ_new==1|solid_organ_therapeutics==1|solid_organ_transplant_snomed<=start_date
drop if advanced_decompensated_cirrhosis<=start_date|decompensated_cirrhosis_icd10<=start_date|ascitic_drainage_snomed<=start_date|liver_disease_nhsd_icd10<=start_date
drop if renal_disease==1|renal_therapeutics==1|ckd_stages_3_5<=start_date|ckd_primis_stage==3|ckd_primis_stage==4|ckd_primis_stage==5|ckd3_icd10<=start_date|ckd4_icd10<=start_date|ckd5_icd10<=start_date
drop if kidney_transplant<=start_date|kidney_transplant_icd10<=start_date|kidney_transplant_procedure<=start_date
drop if dialysis<=start_date|dialysis_icd10<=start_date|dialysis_procedure<=start_date
drop if (egfr_creatinine_ctv3<60&creatinine_operator_ctv3!="<")|(egfr_creatinine_snomed<60&creatinine_operator_snomed!="<")|(eGFR_record<60&eGFR_record>0&eGFR_operator!=">"&eGFR_operator!=">=")|(eGFR_short_record<60&eGFR_short_record>0&eGFR_short_operator!=">"&eGFR_short_operator!=">=")
*drop if drugs_do_not_use<=start_date&drugs_do_not_use>=(start_date-365.25)
*drop if drugs_consider_risk<=start_date&drugs_consider_risk>=(start_date-365.25)
drop if drugs_do_not_use<=start_date&drugs_do_not_use>=(start_date-180)
*drop if drugs_consider_risk<=start_date&drugs_consider_risk>=(start_date-180)

tab drug if liver_disease_nhsd_snomed<=start_date
tab drug if liver_disease==1
tab drug if drugs_do_not_use<=start_date
tab drug if drugs_consider_risk<=start_date

*clean covariates*
tab month_after_vaccinate,m
*combine month7 and over due to small N*
replace month_after_vaccinate=7 if month_after_vaccinate>=7&month_after_vaccinate!=.
gen month_after_vaccinate_missing=month_after_vaccinate
replace month_after_vaccinate_missing=99 if month_after_vaccinate_missing==.
*calendar time*
tab week_after_campaign,m
*combine 9/10 and 26/27 due to small N*
*replace week_after_campaign=10 if week_after_campaign==9
*replace week_after_campaign=26 if week_after_campaign==27
*combine stps with low N (<100) as "Other"*
by stp, sort: gen stp_N=_N if stp!=.
replace stp=99 if stp_N<100
tab stp ,m


*descriptives by drug groups*
by drug,sort: sum age,de
*ttest age , by( drug )
by drug,sort: sum bmi,de
*ttest bmi, by( drug )
sum d_postest_treat ,de
by drug,sort: sum d_postest_treat ,de
*ttest d_postest_treat , by( drug )
*ranksum d_postest_treat,by(drug)
sum week_after_campaign,de
by drug,sort: sum week_after_campaign,de
*ttest week_after_campaign , by( drug )
*ranksum week_after_campaign,by(drug)
sum week_after_vaccinate,de
by drug,sort: sum week_after_vaccinate,de
*ttest week_after_vaccinate , by( drug )
*ranksum week_after_vaccinate,by(drug)
sum d_vaccinate_treat,de
by drug,sort: sum d_vaccinate_treat,de
*ttest d_vaccinate_treat , by( drug )
*ranksum d_vaccinate_treat,by(drug)

tab drug sex,row chi
tab drug ethnicity,row chi
tab drug White,row chi
tab drug imd,row chi
*ranksum imd,by(drug)
tab drug rural_urban,row chi
*ranksum rural_urban,by(drug)
tab drug region_nhs,row chi
tab drug region_covid_therapeutics,row chi
*need to address the error of "too many values"*
tab stp if drug==0
tab stp if drug==1
tab drug age_group3 ,row chi
tab drug d_postest_treat_g2 ,row chi
tab drug d_postest_treat ,row
tab drug downs_syndrome ,row chi
tab drug solid_cancer ,row chi
tab drug solid_cancer_new ,row chi
tab drug haema_disease ,row chi
tab drug renal_disease ,row chi
tab drug liver_disease ,row chi
tab drug imid ,row chi
tab drug immunosupression ,row chi
tab drug immunosupression_new ,row chi
tab drug hiv_aids ,row chi
tab drug solid_organ ,row chi
tab drug solid_organ_new ,row chi
tab drug rare_neuro ,row chi
tab drug high_risk_group ,row chi
tab drug high_risk_group_new ,row chi
tab drug autism_nhsd ,row chi
tab drug care_home_primis ,row chi
tab drug dementia_nhsd ,row chi
tab drug housebound_opensafely ,row chi
tab drug learning_disability_primis ,row chi
tab drug serious_mental_illness_nhsd ,row chi
tab drug bmi_group4 ,row chi
tab drug bmi_g3 ,row chi
tab drug diabetes ,row chi
tab drug chronic_cardiac_disease ,row chi
tab drug hypertension ,row chi
tab drug chronic_respiratory_disease ,row chi
tab drug vaccination_status ,row chi
tab drug month_after_vaccinate,row chi
tab drug sgtf ,row chi
tab drug sgtf_new ,row chi
tab drug drugs_consider_risk_contra,row chi
*tab drug variant_recorded ,row chi
tab drug if covid_test_positive_pre_date!=.

*30-day COVID hosp*
tab drug covid_hospitalisation_30day,row m
tab drug covid_hospitalisation_30day if start_date>=mdy(12,16,2021)&start_date<=mdy(2,10,2022),row m
tab drug covid_hospitalisation_30day if start_date>=mdy(2,11,2022)&start_date<=mdy(5,31,2022),row m
tab drug covid_hospitalisation_30day if start_date>=mdy(6,1,2022)&start_date<=mdy(10,1,2022),row m
*30-day COVID death*
tab drug covid_death_30day,row m
tab drug covid_death_30day if start_date>=mdy(12,16,2021)&start_date<=mdy(2,10,2022),row m
tab drug covid_death_30day if start_date>=mdy(2,11,2022)&start_date<=mdy(5,31,2022),row m
tab drug covid_death_30day if start_date>=mdy(6,1,2022)&start_date<=mdy(10,1,2022),row m
*30-day all-cause death*
tab drug death_30day,row m
tab drug death_30day if start_date>=mdy(12,16,2021)&start_date<=mdy(2,10,2022),row m
tab drug death_30day if start_date>=mdy(2,11,2022)&start_date<=mdy(5,31,2022),row m
tab drug death_30day if start_date>=mdy(6,1,2022)&start_date<=mdy(10,1,2022),row m



*check treatment status*
count if drug==0&paxlovid_covid_therapeutics==paxlovid_covid_approved
count if drug==0&paxlovid_covid_therapeutics==paxlovid_covid_complete
count if drug==0&paxlovid_covid_therapeutics==paxlovid_covid_not_start
count if drug==0&paxlovid_covid_therapeutics==paxlovid_covid_stopped
count if drug==0&paxlovid_covid_approved!=.
count if drug==0&paxlovid_covid_complete!=.
count if drug==0&paxlovid_covid_not_start!=.
count if drug==0&paxlovid_covid_stopped!=.
count if drug==1&sotrovimab_covid_therapeutics==sotrovimab_covid_approved
count if drug==1&sotrovimab_covid_therapeutics==sotrovimab_covid_complete
count if drug==1&sotrovimab_covid_therapeutics==sotrovimab_covid_not_start
count if drug==1&sotrovimab_covid_therapeutics==sotrovimab_covid_stopped
count if drug==1&sotrovimab_covid_approved!=.
count if drug==1&sotrovimab_covid_complete!=.
count if drug==1&sotrovimab_covid_not_start!=.
count if drug==1&sotrovimab_covid_stopped!=.


log close




