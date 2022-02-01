-- This code is run on the High Cost Drugs dataset. The table returned should be saved as 'drug_name_summary.csv' and saved in the location below
-- GitHub\antibody-and-antiviral-deployment\output\high cost drugs codelist\drug_name_summary.csv


use OpenCorona

 select
	DrugName,
	HighCostTariffExcludedDrugCode,
	DerivedSNOMEDFromName,
	DerivedVTM,
	DerivedVTMName,
	case
		when count(Patient_ID) <= 5  then NULL
		else count(Patient_ID) 
	end as Num_Issues
	
from OpenCorona.dbo.HighCostDrugs

group by
	DrugName,
	HighCostTariffExcludedDrugCode,
	DerivedSNOMEDFromName,
	DerivedVTM,
	DerivedVTMName 

order by
	Num_Issues DESC