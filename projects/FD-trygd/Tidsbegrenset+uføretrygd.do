
****************************************************************************
*** Henter opp tidsbegrenset uføretrygd og tilrettelegger ***

use "N:\durable\Grunnlagsdata\FD-trygd\w20_0450_f_tu.dta", replace 

save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\tufor_tmp_2018.dta", replace

use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\tufor_tmp_2018.dta", replace

keep if kode=="0"

gen tilgang= tilgdato
gen avgang= avgdato

destring tilgang, replace
destring avgang, replace


forvalues i=2004/2018 { 
gen tufor_`i'_01 = 0 
replace tufor_`i'_01 = 1 if tilgang<=`i'01 & avgang>=`i'01  
gen tufor_`i'_02 = 0 
replace tufor_`i'_02 = 1 if tilgang<=`i'02 & avgang>=`i'02  
gen tufor_`i'_03 = 0 
replace tufor_`i'_03 = 1 if tilgang<=`i'03 & avgang>=`i'03 
gen tufor_`i'_04 = 0 
replace tufor_`i'_04 = 1 if tilgang<=`i'04 & avgang>=`i'04
gen tufor_`i'_05 = 0 
replace tufor_`i'_05 = 1 if tilgang<=`i'05 & avgang>=`i'05
gen tufor_`i'_06 = 0 
replace tufor_`i'_06 = 1 if tilgang<=`i'06 & avgang>=`i'06
gen tufor_`i'_07 = 0 
replace tufor_`i'_07 = 1 if tilgang<=`i'07 & avgang>=`i'07
gen tufor_`i'_08 = 0 
replace tufor_`i'_08 = 1 if tilgang<=`i'08 & avgang>=`i'08
gen tufor_`i'_09 = 0 
replace tufor_`i'_09 = 1 if tilgang<=`i'09 & avgang>=`i'09
gen tufor_`i'_10 = 0 
replace tufor_`i'_10 = 1 if tilgang<=`i'10 & avgang>=`i'10
gen tufor_`i'_11 = 0 
replace tufor_`i'_11 = 1 if tilgang<=`i'11 & avgang>=`i'11
gen tufor_`i'_12 = 0 
replace tufor_`i'_12 = 1 if tilgang<=`i'12 & avgang>=`i'12

}


save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\tufor_tmp_2018.dta", replace

use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\tufor_tmp_2018.dta", replace


//Finner alle månedsregisteringer innen person og lager nye variabler

forvalues i=2004/2018 {
egen tufor_`i'01= total (tufor_`i'_01), by (w20_0450_lopenr_person)
egen tufor_`i'02= total (tufor_`i'_02), by (w20_0450_lopenr_person)
egen tufor_`i'03= total (tufor_`i'_03), by (w20_0450_lopenr_person)
egen tufor_`i'04= total (tufor_`i'_04), by (w20_0450_lopenr_person)
egen tufor_`i'05= total (tufor_`i'_05), by (w20_0450_lopenr_person)
egen tufor_`i'06= total (tufor_`i'_06), by (w20_0450_lopenr_person)
egen tufor_`i'07= total (tufor_`i'_07), by (w20_0450_lopenr_person)
egen tufor_`i'08= total (tufor_`i'_08), by (w20_0450_lopenr_person)
egen tufor_`i'09= total (tufor_`i'_09), by (w20_0450_lopenr_person)
egen tufor_`i'10= total (tufor_`i'_10), by (w20_0450_lopenr_person)
egen tufor_`i'11= total (tufor_`i'_11), by (w20_0450_lopenr_person)
egen tufor_`i'12= total (tufor_`i'_12), by (w20_0450_lopenr_person)

}

//Tillegger observasjoner med samme forløpsdatoer verdien 1 

forvalues i=2004/2018 {
replace tufor_`i'01= 1 if tufor_`i'01 > 1 
replace tufor_`i'02= 1 if tufor_`i'02 > 1 
replace tufor_`i'03= 1 if tufor_`i'03 > 1 
replace tufor_`i'04= 1 if tufor_`i'04 > 1  
replace tufor_`i'05= 1 if tufor_`i'05 > 1 
replace tufor_`i'06= 1 if tufor_`i'06 > 1  
replace tufor_`i'07= 1 if tufor_`i'07 > 1  
replace tufor_`i'08= 1 if tufor_`i'08 > 1  
replace tufor_`i'09= 1 if tufor_`i'09 > 1  
replace tufor_`i'10= 1 if tufor_`i'10 > 1  
replace tufor_`i'11= 1 if tufor_`i'11 > 1 
replace tufor_`i'12= 1 if tufor_`i'12 > 1  

}



drop kode regdato tilgdato avgdato uforetp uforetp_dato ufg ufg_dato ///
     ungufor ungufor_dato tomdato tusfom_mnd tutom_mnd tilgang avgang ///
	 tufor_2004_01-tufor_2018_12 //fjerner de opprinnelige variablene


sort w20_0450_lopenr_person
quietly by w20_0450_lopenr_person :  gen dup = cond(_N==1,0,_n) //lager duplikat-variabel

drop if dup>1 //beholder kun først observasjon

drop dup

save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\tufor_wide.dta", replace //lagrer filen i wide-format
