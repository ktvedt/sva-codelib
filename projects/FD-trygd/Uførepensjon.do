****************************************************************************

use "N:\durable\Grunnlagsdata\FD-trygd\w20_0450_f_ufp.dta", replace 

save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\uforepensjon_tmp_2010.dta", replace

destring pnsjkode, replace

keep if (pnsjkode==20 | pnsjkode==21) 

drop if pnsjavg== . 

rename pnsjavg avgang

forvalues i=1992/2010 { 
gen uforepensjon_`i'_01 = 0 
replace uforepensjon_`i'_01 = 1 if pnsjtilg<=`i'01 & (avgang>=`i'01)
gen uforepensjon_`i'_02 = 0 
replace uforepensjon_`i'_02 = 1 if pnsjtilg<=`i'02 & (avgang>=`i'02)
gen uforepensjon_`i'_03 = 0 
replace uforepensjon_`i'_03 = 1 if pnsjtilg<=`i'03 & (avgang>=`i'03)
gen uforepensjon_`i'_04 = 0 
replace uforepensjon_`i'_04 = 1 if pnsjtilg<=`i'04 & (avgang>=`i'04)
gen uforepensjon_`i'_05 = 0 
replace uforepensjon_`i'_05 = 1 if pnsjtilg<=`i'05 & (avgang>=`i'05)
gen uforepensjon_`i'_06 = 0 
replace uforepensjon_`i'_06 = 1 if pnsjtilg<=`i'06 & (avgang>=`i'06)
gen uforepensjon_`i'_07 = 0 
replace uforepensjon_`i'_07 = 1 if pnsjtilg<=`i'07 & (avgang>=`i'07)
gen uforepensjon_`i'_08 = 0 
replace uforepensjon_`i'_08 = 1 if pnsjtilg<=`i'08 & (avgang>=`i'08)
gen uforepensjon_`i'_09 = 0 
replace uforepensjon_`i'_09 = 1 if pnsjtilg<=`i'09 & (avgang>=`i'09)
gen uforepensjon_`i'_10 = 0 
replace uforepensjon_`i'_10 = 1 if pnsjtilg<=`i'10 & (avgang>=`i'10)
gen uforepensjon_`i'_11 = 0 
replace uforepensjon_`i'_11 = 1 if pnsjtilg<=`i'11 & (avgang>=`i'11)
gen uforepensjon_`i'_12 = 0 
replace uforepensjon_`i'_12 = 1 if pnsjtilg<=`i'12 & (avgang>=`i'12)

}



//Finner alle månedsregisteringer innen person og lager nye variabler

forvalues i=1992/2010 {
egen uforepensjon_`i'01= total (uforepensjon_`i'_01), by (w20_0450_lopenr_person)
egen uforepensjon_`i'02= total (uforepensjon_`i'_02), by (w20_0450_lopenr_person)
egen uforepensjon_`i'03= total (uforepensjon_`i'_03), by (w20_0450_lopenr_person)
egen uforepensjon_`i'04= total (uforepensjon_`i'_04), by (w20_0450_lopenr_person)
egen uforepensjon_`i'05= total (uforepensjon_`i'_05), by (w20_0450_lopenr_person)
egen uforepensjon_`i'06= total (uforepensjon_`i'_06), by (w20_0450_lopenr_person)
egen uforepensjon_`i'07= total (uforepensjon_`i'_07), by (w20_0450_lopenr_person)
egen uforepensjon_`i'08= total (uforepensjon_`i'_08), by (w20_0450_lopenr_person)
egen uforepensjon_`i'09= total (uforepensjon_`i'_09), by (w20_0450_lopenr_person)
egen uforepensjon_`i'10= total (uforepensjon_`i'_10), by (w20_0450_lopenr_person)
egen uforepensjon_`i'11= total (uforepensjon_`i'_11), by (w20_0450_lopenr_person)
egen uforepensjon_`i'12= total (uforepensjon_`i'_12), by (w20_0450_lopenr_person)

}

//Tillegger observasjoner med samme forløpsdatoer verdien 1 

forvalues i=1992/2010 {
replace uforepensjon_`i'01= 1 if uforepensjon_`i'01 > 1 
replace uforepensjon_`i'02= 1 if uforepensjon_`i'02 > 1  
replace uforepensjon_`i'03= 1 if uforepensjon_`i'03 > 1  
replace uforepensjon_`i'04= 1 if uforepensjon_`i'04 > 1 
replace uforepensjon_`i'05= 1 if uforepensjon_`i'05 > 1  
replace uforepensjon_`i'06= 1 if uforepensjon_`i'06 > 1  
replace uforepensjon_`i'07= 1 if uforepensjon_`i'07 > 1  
replace uforepensjon_`i'08= 1 if uforepensjon_`i'08 > 1  
replace uforepensjon_`i'09= 1 if uforepensjon_`i'09 > 1  
replace uforepensjon_`i'10= 1 if uforepensjon_`i'10 > 1  
replace uforepensjon_`i'11= 1 if uforepensjon_`i'11 > 1  
replace uforepensjon_`i'12= 1 if uforepensjon_`i'12 > 1  

}



sort w20_0450_lopenr_person
quietly by w20_0450_lopenr_person :  gen dup = cond(_N==1,0,_n) //lager duplikat-variabel

drop if dup>1 //beholder kun først observasjon

drop dup

save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\uforepensjon_2010_wide.dta", replace //lagrer filen i wide-format



***2018***


use "N:\durable\Grunnlagsdata\FD-trygd\w20_0450_f_pensj_ufp.dta", replace

gen avgang= 0 

replace avgang= pnsjavg if avgang== 0
replace avgang= pnsjtom if avgang== .


forvalues i=2011/2018 { 
gen uforepensjon_`i'_01 = 0 
replace uforepensjon_`i'_01 = 1 if pnsjtilg<=`i'01 & (avgang>=`i'01 | avgang== .)
gen uforepensjon_`i'_02 = 0 
replace uforepensjon_`i'_02 = 1 if pnsjtilg<=`i'02 & (avgang>=`i'02 | avgang== .)
gen uforepensjon_`i'_03 = 0 
replace uforepensjon_`i'_03 = 1 if pnsjtilg<=`i'03 & (avgang>=`i'03 | avgang== .)
gen uforepensjon_`i'_04 = 0 
replace uforepensjon_`i'_04 = 1 if pnsjtilg<=`i'04 & (avgang>=`i'04 | avgang== .)
gen uforepensjon_`i'_05 = 0 
replace uforepensjon_`i'_05 = 1 if pnsjtilg<=`i'05 & (avgang>=`i'05 | avgang== .)
gen uforepensjon_`i'_06 = 0 
replace uforepensjon_`i'_06 = 1 if pnsjtilg<=`i'06 & (avgang>=`i'06 | avgang== .)
gen uforepensjon_`i'_07 = 0 
replace uforepensjon_`i'_07 = 1 if pnsjtilg<=`i'07 & (avgang>=`i'07 | avgang== .)
gen uforepensjon_`i'_08 = 0 
replace uforepensjon_`i'_08 = 1 if pnsjtilg<=`i'08 & (avgang>=`i'08 | avgang== .)
gen uforepensjon_`i'_09 = 0 
replace uforepensjon_`i'_09 = 1 if pnsjtilg<=`i'09 & (avgang>=`i'09 | avgang== .)
gen uforepensjon_`i'_10 = 0 
replace uforepensjon_`i'_10 = 1 if pnsjtilg<=`i'10 & (avgang>=`i'10 | avgang== .)
gen uforepensjon_`i'_11 = 0 
replace uforepensjon_`i'_11 = 1 if pnsjtilg<=`i'11 & (avgang>=`i'11 | avgang== .)
gen uforepensjon_`i'_12 = 0 
replace uforepensjon_`i'_12 = 1 if pnsjtilg<=`i'12 & (avgang>=`i'12 | avgang== .)

}



//Finner alle månedsregisteringer innen person og lager nye variabler

forvalues i=2011/2018 {
egen uforepensjon_`i'01= total (uforepensjon_`i'_01), by (w20_0450_lopenr_person)
egen uforepensjon_`i'02= total (uforepensjon_`i'_02), by (w20_0450_lopenr_person)
egen uforepensjon_`i'03= total (uforepensjon_`i'_03), by (w20_0450_lopenr_person)
egen uforepensjon_`i'04= total (uforepensjon_`i'_04), by (w20_0450_lopenr_person)
egen uforepensjon_`i'05= total (uforepensjon_`i'_05), by (w20_0450_lopenr_person)
egen uforepensjon_`i'06= total (uforepensjon_`i'_06), by (w20_0450_lopenr_person)
egen uforepensjon_`i'07= total (uforepensjon_`i'_07), by (w20_0450_lopenr_person)
egen uforepensjon_`i'08= total (uforepensjon_`i'_08), by (w20_0450_lopenr_person)
egen uforepensjon_`i'09= total (uforepensjon_`i'_09), by (w20_0450_lopenr_person)
egen uforepensjon_`i'10= total (uforepensjon_`i'_10), by (w20_0450_lopenr_person)
egen uforepensjon_`i'11= total (uforepensjon_`i'_11), by (w20_0450_lopenr_person)
egen uforepensjon_`i'12= total (uforepensjon_`i'_12), by (w20_0450_lopenr_person)

}

//Tillegger observasjoner med samme forløpsdatoer verdien 1 

forvalues i=2011/2018 {
replace uforepensjon_`i'01= 1 if uforepensjon_`i'01 > 1 
replace uforepensjon_`i'02= 1 if uforepensjon_`i'02 > 1  
replace uforepensjon_`i'03= 1 if uforepensjon_`i'03 > 1  
replace uforepensjon_`i'04= 1 if uforepensjon_`i'04 > 1 
replace uforepensjon_`i'05= 1 if uforepensjon_`i'05 > 1  
replace uforepensjon_`i'06= 1 if uforepensjon_`i'06 > 1  
replace uforepensjon_`i'07= 1 if uforepensjon_`i'07 > 1  
replace uforepensjon_`i'08= 1 if uforepensjon_`i'08 > 1  
replace uforepensjon_`i'09= 1 if uforepensjon_`i'09 > 1  
replace uforepensjon_`i'10= 1 if uforepensjon_`i'10 > 1  
replace uforepensjon_`i'11= 1 if uforepensjon_`i'11 > 1  
replace uforepensjon_`i'12= 1 if uforepensjon_`i'12 > 1  

}


sort w20_0450_lopenr_person
quietly by w20_0450_lopenr_person :  gen dup = cond(_N==1,0,_n) //lager duplikat-variabel

drop if dup>1 //beholder kun først observasjon

drop dup


save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\uforepensjon_2018_wide.dta", replace //lagrer filen i wide-format




***Kobler filene***

use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\uforepensjon_2010_wide.dta", replace 

merge 1:1 w20_0450_lopenr_person using "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\uforepensjon_2018_wide.dta"

drop _merge


***Gir de som har missing verdien 0

forvalues i=1992/2018 {
replace uforepensjon_`i'01= 0 if uforepensjon_`i'01 == .
replace uforepensjon_`i'02= 0 if uforepensjon_`i'02 == . 
replace uforepensjon_`i'03= 0 if uforepensjon_`i'03 == . 
replace uforepensjon_`i'04= 0 if uforepensjon_`i'04 == . 
replace uforepensjon_`i'05= 0 if uforepensjon_`i'05 == . 
replace uforepensjon_`i'06= 0 if uforepensjon_`i'06 == . 
replace uforepensjon_`i'07= 0 if uforepensjon_`i'07 == . 
replace uforepensjon_`i'08= 0 if uforepensjon_`i'08 == . 
replace uforepensjon_`i'09= 0 if uforepensjon_`i'09 == . 
replace uforepensjon_`i'10= 0 if uforepensjon_`i'10 == .
replace uforepensjon_`i'11= 0 if uforepensjon_`i'11 == . 
replace uforepensjon_`i'12= 0 if uforepensjon_`i'12 == . 

}


save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\uforepensjon_wide.dta", replace










