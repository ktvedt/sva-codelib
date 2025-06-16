 ****************************************************************************
*** Henter opp uførepensjon og tilrettelegger ***

use "N:\durable\Grunnlagsdata\FD-trygd\w20_0450_f_kvalif.dta", replace 

save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\KVP_tmp.dta", replace

use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\KVP_tmp.dta", replace

forvalues i=2008/2020 { 
gen kvp_`i'_01 = 0 
replace kvp_`i'_01 = 1 if tilgdato<=`i'01 & avgdato>=`i'01  
gen kvp_`i'_02 = 0 
replace kvp_`i'_02 = 1 if tilgdato<=`i'02 & avgdato>=`i'02  
gen kvp_`i'_03 = 0 
replace kvp_`i'_03 = 1 if tilgdato<=`i'03 & avgdato>=`i'03 
gen kvp_`i'_04 = 0 
replace kvp_`i'_04 = 1 if tilgdato<=`i'04 & avgdato>=`i'04
gen kvp_`i'_05 = 0 
replace kvp_`i'_05 = 1 if tilgdato<=`i'05 & avgdato>=`i'05
gen kvp_`i'_06 = 0 
replace kvp_`i'_06 = 1 if tilgdato<=`i'06 & avgdato>=`i'06
gen kvp_`i'_07 = 0 
replace kvp_`i'_07 = 1 if tilgdato<=`i'07 & avgdato>=`i'07
gen kvp_`i'_08 = 0 
replace kvp_`i'_08 = 1 if tilgdato<=`i'08 & avgdato>=`i'08
gen kvp_`i'_09 = 0 
replace kvp_`i'_09 = 1 if tilgdato<=`i'09 & avgdato>=`i'09
gen kvp_`i'_10 = 0 
replace kvp_`i'_10 = 1 if tilgdato<=`i'10 & avgdato>=`i'10
gen kvp_`i'_11 = 0 
replace kvp_`i'_11 = 1 if tilgdato<=`i'11 & avgdato>=`i'11
gen kvp_`i'_12 = 0 
replace kvp_`i'_12 = 1 if tilgdato<=`i'12 & avgdato>=`i'12

}


save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\KVP_tmp.dta", replace

use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\KVP_tmp.dta", replace


//Finner alle månedsregisteringer innen person og lager nye variabler
forvalues i=2008/2020 {
egen kvp_`i'01= total (kvp_`i'_01), by (w20_0450_lopenr_person)
egen kvp_`i'02= total (kvp_`i'_02), by (w20_0450_lopenr_person)
egen kvp_`i'03= total (kvp_`i'_03), by (w20_0450_lopenr_person)
egen kvp_`i'04= total (kvp_`i'_04), by (w20_0450_lopenr_person)
egen kvp_`i'05= total (kvp_`i'_05), by (w20_0450_lopenr_person)
egen kvp_`i'06= total (kvp_`i'_06), by (w20_0450_lopenr_person)
egen kvp_`i'07= total (kvp_`i'_07), by (w20_0450_lopenr_person)
egen kvp_`i'08= total (kvp_`i'_08), by (w20_0450_lopenr_person)
egen kvp_`i'09= total (kvp_`i'_09), by (w20_0450_lopenr_person)
egen kvp_`i'10= total (kvp_`i'_10), by (w20_0450_lopenr_person)
egen kvp_`i'11= total (kvp_`i'_11), by (w20_0450_lopenr_person)
egen kvp_`i'12= total (kvp_`i'_12), by (w20_0450_lopenr_person)

}

//Tillegger observasjoner med samme forløpsdatoer verdien 1 

forvalues i=2008/2020 {
replace kvp_`i'01= 1 if kvp_`i'01 > 1 
replace kvp_`i'02= 1 if kvp_`i'02 > 1  
replace kvp_`i'03= 1 if kvp_`i'03 > 1  
replace kvp_`i'04= 1 if kvp_`i'04 > 1 
replace kvp_`i'05= 1 if kvp_`i'05 > 1  
replace kvp_`i'06= 1 if kvp_`i'06 > 1  
replace kvp_`i'07= 1 if kvp_`i'07 > 1  
replace kvp_`i'08= 1 if kvp_`i'08 > 1  
replace kvp_`i'09= 1 if kvp_`i'09 > 1  
replace kvp_`i'10= 1 if kvp_`i'10 > 1  
replace kvp_`i'11= 1 if kvp_`i'11 > 1  
replace kvp_`i'12= 1 if kvp_`i'12 > 1   

}

drop kvp_2008_01-kvp_2020_12 //fjerner de opprinnelige variablene

drop kode regdato tilgdato avgdato tomdato

sort w20_0450_lopenr_person
quietly by w20_0450_lopenr_person :  gen dup = cond(_N==1,0,_n) //lager duplikat-variabel

drop if dup>1 //beholder kun først observasjon

drop dup


save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\KVP_wide.dta", replace //lagrer filen i wide-format

