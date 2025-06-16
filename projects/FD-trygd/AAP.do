
use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd\w20_0450_f_aap.dta", replace 

save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\AAP_tmp.dta", replace

use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\AAP_tmp.dta", replace


tostring tilgdato, replace
tostring avgdato, replace 

gen tilgang= tilgdato
gen avgang= tomdato

destring tilgang, replace
destring avgang, replace


forvalues i=2010/2018 { 
gen AAP_`i'_01 = 0 
replace AAP_`i'_01 = 1 if tilgang<=`i'01 & (avgang>=`i'01 | avgang== .)
gen AAP_`i'_02 = 0 
replace AAP_`i'_02 = 1 if tilgang<=`i'02 & (avgang>=`i'02 | avgang== .)
gen AAP_`i'_03 = 0 
replace AAP_`i'_03 = 1 if tilgang<=`i'03 & (avgang>=`i'03 | avgang== .)
gen AAP_`i'_04 = 0 
replace AAP_`i'_04 = 1 if tilgang<=`i'04 & (avgang>=`i'04 | avgang== .)
gen AAP_`i'_05 = 0 
replace AAP_`i'_05 = 1 if tilgang<=`i'05 & (avgang>=`i'05 | avgang== .)
gen AAP_`i'_06 = 0 
replace AAP_`i'_06 = 1 if tilgang<=`i'06 & (avgang>=`i'06 | avgang== .)
gen AAP_`i'_07 = 0 
replace AAP_`i'_07 = 1 if tilgang<=`i'07 & (avgang>=`i'07 | avgang== .)
gen AAP_`i'_08 = 0 
replace AAP_`i'_08 = 1 if tilgang<=`i'08 & (avgang>=`i'08 | avgang== .)
gen AAP_`i'_09 = 0 
replace AAP_`i'_09 = 1 if tilgang<=`i'09 & (avgang>=`i'09 | avgang== .)
gen AAP_`i'_10 = 0 
replace AAP_`i'_10 = 1 if tilgang<=`i'10 & (avgang>=`i'10 | avgang== .)
gen AAP_`i'_11 = 0 
replace AAP_`i'_11 = 1 if tilgang<=`i'11 & (avgang>=`i'11 | avgang== .)
gen AAP_`i'_12 = 0 
replace AAP_`i'_12 = 1 if tilgang<=`i'12 & (avgang>=`i'12 | avgang== .)

}

save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\AAP_tmp.dta", replace

use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\AAP_tmp.dta", replace


//Finner alle månedsregisteringer innen person og lager nye variabler

forvalues i=2010/2018 {
egen AAP_`i'01= total (AAP_`i'_01), by (w20_0450_lopenr_person)
egen AAP_`i'02= total (AAP_`i'_02), by (w20_0450_lopenr_person)
egen AAP_`i'03= total (AAP_`i'_03), by (w20_0450_lopenr_person)
egen AAP_`i'04= total (AAP_`i'_04), by (w20_0450_lopenr_person)
egen AAP_`i'05= total (AAP_`i'_05), by (w20_0450_lopenr_person)
egen AAP_`i'06= total (AAP_`i'_06), by (w20_0450_lopenr_person)
egen AAP_`i'07= total (AAP_`i'_07), by (w20_0450_lopenr_person)
egen AAP_`i'08= total (AAP_`i'_08), by (w20_0450_lopenr_person)
egen AAP_`i'09= total (AAP_`i'_09), by (w20_0450_lopenr_person)
egen AAP_`i'10= total (AAP_`i'_10), by (w20_0450_lopenr_person)
egen AAP_`i'11= total (AAP_`i'_11), by (w20_0450_lopenr_person)
egen AAP_`i'12= total (AAP_`i'_12), by (w20_0450_lopenr_person)

}

//Tillegger observasjoner med samme forløpsdatoer verdien 1 

forvalues i=2010/2018 {
replace AAP_`i'01= 1 if AAP_`i'01 > 1
replace AAP_`i'02= 1 if AAP_`i'02 > 1 
replace AAP_`i'03= 1 if AAP_`i'03 > 1 
replace AAP_`i'04= 1 if AAP_`i'04 > 1
replace AAP_`i'05= 1 if AAP_`i'05 > 1 
replace AAP_`i'06= 1 if AAP_`i'06 > 1 
replace AAP_`i'07= 1 if AAP_`i'07 > 1 
replace AAP_`i'08= 1 if AAP_`i'08 > 1
replace AAP_`i'09= 1 if AAP_`i'09 > 1 
replace AAP_`i'10= 1 if AAP_`i'10 > 1 
replace AAP_`i'11= 1 if AAP_`i'11 > 1 
replace AAP_`i'12= 1 if AAP_`i'12 > 1

}


drop AAP_2010_01-AAP_2018_12 //fjerner de opprinnelige variablene

drop kode-avgang


sort w20_0450_lopenr_person
quietly by w20_0450_lopenr_person :  gen dup = cond(_N==1,0,_n) //lager duplikat-variabel

drop if dup>1 //beholder kun først observasjon

drop dup

save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\AAP.dta", replace //lagrer filen i wide-format

