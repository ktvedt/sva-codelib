

****************************************************************************
*** Henter opp medisinsk rehabilitering og tilrettelegger ***

use "N:\durable\Grunnlagsdata\FD-trygd\w20_0450_f_attf.dta", replace 

save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\medrehab_tmp.dta", replace

use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\medrehab_tmp.dta", replace

 
destring tilgdato_mnd, replace
destring avgdato_mnd, replace

destring attfkode, replace

keep if (attfkode==10 | attfkode_mnd==11 | attfkode_mnd==12 | attfkode_mnd==13) /// Beholder kun de på medisinsk rehabilitering
       
drop if avgdato_mnd== . 
	   
	   
forvalues i=1992/2018 { 
gen medrehab_`i'_01 = 0 
replace medrehab_`i'_01 = 1 if tilgdato_mnd<=`i'01 & avgdato_mnd>=`i'01  
gen medrehab_`i'_02 = 0 
replace medrehab_`i'_02 = 1 if tilgdato_mnd<=`i'02 & avgdato_mnd>=`i'02  
gen medrehab_`i'_03 = 0 
replace medrehab_`i'_03 = 1 if tilgdato_mnd<=`i'03 & avgdato_mnd>=`i'03 
gen medrehab_`i'_04 = 0 
replace medrehab_`i'_04 = 1 if tilgdato_mnd<=`i'04 & avgdato_mnd>=`i'04
gen medrehab_`i'_05 = 0 
replace medrehab_`i'_05 = 1 if tilgdato_mnd<=`i'05 & avgdato_mnd>=`i'05
gen medrehab_`i'_06 = 0 
replace medrehab_`i'_06 = 1 if tilgdato_mnd<=`i'06 & avgdato_mnd>=`i'06
gen medrehab_`i'_07 = 0 
replace medrehab_`i'_07 = 1 if tilgdato_mnd<=`i'07 & avgdato_mnd>=`i'07
gen medrehab_`i'_08 = 0 
replace medrehab_`i'_08 = 1 if tilgdato_mnd<=`i'08 & avgdato_mnd>=`i'08
gen medrehab_`i'_09 = 0 
replace medrehab_`i'_09 = 1 if tilgdato_mnd<=`i'09 & avgdato_mnd>=`i'09
gen medrehab_`i'_10 = 0 
replace medrehab_`i'_10 = 1 if tilgdato_mnd<=`i'10 & avgdato_mnd>=`i'10
gen medrehab_`i'_11 = 0 
replace medrehab_`i'_11 = 1 if tilgdato_mnd<=`i'11 & avgdato_mnd>=`i'11
gen medrehab_`i'_12 = 0 
replace medrehab_`i'_12 = 1 if tilgdato_mnd<=`i'12 & avgdato_mnd>=`i'12

}


save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\medrehab_tmp.dta", replace

use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\medrehab_tmp.dta", replace


//Finner alle månedsregisteringer innen person og lager nye variabler

forvalues i=1992/2018 {
egen medrehab_`i'01= total (medrehab_`i'_01), by (w20_0450_lopenr_person)
egen medrehab_`i'02= total (medrehab_`i'_02), by (w20_0450_lopenr_person)
egen medrehab_`i'03= total (medrehab_`i'_03), by (w20_0450_lopenr_person)
egen medrehab_`i'04= total (medrehab_`i'_04), by (w20_0450_lopenr_person)
egen medrehab_`i'05= total (medrehab_`i'_05), by (w20_0450_lopenr_person)
egen medrehab_`i'06= total (medrehab_`i'_06), by (w20_0450_lopenr_person)
egen medrehab_`i'07= total (medrehab_`i'_07), by (w20_0450_lopenr_person)
egen medrehab_`i'08= total (medrehab_`i'_08), by (w20_0450_lopenr_person)
egen medrehab_`i'09= total (medrehab_`i'_09), by (w20_0450_lopenr_person)
egen medrehab_`i'10= total (medrehab_`i'_10), by (w20_0450_lopenr_person)
egen medrehab_`i'11= total (medrehab_`i'_11), by (w20_0450_lopenr_person)
egen medrehab_`i'12= total (medrehab_`i'_12), by (w20_0450_lopenr_person)

}

//Tillegger observasjoner med samme forløpsdatoer verdien 1 

forvalues i=1992/2018 {
replace medrehab_`i'01= 1 if medrehab_`i'01 > 1 
replace medrehab_`i'02= 1 if medrehab_`i'02 > 1 
replace medrehab_`i'03= 1 if medrehab_`i'03 > 1 
replace medrehab_`i'04= 1 if medrehab_`i'04 > 1 
replace medrehab_`i'05= 1 if medrehab_`i'05 > 1 
replace medrehab_`i'06= 1 if medrehab_`i'06 > 1 
replace medrehab_`i'07= 1 if medrehab_`i'07 > 1 
replace medrehab_`i'08= 1 if medrehab_`i'08 > 1 
replace medrehab_`i'09= 1 if medrehab_`i'09 > 1 
replace medrehab_`i'10= 1 if medrehab_`i'10 > 1 
replace medrehab_`i'11= 1 if medrehab_`i'11 > 1 
replace medrehab_`i'12= 1 if medrehab_`i'12 > 1 

}


drop medrehab_1992_01-medrehab_2018_12 //fjerner de opprinnelige variablene

drop attfkode-avgdato_mnd


sort w20_0450_lopenr_person
quietly by w20_0450_lopenr_person :  gen dup = cond(_N==1,0,_n) //lager duplikat-variabel

drop if dup>1 //beholder kun først observasjon

drop dup

drop ufg bfg attftilg_mnd attfavg_mnd ufgdato_mnd attftom_mnd

save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\medrehab_wide.dta", replace //lagrer filen i wide-format






