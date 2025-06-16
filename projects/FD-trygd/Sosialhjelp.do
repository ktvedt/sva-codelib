****************************************************************************
*** Henter opp sosialhjelp og tilrettelegger ***

use "N:\durable\Grunnlagsdata\FD-trygd\w20_0450_f_shj.dta", replace 

save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\sosialhjelp_tmp.dta", replace

use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\sosialhjelp_tmp.dta", replace

keep if shjkode=="0"

//Lager månedsobservasjoner innen år for mottak av sosialhjelp
forvalues i=1992/2018 { 
gen sosialhjelp_`i'_01 = 0 
replace sosialhjelp_`i'_01 = 1 if shj_tilg<=`i'01 & shj_avg>=`i'01  
gen sosialhjelp_`i'_02 = 0 
replace sosialhjelp_`i'_02 = 1 if shj_tilg<=`i'02 & shj_avg>=`i'02  
gen sosialhjelp_`i'_03 = 0 
replace sosialhjelp_`i'_03 = 1 if shj_tilg<=`i'03 & shj_avg>=`i'03 
gen sosialhjelp_`i'_04 = 0 
replace sosialhjelp_`i'_04 = 1 if shj_tilg<=`i'04 & shj_avg>=`i'04
gen sosialhjelp_`i'_05 = 0 
replace sosialhjelp_`i'_05 = 1 if shj_tilg<=`i'05 & shj_avg>=`i'05
gen sosialhjelp_`i'_06 = 0 
replace sosialhjelp_`i'_06 = 1 if shj_tilg<=`i'06 & shj_avg>=`i'06
gen sosialhjelp_`i'_07 = 0 
replace sosialhjelp_`i'_07 = 1 if shj_tilg<=`i'07 & shj_avg>=`i'07
gen sosialhjelp_`i'_08 = 0 
replace sosialhjelp_`i'_08 = 1 if shj_tilg<=`i'08 & shj_avg>=`i'08
gen sosialhjelp_`i'_09 = 0 
replace sosialhjelp_`i'_09 = 1 if shj_tilg<=`i'09 & shj_avg>=`i'09
gen sosialhjelp_`i'_10 = 0 
replace sosialhjelp_`i'_10 = 1 if shj_tilg<=`i'10 & shj_avg>=`i'10
gen sosialhjelp_`i'_11 = 0 
replace sosialhjelp_`i'_11 = 1 if shj_tilg<=`i'11 & shj_avg>=`i'11
gen sosialhjelp_`i'_12 = 0 
replace sosialhjelp_`i'_12 = 1 if shj_tilg<=`i'12 & shj_avg>=`i'12

}


save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\sosialhjelp_tmp.dta", replace

use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\sosialhjelp_tmp.dta", replace

//Finner alle månedsregisteringer innen person og lager nye variabler

forvalues i=1992/2018 {
egen sosialhjelp_`i'01= total (sosialhjelp_`i'_01), by (w20_0450_lopenr_person)
egen sosialhjelp_`i'02= total (sosialhjelp_`i'_02), by (w20_0450_lopenr_person)
egen sosialhjelp_`i'03= total (sosialhjelp_`i'_03), by (w20_0450_lopenr_person)
egen sosialhjelp_`i'04= total (sosialhjelp_`i'_04), by (w20_0450_lopenr_person)
egen sosialhjelp_`i'05= total (sosialhjelp_`i'_05), by (w20_0450_lopenr_person)
egen sosialhjelp_`i'06= total (sosialhjelp_`i'_06), by (w20_0450_lopenr_person)
egen sosialhjelp_`i'07= total (sosialhjelp_`i'_07), by (w20_0450_lopenr_person)
egen sosialhjelp_`i'08= total (sosialhjelp_`i'_08), by (w20_0450_lopenr_person)
egen sosialhjelp_`i'09= total (sosialhjelp_`i'_09), by (w20_0450_lopenr_person)
egen sosialhjelp_`i'10= total (sosialhjelp_`i'_10), by (w20_0450_lopenr_person)
egen sosialhjelp_`i'11= total (sosialhjelp_`i'_11), by (w20_0450_lopenr_person)
egen sosialhjelp_`i'12= total (sosialhjelp_`i'_12), by (w20_0450_lopenr_person)

}


//Tillegger observasjoner med samme forløpsdatoer verdien 1 

forvalues i=1992/2018 {
replace sosialhjelp_`i'01= 1 if sosialhjelp_`i'01 > 1  
replace sosialhjelp_`i'02= 1 if sosialhjelp_`i'02 > 1 
replace sosialhjelp_`i'03= 1 if sosialhjelp_`i'03 > 1  
replace sosialhjelp_`i'04= 1 if sosialhjelp_`i'04 > 1  
replace sosialhjelp_`i'05= 1 if sosialhjelp_`i'05 > 1  
replace sosialhjelp_`i'06= 1 if sosialhjelp_`i'06 > 1 
replace sosialhjelp_`i'07= 1 if sosialhjelp_`i'07 > 1  
replace sosialhjelp_`i'08= 1 if sosialhjelp_`i'08 > 1  
replace sosialhjelp_`i'09= 1 if sosialhjelp_`i'09 > 1  
replace sosialhjelp_`i'10= 1 if sosialhjelp_`i'10 > 1  
replace sosialhjelp_`i'11= 1 if sosialhjelp_`i'11 > 1  
replace sosialhjelp_`i'12= 1 if sosialhjelp_`i'12 > 1  

}



drop sosialhjelp_1992_01-sosialhjelp_2018_12 //fjerner de opprinnelige variablene

sort w20_0450_lopenr_person
quietly by w20_0450_lopenr_person :  gen dup = cond(_N==1,0,_n) //lager duplikat-variabel

drop if dup>1 //beholder kun først observasjon

drop shjkode regdato shj_tilg shj_avg tomdato dup //fjerner opprinnelige variabler 

save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\sosialhjelp_mnd_wide.dta", replace //lagrer filen i wide-format
 