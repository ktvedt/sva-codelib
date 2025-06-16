

****************************************************************************
*** Henter opp attføring og tilrettelegger ***

use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd\w20_0450_f_attf.dta", replace 

save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\attforing_tmp_2001.dta", replace

use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\attforing_tmp_2001.dta", replace

destring attfkode_mnd, replace

keep if (attfkode_mnd==20 | attfkode_mnd==21 | attfkode_mnd==22 | attfkode_mnd==23 | ///
         attfkode_mnd==24) ///Beholder kun attføring
 
rename attftilg_mnd tilgang 
rename attftom_mnd avgang

destring tilgang, replace
destring avgang, replace

forvalues i=1992/2001 { 
gen attforing_`i'_01 = 0 
replace attforing_`i'_01 = 1 if tilgang<=`i'01 & avgang>=`i'01  
gen attforing_`i'_02 = 0 
replace attforing_`i'_02 = 1 if tilgang<=`i'02 & avgang>=`i'02  
gen attforing_`i'_03 = 0 
replace attforing_`i'_03 = 1 if tilgang<=`i'03 & avgang>=`i'03 
gen attforing_`i'_04 = 0 
replace attforing_`i'_04 = 1 if tilgang<=`i'04 & avgang>=`i'04
gen attforing_`i'_05 = 0 
replace attforing_`i'_05 = 1 if tilgang<=`i'05 & avgang>=`i'05
gen attforing_`i'_06 = 0 
replace attforing_`i'_06 = 1 if tilgang<=`i'06 & avgang>=`i'06
gen attforing_`i'_07 = 0 
replace attforing_`i'_07 = 1 if tilgang<=`i'07 & avgang>=`i'07
gen attforing_`i'_08 = 0 
replace attforing_`i'_08 = 1 if tilgang<=`i'08 & avgang>=`i'08
gen attforing_`i'_09 = 0 
replace attforing_`i'_09 = 1 if tilgang<=`i'09 & avgang>=`i'09
gen attforing_`i'_10 = 0 
replace attforing_`i'_10 = 1 if tilgang<=`i'10 & avgang>=`i'10
gen attforing_`i'_11 = 0 
replace attforing_`i'_11 = 1 if tilgang<=`i'11 & avgang>=`i'11
gen attforing_`i'_12 = 0 
replace attforing_`i'_12 = 1 if tilgang<=`i'12 & avgang>=`i'12

}


save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\attforing_tmp_2001.dta", replace

use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\attforing_tmp_2001.dta", replace


//Finner alle månedsregisteringer innen person og lager nye variabler

forvalues i=1992/2001 {
egen attforing_`i'01= total (attforing_`i'_01), by (w20_0450_lopenr_person)
egen attforing_`i'02= total (attforing_`i'_02), by (w20_0450_lopenr_person)
egen attforing_`i'03= total (attforing_`i'_03), by (w20_0450_lopenr_person)
egen attforing_`i'04= total (attforing_`i'_04), by (w20_0450_lopenr_person)
egen attforing_`i'05= total (attforing_`i'_05), by (w20_0450_lopenr_person)
egen attforing_`i'06= total (attforing_`i'_06), by (w20_0450_lopenr_person)
egen attforing_`i'07= total (attforing_`i'_07), by (w20_0450_lopenr_person)
egen attforing_`i'08= total (attforing_`i'_08), by (w20_0450_lopenr_person)
egen attforing_`i'09= total (attforing_`i'_09), by (w20_0450_lopenr_person)
egen attforing_`i'10= total (attforing_`i'_10), by (w20_0450_lopenr_person)
egen attforing_`i'11= total (attforing_`i'_11), by (w20_0450_lopenr_person)
egen attforing_`i'12= total (attforing_`i'_12), by (w20_0450_lopenr_person)

}

//Tillegger observasjoner med samme forløpsdatoer verdien 1 

forvalues i=1992/2001 {
replace attforing_`i'01= 1 if attforing_`i'01 > 1 
replace attforing_`i'02= 1 if attforing_`i'02 > 1 
replace attforing_`i'03= 1 if attforing_`i'03 > 1 
replace attforing_`i'04= 1 if attforing_`i'04 > 1 
replace attforing_`i'05= 1 if attforing_`i'05 > 1 
replace attforing_`i'06= 1 if attforing_`i'06 > 1 
replace attforing_`i'07= 1 if attforing_`i'07 > 1 
replace attforing_`i'08= 1 if attforing_`i'08 > 1 
replace attforing_`i'09= 1 if attforing_`i'09 > 1 
replace attforing_`i'10= 1 if attforing_`i'10 > 1 
replace attforing_`i'11= 1 if attforing_`i'11 > 1 
replace attforing_`i'12= 1 if attforing_`i'12 > 1 

}


drop attfkode-attforing_2001_12 ///fjerner variabler


sort w20_0450_lopenr_person
quietly by w20_0450_lopenr_person :  gen dup = cond(_N==1,0,_n) ///lager duplikat-variabel

drop if dup>1 //beholder kun først observasjon

drop dup

drop ufg bfg

save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\attforing_2001_wide.dta", replace ///lagrer filen i wide-format



****************************************************************************
*** Henter opp attføring og tilrettelegger ***

use "N:\durable\Grunnlagsdata\FD-trygd\w20_0450_f_arbsok.dta", replace 

save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\attforing_tmp_2010.dta", replace

use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\attforing_tmp_2010.dta", replace

keep if ytelse=="AT" /// MERK - sjekk at denne fungerer. 

gen tilgang= tilgdato_mnd
gen avgang= asoktom_mnd

destring tilgang, replace
destring avgang, replace


forvalues i=2001/2018 { 
gen attforing_`i'_01 = 0 
replace attforing_`i'_01 = 1 if tilgang<=`i'01 & (avgang>=`i'01 | avgang== .)
gen attforing_`i'_02 = 0 
replace attforing_`i'_02 = 1 if tilgang<=`i'02 & (avgang>=`i'02 | avgang== .)
gen attforing_`i'_03 = 0 
replace attforing_`i'_03 = 1 if tilgang<=`i'03 & (avgang>=`i'03 | avgang== .)
gen attforing_`i'_04 = 0 
replace attforing_`i'_04 = 1 if tilgang<=`i'04 & (avgang>=`i'04 | avgang== .)
gen attforing_`i'_05 = 0 
replace attforing_`i'_05 = 1 if tilgang<=`i'05 & (avgang>=`i'05 | avgang== .)
gen attforing_`i'_06 = 0 
replace attforing_`i'_06 = 1 if tilgang<=`i'06 & (avgang>=`i'06 | avgang== .)
gen attforing_`i'_07 = 0 
replace attforing_`i'_07 = 1 if tilgang<=`i'07 & (avgang>=`i'07 | avgang== .)
gen attforing_`i'_08 = 0 
replace attforing_`i'_08 = 1 if tilgang<=`i'08 & (avgang>=`i'08 | avgang== .)
gen attforing_`i'_09 = 0 
replace attforing_`i'_09 = 1 if tilgang<=`i'09 & (avgang>=`i'09 | avgang== .)
gen attforing_`i'_10 = 0 
replace attforing_`i'_10 = 1 if tilgang<=`i'10 & (avgang>=`i'10 | avgang== .)
gen attforing_`i'_11 = 0 
replace attforing_`i'_11 = 1 if tilgang<=`i'11 & (avgang>=`i'11 | avgang== .)
gen attforing_`i'_12 = 0 
replace attforing_`i'_12 = 1 if tilgang<=`i'12 & (avgang>=`i'12 | avgang== .)

}


save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\attforing_tmp_2010.dta", replace

use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\attforing_tmp_2010.dta", replace


//Finner alle månedsregisteringer innen person og lager nye variabler

forvalues i=2001/2018 {
egen attforing_`i'01= total (attforing_`i'_01), by (w20_0450_lopenr_person)
egen attforing_`i'02= total (attforing_`i'_02), by (w20_0450_lopenr_person)
egen attforing_`i'03= total (attforing_`i'_03), by (w20_0450_lopenr_person)
egen attforing_`i'04= total (attforing_`i'_04), by (w20_0450_lopenr_person)
egen attforing_`i'05= total (attforing_`i'_05), by (w20_0450_lopenr_person)
egen attforing_`i'06= total (attforing_`i'_06), by (w20_0450_lopenr_person)
egen attforing_`i'07= total (attforing_`i'_07), by (w20_0450_lopenr_person)
egen attforing_`i'08= total (attforing_`i'_08), by (w20_0450_lopenr_person)
egen attforing_`i'09= total (attforing_`i'_09), by (w20_0450_lopenr_person)
egen attforing_`i'10= total (attforing_`i'_10), by (w20_0450_lopenr_person)
egen attforing_`i'11= total (attforing_`i'_11), by (w20_0450_lopenr_person)
egen attforing_`i'12= total (attforing_`i'_12), by (w20_0450_lopenr_person)

}

//Tillegger observasjoner med samme forløpsdatoer verdien 1 

forvalues i=2001/2018 {
replace attforing_`i'01= 1 if attforing_`i'01 > 1 
replace attforing_`i'02= 1 if attforing_`i'02 > 1 
replace attforing_`i'03= 1 if attforing_`i'03 > 1 
replace attforing_`i'04= 1 if attforing_`i'04 > 1 
replace attforing_`i'05= 1 if attforing_`i'05 > 1 
replace attforing_`i'06= 1 if attforing_`i'06 > 1 
replace attforing_`i'07= 1 if attforing_`i'07 > 1 
replace attforing_`i'08= 1 if attforing_`i'08 > 1 
replace attforing_`i'09= 1 if attforing_`i'09 > 1 
replace attforing_`i'10= 1 if attforing_`i'10 > 1 
replace attforing_`i'11= 1 if attforing_`i'11 > 1 
replace attforing_`i'12= 1 if attforing_`i'12 > 1 

}


drop attforing_2001_01-attforing_2018_12 //fjerner de opprinnelige variablene

drop kode-avgdato_mnd

drop attforing_200101-attforing_200104 ///Fjerner variabler som er på den andre fila 

sort w20_0450_lopenr_person
quietly by w20_0450_lopenr_person :  gen dup = cond(_N==1,0,_n) //lager duplikat-variabel

drop if dup>1 //beholder kun først observasjon

drop dup

save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\attforing_2018_wide.dta", replace //lagrer filen i wide-format


***Kobler filene***

use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\attforing_2001_wide.dta", replace 

merge 1:1 w20_0450_lopenr_person using "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\attforing_2018_wide.dta"

drop _merge


***Gir de som har missing verdien 0

forvalues i=1992/2018 {
replace attforing_`i'01= 0 if attforing_`i'01 == .
replace attforing_`i'02= 0 if attforing_`i'02 == . 
replace attforing_`i'03= 0 if attforing_`i'03 == . 
replace attforing_`i'04= 0 if attforing_`i'04 == . 
replace attforing_`i'05= 0 if attforing_`i'05 == . 
replace attforing_`i'06= 0 if attforing_`i'06 == . 
replace attforing_`i'07= 0 if attforing_`i'07 == . 
replace attforing_`i'08= 0 if attforing_`i'08 == . 
replace attforing_`i'09= 0 if attforing_`i'09 == . 
replace attforing_`i'10= 0 if attforing_`i'10 == .
replace attforing_`i'11= 0 if attforing_`i'11 == . 
replace attforing_`i'12= 0 if attforing_`i'12 == . 

}


save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\attforing_wide.dta", replace