

***Lager sysselsettingsfil på månedsbasis fra 1992 til 2003

use "N:\durable\Grunnlagsdata\FD-trygd\w20_0450_f_sys.dta", replace 


//Fjerner de selvstendige

destring syskode, replace

keep if syskode < 20

save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\sysselsatt_2004_tmp.dta", replace

use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\sysselsatt_2004_tmp.dta", replace

gen tilgang=systilg_mnd
gen  avgang=systom_mnd

destring tilgang, replace
destring avgang, replace

//Lager månedsobservasjoner innen år for sysselsetting

forvalues i=1992/2004 { 
gen sysselsatt_`i'_01 = 0 
replace sysselsatt_`i'_01 = 1 if tilgang<=`i'01 & (avgang>=`i'01 | avgang== .)  
gen sysselsatt_`i'_02 = 0 
replace sysselsatt_`i'_02 = 1 if tilgang<=`i'02 & (avgang>=`i'02 | avgang== .)
gen sysselsatt_`i'_03 = 0 
replace sysselsatt_`i'_03 = 1 if tilgang<=`i'03 & (avgang>=`i'03 | avgang== .)
gen sysselsatt_`i'_04 = 0 
replace sysselsatt_`i'_04 = 1 if tilgang<=`i'04 & (avgang>=`i'04 | avgang== .)
gen sysselsatt_`i'_05 = 0 
replace sysselsatt_`i'_05 = 1 if tilgang<=`i'05 & (avgang>=`i'05 | avgang== .)
gen sysselsatt_`i'_06 = 0 
replace sysselsatt_`i'_06 = 1 if tilgang<=`i'06 & (avgang>=`i'06 | avgang== .)
gen sysselsatt_`i'_07 = 0 
replace sysselsatt_`i'_07 = 1 if tilgang<=`i'07 & (avgang>=`i'07 | avgang== .)
gen sysselsatt_`i'_08 = 0 
replace sysselsatt_`i'_08 = 1 if tilgang<=`i'08 & (avgang>=`i'08 | avgang== .)
gen sysselsatt_`i'_09 = 0 
replace sysselsatt_`i'_09 = 1 if tilgang<=`i'09 & (avgang>=`i'09 | avgang== .)
gen sysselsatt_`i'_10 = 0 
replace sysselsatt_`i'_10 = 1 if tilgang<=`i'10 & (avgang>=`i'10 | avgang== .)
gen sysselsatt_`i'_11 = 0 
replace sysselsatt_`i'_11 = 1 if tilgang<=`i'11 & (avgang>=`i'11 | avgang== .)
gen sysselsatt_`i'_12 = 0 
replace sysselsatt_`i'_12 = 1 if tilgang<=`i'12 & (avgang>=`i'12 | avgang== .)

}



//Finner alle månedsregisteringer innen person og lager nye variabler

forvalues i=1992/2004 {
egen sysselsatt_`i'01= total (sysselsatt_`i'_01), by (w20_0450_lopenr_person)
egen sysselsatt_`i'02= total (sysselsatt_`i'_02), by (w20_0450_lopenr_person)
egen sysselsatt_`i'03= total (sysselsatt_`i'_03), by (w20_0450_lopenr_person)
egen sysselsatt_`i'04= total (sysselsatt_`i'_04), by (w20_0450_lopenr_person)
egen sysselsatt_`i'05= total (sysselsatt_`i'_05), by (w20_0450_lopenr_person)
egen sysselsatt_`i'06= total (sysselsatt_`i'_06), by (w20_0450_lopenr_person)
egen sysselsatt_`i'07= total (sysselsatt_`i'_07), by (w20_0450_lopenr_person)
egen sysselsatt_`i'08= total (sysselsatt_`i'_08), by (w20_0450_lopenr_person)
egen sysselsatt_`i'09= total (sysselsatt_`i'_09), by (w20_0450_lopenr_person)
egen sysselsatt_`i'10= total (sysselsatt_`i'_10), by (w20_0450_lopenr_person)
egen sysselsatt_`i'11= total (sysselsatt_`i'_11), by (w20_0450_lopenr_person)
egen sysselsatt_`i'12= total (sysselsatt_`i'_12), by (w20_0450_lopenr_person)

}

// Gir alle månedsregisteringer hvor en person er sysselsatt verdien 1 

forvalues i=1992/2004 {
replace sysselsatt_`i'01= 1 if sysselsatt_`i'01 > 1
replace sysselsatt_`i'02= 1 if sysselsatt_`i'02 > 1
replace sysselsatt_`i'03= 1 if sysselsatt_`i'03 > 1
replace sysselsatt_`i'04= 1 if sysselsatt_`i'04 > 1
replace sysselsatt_`i'05= 1 if sysselsatt_`i'05 > 1
replace sysselsatt_`i'06= 1 if sysselsatt_`i'06 > 1
replace sysselsatt_`i'07= 1 if sysselsatt_`i'07 > 1
replace sysselsatt_`i'08= 1 if sysselsatt_`i'08 > 1
replace sysselsatt_`i'09= 1 if sysselsatt_`i'09 > 1
replace sysselsatt_`i'10= 1 if sysselsatt_`i'10 > 1
replace sysselsatt_`i'11= 1 if sysselsatt_`i'11 > 1
replace sysselsatt_`i'12= 1 if sysselsatt_`i'12 > 1

}

drop  syskode naring FORV_ARB w20_0450_a_o_ref NACE_NAR w20_0450_orgn_ref ///
      regdato_mnd tilgdato_mnd avgdato_mnd systilg_mnd sysavg_mnd NAR_DATO_mnd ///
	  FVA_DATO_mnd A_O_DATO_mnd nacedato_mnd orgndato_mnd tomdato_mnd ///
	  systom_mnd sysselsatt_1992_01-sysselsatt_2004_12

sort w20_0450_lopenr_person
quietly by w20_0450_lopenr_person :  gen dup = cond(_N==1,0,_n) //lager duplikat-variabel
tab dup

drop if dup>1 //beholder kun først observasjon	 

drop dup	 


save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\sysselsatt_2004.dta", replace

**************************************************************************************************
**************************************************************************************************

***Lager sysselsettingsfil på månedsbasis fra 2004 til 2014

use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd\w20_0450_f_jobber.dta", replace 


save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\sysselsatt_2004_2014_tmp.dta", replace

use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\sysselsatt_2004_2014_tmp.dta", replace

gen tilgang=tilgdato_mnd
gen  avgang=tomdato_mnd


destring tilgang, replace
destring avgang, replace
destring regtom_mnd, replace

replace avgang=regtom_mnd if avgang== .


//Lager månedsobservasjoner innen år for sysselsetting

forvalues i=2004/2014 { 
gen sysselsatt_`i'_01 = 0 
replace sysselsatt_`i'_01 = 1 if tilgang<=`i'01 & (avgang>=`i'01 | avgang== .)  
gen sysselsatt_`i'_02 = 0 
replace sysselsatt_`i'_02 = 1 if tilgang<=`i'02 & (avgang>=`i'02 | avgang== .)
gen sysselsatt_`i'_03 = 0 
replace sysselsatt_`i'_03 = 1 if tilgang<=`i'03 & (avgang>=`i'03 | avgang== .)
gen sysselsatt_`i'_04 = 0 
replace sysselsatt_`i'_04 = 1 if tilgang<=`i'04 & (avgang>=`i'04 | avgang== .)
gen sysselsatt_`i'_05 = 0 
replace sysselsatt_`i'_05 = 1 if tilgang<=`i'05 & (avgang>=`i'05 | avgang== .)
gen sysselsatt_`i'_06 = 0 
replace sysselsatt_`i'_06 = 1 if tilgang<=`i'06 & (avgang>=`i'06 | avgang== .)
gen sysselsatt_`i'_07 = 0 
replace sysselsatt_`i'_07 = 1 if tilgang<=`i'07 & (avgang>=`i'07 | avgang== .)
gen sysselsatt_`i'_08 = 0 
replace sysselsatt_`i'_08 = 1 if tilgang<=`i'08 & (avgang>=`i'08 | avgang== .)
gen sysselsatt_`i'_09 = 0 
replace sysselsatt_`i'_09 = 1 if tilgang<=`i'09 & (avgang>=`i'09 | avgang== .)
gen sysselsatt_`i'_10 = 0 
replace sysselsatt_`i'_10 = 1 if tilgang<=`i'10 & (avgang>=`i'10 | avgang== .)
gen sysselsatt_`i'_11 = 0 
replace sysselsatt_`i'_11 = 1 if tilgang<=`i'11 & (avgang>=`i'11 | avgang== .)
gen sysselsatt_`i'_12 = 0 
replace sysselsatt_`i'_12 = 1 if tilgang<=`i'12 & (avgang>=`i'12 | avgang== .)

}



//Finner alle månedsregisteringer innen person og lager nye variabler

forvalues i=2004/2014 {
egen sysselsatt_`i'01= total (sysselsatt_`i'_01), by (w20_0450_lopenr_person)
egen sysselsatt_`i'02= total (sysselsatt_`i'_02), by (w20_0450_lopenr_person)
egen sysselsatt_`i'03= total (sysselsatt_`i'_03), by (w20_0450_lopenr_person)
egen sysselsatt_`i'04= total (sysselsatt_`i'_04), by (w20_0450_lopenr_person)
egen sysselsatt_`i'05= total (sysselsatt_`i'_05), by (w20_0450_lopenr_person)
egen sysselsatt_`i'06= total (sysselsatt_`i'_06), by (w20_0450_lopenr_person)
egen sysselsatt_`i'07= total (sysselsatt_`i'_07), by (w20_0450_lopenr_person)
egen sysselsatt_`i'08= total (sysselsatt_`i'_08), by (w20_0450_lopenr_person)
egen sysselsatt_`i'09= total (sysselsatt_`i'_09), by (w20_0450_lopenr_person)
egen sysselsatt_`i'10= total (sysselsatt_`i'_10), by (w20_0450_lopenr_person)
egen sysselsatt_`i'11= total (sysselsatt_`i'_11), by (w20_0450_lopenr_person)
egen sysselsatt_`i'12= total (sysselsatt_`i'_12), by (w20_0450_lopenr_person)

}

// Gir alle månedsregisteringer hvor en person er sysselsatt verdien 1 

forvalues i=2004/2014 {
replace sysselsatt_`i'01= 1 if sysselsatt_`i'01 > 1
replace sysselsatt_`i'02= 1 if sysselsatt_`i'02 > 1
replace sysselsatt_`i'03= 1 if sysselsatt_`i'03 > 1
replace sysselsatt_`i'04= 1 if sysselsatt_`i'04 > 1
replace sysselsatt_`i'05= 1 if sysselsatt_`i'05 > 1
replace sysselsatt_`i'06= 1 if sysselsatt_`i'06 > 1
replace sysselsatt_`i'07= 1 if sysselsatt_`i'07 > 1
replace sysselsatt_`i'08= 1 if sysselsatt_`i'08 > 1
replace sysselsatt_`i'09= 1 if sysselsatt_`i'09 > 1
replace sysselsatt_`i'10= 1 if sysselsatt_`i'10 > 1
replace sysselsatt_`i'11= 1 if sysselsatt_`i'11 > 1
replace sysselsatt_`i'12= 1 if sysselsatt_`i'12 > 1

}


sort w20_0450_lopenr_person
quietly by w20_0450_lopenr_person :  gen dup = cond(_N==1,0,_n) //lager duplikat-variabel
tab dup

drop if dup>1 //beholder kun først observasjon	 

drop dup	 


save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\sysselsatt_2014.dta", replace








