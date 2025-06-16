


****************************************************************************
*** Henter opp arbeidssøker og tilrettelegger ***

***Lager arbeidssøkerfil på månedsbasis fra 1992 til 2001

use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd\w20_0450_f_arbsok_2001", replace 

save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\arbeidsoker_2001_tmp.dta", replace

use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\arbeidsoker_2001_tmp.dta", replace

***Beholder de MED dagpenger***

keep if (stonad=="DP") 

***Beholder de helt ledige***

gen helt_ledig= substr(arbstat,1,2)

keep if helt_ledig=="HL"

tostring asoktilg_mnd, replace
tostring asoktom_mnd, replace 

gen tilgang= substr(asoktilg_mnd,1,6)
gen avgang= substr(asoktom_mnd,1,6)

destring tilgang, replace
destring avgang, replace


//Lager månedsobservasjoner innen år for arbeidssøker

forvalues i=1992/2001 { 
gen arbeidssoker_DP_`i'_01 = 0 
replace arbeidssoker_DP_`i'_01 = 1 if tilgang<=`i'01 & (avgang>=`i'01)  
gen arbeidssoker_DP_`i'_02 = 0 
replace arbeidssoker_DP_`i'_02 = 1 if tilgang<=`i'02 & (avgang>=`i'02)
gen arbeidssoker_DP_`i'_03 = 0 
replace arbeidssoker_DP_`i'_03 = 1 if tilgang<=`i'03 & (avgang>=`i'03)
gen arbeidssoker_DP_`i'_04 = 0 
replace arbeidssoker_DP_`i'_04 = 1 if tilgang<=`i'04 & (avgang>=`i'04)
gen arbeidssoker_DP_`i'_05 = 0 
replace arbeidssoker_DP_`i'_05 = 1 if tilgang<=`i'05 & (avgang>=`i'05)
gen arbeidssoker_DP_`i'_06 = 0 
replace arbeidssoker_DP_`i'_06 = 1 if tilgang<=`i'06 & (avgang>=`i'06)
gen arbeidssoker_DP_`i'_07 = 0 
replace arbeidssoker_DP_`i'_07 = 1 if tilgang<=`i'07 & (avgang>=`i'07)
gen arbeidssoker_DP_`i'_08 = 0 
replace arbeidssoker_DP_`i'_08 = 1 if tilgang<=`i'08 & (avgang>=`i'08)
gen arbeidssoker_DP_`i'_09 = 0 
replace arbeidssoker_DP_`i'_09 = 1 if tilgang<=`i'09 & (avgang>=`i'09)
gen arbeidssoker_DP_`i'_10 = 0 
replace arbeidssoker_DP_`i'_10 = 1 if tilgang<=`i'10 & (avgang>=`i'10)
gen arbeidssoker_DP_`i'_11 = 0 
replace arbeidssoker_DP_`i'_11 = 1 if tilgang<=`i'11 & (avgang>=`i'11)
gen arbeidssoker_DP_`i'_12 = 0 
replace arbeidssoker_DP_`i'_12 = 1 if tilgang<=`i'12 & (avgang>=`i'12)

}

save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\arbeidsoker_2001_tmp.dta", replace


use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\arbeidsoker_2001_tmp.dta", replace

//Finner alle månedsregisteringer innen person og lager nye variabler

forvalues i=1992/2001 {
egen arbeidssoker_DP_`i'01= total (arbeidssoker_DP_`i'_01), by (w20_0450_lopenr_person)
egen arbeidssoker_DP_`i'02= total (arbeidssoker_DP_`i'_02), by (w20_0450_lopenr_person)
egen arbeidssoker_DP_`i'03= total (arbeidssoker_DP_`i'_03), by (w20_0450_lopenr_person)
egen arbeidssoker_DP_`i'04= total (arbeidssoker_DP_`i'_04), by (w20_0450_lopenr_person)
egen arbeidssoker_DP_`i'05= total (arbeidssoker_DP_`i'_05), by (w20_0450_lopenr_person)
egen arbeidssoker_DP_`i'06= total (arbeidssoker_DP_`i'_06), by (w20_0450_lopenr_person)
egen arbeidssoker_DP_`i'07= total (arbeidssoker_DP_`i'_07), by (w20_0450_lopenr_person)
egen arbeidssoker_DP_`i'08= total (arbeidssoker_DP_`i'_08), by (w20_0450_lopenr_person)
egen arbeidssoker_DP_`i'09= total (arbeidssoker_DP_`i'_09), by (w20_0450_lopenr_person)
egen arbeidssoker_DP_`i'10= total (arbeidssoker_DP_`i'_10), by (w20_0450_lopenr_person)
egen arbeidssoker_DP_`i'11= total (arbeidssoker_DP_`i'_11), by (w20_0450_lopenr_person)
egen arbeidssoker_DP_`i'12= total (arbeidssoker_DP_`i'_12), by (w20_0450_lopenr_person)

}

// Gir alle månedsregisteringer hvor en person er arbeidssoker verdien 1 

forvalues i=1992/2001 {
replace arbeidssoker_DP_`i'01= 1 if arbeidssoker_DP_`i'01 > 1
replace arbeidssoker_DP_`i'02= 1 if arbeidssoker_DP_`i'02 > 1
replace arbeidssoker_DP_`i'03= 1 if arbeidssoker_DP_`i'03 > 1
replace arbeidssoker_DP_`i'04= 1 if arbeidssoker_DP_`i'04 > 1
replace arbeidssoker_DP_`i'05= 1 if arbeidssoker_DP_`i'05 > 1
replace arbeidssoker_DP_`i'06= 1 if arbeidssoker_DP_`i'06 > 1
replace arbeidssoker_DP_`i'07= 1 if arbeidssoker_DP_`i'07 > 1
replace arbeidssoker_DP_`i'08= 1 if arbeidssoker_DP_`i'08 > 1
replace arbeidssoker_DP_`i'09= 1 if arbeidssoker_DP_`i'09 > 1
replace arbeidssoker_DP_`i'10= 1 if arbeidssoker_DP_`i'10 > 1
replace arbeidssoker_DP_`i'11= 1 if arbeidssoker_DP_`i'11 > 1
replace arbeidssoker_DP_`i'12= 1 if arbeidssoker_DP_`i'12 > 1

}

drop asokkode-avgang

drop arbeidssoker_DP_1992_01-arbeidssoker_DP_2001_12

sort w20_0450_lopenr_person
quietly by w20_0450_lopenr_person :  gen dup = cond(_N==1,0,_n) //lager duplikat-variabel
tab dup

drop if dup>1 //beholder kun først observasjon	 

drop dup	 


save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\arbeidsoker_trygd_2001", replace

****************************************************************************************************
****************************************************************************************************
****************************************************************************************************



use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd\w20_0450_f_arbsok_2001", replace 

save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\arbeidsoker_2001_tmp.dta", replace

use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\arbeidsoker_2001_tmp.dta", replace


***Beholder de IKKE dagpenger***

keep if (stonad=="ID") 


***Beholder de helt ledige***

gen helt_ledig= substr(arbstat,1,2)

keep if helt_ledig=="HL"

tostring asoktilg_mnd, replace
tostring asoktom_mnd, replace 

gen tilgang= substr(asoktilg_mnd,1,6)
gen avgang= substr(asoktom_mnd,1,6)

destring tilgang, replace
destring avgang, replace



//Lager månedsobservasjoner innen år for arbeidssøker

forvalues i=1992/2001 { 
gen arbeidssoker_ID_`i'_01 = 0 
replace arbeidssoker_ID_`i'_01 = 1 if tilgang<=`i'01 & (avgang>=`i'01 | avgang== .)  
gen arbeidssoker_ID_`i'_02 = 0 
replace arbeidssoker_ID_`i'_02 = 1 if tilgang<=`i'02 & (avgang>=`i'02 | avgang== .)
gen arbeidssoker_ID_`i'_03 = 0 
replace arbeidssoker_ID_`i'_03 = 1 if tilgang<=`i'03 & (avgang>=`i'03 | avgang== .)
gen arbeidssoker_ID_`i'_04 = 0 
replace arbeidssoker_ID_`i'_04 = 1 if tilgang<=`i'04 & (avgang>=`i'04 | avgang== .)
gen arbeidssoker_ID_`i'_05 = 0 
replace arbeidssoker_ID_`i'_05 = 1 if tilgang<=`i'05 & (avgang>=`i'05 | avgang== .)
gen arbeidssoker_ID_`i'_06 = 0 
replace arbeidssoker_ID_`i'_06 = 1 if tilgang<=`i'06 & (avgang>=`i'06 | avgang== .)
gen arbeidssoker_ID_`i'_07 = 0 
replace arbeidssoker_ID_`i'_07 = 1 if tilgang<=`i'07 & (avgang>=`i'07 | avgang== .)
gen arbeidssoker_ID_`i'_08 = 0 
replace arbeidssoker_ID_`i'_08 = 1 if tilgang<=`i'08 & (avgang>=`i'08 | avgang== .)
gen arbeidssoker_ID_`i'_09 = 0 
replace arbeidssoker_ID_`i'_09 = 1 if tilgang<=`i'09 & (avgang>=`i'09 | avgang== .)
gen arbeidssoker_ID_`i'_10 = 0 
replace arbeidssoker_ID_`i'_10 = 1 if tilgang<=`i'10 & (avgang>=`i'10 | avgang== .)
gen arbeidssoker_ID_`i'_11 = 0 
replace arbeidssoker_ID_`i'_11 = 1 if tilgang<=`i'11 & (avgang>=`i'11 | avgang== .)
gen arbeidssoker_ID_`i'_12 = 0 
replace arbeidssoker_ID_`i'_12 = 1 if tilgang<=`i'12 & (avgang>=`i'12 | avgang== .)

}

save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\arbeidsoker_2001_tmp.dta", replace


use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\arbeidsoker_2001_tmp.dta", replace

//Finner alle månedsregisteringer innen person og lager nye variabler

forvalues i=1992/2001 {
egen arbeidssoker_ID_`i'01= total (arbeidssoker_ID_`i'_01), by (w20_0450_lopenr_person)
egen arbeidssoker_ID_`i'02= total (arbeidssoker_ID_`i'_02), by (w20_0450_lopenr_person)
egen arbeidssoker_ID_`i'03= total (arbeidssoker_ID_`i'_03), by (w20_0450_lopenr_person)
egen arbeidssoker_ID_`i'04= total (arbeidssoker_ID_`i'_04), by (w20_0450_lopenr_person)
egen arbeidssoker_ID_`i'05= total (arbeidssoker_ID_`i'_05), by (w20_0450_lopenr_person)
egen arbeidssoker_ID_`i'06= total (arbeidssoker_ID_`i'_06), by (w20_0450_lopenr_person)
egen arbeidssoker_ID_`i'07= total (arbeidssoker_ID_`i'_07), by (w20_0450_lopenr_person)
egen arbeidssoker_ID_`i'08= total (arbeidssoker_ID_`i'_08), by (w20_0450_lopenr_person)
egen arbeidssoker_ID_`i'09= total (arbeidssoker_ID_`i'_09), by (w20_0450_lopenr_person)
egen arbeidssoker_ID_`i'10= total (arbeidssoker_ID_`i'_10), by (w20_0450_lopenr_person)
egen arbeidssoker_ID_`i'11= total (arbeidssoker_ID_`i'_11), by (w20_0450_lopenr_person)
egen arbeidssoker_ID_`i'12= total (arbeidssoker_ID_`i'_12), by (w20_0450_lopenr_person)

}

// Gir alle månedsregisteringer hvor en person er arbeidssoker verdien 1 

forvalues i=1992/2001 {
replace arbeidssoker_ID_`i'01= 1 if arbeidssoker_ID_`i'01 > 1
replace arbeidssoker_ID_`i'02= 1 if arbeidssoker_ID_`i'02 > 1
replace arbeidssoker_ID_`i'03= 1 if arbeidssoker_ID_`i'03 > 1
replace arbeidssoker_ID_`i'04= 1 if arbeidssoker_ID_`i'04 > 1
replace arbeidssoker_ID_`i'05= 1 if arbeidssoker_ID_`i'05 > 1
replace arbeidssoker_ID_`i'06= 1 if arbeidssoker_ID_`i'06 > 1
replace arbeidssoker_ID_`i'07= 1 if arbeidssoker_ID_`i'07 > 1
replace arbeidssoker_ID_`i'08= 1 if arbeidssoker_ID_`i'08 > 1
replace arbeidssoker_ID_`i'09= 1 if arbeidssoker_ID_`i'09 > 1
replace arbeidssoker_ID_`i'10= 1 if arbeidssoker_ID_`i'10 > 1
replace arbeidssoker_ID_`i'11= 1 if arbeidssoker_ID_`i'11 > 1
replace arbeidssoker_ID_`i'12= 1 if arbeidssoker_ID_`i'12 > 1

}

drop asokkode-avgang

drop arbeidssoker_ID_1992_01-arbeidssoker_ID_2001_12

sort w20_0450_lopenr_person
quietly by w20_0450_lopenr_person :  gen dup = cond(_N==1,0,_n) //lager duplikat-variabel
tab dup

drop if dup>1 //beholder kun først observasjon	 

drop dup	 

save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\arbeidsoker_uten_trygd_2001", replace

****************************************************************************************************
****************************************************************************************************
****************************************************************************************************

***Lager arbeidssøkerfil på månedsbasis fra 2001_05 til 2018

use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd\w20_0450_f_arbsok", replace 

save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\arbeidsoker_2018_tmp.dta", replace

use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\arbeidsoker_2018_tmp.dta", replace


***Beholder de MED dagpenger***

keep if (ytelse=="DP") 


***Beholder de helt ledige***

keep if hovedarbstat=="HL"

tostring asoktilg_mnd, replace
tostring asoktom_mnd, replace 

gen tilgang= substr(asoktilg_mnd,1,6)
gen avgang= substr(asoktom_mnd,1,6)

destring tilgang, replace
destring avgang, replace

//Lager månedsobservasjoner innen år for arbeidssøker


forvalues i=2001/2018 { 
gen arbeidssoker_DP_`i'_01 = 0 
replace arbeidssoker_DP_`i'_01 = 1 if tilgang<=`i'01 & (avgang>=`i'01 | avgang== .)  
gen arbeidssoker_DP_`i'_02 = 0 
replace arbeidssoker_DP_`i'_02 = 1 if tilgang<=`i'02 & (avgang>=`i'02 | avgang== .)
gen arbeidssoker_DP_`i'_03 = 0 
replace arbeidssoker_DP_`i'_03 = 1 if tilgang<=`i'03 & (avgang>=`i'03 | avgang== .)
gen arbeidssoker_DP_`i'_04 = 0 
replace arbeidssoker_DP_`i'_04 = 1 if tilgang<=`i'04 & (avgang>=`i'04 | avgang== .)
gen arbeidssoker_DP_`i'_05 = 0 
replace arbeidssoker_DP_`i'_05 = 1 if tilgang<=`i'05 & (avgang>=`i'05 | avgang== .)
gen arbeidssoker_DP_`i'_06 = 0 
replace arbeidssoker_DP_`i'_06 = 1 if tilgang<=`i'06 & (avgang>=`i'06 | avgang== .)
gen arbeidssoker_DP_`i'_07 = 0 
replace arbeidssoker_DP_`i'_07 = 1 if tilgang<=`i'07 & (avgang>=`i'07 | avgang== .)
gen arbeidssoker_DP_`i'_08 = 0 
replace arbeidssoker_DP_`i'_08 = 1 if tilgang<=`i'08 & (avgang>=`i'08 | avgang== .)
gen arbeidssoker_DP_`i'_09 = 0 
replace arbeidssoker_DP_`i'_09 = 1 if tilgang<=`i'09 & (avgang>=`i'09 | avgang== .)
gen arbeidssoker_DP_`i'_10 = 0 
replace arbeidssoker_DP_`i'_10 = 1 if tilgang<=`i'10 & (avgang>=`i'10 | avgang== .)
gen arbeidssoker_DP_`i'_11 = 0 
replace arbeidssoker_DP_`i'_11 = 1 if tilgang<=`i'11 & (avgang>=`i'11 | avgang== .)
gen arbeidssoker_DP_`i'_12 = 0 
replace arbeidssoker_DP_`i'_12 = 1 if tilgang<=`i'12 & (avgang>=`i'12 | avgang== .)

}

save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\arbeidsoker_2018_tmp.dta", replace


use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\arbeidsoker_2018_tmp.dta", replace

//Finner alle månedsregisteringer innen person og lager nye variabler

forvalues i=2001/2018 {
egen arbeidssoker_DP_`i'01= total (arbeidssoker_DP_`i'_01), by (w20_0450_lopenr_person)
egen arbeidssoker_DP_`i'02= total (arbeidssoker_DP_`i'_02), by (w20_0450_lopenr_person)
egen arbeidssoker_DP_`i'03= total (arbeidssoker_DP_`i'_03), by (w20_0450_lopenr_person)
egen arbeidssoker_DP_`i'04= total (arbeidssoker_DP_`i'_04), by (w20_0450_lopenr_person)
egen arbeidssoker_DP_`i'05= total (arbeidssoker_DP_`i'_05), by (w20_0450_lopenr_person)
egen arbeidssoker_DP_`i'06= total (arbeidssoker_DP_`i'_06), by (w20_0450_lopenr_person)
egen arbeidssoker_DP_`i'07= total (arbeidssoker_DP_`i'_07), by (w20_0450_lopenr_person)
egen arbeidssoker_DP_`i'08= total (arbeidssoker_DP_`i'_08), by (w20_0450_lopenr_person)
egen arbeidssoker_DP_`i'09= total (arbeidssoker_DP_`i'_09), by (w20_0450_lopenr_person)
egen arbeidssoker_DP_`i'10= total (arbeidssoker_DP_`i'_10), by (w20_0450_lopenr_person)
egen arbeidssoker_DP_`i'11= total (arbeidssoker_DP_`i'_11), by (w20_0450_lopenr_person)
egen arbeidssoker_DP_`i'12= total (arbeidssoker_DP_`i'_12), by (w20_0450_lopenr_person)

}

// Gir alle månedsregisteringer hvor en person er arbeidssoker verdien 1 

forvalues i=2001/2018 {
replace arbeidssoker_DP_`i'01= 1 if arbeidssoker_DP_`i'01 > 1
replace arbeidssoker_DP_`i'02= 1 if arbeidssoker_DP_`i'02 > 1
replace arbeidssoker_DP_`i'03= 1 if arbeidssoker_DP_`i'03 > 1
replace arbeidssoker_DP_`i'04= 1 if arbeidssoker_DP_`i'04 > 1
replace arbeidssoker_DP_`i'05= 1 if arbeidssoker_DP_`i'05 > 1
replace arbeidssoker_DP_`i'06= 1 if arbeidssoker_DP_`i'06 > 1
replace arbeidssoker_DP_`i'07= 1 if arbeidssoker_DP_`i'07 > 1
replace arbeidssoker_DP_`i'08= 1 if arbeidssoker_DP_`i'08 > 1
replace arbeidssoker_DP_`i'09= 1 if arbeidssoker_DP_`i'09 > 1
replace arbeidssoker_DP_`i'10= 1 if arbeidssoker_DP_`i'10 > 1
replace arbeidssoker_DP_`i'11= 1 if arbeidssoker_DP_`i'11 > 1
replace arbeidssoker_DP_`i'12= 1 if arbeidssoker_DP_`i'12 > 1

}

drop kode hoved hovedarbstat tiltak ytelse as_f as_gr regdato_mnd ///
     tilgdato_mnd avgdato_mnd asoktilg_mnd asokavg_mnd hovedarbdato_mnd /// 
	 tiltakdato_mnd ytelsedato_mnd dagdato_mnd henddato_mnd asoktom_mnd ///
	 tomdato_mnd tilgang avgang arbeidssoker_DP_2001_01-arbeidssoker_DP_2018_12 ///
     arbeidssoker_DP_200101-arbeidssoker_DP_200104

sort w20_0450_lopenr_person
quietly by w20_0450_lopenr_person :  gen dup = cond(_N==1,0,_n) //lager duplikat-variabel
tab dup

drop if dup>1 //beholder kun først observasjon	 

drop dup	 

sort w20_0450_lopenr_person
save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\arbeidsoker_trygd_2018.dta", replace

****************************************************************************************************
****************************************************************************************************
****************************************************************************************************

***Lager arbeidssøkerfil på månedsbasis fra 2001_05 til 2018

use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd\w20_0450_f_arbsok", replace 

save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\arbeidsoker_2018_tmp.dta", replace

use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\arbeidsoker_2018_tmp.dta", replace


***Beholder de UTEN dagpenger***

keep if (ytelse=="ID") 


***Beholder de helt ledige***

keep if hovedarbstat=="HL"

tostring asoktilg_mnd, replace
tostring asoktom_mnd, replace 

gen tilgang= substr(asoktilg_mnd,1,6)
gen avgang= substr(asoktom_mnd,1,6)

destring tilgang, replace
destring avgang, replace

//Lager månedsobservasjoner innen år for arbeidssøker


forvalues i=2001/2018 { 
gen arbeidssoker_ID_`i'_01 = 0 
replace arbeidssoker_ID_`i'_01 = 1 if tilgang<=`i'01 & (avgang>=`i'01 | avgang== .)  
gen arbeidssoker_ID_`i'_02 = 0 
replace arbeidssoker_ID_`i'_02 = 1 if tilgang<=`i'02 & (avgang>=`i'02 | avgang== .)
gen arbeidssoker_ID_`i'_03 = 0 
replace arbeidssoker_ID_`i'_03 = 1 if tilgang<=`i'03 & (avgang>=`i'03 | avgang== .)
gen arbeidssoker_ID_`i'_04 = 0 
replace arbeidssoker_ID_`i'_04 = 1 if tilgang<=`i'04 & (avgang>=`i'04 | avgang== .)
gen arbeidssoker_ID_`i'_05 = 0 
replace arbeidssoker_ID_`i'_05 = 1 if tilgang<=`i'05 & (avgang>=`i'05 | avgang== .)
gen arbeidssoker_ID_`i'_06 = 0 
replace arbeidssoker_ID_`i'_06 = 1 if tilgang<=`i'06 & (avgang>=`i'06 | avgang== .)
gen arbeidssoker_ID_`i'_07 = 0 
replace arbeidssoker_ID_`i'_07 = 1 if tilgang<=`i'07 & (avgang>=`i'07 | avgang== .)
gen arbeidssoker_ID_`i'_08 = 0 
replace arbeidssoker_ID_`i'_08 = 1 if tilgang<=`i'08 & (avgang>=`i'08 | avgang== .)
gen arbeidssoker_ID_`i'_09 = 0 
replace arbeidssoker_ID_`i'_09 = 1 if tilgang<=`i'09 & (avgang>=`i'09 | avgang== .)
gen arbeidssoker_ID_`i'_10 = 0 
replace arbeidssoker_ID_`i'_10 = 1 if tilgang<=`i'10 & (avgang>=`i'10 | avgang== .)
gen arbeidssoker_ID_`i'_11 = 0 
replace arbeidssoker_ID_`i'_11 = 1 if tilgang<=`i'11 & (avgang>=`i'11 | avgang== .)
gen arbeidssoker_ID_`i'_12 = 0 
replace arbeidssoker_ID_`i'_12 = 1 if tilgang<=`i'12 & (avgang>=`i'12 | avgang== .)

}

save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\arbeidsoker_2018_tmp.dta", replace


use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\arbeidsoker_2018_tmp.dta", replace

//Finner alle månedsregisteringer innen person og lager nye variabler

forvalues i=2001/2018 {
egen arbeidssoker_ID_`i'01= total (arbeidssoker_ID_`i'_01), by (w20_0450_lopenr_person)
egen arbeidssoker_ID_`i'02= total (arbeidssoker_ID_`i'_02), by (w20_0450_lopenr_person)
egen arbeidssoker_ID_`i'03= total (arbeidssoker_ID_`i'_03), by (w20_0450_lopenr_person)
egen arbeidssoker_ID_`i'04= total (arbeidssoker_ID_`i'_04), by (w20_0450_lopenr_person)
egen arbeidssoker_ID_`i'05= total (arbeidssoker_ID_`i'_05), by (w20_0450_lopenr_person)
egen arbeidssoker_ID_`i'06= total (arbeidssoker_ID_`i'_06), by (w20_0450_lopenr_person)
egen arbeidssoker_ID_`i'07= total (arbeidssoker_ID_`i'_07), by (w20_0450_lopenr_person)
egen arbeidssoker_ID_`i'08= total (arbeidssoker_ID_`i'_08), by (w20_0450_lopenr_person)
egen arbeidssoker_ID_`i'09= total (arbeidssoker_ID_`i'_09), by (w20_0450_lopenr_person)
egen arbeidssoker_ID_`i'10= total (arbeidssoker_ID_`i'_10), by (w20_0450_lopenr_person)
egen arbeidssoker_ID_`i'11= total (arbeidssoker_ID_`i'_11), by (w20_0450_lopenr_person)
egen arbeidssoker_ID_`i'12= total (arbeidssoker_ID_`i'_12), by (w20_0450_lopenr_person)

}

// Gir alle månedsregisteringer hvor en person er arbeidssoker verdien 1 

forvalues i=2001/2018 {
replace arbeidssoker_ID_`i'01= 1 if arbeidssoker_ID_`i'01 > 1
replace arbeidssoker_ID_`i'02= 1 if arbeidssoker_ID_`i'02 > 1
replace arbeidssoker_ID_`i'03= 1 if arbeidssoker_ID_`i'03 > 1
replace arbeidssoker_ID_`i'04= 1 if arbeidssoker_ID_`i'04 > 1
replace arbeidssoker_ID_`i'05= 1 if arbeidssoker_ID_`i'05 > 1
replace arbeidssoker_ID_`i'06= 1 if arbeidssoker_ID_`i'06 > 1
replace arbeidssoker_ID_`i'07= 1 if arbeidssoker_ID_`i'07 > 1
replace arbeidssoker_ID_`i'08= 1 if arbeidssoker_ID_`i'08 > 1
replace arbeidssoker_ID_`i'09= 1 if arbeidssoker_ID_`i'09 > 1
replace arbeidssoker_ID_`i'10= 1 if arbeidssoker_ID_`i'10 > 1
replace arbeidssoker_ID_`i'11= 1 if arbeidssoker_ID_`i'11 > 1
replace arbeidssoker_ID_`i'12= 1 if arbeidssoker_ID_`i'12 > 1

}

drop kode hoved hovedarbstat tiltak ytelse as_f as_gr regdato_mnd ///
     tilgdato_mnd avgdato_mnd asoktilg_mnd asokavg_mnd hovedarbdato_mnd /// 
	 tiltakdato_mnd ytelsedato_mnd dagdato_mnd henddato_mnd asoktom_mnd ///
	 tomdato_mnd tilgang avgang arbeidssoker_ID_2001_01-arbeidssoker_ID_2018_12 ///
     arbeidssoker_ID_200101-arbeidssoker_ID_200104

sort w20_0450_lopenr_person
quietly by w20_0450_lopenr_person :  gen dup = cond(_N==1,0,_n) //lager duplikat-variabel
tab dup

drop if dup>1 //beholder kun først observasjon	 

drop dup	 


save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\arbeidsoker_uten_trygd_2018.dta", replace



***Kobler filene***

use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\arbeidsoker_trygd_2001.dta", replace 

merge 1:1 w20_0450_lopenr_person using "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\arbeidsoker_trygd_2018.dta"

drop _merge


***Gir de som har missing verdien 0

forvalues i=1992/2018 {
replace arbeidssoker_DP_`i'01= 0 if arbeidssoker_DP_`i'01 == .
replace arbeidssoker_DP_`i'02= 0 if arbeidssoker_DP_`i'02 == . 
replace arbeidssoker_DP_`i'03= 0 if arbeidssoker_DP_`i'03 == . 
replace arbeidssoker_DP_`i'04= 0 if arbeidssoker_DP_`i'04 == . 
replace arbeidssoker_DP_`i'05= 0 if arbeidssoker_DP_`i'05 == . 
replace arbeidssoker_DP_`i'06= 0 if arbeidssoker_DP_`i'06 == . 
replace arbeidssoker_DP_`i'07= 0 if arbeidssoker_DP_`i'07 == . 
replace arbeidssoker_DP_`i'08= 0 if arbeidssoker_DP_`i'08 == . 
replace arbeidssoker_DP_`i'09= 0 if arbeidssoker_DP_`i'09 == . 
replace arbeidssoker_DP_`i'10= 0 if arbeidssoker_DP_`i'10 == .
replace arbeidssoker_DP_`i'11= 0 if arbeidssoker_DP_`i'11 == . 
replace arbeidssoker_DP_`i'12= 0 if arbeidssoker_DP_`i'12 == . 

}

*MERK DET ER BRUDD FRA 200105 til 200112 - alt for lave tall.


save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\arbeidssoker_DP_wide.dta", replace

*****************************************************************************************************

use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\arbeidsoker_uten_trygd_2001.dta", replace 

merge 1:1 w20_0450_lopenr_person using "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\arbeidsoker_uten_trygd_2018.dta"

drop _merge


***Gir de som har missing verdien 0

forvalues i=1992/2018 {
replace arbeidssoker_ID_`i'01= 0 if arbeidssoker_ID_`i'01 == .
replace arbeidssoker_ID_`i'02= 0 if arbeidssoker_ID_`i'02 == . 
replace arbeidssoker_ID_`i'03= 0 if arbeidssoker_ID_`i'03 == . 
replace arbeidssoker_ID_`i'04= 0 if arbeidssoker_ID_`i'04 == . 
replace arbeidssoker_ID_`i'05= 0 if arbeidssoker_ID_`i'05 == . 
replace arbeidssoker_ID_`i'06= 0 if arbeidssoker_ID_`i'06 == . 
replace arbeidssoker_ID_`i'07= 0 if arbeidssoker_ID_`i'07 == . 
replace arbeidssoker_ID_`i'08= 0 if arbeidssoker_ID_`i'08 == . 
replace arbeidssoker_ID_`i'09= 0 if arbeidssoker_ID_`i'09 == . 
replace arbeidssoker_ID_`i'10= 0 if arbeidssoker_ID_`i'10 == .
replace arbeidssoker_ID_`i'11= 0 if arbeidssoker_ID_`i'11 == . 
replace arbeidssoker_ID_`i'12= 0 if arbeidssoker_ID_`i'12 == . 

}

*MERK DET ER BRUDD FRA 200105 til 200112 - alt for lave tall.


save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\arbeidssoker_ID_wide.dta", replace
