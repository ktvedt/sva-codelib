****************************************************************************
*** Henter opp sykepenger og tilrettelegger ***

use "N:\durable\Grunnlagsdata\FD-trygd\w20_0450_f_sp.dta", replace 

save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\sykepenger_tmp", replace

use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\sykepenger_tmp.dta", replace

destring tilgdato_mnd, replace
destring avgdato_mnd, replace


keep if spkode== 10 | spkode== 20 | spkode== 30 | spkode== 40 | spkode== 50 | ///
        spkode== 60 | spkode== 90 


forvalues i=1992/2018 { 
gen sykepenger_`i'_01 = 0 
replace sykepenger_`i'_01 = 1 if tilgdato_mnd<=`i'01 & (avgdato_mnd>=`i'01 | avgdato_mnd== .)
gen sykepenger_`i'_02 = 0 
replace sykepenger_`i'_02 = 1 if tilgdato_mnd<=`i'02 & (avgdato_mnd>=`i'02 | avgdato_mnd== .)
gen sykepenger_`i'_03 = 0 
replace sykepenger_`i'_03 = 1 if tilgdato_mnd<=`i'03 & (avgdato_mnd>=`i'03 | avgdato_mnd== .)
gen sykepenger_`i'_04 = 0 
replace sykepenger_`i'_04 = 1 if tilgdato_mnd<=`i'04 & (avgdato_mnd>=`i'04 | avgdato_mnd== .)
gen sykepenger_`i'_05 = 0 
replace sykepenger_`i'_05 = 1 if tilgdato_mnd<=`i'05 & (avgdato_mnd>=`i'05 | avgdato_mnd== .)
gen sykepenger_`i'_06 = 0 
replace sykepenger_`i'_06 = 1 if tilgdato_mnd<=`i'06 & (avgdato_mnd>=`i'06 | avgdato_mnd== .)
gen sykepenger_`i'_07 = 0 
replace sykepenger_`i'_07 = 1 if tilgdato_mnd<=`i'07 & (avgdato_mnd>=`i'07 | avgdato_mnd== .)
gen sykepenger_`i'_08 = 0 
replace sykepenger_`i'_08 = 1 if tilgdato_mnd<=`i'08 & (avgdato_mnd>=`i'08 | avgdato_mnd== .)
gen sykepenger_`i'_09 = 0 
replace sykepenger_`i'_09 = 1 if tilgdato_mnd<=`i'09 & (avgdato_mnd>=`i'09 | avgdato_mnd== .)
gen sykepenger_`i'_10 = 0 
replace sykepenger_`i'_10 = 1 if tilgdato_mnd<=`i'10 & (avgdato_mnd>=`i'10 | avgdato_mnd== .)
gen sykepenger_`i'_11 = 0 
replace sykepenger_`i'_11 = 1 if tilgdato_mnd<=`i'11 & (avgdato_mnd>=`i'11 | avgdato_mnd== .)
gen sykepenger_`i'_12 = 0 
replace sykepenger_`i'_12 = 1 if tilgdato_mnd<=`i'12 & (avgdato_mnd>=`i'12 | avgdato_mnd== .)

}

save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\sykepenger_tmp.dta", replace

use "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\sykepenger_tmp.dta", replace


//Finner alle månedsregisteringer innen person og lager nye variabler

forvalues i=1992/2018 {
egen sykepenger_`i'01= total (sykepenger_`i'_01), by (w20_0450_lopenr_person)
egen sykepenger_`i'02= total (sykepenger_`i'_02), by (w20_0450_lopenr_person)
egen sykepenger_`i'03= total (sykepenger_`i'_03), by (w20_0450_lopenr_person)
egen sykepenger_`i'04= total (sykepenger_`i'_04), by (w20_0450_lopenr_person)
egen sykepenger_`i'05= total (sykepenger_`i'_05), by (w20_0450_lopenr_person)
egen sykepenger_`i'06= total (sykepenger_`i'_06), by (w20_0450_lopenr_person)
egen sykepenger_`i'07= total (sykepenger_`i'_07), by (w20_0450_lopenr_person)
egen sykepenger_`i'08= total (sykepenger_`i'_08), by (w20_0450_lopenr_person)
egen sykepenger_`i'09= total (sykepenger_`i'_09), by (w20_0450_lopenr_person)
egen sykepenger_`i'10= total (sykepenger_`i'_10), by (w20_0450_lopenr_person)
egen sykepenger_`i'11= total (sykepenger_`i'_11), by (w20_0450_lopenr_person)
egen sykepenger_`i'12= total (sykepenger_`i'_12), by (w20_0450_lopenr_person)

}

//Tillegger observasjoner med samme forløpsdatoer verdien 1 

forvalues i=1992/2018 {
replace sykepenger_`i'01= 1 if sykepenger_`i'01 > 1
replace sykepenger_`i'02= 1 if sykepenger_`i'02 > 1 
replace sykepenger_`i'03= 1 if sykepenger_`i'03 > 1 
replace sykepenger_`i'04= 1 if sykepenger_`i'04 > 1
replace sykepenger_`i'05= 1 if sykepenger_`i'05 > 1 
replace sykepenger_`i'06= 1 if sykepenger_`i'06 > 1 
replace sykepenger_`i'07= 1 if sykepenger_`i'07 > 1 
replace sykepenger_`i'08= 1 if sykepenger_`i'08 > 1
replace sykepenger_`i'09= 1 if sykepenger_`i'09 > 1 
replace sykepenger_`i'10= 1 if sykepenger_`i'10 > 1 
replace sykepenger_`i'11= 1 if sykepenger_`i'11 > 1 
replace sykepenger_`i'12= 1 if sykepenger_`i'12 > 1

}


drop sykepenger_1992_01-sykepenger_2018_12 //fjerner de opprinnelige variablene

drop spkode spgrad erdag avdag form5 ttype1 ttype2 ttype3 regdato_mnd tilgdato_mnd avgdato_mnd ///
     sptilg_mnd spavg_mnd arbuf_mnd spgrdato_mnd tomdato_mnd frm5dato_mnd


sort w20_0450_lopenr_person
quietly by w20_0450_lopenr_person :  gen dup = cond(_N==1,0,_n) //lager duplikat-variabel

drop if dup>1 //beholder kun først observasjon

drop dup

save "N:\durable\Prosjekt 10\Grunnlagsdata\FD-trygd mnd statuser\sykepenger_wide.dta", replace //lagrer filen i wide-format


