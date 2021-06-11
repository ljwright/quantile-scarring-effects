// Cell 1
clear
set more off
cd "F:/Next Steps 1-8 - 2021-01-14/"

global stata_fld  stata/stata13/eul
global dta_fld    Projects/Quantile Regression Paper/Data
global act_fld    Projects/Activity Histories/Data

capture program drop prog_total
program define prog_total
	args newvar vlist
	
	egen `newvar' = rowtotal(`vlist')
	egen missing = rowmiss(`vlist')
	replace `newvar' = . if missing > 0
	drop missing
end

// Cell 2
use NSID W8DHANVQH using "${stata_fld}/ns8_2015_derived.dta", clear 
merge 1:1 NSID using "${stata_fld}/ns8_2015_main_interview.dta", ///
	nogen keepusing(W8FINWT W8CMSEX W8DACTIVITY) 
merge 1:1 NSID using "${stata_fld}/ns8_2015_self_completion.dta", ///
	nogen keepusing(W8GHQ12_*) 
merge 1:1 NSID using "${stata_fld}/wave_five_lsype_young_person_2020.dta", ///
	nogen keepusing(W5SexYP) 
merge 1:1 NSID using "${stata_fld}/wave_four_lsype_young_person_2020.dta", ///
	nogen keepusing(W4ActivYP W4ConcenYP W4DecideYP W4DepressYP ///
	W4DifficYP W4HappyYP W4Hea1CYP W4NoConfYP W4NoSleepYP W4ProbsYP ///
	W4SexYP W4StrainYP W4UsefulYP W4WthlessYP W4ethgrpYP) 
merge 1:1 NSID using "${stata_fld}/wave_three_lsype_young_person_2020.dta", ///
	nogen keepusing(W3*bulrc W3sexYP) 
merge 1:1 NSID using "${stata_fld}/wave_two_lsype_family_background_2020.dta", ///
	nogen keepusing(IMDRSCORE) 
merge 1:1 NSID using "${stata_fld}/wave_two_lsype_young_person_2020.dta", ///
	nogen keepusing(W2Fat1YP W2Fat2YP W2Fat4YP W2Fat5YP W2Fat7YP W2Fat8YP ///
	W2FinWt W2SexYP W2bulrc W2disabYP W2ethgrpYP ///
	W2hea1cYP W2pbulrc W2yschat1 W2concenYP-W2happyYP) 
merge 1:1 NSID using "${stata_fld}/wave_one_lsype_family_background_2020.dta", ///
	nogen keepusing(W1hiqualgMP W1hiqualgSP W1nssecfam ///
	W1depkids W1managhhMP W1famtyp2) 
merge 1:1 NSID using "${stata_fld}/wave_one_lsype_young_person_2020.dta", ///
	nogen keepusing(W1*bulrc W1disabYP W1ethgrpYP W1risk W1sexYP)



*  2. CLEAN DATA
* Define Labels
label copy W1nssecfam NSSEC8
label define NSSEC8 -999 "" -99 "" -94 "" -92 "" -91 "", modify
label define Binary 0 "No" 1 "Yes"
label define Tenure 1 "Own House" 2 "Mortgage" 3 "Rent Council" 4 "Rent Private"
label define Disabled 1 "No" 2 "Yes, school not affected" 3 "Yes, school affected"
label define Ethnicity 1 "White" 2 "Mixed" 3 "Indian" 4 "Pakistani" 5 "Bangladeshi" /*
	*/ 6 "Black African" 7 "Black Caribbean" 8 "Other"
label define Education 1 "NVQ 5" 2 "NVQ 4" 3 "NVQ 3" 4 "NVQ 2" 5 "NVQ 1" 6 "No/Other Qual"
label define GenHealth 1 "Very Good" 2 "Fairly Good" 3 "Not Very Good" 4 "Not Good at All"
label define NSSEC3 1 "Higher" 2 "Intermediate" 3 "Routine" 4 "LTU"
label define LOC_Cat 1 "Neither" 2 "External" 3 "Internal"
label define Quintiles 1 "20th" 2 "40th" 3 "60th" 4 "80th" 5 "100th"
label define Bullied_Wave 0 "0" 1 "1" 2 "2" 3 "3"
label define Status_W8 1 "Employed" 2 "Education" 3 "Inactive" 4 "Unemployed"
label define Female 0 "Male" 1 "Female"

* Waves 1-8: Fixed Characteristics
egen Female=rowmax(W2SexYP W3sexYP W1sexYP W4SexYP W5SexYP)
replace Female=W8CMSEX if !inlist(Female,1,2)
replace Female=cond(Female>0,Female - 1,.)
egen Ethnicity=rowmax(W2ethgrpYP W1ethgrpYP W4ethgrpYP)
replace Ethnicity=cond(Ethnicity>0,Ethnicity,1)

label values Female Female
label values Ethnicity Ethnicity
drop *sexYP *SexYP *ethgrpYP W8CMSEX


* Wave 1 & 2: Common Time-Varying Variables
tab1 *disabYP
forval i=1/2{
	gen Disabled_W`i'=4-W`i'disabYP if inrange(W`i'disabYP,1,3)
	}
gen Disabled=max(Disabled_W1, Disabled_W2)
drop Disabled_W?
label values Disabled* Disabled
drop W*disabYP 

* Wave 2, 4 & 8: GHQ-12 & General Health
gen Survey_Weight_W2 = W2FinWt  if W2FinWt>0
gen Survey_Weight_W8 = W8FINWT  if W8FINWT>0

gen GenHealth_W2=W2hea1cYP-2 if inrange(W2hea1cYP,3,6)
gen GenHealth_W4=W4Hea1CYP if inrange(W4Hea1CYP,1,4)
label values GenHealth* GenHealth
drop W2hea1cYP W4Hea1CYP

local ghq_positive 1 3 4 7 8 12
local ghq_negative 2 5 6 9 10 11

local w2ghq		W2concenYP W2nosleepYP W2usefulYP W2decideYP ///
				W2strainYP W2difficYP W2activYP W2probsYP ///
				W2depressYP W2noconfYP W2wthlessYP W2happyYP
local w4ghq 	W4ConcenYP W4NoSleepYP W4UsefulYP W4DecideYP ///
				W4StrainYP W4DifficYP W4ActivYP W4ProbsYP ///
				W4DepressYP W4NoConfYP W4WthlessYP W4HappyYP
local w8ghq		W8GHQ12_1 W8GHQ12_2 W8GHQ12_3 W8GHQ12_4 ///
				W8GHQ12_5 W8GHQ12_6 W8GHQ12_7 W8GHQ12_8 ///
				W8GHQ12_9 W8GHQ12_10 W8GHQ12_11 W8GHQ12_12

foreach w in 2 4 8{
	local i = 0
	foreach var of local w`w'ghq{
		local i = `i' + 1
		gen W`w'_GHQ_Likert_`i' = `var' - 1 if inrange(`var', 1, 4)
		gen W`w'_GHQ_Caseness_`i' = inrange(`var', 3, 4) if inrange(`var', 1, 4)
		if `w'==2	replace W`w'_GHQ_Caseness_`i' = 0 if `var'==-1
	}
	
	foreach type in Likert Caseness{
		prog_total GHQ_W`w'_`type' W`w'_GHQ_`type'*
	}
}


* Waves 1, 2 and 3: Bullying
forval i=1/3{
	gen Bullied_W`i' = (W`i'bulrc==1 | W`i'pbulrc==1) if !missing(W`i'bulrc,W`i'pbulrc)
	replace Bullied_W`i' = . if  W`i'bulrc<0 & W`i'pbulrc<0
	}
gen Bullied_Waves=Bullied_W1+Bullied_W2+Bullied_W3
// label define Bullied_Waves 0 "0" 1 "1" 2 "2" 3 "3"
// label values Bullied_Waves Bullied_Waves


* Wave 1
gen Risk_W1 = W1risk if inrange(W1risk,0,8)

gen NSSEC8_W1 = W1nssecfam if inrange(W1nssecfam,1,8)
gen NSSEC3_W1 = 1 if inrange(NSSEC8_W1,1,2)
replace NSSEC3_W1 = 2 if inrange(NSSEC8_W1,3,4)
replace NSSEC3_W1 = 3 if inrange(NSSEC8_W1,5,7)
replace NSSEC3_W1 = 4 if inlist(NSSEC8_W1,8)
label values NSSEC3_W? NSSEC3

gen ParentEduc5_W1 = W1hiqualgMP ///
    if inrange(W1hiqualgMP, 1, 7) & W1hiqualgSP == -98
replace ParentEduc5_W1 = min(W1hiqualgMP, W1hiqualgSP) ///
    if inrange(W1hiqualgMP, 1, 7) & inrange(W1hiqualgSP, 1, 7)
replace ParentEduc5_W1 = 5 if inrange(ParentEduc5_W1, 5, 7)
label define ParentEduc5_W1 1 "Degree" 2 "Other HE" 3 "A-Level" 4 "GCSE A-C" 5 "Other/None"
label values ParentEduc5_W1 ParentEduc5_W1

gen HHType_W1 = W1famtyp2 if inrange(W1famtyp2, 0, 1)
label define HHType_W1 0 "Two Parent" 1 "Single Parent"
label values HHType_W1 HHType_W1

gen FinDiff_W1 = W1managhhMP if inrange(W1managhhMP, 1, 3)
label define FinDiff_W1 1 "Managing Well" 2 "Getting By" 3 "Having Difficulties"
label values FinDiff_W1 FinDiff_W1

gen HHKids_W1 = W1depkids if W1depkids > 0
recode HHKids_W1 (5/max = 5)
label define HHKids_W1 1 "1" 2 "2" 3 "3" 4 "4" 5 "5+"
label values HHKids_W1 HHKids_W1

* Wave 2
gen IMD_W2=IMDRSCORE if IMDRSCORE>=0
xtile IMD_W2_Quintile=IMD_W2 [pweight=Survey_Weight_W2], n(5)
label values IMD_W2_Quintile Quintiles
drop IMDRSCORE

gen SchoolAtt_W2 = W2yschat1 if inrange(W2yschat1,0,48)	

local Int_LOC W2Fat1YP W2Fat5YP W2Fat8YP
local Ext_LOC W2Fat2YP W2Fat4YP W2Fat7YP
foreach type in Int Ext{
	local i = 0
	foreach var of local `type'_LOC{
		local i = `i' + 1
		if "`type'"=="Int"{
			gen Int_LOC_W2_Item`i' = 5 - `var' if inrange(`var', 1, 2)
			replace Int_LOC_W2_Item`i' = 2 if `var'==-1
			replace Int_LOC_W2_Item`i' = 4 - `var' if inrange(`var', 3, 4)
		}	
		else{
			gen Ext_LOC_W2_Item`i' = `var' if inrange(`var', 3, 4)
			replace Ext_LOC_W2_Item`i' = 2 if `var'==-1
			replace Ext_LOC_W2_Item`i' = `var' - 1 if inrange(`var', 1, 2)
		} 		
	}
}
drop `Int_LOC' `Ext_LOC'
label variable Int_LOC_W2_Item1 "Q1. If someone is not a success in life, it is usually their own fault"
label variable Int_LOC_W2_Item2 "Q4. I can pretty much decide what will happen in my life"
label variable Int_LOC_W2_Item3 "Q6. If you work hard at something, you'll usually succeed"
label variable Ext_LOC_W2_Item1 "Q2. Even if I do well in school, I'll have a hard time"
label variable Ext_LOC_W2_Item2 "Q3. People like me don't have much of a chance in life"
label variable Ext_LOC_W2_Item3 "Q5. How well you get on in this world is mostly a matter of luck"

* Wave 8
gen Education_W8=6-W8DHANVQH if inrange(W8DHANVQH,1,5)
replace Education_W8=6 if inlist(W8DHANVQH,95,96)
label values Education* Education

gen Status_W8 = 1 if inrange(W8DACTIVITY, 1, 4) | inlist(W8DACTIVITY, 8, 11, 12)
replace Status_W8 = 2 if inrange(W8DACTIVITY, 5, 6)
replace Status_W8 = 3 if inlist(W8DACTIVITY, 9, 10, 13, 14)
replace Status_W8 = 4 if W8DACTIVITY==5
label values Status_W8 Status_W8

	
* Format
keep 	NSID Survey_Weight_W8 Survey_Weight_W2 Status_W8 ///
		GHQ_W8_Likert GHQ_W2_Caseness GHQ_W4_Likert ///
		GenHealth_W2 GenHealth_W4 Disabled ///
		Female IMD_W2 NSSEC3_W1 *LOC*Item* ///
		Ethnicity ParentEduc5_W1 Education_W8 ///
		Risk_W1 SchoolAtt_W2 Bullied_Waves ///
		HHType_W1 FinDiff_W1 HHKids_W1		
drop if missing(Survey_Weight_W8)
ds *, alpha
order NSID `r(varlist)'
sort NSID
compress
save "${dta_fld}/Dataset", replace

// Cell 4
capture program drop prog_cleanact
program define prog_cleanact
	args lb ub
	
	replace End_MY = floor(End_MY)
	replace Start_MY = floor(Start_MY)
	replace Start_MY = `lb' if Start_MY<`lb'
	drop if Start_MY > End_MY

	by NSID (Spell), sort: replace Spell = _n
	expand 2 if Spell==1, gen(expand)
	replace Spell = 0 if expand==1
	replace Activity = .m if expand==1
	replace End_MY = Start_MY if expand==1
	replace Start_MY = `lb' if expand==1
	drop if End_MY == Start_MY
	drop expand
	by NSID (Spell), sort: replace Spell = _n

	by NSID (Spell), sort: gen gap = Start_MY[_n+1]-End_MY if _n<_N
	by NSID (Spell), sort: replace gap = `ub'-End_MY ///
		if End_MY<`ub' & _n==_N
		
	expand 2 if gap > 0 & !missing(gap), gen(expand)
	by NSID (Spell expand), sort: replace Spell = _n
	replace Start_MY = End_MY if expand==1
	replace End_MY = Start_MY + gap if expand==1
	replace Activity = .m if expand==1
	drop expand gap

	by NSID (Spell), sort: gen new_status = Activity!=Activity[_n-1]
	by NSID (Spell), sort: gen status_spell = sum(new_status)
	by NSID status_spell (Spell), sort: replace End_MY = End_MY[_N]
	by NSID status_spell (Spell), sort: keep if _n==1
	by NSID (status_spell), sort: replace Spell = _n
	drop new_status status_spell	
	
end

// Cell 7
use NSID Spell Activity Start_MY End_MY using "${act_fld}/Activity Histories", clear
prog_cleanact "ym(2008, 10)" "ym(2010, 5)"

gen lb = max(ym(2008, 10), Start_MY)
gen ub = min(ym(2010, 5), End_MY)
replace lb = . if lb>ub
replace ub = . if missing(lb)
format lb ub %tm

gen duration = ub - lb if Activity==3
by NSID (Spell), sort: egen max_duration = max(duration)
by NSID (Spell), sort: gen missing = Activity==.m & !missing(lb, ub)
by NSID (Spell), sort: egen has_missing = max(missing)

foreach m in 3 6 9 12{
	gen Unem_`m'Months = max_duration >= `m' & !missing(max_duration)
	replace Unem_`m'Months = . if Unem_`m'Months == 0 & has_missing == 1
	
	label define Unem_`m'Months 0 "<`m' Months Unemployment" 1 "`m'+ Months Unemployment"
	label values Unem_`m'Months Unem_`m'Months 
}
drop lb-has_missing

keep NSID Unem*
duplicates drop
merge 1:1 NSID using "${dta_fld}/Dataset", keep(match using) nogen
compress

save "${dta_fld}/Dataset", replace

