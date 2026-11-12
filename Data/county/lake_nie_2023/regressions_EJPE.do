clear all


********************************************************************************
****************************** A. Setup ****************************************
********************************************************************************

***********************
*** A.1 Basic Setup ***
***********************
qui {
glo path "C:\Dropbox\covid\replicationEJPE"

cd "$path\estimation"

cap log using "tradeWarCovid_v0.9_EJPE.log", replace

loc packages "reghdfe ftools estout ivreg2 ivreghdfe ranktest"
foreach p in `packages' {
ssc install `p'
}

use "$path\data\estimationDataset.dta", replace

**** Voting variables
loc vote "d_pres2020RepShare_2party"
loc voteLag "d_pres2016RepShare_2party"

**** Health insurance and trade war vars
loc healthInsuranceVars "d_healthInsurance healthInsuranceShare"
loc tradeWarChn "chn_TS3_R chn_TS3_US"
loc tradeWarEarly "TS6_US_old TS6_Retaliation_old"
loc tradeWarVars "TS3_US TS3_R agSubs_mfp_pw"

**** Demo controls
loc controlsDemo1 ""
loc d_controlsDemo1 ""

foreach g in 2024 2544 4564 6574 75up {
loc controlsDemo1 "`controlsDemo1' Age`g'share2016"
loc d_controlsDemo1 "`d_controlsDemo1' d_Age`g'share"
}

loc controlsDemo2 ""
loc d_controlsDemo2 ""

foreach v in FemaleShare rachispan racasian racblk racwht racother languageHomeNonEnglishSh foreignBornShare2 naturalizedCitizensShare {
loc controlsDemo2 "`controlsDemo2' `v'"
loc d_controlsDemo2 "`d_controlsDemo2' d_`v'"

loc controlsDemo "`controlsDemo1' `controlsDemo2' `d_controlsDemo1' `d_controlsDemo2'"


**** Socioecon controls
loc controlsSocio1 ""
loc d_controlsSocio1 ""

foreach k in 25k_50k 50k_75k 75k_99k 100k_150k 150k_200k 200kPlus {
loc controlsSocio1 "`controlsSocio1' hhY_`k'Sh2016"
loc d_controlsSocio1 "`d_controlsSocio1' d_hhY_`k'Sh"
}

loc controlsSocio2 ""
loc d_controlsSocio2 ""

foreach v in educ_HSgrad educ_someColl educ_collGrad povertyPeopleShare medianHouseholdIncomeReal {
loc controlsSocio2 "`controlsSocio2' `v'"
loc d_controlsSocio2 "`d_controlsSocio2' d_`v'"
}

loc controlsSocio "`controlsSocio1' `controlsSocio2' sk14 `d_controlsSocio1' `d_controlsSocio2'"

**** Econ controls
loc controlsEcon1 ""
loc d_controlsEcon1 ""

foreach v in empManufacturingShare empAgMiningShare unemployedShare notInLaborForceShare Population housingUnitsMultiShare publicTransportWorkSh {
loc controlsEcon1 "`controlsEcon1' `v'"
loc d_controlsEcon1 "`d_controlsEcon1' d_`v'"
}
loc controlsEcon "`controlsEcon1' metroSize_large metroSize_small effdens `d_controlsEcon1'"

loc controlsNonCOVID "`controlsDemo' `controlsSocio' `controlsEcon'"


**** Health controls
loc healthVars "diab_hemotest_10 diab_eyeexam_10 diab_lipids_10 mort_30day_hosp_z adjmortmeas_chfall30day adjmortmeas_pnall30day empShareTele"

		
**** Covid vars: covid prevalence (cases/deaths), social distancing, foot traffic, testing
loc covidVars "death* case* mei* visit* d_UR_oct"
loc mobActControls_cum "mei_ave visits_relative_cumulative d_UR_oct"
loc mobActControls_deathsOct "mei_oct_ave visits_change_relative_oct d_UR_oct "
loc mobActControls_casesOct "mei_oct_ave visits_change_relative_oct d_UR_oct"
loc mobActControls_deathsPeak "mei_14dayAve_deaths_adj_max visits_rel_deaths14_adj d_UR_oct"
loc mobActControls_casesPeak "mei_14dayAve_cases_adj_max visits_rel_cases14_adj d_UR_oct"


**** Potential IVs
loc nursingVars "restot2016"
loc ivs "`nursingVars' meatShare"


drop if missing(votesPres2020RepShare_2p)
	* Kalawao county HI 15005 is missing. This county doesn't report county-level voting data

}

}
	
	
	
*************************
*** A.2 Summary Stats ***
*************************
qui {
noi di ""
noi di "*******************************"
noi di "*** Table A.1 Summary Stats ***"
noi di "*******************************"
noi di ""

noi sum `vote' `voteLag' `tradeWarVars' `healthInsuranceVars' `controlsDemo' `controlsSocio' `controlsEcon' `covidVars' educ_HSdropout hhY_0k_25kSh2016 AgeU20share2016 d_educ_HSdropout d_hhY_0k_25kSh d_AgeU20share 

}



********************************************************************************
*********************** B. Baseline analysis ***********************************
********************************************************************************
qui {
noi di "m1: voteLag and US tariffs"
noi di "m2: add retaliation tariffs"
noi di "m3: add ag subsidies"
noi di "m4: add controls"
noi di "m5: add state FEs"
noi di "m6: add COVID vars"
noi di "m7: add HI vars"
noi di "m8: drop voteLag"

* column 1
loc m=1	
reghdfe `vote' `voteLag' TS3_US [w=votesPres2020Total], noabsorb cluster(fipstate)
eststo m`m'

* column 2
loc m=`m'+1
reghdfe `vote' `voteLag' TS3_US TS3_R [w=votesPres2020Total], noabsorb cluster(fipstate)
eststo m`m'

* column 3
loc m=`m'+1
reghdfe `vote' `voteLag' `tradeWarVars' [w=votesPres2020Total], noabsorb cluster(fipstate)
eststo m`m'

* column 4
loc m=`m'+1
reghdfe `vote' `voteLag' `tradeWarVars' `controlsNonCOVID' [w=votesPres2020Total], noabsorb cluster(fipstate)
eststo m`m'

* column 5
loc m=`m'+1
reghdfe `vote' `voteLag' `tradeWarVars' `controlsNonCOVID' [w=votesPres2020Total], absorb(fipstate) cluster(fipstate)
eststo m`m'

* column 6
loc m=`m'+1
reghdfe `vote' `voteLag' `tradeWarVars' `controlsNonCOVID' deaths_ave `healthVars' `mobActControls_cum' [w=votesPres2020Total], absorb(fipstate) cluster(fipstate)
eststo m`m'

* column 7
loc m=`m'+1
reghdfe `vote' `voteLag' `tradeWarVars' `healthInsuranceVars' `controlsNonCOVID' deaths_ave `healthVars' `mobActControls_cum' [w=votesPres2020Total], absorb(fipstate) cluster(fipstate)
eststo m`m'

	* store baseline coefficient estimates for counterfactual analysis
gen bCOVID=_b[deaths_ave]
gen bHI=_b[d_healthInsurance]
gen bTS_US=_b[TS3_US]
gen bTS_R=_b[TS3_R]
gen bAgSubs=_b[agSubs_mfp_pw]


* column 8
loc m=`m'+1
reghdfe `vote'           `tradeWarVars' `healthInsuranceVars' `controlsNonCOVID' deaths_ave `healthVars' `mobActControls_cum' [w=votesPres2020Total], absorb(fipstate) cluster(fipstate)



noi di ""
noi di "***************"
noi di "*** Table 3 ***"
noi di "***************"
noi di ""
noi di "m1: voteLag and US tariffs"
noi di "m2: add retaliation tariffs"
noi di "m3: add ag subsidies"
noi di "m4: add controls"
noi di "m5: add state FEs"
noi di "m6: add COVID vars"
noi di "m7: add HI vars"

noi estout m1 m2 m3 m4 m5 m6 m7, k(`tradeWarVars' d_healthInsurance deaths_ave `voteLag') cells(b(star fmt(3)) se(par fmt(3))) stats(N r2, fmt(%9.0g 3 3 3)) style(fixed) starlevels(# 0.10 ^ 0.05 * 0.01) 
noi estout m7, cells(b(star fmt(3)) se(par fmt(3))) stats(N r2, fmt(%9.0g 3 3 3)) style(fixed) starlevels(# 0.10 ^ 0.05 * 0.01) 
}


************************************************************************
************************** C. IV ***************************************
************************************************************************

qui {

loc m=0

* Setup: endog vars, Z vars, X vars
    
loc Evar1 "TS3_US"
loc Evar2 "TS3_R"
loc Evar3 "agSubs_mfp_pw"
loc Evar4 "d_healthInsurance"

loc Z1s "empManuf hhY_50k_75kSh2016"
loc Z2s "empAgMining d_publicTransportWorkSh"
loc Z3s "empAgMining mei_ave diab_eye"
loc Z4s "healthInsuranceShare diab_lipids_10 diab_hemotest_10 mei_ave visits_relative_cumulative"

loc Xvars "`controlsNonCOVID' `healthVars'  `mobActControls_cum' id_s* healthInsurance"

* Trade war vars
forval i=1/3 {
	di in red "`Evar`i''"
	reg `Evar`i'' `Xvars'
	foreach x in `Xvars' {
		estat hettest `x', iid
						}
	    estat hettest `Z`i's', iid
			}

forval i=1/3 {

	foreach z in `Z`i's' {

	su `z', meanonly
	cap g m`z'=`z'-r(mean)

	reg `Evar`i'' `Xvars'
	cap drop ei
	cap predict ei, res
	cap g z`i'`z'=m`z'*ei
	cap drop m`z'
						}
}


* Health insurance
loc i=4 
	di in red "`Evar`i''"
	reg `Evar`i'' `Xvars'
	foreach x in `Xvars' {
		estat hettest `x', iid
						}
	    estat hettest `Z`i's', iid

	foreach z in `Z`i's' {

	su `z', meanonly
	cap g m`z'=`z'-r(mean)

	reg `Evar`i'' `Xvars'
	cap drop ei
	cap predict ei, res
	cap g z`i'`z'=m`z'*ei
	cap drop m`z'
						}

	

ivreghdfe `vote' (TS3_US = z1*) TS3_R agSubs_mfp_pw deaths_ave `voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_cum' `healthInsuranceVars' [w=votesPres2020Total], gmm2s endog(TS3_US) absorb(fipstate) partial(`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_cum' ) cluster(fipstate)
loc m=`m'+1
eststo p`m'
eststo p`m', add(WeakIVs e(widstat))
eststo p`m', add(UnderidP e(idp))
eststo p`m', add(HansenP e(jp))
eststo p`m', add(SarganEndogP e(estatp))

ivreghdfe `vote' (TS3_R = z2*) TS3_US agSubs_mfp_pw deaths_ave `voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_cum' `healthInsuranceVars' [w=votesPres2020Total],  gmm2s endog(TS3_R) absorb(fipstate) partial(`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_cum' ) cluster(fipstate)
loc m=`m'+1
eststo p`m'
eststo p`m', add(WeakIVs e(widstat))
eststo p`m', add(UnderidP e(idp))
eststo p`m', add(HansenP e(jp))
eststo p`m', add(SarganEndogP e(estatp))

ivreghdfe `vote' (agSubs_mfp_pw = z3*) TS3_US TS3_R deaths_ave `voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_cum' `healthInsuranceVars' [w=votesPres2020Total],  gmm2s endog(agSubs_mfp_pw) absorb(fipstate) partial(`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_cum' ) cluster(fipstate)
loc m=`m'+1
eststo p`m'
eststo p`m', add(WeakIVs e(widstat))
eststo p`m', add(UnderidP e(idp))
eststo p`m', add(HansenP e(jp))
eststo p`m', add(SarganEndogP e(estatp))

ivreghdfe `vote' (`tradeWarVars' = z1* z2* z3*) deaths_ave `voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_cum' `healthInsuranceVars' [w=votesPres2020Total],  gmm2s endog(`tradeWarVars') absorb(fipstate) partial(`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_cum' ) cluster(fipstate)
loc m=`m'+1
eststo p`m'
eststo p`m', add(WeakIVs e(widstat))
eststo p`m', add(UnderidP e(idp))
eststo p`m', add(HansenP e(jp))
eststo p`m', add(SarganEndogP e(estatp))

ivreghdfe `vote' (d_healthInsurance =  z4*) healthInsurance deaths_ave `voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_cum' `tradeWarVars' [w=votesPres2020Total],  gmm2s endog(d_healthInsurance) absorb(fipstate) partial(`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_cum' ) cluster(fipstate)
loc m=`m'+1
eststo p`m'
eststo p`m', add(WeakIVs e(widstat))
eststo p`m', add(UnderidP e(idp))
eststo p`m', add(HansenP e(jp))
eststo p`m', add(SarganEndogP e(estatp))


ivreghdfe `vote' (`tradeWarVars' d_healthInsurance = z1* z2* z3* z4* ) healthInsurance deaths_ave `voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_cum' [w=votesPres2020Total],  gmm2s endog(`tradeWarVars' d_healthInsurance) absorb(fipstate) partial(`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_cum' ) cluster(fipstate)
loc m=`m'+1
eststo p`m'
eststo p`m', add(WeakIVs e(widstat))
eststo p`m', add(UnderidP e(idp))
eststo p`m', add(HansenP e(jp))
eststo p`m', add(SarganEndogP e(estatp))
gen bHI_L=_b[d_healthInsurance]
gen bTS_US_L=_b[TS3_US]
gen bTS_R_L=_b[TS3_R]
gen bAgSubs_L=_b[agSubs_mfp_pw]


noi di ""
noi di "*******************"
noi di "*** Table 5: IV ***"
noi di "*******************"
noi di ""

noi di "m7: Baseline specification"
noi di "p1: Lewbel instruments for US tariffs"
noi di "p2: Lewbel instruments for foreign tariff retaliation"
noi di "p3: Lewbel instruments for ag subsidies"
noi di "p5: Lewbel instruments for all trade war vars"
noi di "p5: Lewbel instruments for d_healthInsurance"
noi di "p6: Lewbel instruments for all trade war vars + d_healthInsurance"

noi estout m7 p1 p2 p3 p4 p5 p6, k(deaths_ave d_healthInsurance `tradeWarVars') cells(b(star fmt(3)) se(par fmt(3))) stats(N UnderidP WeakIVs HansenP SarganEndogP, fmt(%9.0g 3 3 3)) style(fixed) starlevels(# 0.10 ^ 0.05 * 0.01) 

}


********************************************************************************
************************** D. Robustness ***************************************
********************************************************************************

************************************************************
*** D.1 Alternative COVID prevalence measures & COVID IV ***
************************************************************
qui {

* Cumulative deaths
loc m=1	
reghdfe `vote' deaths_ave 						`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_cum' `healthInsuranceVars' `tradeWarVars' [w=votesPres2020Total], absorb(fipstate) cluster(fipstate)
eststo o`m'
ivreghdfe `vote' (deaths_ave = meatShare)		`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_cum' `healthInsuranceVars' `tradeWarVars' [w=votesPres2020Total], endog(deaths_ave) absorb(fipstate) partial(`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_cum' `covidControls') cluster(fipstate)
eststo vm`m'
eststo vm`m', add(WeakIVs e(widstat))
eststo vm`m', add(UnderidP e(idp))
eststo vm`m', add(SarganEndogP e(estatp))
ivreghdfe `vote' (deaths_ave = `nursingVars')	`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_cum' `healthInsuranceVars' `tradeWarVars' [w=votesPres2020Total], endog(deaths_ave) absorb(fipstate) partial(`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_cum' ) cluster(fipstate)
gen bCOVID_L=_b[deaths_ave]
eststo vn`m'
eststo vn`m', add(WeakIVs e(widstat))
eststo vn`m', add(UnderidP e(idp))
eststo vn`m', add(SarganEndogP e(estatp))


* Cumulative cases
loc m=`m'+1
reghdfe `vote' cases_ave 						`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_cum' `healthInsuranceVars' `tradeWarVars' [w=votesPres2020Total], absorb(fipstate) cluster(fipstate)
eststo o`m'
ivreghdfe `vote' (cases_ave = meatShare)		`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_cum' `healthInsuranceVars' `tradeWarVars' [w=votesPres2020Total], endog(cases_ave) absorb(fipstate) partial(`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_cum' ) cluster(fipstate)
eststo vm`m'
eststo vm`m', add(WeakIVs e(widstat))
eststo vm`m', add(UnderidP e(idp))
eststo vm`m', add(SarganEndogP e(estatp))
ivreghdfe `vote' (cases_ave = `nursingVars')	`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_cum' `healthInsuranceVars' `tradeWarVars' [w=votesPres2020Total], endog(cases_ave) absorb(fipstate) partial(`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_cum' ) cluster(fipstate)
eststo vn`m'
eststo vn`m', add(WeakIVs e(widstat))
eststo vn`m', add(UnderidP e(idp))
eststo vn`m', add(SarganEndogP e(estatp))


* October deaths
loc m=`m'+1
reghdfe `vote' deaths_oct_ave_adj							`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_deathsOct' `healthInsuranceVars' `tradeWarVars' [w=votesPres2020Total], absorb(fipstate) cluster(fipstate)
eststo o`m'
ivreghdfe `vote' (deaths_oct_ave_adj = meatShare)			`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_deathsOct' `healthInsuranceVars' `tradeWarVars' [w=votesPres2020Total], endog(deaths_oct_ave_adj) absorb(fipstate) partial(`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_deathsOct' ) cluster(fipstate)
eststo vm`m'
eststo vm`m', add(WeakIVs e(widstat))
eststo vm`m', add(UnderidP e(idp))
eststo vm`m', add(SarganEndogP e(estatp))
ivreghdfe `vote' (deaths_oct_ave_adj = `nursingVars')		`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_deathsOct' `healthInsuranceVars' `tradeWarVars' [w=votesPres2020Total], endog(deaths_oct_ave_adj)absorb(fipstate) partial(`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_deathsOct' ) cluster(fipstate)
eststo vn`m'
eststo vn`m', add(WeakIVs e(widstat))
eststo vn`m', add(UnderidP e(idp))
eststo vn`m', add(SarganEndogP e(estatp))


* October cases
loc m=`m'+1
reghdfe `vote' cases_oct_ave_adj							`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_casesOct' `healthInsuranceVars' `tradeWarVars' [w=votesPres2020Total], absorb(fipstate) cluster(fipstate)
eststo o`m'
ivreghdfe `vote' (cases_oct_ave_adj = meatShare)			`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_casesOct' `healthInsuranceVars' `tradeWarVars' [w=votesPres2020Total], endog(cases_oct_ave_adj)absorb(fipstate) partial(`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_casesOct' ) cluster(fipstate)
eststo vm`m'
eststo vm`m', add(WeakIVs e(widstat))
eststo vm`m', add(UnderidP e(idp))
eststo vm`m', add(SarganEndogP e(estatp))
ivreghdfe `vote' (cases_oct_ave_adj = `nursingVars')		`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_casesOct' `healthInsuranceVars' `tradeWarVars' [w=votesPres2020Total], endog(cases_oct_ave_adj)absorb(fipstate) partial(`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_casesOct' ) cluster(fipstate)
eststo vn`m'
eststo vn`m', add(WeakIVs e(widstat))
eststo vn`m', add(UnderidP e(idp))
eststo vn`m', add(SarganEndogP e(estatp))


* Peak deaths
loc m=`m'+1
reghdfe `vote' deaths_14dayAve_adj_max					 	`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_deathsPeak' `healthInsuranceVars' `tradeWarVars' [w=votesPres2020Total], absorb(fipstate) cluster(fipstate)
eststo o`m'
ivreghdfe `vote' (deaths_14dayAve_adj_max = meatShare)	 	`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_deathsPeak' `healthInsuranceVars' `tradeWarVars' [w=votesPres2020Total], endog(deaths_14dayAve_adj_max) absorb(fipstate) partial(`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_deathsPeak' ) cluster(fipstate)
eststo vm`m'
eststo vm`m', add(WeakIVs e(widstat))
eststo vm`m', add(UnderidP e(idp))
eststo vm`m', add(SarganEndogP e(estatp))
ivreghdfe `vote' (deaths_14dayAve_adj_max = `nursingVars') 	`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_deathsPeak' `healthInsuranceVars' `tradeWarVars' [w=votesPres2020Total], endog(deaths_14dayAve_adj_max) absorb(fipstate) partial(`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_deathsPeak' ) cluster(fipstate)
eststo vn`m'
eststo vn`m', add(WeakIVs e(widstat))
eststo vn`m', add(UnderidP e(idp))
eststo vn`m', add(SarganEndogP e(estatp))


* Peak cases
loc m=`m'+1
reghdfe `vote' cases_14dayAve_adj_max							`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_casesPeak' `healthInsuranceVars' `tradeWarVars' [w=votesPres2020Total], absorb(fipstate) cluster(fipstate)
eststo o`m'
ivreghdfe `vote' (cases_14dayAve_adj_max = meatShare)			`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_casesPeak' `healthInsuranceVars' `tradeWarVars' [w=votesPres2020Total], endog(cases_14dayAve_adj_max) absorb(fipstate) partial(`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_casePeak' ) cluster(fipstate)
eststo vm`m'
eststo vm`m', add(WeakIVs e(widstat))
eststo vm`m', add(UnderidP e(idp))
eststo vm`m', add(SarganEndogP e(estatp))
ivreghdfe `vote' (cases_14dayAve_adj_max = `nursingVars')		`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_casesPeak' `healthInsuranceVars' `tradeWarVars' [w=votesPres2020Total], endog(cases_14dayAve_adj_max) absorb(fipstate) partial(`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_casePeak' ) cluster(fipstate)
eststo vn`m'
eststo vn`m', add(WeakIVs e(widstat))
eststo vn`m', add(UnderidP e(idp))
eststo vn`m', add(SarganEndogP e(estatp))

}


qui {
noi di "*******************************************************"
noi di "*** Table 6A: Alternative COVID prevalence measures ***"
noi di "*******************************************************"
noi estout o1 o2 o3 o4 o5 o6, k(deaths_ave cases_ave deaths_oct_ave_adj cases_oct_ave_adj deaths_14dayAve_adj_max cases_14dayAve_adj_max `tradeWarVars' d_healthInsurance) cells(b(star fmt(3)) se(par fmt(3))) stats(N, fmt(%9.0g 3 3 3)) style(fixed) starlevels(# 0.10 ^ 0.05 * 0.01) 

}

*********************************
*** D.2 Placebo specification ***
*********************************

qui {
*** Baseline	
loc m=1
reghdfe `voteLag' deaths_ave 						`controlsNonCOVID' `healthVars'  `mobActControls_cum' `healthInsuranceVars' `tradeWarVars' [w=votesPres2020Total], absorb(fipstate) cluster(fipstate)
eststo b`m'

*** Health insurance and trade war IV (Lewbel)
qui {

*Zvars

loc Evar1 "TS3_US"
loc Evar2 "TS3_R"
loc Evar3 "agSubs_mfp_pw"
loc Evar4 "d_healthInsurance"

loc Z1s "empManuf hhY_50k_75kSh2016"
loc Z2s "empAgMining d_publicTransportWorkSh"
loc Z3s "empAgMining mei_ave diab_eye"
loc Z4s "healthInsuranceShare diab_lipids_10 diab_hemotest_10 mei_ave visits_relative_cumulative"

loc Xvars "`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_cum' id_s* healthInsurance"

di "`Xvars'"

* Trade war vars
forval i=1/3 {
	di in red "`Evar`i''"
	reg `Evar`i'' `Xvars'
	foreach x in `Xvars' {
		estat hettest `x', iid
						}
	    estat hettest `Z`i's', iid
			}

forval i=1/3 {

	foreach z in `Z`i's' {

	su `z', meanonly
	cap g m`z'=`z'-r(mean)

	reg `Evar`i'' `Xvars'
	cap drop ei
	cap predict ei, res
	cap g z`i'`z'=m`z'*ei
	cap drop m`z'
						}
}


* Health insurance
loc i=4 
	di in red "`Evar`i''"
	reg `Evar`i'' `Xvars'
	foreach x in `Xvars' {
		estat hettest `x', iid
						}
	    estat hettest `Z`i's', iid

	foreach z in `Z`i's' {

	su `z', meanonly
	cap g m`z'=`z'-r(mean)

	reg `Evar`i'' `Xvars' healthInsurance `tradeWarVars'
	cap drop ei
	cap predict ei, res
	cap g z`i'`z'=m`z'*ei
	cap drop m`z'
						}

	
ivreghdfe `voteLag' (`tradeWarVars' d_healthInsurance =  z1* z2* z3* z4*) healthInsurance deaths_ave `controlsNonCOVID' `healthVars'  `mobActControls_cum' [w=votesPres2020Total],  gmm2s endog(d_healthInsurance) absorb(fipstate) partial(`controlsNonCOVID' `healthVars'  `mobActControls_cum' `covidControls') cluster(fipstate)
loc m=`m'+1
eststo b`m'
eststo b`m', add(WeakIVs e(widstat))
eststo b`m', add(UnderidP e(idp))
eststo b`m', add(HansenP e(jp))
eststo b`m', add(SarganEndogP e(estatp))


}

noi di ""
noi di "*************************"
noi di "*** Table 6B. Placebo ***"
noi di "*************************"
noi di ""

noi di "b1: baseline"
noi di "b2: Lewbel instruments for trade war vars, d_healthInsurance"
noi estout b1 b2, k(deaths_ave d_healthInsurance `tradeWarVars') cells(b(star fmt(3)) se(par fmt(3))) stats(N UnderidP WeakIVs HansenP SarganEndogP, fmt(%9.0g 3 3 3)) style(fixed) starlevels(# 0.10 ^ 0.05 * 0.01) 

}



********************************************************************************
*************************** E. Heterogeneities *********************************
********************************************************************************

***********************************
*** E.1 Political heterogeneity ***
***********************************
qui {

*** Competitiveness: solid Rep vs solid Dem vs competitive
reghdfe `vote' `voteLag' `tradeWarVars' `healthInsuranceVars' deaths_ave `controlsNonCOVID' `healthVars'  `mobActControls_cum' if countySolidRepPres==1 [w=votesPres2020Total], absorb(fipstate) cluster(fipstate)
eststo r1
gen bCOVID_Rep=_b[deaths_ave]
gen bHI_Rep=_b[d_healthInsurance]
gen bTS_US_Rep=_b[TS3_US]
gen bTS_R_Rep=_b[TS3_R]
gen bAgSubs_Rep=_b[agSubs_mfp_pw]

reghdfe `vote' `voteLag' `tradeWarVars' `healthInsuranceVars' deaths_ave `controlsNonCOVID' `healthVars'  `mobActControls_cum' if countySolidDemPres==1 [w=votesPres2020Total], absorb(fipstate) cluster(fipstate)
eststo r2
gen bCOVID_Dem=_b[deaths_ave]
gen bHI_Dem=_b[d_healthInsurance]
gen bTS_US_Dem=_b[TS3_US]
gen bTS_R_Dem=_b[TS3_R]
gen bAgSubs_Dem=_b[agSubs_mfp_pw]

reghdfe `vote' `voteLag' `tradeWarVars' `healthInsuranceVars' deaths_ave `controlsNonCOVID' `healthVars'  `mobActControls_cum' if countyCompetitivePres==1 [w=votesPres2020Total], absorb(fipstate) cluster(fipstate)
eststo r3
gen bCOVID_Comp=_b[deaths_ave]
gen bHI_Comp=_b[d_healthInsurance]
gen bTS_US_Comp=_b[TS3_US]
gen bTS_R_Comp=_b[TS3_R]
gen bAgSubs_Comp=_b[agSubs_mfp_pw]


*** Partisanship: Clinton vs Trump counties
reghdfe `vote' `voteLag' `tradeWarVars' `healthInsuranceVars' deaths_ave `controlsNonCOVID' `healthVars'  `mobActControls_cum' if winPresRep2016==1 [w=votesPres2020Total], absorb(fipstate) cluster(fipstate)
eststo r4
gen bCOVID_DT=_b[deaths_ave]
gen bHI_DT=_b[d_healthInsurance]
gen bTS_US_DT=_b[TS3_US]
gen bTS_R_DT=_b[TS3_R]
gen bAgSubs_DT=_b[agSubs_mfp_pw]

reghdfe `vote' `voteLag' `tradeWarVars' `healthInsuranceVars' deaths_ave `controlsNonCOVID' `healthVars'  `mobActControls_cum' if winPresRep2016==0 [w=votesPres2020Total], absorb(fipstate) cluster(fipstate)
eststo r5
gen bCOVID_HC=_b[deaths_ave]
gen bHI_HC=_b[d_healthInsurance]
gen bTS_US_HC=_b[TS3_US]
gen bTS_R_HC=_b[TS3_R]
gen bAgSubs_HC=_b[agSubs_mfp_pw]


noi di ""
noi di "*****************************************************"
noi di "*** Table 7A (cols 1-6) - Political heterogeneity ***"
noi di "*****************************************************"
noi di "m7: baseline model"
noi di "r1: solid Rep counties"
noi di "r2: solid Dem counties"
noi di "r3: competitive counties"
noi di "r4: Trump counties"
noi di "r5: Clinton counties"
noi di ""

noi estout m7 r1 r2 r3 r4 r5, k(`tradeWarVars' d_healthInsurance deaths_ave `voteLag') cells(b(star fmt(3)) se(par fmt(3))) stats(N, fmt(%9.0g 3 3 3)) style(fixed) starlevels(# 0.10 ^ 0.05 * 0.01) 

}


********************************
*** E.2 Racial heterogeneity ***
********************************
qui {

loc D1 "racwht>=50"
loc D2 "racwht<50"

loc m=1	
reghdfe `vote' deaths_ave 						`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_cum' `healthInsuranceVars' `tradeWarVars' [w=votesPres2020Total] if racwht>=50, absorb(fipstate) cluster(fipstate)
eststo o`m'
gen bCOVID_W=_b[deaths_ave]
gen bHI_W=_b[d_healthInsurance]
gen bTS_US_W=_b[TS3_US]
gen bTS_R_W=_b[TS3_R]
gen bAgSubs_W=_b[agSubs_mfp_pw]

loc m=`m'+1	
reghdfe `vote' deaths_ave 						`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_cum' `healthInsuranceVars' `tradeWarVars' [w=votesPres2020Total] if racwht<50, absorb(fipstate) cluster(fipstate)
eststo o`m'
gen bCOVID_NW=_b[deaths_ave]
gen bHI_NW=_b[d_healthInsurance]
gen bTS_US_NW=_b[TS3_US]
gen bTS_R_NW=_b[TS3_R]
gen bAgSubs_NW=_b[agSubs_mfp_pw]

noi di ""
noi di "**************************************************"
noi di "*** Table 7A (cols 7-8) - Racial heterogeneity ***"
noi di "**************************************************"
noi di ""

noi di "model 1: white >=50% (majority white)"
noi di "model 2: white < 50% (majority minority)"

noi estout o1 o2, k(deaths_ave d_healthInsurance `tradeWarVars') cells(b(star fmt(3)) se(par fmt(3))) stats(N UnderidP WeakIVs SarganEndogP, fmt(%9.0g 3 3 3)) style(fixed) starlevels(# 0.10 ^ 0.05 * 0.01) 

}


***********************************
*** E.3 Trade war heterogeneity ***
***********************************
qui {

*** China only trade war
loc m=1
reghdfe `vote' deaths_ave `voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_cum' `healthInsuranceVars' `tradeWarChn' agSubs_mfp_pw [w=votesPres2020Total], absorb(fipstate) cluster(fipstate)
eststo p`m'
gen bCOVID_C=_b[deaths_ave]
gen bHI_C=_b[d_healthInsurance]
gen bTS_US_C=_b[chn_TS3_US]
gen bTS_R_C=_b[chn_TS3_R]
gen bAgSubs_C=_b[agSubs_mfp_pw]


*** Early trade war
loc m=`m'+1
reghdfe `vote' deaths_ave `voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_cum' `healthInsuranceVars' `tradeWarEarly' agSubs_mfp_pw [w=votesPres2020Total], absorb(fipstate) cluster(fipstate)
eststo p`m'
gen bCOVID_E=_b[deaths_ave]
gen bHI_E=_b[d_healthInsurance]
gen bTS_US_E=_b[TS6_US_old]
gen bTS_R_E=_b[TS6_Retaliation_old]
gen bAgSubs_E=_b[agSubs_mfp_pw]


noi di ""
noi di "******************************************************"
noi di "*** Table 7B (cols 1-3) - Trade war heterogeneity  ***"
noi di "******************************************************"
noi di ""

noi di "m7: baseline model"
noi di "model 1: China only trade war"
noi di "model 2: Early trade war"

noi estout m7 p1 p2, k(deaths_ave d_healthInsurance `tradeWarVars' `tradeWarEarly' `tradeWarChn') cells(b(star fmt(3)) se(par fmt(3))) stats(N, fmt(%9.0g 3 3 3)) style(fixed) starlevels(# 0.10 ^ 0.05 * 0.01) 
}


*******************************
*** E.4 COVID heterogeneity ***
*******************************
qui {
* Split sample into terciles

loc covidPrevalence "deaths_ave"

foreach c in `covidPrevalence' {
sort `c'
loc c1=`c'[997]
loc c2=`c'[1994]
loc C1 "`c'<`c1'"
loc C2 "`c'>=`c1' & `c'<`c2'"
loc C3 "`c'>=`c2'"


loc m=1	
reghdfe `vote' `c' 						`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_cum' `healthInsuranceVars' `tradeWarVars' [w=votesPres2020Total] if `c'<`c1', absorb(fipstate) cluster(fipstate)
eststo o`m'
gen bCOVID_lo=_b[deaths_ave]
gen bHI_lo=_b[d_healthInsurance]
gen bTS_US_lo=_b[TS3_US]
gen bTS_R_lo=_b[TS3_R]
gen bAgSubs_lo=_b[agSubs_mfp_pw]

loc m=`m'+1	
reghdfe `vote' `c' 						`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_cum' `healthInsuranceVars' `tradeWarVars' [w=votesPres2020Total] if `c'>=`c1' & `c'<`c2', absorb(fipstate) cluster(fipstate)
eststo o`m'
gen bCOVID_mid=_b[deaths_ave]
gen bHI_mid=_b[d_healthInsurance]
gen bTS_US_mid=_b[TS3_US]
gen bTS_R_mid=_b[TS3_R]
gen bAgSubs_mid=_b[agSubs_mfp_pw]

loc m=`m'+1	
reghdfe `vote' `c' 						`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_cum' `healthInsuranceVars' `tradeWarVars' [w=votesPres2020Total] if `c'>=`c2', absorb(fipstate) cluster(fipstate)
eststo o`m'
gen bCOVID_hi=_b[deaths_ave]
gen bHI_hi=_b[d_healthInsurance]
gen bTS_US_hi=_b[TS3_US]
gen bTS_R_hi=_b[TS3_R]
gen bAgSubs_hi=_b[agSubs_mfp_pw]

}
noi di ""
noi di "*************************************************"
noi di "*** Table 7B (cols 5-6) - COVID heterogeneity ***"
noi di "*************************************************"
noi di ""

noi di "model 1: bottom tercile of COVID prevalence"
noi di "model 2: middle tercile of COVID prevalence"
noi di "model 3: top tercile of COVID prevalence"

noi estout o1 o2 o3, k(deaths_ave d_healthInsurance `tradeWarVars') cells(b(star fmt(3)) se(par fmt(3))) stats(N UnderidP WeakIVs SarganEndogP, fmt(%9.0g 3 3 3)) style(fixed) starlevels(# 0.10 ^ 0.05 * 0.01) 

}



**********************************************************************
****************************** F. COVID ******************************
**********************************************************************

qui {

loc m=0	
reghdfe `vote' deaths_ave 						[w=votesPres2020Total], noabsorb cluster(fipstate)
eststo o`m'

loc m=`m'+1
reghdfe `vote' deaths_ave 						`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_cum' [w=votesPres2020Total], absorb(fipstate) cluster(fipstate)
eststo o`m'

ivreghdfe `vote' (deaths_ave = meatShare)		`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_cum' [w=votesPres2020Total], endog(deaths_ave) absorb(fipstate) cluster(fipstate)
eststo vm`m'
eststo vm`m', add(WeakIVs e(widstat))
eststo vm`m', add(UnderidP e(idp))
eststo vm`m', add(SarganEndogP e(estatp))

ivreghdfe `vote' (deaths_ave = `nursingVars')	`voteLag' `controlsNonCOVID' `healthVars'  `mobActControls_cum' [w=votesPres2020Total], endog(deaths_ave) absorb(fipstate) cluster(fipstate)
eststo vn`m'
eststo vn`m', add(WeakIVs e(widstat))
eststo vn`m', add(UnderidP e(idp))
eststo vn`m', add(SarganEndogP e(estatp))

noi di ""
noi di "**********************"
noi di "*** Table 8: COVID ***"
noi di "**********************"
noi di ""

noi di "model m0: vote on COVID, no controls/FEs"
noi di "model m1: add FEs"
noi di "model vm1: instrument with meat packing IV"
noi di "model vn1: instrument with nursing home IV"

noi estout o0 o1 vm1 vn1, k(deaths_ave `mobActControls_cum') cells(b(star fmt(3)) se(par fmt(3))) stats(N UnderidP WeakIVs SarganEndogP, fmt(%9.0g 3 3 3)) style(fixed) starlevels(# 0.10 ^ 0.05 * 0.01) 

}





********************************************************************************
****************************** G. Counterfactuals ******************************
********************************************************************************
qui {
*** Generate variables representing state-level data	
bys state: gen ns=_n
bys state: egen votesPres2020RepState=total(votesPres2020Rep)
bys state: egen votesPres2020DemState=total(votesPres2020Dem)
bys state: egen votesPres2020TotalState_2party=total(votesPres2020Total_2party)
gen marginState=votesPres2020RepState-votesPres2020DemState
gen marginStateShare=100*(marginState/votesPres2020TotalState)


********************
*** G.1 Baseline ***
********************
* Generate counterfactual effects
gen dcf_pres2020RepSh_2p_COVID=bCOVID*deaths_ave
gen dcf_pres2020RepSh_2p_USt=bTS_US*TS3_US
gen dcf_pres2020RepSh_2p_Rt=bTS_R*TS3_R
gen dcf_pres2020RepSh_2p_Ag=bAgSubs*agSubs_mfp_pw
gen dcf_pres2020RepSh_2p_HI=bHI*d_healthInsurance

loc varsCF "COVID USt Rt Ag HI"
foreach v in `varsCF' {
* County-level counterfactual voting outcomes (note: vote share dependent variable in % points, 0-100)
gen votesCfPres2020Rep=votesPres2020Rep-(1/100)*dcf_pres2020RepSh_2p_`v'*votesPres2020Total_2party
gen votesCfPres2020Dem=votesPres2020Dem+(1/100)*dcf_pres2020RepSh_2p_`v'*votesPres2020Total_2party
gen marginCf=votesCfPres2020Rep-votesCfPres2020Dem

* Aggregate to state-level counterfactual voting outcomes
bys state: egen   votesCfPres2020RepState=total(votesCfPres2020Rep)
bys state: egen   votesCfPres2020DemState=total(votesCfPres2020Dem)
gen marginCfState=votesCfPres2020RepState-votesCfPres2020DemState
gen marginCfStateShare=100*(marginCfState/votesPres2020TotalState_2p)
gen dcf_marginState=(marginCfState-marginState)
gen dcf_marginStateShare=100*(marginCfState-marginState)/votesPres2020TotalState_2p

sort marginStateShare 

* Export to excel
preserve
keep stateAbb marginStateShare marginCfStateShare dcf_marginStateShare ns
keep if ns==1
drop ns
export excel using "baselineCF_`v'.xlsx", firstrow(variables) replace
restore 

drop  votesCf* marginCf* dcf_margin*  
}
drop dcf_pres*


**************
*** G.2 IV ***
**************
* Generate counterfactual effects
gen dcf_pres2020RepSh_2p_COVID_L=bCOVID_L*deaths_ave
gen dcf_pres2020RepSh_2p_USt_L=bTS_US_L*TS3_US
gen dcf_pres2020RepSh_2p_Rt_L=bTS_R_L*TS3_R
gen dcf_pres2020RepSh_2p_Ag_L=bAgSubs_L*agSubs_mfp_pw
gen dcf_pres2020RepSh_2p_HI_L=bHI_L*d_healthInsurance

loc varsCF "COVID_L USt_L Rt_L Ag_L HI_L"
foreach v in `varsCF' {
* County-level counterfactual voting outcomes (note: vote share dependent variable in % points, 0-100)
gen votesCfPres2020Rep=votesPres2020Rep-(1/100)*dcf_pres2020RepSh_2p_`v'*votesPres2020Total_2party
gen votesCfPres2020Dem=votesPres2020Dem+(1/100)*dcf_pres2020RepSh_2p_`v'*votesPres2020Total_2party
gen marginCf=votesCfPres2020Rep-votesCfPres2020Dem

* Aggregate to state-level counterfactual voting outcomes
bys state: egen   votesCfPres2020RepState=total(votesCfPres2020Rep)
bys state: egen   votesCfPres2020DemState=total(votesCfPres2020Dem)
gen marginCfState=votesCfPres2020RepState-votesCfPres2020DemState
gen marginCfStateShare=100*(marginCfState/votesPres2020TotalState_2p)
gen dcf_marginState=(marginCfState-marginState)
gen dcf_marginStateShare=100*(marginCfState-marginState)/votesPres2020TotalState_2p

sort marginStateShare 

* Export to excel
preserve
keep stateAbb marginStateShare marginCfStateShare dcf_marginStateShare ns
keep if ns==1
drop ns
export excel using "LewbelCF_`v'.xlsx", firstrow(variables) replace
restore 

drop  votesCf* marginCf* dcf_margin*  
}
drop dcf_pres*


************************************************************************
*** G.3 Competitiveness (solid Rep, solid Dem, competitive counties) ***
************************************************************************
* Generate counterfactual effects
foreach p in Dem Rep Comp {
gen dcf_pres2020RepSh_2p_COVID_`p'=bCOVID_`p'*deaths_ave
}
foreach p in Dem Rep Comp {
gen dcf_pres2020RepSh_2p_USt_`p'=bTS_US_`p'*TS3_US
}
foreach p in Dem Rep Comp {
gen dcf_pres2020RepSh_2p_Rt_`p'=bTS_R_`p'*TS3_R
}
foreach p in Dem Rep Comp {
gen dcf_pres2020RepSh_2p_Ag_`p'=bAgSubs_`p'*agSubs_mfp_pw
}
foreach p in Dem Rep Comp {
gen dcf_pres2020RepSh_2p_HI_`p'=bHI_`p'*d_healthInsurance
}

loc varsCF "COVID USt Rt Ag HI"
foreach v in `varsCF' {
* County-level counterfactual voting outcomes (note: vote share dependent variable in % points, 0-100)
gen 	votesCfPres2020Rep=votesPres2020Rep-(1/100)*dcf_pres2020RepSh_2p_`v'_Dem*votesPres2020Total_2party if countySolidDemPres==1
replace votesCfPres2020Rep=votesPres2020Rep-(1/100)*dcf_pres2020RepSh_2p_`v'_Rep*votesPres2020Total_2party if countySolidRepPres==1
replace votesCfPres2020Rep=votesPres2020Rep-(1/100)*dcf_pres2020RepSh_2p_`v'_Comp*votesPres2020Total_2party if countyCompetitivePres==1

gen 	votesCfPres2020Dem=votesPres2020Dem+(1/100)*dcf_pres2020RepSh_2p_`v'_Dem*votesPres2020Total_2party if countySolidDemPres==1
replace votesCfPres2020Dem=votesPres2020Dem+(1/100)*dcf_pres2020RepSh_2p_`v'_Rep*votesPres2020Total_2party if countySolidRepPres==1
replace votesCfPres2020Dem=votesPres2020Dem+(1/100)*dcf_pres2020RepSh_2p_`v'_Comp*votesPres2020Total_2party if countyCompetitivePres==1

gen marginCf=votesCfPres2020Rep-votesCfPres2020Dem

* Aggregate to state-level counterfactual voting outcomes
bys state: egen   votesCfPres2020RepState=total(votesCfPres2020Rep)
bys state: egen   votesCfPres2020DemState=total(votesCfPres2020Dem)
gen marginCfState=votesCfPres2020RepState-votesCfPres2020DemState
gen marginCfStateShare=100*(marginCfState/votesPres2020TotalState_2p)
gen dcf_marginState=(marginCfState-marginState)
gen dcf_marginStateShare=100*(marginCfState-marginState)/votesPres2020TotalState_2p

sort marginStateShare 

* Export to excel
preserve
keep stateAbb marginStateShare marginCfStateShare dcf_marginStateShare ns
keep if ns==1
drop ns
export excel using "competitivenessCF_`v'.xlsx", firstrow(variables) replace
restore 

drop  votesCf* marginCf* dcf_margin*  
}
drop dcf_pres*


*****************************************************
*** G.4 Partisanship (Trump vs Clinton counties) ****
*****************************************************
* Generate counterfactual effects
foreach p in DT HC {
gen dcf_pres2020RepSh_2p_COVID_`p'=bCOVID_`p'*deaths_ave
}
foreach p in DT HC {
gen dcf_pres2020RepSh_2p_USt_`p'=bTS_US_`p'*TS3_US
}
foreach p in DT HC {
gen dcf_pres2020RepSh_2p_Rt_`p'=bTS_R_`p'*TS3_R
}
foreach p in DT HC {
gen dcf_pres2020RepSh_2p_Ag_`p'=bAgSubs_`p'*agSubs_mfp_pw
}
foreach p in DT HC {
gen dcf_pres2020RepSh_2p_HI_`p'=bHI_`p'*d_healthInsurance
}

loc varsCF "COVID USt Rt Ag HI"
foreach v in `varsCF' {
* County-level counterfactual voting outcomes (note: vote share dependent variable in % points, 0-100)
gen 	votesCfPres2020Rep=votesPres2020Rep-(1/100)*dcf_pres2020RepSh_2p_`v'_DT*votesPres2020Total_2party if winPresRep2016==1
replace votesCfPres2020Rep=votesPres2020Rep-(1/100)*dcf_pres2020RepSh_2p_`v'_HC*votesPres2020Total_2party if winPresRep2016==0

gen 	votesCfPres2020Dem=votesPres2020Dem+(1/100)*dcf_pres2020RepSh_2p_`v'_DT*votesPres2020Total_2party if winPresRep2016==1
replace votesCfPres2020Dem=votesPres2020Dem+(1/100)*dcf_pres2020RepSh_2p_`v'_HC*votesPres2020Total_2party if winPresRep2016==0

gen marginCf=votesCfPres2020Rep-votesCfPres2020Dem

* Aggregate to state-level counterfactual voting outcomes
bys state: egen   votesCfPres2020RepState=total(votesCfPres2020Rep)
bys state: egen   votesCfPres2020DemState=total(votesCfPres2020Dem)
gen marginCfState=votesCfPres2020RepState-votesCfPres2020DemState
gen marginCfStateShare=100*(marginCfState/votesPres2020TotalState_2p)
gen dcf_marginState=(marginCfState-marginState)
gen dcf_marginStateShare=100*(marginCfState-marginState)/votesPres2020TotalState_2p

sort marginStateShare 

* Export to excel
preserve
keep stateAbb marginStateShare marginCfStateShare dcf_marginStateShare ns
keep if ns==1
drop ns
export excel using "partisanCF_`v'.xlsx", firstrow(variables) replace
restore 

drop  votesCf* marginCf* dcf_margin*  
}
drop dcf_pres*


****************
*** G.6 Race ***
****************
* Generate counterfactual effects
foreach p in W NW {
gen dcf_pres2020RepSh_2p_COVID_`p'=bCOVID_`p'*deaths_ave
}
foreach p in W NW {
gen dcf_pres2020RepSh_2p_USt_`p'=bTS_US_`p'*TS3_US
}
foreach p in W NW {
gen dcf_pres2020RepSh_2p_Rt_`p'=bTS_R_`p'*TS3_R
}
foreach p in W NW {
gen dcf_pres2020RepSh_2p_Ag_`p'=bAgSubs_`p'*agSubs_mfp_pw
}
foreach p in W NW {
gen dcf_pres2020RepSh_2p_HI_`p'=bHI_`p'*d_healthInsurance
}

loc varsCF "COVID USt Rt Ag HI"
foreach v in `varsCF' {
* County-level counterfactual voting outcomes (note: vote share dependent variable in % points, 0-100)
gen 	votesCfPres2020Rep=votesPres2020Rep-(1/100)*dcf_pres2020RepSh_2p_`v'_W*votesPres2020Total_2party if racwht>=50
replace votesCfPres2020Rep=votesPres2020Rep-(1/100)*dcf_pres2020RepSh_2p_`v'_NW*votesPres2020Total_2party if racwht<50

gen 	votesCfPres2020Dem=votesPres2020Dem+(1/100)*dcf_pres2020RepSh_2p_`v'_W*votesPres2020Total_2party if racwht>=50
replace votesCfPres2020Dem=votesPres2020Dem+(1/100)*dcf_pres2020RepSh_2p_`v'_NW*votesPres2020Total_2party if racwht<50

gen marginCf=votesCfPres2020Rep-votesCfPres2020Dem

* Aggregate to state-level counterfactual voting outcomes
bys state: egen   votesCfPres2020RepState=total(votesCfPres2020Rep)
bys state: egen   votesCfPres2020DemState=total(votesCfPres2020Dem)
gen marginCfState=votesCfPres2020RepState-votesCfPres2020DemState
gen marginCfStateShare=100*(marginCfState/votesPres2020TotalState_2p)
gen dcf_marginState=(marginCfState-marginState)
gen dcf_marginStateShare=100*(marginCfState-marginState)/votesPres2020TotalState_2p

sort marginStateShare 

* Export to excel
preserve
keep stateAbb marginStateShare marginCfStateShare dcf_marginStateShare ns
keep if ns==1
drop ns
export excel using "raceCF_`v'.xlsx", firstrow(variables) replace
restore 

drop  votesCf* marginCf* dcf_margin*  
}
drop dcf_pres*



***************************
*** G.7 China trade war ***
***************************
* Generate counterfactual effects
gen dcf_pres2020RepSh_2p_COVID_C=bCOVID_C*deaths_ave
gen dcf_pres2020RepSh_2p_USt_C=bTS_US_C*chn_TS3_US
gen dcf_pres2020RepSh_2p_Rt_C=bTS_R_C*chn_TS3_R
gen dcf_pres2020RepSh_2p_Ag_C=bAgSubs_C*agSubs_mfp_pw
gen dcf_pres2020RepSh_2p_HI_C=bHI_C*d_healthInsurance

loc varsCF "COVID USt Rt Ag HI"
foreach v in `varsCF' {
* County-level counterfactual voting outcomes (note: vote share dependent variable in % points, 0-100)
gen votesCfPres2020Rep=votesPres2020Rep-(1/100)*dcf_pres2020RepSh_2p_`v'_C*votesPres2020Total_2party
gen votesCfPres2020Dem=votesPres2020Dem+(1/100)*dcf_pres2020RepSh_2p_`v'_C*votesPres2020Total_2party
gen marginCf=votesCfPres2020Rep-votesCfPres2020Dem

* Aggregate to state-level counterfactual voting outcomes
bys state: egen   votesCfPres2020RepState=total(votesCfPres2020Rep)
bys state: egen   votesCfPres2020DemState=total(votesCfPres2020Dem)
gen marginCfState=votesCfPres2020RepState-votesCfPres2020DemState
gen marginCfStateShare=100*(marginCfState/votesPres2020TotalState_2p)
gen dcf_marginState=(marginCfState-marginState)
gen dcf_marginStateShare=100*(marginCfState-marginState)/votesPres2020TotalState_2p

sort marginStateShare 

* Export to excel
preserve
keep stateAbb marginStateShare marginCfStateShare dcf_marginStateShare ns
keep if ns==1
drop ns
export excel using "tradeWarChinaCF_`v'.xlsx", firstrow(variables) replace
restore 

drop  votesCf* marginCf* dcf_margin*  
}
drop dcf_pres*


***************************
*** G.8 Early trade war ***
***************************
* Generate counterfactual effects
gen dcf_pres2020RepSh_2p_COVID_E=bCOVID_E*deaths_ave
gen dcf_pres2020RepSh_2p_USt_E=bTS_US_E*TS6_US_old
gen dcf_pres2020RepSh_2p_Rt_E=bTS_R_E*TS6_Retaliation_old
gen dcf_pres2020RepSh_2p_Ag_E=bAgSubs_E*agSubs_mfp_pw
gen dcf_pres2020RepSh_2p_HI_E=bHI_E*d_healthInsurance

loc varsCF "COVID USt Rt Ag HI"
foreach v in `varsCF' {
* County-level counterfactual voting outcomes (note: vote share dependent variable in % points, 0-100)
gen votesCfPres2020Rep=votesPres2020Rep-(1/100)*dcf_pres2020RepSh_2p_`v'_E*votesPres2020Total_2party
gen votesCfPres2020Dem=votesPres2020Dem+(1/100)*dcf_pres2020RepSh_2p_`v'_E*votesPres2020Total_2party
gen marginCf=votesCfPres2020Rep-votesCfPres2020Dem

* Aggregate to state-level counterfactual voting outcomes
bys state: egen   votesCfPres2020RepState=total(votesCfPres2020Rep)
bys state: egen   votesCfPres2020DemState=total(votesCfPres2020Dem)
gen marginCfState=votesCfPres2020RepState-votesCfPres2020DemState
gen marginCfStateShare=100*(marginCfState/votesPres2020TotalState_2p)
gen dcf_marginState=(marginCfState-marginState)
gen dcf_marginStateShare=100*(marginCfState-marginState)/votesPres2020TotalState_2p

sort marginStateShare 

* Export to excel
preserve
keep stateAbb marginStateShare marginCfStateShare dcf_marginStateShare ns
keep if ns==1
drop ns
export excel using "tradeWarEarlyCF_`v'.xlsx", firstrow(variables) replace
restore 

drop  votesCf* marginCf* dcf_margin*  
}
drop dcf_pres*



****************************
*** G.9 COVID prevalence ***
****************************
* Generate counterfactual effects
foreach p in lo mid hi {
gen dcf_pres2020RepSh_2p_COVID_`p'=bCOVID_`p'*deaths_ave
}
foreach p in lo mid hi {
gen dcf_pres2020RepSh_2p_USt_`p'=bTS_US_`p'*TS3_US
}
foreach p in lo mid hi {
gen dcf_pres2020RepSh_2p_Rt_`p'=bTS_R_`p'*TS3_R
}
foreach p in lo mid hi {
gen dcf_pres2020RepSh_2p_Ag_`p'=bAgSubs_`p'*agSubs_mfp_pw
}
foreach p in lo mid hi {
gen dcf_pres2020RepSh_2p_HI_`p'=bHI_`p'*d_healthInsurance
}

loc varsCF "COVID USt Rt Ag HI"
foreach v in `varsCF' {
* County-level counterfactual voting outcomes (note: vote share dependent variable in % points, 0-100)
gen 	votesCfPres2020Rep=votesPres2020Rep-(1/100)*dcf_pres2020RepSh_2p_`v'_lo*votesPres2020Total_2party if deaths_ave<`c1'
replace votesCfPres2020Rep=votesPres2020Rep-(1/100)*dcf_pres2020RepSh_2p_`v'_mid*votesPres2020Total_2party if deaths_ave>=`c1' & deaths_ave<`c2'
replace votesCfPres2020Rep=votesPres2020Rep-(1/100)*dcf_pres2020RepSh_2p_`v'_hi*votesPres2020Total_2party if deaths_ave>`c2'

gen 	votesCfPres2020Dem=votesPres2020Dem+(1/100)*dcf_pres2020RepSh_2p_`v'_lo*votesPres2020Total_2party if deaths_ave<`c1'
replace votesCfPres2020Dem=votesPres2020Dem+(1/100)*dcf_pres2020RepSh_2p_`v'_mid*votesPres2020Total_2party if deaths_ave>=`c1' & deaths_ave<`c2'
replace votesCfPres2020Dem=votesPres2020Dem+(1/100)*dcf_pres2020RepSh_2p_`v'_hi*votesPres2020Total_2party if deaths_ave>`c2'

gen marginCf=votesCfPres2020Rep-votesCfPres2020Dem

* Aggregate to state-level counterfactual voting outcomes
bys state: egen   votesCfPres2020RepState=total(votesCfPres2020Rep)
bys state: egen   votesCfPres2020DemState=total(votesCfPres2020Dem)
gen marginCfState=votesCfPres2020RepState-votesCfPres2020DemState
gen marginCfStateShare=100*(marginCfState/votesPres2020TotalState_2p)
gen dcf_marginState=(marginCfState-marginState)
gen dcf_marginStateShare=100*(marginCfState-marginState)/votesPres2020TotalState_2p

sort marginStateShare 

* Export to excel
preserve
keep stateAbb marginStateShare marginCfStateShare dcf_marginStateShare ns
keep if ns==1
drop ns
export excel using "covidCF_`v'.xlsx", firstrow(variables) replace
restore 

drop  votesCf* marginCf* dcf_margin*  
}
drop dcf_pres*



}

log close

