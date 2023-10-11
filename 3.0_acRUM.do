* ============================================================================
*
* Random Utility Model Script
*
* Input: RUM_3.0.csv
* Output: 
*
* Nicole Hamre
*
* ============================================================================

**# set up

	clear // clear workspace

	local project "hamre_NingalooRUM" // define project
	local imdir "/Users/23088313/Documents/git_repos/`project'/data/02_data" // import data directory
	local exdir "/Users/23088313/Documents/git_repos/`project'/data/03_data" // export data directory
	
	local sim "sim1" // define simulation
	
	* import delimited "`imdir'/2.1_asc_rum_`sim'.csv" // import data - no catch data
	import delimited "`imdir'/2.3_asc_dat_`sim'.csv" // import data
	* import delimited "`imdir'/2.3_dat_att.csv" // import data - attribute model (asc vs att)
	
**# data prep

	gen fchoice = choice == 1 // make factors
	gen fisl_adj = isl_adj == 1 // make factors
	destring *, ignore("NA") replace // change strings to numeric
	
/**#Charlottes rum

	asclogit fchoice c.fc [pweight=smpl_w], case(tripid) alt(gridid_alt) 
	estimates store mod1
*/

**# ASC vs Att models

* ASC + cost ONLY model
/** Determine best cost variable
** Fuel cost varys based on calculation of fuel consumption (flat (?), 2l, 3l, gam) of boat and VOTT varys based on calculation of boat speed (flat (?), 2l, 3l, gam)
** 2l is the median across 2 levels of boat size: small (<= 3.75) and large boats (3.75 +), 3l is the median across 3 levels of boat size: small (<= 3.75), medium (<= 6.75) and large boats (6.75 +) and the gam predicts the consumption/speed as a function of boat size

*** first set - fuel cost only

// 	asclogit fchoice c.fc_flt [pweight = ipw], case(tripid) alt(gridid_alt) // fuel cost with flat consumption (?): - cost parameter
// 	estimates store fc_flt
//	
// 	asclogit fchoice c.fc2l [pweight = ipw], case(tripid) alt(gridid_alt) // fuel cost with 2l for consumption: - cost parameter
// 	estimates store fc2l
//
// 	asclogit fchoice c.fc3l [pweight = ipw], case(tripid) alt(gridid_alt) // fuel cost with 3l for consumption: - cost parameter
// 	estimates store fc3l
//	
// 	asclogit fchoice c.fc_gam [pweight = ipw], case(tripid) alt(gridid_alt) // fuel cost : - cost parameter
// 	estimates store fc_gam
//	
// 	estimates stats fc_flt fc2l fc3l fc_gam // fc_flt lowest BIC: 4969.14
	
*** second set - total cost with same conditions across fuel cost and vott
*** eg. fuel cost based of flat consumption + vott based on flat consumption and boat speed

// 	asclogit fchoice c.tc_flt [pweight = ipw], case(tripid) alt(gridid_alt) // consumption (fuel cost) and speed (VOTT) calculated with flat: - cost parameter
// 	estimates store tc_flt
//	
// 	asclogit fchoice c.tc2l [pweight = ipw], case(tripid) alt(gridid_alt) // consumption (fuel cost) and speed (VOTT) calculated with 2l of boat size: - cost parameter
// 	estimates store tc2l
//
// 	asclogit fchoice c.tc3l [pweight = ipw], case(tripid) alt(gridid_alt) // consumption (fuel cost) and speed (VOTT) calculated with 3l of boat size: - cost parameter
// 	estimates store tc3l
//	
// 	asclogit fchoice c.tc_gam [pweight = ipw], case(tripid) alt(gridid_alt) // consumption (fuel cost) and speed (VOTT) calculated with gam: - cost parameter
// 	estimates store tc_gam
//	
// 	estimates stats tc_flt tc2l tc3l tc_gam // tc_gam lowest BIC: 5009.45
	
*** third set - flat fuel cost with differential vott
*** flat fuel cost + vott based on speeds in 2l, 3l and gam
	
// 	asclogit fchoice c.fcflt_sp2l [pweight = ipw], case(tripid) alt(gridid_alt) // fuel cost with flat consumption  + speed (VOTT) calculted with 2l of boat size: - cost parameter
// 	estimates store fcflt_sp2l
//
// 	asclogit fchoice c.fcflt_sp3l [pweight = ipw], case(tripid) alt(gridid_alt) // fuel cost with flat consumption  +  speed (VOTT) calculated with 3l of boat size: - cost parameter
// 	estimates store fcflt_sp3l
//	
//	asclogit fchoice c.fcflt_spgam [pweight = ipw], case(tripid) alt(gridid_alt) // fuel cost with flat consumption  +  speed (VOTT) calculated with 3l of boat size: - cost parameter
//	estimates store fcflt_spgam
	
// 	estimates stats fcflt_sp2l fcflt_sp3l fcflt_spgam // fcflt_spgam lowest BIC: 4852.454
*/
	
* Final ASC + cost ONLY model uses fcflt_spgam as cost variable, BIC: 4865.4
/*


	asclogit fchoice c.fcflt_spgam [pweight = ipw], case(tripid) alt(gridid_alt) // fuel cost with flat consumption  +  speed (VOTT) calculated with 3l of boat size: - cost parameter
	estimates store ascONLY
	
	estimates stats ascONLY

** Store output

	matrix b = e(b)' // store coefficients 
	matrix v = e(V) // store variance-covariance matrix

	* define directory  to store

	cd "/Users/23088313/Documents/git_repos/hamre_NingalooRUM/data/03_data" // define directory to store model outputs in

	putexcel set asc_only.xlsx, replace
	putexcel A2 = matrix(b), rownames
	putexcel B1 = "Vars"
	putexcel C1 = "Coef"
	putexcel set asc_only.xlsx, modify sheet(v)
	putexcel A2 = matrix(v), rownames
	putexcel set asc_only.xlsx, modify sheet(v)
	putexcel A1 = matrix(v), rownames
*/

**# ASC + site attributes

/*** test vars for convergence

	asclogit fchoice c.fcflt_spgam offdist_log [pweight = ipw], case(tripid) alt(gridid_alt) // no convergence
	estimates store ascatt1
	
	asclogit fchoice c.fcflt_spgam arealog  [pweight = ipw], case(tripid) alt(gridid_alt) // no convergence
	estimates store ascatt2
	
	asclogit fchoice c.fcflt_spgam bl_offdist  [pweight = ipw], case(tripid) alt(gridid_alt) // convergence
	estimates store ascatt3
	
	asclogit fchoice c.fcflt_spgam fcflt_aninc  [pweight = ipw], case(tripid) alt(gridid_alt) // convergence
	estimates store ascatt4
	
	asclogit fchoice c.fcflt_spgam swell  [pweight = ipw], case(tripid) alt(gridid_alt) // convergence
	estimates store ascatt5
	
	asclogit fchoice c.fcflt_spgam wind  [pweight = ipw], case(tripid) alt(gridid_alt) // convergence
	estimates store ascatt6
	
	asclogit fchoice c.fcflt_spgam fisl_adj  [pweight = ipw], case(tripid) alt(gridid_alt) // no convergence
	estimates store ascatt7
	
	asclogit fchoice c.fcflt_spgam pfit_dem  [pweight = ipw], case(tripid) alt(gridid_alt) // convergence
	estimates store ascatt8
*/
	
** Final model with covariates that converge, BIC: 4735.538

// 	asclogit fchoice c.fcflt_spgam bl_offdist fcflt_aninc pfit_dem swell [pweight = ipw], case(tripid) alt(gridid_alt) // convergence, BIC: 4735.538 - old model, removing interactions and offshore distance
// 	estimates store ascatt
	
	asclogit fchoice c.fcflt_spgam  pfit_dem swell [pweight = ipw], case(tripid) alt(gridid_alt) // convergence, BIC: 4867.2
	estimates store ascatt
	
	estimates stats ascatt
	
	** Store output

	matrix b = e(b)' // store coefficients 
	matrix v = e(V) // store variance-covariance matrix

	* define directory  to store

	cd "/Users/23088313/Documents/git_repos/hamre_NingalooRUM/data/03_data" // define directory to store model outputs in

	putexcel set asc_att.xlsx, replace
	putexcel A2 = matrix(b), rownames
	putexcel B1 = "Vars"
	putexcel C1 = "Coef"
	putexcel set asc_att.xlsx, modify sheet(v)
	putexcel A2 = matrix(v), rownames
	putexcel set asc_att.xlsx, modify sheet(v)
	putexcel A1 = matrix(v), rownames

/** Site attributes at ASC scale

	clogit fchoice c.fcflt_spgam  pfit_dem swell [pweight = ipw], group(tripid) // convergence, BIC: 5678.8
	estimates store attatasc
	
	estimates stats attatasc
	
	
	** Store output

	matrix b = e(b)' // store coefficients 
	matrix v = e(V) // store variance-covariance matrix

	* define directory  to store

	cd "/Users/23088313/Documents/git_repos/hamre_NingalooRUM/data/03_data" // define directory to store model outputs in

	putexcel set asc_attatasc.xlsx, replace
	putexcel A2 = matrix(b), rownames
	putexcel B1 = "Vars"
	putexcel C1 = "Coef"
	putexcel set asc_attatasc.xlsx, modify sheet(v)
	putexcel A2 = matrix(v), rownames
	putexcel set asc_attatasc.xlsx, modify sheet(v)
	putexcel A1 = matrix(v), rownames
*/

** Fine scale attribute model
/*
* Comparable model 
 
	clogit fchoice c.nl_fcflt_spgam bl_nl_offdist nl_fcflt_aninc pfit_dem swell [pweight = ipw], group(tripid) // convergence, BIC: 9271
	estimates store att
	
	estimates stats att
	
	
	** Store output

	matrix b = e(b)' // store coefficients 
	matrix v = e(V) // store variance-covariance matrix

	* define directory  to store

	cd "/Users/23088313/Documents/git_repos/hamre_NingalooRUM/data/03_data" // define directory to store model outputs in

	putexcel set attmod.xlsx, replace
	putexcel A2 = matrix(b), rownames
	putexcel B1 = "Vars"
	putexcel C1 = "Coef"
	putexcel set attmod.xlsx, modify sheet(v)
	putexcel A2 = matrix(v), rownames
	putexcel set attmod.xlsx, modify sheet(v)
	putexcel A1 = matrix(v), rownames
*/

** Optimised model
/* Testing
 
	clogit fchoice c.nl_fcflt_spgam bl_nl_offdist nl_fcflt_aninc swell pfit_dem [pweight = ipw], group(tripid) // convergence, BIC: 9271
	estimates store comparable
	
	clogit fchoice c.nl_fcflt_spgam bl_nl_offdist nl_fcflt_aninc swell pfit_dem arealog [pweight = ipw], group(tripid) // convergence, BIC: 8833
	estimates store opt1
	
	clogit fchoice c.nl_fcflt_spgam bl_nl_offdist nl_fcflt_aninc swell pfit_dem arealog log_nl_km_br [pweight = ipw], group(tripid) // convergence, BIC: 8813
	estimates store opt2
	
	clogit fchoice c.nl_fcflt_spgam bl_nl_offdist nl_fcflt_aninc swell pfit_dem arealog log_nl_km_br  fisl_adj [pweight = ipw], group(tripid) // convergence, BIC: 8778
	estimates store opt3
	
	clogit fchoice c.nl_fcflt_spgam  nl_fcflt_aninc swell pfit_dem arealog log_nl_km_br  fisl_adj [pweight = ipw], group(tripid) // convergence, BIC: 8786
	estimates store opt4
	
	clogit fchoice c.nl_fcflt_spgam  bl_nl_offdist nl_fcflt_aninc swell pfit_dem arealog fisl_adj [pweight = ipw], group(tripid) // convergence, BIC: 8789
	estimates store opt5
	
	clogit fchoice c.nl_fcflt_spgam  bl_nl_offdist nl_fcflt_aninc swell pfit_dem arealog fisl_adj log_depth log_nl_km_br [pweight = ipw], group(tripid) // convergence, BIC: 8779
	estimates store opt6
	
	clogit fchoice c.nl_fcflt_spgam  nl_fcflt_aninc swell pfit_dem arealog log_nl_km_br  fisl_adj wind [pweight = ipw], group(tripid) // convergence, BIC: 8798
	estimates store opt7
	
	estimates stats comparable opt1 opt2 opt3 opt4 opt5 opt6 opt7 
	
	** Investigating catch 
	** Apart from comparable model catch is -ve in every model
	
	clogit fchoice c.nl_fcflt_spgam  pfit_dem  [pweight = ipw], group(tripid) // convergence, BIC: 9400
	estimates store opt8
	
	clogit fchoice c.nl_fcflt_spgam  pfit_dem  log_nl_km_br [pweight = ipw], group(tripid) // convergence, BIC: 9409
	estimates store opt9
	
	clogit fchoice c.nl_fcflt_spgam  pfit_dem  bl_nl_offdist [pweight = ipw], group(tripid) // convergence, BIC: 9388
	estimates store opt10
	
	clogit fchoice c.nl_fcflt_spgam  pfit_dem  nl_fcflt_aninc [pweight = ipw], group(tripid) // convergence, BIC: 9266
	estimates store opt11
	
	clogit fchoice c.nl_fcflt_spgam  pfit_dem  swell [pweight = ipw], group(tripid) // convergence, BIC: 9397
	estimates store opt12
	
	clogit fchoice c.nl_fcflt_spgam  pfit_dem  arealog [pweight = ipw], group(tripid) // convergence, BIC: 8964
	estimates store opt13
	
	clogit fchoice c.nl_fcflt_spgam  pfit_dem  fisl_adj [pweight = ipw], group(tripid) // convergence, BIC: 9347
	estimates store opt14
	
	clogit fchoice c.nl_fcflt_spgam  pfit_dem  log_depth [pweight = ipw], group(tripid) // convergence, BIC: 9381
	estimates store opt15
	
	clogit fchoice c.nl_fcflt_spgam  pfit_dem  wind [pweight = ipw], group(tripid) // convergence, BIC: 9398
	estimates store opt16
	
	clogit fchoice c.nl_fcflt_spgam  pfit_dem  swell log_depth [pweight = ipw], group(tripid) // convergence, BIC: 9395
	estimates store opt17
	
	clogit fchoice c.nl_fcflt_spgam  pfit_dem  swell log_depth arealog [pweight = ipw], group(tripid) // convergence, BIC: 8958
	estimates store opt18
	
	clogit fchoice c.nl_fcflt_spgam  pfit_dem  swell log_depth bl_nl_offdist nl_fcflt_aninc [pweight = ipw], group(tripid) // convergence, BIC: 9266
	estimates store opt19
	
	clogit fchoice c.nl_fcflt_spgam  pfit_dem  swell log_depth bl_nl_offdist nl_fcflt_aninc log_nl_km_br [pweight = ipw], group(tripid) // convergence, BIC: 9256
	estimates store opt20
	
	clogit fchoice c.nl_fcflt_spgam  pfit_dem  swell log_depth bl_nl_offdist nl_fcflt_aninc log_nl_km_br fisl_adj [pweight = ipw], group(tripid) // convergence, BIC: 9200
	estimates store opt21
	
	clogit fchoice c.nl_fcflt_spgam  pfit_dem  swell log_depth bl_nl_offdist nl_fcflt_aninc log_nl_km_br fisl_adj wind [pweight = ipw], group(tripid) // convergence, BIC:9214
	estimates store opt22
	
	estimates stats comparable opt8 opt9 opt10 opt11 opt12 opt13 opt14 opt15 opt16 opt17 opt18 opt19 opt20 opt21 opt22
*/
/* Final optimised attribute model
** optimisation 21 is the best model that makes sense, BIC: 9200

	clogit fchoice c.nl_fcflt_spgam  pfit_dem  swell log_depth bl_nl_offdist nl_fcflt_aninc log_nl_km_br fisl_adj [pweight = ipw], group(tripid) // convergence, BIC: 9200
	estimates store opt21

	estimates stats opt21
	
	** Store output

	matrix b = e(b)' // store coefficients 
	matrix v = e(V) // store variance-covariance matrix

	* define directory  to store

	cd "/Users/23088313/Documents/git_repos/hamre_NingalooRUM/data/03_data" // define directory to store model outputs in

	putexcel set attmod_opt.xlsx, replace
	putexcel A2 = matrix(b), rownames
	putexcel B1 = "Vars"
	putexcel C1 = "Coef"
	putexcel set attmod_opt.xlsx, modify sheet(v)
	putexcel A2 = matrix(v), rownames
	putexcel set attmod_opt.xlsx, modify sheet(v)
	putexcel A1 = matrix(v), rownames
*/
