* ============================================================================
*
* Comprehensive Random Utility Model Script
*
* Nicole Hamre
*
* ============================================================================

**# set up

	clear // clear workspace

	local project "hamre_NingalooRUM" // define project
	local imdir "/Users/23088313/Documents/git_repos/`project'/data/02_data" // import data directory
	local exdir "/Users/23088313/Documents/git_repos/`project'/data/03_data" // export data directory
	
	local sim "sim4" // define simulation
	

	import delimited "`imdir'/2.1_nex_`sim'_stata.csv"  // import data - nex rum
	*import delimited "`imdir'/2.1_nex_ex_`sim'_stata.csv"  // import data - nex rum data but just fishing grid
	
**# data prep
	gen fchoice = choice == 1 // make factors
	destring *, ignore("NA") replace // change strings to numeric
	gen fn_sz = n_sz == 1 // make factors
	gen fsn_sz = n_sz == 1 // make factors
	gen fbn_sz = n_sz == 1 // make factors
	gen fres = resident == 1 // make factors
	
********************** Comprehensive RUM with Non-extractive users


**# testing zone variable

//  asclogit fchoice c.fcflt_spgam [pweight = ipw], case(trip_id) alt(gridid_alt)  // works but no nesting
//	estimates store no_zone
	
// 	asclogit fchoice c.fcflt_spgam fn_sz [pweight = ipw], case(trip_id) alt(gridid_alt)  // No convergence
// 	estimates store nex_sz
//	
// 	asclogit fchoice c.fcflt_spgam fsn_sz [pweight = ipw], case(trip_id) alt(gridid_alt)  // No convergence
// 	estimates store sn_sz
//	
// 	asclogit fchoice c.fcflt_spgam fbn_sz [pweight = ipw], case(trip_id) alt(gridid_alt)  // No convergence
// 	estimates store bn_sz
	
	
**# testing nesting
* nesting choice set to relax IRA (independence of irrelevant alternatives)
	
//	nlogit fchoice c.fcflt_spgam [pweight = ipw] || cset: || gridid_alt:, case(trip_id) //taus over 1 for all but s_e

// nlogit fchoice c.fcflt_spgam [pweight = ipw] || cset: || gridid_alt:, case(trip_id) noconst //taus over 1 for all but s_e
	
	** testing 2 levels
	
//	nlogitgen lvl2 = cset(b_e: b_e, b_n: b_n, s_e: s_e, s_n: s_n) // bottom level alts
//	nlogitgen lvl1 = lvl2(boat: b_e | b_n, shore: s_e | s_n) // top level alts	
	
//	nlogittree cset lvl2 lvl1, choice(choice) // nesting tree
	
//	nlogit fchoice c.fcflt_spgam [pweight = ipw] || lvl1: || lvl2: || gridid_alt:, case(trip_id) // no convergence
	
	** testing one level with different nesting structure
	
//	nlogit fchoice c.fcflt_spgam [pweight = ipw] || b_s_set: || gridid_alt:, case(trip_id) // taus over 1
	
//	nlogit fchoice c.fcflt_spgam [pweight = ipw] || n_e_set: || gridid_alt:, case(trip_id) // no convergence

**# try doing independent shore/boat based rums with ex.nex nesting

//	nlogit fchoice c.fcflt_spgam if b_s_set !="s" [pweight = ipw] || n_e_set: || gridid_alt:, case(trip_id) // boat based rum: tc +, tau - (the relationship between tc and tau makes sense but the relationship doesnt)
	
	
//	nlogit fchoice c.fcflt_spgam if b_s_set !="b" [pweight = ipw] || n_e_set: || gridid_alt:, case(trip_id) // shore based rum: tc -, tau over 1 for nex 

**** hausman test for iid

* run asligit with full subset

//	asclogit fchoice c.fcflt_spgam [pweight = ipw], case(trip_id) alt(gridid_alt)  // works but no nesting
//	estimates store fullset

//	asclogit fchoice c.fcflt_spgam [pweight = ipw] if cset != "b_e", case(trip_id) alt(gridid_alt)  // 

//	hausman fullset
	
	*** try without weights - doesnt work for weighted data

// nlogit fchoice c.fcflt_spgam || cset: || gridid_alt:, case(trip_id) // nested logit <- original choice for getting welfare but tc not significant
 
// *** testing two levels without wieghts
//
// 	nlogitgen lvl2 = cset(b_e: b_e, b_n: b_n, s_e: s_e, s_n: s_n) // bottom level alts
// 	nlogitgen lvl1 = lvl2(boat: b_e | b_n, shore: s_e | s_n) // top level alts	
//	
// 	nlogittree cset lvl2 lvl1, choice(choice) // nesting tree
//	
// 	nlogit fchoice c.fcflt_spgam || lvl1: || lvl2: || gridid_alt:, case(trip_id) // no convergence - but taus below 1
	
	
* was the final model - store full model output inc confidenec intervals - although this is not what i need
// putexcel set nex.xlsx, replace
// nlogit fchoice c.fcflt_spgam || cset: || gridid_alt:, case(trip_id) // nested logit
// putexcel (A1) = etable


// Trying to add variables to describe the choice of the cset
** need a significant travel cost - choose best model 

nlogit fchoice c.fcflt_spgam || cset: age || gridid_alt:, case(trip_id) // sign tc
estimates store age

// nlogit fchoice c.fcflt_spgam || cset: ex_times12m || gridid_alt:, case(trip_id) // lest signifuicant?
// estimates store ex_times12m


// nlogit fchoice c.fcflt_spgam || cset: fres  || gridid_alt:, case(trip_id) // not sig tc
// estimates store res
//
// nlogit fchoice c.fcflt_spgam || cset: fres ex_times12m || gridid_alt:, case(trip_id) // not sig tc
// estimates store res

// nlogit fchoice c.fcflt_spgam || cset: age fres ex_times12m || gridid_alt:, case(trip_id) // not sig tc
// estimates store all

**# Postestimation

// estimates store nex // store coefs
// estat alternatives // display summarys for alts
//
//* keep if zone_rm == 0 // filters data
// predict p* // predict p1: pr of choosen site, and p2: pr of choosen nest
// predict condp, condp hlevel(2) // predict condp: pr of choosing site given nest
// predict iv, iv // predict inclusive value for nest (log of dom of condp)
//
//
// /* // predict confidence intervals manually
//  predict xb, xb hlevel(2) // calc linear predictors
//  predict stdp, stdp  // calc error // calculate error for xb - this line does work 
//  generate lb = xb - invnormal(0.975)*error // calc lower ci for xb
//  generate ub = xb + invnormal(0.975)*error // calc upper ci for xb
//   generate plb = invlogit(lb) // convert to lower ci for probabilities
//  generate pub = invlogit(ub) // convert to upper ci for probabilities
// 
//  // this way worked - calculated ci around p2
//  predictnl pr = predict(), ci(t1 t2) // predict p2 (double check its the same) and ci
// */
//
// list trip_id choice cset gridid_alt  p2 p1 condp iv in 1/82, sepby(trip_id) divider
//
// /* Split by cset
// list trip_id choice cset gridid_alt p1 p2 condp iv in 1/34, sepby(trip_id) divider
// list trip_id choice cset gridid_alt p1 p2 condp iv in 35/54, sepby(trip_id) divider
// list trip_id choice cset gridid_alt p1 p2 condp iv in 55/62, sepby(trip_id) divider
// list trip_id choice cset gridid_alt p1 p2 condp iv in 63/82, sepby(trip_id) divider


// **# Store output
//

	matrix b = e(b)' // store coefficients 
	matrix v = e(V) // store variance-covariance matrix

 	* define directory  to store

 	cd "/Users/23088313/Documents/git_repos/hamre_NingalooRUM/data/03_data" // define directory to store model outputs in

 	putexcel set nex.xlsx, replace
 	putexcel A2 = matrix(b), rownames
 	putexcel B1 = "Vars"
 	putexcel C1 = "Coef"
 	putexcel set nex.xlsx, modify sheet(v)
 	putexcel A2 = matrix(v), rownames
 	putexcel set nex.xlsx, modify sheet(v)
 	putexcel A1 = matrix(v), rownames


**# Individual RUMs
	
** Comparable fishing RUM

	asclogit fchoice c.fcflt_spgam if cset == "b_e", case(trip_id) alt(gridid_alt)  

//  asclogit fchoice c.fcflt_spgam if cset == "b_n" [pweight = ipw], case(trip_id) alt(gridid_alt)   // works
//	asclogit fchoice c.fcflt_spgam if cset == "s_n" [pweight = ipw], case(trip_id) alt(gridid_alt)  // works
//	asclogit fchoice c.fcflt_spgam if cset == "s_e" [pweight = ipw], case(trip_id) alt(gridid_alt)  // works
	
	
***** conparable fishing rum to nex rum
	
	** Store output

	matrix b = e(b)' // store coefficients 
 	matrix v = e(V) // store variance-covariance matrix

 	* define directory  to store

 	cd "/Users/23088313/Documents/git_repos/hamre_NingalooRUM/data/03_data" // define directory to store model outputs in

 	putexcel set nex_ex.xlsx, replace
 	putexcel A2 = matrix(b), rownames
 	putexcel B1 = "Vars"
 	putexcel C1 = "Coef"
 	putexcel set nex_ex.xlsx, modify sheet(v)
 	putexcel A2 = matrix(v), rownames
 	putexcel set nex_ex.xlsx, modify sheet(v)
 	putexcel A1 = matrix(v), rownames
