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
* define directory  

	cd "/Users/23088313/Documents/git_repos/hamre_NingalooRUM"

* read data 

	clear
// 	import delimited "data/02_data/2.3_dat_base.csv" 
// 	import delimited "data/02_data/2.2_dat_base.csv" 

import delimited "data/02_data/2.3_dat_att.csv" 
	
**# data prep 

* making binary columns factors not numeric

	gen fisl_adj = isl_adj == 1
	gen fchoice = choice == 1
	gen logkm = log(nl_km_br + 1)
	gen f_pred_catch_bin_fit = pred_catch_bin_fit == 1 // make factor	
	gen dpethsq = depth*depth

* changing strings to numeric

	destring *, ignore("NA") replace

* transformations

	* gen depthsquared = depth*depth	
	
**# MODEL FOR BIOECONOMIC MODEL
//  clogit choice c.l_ffc c.logarea c.pfit_dem, group(tripid)
// 	estimates store mod2
//	
// 	estimates stats mod1 mod2

**# ATTRIBUTE MODEL FOR ASC VS ATT PAPER



// 	clogit choice nl_fcflt_spgam f_pred_catch_bin_fit, group(tripid) // + catch
// 	estimates store mod1
	
// 	clogit choice nl_fcflt_spgam f_pred_catch_bin_fit swell, group(tripid) // + catch, & swell (not good)
// 	estimates store mod2
	
	
// 	clogit choice nl_fcflt_spgam f_pred_catch_bin_fit arealog, group(tripid) // makeds catch negative
// 	estimates store mod3
	
	clogit choice nl_fcflt_spgam f_pred_catch_bin_fit fisl_adj, group(tripid) // + catch
	estimates store mod4
	
// 	clogit choice nl_fcflt_spgam f_pred_catch_bin_fit km_mainland, group(tripid) // + catch
// 	estimates store mod5
//	
// 	estimates stats mod1 mod4 mod5
	
	
	
	* store variance-covariance matrix 

	matrix b = e(b)'
	matrix v = e(V)

	* store outputs 
	* define directory  to store

	cd "/Users/23088313/Documents/git_repos/hamre_NingalooRUM/data/03_data"

	putexcel set attmod.xlsx, replace
	putexcel A2 = matrix(b), rownames
	putexcel B1 = "Vars"
	putexcel C1 = "Coef"
	putexcel set attmod.xlsx, modify sheet(v)
	putexcel A2 = matrix(v), rownames
	putexcel set attmod.xlsx, modify sheet(v)
	putexcel A1 = matrix(v), rownames
	
