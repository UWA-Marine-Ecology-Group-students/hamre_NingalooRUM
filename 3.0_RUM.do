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
	import delimited "data/02_data/2.2_dat_base.csv" 
	
**# data prep 

* making binary columns factors not numeric

	gen fisl_adj = isl_adj == 1
	gen fchoice = choice == 1
	gen logkm = log(nl_km_br + 1)

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

	clogit choice c.nl_tc3l c.logarea c.logkm c.swell fisl_adj, group(tripid)
	estimates store mod1
	
	estimates stats mod1
	
	
	
	* store variance-covariance matrix 

	matrix b = e(b)'
	matrix v = e(V)

	* store outputs 
	* define directory  to store

	cd "/Users/23088313/Documents/git_repos/hamre_NingalooRUM/data/03_data"

	putexcel set attmod_base.xlsx, replace
	putexcel A2 = matrix(b), rownames
	putexcel B1 = "Vars"
	putexcel C1 = "Coef"
	putexcel set attmod_base.xlsx, modify sheet(v)
	putexcel A2 = matrix(v), rownames
	putexcel set attmod_base.xlsx, modify sheet(v)
	putexcel A1 = matrix(v), rownames
	
