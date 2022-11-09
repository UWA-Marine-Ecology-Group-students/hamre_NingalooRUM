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

	cd "/Users/23088313/Documents/git_repos/hamre_MarmionRUM"

* read data 

	clear
	import delimited "data/02_data/2.1_acdat.csv"

**# data prep 


* making binary columns factors not numeric

	gen fsz1 = sz1 == 1
	gen fsz2 = sz2 == 1
	gen fsz3 = sz3 == 1
	gen fsz4 = sz4 == 1
	gen fsz5 = sz5 == 1
	gen fchoice = choice == 1

* storing new data 

	** export delimited using "data/03_data/3.0_dat.csv", replace

**# model
* use "#" for interaction, ## for full factorial interaction
* need to prefix numeric variables with "c." when including in interaction
* prefix factors with "i."

* base model
	
	clogit fchoice c.travelcost fsz1 fsz2 fsz3 fsz4 fsz5, group(tripid)
	estimates store basemod

* line_ma - test interactions example
	* clogit choice c.travelcost c.depth c.depthsquared area i.fisl_adj line_ma, group(tripid)
	* estimates store mod1
	
* get AIC for each

	* estimate stats basemod testmod testmod2 testmod3 testmod4 testmod5 testmod6
	
	* estimates testmod6
	
* store variance-covariance matrix 

	matrix b = e(b)'
	matrix v = e(V)

**# store outputs 

	* define directory  to store

	cd "/Users/23088313/Documents/git_repos/hamre_MarmionRUM/data/03_data"

	putexcel set ac_mod.xlsx, replace
	putexcel A2 = matrix(b), rownames
	putexcel B1 = "Vars"
	putexcel C1 = "Coef"
	putexcel set ac_mod.xlsx, modify sheet(v)
	putexcel A2 = matrix(v), rownames
	putexcel set ac_mod.xlsx, modify sheet(v)
	putexcel A1 = matrix(v), rownames
