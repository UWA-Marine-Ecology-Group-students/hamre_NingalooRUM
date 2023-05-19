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
	
	import delimited "`imdir'/2.1_asc_rum_`sim'.csv" // import data
	
**# wrangle

	gen fchoice = choice == 1 // make factors
	destring *, ignore("NA") replace // change strings to numeric

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
	asclogit fchoice c.fc [pweight=smpl_w], case(tripid) alt(gridid_alt) 
	estimates store mod1
	
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
