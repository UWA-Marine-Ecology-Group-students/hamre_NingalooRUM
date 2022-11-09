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
	**import delimited "data/02_data/2.1_5kmdat.csv"
	import delimited "data/02_data/2.1_dat.csv"
	**import delimited "data/02_data/2.1_dat_dem.csv"
	
**# data prep 

* making binary columns factors not numeric

	* gen fkids = kids == 1
	gen fisl_adj = isl_adj == 1
	gen fchoice = choice == 1
	gen fshelter = shelter == 1
	gen fcomhex = commhex == 1
	gen fshex = shex == 1
	gen fmhex = mhex == 1
	gen flhex = lhex == 1
	* gen fline = line == 1
	* gen ftroll = troll == 1
	* gen fdive = dive == 1

* changing strings to factors

	* encode resident, gen(fres)
	encode hex, gen(fhex)

* changing strings to numeric

	destring *, ignore("NA") replace

* transformations

	* gen depthsquared = depth*depth
	gen arealog = log(area+1)
	
* making interactions
* line
	* gen line_depth = fline*depth 
	* gen  line_depthsq = fline*depthsquared
	* gen line_reef = fline*reef
	* gen line_ma = fline*macroalgae
	* gen line_sg = fline*seagrass
	* gen line_isl = fline*fisl_adj
	
* troll	
	* gen troll_sst = ftroll*sst
	* gen troll_area = ftroll*area
	* gen troll_reef = ftroll*reef	
	* gen troll_sg = ftroll*seagrass
	* gen troll_ma = ftroll*macroalgae
	
* dive
	* gen dive_depth = fdive*depth
	* gen dive_depthsq = fdive*depthsquared
	* gen dive_reef = fdive*reef
	* gen dive_ma = fdive*macroalgae
	
* kids
	* gen kids_tc = fkids*travelcost
	* gen kids_isl = fkids*isl_adj
	* gen res_reef = fres*reef
	* gen boat_tc = boatlength*travelcost
	
* completehex/arealog
	 gen hex_arealog = fcomhex*arealog
	 gen hex_area = fcomhex*area
	 gen shex_arealog = fshex*arealog
	 gen shex_area = fshex*area
	

* storing new data 

	** export delimited using "data/03_data/3.0_dat.csv", replace

**# model
* use "#" for interaction, ## for full factorial interaction
* need to prefix numeric variables with "c." when including in interaction
* prefix factors with "i."

**# base model
	
	clogit choice c.travelcost c.depth arealog km_mainland fisl_adj fshelter, group(tripid)
	estimates store attmod_base
	
	* store variance-covariance matrix 

	matrix b = e(b)'
	matrix v = e(V)

	* store outputs 
	* define directory  to store

	cd "/Users/23088313/Documents/git_repos/hamre_MarmionRUM/data/03_data"

	putexcel set attmod_base.xlsx, replace
	putexcel A2 = matrix(b), rownames
	putexcel B1 = "Vars"
	putexcel C1 = "Coef"
	putexcel set attmod_base.xlsx, modify sheet(v)
	putexcel A2 = matrix(v), rownames
	putexcel set attmod_base.xlsx, modify sheet(v)
	putexcel A1 = matrix(v), rownames
	
	* model overestimates impact of area, with very high coef for area log, increases welfare impact dramatically
	* try removing area to check and see if its the problem
	
**# No area
	
	clogit choice c.travelcost c.depth km_mainland fisl_adj fshelter, group(tripid)
	estimates store attmod_noarea
	
	* store variance-covariance matrix 

	matrix b = e(b)'
	matrix v = e(V)

	* store outputs 
	* define directory  to store

	cd "/Users/23088313/Documents/git_repos/hamre_MarmionRUM/data/03_data"

	putexcel set attmod_noarea.xlsx, replace
	putexcel A2 = matrix(b), rownames
	putexcel B1 = "Vars"
	putexcel C1 = "Coef"
	putexcel set attmod_noarea.xlsx, modify sheet(v)
	putexcel A2 = matrix(v), rownames
	putexcel set attmod_noarea.xlsx, modify sheet(v)
	putexcel A1 = matrix(v), rownames
	* a bit better, area needs to be captured though. Looked at data, there are no data point in small (0 -0.1) or medium (0.1 - 0.6) hexagons. 

**# Area (no log transformation)
	
	clogit choice c.travelcost c.depth area km_mainland fisl_adj fshelter, group(tripid)
	estimates store attmod_area
	
	* store variance-covariance matrix 

	matrix b = e(b)'
	matrix v = e(V)

	* store outputs 
	* define directory  to store

	cd "/Users/23088313/Documents/git_repos/hamre_MarmionRUM/data/03_data"

	putexcel set attmod_area.xlsx, replace
	putexcel A2 = matrix(b), rownames
	putexcel B1 = "Vars"
	putexcel C1 = "Coef"
	putexcel set attmod_area.xlsx, modify sheet(v)
	putexcel A2 = matrix(v), rownames
	putexcel set attmod_area.xlsx, modify sheet(v)
	putexcel A1 = matrix(v), rownames
	
	* No improvemnet
	* -> can we capture area with a binary complete hex var?
	
**# Complete hex
	
	clogit choice c.travelcost c.depth commhex km_mainland fisl_adj fshelter, group(tripid)
	estimates store attmod_comhex
	
	* store variance-covariance matrix 

	matrix b = e(b)'
	matrix v = e(V)

	* store outputs 
	* define directory  to store

	cd "/Users/23088313/Documents/git_repos/hamre_MarmionRUM/data/03_data"

	putexcel set attmod_comhex.xlsx, replace
	putexcel A2 = matrix(b), rownames
	putexcel B1 = "Vars"
	putexcel C1 = "Coef"
	putexcel set attmod_comhex.xlsx, modify sheet(v)
	putexcel A2 = matrix(v), rownames
	putexcel set attmod_comhex.xlsx, modify sheet(v)
	putexcel A1 = matrix(v), rownames
	
	* didn't work the first time must of had varibales muddled - second time it worked. Tried all the below models before this worked.  
	* --> binary complete hex variables and arealog

**# Complete hex + arealog

	clogit choice c.travelcost c.depth arealog commhex km_mainland fisl_adj fshelter, group(tripid)
	estimates store attmod_arealog_comhex
	
	* store variance-covariance matrix 

	matrix b = e(b)'
	matrix v = e(V)

	* store outputs 
	* define directory  to store

	cd "/Users/23088313/Documents/git_repos/hamre_MarmionRUM/data/03_data"

	putexcel set attmod_arealog_comhex.xlsx, replace
	putexcel A2 = matrix(b), rownames
	putexcel B1 = "Vars"
	putexcel C1 = "Coef"
	putexcel set attmod_arealog_comhex.xlsx, modify sheet(v)
	putexcel A2 = matrix(v), rownames
	putexcel set attmod_arealog_comhex.xlsx, modify sheet(v)
	putexcel A1 = matrix(v), rownames
	

**# Complete hex + area

	clogit choice c.travelcost c.depth area commhex km_mainland fisl_adj fshelter, group(tripid)
	estimates store attmod_arealog_comhex
	
	* store variance-covariance matrix 

	matrix b = e(b)'
	matrix v = e(V)

	* store outputs 
	* define directory  to store

	cd "/Users/23088313/Documents/git_repos/hamre_MarmionRUM/data/03_data"

	putexcel set attmod_area_comhex.xlsx, replace
	putexcel A2 = matrix(b), rownames
	putexcel B1 = "Vars"
	putexcel C1 = "Coef"
	putexcel set attmod_area_comhex.xlsx, modify sheet(v)
	putexcel A2 = matrix(v), rownames
	putexcel set attmod_area_comhex.xlsx, modify sheet(v)
	putexcel A1 = matrix(v), rownames

**# Complete hex*arealog	

	clogit choice c.travelcost c.depth hex_arealog km_mainland fisl_adj fshelter, group(tripid)
	estimates store attmod_arealoghex
	
	* store variance-covariance matrix 

	matrix b = e(b)'
	matrix v = e(V)

	* store outputs 
	* define directory  to store

	cd "/Users/23088313/Documents/git_repos/hamre_MarmionRUM/data/03_data"

	putexcel set attmod_arealoghex.xlsx, replace
	putexcel A2 = matrix(b), rownames
	putexcel B1 = "Vars"
	putexcel C1 = "Coef"
	putexcel set attmod_arealoghex.xlsx, modify sheet(v)
	putexcel A2 = matrix(v), rownames
	putexcel set attmod_arealoghex.xlsx, modify sheet(v)
	putexcel A1 = matrix(v), rownames


**# Complete hex*arealog + arealog	

	clogit choice c.travelcost c.depth arealog hex_arealog km_mainland fisl_adj fshelter, group(tripid)
	estimates store attmod_arealog_arealoghex
	
	* store variance-covariance matrix 

	matrix b = e(b)'
	matrix v = e(V)

	* store outputs 
	* define directory  to store

	cd "/Users/23088313/Documents/git_repos/hamre_MarmionRUM/data/03_data"

	putexcel set attmod_arealog_arealoghex.xlsx, replace
	putexcel A2 = matrix(b), rownames
	putexcel B1 = "Vars"
	putexcel C1 = "Coef"
	putexcel set attmod_arealog_arealoghex.xlsx, modify sheet(v)
	putexcel A2 = matrix(v), rownames
	putexcel set attmod_arealog_arealoghex.xlsx, modify sheet(v)
	putexcel A1 = matrix(v), rownames
	
**# Complete hex*area
	
	clogit choice c.travelcost c.depth hex_area km_mainland fisl_adj fshelter, group(tripid)
	estimates store attmod_areahex
	
	* store variance-covariance matrix 

	matrix b = e(b)'
	matrix v = e(V)

	* store outputs 
	* define directory  to store

	cd "/Users/23088313/Documents/git_repos/hamre_MarmionRUM/data/03_data"

	putexcel set attmod_areahex.xlsx, replace
	putexcel A2 = matrix(b), rownames
	putexcel B1 = "Vars"
	putexcel C1 = "Coef"
	putexcel set attmod_areahex.xlsx, modify sheet(v)
	putexcel A2 = matrix(v), rownames
	putexcel set attmod_areahex.xlsx, modify sheet(v)
	putexcel A1 = matrix(v), rownames
	
**# Complete hex*area, area
	
	clogit choice c.travelcost c.depth area hex_area km_mainland fisl_adj fshelter, group(tripid)
	estimates store attmod_area_areahex
	
	* store variance-covariance matrix 

	matrix b = e(b)'
	matrix v = e(V)

	* store outputs 
	* define directory  to store

	cd "/Users/23088313/Documents/git_repos/hamre_MarmionRUM/data/03_data"

	putexcel set attmod_area_areahex.xlsx, replace
	putexcel A2 = matrix(b), rownames
	putexcel B1 = "Vars"
	putexcel C1 = "Coef"
	putexcel set attmod_area_areahex.xlsx, modify sheet(v)
	putexcel A2 = matrix(v), rownames
	putexcel set attmod_area_areahex.xlsx, modify sheet(v)
	putexcel A1 = matrix(v), rownames
	
**# shex
	
	clogit choice c.travelcost c.depth shex km_mainland fisl_adj fshelter, group(tripid)
	estimates store attmod_shex
	
	* store variance-covariance matrix 

	matrix b = e(b)'
	matrix v = e(V)

	* store outputs 
	* define directory  to store

	cd "/Users/23088313/Documents/git_repos/hamre_MarmionRUM/data/03_data"

	putexcel set attmod_shex.xlsx, replace
	putexcel A2 = matrix(b), rownames
	putexcel B1 = "Vars"
	putexcel C1 = "Coef"
	putexcel set attmod_shex.xlsx, modify sheet(v)
	putexcel A2 = matrix(v), rownames
	putexcel set attmod_shex.xlsx, modify sheet(v)
	putexcel A1 = matrix(v), rownames
	
**# shex, arealog
	
	clogit choice c.travelcost c.depth arealog shex km_mainland fisl_adj fshelter, group(tripid)
	estimates store attmod_arealog_shex
	
	* store variance-covariance matrix 

	matrix b = e(b)'
	matrix v = e(V)

	* store outputs 
	* define directory  to store

	cd "/Users/23088313/Documents/git_repos/hamre_MarmionRUM/data/03_data"

	putexcel set attmod_arealog_shex.xlsx, replace
	putexcel A2 = matrix(b), rownames
	putexcel B1 = "Vars"
	putexcel C1 = "Coef"
	putexcel set attmod_arealog_shex.xlsx, modify sheet(v)
	putexcel A2 = matrix(v), rownames
	putexcel set attmod_arealog_shex.xlsx, modify sheet(v)
	putexcel A1 = matrix(v), rownames
	
**# shex, area
	
	clogit choice c.travelcost c.depth area shex km_mainland fisl_adj fshelter, group(tripid)
	estimates store attmod_area_shex
	
	* store variance-covariance matrix 

	matrix b = e(b)'
	matrix v = e(V)

	* store outputs 
	* define directory  to store

	cd "/Users/23088313/Documents/git_repos/hamre_MarmionRUM/data/03_data"

	putexcel set attmod_area_shex.xlsx, replace
	putexcel A2 = matrix(b), rownames
	putexcel B1 = "Vars"
	putexcel C1 = "Coef"
	putexcel set attmod_area_shex.xlsx, modify sheet(v)
	putexcel A2 = matrix(v), rownames
	putexcel set attmod_area_shex.xlsx, modify sheet(v)
	putexcel A1 = matrix(v), rownames
	
	
**# shex*arealog
	
	clogit choice c.travelcost c.depth shex_arealog km_mainland fisl_adj fshelter, group(tripid)
	estimates store attmod_shex_arealog
	
	* store variance-covariance matrix 

	matrix b = e(b)'
	matrix v = e(V)

	* store outputs 
	* define directory  to store

	cd "/Users/23088313/Documents/git_repos/hamre_MarmionRUM/data/03_data"

	putexcel set attmod_shex_arealog.xlsx, replace
	putexcel A2 = matrix(b), rownames
	putexcel B1 = "Vars"
	putexcel C1 = "Coef"
	putexcel set attmod_shex_arealog.xlsx, modify sheet(v)
	putexcel A2 = matrix(v), rownames
	putexcel set attmod_shex_arealog.xlsx, modify sheet(v)
	putexcel A1 = matrix(v), rownames
	
**# base model 5km
	
	clogit choice c.travelcost c.depth arealog km_mainland fisl_adj, group(tripid)
	estimates store attmod_base5km
	
	* store variance-covariance matrix 

	matrix b = e(b)'
	matrix v = e(V)

	* store outputs 
	* define directory  to store

	cd "/Users/23088313/Documents/git_repos/hamre_MarmionRUM/data/03_data"

	putexcel set attmod_5kmbase.xlsx, replace
	putexcel A2 = matrix(b), rownames
	putexcel B1 = "Vars"
	putexcel C1 = "Coef"
	putexcel set attmod_5kmbase.xlsx, modify sheet(v)
	putexcel A2 = matrix(v), rownames
	putexcel set attmod_5kmbase.xlsx, modify sheet(v)
	putexcel A1 = matrix(v), rownames
	* works but is verrry different to working 1km and asc
	* --> like for like contrast between 1 km and 5km
	
**# 5km complete hex
	
	clogit choice c.travelcost c.depth commhex km_mainland fisl_adj, group(tripid)
	estimates store attmod_5km_commhex
	
	* store variance-covariance matrix 

	matrix b = e(b)'
	matrix v = e(V)

	* store outputs 
	* define directory  to store

	cd "/Users/23088313/Documents/git_repos/hamre_MarmionRUM/data/03_data"

	putexcel set attmod_5km_commhex.xlsx, replace
	putexcel A2 = matrix(b), rownames
	putexcel B1 = "Vars"
	putexcel C1 = "Coef"
	putexcel set attmod_5km_commhex.xlsx, modify sheet(v)
	putexcel A2 = matrix(v), rownames
	putexcel set attmod_5km_commhex.xlsx, modify sheet(v)
	putexcel A1 = matrix(v), rownames
	* works but is verrry different to working 1km and asc
	* --> like for like contrast between 1 km and 5km	

	
	
	
**# 1km no shelter
	
	clogit choice c.travelcost c.depth commhex km_mainland fisl_adj, group(tripid)
	estimates store attmod_1km_noshelter
	
	* store variance-covariance matrix 

	matrix b = e(b)'
	matrix v = e(V)

	* store outputs 
	* define directory  to store

	cd "/Users/23088313/Documents/git_repos/hamre_MarmionRUM/data/03_data"

	putexcel set attmod_1km_noshelter.xlsx, replace
	putexcel A2 = matrix(b), rownames
	putexcel B1 = "Vars"
	putexcel C1 = "Coef"
	putexcel set attmod_1km_noshelter.xlsx, modify sheet(v)
	putexcel A2 = matrix(v), rownames
	putexcel set attmod_1km_noshelter.xlsx, modify sheet(v)
	putexcel A1 = matrix(v), rownames
	* works but is verrry different to working 1km and asc
	* --> like for like contrast between 1 km and 5km
	
**# 1km, log(area+1), no shelter
	
	clogit choice c.travelcost c.depth arealog km_mainland fisl_adj, group(tripid)
	estimates store attmod_1kmlogarea_noshelter
	
	* store variance-covariance matrix 

	matrix b = e(b)'
	matrix v = e(V)

	* store outputs 
	* define directory  to store

	cd "/Users/23088313/Documents/git_repos/hamre_MarmionRUM/data/03_data"

	putexcel set attmod_1kmlogarea_noshelter.xlsx, replace
	putexcel A2 = matrix(b), rownames
	putexcel B1 = "Vars"
	putexcel C1 = "Coef"
	putexcel set attmod_1kmlogarea_noshelter.xlsx, modify sheet(v)
	putexcel A2 = matrix(v), rownames
	putexcel set attmod_1kmlogarea_noshelter.xlsx, modify sheet(v)
	putexcel A1 = matrix(v), rownames
	
	
	
	**# Complete hex demersal
	
	clogit choice c.travelcost c.depth commhex km_mainland fisl_adj fshelter, group(tripid)
	estimates store attmod_comhex_dem
	
	* store variance-covariance matrix 

	matrix b = e(b)'
	matrix v = e(V)

	* store outputs 
	* define directory  to store

	cd "/Users/23088313/Documents/git_repos/hamre_MarmionRUM/data/03_data"

	putexcel set attmod_comhex_dem.xlsx, replace
	putexcel A2 = matrix(b), rownames
	putexcel B1 = "Vars"
	putexcel C1 = "Coef"
	putexcel set attmod_comhex_dem.xlsx, modify sheet(v)
	putexcel A2 = matrix(v), rownames
	putexcel set attmod_comhex_dem.xlsx, modify sheet(v)
	putexcel A1 = matrix(v), rownames
**********
* line_ma - test interactions example
	* clogit choice c.travelcost c.depth c.depthsquared area i.fisl_adj line_ma, group(tripid)
	* estimates store mod1
	
* get AIC for each

	estimate stats attmod_comhex attmod_arealoghex attmod_areahex
	
	
* store variance-covariance matrix 

	matrix b = e(b)'
	matrix v = e(V)

**# store outputs 

	* define directory  to store

	cd "/Users/23088313/Documents/git_repos/hamre_MarmionRUM/data/03_data"

	putexcel set att_mod.xlsx, replace
	putexcel A2 = matrix(b), rownames
	putexcel B1 = "Vars"
	putexcel C1 = "Coef"
	putexcel set att_mod.xlsx, modify sheet(v)
	putexcel A2 = matrix(v), rownames
	putexcel set att_mod.xlsx, modify sheet(v)
	putexcel A1 = matrix(v), rownames
