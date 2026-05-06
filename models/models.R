# Model 0: Not Allowing correlated structures ----------------------------------------------------
models.cat0 <- list()

# total effect
models.cat0[["total_effect"]] <- '
  Y ~ te1*A1+te2*A2+te3*A3+sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
  sex ~~ 0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  edu ~~ 0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  age ~~ 0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  contract ~~ 0*chronic_ill+0*func_lim+0*social
  chronic_ill~~0*func_lim+0*social
  func_lim~~0*social
  A1 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A2 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A3 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  
  A1 ~~   0*A2 + 0*A3
  A2 ~~   0*A3
'
# parallel path model
models.cat0[["noMcovary"]] <- '
	Y ~  bA1*A1 + bA2*A2 + bA3*A3 + b1*M1 + b2*M2 + b3*M3 + b4*M4 + b5*M5 + b6*M6 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M1 ~ d1a*A1 + d1b*A2+ d1c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M2 ~ d2a*A1 + d2b*A2+ d2c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social 
	M3 ~ d3a*A1 + d3b*A2+ d3c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M4 ~ d4a*A1 + d4b*A2+ d4c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M5 ~ d5a*A1 + d5b*A2+ d5c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M6 ~ d6a*A1 + d6b*A2+ d6c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	
	# fix covariates
	sex ~~ 0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  edu ~~ 0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  age ~~ 0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  contract ~~ 0*chronic_ill+0*func_lim+0*social
  chronic_ill~~0*func_lim+0*social
  func_lim~~0*social
  A1 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A2 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A3 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  
  A1 ~~   0*A2 + 0*A3
  A2 ~~   0*A3
  
	# define interventional indirect effects via each mediator: monthly
	ie1M := b1*d1a
	ie2M := b2*d2a
	ie3M := b3*d3a
	ie4M := b4*d4a
	ie5M := b5*d5a
	ie6M := b6*d6a
	
	#weekly
	ie1W := b1*d1b
	ie2W := b2*d2b
	ie3W := b3*d3b
	ie4W := b4*d4b
	ie5W := b5*d5b
	ie6W := b6*d6b
	
	#full-time
	ie1d := b1*d1c
	ie2d := b2*d2c
	ie3d := b3*d3c
	ie4d := b4*d4c
	ie5d := b5*d5c
	ie6d := b6*d6c
	
  #Monthly: direct and joint indirect effects, and their sum
	de_A1 := bA1
	ie_jt_M := ie1M + ie2M + ie3M + ie4M + ie5M + ie6M
	de_ie_sum_M := de_A1 + ie_jt_M
	
	#Weekly: define direct and joint indirect effects, and their sum
	de_A2 := bA2
	ie_jt_W := ie1W + ie2W + ie3W + ie4W + ie5W + ie6W
	de_ie_sum_W := de_A2 + ie_jt_W
	
	#Weekly: define direct and joint indirect effects, and their sum
	de_A3 := bA3
	ie_jt_d := ie1d + ie2d + ie3d + ie4d + ie5d + ie6d
	de_ie_sum_d := de_A3 + ie_jt_d
	
	'	
# parallel path model but permit mediators to covary
models.cat0[["Mcovary"]] <- paste(
  models.cat0[["noMcovary"]],
  "M1 ~~ M2 + M3 + M4 + M5 + M6",
  "M2 ~~ M3 + M4 + M5 + M6",
  "M3 ~~ M4 + M5 + M6",
  "M4 ~~ M5 + M6",
  "M5 ~~ M6",
  sep=" \n ")

# Model 1: Allowing all correlated structures ----------------------------------------------------
models.cat1 <- list()

# total effect
models.cat1[["total_effect"]] <- '
  Y ~ te1*A1+te2*A2+te3*A3+sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
  sex ~~ edu+age+work_skill+contract+chronic_ill+func_lim+social
  edu ~~ age+work_skill+contract+chronic_ill+func_lim+social
  age ~~ work_skill+contract+chronic_ill+func_lim+social
  contract ~~ chronic_ill+func_lim+social
  chronic_ill~~func_lim+social
  func_lim~~social
  A1 ~~   sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
  A2 ~~   sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
  A3 ~~   sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
  
  A1 ~~   0*A2 + 0*A3
  A2 ~~   0*A3
  '
# parallel path model
models.cat1[["noMcovary"]] <- '
	Y ~  bA1*A1 + bA2*A2 + bA3*A3 + b1*M1 + b2*M2 + b3*M3 + b4*M4 + b5*M5 + b6*M6 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M1 ~ d1a*A1 + d1b*A2+ d1c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M2 ~ d2a*A1 + d2b*A2+ d2c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social 
	M3 ~ d3a*A1 + d3b*A2+ d3c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M4 ~ d4a*A1 + d4b*A2+ d4c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M5 ~ d5a*A1 + d5b*A2+ d5c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M6 ~ d6a*A1 + d6b*A2+ d6c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	
	# fix covariates
	sex ~~ edu+age+work_skill+contract+chronic_ill+func_lim+social
  edu ~~ age+work_skill+contract+chronic_ill+func_lim+social
  age ~~ work_skill+contract+chronic_ill+func_lim+social
  contract ~~ chronic_ill+func_lim+social
  chronic_ill~~func_lim+social
  func_lim~~social
  A1 ~~   sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
  A2 ~~   sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
  A3 ~~   sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
  
  A1 ~~   0*A2 + 0*A3
  A2 ~~   0*A3
	# define interventional indirect effects via each mediator: monthly
	ie1M := b1*d1a
	ie2M := b2*d2a
	ie3M := b3*d3a
	ie4M := b4*d4a
	ie5M := b5*d5a
	ie6M := b6*d6a
	
	#weekly
	ie1W := b1*d1b
	ie2W := b2*d2b
	ie3W := b3*d3b
	ie4W := b4*d4b
	ie5W := b5*d5b
	ie6W := b6*d6b
	
	#full-time
	ie1d := b1*d1c
	ie2d := b2*d2c
	ie3d := b3*d3c
	ie4d := b4*d4c
	ie5d := b5*d5c
	ie6d := b6*d6c
	
  #Monthly: direct and joint indirect effects, and their sum
	de_A1 := bA1
	ie_jt_M := ie1M + ie2M + ie3M + ie4M + ie5M + ie6M
	de_ie_sum_M := de_A1 + ie_jt_M
	
	#Weekly: define direct and joint indirect effects, and their sum
	de_A2 := bA2
	ie_jt_W := ie1W + ie2W + ie3W + ie4W + ie5W + ie6W
	de_ie_sum_W := de_A2 + ie_jt_W
	
	#Weekly: define direct and joint indirect effects, and their sum
	de_A3 := bA3
	ie_jt_d := ie1d + ie2d + ie3d + ie4d + ie5d + ie6d
	de_ie_sum_d := de_A3 + ie_jt_d
	
	'	
# parallel path model but permit mediators to covary
models.cat1[["Mcovary"]] <- paste(
  models.cat1[["noMcovary"]],
  "M1 ~~ M2 + M3 + M4 + M5 + M6",
  "M2 ~~ M3 + M4 + M5 + M6",
  "M3 ~~ M4 + M5 + M6",
  "M4 ~~ M5 + M6",
  "M5 ~~ M6",
  sep=" \n ")
# Model 2: Data-driven & theoretical model ----------------------------------------------------
##add chronic_ill ~ work_skill 
##edu ~ age + work_skill
##age ~ work_skill
# model fit departing from m0
models.cat2 <- list()

# total effect
models.cat2[["total_effect"]] <- '
  Y ~ te1*A1+te2*A2+te3*A3+sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
  sex ~~ 0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  edu ~~ age+work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  age ~~ work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  contract ~~ 0*chronic_ill+0*func_lim+0*social
  chronic_ill~~func_lim+0*social
  func_lim~~0*social
  A1 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A2 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A3 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  
  A1 ~~   0*A2 + 0*A3
  A2 ~~   0*A3
  
'
# parallel path model
models.cat2[["noMcovary"]] <- '
	Y ~  bA1*A1 + bA2*A2 + bA3*A3 + b1*M1 + b2*M2 + b3*M3 + b4*M4 + b5*M5 + b6*M6 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M1 ~ d1a*A1 + d1b*A2+ d1c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M2 ~ d2a*A1 + d2b*A2+ d2c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M3 ~ d3a*A1 + d3b*A2+ d3c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M4 ~ d4a*A1 + d4b*A2+ d4c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M5 ~ d5a*A1 + d5b*A2+ d5c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M6 ~ d6a*A1 + d6b*A2+ d6c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social

	# fix covariates
	sex ~~ 0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  edu ~~ age+work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  age ~~ work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  contract ~~ 0*chronic_ill+0*func_lim+0*social
  chronic_ill~~func_lim+0*social
  func_lim~~0*social
  A1 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A2 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A3 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  
  A1 ~~   0*A2 + 0*A3
  A2 ~~   0*A3
	# define interventional indirect effects via each mediator: monthly
	ie1M := b1*d1a
	ie2M := b2*d2a
	ie3M := b3*d3a
	ie4M := b4*d4a
	ie5M := b5*d5a
	ie6M := b6*d6a

	#weekly
	ie1W := b1*d1b
	ie2W := b2*d2b
	ie3W := b3*d3b
	ie4W := b4*d4b
	ie5W := b5*d5b
	ie6W := b6*d6b

	#full-time
	ie1d := b1*d1c
	ie2d := b2*d2c
	ie3d := b3*d3c
	ie4d := b4*d4c
	ie5d := b5*d5c
	ie6d := b6*d6c

  #Monthly: direct and joint indirect effects, and their sum
	de_A1 := bA1
	ie_jt_M := ie1M + ie2M + ie3M + ie4M + ie5M + ie6M
	de_ie_sum_M := de_A1 + ie_jt_M

	#Weekly: define direct and joint indirect effects, and their sum
	de_A2 := bA2
	ie_jt_W := ie1W + ie2W + ie3W + ie4W + ie5W + ie6W
	de_ie_sum_W := de_A2 + ie_jt_W

	#Weekly: define direct and joint indirect effects, and their sum
	de_A3 := bA3
	ie_jt_d := ie1d + ie2d + ie3d + ie4d + ie5d + ie6d
	de_ie_sum_d := de_A3 + ie_jt_d

	'
# parallel path model but permit mediators to covary
models.cat2[["Mcovary"]] <- paste(
  models.cat2[["noMcovary"]],
  "M1 ~~ M2 + M3 + M4 + M5 + M6",
  "M2 ~~ M3 + M4 + M5 + M6",
  "M3 ~~ M4 + M5 + M6",
  "M4 ~~ M5 + M6",
  "M5 ~~ M6",
  sep=" \n ")


# Model 3: Data-driven & theoretical model ----------------------------------------------------
##add chronic_ill ~ work_skill 
##edu ~ age + work_skill
##age ~ work_skill
##add as well covariances between exposures
# model fit departing from m0
models.cat3 <- list()

# total effect
models.cat3[["total_effect"]] <- '
  Y ~ te1*A1+te2*A2+te3*A3+sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
  sex ~~ 0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  edu ~~ age+work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  age ~~ work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  contract ~~ 0*chronic_ill+0*func_lim+0*social
  chronic_ill~~func_lim+0*social
  func_lim~~0*social
  
  A1 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A2 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A3 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A1 ~~   A2 + A3
  A2 ~~   A3
'
# parallel path model
models.cat3[["noMcovary"]] <- '
	Y ~  bA1*A1 + bA2*A2 + bA3*A3 + b1*M1 + b2*M2 + b3*M3 + b4*M4 + b5*M5 + b6*M6 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M1 ~ d1a*A1 + d1b*A2+ d1c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M2 ~ d2a*A1 + d2b*A2+ d2c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M3 ~ d3a*A1 + d3b*A2+ d3c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M4 ~ d4a*A1 + d4b*A2+ d4c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M5 ~ d5a*A1 + d5b*A2+ d5c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M6 ~ d6a*A1 + d6b*A2+ d6c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social

	# fix covariates
	sex ~~ 0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  edu ~~ age+work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  age ~~ work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  contract ~~ 0*chronic_ill+0*func_lim+0*social
  chronic_ill~~func_lim+0*social
  func_lim~~0*social
  A1 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A2 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A3 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  
  A1 ~~   A2 + A3
  A2 ~~   A3

	# define interventional indirect effects via each mediator: monthly
	ie1M := b1*d1a
	ie2M := b2*d2a
	ie3M := b3*d3a
	ie4M := b4*d4a
	ie5M := b5*d5a
	ie6M := b6*d6a

	#weekly
	ie1W := b1*d1b
	ie2W := b2*d2b
	ie3W := b3*d3b
	ie4W := b4*d4b
	ie5W := b5*d5b
	ie6W := b6*d6b

	#full-time
	ie1d := b1*d1c
	ie2d := b2*d2c
	ie3d := b3*d3c
	ie4d := b4*d4c
	ie5d := b5*d5c
	ie6d := b6*d6c

  #Monthly: direct and joint indirect effects, and their sum
	de_A1 := bA1
	ie_jt_M := ie1M + ie2M + ie3M + ie4M + ie5M + ie6M
	de_ie_sum_M := de_A1 + ie_jt_M

	#Weekly: define direct and joint indirect effects, and their sum
	de_A2 := bA2
	ie_jt_W := ie1W + ie2W + ie3W + ie4W + ie5W + ie6W
	de_ie_sum_W := de_A2 + ie_jt_W

	#Weekly: define direct and joint indirect effects, and their sum
	de_A3 := bA3
	ie_jt_d := ie1d + ie2d + ie3d + ie4d + ie5d + ie6d
	de_ie_sum_d := de_A3 + ie_jt_d

	'
# parallel path model but permit mediators to covary
models.cat3[["Mcovary"]] <- paste(
  models.cat3[["noMcovary"]],
  "M1 ~~ M2 + M3 + M4 + M5 + M6",
  "M2 ~~ M3 + M4 + M5 + M6",
  "M3 ~~ M4 + M5 + M6",
  "M4 ~~ M5 + M6",
  "M5 ~~ M6",
  sep=" \n ")


# Model 4: Data-driven & theoretical model ----------------------------------------------------
##add as well covariances between exposures
##other covariances as null
# model fit departing from m0
models.cat4 <- list()

# total effect
models.cat4[["total_effect"]] <- '
  Y ~ te1*A1+te2*A2+te3*A3+sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
  sex ~~ 0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  edu ~~ 0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  age ~~ 0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  contract ~~ 0*chronic_ill+0*func_lim+0*social
  chronic_ill~~0*func_lim+0*social
  func_lim~~0*social
  
  A1 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A2 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A3 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A1 ~~   A2 + A3
  A2 ~~   A3
'
# parallel path model
models.cat4[["noMcovary"]] <- '
	Y ~  bA1*A1 + bA2*A2 + bA3*A3 + b1*M1 + b2*M2 + b3*M3 + b4*M4 + b5*M5 + b6*M6 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M1 ~ d1a*A1 + d1b*A2+ d1c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M2 ~ d2a*A1 + d2b*A2+ d2c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M3 ~ d3a*A1 + d3b*A2+ d3c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M4 ~ d4a*A1 + d4b*A2+ d4c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M5 ~ d5a*A1 + d5b*A2+ d5c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M6 ~ d6a*A1 + d6b*A2+ d6c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social

	# fix covariates
	sex ~~ 0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  edu ~~ 0*age+work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  age ~~ 0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  contract ~~ 0*chronic_ill+0*func_lim+0*social
  chronic_ill~~0*func_lim+0*social
  func_lim~~0*social
  A1 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A2 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A3 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  
  A1 ~~   A2 + A3
  A2 ~~   A3

	# define interventional indirect effects via each mediator: monthly
	ie1M := b1*d1a
	ie2M := b2*d2a
	ie3M := b3*d3a
	ie4M := b4*d4a
	ie5M := b5*d5a
	ie6M := b6*d6a

	#weekly
	ie1W := b1*d1b
	ie2W := b2*d2b
	ie3W := b3*d3b
	ie4W := b4*d4b
	ie5W := b5*d5b
	ie6W := b6*d6b

	#full-time
	ie1d := b1*d1c
	ie2d := b2*d2c
	ie3d := b3*d3c
	ie4d := b4*d4c
	ie5d := b5*d5c
	ie6d := b6*d6c

  #Monthly: direct and joint indirect effects, and their sum
	de_A1 := bA1
	ie_jt_M := ie1M + ie2M + ie3M + ie4M + ie5M + ie6M
	de_ie_sum_M := de_A1 + ie_jt_M

	#Weekly: define direct and joint indirect effects, and their sum
	de_A2 := bA2
	ie_jt_W := ie1W + ie2W + ie3W + ie4W + ie5W + ie6W
	de_ie_sum_W := de_A2 + ie_jt_W

	#Weekly: define direct and joint indirect effects, and their sum
	de_A3 := bA3
	ie_jt_d := ie1d + ie2d + ie3d + ie4d + ie5d + ie6d
	de_ie_sum_d := de_A3 + ie_jt_d

	'
# parallel path model but permit mediators to covary
models.cat4[["Mcovary"]] <- paste(
  models.cat4[["noMcovary"]],
  "M1 ~~ M2 + M3 + M4 + M5 + M6",
  "M2 ~~ M3 + M4 + M5 + M6",
  "M3 ~~ M4 + M5 + M6",
  "M4 ~~ M5 + M6",
  "M5 ~~ M6",
  sep=" \n ")



# Model 5: Data-driven & theoretical model ----------------------------------------------------
##add chronic_ill ~ work_skill 
##edu ~~ age + work_skill
##age ~~ work_skill
##add as well covariances between exposures

##age ~~ contract
##func_lim ~~ social
# model fit departing from m0
models.cat5 <- list()

# total effect
models.cat5[["total_effect"]] <- '
  Y ~ te1*A1+te2*A2+te3*A3+sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
  sex ~~ 0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  edu ~~ age+work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  age ~~ work_skill+contract+0*chronic_ill+0*func_lim+0*social
  contract ~~ 0*chronic_ill+0*func_lim+0*social
  chronic_ill~~func_lim+0*social
  func_lim~~social
  
  A1 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A2 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A3 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A1 ~~   A2 + A3
  A2 ~~   A3
'
# parallel path model
models.cat5[["noMcovary"]] <- '
	Y ~  bA1*A1 + bA2*A2 + bA3*A3 + b1*M1 + b2*M2 + b3*M3 + b4*M4 + b5*M5 + b6*M6 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M1 ~ d1a*A1 + d1b*A2+ d1c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M2 ~ d2a*A1 + d2b*A2+ d2c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M3 ~ d3a*A1 + d3b*A2+ d3c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M4 ~ d4a*A1 + d4b*A2+ d4c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M5 ~ d5a*A1 + d5b*A2+ d5c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M6 ~ d6a*A1 + d6b*A2+ d6c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social

	# fix covariates
  sex ~~ 0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  edu ~~ age+work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  age ~~ work_skill+contract+0*chronic_ill+0*func_lim+0*social
  contract ~~ 0*chronic_ill+0*func_lim+0*social
  chronic_ill~~func_lim+0*social
  func_lim~~social
  
  A1 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A2 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A3 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A1 ~~   A2 + A3
  A2 ~~   A3

	# define interventional indirect effects via each mediator: monthly
	ie1M := b1*d1a
	ie2M := b2*d2a
	ie3M := b3*d3a
	ie4M := b4*d4a
	ie5M := b5*d5a
	ie6M := b6*d6a

	#weekly
	ie1W := b1*d1b
	ie2W := b2*d2b
	ie3W := b3*d3b
	ie4W := b4*d4b
	ie5W := b5*d5b
	ie6W := b6*d6b

	#full-time
	ie1d := b1*d1c
	ie2d := b2*d2c
	ie3d := b3*d3c
	ie4d := b4*d4c
	ie5d := b5*d5c
	ie6d := b6*d6c

  #Monthly: direct and joint indirect effects, and their sum
	de_A1 := bA1
	ie_jt_M := ie1M + ie2M + ie3M + ie4M + ie5M + ie6M
	de_ie_sum_M := de_A1 + ie_jt_M

	#Weekly: define direct and joint indirect effects, and their sum
	de_A2 := bA2
	ie_jt_W := ie1W + ie2W + ie3W + ie4W + ie5W + ie6W
	de_ie_sum_W := de_A2 + ie_jt_W

	#Weekly: define direct and joint indirect effects, and their sum
	de_A3 := bA3
	ie_jt_d := ie1d + ie2d + ie3d + ie4d + ie5d + ie6d
	de_ie_sum_d := de_A3 + ie_jt_d

	'
# parallel path model but permit mediators to covary
models.cat5[["Mcovary"]] <- paste(
  models.cat5[["noMcovary"]],
  "M1 ~~ M2 + M3 + M4 + M5 + M6",
  "M2 ~~ M3 + M4 + M5 + M6",
  "M3 ~~ M4 + M5 + M6",
  "M4 ~~ M5 + M6",
  "M5 ~~ M6",
  sep=" \n ")



# Model 6: Data-driven & theoretical model ----------------------------------------------------
##add chronic_ill ~ work_skill 
##edu ~~ age + work_skill
##age ~~ work_skill
##add as well covariances between exposures

##age ~~ contract
##func_lim ~~ social
##
##sex ~~ edu
##chronic_ill ~~ social
##sex ~~ work_skill
# model fit departing from m0
models.cat6 <- list()

# total effect
models.cat6[["total_effect"]] <- '
  Y ~ te1*A1+te2*A2+te3*A3+sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
  sex ~~ edu+0*age+work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  edu ~~ age+work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  age ~~ work_skill+contract+0*chronic_ill+0*func_lim+0*social
  contract ~~ 0*chronic_ill+0*func_lim+0*social
  chronic_ill~~func_lim+social
  func_lim~~social
  
  A1 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A2 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A3 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A1 ~~   A2 + A3
  A2 ~~   A3
'
# parallel path model
models.cat6[["noMcovary"]] <- '
	Y ~  bA1*A1 + bA2*A2 + bA3*A3 + b1*M1 + b2*M2 + b3*M3 + b4*M4 + b5*M5 + b6*M6 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M1 ~ d1a*A1 + d1b*A2+ d1c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M2 ~ d2a*A1 + d2b*A2+ d2c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M3 ~ d3a*A1 + d3b*A2+ d3c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M4 ~ d4a*A1 + d4b*A2+ d4c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M5 ~ d5a*A1 + d5b*A2+ d5c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M6 ~ d6a*A1 + d6b*A2+ d6c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social

	# fix covariates
  sex ~~ edu+0*age+work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  edu ~~ age+work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  age ~~ work_skill+contract+0*chronic_ill+0*func_lim+0*social
  contract ~~ 0*chronic_ill+0*func_lim+0*social
  chronic_ill~~func_lim+social
  func_lim~~social
  
  A1 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A2 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A3 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A1 ~~   A2 + A3
  A2 ~~   A3

	# define interventional indirect effects via each mediator: monthly
	ie1M := b1*d1a
	ie2M := b2*d2a
	ie3M := b3*d3a
	ie4M := b4*d4a
	ie5M := b5*d5a
	ie6M := b6*d6a

	#weekly
	ie1W := b1*d1b
	ie2W := b2*d2b
	ie3W := b3*d3b
	ie4W := b4*d4b
	ie5W := b5*d5b
	ie6W := b6*d6b

	#full-time
	ie1d := b1*d1c
	ie2d := b2*d2c
	ie3d := b3*d3c
	ie4d := b4*d4c
	ie5d := b5*d5c
	ie6d := b6*d6c

  #Monthly: direct and joint indirect effects, and their sum
	de_A1 := bA1
	ie_jt_M := ie1M + ie2M + ie3M + ie4M + ie5M + ie6M
	de_ie_sum_M := de_A1 + ie_jt_M

	#Weekly: define direct and joint indirect effects, and their sum
	de_A2 := bA2
	ie_jt_W := ie1W + ie2W + ie3W + ie4W + ie5W + ie6W
	de_ie_sum_W := de_A2 + ie_jt_W

	#Weekly: define direct and joint indirect effects, and their sum
	de_A3 := bA3
	ie_jt_d := ie1d + ie2d + ie3d + ie4d + ie5d + ie6d
	de_ie_sum_d := de_A3 + ie_jt_d

	'
# parallel path model but permit mediators to covary
models.cat6[["Mcovary"]] <- paste(
  models.cat6[["noMcovary"]],
  "M1 ~~ M2 + M3 + M4 + M5 + M6",
  "M2 ~~ M3 + M4 + M5 + M6",
  "M3 ~~ M4 + M5 + M6",
  "M4 ~~ M5 + M6",
  "M5 ~~ M6",
  sep=" \n ")



# Model 7: Data-driven & theoretical model ----------------------------------------------------
##add chronic_ill ~ work_skill 
##edu ~~ age + work_skill
##age ~~ work_skill
##add as well covariances between exposures

##age ~~ contract
##func_lim ~~ social
##
##sex ~~ edu
##chronic_ill ~~ social
##sex ~~ work_skill

##age~~chronic_ill
##work_skill~~ 0*contract+chronic_ill+0*func_lim+social
# model fit departing from m0
models.cat7 <- list()

# total effect
models.cat7[["total_effect"]] <- '
  Y ~ te1*A1+te2*A2+te3*A3+sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
  sex ~~ edu+0*age+work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  edu ~~ age+work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  age ~~ work_skill+contract+chronic_ill+0*func_lim+0*social
  work_skill~~ 0*contract+chronic_ill+0*func_lim+social
  contract ~~ 0*chronic_ill+0*func_lim+0*social
  chronic_ill~~func_lim+social
  func_lim~~social
  
  A1 ~~   0*sex+0*edu+0*age+work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A2 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A3 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A1 ~~   A2 + A3
  A2 ~~   A3
'
# parallel path model
models.cat7[["noMcovary"]] <- '
	Y ~  bA1*A1 + bA2*A2 + bA3*A3 + b1*M1 + b2*M2 + b3*M3 + b4*M4 + b5*M5 + b6*M6 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M1 ~ d1a*A1 + d1b*A2+ d1c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M2 ~ d2a*A1 + d2b*A2+ d2c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M3 ~ d3a*A1 + d3b*A2+ d3c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M4 ~ d4a*A1 + d4b*A2+ d4c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M5 ~ d5a*A1 + d5b*A2+ d5c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M6 ~ d6a*A1 + d6b*A2+ d6c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social

	# fix covariates
  sex ~~ edu+0*age+work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  edu ~~ age+work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  age ~~ work_skill+contract+chronic_ill+0*func_lim+0*social
  work_skill~~ 0*contract+chronic_ill+0*func_lim+social
  contract ~~ 0*chronic_ill+0*func_lim+0*social
  chronic_ill~~func_lim+social
  func_lim~~social
  
  A1 ~~   0*sex+0*edu+0*age+work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A2 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A3 ~~   0*sex+0*edu+0*age+0*work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A1 ~~   A2 + A3
  A2 ~~   A3

	# define interventional indirect effects via each mediator: monthly
	ie1M := b1*d1a
	ie2M := b2*d2a
	ie3M := b3*d3a
	ie4M := b4*d4a
	ie5M := b5*d5a
	ie6M := b6*d6a

	#weekly
	ie1W := b1*d1b
	ie2W := b2*d2b
	ie3W := b3*d3b
	ie4W := b4*d4b
	ie5W := b5*d5b
	ie6W := b6*d6b

	#full-time
	ie1d := b1*d1c
	ie2d := b2*d2c
	ie3d := b3*d3c
	ie4d := b4*d4c
	ie5d := b5*d5c
	ie6d := b6*d6c

  #Monthly: direct and joint indirect effects, and their sum
	de_A1 := bA1
	ie_jt_M := ie1M + ie2M + ie3M + ie4M + ie5M + ie6M
	de_ie_sum_M := de_A1 + ie_jt_M

	#Weekly: define direct and joint indirect effects, and their sum
	de_A2 := bA2
	ie_jt_W := ie1W + ie2W + ie3W + ie4W + ie5W + ie6W
	de_ie_sum_W := de_A2 + ie_jt_W

	#Weekly: define direct and joint indirect effects, and their sum
	de_A3 := bA3
	ie_jt_d := ie1d + ie2d + ie3d + ie4d + ie5d + ie6d
	de_ie_sum_d := de_A3 + ie_jt_d

	'
# parallel path model but permit mediators to covary
models.cat7[["Mcovary"]] <- paste(
  models.cat7[["noMcovary"]],
  "M1 ~~ M2 + M3 + M4 + M5 + M6",
  "M2 ~~ M3 + M4 + M5 + M6",
  "M3 ~~ M4 + M5 + M6",
  "M4 ~~ M5 + M6",
  "M5 ~~ M6",
  sep=" \n ")
# Model 8: Data-driven & theoretical model ----------------------------------------------------
##add chronic_ill ~ work_skill 
##edu ~~ age + work_skill
##age ~~ work_skill
##add as well covariances between exposures

##age ~~ contract
##func_lim ~~ social
##
##sex ~~ edu
##chronic_ill ~~ social
##sex ~~ work_skill

##age~~chronic_ill
##work_skill~~ 0*contract+chronic_ill+0*func_lim+social
# model fit departing from m0
models.cat8 <- list()

# total effect
models.cat8[["total_effect"]] <- '
  Y ~ te1*A1+te2*A2+te3*A3+sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
  sex ~~ edu+0*age+work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  edu ~~ age+work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  age ~~ work_skill+contract+chronic_ill+0*func_lim+0*social
  work_skill~~ 0*contract+chronic_ill+0*func_lim+social
  contract ~~ 0*chronic_ill+0*func_lim+0*social
  chronic_ill~~func_lim+social
  func_lim~~social
  
  A1 ~~   0*sex+edu+0*age+work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A2 ~~   0*sex+edu+0*age+work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A3 ~~   0*sex+edu+0*age+work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A1 ~~   A2 + A3
  A2 ~~   A3
'
# parallel path model
models.cat8[["noMcovary"]] <- '
	Y ~  bA1*A1 + bA2*A2 + bA3*A3 + b1*M1 + b2*M2 + b3*M3 + b4*M4 + b5*M5 + b6*M6 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M1 ~ d1a*A1 + d1b*A2+ d1c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M2 ~ d2a*A1 + d2b*A2+ d2c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M3 ~ d3a*A1 + d3b*A2+ d3c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M4 ~ d4a*A1 + d4b*A2+ d4c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M5 ~ d5a*A1 + d5b*A2+ d5c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M6 ~ d6a*A1 + d6b*A2+ d6c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social

	# fix covariates
    sex ~~ edu+0*age+work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  edu ~~ age+work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  age ~~ work_skill+contract+chronic_ill+0*func_lim+0*social
  work_skill~~ 0*contract+chronic_ill+0*func_lim+social
  contract ~~ 0*chronic_ill+0*func_lim+0*social
  chronic_ill~~func_lim+social
  func_lim~~social
  
  A1 ~~   0*sex+edu+0*age+work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A2 ~~   0*sex+edu+0*age+work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A3 ~~   0*sex+edu+0*age+work_skill+0*contract+0*chronic_ill+0*func_lim+0*social
  A1 ~~   A2 + A3
  A2 ~~   A3

	# define interventional indirect effects via each mediator: monthly
	ie1M := b1*d1a
	ie2M := b2*d2a
	ie3M := b3*d3a
	ie4M := b4*d4a
	ie5M := b5*d5a
	ie6M := b6*d6a

	#weekly
	ie1W := b1*d1b
	ie2W := b2*d2b
	ie3W := b3*d3b
	ie4W := b4*d4b
	ie5W := b5*d5b
	ie6W := b6*d6b

	#full-time
	ie1d := b1*d1c
	ie2d := b2*d2c
	ie3d := b3*d3c
	ie4d := b4*d4c
	ie5d := b5*d5c
	ie6d := b6*d6c

  #Monthly: direct and joint indirect effects, and their sum
	de_A1 := bA1
	ie_jt_M := ie1M + ie2M + ie3M + ie4M + ie5M + ie6M
	de_ie_sum_M := de_A1 + ie_jt_M

	#Weekly: define direct and joint indirect effects, and their sum
	de_A2 := bA2
	ie_jt_W := ie1W + ie2W + ie3W + ie4W + ie5W + ie6W
	de_ie_sum_W := de_A2 + ie_jt_W

	#Weekly: define direct and joint indirect effects, and their sum
	de_A3 := bA3
	ie_jt_d := ie1d + ie2d + ie3d + ie4d + ie5d + ie6d
	de_ie_sum_d := de_A3 + ie_jt_d

	'
# parallel path model but permit mediators to covary
models.cat8[["Mcovary"]] <- paste(
  models.cat8[["noMcovary"]],
  "M1 ~~ M2 + M3 + M4 + M5 + M6",
  "M2 ~~ M3 + M4 + M5 + M6",
  "M3 ~~ M4 + M5 + M6",
  "M4 ~~ M5 + M6",
  "M5 ~~ M6",
  sep=" \n ")
# Model 9: Data-driven & theoretical model ----------------------------------------------------
##add chronic_ill ~ work_skill 
##edu ~~ age + work_skill
##age ~~ work_skill
##add as well covariances between exposures

##age ~~ contract
##func_lim ~~ social
##
##sex ~~ edu
##chronic_ill ~~ social
##sex ~~ work_skill

##age~~chronic_ill
##work_skill~~ 0*contract+chronic_ill+0*func_lim+social
##
##edu ~~social
##edu ~~contract
##age~~ func_lim
##work_skill ~~contract
##sex~~ func_lim + social
# model fit departing from m0
models.cat9 <- list()

# total effect
models.cat9[["total_effect"]] <- '
  Y ~ te1*A1+te2*A2+te3*A3+sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
  sex ~~ edu+0*age+work_skill+0*contract+0*chronic_ill+func_lim+social
  edu ~~ age+work_skill+contract+0*chronic_ill+0*func_lim+social
  age ~~ work_skill+contract+chronic_ill+func_lim+0*social
  work_skill~~ contract+chronic_ill+0*func_lim+social
  contract ~~ 0*chronic_ill+0*func_lim+0*social
  chronic_ill~~func_lim+social
  func_lim~~social
  
  A1 ~~   0*sex+edu+age+work_skill+0*contract+0*chronic_ill+func_lim+social
  A2 ~~   0*sex+edu+age+work_skill+0*contract+0*chronic_ill+func_lim+social
  A3 ~~   0*sex+edu+age+work_skill+0*contract+0*chronic_ill+func_lim+social
  A1 ~~   A2 + A3
  A2 ~~   A3
'
# parallel path model
models.cat9[["noMcovary"]] <- '
	Y ~  bA1*A1 + bA2*A2 + bA3*A3 + b1*M1 + b2*M2 + b3*M3 + b4*M4 + b5*M5 + b6*M6 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M1 ~ d1a*A1 + d1b*A2+ d1c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M2 ~ d2a*A1 + d2b*A2+ d2c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M3 ~ d3a*A1 + d3b*A2+ d3c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M4 ~ d4a*A1 + d4b*A2+ d4c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M5 ~ d5a*A1 + d5b*A2+ d5c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social
	M6 ~ d6a*A1 + d6b*A2+ d6c*A3 +sex+edu+age+work_skill+contract+chronic_ill+func_lim+social

	# fix covariates
  sex ~~ edu+0*age+work_skill+0*contract+0*chronic_ill+func_lim+social
  edu ~~ age+work_skill+contract+0*chronic_ill+0*func_lim+social
  age ~~ work_skill+contract+chronic_ill+func_lim+0*social
  work_skill~~ contract+chronic_ill+0*func_lim+social
  contract ~~ 0*chronic_ill+0*func_lim+0*social
  chronic_ill~~func_lim+social
  func_lim~~social
  
  A1 ~~   0*sex+edu+age+work_skill+0*contract+0*chronic_ill+func_lim+social
  A2 ~~   0*sex+edu+age+work_skill+0*contract+0*chronic_ill+func_lim+social
  A3 ~~   0*sex+edu+age+work_skill+0*contract+0*chronic_ill+func_lim+social
  A1 ~~   A2 + A3
  A2 ~~   A3

	# define interventional indirect effects via each mediator: monthly
	ie1M := b1*d1a
	ie2M := b2*d2a
	ie3M := b3*d3a
	ie4M := b4*d4a
	ie5M := b5*d5a
	ie6M := b6*d6a

	#weekly
	ie1W := b1*d1b
	ie2W := b2*d2b
	ie3W := b3*d3b
	ie4W := b4*d4b
	ie5W := b5*d5b
	ie6W := b6*d6b

	#full-time
	ie1d := b1*d1c
	ie2d := b2*d2c
	ie3d := b3*d3c
	ie4d := b4*d4c
	ie5d := b5*d5c
	ie6d := b6*d6c

  #Monthly: direct and joint indirect effects, and their sum
	de_A1 := bA1
	ie_jt_M := ie1M + ie2M + ie3M + ie4M + ie5M + ie6M
	de_ie_sum_M := de_A1 + ie_jt_M

	#Weekly: define direct and joint indirect effects, and their sum
	de_A2 := bA2
	ie_jt_W := ie1W + ie2W + ie3W + ie4W + ie5W + ie6W
	de_ie_sum_W := de_A2 + ie_jt_W

	#Weekly: define direct and joint indirect effects, and their sum
	de_A3 := bA3
	ie_jt_d := ie1d + ie2d + ie3d + ie4d + ie5d + ie6d
	de_ie_sum_d := de_A3 + ie_jt_d

	'
# parallel path model but permit mediators to covary
models.cat9[["Mcovary"]] <- paste(
  models.cat9[["noMcovary"]],
  "M1 ~~ M2 + M3 + M4 + M5 + M6",
  "M2 ~~ M3 + M4 + M5 + M6",
  "M3 ~~ M4 + M5 + M6",
  "M4 ~~ M5 + M6",
  "M5 ~~ M6",
  sep=" \n ")