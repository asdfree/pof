if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)
# examine all available POF microdata files
pof_cat <-
	get_catalog( "pof" ,
		output_dir = file.path( getwd() ) )

# 2008-2009 only
pof_cat <- subset( pof_cat , period == "2008_2009" )
# download the microdata to your local computer
stopifnot( nrow( pof_cat ) > 0 )

options( survey.lonely.psu = "adjust" )

library(survey)

poststr <- 
	readRDS( 
		file.path( getwd() , 
			"2008_2009/poststr.rds" ) 
		)

		
t_morador_s <- 
	readRDS( 
		file.path( getwd() , 
			"2008_2009/t_morador_s.rds" ) 
		)

t_morador_s <-
	transform(
		t_morador_s , 
		control = paste0( cod_uf , num_seq , num_dv ) 
	)
	
pof_df <- merge( t_morador_s , poststr )

stopifnot( nrow( pof_df ) == nrow( t_morador_s ) )

pre_stratified_df <- 
	svydesign(
		id = ~control , 
		strata = ~estrato_unico ,
		weights = ~fator_expansao1 ,
		data = pof_df ,
		nest = TRUE
	)

population_totals <- 
	data.frame(
		pos_estrato = unique( pre_stratified_df$pos_estrato ) , 
		Freq = unique( pre_stratified_df$tot_pop ) 
	)

pof_design <-
	postStratify(
		pre_stratified_df , 
		~pos_estrato , 
		population_totals
	)
pof_design <- 
	update(
		pof_design , 
		
		one = 1 ,
		
		# centimeters instead of meters
		altura_imputado = altura_imputado / 100 ,
		
		age_categories =
			factor( 
				1 + findInterval( idade_anos , 
					c( 20 , 25 , 30 , 35 , 45 , 55 , 65 , 75 ) ) ,
				levels = 1:9 , labels = c( "under 20" , "20-24" , "25-29" ,
				"30-34" , "35-44" , "45-54" , "55-64" , "65-74" , "75+" )
			) ,
		
		# create a body mass index (bmi) variable, excluding babies (who have altura_imputado==0)			
		body_mass_index = ifelse( altura_imputado == 0 , 0 , peso_imputado / ( altura_imputado ^ 2 ) ) ,
		
		sexo = ifelse( cod_sexo == '01' , "masculino" , ifelse( cod_sexo == '02' , "feminino" , NA ) )
		
		
	)

pof_design <-
	transform(
		pof_design ,
		
		# individuals with a low bmi - underweight
		underweight = ifelse( body_mass_index < 18.5 , 1 , 0 ) ,
		
		# individuals with a high bmi - overweight
		overweight = ifelse( body_mass_index >= 25 , 1 , 0 ) ,
		
		# individuals with a very high bmi - obese
		obese = ifelse( body_mass_index >= 30 , 1 , 0 )
	)
sum( weights( pof_design , "sampling" ) != 0 )

svyby( ~ one , ~ sexo , pof_design , unwtd.count )
svytotal( ~ one , pof_design )

svyby( ~ one , ~ sexo , pof_design , svytotal )
svymean( ~ body_mass_index , pof_design , na.rm = TRUE )

svyby( ~ body_mass_index , ~ sexo , pof_design , svymean , na.rm = TRUE )
svymean( ~ age_categories , pof_design )

svyby( ~ age_categories , ~ sexo , pof_design , svymean )
svytotal( ~ body_mass_index , pof_design , na.rm = TRUE )

svyby( ~ body_mass_index , ~ sexo , pof_design , svytotal , na.rm = TRUE )
svytotal( ~ age_categories , pof_design )

svyby( ~ age_categories , ~ sexo , pof_design , svytotal )
svyquantile( ~ body_mass_index , pof_design , 0.5 , na.rm = TRUE )

svyby( 
	~ body_mass_index , 
	~ sexo , 
	pof_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE ,
	na.rm = TRUE
)
svyratio( 
	numerator = ~ peso_imputado , 
	denominator = ~ altura_imputado , 
	pof_design ,
	na.rm = TRUE
)
sub_pof_design <- subset( pof_design , underweight == 1 )
svymean( ~ body_mass_index , sub_pof_design , na.rm = TRUE )
this_result <- svymean( ~ body_mass_index , pof_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ body_mass_index , 
		~ sexo , 
		pof_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( pof_design )
svyvar( ~ body_mass_index , pof_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ body_mass_index , pof_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ body_mass_index , pof_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ obese , pof_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( body_mass_index ~ obese , pof_design )
svychisq( 
	~ obese + age_categories , 
	pof_design 
)
glm_result <- 
	svyglm( 
		body_mass_index ~ obese + age_categories , 
		pof_design 
	)

summary( glm_result )
library(srvyr)
pof_srvyr_design <- as_survey( pof_design )
pof_srvyr_design %>%
	summarize( mean = survey_mean( body_mass_index , na.rm = TRUE ) )

pof_srvyr_design %>%
	group_by( sexo ) %>%
	summarize( mean = survey_mean( body_mass_index , na.rm = TRUE ) )

