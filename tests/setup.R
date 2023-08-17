# shopping na praia
# roupa, comida, pede
# tres havaianas
library(archive)

dictionary_tf <- tempfile()

dictionary_url <-
	paste0(
		"https://ftp.ibge.gov.br/Orcamentos_Familiares/" ,
		"Pesquisa_de_Orcamentos_Familiares_2017_2018/Microdados/Documentacao_20230713.zip"
	)

download.file( dictionary_url , dictionary_tf , mode = 'wb' )

dictionary_files <- archive_extract( dictionary_tf , dir = tempdir() )
library(readxl)

dictionary_fn <- file.path( tempdir() , "Dicionários de váriaveis.xls" )

dictionary_tbl <- read_excel( dictionary_fn , sheet = "Morador" , skip = 3 )

dictionary_df <- data.frame( dictionary_tbl )

names( dictionary_df ) <- c( 'position' , 'length' , 'decimals' , 'column_name' , 'description' , 'variable_labels' )

dictionary_df[ c( 'position' , 'length' , 'decimals' ) ] <- sapply( dictionary_df[ c( 'position' , 'length' , 'decimals' ) ] , as.integer )

dictionary_df <- subset( dictionary_df , !is.na( position ) )


post_stratification_fn <- file.path( tempdir() , "Pos_estratos_totais.xlsx" )

post_stratification_tbl <- read_excel( post_stratification_fn , skip = 5 )
	
post_stratification_df <- data.frame( post_stratification_tbl )

names( post_stratification_df ) <- c( 'estrato_pof' , 'pos_estrato' , 'total_pessoas' , 'uf' , 'cod_upa' )
this_tf <- tempfile()

this_url <-
	paste0(
		"https://ftp.ibge.gov.br/Orcamentos_Familiares/" ,
		"Pesquisa_de_Orcamentos_Familiares_2017_2018/Microdados/Dados_20230713.zip"
	)

download.file( this_url , this_tf , mode = 'wb' )

unzipped_files <- unzip( this_tf , exdir = tempdir() )

morador_fn <- grep( 'MORADOR\.txt$' , unzipped_files , value = TRUE )
library(readr)

morador_tbl <-
	read_fwf(
		morador_fn ,
		fwf_widths( 
			widths = dictionary_df[ , 'length' ] , 
			col_names = dictionary_df[ , 'column_name' ] 
		) ,
		col_types = 
			paste0( ifelse( is.na( dictionary_df[ , 'decimals' ] ) , "c" , "d" ) , collapse = '' )
	)

morador_df <- data.frame( morador_tbl )

names( morador_df ) <- tolower( names( morador_df ) )

pof_df <- merge( morador_df , post_stratification_df )

stopifnot( nrow( pof_df ) == nrow( morador_df ) )
# pof_fn <- file.path( path.expand( "~" ) , "POF" , "this_file.rds" )
# saveRDS( pof_df , file = pof_fn , compress = FALSE )
# pof_df <- readRDS( pof_fn )
options( survey.lonely.psu = "adjust" )

library(survey)

pre_stratified_design <- 
	svydesign(
		id = ~ cod_upa , 
		strata = ~ estrato_pof ,
		weights = ~ peso_final ,
		data = pof_df ,
		nest = TRUE
	)

population_totals <- 
	data.frame(
		pos_estrato = unique( pof_df[ , 'pos_estrato' ] ) , 
		Freq = unique( pof_df[ , 'total_pessoas' ] ) 
	)

pof_design <-
	postStratify(
		pre_stratified_design , 
		~ pos_estrato , 
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
	ci = TRUE , na.rm = TRUE
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
nationwide_adult_population <- svytotal( ~ pia , pof_design , na.rm = TRUE )
	
stopifnot( round( coef( nationwide_adult_population ) / 1000000 , 3 ) == 174.228 )
stopifnot( round( cv( nationwide_adult_population ) / 1000000 , 3 ) == 0 )
	
nationwide_labor_force <- svytotal( ~ pea_c , pof_design , na.rm = TRUE )

stopifnot( round( coef( nationwide_labor_force ) / 1000000 , 3 ) == 107.257 )
stopifnot( round( cv( nationwide_labor_force ) * 100 , 1 ) == 0.2 )
	
nationwide_employed <- svytotal( ~ ocup_c , pof_design , na.rm = TRUE )

stopifnot( round( coef( nationwide_employed ) / 1000000 , 3 ) == 97.825 )
stopifnot( round( cv( nationwide_employed ) * 100 , 1 ) == 0.2 )
	
nationwide_unemployed <- svytotal( ~ desocup30 , pof_design , na.rm = TRUE )

stopifnot( round( coef( nationwide_unemployed ) / 1000000 , 3 ) == 9.432 )
stopifnot( round( cv( nationwide_unemployed ) * 100 , 1 ) == 1.2 )
	
nationwide_not_in_labor_force <-
	svytotal( ~ as.numeric( pia & !pea_c ) , pof_design , na.rm = TRUE )

stopifnot( round( coef( nationwide_not_in_labor_force ) / 1000000 , 3 ) == 66.972 )
stopifnot( round( cv( nationwide_not_in_labor_force ) * 100 , 1 ) == 0.3 )
	

library(convey)
pof_design <- convey_prep( pof_design )

svygini( ~ vd4020n , pof_design , na.rm = TRUE )
library(srvyr)
pof_srvyr_design <- as_survey( pof_design )
pof_srvyr_design %>%
	summarize( mean = survey_mean( body_mass_index , na.rm = TRUE ) )

pof_srvyr_design %>%
	group_by( sexo ) %>%
	summarize( mean = survey_mean( body_mass_index , na.rm = TRUE ) )
