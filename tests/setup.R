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

domicilio_dictionary_tbl <- read_excel( dictionary_fn , sheet = "Domicílio" , skip = 3 )

domicilio_dictionary_df <- data.frame( domicilio_dictionary_tbl )

names( domicilio_dictionary_df ) <- c( 'position' , 'length' , 'decimals' , 'column_name' , 'description' , 'variable_labels' )

domicilio_dictionary_df[ c( 'position' , 'length' , 'decimals' ) ] <- sapply( domicilio_dictionary_df[ c( 'position' , 'length' , 'decimals' ) ] , as.integer )

domicilio_dictionary_df <- subset( domicilio_dictionary_df , !is.na( position ) )
morador_dictionary_tbl <- read_excel( dictionary_fn , sheet = "Morador" , skip = 3 )

morador_dictionary_df <- data.frame( morador_dictionary_tbl )

names( morador_dictionary_df ) <- c( 'position' , 'length' , 'decimals' , 'column_name' , 'description' , 'variable_labels' )

morador_dictionary_df[ c( 'position' , 'length' , 'decimals' ) ] <- sapply( morador_dictionary_df[ c( 'position' , 'length' , 'decimals' ) ] , as.integer )

morador_dictionary_df <- subset( morador_dictionary_df , !is.na( position ) )

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
library(readr)

domicilio_fn <- grep( 'DOMICILIO\\.txt$' , unzipped_files , value = TRUE )

domicilio_tbl <-
	read_fwf(
		domicilio_fn ,
		fwf_widths( 
			widths = domicilio_dictionary_df[ , 'length' ] , 
			col_names = domicilio_dictionary_df[ , 'column_name' ] 
		)
	)

domicilio_df <- data.frame( domicilio_tbl )

names( domicilio_df ) <- tolower( names( domicilio_df ) )

morador_fn <- grep( 'MORADOR\\.txt$' , unzipped_files , value = TRUE )

morador_tbl <-
	read_fwf(
		morador_fn ,
		fwf_widths( 
			widths = morador_dictionary_df[ , 'length' ] , 
			col_names = morador_dictionary_df[ , 'column_name' ] 
		)
	)

morador_df <- data.frame( morador_tbl )

names( morador_df ) <- tolower( names( morador_df ) )

dom_mor_df <- merge( domicilio_df[ c( 'cod_upa' , 'num_dom' , 'v6199' ) ] , morador_df )

pof_df <- merge( dom_mor_df , post_stratification_df )

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
		weights = ~ peso ,
		data = pof_df ,
		nest = TRUE
	)

population_totals <- 
	aggregate( peso_final ~ pos_estrato , data = pof_df , sum )
	
names( population_totals ) <- c( 'pos_estrato' , 'Freq' )

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
		
		food_security = factor( v6199 , levels = 1:4 , labels = c( 'food secure' , 'mild' , 'moderate' , 'severe' ) ) ,
	
		age_categories =
			factor( 
				1 + findInterval( v0403 , 
					c( 20 , 25 , 30 , 35 , 45 , 55 , 65 , 75 ) ) ,
				levels = 1:9 , labels = c( "under 20" , "20-24" , "25-29" ,
				"30-34" , "35-44" , "45-54" , "55-64" , "65-74" , "75+" )
			) ,
		
		sexo = factor( v0404 , levels = 1:2 , labels = c( 'male' , 'female' ) ) ,
		
		urban = as.numeric( tipo_situacao_reg )

	)

sum( weights( pof_design , "sampling" ) != 0 )

svyby( ~ one , ~ sexo , pof_design , unwtd.count )
svytotal( ~ one , pof_design )

svyby( ~ one , ~ sexo , pof_design , svytotal )
svymean( ~ renda_total , pof_design )

svyby( ~ renda_total , ~ sexo , pof_design , svymean )
svymean( ~ age_categories , pof_design )

svyby( ~ age_categories , ~ sexo , pof_design , svymean )
svytotal( ~ renda_total , pof_design )

svyby( ~ renda_total , ~ sexo , pof_design , svytotal )
svytotal( ~ age_categories , pof_design )

svyby( ~ age_categories , ~ sexo , pof_design , svytotal )
svyquantile( ~ renda_total , pof_design , 0.5 )

svyby( 
	~ renda_total , 
	~ sexo , 
	pof_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE 
)
svyratio( 
	numerator = ~ renda_total , 
	denominator = ~ anos_estudo , 
	pof_design ,
	na.rm = TRUE
)
sub_pof_design <- subset( pof_design , v0409 > 0 )
svymean( ~ renda_total , sub_pof_design )
this_result <- svymean( ~ renda_total , pof_design )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ renda_total , 
		~ sexo , 
		pof_design , 
		svymean 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( pof_design )
svyvar( ~ renda_total , pof_design )
# SRS without replacement
svymean( ~ renda_total , pof_design , deff = TRUE )

# SRS with replacement
svymean( ~ renda_total , pof_design , deff = "replace" )
svyciprop( ~ urban , pof_design ,
	method = "likelihood" )
svyttest( renda_total ~ urban , pof_design )
svychisq( 
	~ urban + age_categories , 
	pof_design 
)
glm_result <- 
	svyglm( 
		renda_total ~ urban + age_categories , 
		pof_design 
	)

summary( glm_result )
person_level_food_security <- svymean( ~ food_security , pof_design , na.rm = TRUE )
	
stopifnot( all.equal( round( coef( person_level_food_security ) , 2 ) , c( 0.59 , 0.27 , 0.09 , 0.05 ) , check.attributes = FALSE ) )

library(convey)
pof_design <- convey_prep( pof_design )

svygini( ~ renda_total , pof_design , na.rm = TRUE )
library(srvyr)
pof_srvyr_design <- as_survey( pof_design )
pof_srvyr_design %>%
	summarize( mean = survey_mean( renda_total ) )

pof_srvyr_design %>%
	group_by( sexo ) %>%
	summarize( mean = survey_mean( renda_total ) )
