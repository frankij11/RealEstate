source("./scripts/sdat.R")

library(geosphere)
library(skimr)


update_data <- F

# location
loc <- list(lat=38.9072,lon= -77.0369, miles =30) # DC
dc <- list(lat=38.9072,lon= -77.0369, miles =30) # DC
#loc <- list(lat=38.8159, lon=-76.7497) # Upper Marlbor

# get zipcode
zips <- read.csv("data/superzip.csv") %>%
		filter(state=="MD") %>%
		mutate(college =  gsub("%", "", college) %>% as.numeric()) %>%
		select(zipcode, centile, superzip, rank,adultpop, households,college,income)

# Rename vars

# Get Processed Data
if(!file.exists("data/md_housing.csv") | update_data==T){
  
  df<- sdat_query(where=where_comps(lat = loc$lat,lon = loc$lon, miles =loc$miles, year=2019))
  write.csv(df, "data/md_housing.csv", row.names=F)

}else{
  df <- read.csv("data/md_housing.csv")
}
df.proc <- df
df.skim <- skim(df)

df.skim.raw <- skim(df)
df <- sdat_filter(df)
df <- sdat_add_features(df)
df <- sdat_drop_cols(df)

df.skim <- skim(df)

# remove outliers
df <- df %>% filter(price < 2000000, price > 10000)


# calculate median values by city
# df.median <- df.proc %>%  
#   sdat_filter() %>% 
#   sdat_add_features() %>%
#   mutate(sqft = round(sqft,-2)) %>%
# 	group_by(county, city, zipcode,  stories,basement, sqft) %>%
# 	summarise(N=n(),
# 	  price.Max = max(price, na.rm=T),
# 	  price.50 = quantile(price, .5),
# 		price.95 = quantile(price, .95),
# 		price.05 = quantile(price, .05),	
# 		sqft.median = median(sqft),
# 		acre = median(acre, na.rm=T),
# 		tax_assessment = median(tax_assessment),
# 		year_built = median(year_built)
# 	) %>%
#   ungroup() %>%
#   #mutate(zipcode = as.numeric(zipcode)) %>%
# 	left_join(zips %>% mutate(zipcode = factor(zipcode)), by = "zipcode", how = "left")
# 	#select(-city, -zipcode, -rank)

#df.median %>% 
#	filter(sqft>2800, sqft<3400, basement==T, stories == "2") %>%
#	arrange(desc(centile)) %>%
#	View()
df.raw <- df
# df.median.raw <- df.median
