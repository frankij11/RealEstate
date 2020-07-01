library(broom)
sdat_median_analysis <- function(){
df.raw <- read.csv("data/md_housing.csv") %>% sdat_filter()

df.city <- df.raw %>% 
  mutate(sqft = round(sqft,-2)) %>% group_by(county,city, sqft) %>% 
  summarise(n=n() , price.05 = quantile(price,.05) , price.50 = quantile(price, .5), price.95 = quantile(price, .95)) %>%
  filter(sqft<5000, n>5)

df.quality <- df.raw %>% 
  mutate(sqft = round(sqft,-2)) %>% group_by(county, sqft) %>% 
  summarise(n=n() , price.05 = quantile(price,.05) , price.50 = quantile(price, .5), price.95 = quantile(price, .95)) %>%
  filter(sqft<5000, n>5)
#View(df.quality)
#ggplot(df.quality, aes(x=sqft, y=price.50, color=county)) + geom_point()
#ggplot(df.city, aes(x=sqft, y=price.50, color=county)) + geom_point() + facet_grid(df.city$city)
county.mods <- list()

county.mods$lm.50 <- lm(price.50 ~ county : sqft, data=df.quality)
county.mods$lm.05 <- lm(price.05 ~ county : sqft, data=df.quality )
county.mods$lm.95 <- lm(price.95 ~ county : sqft, data=df.quality )
county.mods$lm.95.log <- lm(log(price.95) ~ county : sqft, data=df.quality )

preds <- sapply(county.mods, predict, df.quality) %>% as.data.frame() %>% bind_cols(df.quality)
#preds$lm.50.log <- exp(preds$lm.50.log)
preds$lm.95.log <- exp(preds$lm.95.log)


county.summary <- lapply(county.mods, glance) %>% bind_rows()
county.coef <- lapply(county.mods, tidy) %>% bind_rows(.id="Model") %>% 
  select(Model,term,estimate) %>%
  pivot_wider(names_from=term, values_from=estimate)
county.summary <- bind_cols(county.summary, county.coef) %>% select(Model, everything())

#View(county.summary)

#with(preds, plot(price.95, lm.95.log))
#points(preds$price.95, preds$lm.95, col="blue")
#abline(a=0, b=1)

  
  
city.mods <- list()
city.mods$lm.50 <- lm(price.50 ~ city : sqft + county -1, data=df.city) %>% step(trace = 0)
city.mods$lm.05 <- lm(price.05 ~ city : sqft + county -1, data=df.city) %>% step(trace = 0)
city.mods$lm.95 <- lm(price.95 ~ city : sqft + county -1, data=df.city) %>% step(trace = 0)
city.mods$lm.95.log <- lm(log(price.95) ~ city : sqft + county -1, data=df.city) %>% step(trace = 0)
city.preds <- sapply(county.mods, predict, df.city) %>% as.data.frame() %>% bind_cols(df.city)
city.preds$lm.95.log <- exp(city.preds$lm.95.log)

#with(city.preds, plot(price.95, lm.95.log))
#points(preds$price.95, preds$lm.95, col="blue")
#abline(a=0, b=1)


city.summary <- lapply(city.mods, glance) %>% bind_rows()
city.coef <- lapply(city.mods, tidy) %>% bind_rows(.id="Model") %>% 
  select(Model,term,estimate) %>%
  pivot_wider(names_from=term, values_from=estimate)
city.summary <- bind_cols(city.summary, city.coef) %>% select(Model,everything()) 


#View(city.summary)
saveRDS(county.mods, "results/county_mods.rds")
saveRDS(city.mods, "results/city_mods.rds")

}
sdat_median_analysis()
county.mods <- readRDS("results/county_mods.rds")
city.mods <- readRDS("results/city_mods.rds")