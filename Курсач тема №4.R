library("tidyverse")
library("nycflights13")
library("tidyr")
library("stringr")
library("dplyr")
library("tibble")
library("readr")
tbl = read.csv("eddypro.csv", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("[")) 
tbl = tbl[-1,] 
tbl 
tbl=tbl[tbl$DOY > 62 & tbl$DOY < 156,]
tbl 
tbl=tbl[tbl$daytime == TRUE,] 
glimpse(tbl)
sapply(tbl,is.numeric)
glimpse(tbl) 
tbl = select(tbl, -(roll))
tbl = tbl %>% mutate_if(is.character, factor)
names(tbl) =  str_replace_all(names(tbl), "[!]","_emph_")
names(tbl) = names(tbl) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>%
  str_replace_all("[-]","_minus_") %>%
  str_replace_all("[@]","_at_") %>%
  str_replace_all("[$]","_dollar_") %>%
  str_replace_all("[#]","_hash_") %>%
  str_replace_all("[/]","_div_") %>%
  str_replace_all("[%]","_perc_") %>%
  str_replace_all("[&]","_amp_") %>%
  str_replace_all("[\\^]","_power_") %>%
  str_replace_all("[()]","_") 
tbl_numeric = tbl[,sapply(tbl,is.numeric) ]
tbl_non_numeric = tbl[,!sapply(tbl,is.numeric) ]
cor_td = cor(tbl_numeric) 
cor_td 
cor_td = cor(drop_na(tbl_numeric)) 
cor_td 
cor_td = cor(drop_na(tbl_numeric)) %>% as.data.frame %>% select(co2_flux) 
vars = row.names(cor_td)[cor_td$co2_flux^2 > .1] %>% na.exclude 
vars 
formula1 = as.formula(paste("co2_flux~" , paste(vars,collapse = "+"), sep="")) 
formula1 
mod = lm(formula1, data = tbl)
summary(mod) 
anova(mod) 
formula2 = as.formula(paste("co2_flux ~ LE +  
    co2_flux +  rand_err_co2_flux + 
    h2o_flux + 
    air_density + air_molar_volume + 
    X.z.d..L + T. + x_offset + x_70. + un_Tau + 
    un_H + LE_scf + un_co2_flux + 
    w_var + w.ts_cov") )
mod1 = lm(formula2, data = tbl)
summary(mod1)
anova(mod1) 
formula3 = as.formula(paste("co2_flux ~ LE +  
   co2_flux +  rand_err_co2_flux + 
   h2o_flux + un_Tau + 
   LE_scf + un_co2_flux") )
mod2= lm(formula3, data = tbl) 
summary(mod2)
anova(mod2) 