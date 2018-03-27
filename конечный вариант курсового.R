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
tbl=tbl[tbl$daytime == TRUE,] 
glimpse(tbl) 
sapply(tbl,is.numeric) 
glimpse(tbl) 
tbl = select(tbl, -(roll)) 
tbl = tbl %>% mutate_if(is.character, factor) 
names(tbl) = str_replace_all(names(tbl), "[!]","_emph_") 
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
cor_td = cor(drop_na(tbl_numeric)) 
cor_td 
cor_td = cor(drop_na(tbl_numeric)) %>% as.data.frame %>% select(co2_flux) 
vars = row.names(cor_td)[cor_td$co2_flux^2 > .1] %>% na.exclude 
vars 
formula1 = as.formula(paste("co2_flux~" , paste(vars,collapse = "+"), sep="")) 
formula1 
mod = lm(formula1, data = tbl) 
coef(mod)
resid(mod) 
confint(mod) 
summary(mod) 
anova(mod) 
plot(mod) 
return() 
return() 
return() 
return() 
formula2 = as.formula(paste("co2_flux ~ LE + rand_err_co2_flux + 
                            h2o_flux + air_density + air_molar_volume + 
                            X.z.d..L + T. + x_offset + x_70. + un_Tau + 
                            un_H + LE_scf + un_co2_flux + 
                            w_var + w.co2_cov") ) 
mod1 = lm(formula2, data = tbl) 


mod1 = lm(co2_flux~(H + LE + rand_err_co2_flux + 
                      h2o_flux + air_density + air_molar_volume + 
                      X.z.d..L + T. + x_offset + x_70. + un_Tau + 
                      un_H + LE_scf + un_co2_flux + 
                      w_var + w.co2_cov)^2, data = tbl )

coef(mod1)
resid(mod1) 
confint(mod1)
summary(mod1) 
anova(mod1) 
plot(mod1) 
return() 
return() 
return() 
return() 
formula3 = as.formula(paste("co2_flux ~ LE + co2_flux + rand_err_co2_flux + h2o_flux + un_Tau + LE_scf + un_co2_flux") ) 
mod2= lm(formula3, data = tbl) 

mod2 = lm(co2_flux~(H + LE + rand_err_co2_flux + h2o_flux + air_density + air_molar_volume + X.z.d..L +
                      T. + x_offset + x_70. + un_Tau + un_H + LE_scf + un_co2_flux + 
                      w_var + w.co2_cov)^2 - X.z.d..L - T. - rand_err_co2_flux - H - un_Tau - un_H - 
            un_co2_flux - w_var - w.co2_cov - H:LE - H:h2o_flux - H:air_density - H:air_molar_volume - 
            H:x_70. - H:un_Tau - H:w_var - LE:rand_err_co2_flux - LE:un_H - LE:w_var - 
            rand_err_co2_flux:h2o_flux - rand_err_co2_flux:air_density - rand_err_co2_flux:air_molar_volume -
            rand_err_co2_flux:T. - rand_err_co2_flux:un_Tau - rand_err_co2_flux:un_co2_flux - rand_err_co2_flux:w_var - 
            rand_err_co2_flux:w.co2_cov - h2o_flux:un_H - h2o_flux:w_var - air_density:air_molar_volume - 
            air_density:X.z.d..L - air_density:T. - air_density:un_Tau - air_density:un_H - air_density:un_co2_flux -
            air_density:w_var - air_density:w.co2_cov - air_molar_volume:X.z.d..L - air_molar_volume:T. - air_molar_volume:un_Tau -
            air_molar_volume:un_H - air_molar_volume:un_co2_flux - air_molar_volume:w_var - air_molar_volume:w.co2_cov  -  
            X.z.d..L:T.  -  X.z.d..L:un_Tau  -  T.:x_70.   - T.:un_Tau   - T.:w_var   - x_offset:x_70. - x_offset:LE_scf  - 
            x_offset:un_co2_flux - x_offset:w.co2_cov - x_70.:un_H - x_70.:un_co2_flux - x_70.:w.co2_cov - un_Tau:un_H - 
            un_Tau:LE_scf - un_Tau:w_var - un_H:w_var - LE_scf:w_var - LE_scf:w.co2_cov  - un_co2_flux:w_var - 
            un_co2_flux:w.co2_cov  - w_var:w.co2_cov , data = tbl )

coef(mod2) 
resid(mod2) 
confint(mod2)
summary(mod2) 
anova(mod2) 
plot(mod2)
return() 
return() 
return() 
return()

mod3 = lm(co2_flux~(H + LE + rand_err_co2_flux + h2o_flux + air_density + air_molar_volume + X.z.d..L +
                      T. + x_offset + x_70. + un_Tau + un_H + LE_scf + un_co2_flux + 
                      w_var + w.co2_cov)^2 - X.z.d..L - T. - rand_err_co2_flux - H - un_Tau - un_H - 
            un_co2_flux - w_var - w.co2_cov - H:LE - H:h2o_flux - H:air_density - H:air_molar_volume - 
            H:x_70. - H:un_Tau - H:w_var - LE:rand_err_co2_flux - LE:un_H - LE:w_var - 
            rand_err_co2_flux:h2o_flux - rand_err_co2_flux:air_density - rand_err_co2_flux:air_molar_volume -
            rand_err_co2_flux:T. - rand_err_co2_flux:un_Tau - rand_err_co2_flux:un_co2_flux - rand_err_co2_flux:w_var - 
            rand_err_co2_flux:w.co2_cov - h2o_flux:un_H - h2o_flux:w_var - air_density:air_molar_volume - 
            air_density:X.z.d..L - air_density:T. - air_density:un_Tau - air_density:un_H - air_density:un_co2_flux -
            air_density:w_var - air_density:w.co2_cov - air_molar_volume:X.z.d..L - air_molar_volume:T. - air_molar_volume:un_Tau -
            air_molar_volume:un_H - air_molar_volume:un_co2_flux - air_molar_volume:w_var - air_molar_volume:w.co2_cov  -  
            X.z.d..L:T.  -  X.z.d..L:un_Tau  -  T.:x_70.   - T.:un_Tau   - T.:w_var   - x_offset:x_70. - x_offset:LE_scf  - 
            x_offset:un_co2_flux - x_offset:w.co2_cov - x_70.:un_H - x_70.:un_co2_flux - x_70.:w.co2_cov - un_Tau:un_H - 
            un_Tau:LE_scf - un_Tau:w_var - un_H:w_var - LE_scf:w_var - LE_scf:w.co2_cov  - un_co2_flux:w_var - 
            un_co2_flux:w.co2_cov  - w_var:w.co2_cov - LE - h2o_flux - air_density - air_molar_volume - x_70. - 
            LE_scf - H:rand_err_co2_flux - H:X.z.d..L - H:T. - H:un_co2_flux - H:w.co2_cov - LE:h2o_flux - 
            LE:air_density - LE:air_molar_volume - LE:X.z.d..L - LE:T. - LE:x_70. - LE:LE_scf - LE:un_co2_flux - 
            LE:w.co2_cov - rand_err_co2_flux:x_offset - rand_err_co2_flux:x_70. - rand_err_co2_flux:un_H - 
            rand_err_co2_flux:LE_scf - h2o_flux:air_density - h2o_flux:air_molar_volume - h2o_flux:X.z.d..L - 
            h2o_flux:T. - h2o_flux:x_70. - h2o_flux:LE_scf - h2o_flux:un_co2_flux - h2o_flux:w.co2_cov - 
            air_density:x_70. - air_density:LE_scf - air_molar_volume:x_70. - air_molar_volume:LE_scf - 
            X.z.d..L:x_offset - X.z.d..L:un_H - X.z.d..L:un_co2_flux - X.z.d..L:w_var - X.z.d..L:w.co2_cov - 
            T.:x_offset - T.:un_H - T.:un_co2_flux - T.:w.co2_cov - x_offset:un_Tau - x_offset:w_var - x_70.:un_Tau - 
            x_70.:w_var - un_Tau:un_co2_flux - un_Tau:w.co2_cov - un_H:un_co2_flux - un_H:w.co2_cov, data = tbl )

coef(mod3)
resid(mod3) 
confint(mod3)
summary(mod3) 
anova(mod3) 
plot(mod3)
return() 
return() 
return() 
return()

mod4 = lm(co2_flux~(H + LE + rand_err_co2_flux + h2o_flux + air_density + air_molar_volume + X.z.d..L +
                      T. + x_offset + x_70. + un_Tau + un_H + LE_scf + un_co2_flux + 
                      w_var + w.co2_cov)^2 - X.z.d..L - T. - rand_err_co2_flux - H - un_Tau - un_H - 
            un_co2_flux - w_var - w.co2_cov - H:LE - H:h2o_flux - H:air_density - H:air_molar_volume - 
            H:x_70. - H:un_Tau - H:w_var - LE:rand_err_co2_flux - LE:un_H - LE:w_var - 
            rand_err_co2_flux:h2o_flux - rand_err_co2_flux:air_density - rand_err_co2_flux:air_molar_volume -
            rand_err_co2_flux:T. - rand_err_co2_flux:un_Tau - rand_err_co2_flux:un_co2_flux - rand_err_co2_flux:w_var - 
            rand_err_co2_flux:w.co2_cov - h2o_flux:un_H - h2o_flux:w_var - air_density:air_molar_volume - 
            air_density:X.z.d..L - air_density:T. - air_density:un_Tau - air_density:un_H - air_density:un_co2_flux -
            air_density:w_var - air_density:w.co2_cov - air_molar_volume:X.z.d..L - air_molar_volume:T. - air_molar_volume:un_Tau -
            air_molar_volume:un_H - air_molar_volume:un_co2_flux - air_molar_volume:w_var - air_molar_volume:w.co2_cov  -  
            X.z.d..L:T.  -  X.z.d..L:un_Tau  -  T.:x_70.   - T.:un_Tau   - T.:w_var   - x_offset:x_70. - x_offset:LE_scf  - 
            x_offset:un_co2_flux - x_offset:w.co2_cov - x_70.:un_H - x_70.:un_co2_flux - x_70.:w.co2_cov - un_Tau:un_H - 
            un_Tau:LE_scf - un_Tau:w_var - un_H:w_var - LE_scf:w_var - LE_scf:w.co2_cov  - un_co2_flux:w_var - 
            un_co2_flux:w.co2_cov  - w_var:w.co2_cov - LE - h2o_flux - air_density - air_molar_volume - x_70. - 
            LE_scf - H:rand_err_co2_flux - H:X.z.d..L - H:T. - H:un_co2_flux - H:w.co2_cov - LE:h2o_flux - 
            LE:air_density - LE:air_molar_volume - LE:X.z.d..L - LE:T. - LE:x_70. - LE:LE_scf - LE:un_co2_flux - 
            LE:w.co2_cov - rand_err_co2_flux:x_offset - rand_err_co2_flux:x_70. - rand_err_co2_flux:un_H - 
            rand_err_co2_flux:LE_scf - h2o_flux:air_density - h2o_flux:air_molar_volume - h2o_flux:X.z.d..L - 
            h2o_flux:T. - h2o_flux:x_70. - h2o_flux:LE_scf - h2o_flux:un_co2_flux - h2o_flux:w.co2_cov - 
            air_density:x_70. - air_density:LE_scf - air_molar_volume:x_70. - air_molar_volume:LE_scf - 
            X.z.d..L:x_offset - X.z.d..L:un_H - X.z.d..L:un_co2_flux - X.z.d..L:w_var - X.z.d..L:w.co2_cov - 
            T.:x_offset - T.:un_H - T.:un_co2_flux - T.:w.co2_cov - x_offset:un_Tau - x_offset:w_var - x_70.:un_Tau - 
            x_70.:w_var - un_Tau:un_co2_flux - un_Tau:w.co2_cov - un_H:un_co2_flux - un_H:w.co2_cov - x_offset - 
            LE:x_offset - h2o_flux:x_offset - air_density:x_offset - air_molar_volume:x_offset - X.z.d..L:x_70. - 
            X.z.d..L:LE_scf - T.:LE_scf - x_70.:LE_scf, data = tbl )

coef(mod4) 
resid(mod4) 
confint(mod4)
summary(mod4) 
anova(mod4) 
plot(mod4)
return() 
return() 
return() 
return()


mod5 = lm(co2_flux~(H + LE + rand_err_co2_flux + h2o_flux + air_density + air_molar_volume + X.z.d..L +
                      T. + x_offset + x_70. + un_Tau + un_H + LE_scf + un_co2_flux + 
                      w_var + w.co2_cov)^2 - X.z.d..L - T. - rand_err_co2_flux - H - un_Tau - un_H - 
            un_co2_flux - w_var - w.co2_cov - H:LE - H:h2o_flux - H:air_density - H:air_molar_volume - 
            H:x_70. - H:un_Tau - H:w_var - LE:rand_err_co2_flux - LE:un_H - LE:w_var - 
            rand_err_co2_flux:h2o_flux - rand_err_co2_flux:air_density - rand_err_co2_flux:air_molar_volume -
            rand_err_co2_flux:T. - rand_err_co2_flux:un_Tau - rand_err_co2_flux:un_co2_flux - rand_err_co2_flux:w_var - 
            rand_err_co2_flux:w.co2_cov - h2o_flux:un_H - h2o_flux:w_var - air_density:air_molar_volume - 
            air_density:X.z.d..L - air_density:T. - air_density:un_Tau - air_density:un_H - air_density:un_co2_flux -
            air_density:w_var - air_density:w.co2_cov - air_molar_volume:X.z.d..L - air_molar_volume:T. - air_molar_volume:un_Tau -
            air_molar_volume:un_H - air_molar_volume:un_co2_flux - air_molar_volume:w_var - air_molar_volume:w.co2_cov  -  
            X.z.d..L:T.  -  X.z.d..L:un_Tau  -  T.:x_70.   - T.:un_Tau   - T.:w_var   - x_offset:x_70. - x_offset:LE_scf  - 
            x_offset:un_co2_flux - x_offset:w.co2_cov - x_70.:un_H - x_70.:un_co2_flux - x_70.:w.co2_cov - un_Tau:un_H - 
            un_Tau:LE_scf - un_Tau:w_var - un_H:w_var - LE_scf:w_var - LE_scf:w.co2_cov  - un_co2_flux:w_var - 
            un_co2_flux:w.co2_cov  - w_var:w.co2_cov - LE - h2o_flux - air_density - air_molar_volume - x_70. - 
            LE_scf - H:rand_err_co2_flux - H:X.z.d..L - H:T. - H:un_co2_flux - H:w.co2_cov - LE:h2o_flux - 
            LE:air_density - LE:air_molar_volume - LE:X.z.d..L - LE:T. - LE:x_70. - LE:LE_scf - LE:un_co2_flux - 
            LE:w.co2_cov - rand_err_co2_flux:x_offset - rand_err_co2_flux:x_70. - rand_err_co2_flux:un_H - 
            rand_err_co2_flux:LE_scf - h2o_flux:air_density - h2o_flux:air_molar_volume - h2o_flux:X.z.d..L - 
            h2o_flux:T. - h2o_flux:x_70. - h2o_flux:LE_scf - h2o_flux:un_co2_flux - h2o_flux:w.co2_cov - 
            air_density:x_70. - air_density:LE_scf - air_molar_volume:x_70. - air_molar_volume:LE_scf - 
            X.z.d..L:x_offset - X.z.d..L:un_H - X.z.d..L:un_co2_flux - X.z.d..L:w_var - X.z.d..L:w.co2_cov - 
            T.:x_offset - T.:un_H - T.:un_co2_flux - T.:w.co2_cov - x_offset:un_Tau - x_offset:w_var - x_70.:un_Tau - 
            x_70.:w_var - un_Tau:un_co2_flux - un_Tau:w.co2_cov - un_H:un_co2_flux - un_H:w.co2_cov - x_offset - 
            LE:x_offset - h2o_flux:x_offset - air_density:x_offset - air_molar_volume:x_offset - X.z.d..L:x_70. - 
            X.z.d..L:LE_scf - T.:LE_scf - x_70.:LE_scf - H:x_offset - x_offset:un_H, data = tbl )

coef(mod5)
resid(mod5) 
confint(mod5)
summary(mod5) 
anova(mod5) 
plot(mod5)
return() 
return() 
return() 
return()

