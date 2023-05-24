rm(list=ls())
library("tidyverse")
library("readr")
library("stringr")
library("dplyr")
library("ggplot2")
library("tidyr")
library("stringr")
library("lubridate")

# Подгрузим данные из таблицы
tbl = read_csv("eddypro.csv", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("["))

# Подготовка данных к анализу
tbl = tbl[-1,]; tbl
tbl = select(tbl, -(roll))

tbl$date <- as.Date (tbl$date, format = "%Y-%m-%d")
# Добавляем столбцы с годом и месяцем
tbl = mutate(tbl, month = months(tbl$date))
tbl = mutate(tbl, year = year(tbl$date))

# Переводим текстовые значения в факторные
tbl = tbl %>% mutate_if(is.character, factor)
glimpse(tbl)
# Меняем некорректные символы
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
glimpse(tbl)

# Выбираем ночное время
tbl = filter(tbl, daytime %in% FALSE)
# Выбираем необходимый год
tbl = filter(tbl, year %in% 2013)
# Выбираем необходимые месяцы
tbl = filter(tbl, month %in% c("Сентябрь","Октябрь","Ноябрь"))
unique(tbl$month) # проверяем какие месяца остались
# Убираем пустые значения
tbl = drop_na(tbl)


# Перевод данных + проведем корреляционный анализ
tbl_numeric = tbl[,sapply(tbl,is.numeric) ]
cor_td = cor(drop_na(tbl_numeric), method = "spearman") %>% as.data.frame %>% select(h2o_flux)
cor_td
cor_td = drop_na(cor_td)
cor_td
vars = row.names(cor_td)[cor_td$h2o_flux^2 > 0.2] %>% na.exclude

# Выодим формулу со всеми параметрами
formula = as.formula(paste("h2o_flux~", paste(vars,collapse = "+"), sep=""));formula


#создание моделей и вывод оптимальной модели
model0 = lm(h2o_flux ~ (LE + rand_err_LE + rand_err_h2o_flux +
                          co2_molar_density + co2_mole_fraction + co2_mixing_ratio +
                          RH + VPD + un_LE + un_h2o_flux + h2o_var + w_div_h2o_cov +
                          co2...125 + co2...127 + co2_signal_strength_7200 + h2o_signal_strength_7200), data = tbl_numeric)

coef(model0)
resid(model0)
confint(model0)
summary(model0)
anova(model0)

model1 = lm(h2o_flux ~ (LE + rand_err_LE + rand_err_h2o_flux +
                          co2_molar_density + co2_mole_fraction + co2_mixing_ratio +
                          RH + VPD + un_LE + un_h2o_flux + h2o_var + w_div_h2o_cov + co2...127 + co2_signal_strength_7200 + h2o_signal_strength_7200), data = tbl_numeric)


coef(model1)
resid(model1)
confint(model1)
summary(model1)
anova(model1)


model2 = lm(h2o_flux ~ (LE + rand_err_LE + rand_err_h2o_flux +
                          co2_molar_density + co2_mole_fraction + co2_mixing_ratio +
                          RH + VPD + un_LE + un_h2o_flux + h2o_var + w_div_h2o_cov + h2o_signal_strength_7200), data = tbl_numeric)

coef(model2)
resid(model2)
confint(model2)
summary(model2)
anova(model2)

model3 = lm(h2o_flux ~ (LE + rand_err_LE + rand_err_h2o_flux +
                          co2_molar_density + co2_mole_fraction + co2_mixing_ratio +
                          RH + VPD + un_LE + un_h2o_flux + w_div_h2o_cov), data = tbl_numeric)


coef(model3)
resid(model3)
confint(model3)
summary(model3)
anova(model3)
anova(model2, model3)

# Получившаяся model3 модель имеет все зависимые параметры, при этом показатели практически не изменены