#хи квадрат для активности
t_act <- table(df_act$`happiness_high`, df_act$activity)
chisq.test(t_act)

#хи квадрат для вариативности
t_var <- table(df_var)
chisq.test(t_var)

#логит для активности
fit_act <- glm(happiness_high ~ activity, df_act, family = "binomial")
coef(fit_act)
summary(fit_act)

#логит для вариативности
fit_var <- glm(happiness_high ~ variety_high, df_var, family = "binomial")
coef(fit_var)
summary(fit_var)

#обработка выбросов
df_sum <- na.omit(sum)
df_sum_obr <- subset(df_sum, Q5_r4 != "Затрудняюсь ответить" & Q5_r12 != "Затрудняюсь ответить" & 
                    Q18_r1 != "Затрудняюсь ответить / Не могу оценить" & Q18_r2 != "Затрудняюсь ответить / Не могу оценить" & 
                    Q23_r1 != "Затрудняюсь ответить" & Q23_r2 != "Затрудняюсь ответить" & Q23_r3 != "Затрудняюсь ответить" & 
                    Q23_r4 != "Затрудняюсь ответить" & Q23_r5 != "Затрудняюсь ответить" & Q23_r6 != "Затрудняюсь ответить" & 
                    Q23_r7 != "Затрудняюсь ответить" & Q23_r8 != "Затрудняюсь ответить" & Q23_r9 != "Затрудняюсь ответить" & 
                    Q23_r10 != "Затрудняюсь ответить" & Q23_r11 != "Затрудняюсь ответить")

#удовлетворенность

for (i in 1:5898)
{
  s <- 0
  if(df_sum_obr$Q5_r4[i] == "Полностью согласен(-на)"){s = s + 3}
  if(df_sum_obr$Q5_r4[i] == "Скорее согласен(-на)"){s = s + 2}
  if(df_sum_obr$Q5_r4[i] == "Скорее не согласен(-на)"){s = s + 1}
  if(df_sum_obr$Q5_r12[i] == "Совершенно не согласен(-на)"){s = s + 0}
  if(df_sum_obr$Q5_r12[i] == "Полностью согласен(-на)"){s = s + 3}
  if(df_sum_obr$Q5_r12[i] == "Скорее согласен(-на)"){s = s + 2}
  if(df_sum_obr$Q5_r12[i] == "Скорее не согласен(-на)"){s = s + 1}
  if(df_sum_obr$Q5_r12[i] == "Совершенно не согласен(-на)"){s = s + 0}
  av <- s / 6
  if (av >= 0.5) {df_summ$happiness_high[i] <- 1}
  if (av < 0.5) {df_summ$happiness_high[i] <- 0}
}

#активность

for (i in 1:5898)
{
  s <- 0
  if(df_sum_obr$Q23_r1[i] == "Очень часто"){s = s + 4}
  if(df_sum_obr$Q23_r1[i] == "Скорее часто"){s = s + 3}
  if(df_sum_obr$Q23_r1[i] == "Время от времени"){s = s + 2}
  if(df_sum_obr$Q23_r1[i] == "Редко"){s = s + 1}
  if(df_sum_obr$Q23_r1[i] == "Никогда"){s = s + 0}
  if(df_sum_obr$Q23_r2[i] == "Очень часто"){s = s + 4}
  if(df_sum_obr$Q23_r2[i] == "Скорее часто"){s = s + 3}
  if(df_sum_obr$Q23_r2[i] == "Время от времени"){s = s + 2}
  if(df_sum_obr$Q23_r2[i] == "Редко"){s = s + 1}
  if(df_sum_obr$Q23_r2[i] == "Никогда"){s = s + 0}
  if(df_sum_obr$Q23_r3[i] == "Очень часто"){s = s + 4}
  if(df_sum_obr$Q23_r3[i] == "Скорее часто"){s = s + 3}
  if(df_sum_obr$Q23_r3[i] == "Время от времени"){s = s + 2}
  if(df_sum_obr$Q23_r3[i] == "Редко"){s = s + 1}
  if(df_sum_obr$Q23_r3[i] == "Никогда"){s = s + 0}
  if(df_sum_obr$Q23_r4[i] == "Очень часто"){s = s + 4}
  if(df_sum_obr$Q23_r4[i] == "Скорее часто"){s = s + 3}
  if(df_sum_obr$Q23_r4[i] == "Время от времени"){s = s + 2}
  if(df_sum_obr$Q23_r4[i] == "Редко"){s = s + 1}
  if(df_sum_obr$Q23_r4[i] == "Никогда"){s = s + 0}
  if(df_sum_obr$Q23_r5[i] == "Очень часто"){s = s + 4}
  if(df_sum_obr$Q23_r5[i] == "Скорее часто"){s = s + 3}
  if(df_sum_obr$Q23_r5[i] == "Время от времени"){s = s + 2}
  if(df_sum_obr$Q23_r5[i] == "Редко"){s = s + 1}
  if(df_sum_obr$Q23_r5[i] == "Никогда"){s = s + 0}
  if(df_sum_obr$Q23_r6[i] == "Очень часто"){s = s + 4}
  if(df_sum_obr$Q23_r6[i] == "Скорее часто"){s = s + 3}
  if(df_sum_obr$Q23_r6[i] == "Время от времени"){s = s + 2}
  if(df_sum_obr$Q23_r6[i] == "Редко"){s = s + 1}
  if(df_sum_obr$Q23_r6[i] == "Никогда"){s = s + 0}
  if(df_sum_obr$Q23_r7[i] == "Очень часто"){s = s + 4}
  if(df_sum_obr$Q23_r7[i] == "Скорее часто"){s = s + 3}
  if(df_sum_obr$Q23_r7[i] == "Время от времени"){s = s + 2}
  if(df_sum_obr$Q23_r7[i] == "Редко"){s = s + 1}
  if(df_sum_obr$Q23_r7[i] == "Никогда"){s = s + 0}
  if(df_sum_obr$Q23_r8[i] == "Очень часто"){s = s + 4}
  if(df_sum_obr$Q23_r8[i] == "Скорее часто"){s = s + 3}
  if(df_sum_obr$Q23_r8[i] == "Время от времени"){s = s + 2}
  if(df_sum_obr$Q23_r8[i] == "Редко"){s = s + 1}
  if(df_sum_obr$Q23_r8[i] == "Никогда"){s = s + 0}
  if(df_sum_obr$Q23_r9[i] == "Очень часто"){s = s + 0}
  if(df_sum_obr$Q23_r9[i] == "Скорее часто"){s = s + 1}
  if(df_sum_obr$Q23_r9[i] == "Время от времени"){s = s + 2}
  if(df_sum_obr$Q23_r9[i] == "Редко"){s = s + 3}
  if(df_sum_obr$Q23_r9[i] == "Никогда"){s = s + 4}
  if(df_sum_obr$Q23_r10[i] == "Очень часто"){s = s + 0}
  if(df_sum_obr$Q23_r10[i] == "Скорее часто"){s = s + 1}
  if(df_sum_obr$Q23_r10[i] == "Время от времени"){s = s + 2}
  if(df_sum_obr$Q23_r10[i] == "Редко"){s = s + 3}
  if(df_sum_obr$Q23_r10[i] == "Никогда"){s = s + 4}
  if(df_sum_obr$Q23_r11[i] == "Очень часто"){s = s + 0}
  if(df_sum_obr$Q23_r11[i] == "Скорее часто"){s = s + 1}
  if(df_sum_obr$Q23_r11[i] == "Время от времени"){s = s + 2}
  if(df_sum_obr$Q23_r11[i] == "Редко"){s = s + 3}
  if(df_sum_obr$Q23_r11[i] == "Никогда"){s = s + 4}
  av <- s / 44
  df_sum_obr$av[i] <- s / 44
  if (av >= 0.66) {df_summ$activity[i] <- "высокая"}
  if (av < 0.33) {df_summ$activity[i] <- "низкая"}
  if (av >= 0.33 & av <0.66) {df_summ$activity[i] <- "средняя"}
}

#вариативность

for (i in 1:5898)
{
  s <- 0
  if(df_sum_obr$Q18_r1[i] == "Полностью согласен(-на)"){s = s + 3}
  if(df_sum_obr$Q18_r1[i] == "Скорее согласен(-на)"){s = s + 2}
  if(df_sum_obr$Q18_r1[i] == "Скорее не согласен(-на)"){s = s + 1}
  if(df_sum_obr$Q18_r1[i] == "Совершенно не согласен(-на)"){s = s + 0}
  if(df_sum_obr$Q18_r2[i] == "Полностью согласен(-на)"){s = s + 3}
  if(df_sum_obr$Q18_r2[i] == "Скорее согласен(-на)"){s = s + 2}
  if(df_sum_obr$Q18_r2[i] == "Скорее не согласен(-на)"){s = s + 1}
  if(df_sum_obr$Q18_r2[i] == "Совершенно не согласен(-на)"){s = s + 0}
  av <- s / 6
  if (av >= 0.5) {df_summ$variety_high[i] <- 1}
  if (av < 0.5) {df_summ$variety_high[i] <- 0}
}

#логит для обоих предикторов
fit_sum <- glm(happiness_high ~ variety_high + activity, df_summ, family = "binomial")
coef(fit_sum)
summary(fit_sum)

cor(df_summ$variety_high, df_sum_obr$av)

#графики
tab_act <- table(happiness = df_act$happiness_high, activity = df_act$activity)
mosaicplot(tab_act, color = "yellow", border = "black", main = "Активность")

tab_var <- table(happiness = df_var$happiness_high, variety = df_var$variety_high)
mosaicplot(tab_var, color = "yellow", border = "black", main = "Вариативность")

tab_sum <- table(hapiness = df_summ$happiness_high, activity = df_summ$activity, variety = df_summ$variety_high)
mosaicplot(tab_sum, color = "yellow", border = "black", main = "summ")
