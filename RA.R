
df = read.csv("C:/Users/Fabian/Downloads/wage2015_subsample_inference.csv")

df

colnames(df)

df_scl = subset(df, df["scl"] == 1.0, select = colnames(df))
df_scl
nrow(df_scl)


df_clg = subset(df, df["clg"] == 1.0, select = colnames(df))
df_clg
nrow(df_clg)

sample = rbind(df_scl, df_clg)
print(sample)

nrow(sample)



# install.packages("tidyverse")
library(tidyverse)
library(ggplot2)


formula1 = "lwage ~ sex + exp1 + shs + hsg + mw + so + we + occ2 + ind2"

linear_m = lm(formula1, data=sample)
linear_m
summary(linear_m)


formula2 = "lwage ~ sex + (exp1 + exp2 + exp3 + exp4 + shs + hsg + occ2 + ind2 + mw + so + we) ** 2"
linear_m_flex = lm(formula2, data=sample)
linear_m_flex
summary(linear_m_flex)


lm_coef_sex = linear_m$coefficients[2]
error = lm_coef_sex - confint(linear_m)[2][1]
error

lm_flex_coef_sex = linear_m_flex$coefficients[2]
error_flex = lm_flex_coef_sex  - confint(linear_m_flex)[2][1]
error_flex

coef_df = data.frame("coef" = c(lm_coef_sex, lm_flex_coef_sex),
                     "err" = c(error, error_flex),
                     "varname" = c("basic_model", "flexible_model"))
coef_df


coefplot1 = function(){
  n = c("Basic Model", "Flexible Model")
  vals = c(coef_df$coef[1], coef_df$coef[2])
  errors = c(coef_df$err[1], coef_df$err[2])
  ggplot() + geom_errorbar(aes(x=n, y=vals, ymin=vals-errors,
                               ymax=vals+errors), width=.2) +
    geom_point(aes(x=n, y=vals)) + ylim(-0.13, 0) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle=90, vjust=0.5, hjust=1)) + 
    labs(title="Interval confidence of sex coefficient", x="", y="")
}


coefplot1()



# install.packages("pracma")
library(pracma)


# para df_scl
p_scl = polyfit(df_scl$exp1, df_scl$lwage, 4)

yf_scl = data.frame("x" = df_scl$exp1,
                    "y" = polyval(p_scl, df_scl$exp1))
head(yf_scl)


# para df_clg
p_clg = polyfit(df_clg$exp1, df_clg$lwage, 4)

yf_clg = data.frame("x" = df_clg$exp1,
                "y" = polyval(p_clg, df_clg$exp1))



grafico = ggplot() + ylim(c(2, 3.5)) + xlim(c(0, 40))+ theme(legend.position = "none")  +  labs(x = "Years of Experience", y = "Log Wage")

grafico = grafico + geom_point(data = df_scl, aes(exp1, lwage, colour=rgb(0.1, 0.2, 0.8)), alpha=0.05) + geom_line(data = df_scl, aes(exp1, yf_scl$y), colour = rgb(0.1, 0.2, 0.8))
grafico = grafico + geom_point(data = df_clg, aes(exp1, lwage, colour=rgb(0.8, 0.2, 0.1)), alpha=0.05) + geom_line(data = df_clg, aes(exp1, yf_clg$y), colour=rgb(0.8, 0.2, 0.1))
grafico = grafico + geom_segment(aes(x = 10, y = 2.6, xend = 9, yend = 2.779), arrow = arrow(length = unit(0.3, "cm")))
grafico = grafico + geom_segment(aes(x = 5, y = 3.27, xend = 10, yend =3.15), arrow = arrow(length = unit(0.3, "cm")))
grafico = grafico + annotate("text", x = 10, y = 2.57, label = sprintf('Some College'))
grafico = grafico + annotate("text", x = 5, y = 3.3, label = sprintf('College Graduate'))
grafico




