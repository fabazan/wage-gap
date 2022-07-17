
# install.packages("tidyverse")
library(tidyverse)

# getwd()
username = unlist(strsplit(dirname("~"), "/"))[3]
root = paste0("/Users/", username, "/workspace/wage-gap/")


df = read.csv(paste0(root, "wage2015_subsample_inference.csv"))
df

df_scl = subset(df, df["scl"] == 1.0, select=colnames(df))
nrow(df_scl)


df_clg = subset(df, df["clg"] == 1.0, select=colnames(df))
nrow(df_clg)

sample = rbind(df_scl, df_clg)
nrow(sample)
print(sample)


formula_basic = "lwage ~ sex + exp1 + shs + hsg + mw + so + we + occ2 + ind2"
formula_flex = "lwage ~ sex + (exp1 + exp2 + exp3 + exp4 + shs + hsg + occ2 + ind2 + mw + so + we) ** 2"

linear_model_basic = lm(formula_basic, data=sample)
summary(linear_model_basic)

linear_model_flex = lm(formula_flex, data=sample)
summary(linear_model_flex)


# Gráficos estadísticos
#


coefplot = function(lmb, lmf){
  coefs = c(lmb$coefficients[2], lmf$coefficients[2])
  errors = c(lmb$coefficients[2] - confint(lmb)[2][1],
             lmf$coefficients[2] - confint(lmf)[2][1])
  varname = c("Basic Model", "Flexible Model")
  ggplot() + geom_errorbar(aes(x=varname, y=coefs, ymin=coefs-errors,
                               ymax=coefs+errors), width=.1) +
    geom_point(aes(x=varname, y=coefs)) + ylim(-0.13, 0) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle=90, vjust=0.5, hjust=1)) + 
    labs(title="Interval confidence of sex coefficient", x="", y="")
  ggsave(path=root, filename="coefplot_r.png", width=685/96, height=480/96, device='png', dpi=100)
}


coefplot(linear_model_basic, linear_model_flex)


# install.packages("pracma")
library(pracma)


poly_regression = function(df, degree){
  p = polyfit(df$exp1, df$lwage, 4)
  output = data.frame("x" = df$exp1,
                      "y" = polyval(p, df$exp1))
  return(output)
}


reg_line_scl = poly_regression(df_scl, 4)
reg_line_clg = poly_regression(df_clg, 4)


graph = ggplot() + ylim(c(2, 3.5)) + xlim(c(0, 40)) +
          labs(x="Years of Experience", y="Log Wage", title="Wage and Experience for College and High School Graduates") +
          theme(plot.title = element_text(hjust = 0.5))

graph = graph + geom_point(data=df_scl, aes(exp1, lwage, shape="Actual"), fill=rgb(0.9, 0.9, 0.9), alpha=0.02, size=3) +
          geom_line(data=reg_line_scl, aes(x, y, color="Fitted"), linetype="dashed")
graph = graph + geom_point(data=df_clg, aes(exp1, lwage), fill=rgb(0.9, 0.9, 0.9), alpha=0.02, size=3) +
          geom_line(data=reg_line_clg, aes(x, y), linetype="dashed")
graph = graph + geom_segment(aes(x=10, y=2.6, xend=9, yend=2.779), arrow=arrow(length=unit(0.3, "cm")))
graph = graph + geom_segment(aes(x=5, y=3.27, xend=10, yend=3.15), arrow=arrow(length=unit(0.3, "cm")))
graph = graph + annotate("text", x=10, y=2.57, label=sprintf('Some College'))
graph = graph + annotate("text", x=5, y=3.3, label=sprintf('College Graduate'))
graph = graph + scale_color_manual(values=c("Fitted"=rgb(0.2, 0.2, 0.2)))
graph = graph + theme(legend.position=c(0.9, 0.35), legend.title = element_blank())
graph
ggsave(path=root, filename="wageexpforclgandscl_r.png", width=900/96, height=600/96, device='png', dpi=100)




