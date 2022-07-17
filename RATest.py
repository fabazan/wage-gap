
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

import statsmodels.formula.api as smf
from scipy import stats

df = pd.read_csv("wage2015_subsample_inference.csv", encoding="utf-8")
print(f"Total number of instances in df (unmodified, raw data): {df.shape[0]}")

df_scl = df.loc[df["scl"] == 1]
df_clg = df.loc[df["clg"] == 1]

sample = pd.concat([df_scl, df_clg])
print(f"Total number of instances in sample (filtered by (scl or clg == 1)): {sample.shape[0]}")

# Gráficos estadísticos
#

formula_basic = "lwage ~ sex + exp1 + shs + hsg + mw + so + we + occ2 + ind2"
formula_flex = "lwage ~ sex + (exp1 + exp2 + exp3 + exp4 + shs + hsg + occ2 + ind2 + mw + so + we) ** 2"

linear_model_basic = smf.ols(formula_basic, data=sample)
linear_model_basic = linear_model_basic.fit()
print(linear_model_basic.summary())

linear_model_flex = smf.ols(formula_flex, data=sample)
linear_model_flex = linear_model_flex.fit()
print(linear_model_flex.summary())


coef_df = pd.DataFrame({"varname": ["basic_model", "flexible_model"],
                        "coef": [linear_model_basic.params[1], linear_model_flex.params[1]],
                        "error": [linear_model_basic.params[1] - linear_model_basic.conf_int()[0][1],
                                  linear_model_flex.params[1] - linear_model_flex.conf_int()[0][1]]})

print(coef_df)

fig, ax = plt.subplots(figsize=(7, 5))
coef_df.plot(x="varname", y="coef", kind="bar",
             ax=ax, color="none", yerr="error", legend=False)
plt.title("Interval confidence of sex coefficient")
ax.set_ylabel("")
ax.set_xlabel("")
ax.scatter(x=np.arange(coef_df.shape[0]), marker="s",
           s=120, y=coef_df["coef"], color="black")
ax.xaxis.set_ticks_position("none")
ax.set_xticklabels(["Basic Model", "Flexible Model"],
                   rotation=90, fontsize=9)
plt.savefig("coefplot_py.png")
plt.show()


def poly_regression(df, degree):
    z = np.polyfit(df["exp1"], df["lwage"], degree)
    p = np.poly1d(z)
    output = pd.DataFrame({"x": df["exp1"], "y": p(df["exp1"])})
    output = output.sort_values(by="x", ascending=True)
    return output


reg_line_scl = poly_regression(df_scl, 4)
reg_line_clg = poly_regression(df_clg, 4)


fig, ax = plt.subplots(figsize=(10, 7))
ax = plt.gca()
ax.set_ylim([2, 3.5])
ax.set_xlim([0, 40])

plt.scatter(df_scl["exp1"], df_scl["lwage"], color=(0.9, 0.9, 0.9, 0.4), label="Actual")
plt.plot(reg_line_scl["x"], reg_line_scl["y"], "--", color=(0.2, 0.2, 0.2), label="Fitted")
ax.annotate("College Graduated", xy=(10, 3.15), xytext=(5, 3.27), arrowprops=dict(facecolor="black", shrink=0.01))

plt.scatter(df_clg["exp1"], df_clg["lwage"], color=(0.9, 0.9, 0.9, 0.4), label="_nolegend_")
plt.plot(reg_line_clg["x"], reg_line_clg["y"], "--", color=(0.2, 0.2, 0.2), label="_nolegend_")
ax.annotate("Some College", xy=(9, 2.779), xytext=(7, 2.6), arrowprops=dict(facecolor="black", shrink=0.01))

plt.ylabel("Log Wage")
plt.xlabel("Years of Experience")
plt.title("Wage and Experience for College and High School Graduates")
plt.legend()
plt.savefig("wageexpforclgandscl_py.png")
plt.show()




