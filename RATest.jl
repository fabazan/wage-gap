
using CSV
using DataFrames
using GLM
using Plots

username = splitdir(homedir())[end]
dirpath = "/Users/$username/workspace/wage-gap"

df = CSV.read("$dirpath/wage2015_subsample_inference.csv", DataFrame);
# println(first(df, 8))
println("Total number of instances in df (unmodified, raw data): " * string(length(df.rownames)))

df_scl = filter(:scl => ==(1), df);
df_clg = filter(:clg => ==(1), df);

sample = [df_scl; df_clg];
println("Total number of instances in sample (filtered by (scl or clg == 1)): " * string(length(sample.rownames)))

# Gráficos estadísticos
#

linear_model_basic = lm(@formula(lwage ~ sex + exp1 + shs + hsg + mw + so + we + occ2 + ind2), sample)
println(linear_model_basic)

linear_model_flex = lm(@formula(lwage ~ sex + exp1 + exp1*exp2 + exp1*exp3 + exp1*exp4 + exp1*hsg + exp1*ind2 + exp1*mw + exp1*occ2 + exp1*shs + exp1*so + exp1*we +
                    exp2 + exp2*exp3 + exp2*exp4 + exp2*hsg + exp2*ind2 + exp2*mw + exp2*occ2 + exp2*shs + exp2*so + exp2*we +
                    exp3 + exp3*exp4 + exp3*hsg + exp3*ind2 + exp3*mw + exp3*occ2 + exp3*shs + exp3*so + exp3*we +
                    exp4 + exp4*hsg + exp4*ind2 + exp4*mw + exp4*occ2 + exp4*shs + exp4*so + exp4*we +
                    hsg + hsg*ind2 + hsg*mw + hsg*occ2 + hsg*shs + hsg*so + hsg*we +
                    ind2 + ind2*mw + ind2*occ2 + ind2*shs + ind2*so + ind2*we +
                    mw + mw*occ2 + mw*shs + mw*so + mw*we +
                    occ2 + occ2*shs + occ2*so + occ2*we +
                    shs + shs*so + shs*we +
                    so + so*we +
                    we), sample)
println(linear_model_flex)

function coefplot(lmb, lmf)
    varname = ["Basic Model", "        Flexible Model"]
    coefs = [coef(lmb)[2], coef(lmf)[2]]
    error = [stderror(lmb)[2], stderror(lmf)[2]]
    scatter(varname, coefs,
        legend = false,
        yerror = 1.96 .* error,
        title = "Interval confidence of sex coefficient",
        xrotation = 90,
        xlim = (0.3, 1.8),
        ylim = (-0.13, 0))
    savefig("$dirpath/coefplot_jl.png")
end


function poly_regression(df, degree)
    z = np.polyfit(df[!, "exp1"], df[!, "lwage"], degree)
    p = np.poly1d(z)
    output = DataFrame("x" => df[!, "exp1"], "y" => p(df[!, "exp1"]))
    output = sort(output, [order("x")])
    return output
end


coefplot(linear_model_basic, linear_model_flex)


using PyCall

np = pyimport("numpy")
plt = pyimport("matplotlib.pyplot")


reg_line_scl = poly_regression(df_scl, 4)
reg_line_clg = poly_regression(df_clg, 4)


fig, ax = plt.subplots(figsize=(10, 7))
ax = plt.gca()
ax.set_ylim([2, 3.5])
ax.set_xlim([0, 40])

# For Some college:
plt.scatter(df_scl[!, "exp1"], df_scl[!, "lwage"], color=(0.9, 0.9, 0.9, 0.4), label="Actual")
plt.plot(reg_line_scl[!, "x"], reg_line_scl[!, "y"], "--", color=(0.2, 0.2, 0.2), label="Fitted")
ax.annotate("College Graduated", xy=(10, 3.15), xytext=(5, 3.27), arrowprops = Dict("facecolor" => "black", "shrink" => 0.01))

# For College graduated:
plt.scatter(df_clg[!, "exp1"], df_clg[!, "lwage"], color=(0.9, 0.9, 0.9, 0.4), label="_nolegend_")
plt.plot(reg_line_clg[!, "x"], reg_line_clg[!, "y"], "--", color=color=(0.2, 0.2, 0.2), label="_nolegend_")
ax.annotate("Some College", xy=(9, 2.779), xytext=(7, 2.6), arrowprops = Dict("facecolor" => "black", "shrink" => 0.01))

plt.ylabel("Log Wage")
plt.xlabel("Years of Experience")
plt.title("Wage and Experience for College and High School Graduates")
plt.legend()
plt.savefig("$dirpath/wageexpforclgandscl_jl.png")
plt.show()


