# Example7
# Exercises Q5 Chapter 14 of Veronesi
#clear the workspace and functions
rm(list=ls())
rt =VeronesiTable14p7q5$rt
Delta = 1/252
N = length(rt)
y = rt[2:N]
x = rt[1:(N-1)]
model = lm(y~x)
mle.gamma= -log(as.numeric(model$coefficients)[2])/Delta
mle.rbar = as.numeric(model$coefficients)[1]/
  (1-as.numeric(model$coefficients)[2])
mle.sigma = sigma(model)*(2*mle.gamma/
                            (1-as.numeric(model$coefficients)[2]^2))^.5