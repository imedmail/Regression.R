# Load required package
library(mgcv)

# fit the model with increased basis dimension (k)
# Increase k to 15 for all smooth terms to allow more flexibility
model_increased_k <- gam(RGDP.PER.CAP.LC ~ s(RealFunExp, k = 15) + 
						  s(RealEquiExp, k = 15) + 
						  s(TotExp, k = 15) + 
						  s(RevHydroRate, k = 15) + 
						  s(Taxes, k = 15) + 
						  s(TotRev, k = 15),
						data = as.data.frame(Rawdf), 
						family = gaussian(link = "identity"),
						method = "GCV.Cp", 
						optimizer = "magic")

# Print summary of model
summary(model_increased_k)
# Check basis dimension diagnostics for model
gam.check(model_increased_k)

# Simplify the model by keeping only significant terms using GCV.cp
# Based on original output, keep RealFunExp and RealEquiExp (p < 0.05)
model_simplified <- gam(RGDP.PER.CAP.LC ~ s(RealFunExp, k = 15) + 
						 s(RealEquiExp, k = 15),
					   data = as.data.frame(Rawdf), 
					   family = gaussian(link = "identity"),
					   method = "GCV.Cp", 
					   optimizer = "magic")

# Print summary of simplified model
summary(model_simplified)
# Check basis dimension diagnostics for simplified model
gam.check(model_simplified)

# Visualize smooth terms for significant predictors
par(mfrow = c(1, 2))  # Set up plot layout
plot(model_simplified, select = 1, main = "Smooth for RealFunExp", shade = TRUE)
plot(model_simplified, select = 2, main = "Smooth for RealEquiExp", shade = TRUE)

# Compare models using AIC
AIC(gam_model, model_simplified)

draw(gam_model) +
  labs(title = "Smooth Effects of Fiscal Policy on RGDP.Per.CAP.LC", y = "Effect on RGDP.Per.CAP.LC") + 
  theme_minimal()

# refit models using REML method and re-check
model_simplified <- gam(RGDP.PER.CAP.LC ~ s(RealFunExp) + s(RealEquiExp),
						data = Rawdf,
						method = "REML")
summary(model_simplified)
gam.check(model_simplified)
