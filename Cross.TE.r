if (!require("RTransferEntropy")) install.packages("RTransferEntropy")
if (!require("future")) install.packages("future")

library(RTransferEntropy)
library(future)
plan(multisession)

te_test <- function(X, Y, lx = 1, ly = 1, q = 0.1, shuffles = 200) {
 result <- transfer_entropy(
	x = X,
	y = Y,
	lx = lx,
	ly = ly,
	q = q,
	nboot = shuffles,
	quiet = TRUE
 )
 # te-value Transfer Entropy <- $coef[1,1]
 # p-value <- $coef[1,4]
 return(result)
}

df <- read.csv("algeria_data.csv", stringsAsFactors = FALSE)
df <- df[order(df$year), ]
Reg.var <- Fun_get_var_name("RGDP.Growth TotExp.Per.TotRev Taxes_prog RevHydroRate TotRev_prog RealFunExp_prog RealEquiExp_prog TotExp_prog")
active_df <- df[, colnames(df) %in% Reg.var, drop = FALSE]

n <- ncol(active_df)
Tab <- matrix(NA, nrow = n, ncol = n, dimnames = list(colnames(active_df), colnames(active_df)))

for(i in 1:n) {
	for(j in 1:n) {
	 if(i == j) {
		Tab[i,j] <- 1.000
	 } else {
		YX <- na.omit(cbind(active_df[,j], active_df[,i]))
		Y <- YX[,1]
		X <- YX[,-1]
		test_result <- te_test(X, Y, lx = 1, ly = 1, shuffles = 300)
		Tab[i,j] <- paste0(round(test_result$coef[1,1],3), " (",round(test_result$coef[1,4],3),")")
	 }
	}
}
Tab <- as.data.frame(Tab)
print(Tab)

