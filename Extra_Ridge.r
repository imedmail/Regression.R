install.packages("ridgregextra")
library(isdals)
library(ridgregextra)

X=active_df[,-c(1,2)]
Y=active_df[,2]
# Run ridgereg_k function to get coefficients by using alternative approach to traditional ridge regression techniques.
ridgereg_k(X,Y,0,1)
ridge_reg(X,Y,0.1)
vif_k(X, Y, 0, 1)


