
library(GGally)
library(mgcv)
library(dplyr)
library(ggplot2)

panel_edf <- function(data, mapping, ..., k = 10, min_points = 5) {
  x_var <- as.character(mapping$x)[2]
  y_var <- as.character(mapping$y)[2]
  
  if (is.null(x_var) || is.null(y_var)) {
	return(ggplot() + annotate("text", x=.5, y=.5, label="Error", color="red") + theme_void())
  }
  
  df <- data[, c(x_var, y_var), drop = FALSE]
  df <- df[complete.cases(df), , drop = FALSE]
  
  n <- nrow(df)
  n_unique_x <- length(unique(df[[x_var]]))
  
  if (n < min_points || n_unique_x < 3) {
	msg <- ifelse(n < min_points, 
				  paste("n =", n), 
				  paste("unique x =", n_unique_x))
	return(ggplot() + 
			 annotate("text", x=.5, y=.5, label=msg, color="gray", size=5) + 
			 theme_void())
  }
  
  form <- as.formula(paste(y_var," ~ s(", x_var,") -1")) #  , k =", k,"
  gam_fit <- try(gam(form, data = df, method = "REML"), silent = TRUE)
  
  if (inherits(gam_fit, "try-error") || is.null(gam_fit)) {
	edf_val <- "Error"
	r2_val <- "Error"
	p_val <- "Error"
  } else {
	sm <- summary(gam_fit)
	edf_val <- if (!is.null(sm$edf) && length(sm$edf) > 0) {
	  round(sm$edf[1], 1)
	} else {
	  round(gam_fit$edf[1], 1)
	}
	r2_val <- if (!is.null(sm$r.sq) && length(sm$r.sq) > 0) {
	  round(sqrt(sm$r.sq), 2)
	} else {
	  round(1, 0)
	}
	p_val <- if (!is.null(sm$s.table[, "p-value"]) && length(sm$s.table[, "p-value"]) > 0) {
	  round(sm$s.table[, "p-value"], 2)
	} else {
	  round(0, 0)
	}
  }

  ggplot() +
	annotate("text",
			 x = 0.5, y = 0.5,
			 label = paste("EDF=", edf_val,"(", p_val,")\nR-adj=", r2_val, "(", x_var,")"),
			 size = 4,
			 color = ifelse(is.numeric(edf_val) || is.na(r2_val) || p_val >0.1, "darkblue", "red"),
			 fontface = ifelse(is.numeric(edf_val) || is.na(r2_val) || p_val >0.1, "plain", "italic")) +
	theme_void()
}

ggpairs(
  active_df,
  columns = names(active_df),
  title = "Pairwise Relationships with GAM Smoothers",
  lower = list(
	continuous = wrap(
	  "smooth",
	  method = "gam",
	  formula = y ~ s(x) -1, #, k = 10
	  color = "steelblue",
	  fill = "lightblue",
	  alpha = 0.4
	)
  ),
  upper = list(
	continuous = panel_edf
  ),
  diag = list(
	continuous = wrap("densityDiag", fill = "gray", alpha = 0.9)
  )
) + theme_minimal(base_size = 11) +
  theme(
	axis.text.x = element_text(angle = 90, hjust = 0, size=5),
	axis.text.y = element_text(angle = 0, hjust = 0, size=5),
	plot.title = element_text(hjust = 0.5, face = "bold", size=15),
	text = element_text(family = "Arial")
	)

