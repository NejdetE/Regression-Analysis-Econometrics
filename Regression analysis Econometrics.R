#+eval=FALSE
#Packages
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, cowplot, ggvis, 
               httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr, car, stargazer, L1pack, lmtest, LadR,
               AER, mfx, ggforce, dynlm, prais, ggdist, distributional, 
               forecast, fpp2, tseries, FinTS, rugarch, xts, AICcmodavg, 
               MuMIn, plotly, officer, gganimate, gifski, magick, 
               htmlwidgets, modelsummary, gtsummary, ggthemes, esquisse, 
               kableExtra, gt, sas7bdat, ggtext, patchwork)
webshot::install_phantomjs()

#Exercise 1
library(readxl)
Units <- read_excel("xlsx-file")

Units_OLS <- lm(value ~ area, data = Units)
summary(Units_OLS)
OLS_Regression <- ggplot(Units_OLS, aes(x=area, y=value)) + 
                  geom_point(size = 2.5) + 
                  geom_smooth(method="lm", se = FALSE, col = "red") +
                  ggtitle("Level-level model")
OLS_Regression
ggplotly(OLS_Regression)

Units_WLS <- lm(value ~ area, data = Units, weights = 1/area)
summary(Units_WLS)

#modelsummary package
models <- list(
  "OLS" = lm(value ~ area, data = Units),
  "WLS" = lm(value ~ area, data = Units, weights = 1/area)
)
msummary(models) %>% 
  row_spec(1, bold = T, italic = T, col = "black", background = "orange") %>%
  row_spec(2, bold = T, italic = T, col = "white", background = "darkblue") %>% 
  row_spec(3, bold = T, italic = T, col = "black", background = "orange") %>% 
  row_spec(4:11, bold = T, italic = T, col = "white", background = "darkblue")
  
msummary(models, statistic = "statistic")
msummary(models, statistic = "p.value")
msummary(models, statistic = "conf.int", conf_level = .95)

msummary(models) %>% 
  tab_style(style = cell_fill(color = "lightcyan"),
            locations = cells_body(columns = vars("OLS"))) %>% 
  tab_style(style = cell_fill(color = "#F9E3D6"),
            locations = cells_body(columns = vars("WLS")))

#gtsummary package
tbl.OLS <- tbl_regression(Units_OLS, exponentiate = T)
tbl.WLS <- tbl_regression(Units_WLS, exponentiate = T)

tbl_merge(
  tbls = list(tbl.OLS, tbl.WLS)
  
)
############
bptest(Units_OLS)
bptest(Units_WLS)

stargazer::stargazer(Units_OLS, Units_WLS,  
                     title = "Table1:Results", 
                     type = "html", 
                     out = "Exercise1Regs.html",
                     column.labels = c("OLS", "WLS"))

new.values <- data.frame(area = c(75.9, 
                                  119.30, 
                                  151, 
                                  192.21, 
                                  232.4, 
                                  300.9))

predict(Units_WLS, new.values, interval = "prediction")

#Exercise 2
#Regressions and table
n  <- 1280
x1 <- rgamma(n, 4/9, scale = 108)
x2 <- rpois(n, 3)
ux <- rnorm(n, mean = 0, sd = 5.5)
x3 <- 40 + 0.1*x1 + ux
x4 <- rgamma(n, 3/10, scale = 89)
u  <- rnorm(n, mean = 0, sd = 80)
y  <- 5 + 2*x1 - 50*x2 + x3 + u

Reg_1 <- lm(y ~ x1)
Reg_2 <- lm(y ~ x1 + x2)
Reg_3 <- lm(y ~ x1 + x2 + x3) 
Reg_4 <- lm(y ~ x1 + x2 + x3 + x4)

stargazer::stargazer(Reg_1, Reg_2, Reg_3, Reg_4, 
                     title = "Table3:Results", 
                     type = "html", 
                     out = "Exercise2Regs.html")

#Hypothesis testing for Regression 3 and 4
linearHypothesis(Reg_3, c("(Intercept)=5", "x1=2", "x2=-50", "x3=1"), 
                 test = "F", level=0.95)
confint(Reg_4, 'x4', level=0.95)

#Test adding x4
y_test  <- 5 + 2*x1 - 50*x2 + x3 + x4 + u
Reg_Test <- lm(y_test ~ x1 + x2 + x3 + x4)
confint(Reg_Test, 'x4', level=0.95)

#FGLS
#1
Units_OLS <- lm(value ~ area, data = Units)
summary(Units_OLS)
OLS_res <- Units_OLS$residuals
#2
OLS_res.sq <- (OLS_res)^2
log.OLS_res.sq <- log(OLS_res.sq)
#3
OLS_log.res.sq <- lm(log.OLS_res.sq ~ area, data = Units)
fitted_val <- OLS_log.res.sq$fitted.values
#4
exponentiate_fitted.val <- exp(fitted_val)
#5
Units_FGLS <- lm(value ~ area, data = Units, weights = 1/exponentiate_fitted.val)
summary(Units_FGLS)

predict(Units_FGLS, new.values, interval = "prediction")
