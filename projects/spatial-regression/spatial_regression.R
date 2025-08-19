# Spatial regression
# test for spatial autocorrelation in ols residuals, and test alternative models: 
# spatial autoregressive (SAR) and spatial error models (SEM)

# load packages
library(tidyverse)
library(sf)
library(tmap)
library(GGally)
library(cartography)
library(RColorBrewer)
library(ggspatial)
library(spatialreg)
library(texreg)
library(spdep)

# running slow? turn off spherical geometry,
# sf::sf_use_s2(FALSE)

# read data
df <- readRDS("Data/data_spatial_africa_mortality.rds")
dim(df)

# check geometry
# plot(st_geometry(df))  # will be slow due to detailed polygons

# neighbours ---------------
# Identify neighbors
df2 <- st_make_valid(df)

# remove missing
df2 <- df2 %>%
  filter(!is.na(nevents) & !is.na(u5mr))

# construct list of neighbours from polygons
dat_nb <- spdep::poly2nb(pl = df2,queen = TRUE)  # queen neighbours (one boundary point) 
summary(dat_nb)  # descriptive statistics of neighbour list
str(dat_nb)

# plot
plot(st_geometry(df2), border="grey") # plot sf and neighbors

# plot map with the list of neighbours (dat_nb)
plot(dat_nb, st_geometry(df2), add=TRUE)


# create spatial weights for neighbours
dat_listw_w <- spdep::nb2listw(neighbours = dat_nb, 
                               style = "C", # choose style: B, W, C etc?
                               zero.policy = TRUE)

# What's the global spatial autocorrelation? 
# run Moran's I on the variable under 5 mortality rate
moran.test(df2$u5mr, 
           listw = dat_listw_w, 
           zero.policy = TRUE, na.action = na.omit)
# output: 0.85, far larger than the near-0 null expectation, tiny p-value.
# conclusion: under-5 mortality rates are strongly positively spatially autocorrelated.

# spatial autocorrelation ------------------------------------------------------
# explore spatial autocorrelation using different neighbor definitions and variables


# linear regression model ------------------------------------------------------
# simple OLS model
ols1 <- lm(nevents ~ u5mr, data = df2)

# plot the residuals (unexplained variance)
df2$residuals <- residuals(ols1)
tm_shape(df2) + 
  tm_fill(col = "residuals", palette = "RdBu", n = 9) + 
  tm_layout(title = "Residuals of OLS model: The effect of mortality rate on events")
# it looks like there's clear spatial autocorrelation in the residuals

# test for spatial autocorrelation in the residuals
lm.morantest(model = ols1, listw = dat_listw_w, zero.policy = TRUE)
# Moran's I ≈ 0.176 >> expectation (-0.003), highly significant. 
# conclusion: the residuals are not randomly spatially distributed, and the OLS model misses the spatial data structure.


# Spatial regression models ----------------------------------------------------
# Spatial lag model
sar.fit <- lagsarlm(nevents ~ u5mr, 
                    df2, listw = dat_listw_w, 
                    na.action = na.omit, zero.policy = TRUE)

# Spatial error model
sem.fit <- errorsarlm(nevents ~ u5mr, 
                      df2, listw = dat_listw_w, 
                      na.action = na.omit, zero.policy = TRUE)

screenreg(list(ols1, sar.fit, sem.fit)) # LR test


# Explore the models ------------------------------------
AIC(ols1, sar.fit, sem.fit)  # model fit is worse on OLS, similar on SAR and SEM 

# ols: fix assumptions before testing
dat_listw_w <- spdep::nb2listw(dat_nb, style = "W", zero.policy = TRUE)
# run tests
lm.LMtests(model = ols1, listw = dat_listw_w, zero.policy = TRUE, test = c("LMerr", "LMlag","RLMerr", "RLMlag"))
# conclusion: mixed dependence? both the simple lag and error tests are significant, but the robust ones are not.
# the jury's out on whether a spatilal lag (SAR) or error model (SEM) make the best fit.

# Assess impacts in spatial lag models -------------------------------------------------
spatialreg::impacts(obj = sar.fit, listw = dat_listw_w) # direct: avg change in Y given 1 unit increase in X.

# impacts:
# Direct Impact (4.194185): This value represents the average change in the dependent variable (the number of events, nevents) 
# in a given location due to a one-unit change in the independent variable (under-5 mortality rate, u5mr), 
# holding the values of u5mr constant in all other locations.
# In simpler terms, it's the impact that changes in the under-5 mortality rate 
# have on the number of events in the same area. The positive value indicates that as the under-5 mortality rate increases, 
# the number of events in that same location tends to increase.


# Indirect Impact (Spillover Effect)
# Indirect Impact (1.952575): This value quantifies the spillover effect or the impact of changing the under-5 mortality rate 
# in one location on the number of events in neighboring locations. It captures the spatial externalities or the influence that 
# a unit's characteristics have on neighboring units. The positive indirect impact suggests that an increase in the 
# under-5 mortality rate in one area not only affects that area but also leads to an increase in the number of events in surrounding areas.


# Total Impact (6.146759): This is the sum of the direct and indirect impacts, representing the overall effect of a change 
# in the under-5 mortality rate on the number of events, accounting for both the local and neighboring effects. 
# The positive total impact indicates that, overall, higher under-5 mortality rates are associated with a higher number of events 
# across the study area, considering both direct and spillover effects.

