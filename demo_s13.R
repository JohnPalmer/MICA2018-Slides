#########################
### Sessio 13: R Demo ###
#########################

## Paquets
library(tidyverse)
library(rgdal)
library(spdep)
library(RColorBrewer)

## Dades
brut = read_csv("bruticia.csv") %>% select(X="X Coordinate (State Plane)", Y="Y Coordinate (State Plane)") %>% filter(!is.na(X) & !is.na(Y))

llum = read_csv("llum.csv") %>% select(X="X Coordinate (State Plane)", Y="Y Coordinate (State Plane)") %>% filter(!is.na(X) & !is.na(Y))

D = read_csv("NYC_combo_tab.csv") %>% select(ZIPCODE, POPULATION, N_dirty_co, N_light_co, N_assaults) %>% group_by(ZIPCODE) %>% summarise(pop=sum(POPULATION), brut=sum(N_dirty_co), llum=sum(N_light_co), assalts=sum(N_assaults)) %>% mutate(pop1=pop+1, assalt_per_pop=assalts/pop1)

E = read_csv("entrades_codi_postal.csv") %>% group_by(ZIPCODE) %>% summarise(entrades=sum(N_entrades)) 

## Agrupament
clusters = kmeans(brut,9)

plot(brut, col=clusters$cluster, pch=20)

clusters

sum(clusters$withinss)

# Quin es el millor valor per k? Probem valors differents:
K=20
ss = rep(NA, K)
for (i in 1:K) {
  clusters=kmeans(brut, centers=i)
  ss[i] = sum(clusters$withinss)
}

plot(1:K, ss, type="b")

clusters = kmeans(brut,15)
plot(brut, col=clusters$cluster, pch=20)

# Ara proba amb llums


## Unions
D = left_join(D, E, by="ZIPCODE")

# Models
ggplot(D, aes(x=brut, y=assalt_per_pop)) + geom_point( )

M = lm(assalt_per_pop~brut, data=D)
summary(M)

# ara proba llum i entrades tambe
M = lm(assalt_per_pop~brut+llum+entrades, data=D)
summary(M)

# read polygons
zipcode_polygons = readOGR("nyc_zipcodes.geojson", "OGRGeoJSON")

zipcodes = merge(zipcode_polygons, D, by="ZIPCODE")

zipcodes$popdensitat = zipcodes$pop/zipcodes$AREA

spplot(zipcodes, "assalts")
spplot(zipcodes, "pop")
spplot(zipcodes, "popdensitat", at=seq(0,60, 1))
spplot(zipcodes, "assalt_per_pop")
spplot(zipcodes, "llum")
spplot(zipcodes, "brut")
spplot(zipcodes, "entrades")

nb = poly2nb(zipcodes)
lw <- nb2listw(nb, zero.policy=TRUE)


plot(zipcodes)
plot(nb, coordinates(zipcodes), add=TRUE)

M = lm(assalt_per_pop~ brut+llum+entrades, data=zipcodes)
summary(M)

grps <- 10
brks <- quantile(zipcodes$residuals, 0:(grps-1)/(grps-1), na.rm=TRUE)
spplot(zipcodes, "residuals", at=brks, col.regions=rev(brewer.pal(grps, "RdBu")), col="transparent")

zipcodes$residuals = residuals(M)
moran.test(zipcodes$residuals, lw, zero.policy = TRUE)
moran.mc(zipcodes$residuals, lw, 999, zero.policy = TRUE)


M = lagsarlm(assalt_per_pop~brut+llum+entrades, listw=lw, data=zipcodes, zero.policy = TRUE, tol.solve=1.0e-30)
summary(M)

zipcodes$residuals = residuals(M)

grps <- 10
brks <- quantile(zipcodes$residuals, 0:(grps-1)/(grps-1), na.rm=TRUE)
spplot(zipcodes, "residuals", at=brks, col.regions=rev(brewer.pal(grps, "RdBu")), col="transparent")

moran.test(zipcodes$residuals, lw, 999, zero.policy = TRUE)
moran.mc(zipcodes$residuals, lw, 999, zero.policy = TRUE)

M = lagsarlm(assalt_per_pop~brut+llum+entrades, listw=lw, data=zipcodes, type="mixed", zero.policy = TRUE, tol.solve=1.0e-30)
summary(M)

zipcodes$residuals = residuals(M)

grps <- 10
brks <- quantile(zipcodes$residuals, 0:(grps-1)/(grps-1), na.rm=TRUE)
spplot(zipcodes, "residuals", at=brks, col.regions=rev(brewer.pal(grps, "RdBu")), col="transparent")

moran.mc(zipcodes$residuals, lw, 999, zero.policy = TRUE)
moran.test(zipcodes$residuals, lw, zero.policy = TRUE)


M = errorsarlm(assalt_per_pop~brut+llum+entrades, listw=lw, data=zipcodes, zero.policy = TRUE, tol.solve=1.0e-30)
summary(M)

zipcodes$residuals = residuals(M)

grps <- 10
brks <- quantile(zipcodes$residuals, 0:(grps-1)/(grps-1), na.rm=TRUE)
spplot(zipcodes, "residuals", at=brks, col.regions=rev(brewer.pal(grps, "RdBu")), col="transparent")

moran.mc(zipcodes$residuals, lw, 999, zero.policy = TRUE)
moran.test(zipcodes$residuals, lw, zero.policy = TRUE)
