#########################
### Sessio 12: R Demo ###
#########################

## Paquets
library(tidyverse)

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
