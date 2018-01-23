########################
# R Script  Sessio 5   #
########################

# carrega dades
D = read.csv2('shoesize.csv')

# explora dades
head(D)

# explora "Height"
quantile(D$Height)
mean(D$Height)
sd(D$Height)
hist(D$Height)
plot(density(D$Height))

# troba l'estimació puntual i interval de confiança de "Height"

# quina és la relació entre "Height" and "Size"?

plot(D$Size, D$Height)

M = lm(D$Height~D$Size)
summary(M)

M = lm(D$Height~D$Size+D$Gender)
summary(M)

