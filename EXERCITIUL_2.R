#REGRESIE LINIARA SIMPLA

data("stackloss")
library(ggplot2)
#Relatia dintre stack.loss(variabila dependenta) si Air.Flow(variabila independenta)
#Calculam corelatia dintre cele doua variabile de mai sus
cor(stackloss$Air.Flow, stackloss$stack.loss)

#Construim modelul liniar
linearMod1 <- lm(stack.loss ~ Air.Flow, data=stackloss)
print(linearMod1)

#stack.loss = -44.13 + 1.02 * Air.Flow
summary(linearMod1)

ggplot(stackloss, aes(x = Air.Flow, y = stack.loss)) +
  geom_point() +
  stat_smooth(method = lm)


#Relatia dintre stack.loss(variabila dependenta) si Water Temp(variabila independenta)
#Calculam corelatia dintre cele doua variabile de mai sus

cor(stackloss$Water.Temp, stackloss$stack.loss)

#construim modelul liniar
linearMod2 <- lm(stack.loss ~ Water.Temp, data=stackloss)
print(linearMod2)

#stack.loss = -41.911 + 2.817 * Water.Temp

summary(linearMod2)

ggplot(stackloss, aes(x = Water.Temp, y = stack.loss)) +
  geom_point() +
  stat_smooth(method = lm)

#CALCUL RESIDUAL STANDARD ERROR
sigma(linearMod2)/mean(stackloss$stack.loss)
sigma(linearMod1)/mean(stackloss$stack.loss)

#CALCUL AIC
AIC(linearMod1)
AIC(linearMod2)

#REGRESIE LINIARA MULTIPLA

model1<- lm(stack.loss ~ ., data = stackloss)
summary(model1)
modelCoefs1 <- summary(model1)$coefficients
print(modelCoefs1)

#ELIMINAM VARIABILA ACID.CONC
model2 <- update(model1, ~. -Acid.Conc.)
sigma(model2)/mean(stackloss$stack.loss) # CALCUL RESIDUAL STANDARD ERROR
summary(model2)
modelCoefs2 <- summary(model2)$coefficients
print(modelCoefs2)

#CALCUL AIC
AIC(linearMod1)
AIC(model2)

#ADAUGARE VARIABILA
m <- mean(stackloss$Water.Temp)
sd <- sd(stackloss$Water.Temp)
vec <- c(stackloss$Water.Temp)
x <- c(vec)
y <- dnorm(x, m, sd)
plot(x,y,sub = paste("WATER.TEMP"))

l <- rnorm(21,m,sd)

stackloss$light <- l
vec <- c(stackloss$light)
x2 <- c(vec)
y2 <- dnorm(x2, m, sd)
plot(x2,y2,sub = paste("LIGHT"))

