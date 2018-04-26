library('MASS') # набор данных Boston 
library('splines') # сплайны 
library('gam') # обобщённые аддитивные модели 
library('akima') # график двумерной плоскости 
library('ggplot2') # красивые графики 

my.seed <- 1 
attach(Boston) 
str(Boston) 

gp <- ggplot(data = Boston, aes(x = dis, y = crim)) 
gp <- gp + geom_point() + geom_abline(slope = 0, intercept = 30, col = 'red') 
gp


#полином 3ой степени
fit <- lm(crim ~ poly(dis, 3), data = Boston)
round(coef(summary(fit)), 2)
# границы изменения переменной nox
dislims <- range(dis)
# значения nox, для которых делаем прогноз 
dis.grid <- seq(from = dislims[1], to = dislims[2], length.out = 506)
# рассчитать прогнозы и их стандартные ошибки
preds <- predict(fit, newdata = list(dis = dis.grid), se = T)

# границы доверительного интервала для уровня пеступности
se.bands <- cbind(lower.bound = preds$fit - 2*preds$se.fit,
                  upper.bound = preds$fit + 2*preds$se.fit)
# смотрим результат
round(head(se.bands), 2)

par(mfrow=c(1,1))
plot(dis, crim, xlim = dislims, cex = 0.5, col = 'darkgrey')

title('Локальная регрессия')

# подгоняем модель c окном 0.8
fit1 <- loess(crim ~ dis, span = 0.8, data = Boston)


# рисум модели
lines(dis.grid, predict(fit1, data.frame(dis = dis.grid)),
      col = 'red', lwd = 2)

# доверительные интервалы прогноза
matlines(x = dis.grid, y = se.bands, lwd = 1, col = 'black', lty = 3)
# легенда
legend('topright', 
       c('s = 0.8'),
       col = c('red'), lty = 1, lwd = 2, cex = 0.8)

gam.lo <- gam(crim ~  lo(dis, span = 0.7) , 
              data = Boston)
par(mfrow = c(1, 1))
plot(gam.lo, se = T, col = 'green')

summary(gam.lo)
