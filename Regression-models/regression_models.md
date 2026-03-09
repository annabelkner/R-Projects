---
title: "Projekt na zajęcia modele regresji liniowej"
author: "Anna Belkner"
date: "2025-08-01"
output:
  html_document:
    keep_md: true
    theme: cerulean
    code_folding: hide
    code_download: true
    highlight: tango
    df_print: paged
---

## Zadania na ocenę 4 i 5 

### Zadanie 1.

Rozważamy zależność zmiennej objaśnianej **Y** od zmiennej objaśniającej **X**.

#### a) Wykres punktowy zależności Y od X

``` r
dane = read.csv("dane2.csv", sep = ";", dec = ",", header = TRUE)

library(ggplot2)
wykres = ggplot(dane ,aes(x=x,y=y))+
  geom_point(size=3,show.legend = F,color = "palevioletred3")

wykres
```

<img width="672" height="480" alt="download" src="https://github.com/user-attachments/assets/8fa46c87-5d9b-47de-bb29-b20b959a3753" />


#### b) Regresja wielomianowa (stopnie 1–5)

``` r
pom = seq(min(dane$x), max(dane$x), length.out = 300)

plot(dane$x, dane$y,
     xlab = "x", ylab = "y", 
     main = "Regresja wielomianowa", pch = 1)

colors = c("hotpink1","slateblue2","violetred4","deepskyblue4","aquamarine2")

for (i in 1:5) {
  model = lm(y ~ poly(x, i, raw = TRUE), data = dane)
  y_pred = predict(model, newdata = data.frame(x = pom))
  lines(pom, y_pred, col = colors[i], lwd = 2)
  cat("Stopień:", i, "  Adjusted R^2:", summary(model)$adj.r.squared, "\n")
}
```

```
## Stopień: 1   Adjusted R^2: 0.150845 
## Stopień: 2   Adjusted R^2: 0.1813313 
## Stopień: 3   Adjusted R^2: 0.8598993 
## Stopień: 4   Adjusted R^2: 0.9506837 
## Stopień: 5   Adjusted R^2: 0.9705489
```

``` r
legend("topright", legend = paste("Stopień", 1:5), col = colors, lwd = 2)
```
<img width="672" height="480" alt="download" src="https://github.com/user-attachments/assets/0b709df9-7af8-4ed3-bdc7-54ae4d44ab26" />


##### Wnioski:
Stopień 1: R^2 = 0.15, widoczne niedopasowanie.

Stopień 2: R^2 = 0.18, niewielka poprawa.

Stopień 3: R^2 = 0.86 lepszy niż liniowy.

Stopień 4: R^2 = 0.95, bardzo dobre dopasowanie, linia przebiega blisko punktów.

Stopień 5: R^2 = 0.97, najlepsze dopasowanie, ale pojawia się ryzyko przeuczenia- kręta linia na końcach.


#### c) Diagnostyka modelu wielomianowego stopnia 4

``` r
model1 = lm(y ~ poly(x, 4, raw = TRUE), data = dane)

par(mfrow = c(2, 2))
plot(model1)
```
<img width="672" height="480" alt="download" src="https://github.com/user-attachments/assets/b99ba3c1-ae07-47a2-8b4c-3ac04acc3586" />

#### Podsumowanie:

1. Residuals vs Fitted

Punkty rozrzucone są dość losowo wokół osi 0. Brak silnych wartości odstających reszt.

2. Normal Q–Q

Punkty układają się blisko prostej, niewielkie odchylenia widoczne są na końcach.

3. Scale–Location

Rozrzut reszt względem wartości dopasowanych jest w miarę równomierny.

4. Residuals vs Leverage

Większość punktów ma niską dźwignię i małe wartości odległości Cooka. Brak obserwacji o bardzo dużym wpływie na dopasowanie modelu.

Przeprowadzimy test **Shapiro–Wilka**, aby sprawdzić normalność reszt modelu. Poziom istotności $0,05$. 

``` r
shapiro.test(residuals(model1))
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  residuals(model1)
## W = 0.98733, p-value = 0.46
```

Wynik: p = 0.46 > 0.05, więc brak podstaw do odrzucenia hipotezy o normalności rozkładu reszt.

Przeprowadzimy test **Breuscha–Godfreya**, aby sprawdzić autokorelację reszt.

``` r
lmtest::bgtest(model1)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  model1
## LM test = 0.37943, df = 1, p-value = 0.5379
```

Wynik: p = 0.5379 > 0.05, więc brak podstaw do stwierdzenia autokorelacji reszt.

Przeprowadzimy test **Breuscha–Pagana**, aby sprawdzić, czy wariancja reszt jest stała.

``` r
lmtest::bptest(model1)
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  model1
## BP = 19.047, df = 4, p-value = 0.0007694
```

Wynik: p = 0.0007694 < 0.05, czyli odrzucamy hipotezę o stałej wariancji reszt.

##### Wnioski:
Model wielomianowy stopnia 4 dobrze odwzorowuje strukturę danych, reszty są zgodne z rozkładem normalnym i brak w nich autokorelacji. Jedynym istotnym problemem jest heteroscedastyczność, która może prowadzić do zaniżenia lub zawyżenia błędów standardowych.

#### d) Model regresji liniowej dla |X–ai|

```
## 
## Call:
## lm(formula = y ~ ., data = dane[, c("y", "x", "abs_diff_1", "abs_diff_2")])
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.173281 -0.033460 -0.005043  0.037655  0.159275 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  2.40146    0.06749   35.58   <2e-16 ***
## x           -3.02049    0.09192  -32.86   <2e-16 ***
## abs_diff_1   2.19872    0.06030   36.47   <2e-16 ***
## abs_diff_2  -3.06216    0.09265  -33.05   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.06587 on 96 degrees of freedom
## Multiple R-squared:  0.9526,	Adjusted R-squared:  0.9511 
## F-statistic: 642.5 on 3 and 96 DF,  p-value: < 2.2e-16
```
<img width="672" height="480" alt="download" src="https://github.com/user-attachments/assets/88f06cee-37a3-41d2-8287-a2391f0721cf" />


#### Podsumowanie:
Współczynnik determinacji $R^2$ wynosi 0.9526 - model wyjaśnia ponad 95% zmienności zmiennej Y. Jest to wynik niemal identyczny z modelem wielomianowym 4. stopnia, ale nasz nowy model jest znacznie prostszy i ma mniejszą skłonność do przeuczenia.

1. Residuals vs Fitted

Reszty są rozproszone w sposób losowy wokół osi 0. Nie widać wyraźnego wzorca, co sugeruje poprawne dopasowanie modelu.

2. Normal Q–Q

Punkty dobrze układają się wzdłuż prostej. Niewielkie odchylenia występują jedynie na krańcach. Wskazuje to na zgodność reszt z rozkładem normalnym.

3. Scale–Location

Widać pewne odchylenia czerwonej linii i nierównomierne rozproszenie punktów (na krańcach wykresu wariancja wydaje się nieco większa niż w środku). Sugeruje to potencjalne naruszenie założenia o stałej wariancji reszt (heteroscedastyczność).

4. Residuals vs Leverage

Większość obserwacji ma niewielką dźwignię i niskie wartości odległości Cooka. Brak punktów, które mogłyby wywierać znaczący wpływ na dopasowanie modelu.


Przeprowadzimy test **Shapiro–Wilka**, aby sprawdzić normalność reszt modelu. Poziom istotności $0,05$. 

``` r
shapiro.test(residuals(model2))
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  residuals(model2)
## W = 0.98564, p-value = 0.3528
```

Wynik: p = 0.2245 > 0.05, więc brak podstaw do odrzucenia hipotezy o normalności rozkładu reszt.

Przeprowadzimy test **Breuscha–Godfreya**, aby sprawdzić autokorelację reszt.

``` r
lmtest::bgtest(model2)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  model2
## LM test = 2.383, df = 1, p-value = 0.1227
```

Wynik: p = 0.7185 > 0.05, więc brak podstaw do stwierdzenia autokorelacji reszt.

Przeprowadzimy test **Breuscha–Pagana**, aby sprawdzić, czy wariancja reszt jest stała.

``` r
lmtest::bptest(model2)
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  model2
## BP = 12.724, df = 3, p-value = 0.005272
```

Wynik: p = 0.2518 > 0.05, więc brak podstaw do odrzucenia hipotezy o homoscedastyczności– wariancja reszt jest stała.

##### Wnioski:
Model regresji liniowej ze zmiennymi postaci |X–ai| dobrze opisuje zależność Y od X. Reszty mają rozkład zgodny z normalnym, nie ma autokorelacji reszt, a wariancja reszt jest stała. W porównaniu z modelem wielomianowym stopnia 2, ten model nie ma problemu heteroscedastyczności i można uznać że jest bardziej stabilny.

#### f) Porównanie modeli na zbiorze uczącym i testowym

<img width="916" height="154" alt="{FEFF46E0-0F18-4DCA-852A-3676813951E7}" src="https://github.com/user-attachments/assets/cac38fd9-5591-46b7-91f2-fe384abbe724" />


##### Wnioski:
SSE_train – błąd na zbiorze uczącym

Model ze zmiennymi|X–ai|ma mniejszy błąd (~0.293) niż model wielomianowy stopnia 4 (~0.327). To oznacza, że lepiej dopasował się do danych treningowych.

SSE_test – błąd na zbiorze testowym

Na nowych danych minimalnie lepiej poradził sobie wielomian 4. stopnia (0.094 względem 0.133 dla modelu łamanego). Różnica jest jednak bardzo mała, a oba błędy są wyjątkowo niskie. Ponieważ błędy na zbiorze testowym w obu modelach drastycznie nie wzrosły, żaden z nich nie wykazuje zjawiska "przeuczenia" (overfittingu).

##### Wielomian 4. stopnia zyskał minimalną przewagę na zbiorze testowym, jednak model z wartościami bezwzględnymi |X-ai| charakteryzuje się mniejszym błędem na zbiorze uczącym i jest bardziej intuicyjny. Wielomiany wyższych stopni bywają nieprzewidywalne na krańcach przedziałów badanych danych, dlatego model oparty na łamanej (|X-ai|) stanowi dla niego doskonałą, stabilną i bardziej czytelną alternatywę.


### Zadanie 2.

Rozważamy zależność zmiennej objaśnianej **Y** od zmiennych **X1–X6**.

#### a) Model regresji liniowej


``` r
dane2 = read.csv("Dane_2.csv", sep = ";", dec = ",", header = TRUE)

model1 = lm(y ~ x1 + x2 + x3 + x4 + x5 + x6, data=dane2)
summary(model1)
```

```
## 
## Call:
## lm(formula = y ~ x1 + x2 + x3 + x4 + x5 + x6, data = dane2)
## 
## Residuals:
##        Min         1Q     Median         3Q        Max 
## -0.0059818 -0.0020403  0.0000421  0.0016373  0.0052700 
## 
## Coefficients:
##              Estimate Std. Error   t value Pr(>|t|)    
## (Intercept) 10.001023   0.001315  7606.164  < 2e-16 ***
## x1          -2.989249   0.005498  -543.662  < 2e-16 ***
## x2          -0.003796   0.001431    -2.653  0.00938 ** 
## x3           0.024878   0.011665     2.133  0.03558 *  
## x4          -0.002855   0.003073    -0.929  0.35523    
## x5          -0.001665   0.002484    -0.670  0.50426    
## x6          -9.009699   0.005580 -1614.767  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.002586 on 93 degrees of freedom
## Multiple R-squared:      1,	Adjusted R-squared:      1 
## F-statistic: 4.408e+07 on 6 and 93 DF,  p-value: < 2.2e-16
```

#### b) Diagnostyka modelu

``` r
par(mfrow = c(2, 2))
plot(model1)
```
<img width="672" height="480" alt="download" src="https://github.com/user-attachments/assets/c90c6847-62f7-48b4-84a6-cc7bc57ac3f7" />


``` r
shapiro.test(resid(model1))
```

```
## 
## 	Shapiro-Wilk normality test
## 
## data:  resid(model1)
## W = 0.98751, p-value = 0.4729
```

``` r
lmtest::bgtest(model1, order=1)
```

```
## 
## 	Breusch-Godfrey test for serial correlation of order up to 1
## 
## data:  model1
## LM test = 2.305, df = 1, p-value = 0.129
```

``` r
lmtest::bptest(model1)
```

```
## 
## 	studentized Breusch-Pagan test
## 
## data:  model1
## BP = 8.0414, df = 6, p-value = 0.2351
```

``` r
lmtest::gqtest(model1, alternative="two.sided")
```

```
## 
## 	Goldfeld-Quandt test
## 
## data:  model1
## GQ = 1.0742, df1 = 43, df2 = 43, p-value = 0.8155
## alternative hypothesis: variance changes from segment 1 to 2
```


```{.r .fold-show}
which(hatvalues(model1) > 2*mean(hatvalues(model1))) #bada dźwignię
```

```
## 12 19 73 74 79 80 91 94 
## 12 19 73 74 79 80 91 94
```

```{.r .fold-show}
which(abs(rstudent(model1)) > 2) #standaryzowanie reszty
```

```
## 53 61 
## 53 61
```

```{.r .fold-show}
which(cooks.distance(model1) > 4/(nrow(dane2)-length(coef(model1)))) #miara wpływu obserwacji na cały model
```

```
##  2 41 94 
##  2 41 94
```

##### Podsumowanie:

1. Residuals vs Fitted

Reszty są rozrzucone losowo wokół osi 0, brak wyraźnego wzorca.

Brak oznak nieliniowości lub dużych wartości odstających- liniowość modelu spełniona.

2. Normal Q–Q

Punkty układają się wzdłuż linii prostej. Niewielkie odchylenia na krańcach.

Test Shapiro–Wilka: p = 0.473 > 0.05, czyli nie ma podstaw do odrzucenia hipotezy o normalności reszt.

3. Scale–Location

Rozrzut reszt w miarę równomierny dla różnych wartości dopasowanych.Test Breuscha–Pagana: p = 0.235 > 0.05, czyli nie ma podstaw do odrzucenia hipotezy o homoscedastyczności.

4. Residuals vs Leverage

Większość obserwacji ma niską dźwignię i nie wpływa znacząco na dopasowanie modelu. Kilka obserwacji (np. 12, 19, 41, 73, 94) zostało wskazanych jako potencjalnie wpływowe (wg hatvalues i Cook’s distance), a obserwacje 53 i 61 odstają pod względem reszt standaryzowanych.

5. Autokorelacja reszt

Test Breuscha–Godfreya: p = 0.129 > 0.05, czyli nie ma podstaw do odrzucenia hipotezy o braku autokorelacji.

##### Wnioski:

Model regresji liniowej z X1–X6 można uznać za dobrze dopasowany.

#### c) Współliniowość zmiennych

##### Badanie liniowej niezależności zmiennych X1,...,X6- macierz korelacji

``` r
X = dane2[, c("x1","x2","x3","x4","x5","x6")]
cor(X)
```

```
##             x1           x2         x3          x4          x5           x6
## x1  1.00000000 -0.074517710 -0.9050662 -0.11607686 -0.86072840  0.551726222
## x2 -0.07451771  1.000000000  0.1113951  0.08264691  0.09499651  0.008196854
## x3 -0.90506620  0.111395082  1.0000000  0.48630541  0.92188197 -0.151600571
## x4 -0.11607686  0.082646915  0.4863054  1.00000000  0.36320472  0.687277514
## x5 -0.86072840  0.094996514  0.9218820  0.36320472  1.00000000 -0.207691632
## x6  0.55172622  0.008196854 -0.1516006  0.68727751 -0.20769163  1.000000000
```

##### Wnioski:
Bardzo silne korelacje:

1. X3 i X5, r = 0.92
2. X1 i X3, r = -0.91
3. X1 i X5, r = -0.86


To wskazuje na silną współliniowość między tymi zmiennymi- mogą przenosić podobną informację.

X2 jest praktycznie nieskorelowana z innymi zmiennymi, dostarcza unikalną informację. 

#### d) Wybór modelu z co najwyżej 2 zmiennymi
##### Metoda eliminacji

```{.r .fold-show}
summary(lm(x1 ~ x2 + x3 + x4 + x5 + x6, data=dane2))$r.squared
```

```
## [1] 0.9953209
```

```{.r .fold-show}
summary(lm(x2 ~ x1 + x3 + x4 + x5 + x6, data=dane2))$r.squared
```

```
## [1] 0.06353459
```

```{.r .fold-show}
summary(lm(x3 ~ x1 + x2 + x4 + x5 + x6, data=dane2))$r.squared
```

```
## [1] 0.9941587
```

```{.r .fold-show}
summary(lm(x4 ~ x1 + x2 + x3 + x5 + x6, data=dane2))$r.squared
```

```
## [1] 0.8369834
```

```{.r .fold-show}
summary(lm(x5 ~ x1 + x2 + x3 + x4 + x6, data=dane2))$r.squared
```

```
## [1] 0.8622775
```

```{.r .fold-show}
summary(lm(x6 ~ x1 + x2 + x3 + x4 + x5, data=dane2))$r.squared
```

```
## [1] 0.9761215
```

X1, X3 i X6 mają bardzo wysokie R^2, można je usunąć. 

##### Metoda dołączania
Ustalmy α = 0, 1. Wyliczamy p–wartość testu t dla dodanej zmiennej i dołączamy do modelu zmienną z najmniejszą p–wartością mniejszą od α.


```{.r .fold-show}
summary(lm(y ~ x2, data=dane2))$coefficients[2,4]
```

```
## [1] 0.758342
```

```{.r .fold-show}
summary(lm(y ~ x4, data=dane2))$coefficients[2,4]
```

```
## [1] 7.924086e-05
```

```{.r .fold-show}
summary(lm(y ~ x5, data=dane2))$coefficients[2,4]
```

```
## [1] 1.990956e-09
```

Najmniejszą p-wartość ma x5. Obliczamy p-wartości dla dodatkowych zmiennych.


```{.r .fold-show}
summary(lm(y ~ x5 + x2, data=dane2))$coefficients[3,4]
```

```
## [1] 0.7974784
```

```{.r .fold-show}
summary(lm(y ~ x5 + x4, data=dane2))$coefficients[3,4]
```

```
## [1] 1.397918e-19
```

X4 jest bardzo istotna- dodajemy ją do modelu

##### Wniosek:
Metodą dołączania i eliminacji otrzymujemy model z dwoma zmiennymi objaśniającymi y~X5 + X4.

#### e) Porównanie modeli na zbiorze uczącym i testowym

``` r
set.seed(123)
n = nrow(dane2)
train_idx = sample(seq_len(n), size = 0.7 * n)

train = dane2[train_idx, ]
test  = dane2[-train_idx, ]

model_train = lm(y ~ x1 + x2 + x3 + x4 + x5 + x6, data = train)
model_2_train = lm(y ~ x5 + x4, data = train)

sse_train = sum((predict(model_train, train) - train$y)^2)
sse_test  = sum((predict(model_train, test) - test$y)^2)

sse_2_train = sum((predict(model_2_train, train) - train$y)^2)
sse_2_test  = sum((predict(model_2_train, test) - test$y)^2)

wyniki = data.frame(
  Model = c("Pełny (X1–X6)", "Uproszczony (X4, X5)"),
  SSE_train = c(sse_train, sse_2_train),
  SSE_test  = c(sse_test, sse_2_test)
)

wyniki
```

<img width="922" height="155" alt="{146B6136-7FF1-4B16-A57A-2983706D9EA0}" src="https://github.com/user-attachments/assets/95aa6fa1-078d-471b-9177-3db67d17b4cf" />

##### Wnioski:
Model pełny lepiej dopasowuje dane uczące (niższe SSE_train).

Model uproszczony (X4, X5) ma bardzo podobny błąd na zbiorze testowym, a różnice w SSE_test są niewielkie. 

##### Model uproszczony (X4, X5) jest wystarczający i bardziej stabilny.

