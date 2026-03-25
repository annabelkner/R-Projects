#' ### Zadanie 1.
#' 
#' Rozważamy zależność zmiennej objaśnianej **Y** od zmiennej objaśniającej **X**.
#' 
#' #### a) Wykres punktowy zależności Y od X
#' 
## ----wykres, warning=F, message=F--------------------------------------
dane = read.csv("dane2.csv", sep = ";", dec = ",", header = TRUE)

library(ggplot2)
wykres = ggplot(dane ,aes(x=x,y=y))+
  geom_point(size=3,show.legend = F,color = "palevioletred3")

wykres

#' 
#' 
#' #### b) Regresja wielomianowa (stopnie 1–5)
## ----regresja,  warning=F, message=F-----------------------------------

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

legend("topright", legend = paste("Stopień", 1:5), col = colors, lwd = 2)

#' 
#' ##### Wnioski:
#' Stopień 1: R^2 = 0.15, widoczne niedopasowanie.
#' 
#' Stopień 2: R^2 = 0.18, niewielka poprawa.
#' 
#' Stopień 3: R^2 = 0.86 lepszy niż liniowy.
#' 
#' Stopień 4: R^2 = 0.95, bardzo dobre dopasowanie, linia przebiega blisko punktów.
#' 
#' Stopień 5: R^2 = 0.97, najlepsze dopasowanie, ale pojawia się ryzyko przeuczenia- kręta linia na końcach.
#' 
#' 
#' #### c) Diagnostyka modelu wielomianowego stopnia 4
## ----wykresy_diagnostyczne,  warning=F, message=F----------------------
model1 = lm(y ~ poly(x, 4, raw = TRUE), data = dane)

par(mfrow = c(2, 2))
plot(model1)

#' 
#' #### Podsumowanie:
#' 
#' 1. Residuals vs Fitted
#' 
#' Punkty rozrzucone są dość losowo wokół osi 0. Brak silnych wartości odstających reszt.
#' 
#' 2. Normal Q–Q
#' 
#' Punkty układają się blisko prostej, niewielkie odchylenia widoczne są na końcach.
#' 
#' 3. Scale–Location
#' 
#' Rozrzut reszt względem wartości dopasowanych jest w miarę równomierny.
#' 
#' 4. Residuals vs Leverage
#' 
#' Większość punktów ma niską dźwignię i małe wartości odległości Cooka. Brak obserwacji o bardzo dużym wpływie na dopasowanie modelu.
#' 
#' Przeprowadzimy test **Shapiro–Wilka**, aby sprawdzić normalność reszt modelu. Poziom istotności $0,05$. 
## ----sahpiro, warning=FALSE, message=FALSE-----------------------------
shapiro.test(residuals(model1))

#' 
#' Wynik: p = 0.46 > 0.05, więc brak podstaw do odrzucenia hipotezy o normalności rozkładu reszt.
#' 
#' Przeprowadzimy test **Breuscha–Godfreya**, aby sprawdzić autokorelację reszt.
## ----bgtest, warning=FALSE, message=FALSE------------------------------
lmtest::bgtest(model1)

#' 
#' Wynik: p = 0.5379 > 0.05, więc brak podstaw do stwierdzenia autokorelacji reszt.
#' 
#' Przeprowadzimy test **Breuscha–Pagana**, aby sprawdzić, czy wariancja reszt jest stała.
## ----bptest, warning=FALSE, message=FALSE------------------------------
lmtest::bptest(model1)

#' 
#' Wynik: p = 0.0007694 < 0.05, czyli odrzucamy hipotezę o stałej wariancji reszt.
#' 
#' ##### Wnioski:
#' Model wielomianowy stopnia 4 dobrze odwzorowuje strukturę danych, reszty są zgodne z rozkładem normalnym i brak w nich autokorelacji. Jedynym istotnym problemem jest heteroscedastyczność, która może prowadzić do zaniżenia lub zawyżenia błędów standardowych.
#' 
#' #### d) Model regresji liniowej dla |X–ai|
## ----stale, echo = FALSE-----------------------------------------------
a = c(0.33, 0.80)

for (i in seq_along(a)) {
  dane[[paste0("abs_diff_", i)]] = abs(dane$x - a[i])
}

model2 = lm(y ~ ., data = dane[, c("y", "x", "abs_diff_1", "abs_diff_2")])
summary(model2)

par(mfrow = c(2, 2))
plot(model2)

#' 
#' #### Podsumowanie:
#' Współczynnik determinacji $R^2$ wynosi 0.9526 - model wyjaśnia ponad 95% zmienności zmiennej Y. Jest to wynik niemal identyczny z modelem wielomianowym 4. stopnia, ale nasz nowy model jest znacznie prostszy i ma mniejszą skłonność do przeuczenia.
#' 
#' 1. Residuals vs Fitted
#' 
#' Reszty są rozproszone w sposób losowy wokół osi 0. Nie widać wyraźnego wzorca, co sugeruje poprawne dopasowanie modelu.
#' 
#' 2. Normal Q–Q
#' 
#' Punkty dobrze układają się wzdłuż prostej. Niewielkie odchylenia występują jedynie na krańcach. Wskazuje to na zgodność reszt z rozkładem normalnym.
#' 
#' 3. Scale–Location
#' 
#' Widać pewne odchylenia czerwonej linii i nierównomierne rozproszenie punktów (na krańcach wykresu wariancja wydaje się nieco większa niż w środku). Sugeruje to potencjalne naruszenie założenia o stałej wariancji reszt (heteroscedastyczność).
#' 
#' 4. Residuals vs Leverage
#' 
#' Większość obserwacji ma niewielką dźwignię i niskie wartości odległości Cooka. Brak punktów, które mogłyby wywierać znaczący wpływ na dopasowanie modelu.
#' 
#' 
#' Przeprowadzimy test **Shapiro–Wilka**, aby sprawdzić normalność reszt modelu. Poziom istotności $0,05$. 
## ----shapiro2, warning=FALSE, message=FALSE----------------------------
shapiro.test(residuals(model2))

#' 
#' Wynik: p = 0.2245 > 0.05, więc brak podstaw do odrzucenia hipotezy o normalności rozkładu reszt.
#' 
#' Przeprowadzimy test **Breuscha–Godfreya**, aby sprawdzić autokorelację reszt.
## ----bgtest2, warning=FALSE, message=FALSE-----------------------------
lmtest::bgtest(model2)

#' 
#' Wynik: p = 0.7185 > 0.05, więc brak podstaw do stwierdzenia autokorelacji reszt.
#' 
#' Przeprowadzimy test **Breuscha–Pagana**, aby sprawdzić, czy wariancja reszt jest stała.
## ----bptest2, warning=FALSE, message=FALSE-----------------------------
lmtest::bptest(model2)

#' 
#' Wynik: p = 0.2518 > 0.05, więc brak podstaw do odrzucenia hipotezy o homoscedastyczności– wariancja reszt jest stała.
#' 
#' ##### Wnioski:
#' Model regresji liniowej ze zmiennymi postaci |X–ai| dobrze opisuje zależność Y od X. Reszty mają rozkład zgodny z normalnym, nie ma autokorelacji reszt, a wariancja reszt jest stała. W porównaniu z modelem wielomianowym stopnia 2, ten model nie ma problemu heteroscedastyczności i można uznać że jest bardziej stabilny.
#' 
#' #### f) Porównanie modeli na zbiorze uczącym i testowym
#' 
## ----train_test, echo = FALSE------------------------------------------
set.seed(123)
n = nrow(dane)
train_idx = sample(seq_len(n), size = 0.7 * n)

train = dane[train_idx, ]
test  = dane[-train_idx, ]

model_train = lm(y ~ poly(x, 4, raw = TRUE), data = train)
model_2_train = lm(y ~ x + abs_diff_1 + abs_diff_2, data = train)

sse_train = sum((predict(model_train, train) - train$y)^2)
sse_test  = sum((predict(model_train, test) - test$y)^2)

sse_2_train = sum((predict(model_2_train, train) - train$y)^2)
sse_2_test  = sum((predict(model_2_train, test) - test$y)^2)

wyniki = data.frame(
  Model = c("Stopień 4", "|X-ai|"),
  SSE_train = c(sse_train, sse_2_train),
  SSE_test  = c(sse_test, sse_2_test)
)


wyniki

#' 
#' ##### Wnioski:
#' SSE_train – błąd na zbiorze uczącym
#' 
#' Model ze zmiennymi|X–ai|ma mniejszy błąd (~0.293) niż model wielomianowy stopnia 4 (~0.327). To oznacza, że lepiej dopasował się do danych treningowych.
#' 
#' SSE_test – błąd na zbiorze testowym
#' 
#' Na nowych danych minimalnie lepiej poradził sobie wielomian 4. stopnia (0.094 względem 0.133 dla modelu łamanego). Różnica jest jednak bardzo mała, a oba błędy są wyjątkowo niskie. Ponieważ błędy na zbiorze testowym w obu modelach drastycznie nie wzrosły, żaden z nich nie wykazuje zjawiska "przeuczenia" (overfittingu).
#' 
#' ##### Wielomian 4. stopnia zyskał minimalną przewagę na zbiorze testowym, jednak model z wartościami bezwzględnymi |X-ai| charakteryzuje się mniejszym błędem na zbiorze uczącym i jest bardziej intuicyjny. Wielomiany wyższych stopni bywają nieprzewidywalne na krańcach przedziałów badanych danych, dlatego model oparty na łamanej (|X-ai|) stanowi dla niego doskonałą, stabilną i bardziej czytelną alternatywę.
#' 
#' 
#' ### Zadanie 2.
#' 
#' Rozważamy zależność zmiennej objaśnianej **Y** od zmiennych **X1–X6**.
#' 
#' #### a) Model regresji liniowej
#' 
## ----model, warning=F, message=F---------------------------------------
dane2 = read.csv("Dane_2.csv", sep = ";", dec = ",", header = TRUE)

model1 = lm(y ~ x1 + x2 + x3 + x4 + x5 + x6, data=dane2)
summary(model1)

#' 
#' #### b) Diagnostyka modelu
## ----diagnostyka2, warning=F, message=F--------------------------------

par(mfrow = c(2, 2))
plot(model1)

shapiro.test(resid(model1))
lmtest::bgtest(model1, order=1)
lmtest::bptest(model1)
lmtest::gqtest(model1, alternative="two.sided")

#' 
## ----diagnostyka3, class.source = "fold-show"--------------------------
which(hatvalues(model1) > 2*mean(hatvalues(model1))) #bada dźwignię
which(abs(rstudent(model1)) > 2) #standaryzowanie reszty
which(cooks.distance(model1) > 4/(nrow(dane2)-length(coef(model1)))) #miara wpływu obserwacji na cały model

#' 
#' ##### Podsumowanie:
#' 
#' 1. Residuals vs Fitted
#' 
#' Reszty są rozrzucone losowo wokół osi 0, brak wyraźnego wzorca.
#' 
#' Brak oznak nieliniowości lub dużych wartości odstających- liniowość modelu spełniona.
#' 
#' 2. Normal Q–Q
#' 
#' Punkty układają się wzdłuż linii prostej. Niewielkie odchylenia na krańcach.
#' 
#' Test Shapiro–Wilka: p = 0.473 > 0.05, czyli nie ma podstaw do odrzucenia hipotezy o normalności reszt.
#' 
#' 3. Scale–Location
#' 
#' Rozrzut reszt w miarę równomierny dla różnych wartości dopasowanych.Test Breuscha–Pagana: p = 0.235 > 0.05, czyli nie ma podstaw do odrzucenia hipotezy o homoscedastyczności.
#' 
#' 4. Residuals vs Leverage
#' 
#' Większość obserwacji ma niską dźwignię i nie wpływa znacząco na dopasowanie modelu. Kilka obserwacji (np. 12, 19, 41, 73, 94) zostało wskazanych jako potencjalnie wpływowe (wg hatvalues i Cook’s distance), a obserwacje 53 i 61 odstają pod względem reszt standaryzowanych.
#' 
#' 5. Autokorelacja reszt
#' 
#' Test Breuscha–Godfreya: p = 0.129 > 0.05, czyli nie ma podstaw do odrzucenia hipotezy o braku autokorelacji.
#' 
#' ##### Wnioski:
#' 
#' Model regresji liniowej z X1–X6 można uznać za dobrze dopasowany.
#' 
#' #### c) Współliniowość zmiennych
#' 
#' ##### Badanie liniowej niezależności zmiennych X1,...,X6- macierz korelacji
## ----macierz_kor , warning=F, message=F--------------------------------
X = dane2[, c("x1","x2","x3","x4","x5","x6")]
cor(X)

#' 
#' ##### Wnioski:
#' Bardzo silne korelacje:
#' 
#' 1. X3 i X5, r = 0.92
#' 2. X1 i X3, r = -0.91
#' 3. X1 i X5, r = -0.86
#' 
#' 
#' To wskazuje na silną współliniowość między tymi zmiennymi- mogą przenosić podobną informację.
#' 
#' X2 jest praktycznie nieskorelowana z innymi zmiennymi, dostarcza unikalną informację. 
#' 
#' #### d) Wybór modelu z co najwyżej 2 zmiennymi
#' ##### Metoda eliminacji
## ----eliminacja, warning=F, message=F, class.source = "fold-show"------
summary(lm(x1 ~ x2 + x3 + x4 + x5 + x6, data=dane2))$r.squared
summary(lm(x2 ~ x1 + x3 + x4 + x5 + x6, data=dane2))$r.squared
summary(lm(x3 ~ x1 + x2 + x4 + x5 + x6, data=dane2))$r.squared
summary(lm(x4 ~ x1 + x2 + x3 + x5 + x6, data=dane2))$r.squared
summary(lm(x5 ~ x1 + x2 + x3 + x4 + x6, data=dane2))$r.squared
summary(lm(x6 ~ x1 + x2 + x3 + x4 + x5, data=dane2))$r.squared

#' 
#' X1, X3 i X6 mają bardzo wysokie R^2, można je usunąć. 
#' 
#' ##### Metoda dołączania
#' Ustalmy α = 0, 1. Wyliczamy p–wartość testu t dla dodanej zmiennej i dołączamy do modelu zmienną z najmniejszą p–wartością mniejszą od α.
#' 
## ----dolaczanie, echo=TRUE, class.source = "fold-show"-----------------
summary(lm(y ~ x2, data=dane2))$coefficients[2,4]
summary(lm(y ~ x4, data=dane2))$coefficients[2,4]
summary(lm(y ~ x5, data=dane2))$coefficients[2,4]

#' 
#' Najmniejszą p-wartość ma x5. Obliczamy p-wartości dla dodatkowych zmiennych.
#' 
## ----dolaczanie2, class.source = "fold-show"---------------------------
summary(lm(y ~ x5 + x2, data=dane2))$coefficients[3,4]
summary(lm(y ~ x5 + x4, data=dane2))$coefficients[3,4]

#' 
#' X4 jest bardzo istotna- dodajemy ją do modelu
#' 
#' ##### Wniosek:
#' Metodą dołączania i eliminacji otrzymujemy model z dwoma zmiennymi objaśniającymi y~X5 + X4.
#' 
#' #### e) Porównanie modeli na zbiorze uczącym i testowym
## ----test_train2-------------------------------------------------------
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

#' ##### Wnioski:
#' Model pełny lepiej dopasowuje dane uczące (niższe SSE_train).
#' 
#' Model uproszczony (X4, X5) ma bardzo podobny błąd na zbiorze testowym, a różnice w SSE_test są niewielkie. 
#' 
#' ##### Model uproszczony (X4, X5) jest wystarczający i bardziej stabilny.
