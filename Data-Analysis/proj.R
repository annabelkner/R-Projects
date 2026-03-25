#' ---
#' title: "Projekt z zajęć Analiza danych w badaniach naukowych"
#' author: "Anna Belkner"
#' date: "2026-02-10"
#' output: 
#'   html_document:
#'     keep_md: true
#'     theme: cerulean
#'     code_folding: show
#'     code_download: true
#'     highlight: tango
#' ---
#' # Zadania {.tabset}
#' 
#' ## Podpunkt a)
#' Polecenie: Zbudować model klasyfikacji metodą LDA dla dwóch pierwszych zmiennych kanonicznych. Zrobić odpowiedni wykres z obszarami klasyfikacji (obszary wyznaczone za pomocą odpowiednich prostych).
#' 
#' ### Wstępna analiza
#' 
## ----dane--------------------------------------------------------------
dane = read.csv("Dane 49 .csv")
attach(dane)
dane$Class = as.factor(dane$Class)
head(dane)
summary(dane)

#' 
#' Analizujemy łącznie 474 obserwacje. Zmienna wynikowa Class przyjmuje trzy wartości (1,2,3).
#' 
#' Klasy są zbalansowane, mają podobną ilość obserwacji:
#' 
#' * Klasa 1: 175 obserwacji
#' 
#' * Klasa 2: 158 obserwacji
#' 
#' * Klasa 3: 141 obserwacji
#' 
#' Wszystkie trzy zmienne mają średnie oraz mediany bardzo bliskie zera. Zmienna C charakteryzuje się największym rozrzutem wartości (od ok. -3.94 do 5.34), podczas gdy zmienna A jest najbardziej skupiona (zakres od ok. -2.00 do 1.53).
#' 
#' *Wykresy pudełkowe dla danych*
#' 
## ----boxplot-----------------------------------------------------------
par(mfrow=c(1,3))
boxplot(A ~ Class, data = dane, col = "lightblue", main = "Zmienna A")
boxplot(B ~ Class, data = dane, col = "lightgreen", main = "Zmienna B")
boxplot(C ~ Class, data = dane, col = "salmon", main = "Zmienna C")

#' 
#' *Wykresy kropkowe dla danych*
#' 
## ----kropkowy, warning=F-----------------------------------------------
library(tidyr)
df_long = pivot_longer(dane, cols = c("A", "B", "C"), names_to = "Zmienna", values_to = "Wartosc")

library(ggplot2)
ggplot(df_long, aes(x = Class, y = Wartosc)) +
  geom_jitter(aes(colour = Class), width = 0.15, size = 3, show.legend = F) +
  facet_wrap(~Zmienna, scales = "free_y")

#' 
#' * Zmienna C świetnie radzi sobie z odróżnianiem klasy 1. Średnia i zakres wartości dla tej klasy są wyraźnie wyższe i niemal całkowicie odseparowane od klas 2 i 3. Zmienne A i B też dobrze radzą sobie z odróżnieniem klasy 1 od pozostałych (Klasa 1 ma wyraźnie niższe wartości średnie), ale klasy 2 i 3 się nakładają.
#' 
#' * Normalność rozkładu: Wykresy punktowe sugerują, że dane są dość gęste i symetryczne wokół centrum, co jest dobrym znakiem dla LDA. Widać jednak kilka wartości odstających.
#' 
#' * Homogeniczność wariancji: LDA zakłada, że wszystkie klasy mają podobną macierz kowariancji. Patrząc na szerokość pudełek (IQR), wariancje wydają się w miarę zbliżone, choć w Zmiennej C klasa 1 wydaje się mieć większy rozrzut niż pozostałe.
#' 
#' 
#' ### Model LDA i wykres obszarów klasyfikacji
#' 
## ----LDA, warning=F----------------------------------------------------

library(MASS)
model_lda = lda(Class ~ ., data = dane)

pred_lda = predict(model_lda)
dane_lda = data.frame(LD1 = pred_lda$x[,1], 
                       LD2 = pred_lda$x[,2], 
                       Class = dane$Class)

library(klaR)
partimat(Class ~ LD1 + LD2, data = dane_lda, method = "lda", imageplot = T)

#' 
#' ### Wnioski z wykresu
#' 
#' Na podstawie wykresu możemy wyciągnąć następujące wnioski dotyczące skuteczności modelu:
#' 
#' * Błąd dopasowania do danych treningowych wynosi 33,8%, co sugeruje, że model ma trudności z separacją klas nawet na zbiorze uczącym.
#' 
#' * Separacja klas:
#' Klasa 1 jest bardzo dobrze odseparowana od pozostałych. Wszystkie jedynki znajdują się w błękitnym obszarze. LDA świetnie radzi sobie z odróżnieniem tej grupy.
#' Dla klas 2 i 3 widzimy nakładanie się na siebie trójek i dwójek. Granica decyzyjna próbuje je rozdzielić, ale wiele dwójek wpada w obszar trójek i odwrotnie.
#' 
#' LDA zakłada liniowe granice między klasami. Na wykresie widzimy proste linie podziału. Ponieważ dane (szczególnie klasy 2 i 3) wydają się mocno wymieszane, liniowy podział może być niewystarczający.
#' 
## ----table-------------------------------------------------------------
table(dane$Class, pred_lda$class)

#' Model LDA świetnie radzi sobie z identyfikacją Klasy 1. Wszystkie 175 obserwacji z tej grupy zostało poprawnie zaklasyfikowanych (brak błędów I i II rodzaju dla tej klasy). Potwierdza to wcześniejsze obserwacje z wykresów, że Klasa 1 jest liniowo odseparowalna od reszty.
#' 
#' Model całkowicie gubi się przy rozróżnianiu K klasy 2 i 3. Prawie połowa (77 z 158) obserwacji klasy 2 została błędnie przypisana do klasy 3. Dla Klasy 3 model częściej się mylił (82 razy) niż wskazywał poprawnie (59 trafień).
#' 
#' 
## ----scaling-----------------------------------------------------------
model_lda$scaling

#' 
#' W obu funkcjach dyskryminacyjnych (LD1 i LD2) największe co do modułu wagi mają zmienne A i B. 
#' 
#' __Model LDA radzi sobie średnio. O ile klasa 1 jest rozpoznawana bezbłędnie, o tyle rozróżnienie między klasą 2 a 3 jest dla tego modelu bardzo trudne.__
#' 
#' ## Podpunkt b)
#' Polecenie:  Zbudować model klasyfikacji oparty na regresji logistycznej (z wieloma klasami) z karą. Jako zmienne objaśniające przyjąć wartości funkcji bazowych (każda funkcja daje nową zmienną) $\phi$i(x) = ||x-mi||^2 + $\beta$^2 których argumentami są nasze pierwotne zmienne x=(A,B,C). Znaleźć parametry tak aby prawdopodobieństwo błędnej klasyfikacji było jak najmniejsze, ich ilość jest dowolna ale staramy się żeby było ich jak najmniej.
#' 
#' ### Model klasyfikacji oparty na regresji logistycznej
#' 
## ----regresja, warning = F, message=F----------------------------------
phi_func = function(x, m, beta) {
  r = sum((x - m)^2)
  return(r + beta^2)
}

library(glmnet)

# Parametry do sprawdzenia
liczby_centrow = c(4, 6, 8, 10)
betas = c(0.4, 0.7, 1, 2, 5) 

best_acc_b = 0
# Domyślne wartości startowe
best_params_b = list(n_centers = 5, beta = 1, lambda = 0.01)

n = nrow(dane) 

for(k in liczby_centrow){
  
  # Wybór centrów
  set.seed(123)
  km = kmeans(dane[,1:3], centers = k)
  centra_k = km$centers
  
  for(b in betas){
    
    X = matrix(0, nrow = n, ncol = k)
    for(i in 1:n) {
      for(j in 1:k) {
         X[i, j] = phi_func(as.numeric(dane[i, 1:3]), as.numeric(centra_k[j, ]), b)
      }
    }
    
    model_cv =  cv.glmnet(X, dane$Class, family="multinomial", type.measure="class")
    
    acc = 1 - min(model_cv$cvm)
      
    if(acc > best_acc_b){
        best_acc_b = acc
        best_params_b = list(n_centers=k, beta=b, lambda=model_cv$lambda.min)
      }
  }
}


#' 
## ----najlepsze parametry,echo = F--------------------------------------

cat("Liczba centrów:", best_params_b$n_centers, "\n")
cat("Beta:", best_params_b$beta, "\n")
cat("Lambda:", best_params_b$lambda, "\n")
cat("Dokładność (ACC):", best_acc_b, "\n")

#' 
#' 
#' Szukamy najmniejszej liczby funkcji bazowych zapewniającej wysoką jakość klasyfikacji. Algorytm wskazał, że optymalnym kompromisem jest użycie **8** funkcji bazowych, co dało dokładność na poziomie **0.7700422 **. Jednak wynik Accuracy jest niewiele wyższy dla 8 funckji bazowych jak dla 4 funkcji bazowych, lepiej jest więc zbudować mniej skomplikowany model o **4** funkcjach bazowych i dokładności około **0.75**.
#' 
#' Wnioski:
#' Regresja logistyczna na funkcjach radialnych radzi sobie lepiej niż LDA, ale mimo to, wynik na poziomie 76% sugeruje, że klasy 2 i 3 nadal są trudne do idealnego rozdzielenia tą metodą.
#' 
#' Decyzja: Do kroswalidacji wybrano model oparty na 4 centrach ($\beta=0.4$).
#' 
## ----recznie parametry, echo = F---------------------------------------
set.seed(123)
final_k = 4
final_beta = 0.4

km = kmeans(dane[,1:3], centers = final_k)
centra_k = km$centers
  
X_final = matrix(0, nrow = n, ncol = final_k)
for(i in 1:n) {
  for(j in 1:final_k) {
    X_final[i, j] = phi_func(as.numeric(dane[i, 1:3]), as.numeric(centra_k[j, ]), final_beta)
  }
}
# Zapisujemy najlepszą lambdę do użycia w podpunkcie d)
model_cv = cv.glmnet(X_final, dane$Class, family = "multinomial", type.measure = "class")
best_lambda = model_cv$lambda.min
acc = 1 - min(model_cv$cvm)
cat("Dokładność modelu opartego na 4 centrach i beta = 0.4: ", acc)

#' 
#' 
#' ## Podpunkt c)
#' Polecenie: Zbudować model klasyfikacji za pomocą perceprtonu wielowarstwowego. Dobrać optymalnie liczbę warstw i neuronow aby prawdopodobieństwo błędnej klasyfikacji było jak najmniejsze ale tak żeby w rozsądnym czasie wykonała się kroswalidacja. Tutaj wykonać tylko kroswalidację 10-krotna.
#' 
## ----przygotowanie_danych, warning=F, message=F, echo = F--------------
library(neuralnet)

dane1 <- dane[, 1:3]
dane1$C1 <- ifelse(dane$Class == 1, 1, 0)
dane1$C2 <- ifelse(dane$Class == 2, 1, 0)
dane1$C3 <- ifelse(dane$Class == 3, 1, 0)
dane1$TrueClass <- as.numeric(dane$Class)

#' 
## ----inicjalizacja foldow, echo = F------------------------------------
set.seed(123)
n = nrow(dane)
k = 10
folds = sample(1:k, nrow(dane), replace=TRUE)

#' 
#' ### Sieć jednowarstwowa
#' 
## ----siec 3, warning=F, message=F--------------------------------------
bledy = 0

for(i in 1:k){
  train = dane1[folds != i, ]
  test  = dane1[folds == i, ]
  
  nn = neuralnet(C1 + C2 + C3 ~ A + B + C, data = train, hidden = c(3), linear.output = FALSE, err.fct = "ce", stepmax = 1e6, rep=1)
 
    pr = predict(nn, newdata = test[, c("A", "B", "C")])
    pred_cls = max.col(pr)
    true_cls = test$TrueClass
    bledy = bledy + sum(pred_cls != true_cls)
}

acc = 1 - (bledy / nrow(dane))
print(paste("Dokładność:", acc))

#' 
#' ### Sieć trzywarstwowa
## ----siec 3,3,3, warning=F, message=F----------------------------------
bledy = 0

for(i in 1:k){
  train = dane1[folds != i, ]
  test  = dane1[folds == i, ]
  
  nn =  neuralnet(C1 + C2 + C3 ~ A + B + C, data = train, hidden = c(3,3,3), 
                  linear.output = FALSE, err.fct = "ce", stepmax = 1e6, rep=1)
  
  pr = predict(nn, newdata = test[, c("A", "B", "C")])
  pred_cls = max.col(pr)
  true_cls = test$TrueClass
    
    bledy = bledy + sum(pred_cls != true_cls)
}

acc = 1 - (bledy / nrow(dane))
print(paste("Dokładność ", acc))

#' 
#' 
#' ### Sieć dwuwarstwowa 
#' 
## ----siec 3-2, warning=F, message=F------------------------------------
bledy = 0

for(i in 1:k){
  train = dane1[folds != i, ]
  test  = dane1[folds == i, ]
  
  nn =  neuralnet(C1 + C2 + C3 ~ A + B + C, data = train, hidden = c(3,2), 
                  linear.output = FALSE, err.fct = "ce", stepmax = 1e6, rep=1)
  
  pr = predict(nn, newdata = test[, c("A", "B", "C")])
  pred_cls = max.col(pr)
  true_cls = test$TrueClass
    
    bledy = bledy + sum(pred_cls != true_cls)
}

acc = 1 - (bledy / nrow(dane))
print(paste("Dokładność dla 3-2 neuronów:", acc))

#' 
#' 
#' Interpretacja wyników
#' 
#' Dla 3 neuronów dokładność jest około 0,92, to bardzo dobry wynik, znacznie lepszy od LDA. Oznacza to, że już niewielka nieliniowość wystarczy, by poprawnie rozdzielić większość trudnych przypadków. Ten model jest najmniej podatny na przeuczenie.
#' 
#' Dla 3-2 neuronów mamy najlepszy wynik. Dokładność około 0,94 pokazuje, że więcej warst pozwala lepiej dopasować granicę decyzyjną do nakładających się klas 2 i 3.
#' 
#' Dla 3-3-3 neuronów– przeuczenie. Wynik jest minimalnie gorszy od modelu 3-2, a sieć jest znacznie bardziej skomplikowana (ma 3 warstwy). 
#' Sieci neuronowe okazały się najlepsze dla tego zbioru danych. Tak duża różnica między wynikami sieci a LDA sugeruje, że analizowane dane mają silnie nieliniową strukturę.
#' 
#' Decyzja: Do kroswalidacji wybrano sieć o konfiguracji (3, 2), która stanowi optymalny kompromis między złożonością modelu a jego Accuracy.
#' 
#' ## Podpunkt d)
#' Porównać prawdopodobieństwa błędnej klasyfikacji w podpunktach a), b) i c) za pomocą kroswalidacji n-krotnej i 10-krotnej.
#' 
#' ### Kroswalidacja 10-krotna LDA
#' 
## ----kroswalidacja LDA 10, warning = F, message=F----------------------
library(MASS)

bledy_10 = 0

# 1. KROSWALIDACJA 10-KROTNA

for(i in 1:k){
  train = dane[folds != i, ]
  test  = dane[folds == i, ]
  
  mod = lda(Class ~ ., data = train)
  
  pred = predict(mod, test)$class

  bledy_10 = bledy_10 + sum(pred != test$Class)
}

wynik_lda_10 = bledy_10 / n

#' 
#' ### Kroswalidacja n-krotna LDA
## ----kroswalidacja LDA N, warning = F----------------------------------
# 2. KROSWALIDACJA N-KROTNA
bledy_n = 0

for(i in 1:n){
  train = dane[-i, ]
  test  = dane[i, ]
  
  mod = lda(Class ~ ., data = train)
  pred = predict(mod, test)$class
  
  if(pred != test$Class){
    bledy_n = bledy_n + 1
  }
}

wynik_lda_n = bledy_n / n

#' 
## ----wyniki LDA--------------------------------------------------------
cat("Błąd 10-krotna kroswalidacja:", wynik_lda_10, "\nBłąd n-krotna kroswalidacja: ", wynik_lda_n)

#' 
#' ### Kroswalidacja 10-krotna modelu opartego na regresji logistycznej
#' 
## ----kroswalidacja regresja, warning = F, message=F--------------------
# 1. KROSWALIDACJA 10-KROTNA
bledy_10 = 0

for(i in 1:k){
  dane_train = dane[folds != i, 1:3]
  y_train = dane$Class[folds != i]
  dane_test = dane[folds == i, 1:3]
  y_test = dane$Class[folds == i]

  set.seed(123 + i) 
  km = kmeans(dane_train, centers = final_k)
  centra_k = km$centers

  n_train = nrow(dane_train)
  X_train = matrix(0, nrow = n_train, ncol = final_k)
  for(r in 1:n_train) {
    for(c in 1:final_k) {
      X_train[r, c] = phi_func(as.numeric(dane_train[r, ]), as.numeric(centra_k[c, ]), final_beta)
    }
  }

  n_test = nrow(dane_test)
  X_test = matrix(0, nrow = n_test, ncol = final_k)
  for(r in 1:n_test) {
    for(c in 1:final_k) {
      X_test[r, c] = phi_func(as.numeric(dane_test[r, ]), as.numeric(centra_k[c, ]), final_beta)
    }
  }
  cv_ = cv.glmnet(X_train, y_train, family = "multinomial", type.measure = "class")
  pred = predict(cv_, newx = X_test, type = "class", s = cv_$lambda.min)
  
  bledy_10 = bledy_10 + sum(pred != y_test)
}
wynik_rbf_10 = bledy_10 / n


#' 
#' ### Kroswalidacja n-krotna modelu opartego na regresji logistycznej
#' 
## ----krowalidacja regresja N-------------------------------------------

# 2. KROSWALIDACJA N-KROTNA

bledy_n = 0

for(i in 1:n){
  dane_train = dane[-i, 1:3]
  y_train = dane$Class[-i]
  dane_test = dane[i, 1:3]
  y_test = dane$Class[i]
  
  set.seed(123 + i) 
  km = kmeans(dane_train, centers = final_k)
  centra_k = km$centers

  n_train = nrow(dane_train)
  X_train = matrix(0, nrow = n_train, ncol = final_k)
  
  for(r in 1:n_train) {
    for(c in 1:final_k) {
      X_train[r, c] = phi_func(as.numeric(dane_train[r, ]), as.numeric(centra_k[c, ]), final_beta)
    }
  }
  X_test = matrix(0, nrow = 1, ncol = final_k)
  for(c in 1:final_k) {
    X_test[1, c] = phi_func(as.numeric(dane_test[1, ]), as.numeric(centra_k[c, ]), final_beta)
  }

  cv_ = cv.glmnet(X_train, y_train, family = "multinomial", type.measure = "class")
  pred = predict(cv_, newx = X_test, type = "class", s = cv_$lambda.min)

  if(pred != y_test){
    bledy_n = bledy_n + 1
  }
}
wynik_rbf_n = bledy_n / n


#' 
## ----wyniki regresja---------------------------------------------------
cat("Błąd 10-krotna kroswalidacja:", wynik_rbf_10, "\nBłąd n-krotna kroswalidacja: ", wynik_rbf_n)

#' 
#' ### Kroswalidacja 10-krotna MLP
## ----kroswalidacja MLP 10, warning = F, message=F----------------------
# 1. KROSWALIDACJA 10-KROTNA
bledy_10 = 0

for(i in 1:k){
  train = dane1[folds != i, ]
  test  = dane1[folds == i, ]
  
  mod = neuralnet(C1 + C2 + C3 ~ A + B + C, data = train, hidden = c(3,2), linear.output = FALSE, err.fct = "ce", stepmax = 1e6, threshold = 0.1)
  
  res = predict(mod, newdata = test[, c("A", "B", "C")])
  pred = max.col(res)
  
  y_test = test$TrueClass
  bledy_10 = bledy_10 + sum(pred != y_test)
}

wynik_mlp_10 = bledy_10 / n

#' 
#' ### Kroswalidacja n-krotna MLP
#' 
## ----kroswalidacja MLP N-----------------------------------------------
# 2. KROSWALIDACJA N-KROTNA
bledy_n = 0

for(i in 1:n){
  train = dane1[-i, ]
  test  = dane1[i, ]
  
  mod = tryCatch(
    neuralnet(C1 + C2 + C3 ~ A + B + C, data = train, hidden = c(3,2), linear.output = FALSE, err.fct = "ce", stepmax = 1e6, threshold = 0.1),
    error = function(e) NULL
  )
  
  if(is.null(mod)){
   bledy_n = bledy_n + 1
   next
}
  
  res = tryCatch(
    predict(mod, newdata = as.data.frame(lapply(test[, c("A", "B", "C")], as.numeric))),
    error = function(e) NULL
  )
  
  if(is.null(res)){
   bledy_n = bledy_n + 1
   next
}
  
  pred = which.max(as.numeric(res))
  
  y_test = test$TrueClass
  
  if(pred != y_test){
    bledy_n = bledy_n + 1
  }
}

wynik_mlp_n = bledy_n / n

#' 
## ----wyniki MLP--------------------------------------------------------
cat("Błąd 10-krotna kroswalidacja:", wynik_mlp_10, "\nBłąd n-krotna kroswalidacja: ", wynik_mlp_n)

#' 
#' ### Podsumowanie:
#' 
#' LDA i Regresja RBF: Obie metody wykazują wysoką zgodność między walidacją 10-krotną a n-krotną. Oznacza to, że ich wyniki (choć słabe – błędy odpowiednio ~35% i ~25%) są stabilne i wiarygodne.
#' 
#' * LDA: Błąd na poziomie ~35-37%. Model ten systematycznie nie radzi sobie z separacją klas 2 i 3. Liniowa granica decyzyjna jest tu niewystarczająca.
#' 
#' * Regresja (RBF): Błąd na poziomie ~25%. Zastosowanie nieliniowych funkcji bazowych poprawiło wynik, ale wciąż co czwarta obserwacja jest klasyfikowana błędnie.
#' 
#' Sieć Neuronowa (MLP): Model ten osiągnął zdecydowanie najlepsze wyniki w obu testach i osiągnał błąd  na poziomie ~4%.
#' 
#' Walidacja n-krotna potwierdziła, a nawet poprawiła wynik uzyskany w walidacji 10-krotnej. Podobne wyniki sugerują, że oszacowanie błędu jest stabilne.
#' 
#' *Podczas walidacji n-krotnej pojawiły się ostrzeżenia o braku zbieżności algorytmu (did not converge) dla pojedynczych powtórzeń. Oznacza to, że dla kilku specyficznych punktów (będących prawdopodobnie wartościami odstającymi) sieć miała problem ze znalezieniem globalnego minimum w zadanym limicie kroków.*
#' 
#' Ostateczny wniosek: **Sieć Neuronowa (MLP) o strukturze (3,2) jest najlepsza.** Wyniki sugerują, że liniowa granica decyzyjna jest niewystarczająca dla klas 2 i 3.
