# Projekt: Modele Regresji i Klasyfikacji
*Regression and Classification Models Project*

Ten projekt zawiera implementację szeregu zadań z zakresu modelowania statystycznego i uczenia maszynowego. Głównym celem jest praktyczne zastosowanie diagnostyki modeli, transformacji cech oraz metod selekcji zmiennych.

*This project includes the implementation of a series of tasks in statistical modeling and machine learning. The main goal is the practical application of model diagnostics, feature transformations, and variable selection methods.*

---

  a) Liniowa Analiza Dyskryminacyjna (LDA) - Budowa modelu klasyfikacji w oparciu o dwie pierwsze zmienne kanoniczne. Tworzenie odpowiedni wykres z obszarami klasyfikacji (obszary wyznaczone za pomocą odpowiednich prostych).

  b) Regresja Logistyczna z Funkcjami Bazowymi - Model klasyfikacji wieloklasowej z regularyzacją (karą). Jako zmienne objaśniające wykorzystano wartości radialnych funkcji bazowych (RBF) dla pierwotnych zmiennych 
  
  $x = (A, B, C)$: $$\phi_i(x) = \lVert x - m_i \rVert^2 + \beta^2$$
  
(gdzie ||.|| to norma euklidesowa). Celem jest optymalizacja liczby i parametrów funkcji bazowych tak, aby zminimalizować prawdopodobieństwo błędnej klasyfikacji przy zachowaniu jak najprostszej struktury modelu.

  c) Perceptron Wielowarstwowy (MLP) - Implementacja sieci neuronowej z optymalizacją liczby warstw ukrytych oraz neuronów. Model jest trenowany i oceniany wyłącznie przy pomocy 10-krotnej walidacji krzyżowej (10-fold cross-validation).
  
  d) Ewaluacja i Porównanie - Porównanie skuteczności modeli LDA oraz Regresji Logistycznej przy użyciu:
* Walidacji krzyżowej Leave-One-Out ($n$-krotnej).
* Walidacji krzyżowej 10-krotnej.

---

*a) Linear Discriminant Analysis (LDA) - Building a classification model based on the first two canonical variates. Creating a suitable graph with classification regions (regions defined by the corresponding lines).*

*b) Logistic Regression with Basis Functions - A multiclass classification model with regularization (penalty). Radial basis function (RBF) values ​​for the original variables are used as explanatory variables.*

$x = (A, B, C)$: $$\phi_i(x) = \lVert x - m_i \rVert^2 + \beta^2$$

*(where ||.|| is the Euclidean norm). The goal is to optimize the number and parameters of basis functions to minimize the probability of misclassification while maintaining the simplest possible model structure.*

*c) Multilayer Perceptron (MLP) - A neural network implementation with optimized hidden layers and neurons. The model is trained and evaluated using 10-fold cross-validation only.*

*d) Evaluation and Comparison - A comparison of the performance of LDA and Logistic Regression models using:*
* *Leave-One-Out ($n$-fold) cross-validation.*
* *10-fold cross-validation.*
