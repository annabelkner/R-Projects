# Polecenia
a) Zbudować model klasyfikacji metodą LDA dla dwóch pierwszych zmiennych kanonicznych. Zrobić odpowiedni wykres z obszarami klasyfikacji (obszary wyznaczone za pomocą odpowiednich prostych). 

b) Zbudować model klasyfikacji oparty na regresji logistycznej (z wieloma klasami) z karą. Jako zmienne objaśniające przyjąć wartości funkcji bazowych (każda funkcja daje nową zmienną) 

$$
\phi_i(x) = \lVert x - m_i \rVert^2 + \beta^2
$$

(gdzie ||.|| to norma euklidesowa) 

których argumentami są nasze pierwotne zmienne x=(A,B,C) . Znaleźć parametry tak aby prawdopodobieństwo błędnej klasyfikacji było jak najmniejsze, ich ilość jest dowolna ale staramy się żeby było ich jak najmniej. 

c) Zbudowac model klasyfikacji za pomoca perceprtonu wielowarstwowego. Dobrac optymalnie liczbe warstw i neuronow aby prawdopodobienstwo blednej klasyfikacji bylo jak najmniejsze . Tutaj wykonac tylko kroswalidacje 10-krotna. 

d) Porównać prawdopodobieństwa błędnej klasyfikacji w podpunktach a), b) za pomocą kroswalidacji n-krotnej i 10-krotnej.
