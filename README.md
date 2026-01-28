
# Social Media Addiction (studenci) – analiza danych w R

**TL;DR:** Repo pokazuje pełną analizę danych o korzystaniu z mediów społecznościowych przez studentów: czyszczenie danych, EDA (statystyki + wykresy), testy statystyczne, korelacje oraz modele (regresja liniowa i logistyczna). Wykresy zapisują się jako PNG do folderu `fig/`.

## O co chodzi w projekcie
Celem projektu jest sprawdzenie, jak różne aspekty korzystania z social mediów mogą się wiązać z innymi cechami studentów (np. nauka, sen, samopoczucie).  
To jest analiza „na danych” – wyniki wychodzą z obliczeń w R, a nie z pisania „z głowy”.

## Co jest w repo
- `analiza_socialmedia.R` – główny skrypt analizy (od wczytania danych do modeli i wykresów).
- `fig/` – tu zapisują się wykresy PNG .
- (opcjonalnie) raport `*.Rmd`/`*.html` – jeśli robisz wersję raportową, to pod to repo jest gotowe.

## Dane
Skrypt oczekuje pliku:
- `Students Social Media Addiction.csv`

Najprościej: wrzuć CSV do tego samego folderu co skrypt (albo co projekt `.Rproj`).

## Jak uruchomić (najprościej)
1. Otwórz projekt w RStudio (albo po prostu ustaw katalog roboczy na folder repo).
2. Upewnij się, że CSV jest w folderze repo i ma poprawną nazwę.
3. Uruchom skrypt `analiza_socialmedia.R`.

Skrypt sam:
- doinstaluje brakujące pakiety,
- policzy statystyki/testy,
- dopasuje modele,
- zapisze wykresy do `fig/`.

## Co dokładnie robi skrypt (w skrócie)
- **Czyszczenie i przygotowanie danych** (kontrola typów, braków, podstawowe porządki).
- **Statystyki opisowe** i wykresy rozkładów.
- **Korelacje** i macierz korelacji (corrplot).
- **Testy statystyczne** (dobierane do rodzaju zmiennych).
- **Modele**:
  - regresja liniowa (lm) + diagnostyka reszt,
  - regresja logistyczna + ROC/AUC (pakiet `pROC`).
- **Eksport wykresów** do PNG w `fig/`.

## Wymagania
- R (najlepiej aktualny) + RStudio (opcjonalnie, ale wygodnie).
- Pakiety instalują się automatycznie z CRAN (m.in. `tidyverse`, `broom`, `corrplot`, `car`, `pROC`).

## Autor / kontekst
Projekt robiony jako praca zaliczeniowa z analizy danych (PG WZiE). 
