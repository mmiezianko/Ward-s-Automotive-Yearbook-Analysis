library(dplyr)

#Tworzę funkcję, która czyta plik csv, a następnie dokonuje lekkich zmian w strukturze danych.
func_zbior_danych <- function(file = '~/Downloads/Automobile_data.csv')
{
  #Ładuję plik csv
  auto_odczyt <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)

  # Zmieniam typ niektórych danych na numeryczny
  kolumny_string <- c('price', 'bore', 'stroke', 'horsepower', 'peak.rpm')
  auto_odczyt[, kolumny_string] <- lapply(auto_odczyt[, kolumny_string], as.numeric)
  
  # Usuwam rzędy z brakującymi danymi. 
  auto_odczyt[complete.cases(auto_odczyt), ]
}

  # Tworzę zmienną automobil_dane, która uruchamia napisaną powyżej funkcję
  automobil_dane <- func_zbior_danych()
  str(automobil_dane)  
  head(automobil_dane)
  
  #Sprawdzamy jak wygląda po wstępnym preprocessingu nasz dataset
  str(automobil_dane)
  
  #Usuwam dane, które nie będą mi potrzebne. Tworzę zmienną automobil clean z oczyszczonymi danymi.
  automobil_clean <- select(automobil_dane, -normalized.losses, - symboling)

  #Sprawdzam, czy zostały usunięte dwie kolumny
  str(automobil_clean)  
  head(automobil_clean)

  #Definiuje nową zmienną, która zawiera dane z wartościami numerycznymi
  auto_summary <- automobil_clean[,c('wheel.base','length','width','height','curb.weight','engine.size',
                                     'bore','stroke','compression.ratio','horsepower','peak.rpm','city.mpg',
                                     'highway.mpg','price')]
  
  #Obliczam średnią, minimalne, maksymalne wartości, kwartyle oraz odchylenie standardowe poszczególnych zmiennych
  summary(auto_summary)
  
  #Obliczam odchylenie standardowe dla poszczególnych zmiennych
  cat('\nOdchylenie standardowe danych\n')
  lapply(auto_summary, sd)
  
  #Sprawdzam skośnośc rozkładów danych
  install.packages ("moments")
  library (moments)
  skewness(auto_summary)
  
  #Tabela częstotliwości
  freq_table <- table(automobil_clean$make, automobil_clean$fuel.type, automobil_clean$aspiration)
  table(automobil_clean$fuel.type)
  table(automobil_clean$aspiration)  
  
  # Wyodrębniam marki samochodów
    automobil_clean %>% group_by(make) %>% summarise(count = n()) %>%  
    ggplot(aes(x= reorder(make,count),y=count)) + geom_bar(stat = "identity") + coord_flip() +
    xlab("Marka samochodu") + ylab("Ilość")
    
  # Wykres pudełkowy cen w zależności od marki
  automobil_clean %>% select(make,price) %>%  ggplot(aes(x= make,y=price)) + geom_boxplot() + coord_flip() + xlab("Marka samochodu") + ylab("Cena")
  ###
  
  #Sprawdzam czy auta napędzane gazem mają mniejsze spalanie w mieście.
  library(ggplot2)
  ggplot(automobil_clean, aes(x=city.mpg, y=fuel.type)) + geom_boxplot()
 
   #Sprawdzam założenia testu t-Studenta -rozkład wyników zmiennej zależnej w każdej z analizowanych grup jest zbliżony do rozkładu normalnego
  shapiro.test(automobil_clean$city.mpg)
  
  #Zamieniam "gaz" na 1 oraz "diesel" na 0
  gaz_diesel_0_1 <- automobil_clean
  gaz_diesel_0_1$fuel.type[gaz_diesel_0_1$fuel.type == "gas"] <-1
  gaz_diesel_0_1$fuel.type[gaz_diesel_0_1$fuel.type == "diesel"] <- 0
  gaz_diesel_0_1$fuel.type <-as.integer(gaz_diesel_0_1$fuel.type)
  
  #Sprawdzam rozkład zmiennej fuel.type
  shapiro.test(gaz_diesel_0_1$fuel.type)
  
  #Sprawdzam homogeniczność wariancji w grupach
  library(car)
  leveneTest(city.mpg ~ fuel.type, data = automobil_clean)
  
  #Po sprawdzeniu zalozen wykonuje test studenta, by sprawdzic czy auta napędzane gazem mają mniejsze spalanie w mieście.
  t.test(city.mpg ~ fuel.type, data=automobil_clean)
  
  #Stosuję nieparametryczny odpowiednik testu istotności 2 wartości oczekiwanych
  #test U- MANNA-WHITNEY'A- WILCOXONA
  wilcox.test(city.mpg ~ fuel.type, data=gaz_diesel_0_1)
  #wilcox.test(city.mpg ~ fuel.type, data=gaz_diesel_0_1, exact = FALSE, alternative = "less")
  
  
  #Sprawdzam korelację zmiennych
  korelacja <- cor(auto_summary, method = "pearson")
  korelacja  
  cor.test(automobil_clean$engine.size, automobil_clean$curb.weight, method = "pearson")
  #Prezentuje korelacje na wykresie
  install.packages("ellipse")
  require(ellipse)
  library(RColorBrewer)
  ylOrBn4<-c("#FFFFD4", "#FED98E", "#FE9929","#CC4C02")
  plotcorr(korelacja, col = colorRampPalette(ylOrBn4)(10))
  #Inny sposob
  install.packages("corrplot")
  library(corrplot)
  corrplot(korelacja, method = "circle")
  
  ###
  
  #Sprawdzam czy ceny mają rozkład normalny
  ceny <- automobil_clean$price
  
  #Losowa próba z rozkładu normalnego ze średnią (price) i odchyleniem standardowym = sd (price)
  srednia_cen <- mean(ceny)
  odchyl_st_cen <- sd(ceny)
  rozkl_norm_ceny <- rnorm(195, mean = srednia_cen, sd = odchyl_st_cen) #rnorm to tzw. "random sampling from the normal distribution"

  
  cat(paste('Srednia cen = ', as.character(round(srednia_cen,2)),', wariancja cen = ', as.character(round(odchyl_st_cen^2,2)),', odchylenie standardowe cen = ', as.character(round(odchyl_st_cen,2))))
  
  #Losowa próba z rozkładu normalnego ze średnią (log (price)) i odchyleniem standardowym = sd (log (price))
  log_ceny <- log(automobil_clean$price)
  log_srednia_cen <- mean(log_ceny)
  log_odchyl_st_cen <- sd(log_ceny) 
  log_rozkl_norm_ceny <- rnorm(195, mean = log_srednia_cen, sd = log_odchyl_st_cen)
  
  cat(paste('Srednia logarytmu cen = ', as.character(round(log_srednia_cen,2)),', wariancja logarytmu cen = ', as.character(round(log_odchyl_st_cen^2,2)),', odchylenie standardowe logarytmu cen = ', as.character(round(log_odchyl_st_cen,2))))
      
  #Krzywa gęstosci zmiennej price
  krzywa_gestosci <- ggplot(automobil_clean, aes(price)) + geom_density()
  
  #Krzywa gęstosci zmiennej log(price)
  log_krzywa_gestosci <- ggplot(automobil_clean, aes(log(price))) + geom_density()
  
  #Zestawiam oby dwa wykresy
  install.packages("gridExtra")
  library("gridExtra")
  grid.arrange(krzywa_gestosci, log_krzywa_gestosci, ncol = 2)

  #zestawiam histogram zmiennej price z krzywą Gaussa - jednocześnie połączę ten wykres oraz poniższy dla porównania
  #Zmieniamy wymiar okna na 6 x 12 
  options(repr.plot.width=6, repr.plot.height=12)
  par(mfrow=c(2,1)) #Parametry mfrow i mfcol pozwalają na utworzenie macierzy wykresów w jednym obszarze kreślenia. 
  {
    hist(ceny, main=paste('Rozkład ceny samochodów VS rozkład Gaussa'),  freq=FALSE, xlim = c(-15000,55000))
    sekwencja <- seq(-15000,55000, length = 195)#używamy lenght = 195, ponieważ mamy 195 obserwacji w zbiorze
    gauss1 <- dnorm(sekwencja, mean = srednia_cen,sd = odchyl_st_cen) #dnorm to "density function of the normal distribution"
    lines(sekwencja, gauss1, xlim = c(-15000,55000))   
  }
  
  
  #Zestawiam histogram zmiennej log(price) z krzywą Gaussa - znajdzie sie od pod powyzszym wykresem
  
  {
    hist(log_ceny, main = 'Rozkład logarytmu ceny samochodów VS rozkład Gaussa', freq = FALSE, xlim = c(7,13))
    sekwencja2 <- seq(7, 13, length=195)
    gauss2 <- dnorm(sekwencja2, mean=log_srednia_cen, sd=log_odchyl_st_cen)
    lines(sekwencja2,gauss2,xlim=c(7,13))
  }
  par(mfrow=c(1,1)) 
  
  #Tworze wykres QQ, dzieki ktoremu rowniez mozemy zbadac normalnosc rozkladu danych
  
  options(repr.plot.width=10, repr.plot.height=5)
  
  par(mfrow=c(2,2))
  #Wykres QQ ceny
  qqnorm(ceny, main = 'Wykres QQ ceny')
  qqline(ceny, col = "firebrick")
  library("car")
  qqPlot(ceny, main = "Obszar referencyjny")
  #Wykres QQ logarytmu ceny
  qqnorm(log_ceny, main = 'Wykres QQ logarytmu ceny')
  qqline(log_ceny, col = "firebrick")
  qqPlot(log_ceny, main = "Obszar referencyjny")
  par(mfrow=c(1,1))

  #Tworze dystrybuanty
  #dystrybuanta zmiennej price
  dystrybuanta_cen <- sapply(sekwencja, function(x) {sum(ceny<x)/195}) #dystrybuanta empiryczna
  
  #dystrybuanta rozkładu normalnego
  dystrybuanta_rozkl_norm_cen <-sapply(sekwencja, function(x){sum(rozkl_norm_ceny<x)/195}) #dystrybuanta rozkładu normalnego

  #Sprawdzam maksymalna odleglosc między przyjętym CDF i EDF 
  maksymalna_roznica_miedzy_dystr <- max(abs(dystrybuanta_cen - dystrybuanta_rozkl_norm_cen))
  cat(paste("Maksymalna różnica między dystrybuantą empiryczną a teoretyczną", as.character(maksymalna_roznica_miedzy_dystr)))
  
  ###
  
  #Ustawiam szerokość i wysokość okienka wykresu oraz ilość i konfiguracje wykresów zaprezentowanych w okienku
  options(repr.plot.width=12, repr.plot.height=6)
  par(mfrow=c(1,2))
  
  #Wykres dystrybuanty empirycznej oraz teoretycznej 
  
  plot(ecdf(ceny), col='royalblue3', main ='CDF oraz EDF', xlab = 'Ceny', ylab = 'F(x)') #ecdf "compute an empirical cumulative distribution function". tworze dystrybuante empiryczna
  lines(ecdf(rozkl_norm_ceny), col='violetred3') #dzieki temu dystrybuanta teoretyczna bedzie na tym samym wykresie
 # mtext(text = expression(hat(F)[n](x)), side = 2, line = 2.5) proba zrobienia daszka nad F
  legend("bottomright", legend=c("EDF", "CDF"),col=c("royalblue", "violetred3"), lty = 1:1, cex=0.8)
  
  #Sprawdzam gdzie znajduje sie maksymalna odleglosc miedzy dystrybuantami 
 
  maksymalna_roznica_wykres_y <- which.max(abs(dystrybuanta_cen - dystrybuanta_rozkl_norm_cen)) #"which.max returns the position of the element with the maximal value in a vector."
  max_punkt_x <- sekwencja[maksymalna_roznica_wykres_y]
  plot(sekwencja, dystrybuanta_cen, col='royalblue3',pch=16, main ='Statystyka K-S', xlab = 'Ceny', ylab = 'F(x)') #ecdf "compute an empirical cumulative distribution function". tworze dystrybuante empiryczna
  points(sekwencja,dystrybuanta_rozkl_norm_cen,col='violetred3', pch=16) 
  lines(c(max_punkt_x,max_punkt_x), c(dystrybuanta_cen[maksymalna_roznica_wykres_y],dystrybuanta_rozkl_norm_cen[maksymalna_roznica_wykres_y]), col='black', lwd=7) #pierwszy wektor to x drugi wektor to y
  legend("bottomright", legend=c("EDF", "CDF"),col=c("royalblue", "violetred3"), lty = 1:1, cex=0.8)
  par(mfrow=c(1,1))
  
  #Przeprowadzam KS test
  ks.test(ceny,rozkl_norm_ceny)
  
  ###
  
  #Tworze dystrybuane logarytmu cen
  dystrybuanta_log_cen <- sapply(sekwencja2, function(x) {sum(log_ceny<x)/195}) #dystrybuanta empiryczna
  
  #Tworze dystrybuane rozkladu normalnego logarytmu cen
  dystrybuanta_rozkl_norm_log_cen <- sapply(sekwencja2, function(x) {(sum(log_rozkl_norm_ceny<x)/195)}) #dystrybuanta teoretyczna
  
  #Sprawdzam maksymalna odleglosc między przyjętym CDF i EDF 
  maksymalna_roznica_miedzy_log_dystr <- max(abs(dystrybuanta_log_cen - dystrybuanta_rozkl_norm_log_cen))
  cat(paste("Maksymalna różnica między dystrybuantą empiryczną a teoretyczną logarytmu cen", as.character(maksymalna_roznica_miedzy_log_dystr)))
  
  #Ustawiam szerokość i wysokość okienka wykresu oraz ilość i konfiguracje wykresów zaprezentowanych w okienku
  options(repr.plot.width=12, repr.plot.height=6)
  par(mfrow=c(1,2))
  
  #Wykres dystrybuanty empirycznej oraz teoretycznej 
  plot(ecdf(log_ceny), col='royalblue3', main ='CDF oraz EDF', xlab = 'Logarytm ceny', ylab = 'F(x)')
  lines(ecdf(log_rozkl_norm_ceny), col='violetred3')
  legend("bottomright", legend=c("EDF", "CDF"),col=c("royalblue", "violetred3"), lty = 1:2, cex=0.8)
  
  #Sprawdzam gdzie znajduje sie maksymalna odleglosc miedzy dystrybuantami 
  
  maksymalna_roznica_log_wykres_y <- which.max(abs(dystrybuanta_log_cen - dystrybuanta_rozkl_norm_log_cen))
  max_punkt_log_x <- sekwencja2[maksymalna_roznica_log_wykres_y]
  plot(sekwencja2, dystrybuanta_log_cen, col='royalblue3',pch=16, main ='Statystyka K-S', xlab = 'Logarytm ceny', ylab = 'F(x)')
  points(sekwencja2, dystrybuanta_rozkl_norm_log_cen, col='violetred3', pch=16)
  lines(c(max_punkt_log_x,max_punkt_log_x), c(dystrybuanta_log_cen[maksymalna_roznica_log_wykres_y], dystrybuanta_rozkl_norm_log_cen[maksymalna_roznica_log_wykres_y]), col="black",lwd=5)  
  par(mfrow=c(1,1))
  legend("bottomright", legend=c("EDF", "CDF"),col=c("royalblue", "violetred3"), lty = 1:2, cex=0.8)
  
  #Przeprowadzam KS test
  ks.test(log_ceny, log_rozkl_norm_ceny)

  #Przeprowadzam testy Shapiro
  shapiro.test(ceny)
  shapiro.test(log_ceny)
  
  #Wyodrebniam zmienną price z podziałem na typ silnika:std i turbo
  table(automobil_clean$aspiration)
  ssanie_ceny <- split(automobil_clean$price, as.factor(automobil_clean$aspiration), drop = FALSE)
  
  #Compute logarithms
  log_ssanie_ceny_std <- log(ssanie_ceny$std)
  log_ssanie_ceny_turbo <- log(ssanie_ceny$turbo)
  
  #Sprawdzam czy ceny samochodów z turbodoładowaniem różnią się istotnie od cen samochodów standardowych
 
  #Wykres dzieki ktoremu porownamy ceny
  histogramy_funkcja <- function(a, b, cols = c('podpis 1 histogramu', 'podpis 2 histogramu'), nbins = 20) #nbins okresla ile chce narysowac slupków
  {
    maksymalna_cena <- max(c(max(a), max(b)))
    minimalna_cena <- min(c(min(a), min(b)))
    breaks = seq(maksymalna_cena, minimalna_cena, length.out = (nbins + 1)) #nbins + 1 poniewaz podziałek musi być o 1 wiecej
    par(mfrow = c(2, 1)) #ustalam jak chce aby zostaly wyswietlone wykresy
    hist(a, breaks = breaks, main = paste('Histogram:', cols[1]), xlab = cols[1])
    abline(v = mean(a), lwd = 4, col = 'violetred3') #Linia wskazująca średnią
    legend("topright", legend=c("srednia"),col=c("violetred3"), lty = 1, cex=0.5, inset = .001, bty = "n")
    hist(b, breaks = breaks, main = paste('Histogram:', cols[2]), xlab = cols[2])
    abline(v = mean(b), lwd = 4, col = 'violetred3')
    legend("topright", legend=c("srednia"),col=c("violetred3"), lty = 1, cex=0.5, inset=.001, bty = "n")
    par(mfrow = c(1, 1))
  }
  options(repr.plot.width=7, repr.plot.height=10) ## Ustawiam wielkosc wykresu
  
  histogramy_funkcja(log_ssanie_ceny_turbo,log_ssanie_ceny_std, c('logarytm cen aut turbo', 'logarytm cen aut std'))
  
  #Wykonuje test studenta
  t.test(log_ssanie_ceny_turbo,log_ssanie_ceny_std, alternative = "greater")

  #Dziele zmienną cena (wektor cena) ze względu na wygląd auta (zmienna body.style)
  table(automobil_clean$body.style) #wyswietlam grupy
  ceny_nadwozia <- split(automobil_clean$price, as.factor(automobil_clean$body.style), drop = TRUE)

  #Obliczam logarytmy cen w poszczególnych grupach
  ceny_nadwozia_Hatchback <- log(ceny_nadwozia$hatchback)
  ceny_nadwozia_Sedan <- log(ceny_nadwozia$sedan)
  ceny_nadwozia_Wagon <- log(ceny_nadwozia$wagon)
  
  #Tworzę dataframe z trzema grupami, które będą faktorami
  ramka_danych <- data.frame('nadwozia'=c(rep(1,length(ceny_nadwozia$hatchback)),rep(2,length(ceny_nadwozia$sedan)),rep(3,length(ceny_nadwozia$wagon))),
                   'log_ceny' = c(log(ceny_nadwozia$hatchback),log(ceny_nadwozia$sedan),log(ceny_nadwozia$wagon)))
  
  ramka_danych$nadwozia <- factor(ramka_danych$nadwozia) # Przekształcam nadwozia w factor
  
  #Tworze wykresy pudełkowe
  boxplot(ramka_danych$log_ceny ~ ramka_danych$nadwozia, xlab="nadwozia",
          ylab="logarytm cen")
  legend("topright", legend=c("1 - hatchback", "2 - sedan", "3 - kombi"), cex=0.5, inset=.001, bty = "n")
  
  str(ramka_danych)
  
  #Test ANOVA - ANALIZA WARIANCJI (H0: wartości oczekiwane w podgrupach są sobie równe, H1: przynajmniej jedna jest różna)
  anova <- aov(log_ceny ~ nadwozia, data = ramka_danych) 
  
  # PODSUMOWANIE ANOVY - bez tego nie dostaniemy się do p-value
  summary(anova)
  print(anova)
  
  # TEST POST-HOC DLA ANOVY- test pozwalający zweryfikować w których dokładnie grupach zachodzą istotne różnice
  # (H0: m1=m2, H1: są różne )
  HSD <- TukeyHSD(anova)  
  HSD
  plot(HSD)
  
  #TESTOWANIE ZAŁOŻENIA O NORMALNOŚCI ROZKŁADU RESZT ANOVY
  # wydobywamy reszty
  anova_reszty <- residuals(object = anova )
  # test Shapiro- Wilka (H0: normalność rozkładu)
  shapiro.test(x = anova_reszty )
  
  #TESTY WERYFIKUJĄCE JEDNORODNOŚĆ WARIANCJI W WIĘCEJ NIŻ 2 GRUPACH (H0: wariancje są równe, H1: przynajmniej jedna jest różna)
  #test Levene'a
  leveneTest(log_ceny ~ nadwozia, data = ramka_danych)
  #test Bartletta
  bartlett.test(log_ceny ~ nadwozia, data = ramka_danych)
  
  #KIEDY ZAŁOŻENIA ANOVY NIE SĄ SPEŁNIONE STOSUJEMY JEJ NIEPARAMETRYCZNY ODPOWIEDNIK
  #test Kruskala-Wallisa
  kruskal.test(log_ceny ~ nadwozia, data = ramka_danych)
  
  #TEST POST-HOC DLA KRUSKALA-WALLISA
  install.packages("PMCMRplus")
  library(dunn.test)
  library(PMCMR)
  posthoc.kruskal.dunn.test(log_ceny ~ nadwozia, data = ramka_danych)
  

  