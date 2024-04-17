####POBIERANIE I PORZ¥DKOWANIE DANYCH

##Ustawianie lokalizacji pliku Excela
getwd()
setwd("C:/Users/Asus/Desktop/projekt statystyka")
##Biblioteki
library(readxl) #do funkcji read_exel
library(dplyr) #do funkcji mutate_at
library(e1071) #do parametrów typu skoœnoœci itp.
library(scatterplot3d) #do wykresu punktowego
library(ggplot2) #do wykresu s³upkowego
##Pobieranie pliku Excel
tabl22_budzet_panstwa_2 <- read_excel("tabl22_budzet_panstwa_2.xlsx",  na = "0", col_names=FALSE)
View(tabl22_budzet_panstwa_2)
##Uporz¹dkowywanie tabeli
tabela <- as.data.frame(tabl22_budzet_panstwa_2[-c(1:4),]) #zamienianie typu tabeli z tibble na ramkê danych i usuwanie niepotrzebnych wierszy
tabela <- tabela[-2,] #usuniêcie niepotrzbnego wiersza
rownames(tabela) <- tabela[,1] #ustawienie pierwszej kolumny na nazwê wierszy
colnames(tabela) <- tabela[1,] #ustawienie pierwszego wiersza na nazwê kolumn
tabela <- tabela[-1,-1] #usuniêcie zduplikowanego wiersza i kolumny
tabela <- tabela[ , colSums(is.na(tabela))==0] #usuniêcie kolumn o pustych wartoœciach
tabela[is.na(tabela) | tabela == "-"] <- 0 #zmienienie znaku - na wartoœæ 0
tabela[is.na(tabela) | tabela == "NA"] <- 0   #zmienienie wartosci NA - na wartoœæ 0
##Zmiana nazw kolumn 
names(tabela) #sprawdzenie, jak wygl¹daj¹ przed zmian¹
obecne_nazwy_kolumn <- colnames(tabela) # Pobranie obecnych nazw kolumn by usun¹æ "\r" i "\n" z nazw kolumn
nowe_nazwy_kolumn <- trimws(obecne_nazwy_kolumn, "both", "\r\n") # Usuniêcie znaków specjalnych "\r" i "\n" z nazw kolumn
colnames(tabela) <- nowe_nazwy_kolumn # Przypisanie nowych nazw kolumn do ramki danych
names(tabela) #sprawdzenie, jak wygl¹daj¹ po zmianie
##Zmiana typu danych na numeryczny
tabela <- mutate_at(tabela, vars(1:22), as.numeric)

####OBLICZANIE PARAMETRÓW

#1. ŒREDNIA
œrednia <- mean(tabela$`Ogó³em`)
œrednia
#2. MEDIANA
mediana <- median(tabela$`œrodki w³asne `)
mediana
#3. NAJWIÊKSZA WARTOŒÆ
najwiêksza_wartoœæ <- max(tabela$`z dywidend i wp³yów z zysku`)
najwiêksza_wartoœæ
#4. NAJMNIEJSZA WARTOŒÆ
najmniejsza_wartoœæ <- min(tabela$`z podatku dochodowego od osób fizycznych`)
najmniejsza_wartoœæ
#5. WARIANCJA
wariancja <- var(tabela$`z podatku od towarów i us³ug (VAT)`)
wariancja
#6. ODCHYLENIE STANDARDOWE
odchylenie_standardowe <- sd(tabela$`z podatku akcyzowego`)
odchylenie_standardowe
#7. SUMA
suma <- sum(tabela$`z podatków poœrednich`)
suma
#8. LICZBA UNIKALNYCH
liczba_unikalnych <- length(unique(tabela$`Wynik bud¿etu pañstwa`))
liczba_unikalnych
#9. PIERWSZY KWARTYL
pierwszy_kwartyl <- quantile(tabela$Ogó³em, 0.25)
pierwszy_kwartyl
#10. SKOŒNOŒÆ
skosnosc <- skewness(tabela$`subwencja ogólna dla jednostek samorz¹du terytorialnego`)
skosnosc
#11. KURTOZA
kurtoza <- kurtosis(tabela$`z podatku akcyzowego`)
kurtoza
#12. SUMA KUMULTATYWNA
suma_kumulatywna <- cumsum(tabela$`wydatki bie¿¹ce jednostek bud¿etowych`)
suma_kumulatywna
#Ramka z parametrami
ramka_danych <- data.frame(œrednia, mediana, najwiêksza_wartoœæ, najmniejsza_wartoœæ, wariancja, odchylenie_standardowe, suma, liczba_unikalnych, pierwszy_kwartyl, skosnosc, kurtoza)
####GRAFICZNA PREZENTACJA DANYCH


#HISTOGRAM
par(mar=c(5, 6, 4, 2) + 0.1) #ustawienie wiêkszego marginesu dla osi y
hist(tabela$`z podatków poœrednich`, main="Histogram - z podatków poœrednich", xlab = "Zakres wartoœci podatków poœrednich", ylab= "Iloœæ obserwacji w ka¿dym z przedzia³ów", col="darkblue", border="white") 
legend("topright", legend="Przedzia³y wartoœci", col="darkblue", lwd=10, cex=0.5)
#WYKRES PUDE£KOWY
boxplot(tabela$`z podatku od towarów i us³ug (VAT)`, 
        main="Wykres pude³kowy - z podatku od towarów i us³ug (VAT)", 
        col="darkgreen", 
        border="white",
        xlab="Okresy miesiêczne (2010-2022)",
        ylab="Wartoœci podatku od towarów i us³ug")
legend("topright", legend="Wp³ywy z podatku od towarów i us³ug(VAT)", col="darkgreen", lwd=10, cex=0.5)
#DYSTRYBUANTA
par(mar=c(5, 6, 4, 2) + 0.1) #ustawienie wiêkszego marginesu dla osi y
plot(ecdf(tabela$`z podatku od niektórych instytucji finansowych`), main="Dystrybuanta - z podatku od niektórych instytucji finansowych", xlab = "Wartoœci dla dystrybuanty", ylab= "Funkcja dystrybunaty", col="purple")
legend("bottomright", legend="wartoœci z kolumny:z podatku od niektórych instytucji finansowych", col="purple", lwd=10, cex=0.5)
#WYKRES KOLUMNOWY
barplot(tabela$`z podatku akcyzowego`, main="Wykres kolumnowy - z podatku akcyzowego", ylab="Wartoœci wp³ywów z podatku akcyzowego", xlab="Miesi¹ce na przedziale 2010-2022r.", col="orange")
legend("topright", legend="Wp³ywy z podatku akcyzowego", col="orange", lwd=10, cex=0.5)
#WYKRES KO£OWY
par(mar=c(6, 7, 7, 7) - 6)  #ustawienie marginesu
pie(tabela$`z wp³yów z c³a`, main="Wykres ko³owy - z wp³ywów z c³a", col=c("red", "blue", "green", "yellow"), cex=0.5)
legend("topright", legend=c("z wp³ywów z c³a A", "z wp³ywów z c³a B", "z wp³ywów z c³a C", "z wp³ywów z c³a D"), col=c("red", "blue", "green", "yellow"), lwd=10, cex=0.5)
#WYKRES PUNKTOWY
scatterplot3d(tabela$`obs³uga d³ugu Skarbu Pañstwa`,tabela$`z podatku od towarów i us³ug (VAT)`, tabela$`z podatku dochodowego od osób prawnych`, xlab= "Obs³uga d³ugu Skarbu Pañstwa", ylab = "Z podatku od towarów i us³ug (VAT)", zlab = "Z podatku dochodowego od osób prawnych", color="darkred")
legend("topright", legend="Wykres punktowy", col="darkred", lwd=10, cex=0.5)
#WYKRES S£UPKOWY
ggplot(data = tabela, aes(x = `œrodki z Unii Europejskiej i innych Ÿróde³ niepodlegaj¹ce zwrotowi`, y = `œrodki z Unii Europejskiej i innych Ÿróde³ niepodlegaj¹ce zwrotowi`, fill = `œrodki z Unii Europejskiej i innych Ÿróde³ niepodlegaj¹ce zwrotowi`)) +
  geom_bar(stat = "identity", width = 100) +
  labs(title = "Wykres s³upkowy", x = "Wartoœci", y = "Liczba wyst¹pieñ") +
  scale_fill_gradient(low = "red", high = "blue") +
  theme(axis.text.x = element_text(size = 10))


####WERYFIKACJA HIPOTEZ STATYSTYCZNYCH

#Hipoteza pierwsza
#Za³ó¿my, ¿e chcemy przetestowaæ hipotezê zerow¹, ¿e œrednia wartoœæ w pierwszej kolumnie jest równa 200000. Jest to przybli¿ona liczba, z tej któr¹ wyliczyliœmy w parametrach.
wynik_hip1 <- t.test(tabela[, 1], mu = 200000, alternative = "greater") # Wykonanie testu t
print(wynik_hip1) # Wyœwietlenie wyników testu

#Hipoteza druga
# Za³ó¿my, ¿e chcemy przetestowaæ hipotezê zerow¹, która mówi, ¿e wariancja kolumny trzeciej: z podatku z towarów i us³ug (VAT) jest taka sama co wariancja kolumny czwartej: z podatku akcyzowego, przy hipotezie alternatywnej mówi¹cej, ¿e s¹ one ró¿ne. Poziom istotnoœci wynosi 0,05. Skorzysta³am z funkcji var.test, która porówna³a wariancjê tych dwóch próbek.
wynik_hip2 <- var.test(tabela$`z podatku od towarów i us³ug (VAT)`, tabela$`z podatku akcyzowego`, ratio=1)
print(wynik_hip2)


