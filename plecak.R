plecak <- function(limit, wagi, ceny, n){
  #jesli brak przedmiotow do wkladania
  #lub jesli plecak pelny, to zakoncz szukanie
  if (n == 0 || limit == 0) {
    return(list(0,vector()))
  } 

  #jesli przedmiot sie nie miesci
  #to zwroc obecny plecak, bez przedmiotu
  if (wagi[n] > limit) { 
    return(plecak(limit, wagi, ceny, n-1)) 
  }
  #jesli przedmiot sie miesci, 
  #to rozpatrz dwa przypadki
  else { 
    #a) nalezy go wziac do plecaka
    p1 <- plecak(limit - wagi[n], wagi, ceny, n-1)
    p1[[1]] <- p1[[1]] + ceny[n]
    p1[[2]] <- append(p1[[2]],n)
    
    #b) nie nalezy go brac do plecaka
    p2 <- plecak(limit, wagi, ceny,  n-1)
    
    #wybierz lepszy przypadek
   
    if (p1[[1]] >= p2[[1]]) {
      return(p1)
    } else {
      return(p2)
    }
  }
}


#przyklad
ceny <- c(60, 100, 120) 
wagi <- c(10, 20, 30) 
limit <- 50
n <- length(ceny) 
start_time <- Sys.time()
p <- plecak(limit, wagi, ceny, n)
end_time <- Sys.time()
print(paste("Najlepszy zestaw przedmiotow: ",p[2])) 
print(paste("Cena tych przedmiotow: ",p[[1]]))
print(paste("Algorytm dzialal ",end_time-start_time, " sekund"))

time = c()

#16
ceny<- sample(10:100,16,1)
wagi<- sample(5:100,16,1)
limit<- round(mean(wagi)*length(ceny)/2)
n <- length(ceny) 
start_time <- Sys.time()
p <- plecak(limit, wagi, ceny, n)
end_time <- Sys.time()
print(paste("Najlepszy zestaw przedmiotow: ",p[2])) 
print(paste("Cena tych przedmiotow: ",p[[1]]))
print(paste("Algorytm dzialal ",end_time-start_time, " sekund"))

time = append(time,as.numeric(end_time-start_time))

#18
ceny<- sample(10:100,18,1)
wagi<- sample(5:100,18,1)
limit<- round(mean(wagi)*length(ceny)/2)
n <- length(ceny) 
start_time <- Sys.time()
p <- plecak(limit, wagi, ceny, n)
end_time <- Sys.time()
print(paste("Najlepszy zestaw przedmiotow: ",p[2])) 
print(paste("Cena tych przedmiotow: ",p[[1]]))
print(paste("Algorytm dzialal ",end_time-start_time, " sekund"))

time = append(time,as.numeric(end_time-start_time))

#20

ceny<- sample(10:100,20,1)
wagi<- sample(5:100,20,1)
limit<- round(mean(wagi)*length(ceny)/2)
n <- length(ceny) 
start_time <- Sys.time()
p <- plecak(limit, wagi, ceny, n)
end_time <- Sys.time()
print(paste("Najlepszy zestaw przedmiotow: ",p[2])) 
print(paste("Cena tych przedmiotow: ",p[[1]]))
print(paste("Algorytm dzialal ",end_time-start_time, " sekund"))


time = append(time,as.numeric(end_time-start_time))

#22

ceny<- sample(10:100,22,1)
wagi<- sample(5:100,22,1)
limit<- round(mean(wagi)*length(ceny)/2)
n <- length(ceny) 
start_time <- Sys.time()
p <- plecak(limit, wagi, ceny, n)
end_time <- Sys.time()
print(paste("Najlepszy zestaw przedmiotow: ",p[2])) 
print(paste("Cena tych przedmiotow: ",p[[1]]))
print(paste("Algorytm dzialal ",end_time-start_time, " sekund"))

time = append(time,as.numeric(end_time-start_time))


ilosc_przedmiotow= c(16,18,20,22)


plot(ilosc_przedmiotow,time,type= "o",main = "Wykres czasu wzgledem ilosci przedmiotow",xlab="Ilosc przedmiotow",ylab="Czas")

warnings()