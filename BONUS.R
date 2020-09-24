frepcomgen <- function(n, m) {
  #Initializarea tuturor variabilelor necesare
  A <- matrix(nrow = n + 2, ncol = m + 2) #matricea 'masca'
  dl <- c(0, 1, 0, -1) #directia pe linii
  dc <- c(1, 0, -1, 0) #directia pe coloane
  d <- 1 
  i <- 2
  j <- 2
  A[2, 2] = 1
  previ <- 2
  prevj <- 2
  ct <- n * (m + 1)
  
  #Generarea elementelor matricei care vor fi cunoscute la inceput
  # -1<-necunoscut  1<-cunoscut
  #Parcurgem matricea in spirala iar atunci cand schimbam directia marcam cu -1 casuta
  while (ct > 0) {
    i = i + dl[d]
    j = j + dc[d]
    if (i > n + 1 ||
        i < 2 || j > m + 2 || j < 2 || is.na(A[i, j]) == FALSE) { 
      #daca am gasit un element deja generat sau am iesit din limitele matricei schimbam directia
      i = previ
      j = prevj
      A[i, j] = -1 #marcarea
      d = d + 1 #schimbarea directiei
      if (d == 5)
        d = 1
      i = i + dl[d]
      j = j + dc[d]
    }
    previ = i
    prevj = j
    if (is.na(A[i, j]) == TRUE)
      A[i, j] = 1
    ct <- ct - 1
  }
  A[n + 2, 2] = -1
  #Generam ultima coloana
  for (j in 2:m + 1) {
    A[n + 2, j] = 1
  }
  
  #Pre-generarea tuturor valorilor matricei
  sline = vector(mode = "integer", length = n + 2) #sline[i] - suma liniei i
  scol = vector(mode = "integer", length = m + 2) #scol[i] - suma coloanei i
  B <- matrix(nrow = n + 2, ncol = m + 2) #matricea de valori brute
  Sum <- 0
  #generam valori pentru intreaga matrice, cu exceptia ultimei linii si a ultimei coloane
  for (i in 2:(n + 1))
    for (j in 2:(m + 1)) {
      B[i, j] = sample(0:100, 1)
      sline[i] = sline[i] + B[i, j]
      scol[j] = scol[j] + B[i, j]
      Sum <- Sum + B[i , j]
    }
  
  #generam valori pentru ultima coloana
  for (i in 2:(n + 1))
    B[i, m + 2] = sline[i]
  
  #generam valori pentru ultima linie
  for (i in 2:(m + 1))
    B[n + 2, i] = scol[i]
  
  C <- matrix(nrow = n + 2, ncol = m + 2) #matriciea finala - combinatie intre matricea 'masca' si cea cu valori brute
  for (i in 2:(n + 2))
    for (j in 2:(m + 2)) {
      if (!is.na(A[i, j]) && A[i, j] == 1) { #se completeaza doar elementele care au valoare 1 in matricea A
        C[i, j] <- B[i, j] / Sum #se imparte la Sum pentru ca valorile sa aiba sens
      }
    }
    #generam valorile pentru X si pentru Y
    v1 <- sample(-10:10, ncol(C))
    v2 <- sample(-10:10, nrow(C))
    v1<-sort(v1,decreasing = FALSE)
    v2<-sort(v2,decreasing = FALSE)
    v1[ncol(C)]=NA
    v2[nrow(C)]=NA
    C[1,]<-v1
    C[,1]<-v2
    C[1, 1] <- "x\\y"
    C[nrow(C),ncol(C)]<-"1"
    
  #returnez matricea generata
  return(C)
}



fcomplrepcom <- function(A) {
  ok <- TRUE
  
  #cat timp mai am elemente necompletate execut acest while
  while (ok == TRUE) {
    ok <- FALSE
    
    #calculez suma pe linii
    sline = vector(mode = "integer", length = nrow(A))
    for (i in 2:(nrow(A)))
      sline[i] = ncol(A)
    
    #calculez suma pe coloane
    scol = vector(mode = "integer", length = ncol(A))
    for (i in 2:(nrow(A)))
      scol[i] = nrow(A)
    
    #verific daca sunt elementele necompletate
    for (i in 2:(nrow(A)))
      for (j in 2:(ncol(A))) {
        if ((i != nrow(A) || j != ncol(A)) && is.na(A[i, j])) {
          ok<-TRUE
          sline[i] <- sline[i] - 1
          scol[j] <- scol[j] - 1
        }
      }
    
    #completarea pe linii
    for (i in 2:(nrow(A) - 1)) {
      if (sline[i] == ncol(A) - 1) {
        sum <- 0.0
        poz <- 0 #indicele elementului necompletat
        for (j in 2:(ncol(A) - 1)) {
          if (!is.na(A[i, j])) {
            sum <- sum + as.numeric(A[i, j])
          }
          else{
            poz <- j
          }
        }
        #completez pozitia gasita
        if (poz != 0) { 
          A[i, poz] = as.numeric(A[i, ncol(A)]) - sum
        }
        else {  #caz particular in care pozitia este pe ultima coloana
          poz<-ncol(A)
          A[i, ncol(A)] = sum
        }
        sline[i] <- sline[i] + 1
        scol[poz] <- scol[poz] + 1
        
      }
    }
    #completez ultima linie
    sum <- 0
    if (sline[nrow(A)] == ncol(A) - 1) {
      for (j in 2:(ncol(A) - 1))
        if (!is.na(A[nrow(A), j])) {
          sum <- sum + as.numeric(A[nrow(A), j])
        }
      else{
        poz <- j
      }
      #completez pozitia gasita
      A[nrow(A),poz]<-1-sum
      sline[nrow(A)] <- sline[nrow(A)] + 1
      scol[poz] <- scol[poz] + 1
    }
    
    #completarea pe coloane
    for (j in 2:(ncol(A) - 1)) {
      if (scol[j] == nrow(A) - 1) {
        sum <- 0.0
        poz <- 0
        for (i in 2:(nrow(A) - 1)) {
          if (!is.na(A[i, j])) {
            sum <- sum + as.numeric(A[i, j])
          }
          else{
            poz <- i
          }
        }
        #completez pozitia gasita
        if (poz != 0) {
          A[poz, j] = as.numeric(A[nrow(A), j]) - sum
        }
        else {#caz particular in care pozitia este pe ultima linie
          poz<-nrow(A)
          A[nrow(A), j] = sum
        }
        sline[poz] <- sline[poz] + 1
        scol[j] <- scol[j] + 1
      }
    }
    #completez ultima coloana
    sum <- 0
    if (scol[ncol(A)] == nrow(A) - 1) {
      for (i in 2:(nrow(A) - 1))
        if (!is.na(A[i, ncol(A)])) {
          sum <- sum + as.numeric(A[i, ncol(A)])
        }
      else{
        poz <- i
      }
      #completez pozitia gasita
      A[poz,ncol(A)]<-1-sum
      sline[poz] <- sline[poz] + 1
      scol[ncol(A)] <- scol[ncol(A)] + 1
    }
  }
  
  #returnez matricea completata
  return(A)
}

#Cov(5X, -3Y)
functiec1 <- function(A){
  
  #extrag repartitia lui X
  X <- matrix(nrow = 2, ncol = nrow(A)-2)
  X[1,] <- 5*as.numeric(A[2:(nrow(A)-1),1])
  X[2,] <- A[2:(nrow(A)-1),ncol(A)]
  
  #extrag repartitia lui Y
  Y <- matrix(nrow = 2, ncol = ncol(A)-2)
  Y[1,] <- (-3)*as.numeric(A[1,2:(ncol(A)-1)])
  Y[2,] <- A[nrow(A),2:(ncol(A)-1)]
  
  #calculez media lui X
  E1<-0
  for(i in 1:ncol(X)){
    E1<-E1+as.numeric(X[1,i])*as.numeric(X[2,i])
  }
  
  #calculez media lui Y
  E2<-0
  for(i in 1:ncol(Y)){
    E2<-E2+as.numeric(Y[1,i])*as.numeric(Y[2,i])
  }
  
  #calculez media lui XY
  E12<-0
  XY<-matrix(nrow = 2, ncol = ncol(X)*ncol(Y))
  ct<-0
  for(i in 1:ncol(X)){
    for(j in 1:ncol(Y)){
      ct<-ct+1
      XY[1,ct]<-as.numeric(X[1,i])*as.numeric(Y[1,j])
      XY[2,ct]<-A[i+1,j+1]
      E12<-E12+as.numeric(XY[1,ct])*as.numeric(XY[2,ct])
    }
  }
  XY<-XY[,order(as.numeric(XY[1,]))]
  
  #returnez covarianta
  return(E12-E1*E2)
}

#Aceasta functie este copy-paste de la cea precedenta, singura diferenta find faptul ca am sters constantele 5 si -3
#Cov(X,Y)
covar <- function(A){
  X <- matrix(nrow = 2, ncol = nrow(A)-2)
  X[1,] <- as.numeric(A[2:(nrow(A)-1),1])
  X[2,] <- A[2:(nrow(A)-1),ncol(A)]
  
  
  Y <- matrix(nrow = 2, ncol = ncol(A)-2)
  Y[1,] <- as.numeric(A[1,2:(ncol(A)-1)])
  Y[2,] <- A[nrow(A),2:(ncol(A)-1)]
  
  E1<-0
  for(i in 1:ncol(X)){
    E1<-E1+as.numeric(X[1,i])*as.numeric(X[2,i])
  }
  
  E2<-0
  for(i in 1:ncol(Y)){
    E2<-E2+as.numeric(Y[1,i])*as.numeric(Y[2,i])
  }
  
  E12<-0
  XY<-matrix(nrow = 2, ncol = ncol(X)*ncol(Y))
  ct<-0
  for(i in 1:ncol(X)){
    for(j in 1:ncol(Y)){
      ct<-ct+1
      XY[1,ct]<-as.numeric(X[1,i])*as.numeric(Y[1,j])
      XY[2,ct]<-A[i+1,j+1]
      E12<-E12+as.numeric(XY[1,ct])*as.numeric(XY[2,ct])
    }
  }
  XY<-XY[,order(as.numeric(XY[1,]))]
  #print(XY)
  return(E12-E1*E2)
}

#P(0<X<3/Y>2)
functiec2<- function(A){
  sum<-0
  sumY<-0
  for(i in 2:(nrow(A)-1))
    for(j in 2:(ncol(A)-1))
    { #Adun toate elementele din repartitia comuna care respecta ambele proprietati
      if(as.numeric(A[i,1])>0&&as.numeric(A[i,1])<3&&as.numeric(A[1,j])>2){
        sum<-sum+as.numeric(A[i,j])
      }
    }
  for(j in 2:(ncol(A)-1))
    if(as.numeric(A[1,j])>2)sumY<-sumY+as.numeric(A[nrow(A),j])
  
  #returnez P(0<X<2,Y>2)/P(Y>2)
  return(sum/sumY)
}

#P(X>6,Y<7)
functiec3 <- function(A){
  sum<-0
  for(i in 2:(nrow(A)-1))
    for(j in 2:(ncol(A)-1))
    {  #Adun toate elementele din repartitia comuna care respecta ambele proprietati
      if(as.numeric(A[i,1])>6&&as.numeric(A[1,j])<7)sum<-sum+as.numeric(A[i,j])
    }
  return(sum)
}

#functie ce verifica daca repartitiile sunt independente
fverind <- function(A){
  ok<-TRUE
  for(i in 2:(nrow(A)-1))
    for(j in 2:(ncol(A)-1))
      if(as.numeric(A[i,j])!=as.numeric(A[nrow(A),j])*as.numeric(A[i,ncol(A)]))ok<-FALSE
  return(ok)
}

#functie ce verifica daca repartitille sunt necorelate
fvernecor <- function(A){
  if(abs(covar(A))==0) return(TRUE)
     return(FALSE)
}

#Testarea functiile scrise anterior
A <- frepcomgen(10, 10)
A <- fcomplrepcom(A)
B <-matrix(c(NA,1,2,3,NA,1,1/4,1/4,0,1/2,2,0,1/4,1/4,1/2,NA,1/4,1/2,1/4,NA),nrow=4,byrow=TRUE)
functiec1(A)
functiec2(A)
functiec3(A)

B<-matrix(c(NA,1,2,NA,1,1/4,1/4,1/2,2,1/4,1/4,1/2,NA,1/2,1/2,1), nrow=4, byrow=TRUE)
fverind(B)
fvernecor(B)

