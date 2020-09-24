
#REZOLVARE CU FUNCTII FACUTE DE MANA

#Functia de densitate de probabilitate pentru repartitia trapezoidala
dtrapezoidal <- function(x, a, b, c, d){
    
  answer = vector()
  
  for(val in x){
    if(a == b && c == d){
      if(a <= val && val <= c){
        answer <- c(answer, 1 / (c - a))
      }
      else{
        answer <- c(answer, 0)
      }
    }
    else if (a <= val && val < b){
      answer <- c(answer, 2 / (d + c - a - b) * (val - a) / (b - a))
    }
    else if (b <= val && val < c){
      answer <- c(answer, 2 / (d + c - a - b))
    }
    else if(c<= val && val <= d){
      answer <- c(answer, 2 / (d + c - a - b) * (d - val) / (d - c))
    }
    else{
      answer <- c(answer, 0)
    }
  }
  return(answer)
}

#Functia de distributie pentru repartitia trapezoidala
ptrapezoidal <- function(x, a, b, c, d){
  
  answer = vector()
  
  for(val in x){
    if(a == b && c == d){
      if(val < a){
        answer <- c(answer, 0)
      }
      else if(a <= val && val <= c){
        answer <- c(answer, (val - a) / (c - a))
      }
      else if(val > c){
        answer <- c(answer, 1)
      }
    }
    else if(val < a){
      answer <- c(answer, 0)
    }
    else if(a <= val && val < b){
      answer <- c(answer, 1 / (d + c - b - a) / (b - a) * (val - a) * (val - a))
    }
    else if(b <= val && val < c){
      answer <- c(answer, 1 / (d + c - b - a) * (2 * val - a - b))
    }
    else if(c <= val && val < d){
      answer <- c(answer, 1 - 1/(d + c - b - a) / (d - c) * (d - val) * (d - val))
    }
    else if(val >= d){
      answer <- c(answer, 1)
    }
  }
  return(answer)
}

#Parametrizez functiile de mai sus pentru a le reprezenta

functieParametrizataPDF <- function(x){
  return(dtrapezoidal(x, -3, -2, -1, 0))
}
functieParametrizataCDF <- function(x){
  return(ptrapezoidal(x, -3, -2, -1, 0))
}

#Plotare
x = seq(-4, 5, 0.001)
par(mfrow = c(1,2))
plot(x, dtrapezoidal(x, -4, -3, 2, 5), main = "Functia de densiatate", col = "red")
plot(x, ptrapezoidal(x, -4, -3, 2, 5), main = "Functia de repartitie", col = "blue")

#CAZ SPECIAL ( REPARTITIE UNIFORMA )
plot(x, dtrapezoidal(x, -4, -4, 5, 5), main = "Functia de densiatate", col = "red")
plot(x, ptrapezoidal(x, -4, -4, 5, 5), main = "Functia de repartitie", col = "blue")

#CAZ SPECIAL (REPARTITIE TRIUNGHIULARA)
plot(x, dtrapezoidal(x, -4, 2, 2, 5), main = "Functia de densiatate", col = "red")
plot(x, ptrapezoidal(x, -4, 2, 2, 5), main = "Functia de repartitie", col = "blue")

#REZOLVARE CU FUNCTII PREDEFINITE
library("trapezoid")

x <- seq(from = 0, to = 1, by = 0.01)

# Plot default trapezoid distribution
curve(dtrapezoid(x, min = 0, mode1 = 1/3, mode2 = 2/3, max = 1), from = 0, to = 1)
# Plot triangular trapezoid distribution
curve(dtrapezoid(x, min = 0, mode1 = 1/2, mode2 = 1/2, max = 1), from = 0, to = 1)
# Plot uniform trapezoid distribution
curve(dtrapezoid(x, min = 0, mode1 = 0, mode2 = 1, max = 1), from = 0, to = 1)


curve(ptrapezoid(x, min = 0, mode1 = 1/3, mode2 = 2/3, max = 1, n1 = 2, n3 = 2,
                 alpha = 1, lower.tail = TRUE, log.p = FALSE))


