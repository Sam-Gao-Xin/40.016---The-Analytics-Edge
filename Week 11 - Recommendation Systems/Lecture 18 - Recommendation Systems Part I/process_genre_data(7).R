
process_genre_data <- function(genres) {
  #Levels are the unique factor values in that dataframe #Union takes the all the values common in the two level() function
  fac <- union(union(union(union(union(union(levels(genres$X1), levels(genres$X2)),levels(genres$X3)),levels(genres$X4)), levels(genres$X5)), levels(genres$X6)),levels(genres$X7))
  
  # To standardize across all variables
  genres$X1 <- factor(genres$X1, fac)
  genres$X2 <- factor(genres$X2, fac)
  genres$X3 <- factor(genres$X3, fac)
  genres$X4 <- factor(genres$X4, fac)
  genres$X5 <- factor(genres$X5, fac)
  genres$X6 <- factor(genres$X6, fac)
  genres$X7 <- factor(genres$X7, fac)
  levels(genres$X1)
  
  # Then, we create a matrix with 8569 rows (movies) and 20 columns (categories), 
  # and we assign the column name to matrix as genres
  M <- matrix(0,nrow=8569,ncol=20)
  colnames(M) <- fac
  
  # With this command, we create a matrix with entry 1 if a movie is of a particular genre (and 0 otherwise)
  for(i in 1:8569){
    M[i,genres[i,"X1"]] <- 1
    M[i,genres[i,"X2"]] <- 1
    M[i,genres[i,"X3"]] <- 1 
    M[i,genres[i,"X4"]] <- 1 
    M[i,genres[i,"X5"]] <- 1 
    M[i,genres[i,"X6"]] <- 1
    M[i,genres[i,"X7"]] <- 1
  }
    # Return M
    return(M)
}




























