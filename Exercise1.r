# insertion sort function to sort array
insertionSortFunction <- function(x){
    n <- length(x);
    for (i in 2 : (n)){

        key = x[i];
        j = i - 1;

        while (j > 0 && x[j] > key){ 
          x[j + 1] = x[j] 
          j = j - 1 
        }

      x[j + 1] = key;
    }
    return(x);
}

averageFunction <- function(vector) {
	result = 0;
	for(i in vector){
		result <- (result + i);
	}

	result <- result / length(vector);
	return(result);
}

medianFunction <- function(vector) {
	vector <- insertionSortFunction(vector);
	n <- (length(vector)+1)/2;
	return(vector[n]);
}

varianceFunction <- function(vector, average) {
	result = 0;
	for(i in vector){
		result <- ((i- average)**2);
	}

	result <- result / (length(vector) -1);
	return(result);
}

MedianAverageVariancePrintsFunction <- function(df) {
print("Medianas:");
cat(sprintf("Mediana Feridos: %d", medianFunction(as.vector(df$feridos))), "\n");
cat(sprintf("Mediana Idade: %d", medianFunction(as.vector(df$idade))), "\n");
cat(sprintf("Mediana Envolvidas: %d", medianFunction(as.vector(df$envolvidas))), "\n");
cat("\n");

## Average calcs and prints
print("Medias:");
cat(sprintf("Media Feridos: %.2f", averageFunction(as.vector(df$feridos))), "\n");
cat(sprintf("Media Idade: %.2f", averageFunction(as.vector(df$idade))), "\n");
cat(sprintf("Media Envolvidas: %.2f", averageFunction(as.vector(df$envolvidas))), "\n");
cat("\n");

## Variance calcs and prints
print("Variancias:");
averageHolder <- averageFunction(as.vector(df$feridos));
holder <- varianceFunction(as.vector(df$feridos), averageHolder);
cat(sprintf("Variancia Feridos: %.2f | Desvio padrão: %.2f", holder, sqrt(holder)), "\n");

averageHolder <- averageFunction(as.vector(df$idade));
holder <- varianceFunction(as.vector(df$idade), averageHolder);
cat(sprintf("Variancia Idade: %.2f | Desvio padrão: %.2f", holder, sqrt(holder)), "\n");

averageHolder <- averageFunction(as.vector(df$envolvidas));
holder <- varianceFunction(as.vector(df$envolvidas), averageHolder);
cat(sprintf("Variancia Envolvidas: %.2f | Desvio padrão: %.2f", holder, sqrt(holder)), "\n");
cat("\n");

return(df);
}


### Main Function
exercise1A.data <- data.frame(
	id_acidente = c(1 : 20),

	veiculo = c("carro", "moto", "moto", "carro", "carro", 
				"carro", "moto", "carro", "carro", "carro", 
				"moto", "moto", "carro", "moto", "moto", 
				"carro", "moto", "carro", "moto", "carro"),

	gravidade = c("pequena", "elevada", "moderada", "moderada", "pequena", 
				"moderada", "elevada", "elevada", "elevada", "moderada", 
				"moderada", "elevada", "elevada", "pequena", "pequena", 
				"pequena", "moderada", "moderada", "elevada", "moderada"),

	feridos = c(0, 3, 2, 1, 1, 2, 4, 5, 4, 2, 2, 3, 4, 1, 0, 0, 2, 1, 3, 1),

	idade = c(35, 22, 25, 21, 40, 18, 18, 21, 20, 23, 25, 18, 22, 20, 44, 18, 26, 27, 33, 29),

	seguro = c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE,
				FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE),

	envolvidas = c(4, 4, 4, 8, 6, 5, 3, 2, 5, 4, 3, 3, 4, 2, 1, 3, 3, 2, 4, 3)
	);

## Exercise 1 - A)
# Median calcs and prints
cat("Exercise 1 - A)");
exercise1A.data = MedianAverageVariancePrintsFunction(exercise1A.data);
cat("\n");

## Exercise 1 - B)
cat("Exercise 1 - B)\n");
exercise1B.data <-exercise1A.data[ which(exercise1A.data$veiculo=='carro'),]
exercise1B.data = MedianAverageVariancePrintsFunction(exercise1B.data);
cat("\n");

## Exercise 1 - C)
cat("Exercise 1 - C)\n");
exercise1C.data <- exercise1A.data[ which(exercise1A.data$veiculo=='moto' & exercise1A.data$gravidade=='elevada'),]
exercise1C.data = MedianAverageVariancePrintsFunction(exercise1C.data);
cat("\n");

## Exercise 1 - D)
cat("Exercise 1 - D)\n");
feridos <- as.vector(exercise1A.data$feridos);
envolvidas <- as.vector(exercise1A.data$envolvidas);
taxa <- c();
taxa <- append(taxa, (feridos / envolvidas));
cat("Mediana:", medianFunction(taxa), "\n");
cat("Media:", averageFunction(taxa), "\n");