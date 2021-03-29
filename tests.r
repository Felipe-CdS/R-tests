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


### Main Function

exercise1.data <- data.frame(
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

## Median calcs and prints
print("Medianas:");
holder <- medianFunction(as.vector(exercise1.data$feridos));
sprintf("Mediana Feridos: %d", holder);

holder <- medianFunction(as.vector(exercise1.data$idade));
sprintf("Mediana Idade: %d", holder);

holder <- medianFunction(as.vector(exercise1.data$envolvidas));
sprintf("Mediana Envolvidas: %d", holder);
cat("\n");

## Average calcs and prints
print("Medias:");
holder <- averageFunction(as.vector(exercise1.data$feridos));
sprintf("Media Feridos: %.2f", holder);

holder <- averageFunction(as.vector(exercise1.data$idade));
sprintf("Media Idade: %.2f", holder);

holder <- averageFunction(as.vector(exercise1.data$envolvidas));
sprintf("Media Envolvidas: %.2f", holder);
cat("\n");

## Variance calcs and prints
print("Variancias:");
averageHolder <- averageFunction(as.vector(exercise1.data$feridos));
holder <- varianceFunction(as.vector(exercise1.data$feridos), averageHolder);
sprintf("Variancia Feridos: %.2f | Desvio padrão: %.2f", holder, sqrt(holder));

averageHolder <- averageFunction(as.vector(exercise1.data$idade));
holder <- varianceFunction(as.vector(exercise1.data$idade), averageHolder);
sprintf("Variancia Idade: %.2f | Desvio padrão: %.2f", holder, sqrt(holder));

averageHolder <- averageFunction(as.vector(exercise1.data$envolvidas));
holder <- varianceFunction(as.vector(exercise1.data$envolvidas), averageHolder);
sprintf("Variancia Envolvidas: %.2f | Desvio padrão: %.2f", holder, sqrt(holder));
cat("\n");