#exercise 1

c(0L,1L,1L,2L,3L,5L,8L,13L,21L)

c(one = 42, two = 42, three = 3.14)

c("University", "of", "Illinois", "at", "Urbana-Champaign")

c(TRUE, TRUE, FALSE, TRUE, TRUE, FALSE,TRUE, TRUE, FALSE,TRUE, TRUE, FALSE,TRUE, TRUE, FALSE,TRUE, TRUE, FALSE,TRUE, TRUE, FALSE,TRUE, TRUE, FALSE,TRUE, TRUE, FALSE,TRUE, TRUE, FALSE)



#exercise 2

c_matrix <- matrix(1:100, nrow = 25, byrow = FALSE)

r_matrix <- matrix(1:100, nrow = 25, byrow = TRUE)



#exercise 3

list(x=c(42),y=matrix(nrow = 5, ncol=2), z=c(month.name))



#exercise 4

starter_pokemon <- data.frame(pokedex_num = c(1:9),  
                              name = c("Bulbasaur", "Ivysaur", "Venusaur", "Charmander", "Charmeleon", "Charizard", "Squirtle", "Wartortle", "Blastoise"), 
                              type_primary = c("Grass", "Grass", "Grass", "Fire", "Fire", "Fire", "Water", "Water", "Water"),
                              type_secondary = c("Poison", "Poison", "Poison", "", "", "", "", "", ""))

starter_pokemon



#exercise 5

pokemon = read.csv("https://stat385.org/data/pokemon.csv")
head(pokemon)
tail(pokemon)
pokemon[1:25, 1:4]
pokemon[pokemon$legendary == TRUE, ]
