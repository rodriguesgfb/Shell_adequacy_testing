##Function to calculate Shell adequancy index (SAI) adapted from Vance, 1972. Vance, R. R. (1972). Competition and mechanism of coexistence in three sympatric of intertidal hermit crabs. Ecology, 53(6), 1062-1074.
##This function calcules SAI of a hermit crab pool interacting with a gastropod shell pool.


multiple_SAI<- function(list_hermit, list_shell, Shell_species, Hermit_species){
  Shell_names = Shell_species
  lm<- lm(log(list_hermit) ~ log(list_shell))
  b = lm[["coefficients"]][["(Intercept)"]]
  x = lm[["coefficients"]][["log(list_shell)"]]
  out <- c()
  for(i in list_hermit){
    for(j in list_shell){
      out<- c(out, (b+j^x)/i)
    }
  }
  my_vector <- vector(mode="numeric")
  for(s in Hermit_species){
    h_names<- rep(s, length(Hermit_species))
    my_vector <- append(my_vector, h_names)
  }
  return(data.frame(SAI = out, Shell_sp = rep(Shell_names, length(Shell_names)),Hermit_sp = my_vector))
}



