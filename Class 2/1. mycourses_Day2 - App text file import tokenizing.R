library(textreadr)
#this could be one document in your case, 
#if that is the case, import only one document
MBA <- read_document(file="xxxxxxxxx")
MIB <- read_document(file="xxxxxxxx")
class_combo <- c(MBA, MIB)

my_df <- as.data.frame(matrix(nrow=60, ncol=9))

for(z in 1:9){
  for(i in 1:60){
    my_df[i,z]<- class_combo[i*9+z-9]
  }#closing z loop
}#closing i loop


