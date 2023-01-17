to_alphanumeric_lowercase <- function (x)
{
  tolower(gsub("[^a-zA-Z0-9_]", "\\_", x))
}


accented_letters <- function (x){
  stri_replace_all_fixed(x,
                         c("á","é","ń","í","ó","ú","ñ","ü","Á","Ó","Í","Ñ"),
                         c("a","e","n","i","o","u","n","u","A","O","I","N"),
                         vectorize_all = FALSE)}



