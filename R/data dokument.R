#' Et eksempel på et datasæt der skal formateres
#' @encoding UTF-8
#' @format Et datasæt med 5 variable og 9 observationer
#'
"datasaetFormateringsfunktioner"


#' Et eksempel på en formatfil
#' @encoding UTF-8
#' @encoding UTF-8
#' @format Formatfil til en datasæt indeholder fem variable og x antal observationer svarende til de variable man ønsker at beholde i datasættet som formatfilen tilhører.
#'     Hver række i formatFilen svarer til en variabel, man ønsker at beholde og formatere i datasættet.\cr
#'     formatFilen består af følgende fem variable:\cr
#'     \itemize{
#'     \item 'variabelNavn' navnet på variablen man ønsker at formatere og beholde i datasættet
#'     \item 'type' den ønskede variabeltype for variablen
#'     \item 'datoFormat' datoformatet som variablen skal konverteres fra (eks. \%d-\%m-\%Y eller '\%d-\%m-\%Y \%H:\%M')
#'     \item 'nytVarNavn' navnet på variablen fremover
#'     \item 'origin' origin for dato (dato og tid) variablen, når denne er numerisk. Default for SQL er '1960-01-01' og '1970-01-01' for R.
#'     }
#'
"formatfilFormateringsfunktioner"
