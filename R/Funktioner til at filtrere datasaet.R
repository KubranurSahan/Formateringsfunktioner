#' Filtrerer datasæt med udgangspunkt i en enkelt variabel
#' @encoding UTF-8
#'
#' @param datasaet Datasættet som skal filtreres (dataframe)
#' @param variablen Navnet på variablen filtrering tager udgangspunkt i (character)
#' @param vaerdi Den eller de værdier for den valgte variabel, hvor observationerne med værdien i variablen skal beholdes
#'     vaerdi skal have samme objektype som 'variablen' i 'datasaet'.
#' @param tilladNAIVariablen TRUE/FALSE variabel der angiver om der må filtreres med udgangspunkt i 'variablen' når denne indeholder NA.
#'     Default er FALSE - Hvis NA findes i 'variablen' melder funktionen fejl.
#'
#' @return datasaet hvor kun de rækker med de angivne 'vaerdi'er i 'variablen'
#'
#' @examples
#' datasaetEksempel <- retOgFormaterDatasaet(datasaettet = datasaetFormateringsfunktioner, datasaetFormatFilen = formatfilFormateringsfunktioner)
#' filtrereDatasaetEnkeltVariabel(datasaet = datasaetEksempel, variablen = "ydelsesType", vaerdi = c("X", "XY"))
filtrereDatasaetEnkeltVariabel <- function(datasaet, # Datasættet som skal filtreres (dataframe)
                                           variablen, # Navnet på variablen filtrering tager udgangspunkt i (character)
                                           vaerdi, # Den eller de værdier for den valgte variabel, hvor observationerne med værdien i variablen skal beholdes
                                           tilladNAIVariablen = FALSE){

  # Tjekker om variablene er angivet korrekt
  if(!("data.frame" %in% class(datasaet)) | class(variablen) != "character" | length(variablen) != 1){

    stop("Mindst et af inputs til filtrereDatasaet funktionen er ikke angivet korrekt.
         'datasaet' skal være en data.frame
         'variablen' skal være af typen character med en enkelt værdi/entry
         'vaerdier' skal være af typen character")

  }

  # Tjekker til at starte med om variablen findes i datasættet
  if(!(variablen %in% colnames(datasaet))){

    stop("Den valgte variabel findes ikke i datasættet.")

  }

  # Ændrer navnet på variablen i datasættet til 'variablen'
  colnames(datasaet)[colnames(datasaet) == variablen] <- "variablen"

  if(!tilladNAIVariablen){
    # Tjekker om variablen indeholder NA' values
    if(sum(is.na(datasaet$variablen)) != 0){

      stop("Den valgte variabel indeholder NA'er. Der kan ikke filtreres med udgangspunkt i denne variabel.")

    }

  }

  # Tjekker om variablen indeholder mindst et af de angivne værdier
  if(!any(vaerdi %in% unique(datasaet$variablen))){

    stop("Ingen af de valgte værdier, der skal beholdes findes i variablen i datasættet.
         Der kan ikke filtreres med udgangspunkt i disse værdier.")

  }

  datasaet <- datasaet%>%
    dplyr::filter(variablen %in% vaerdi)

  # Ændrer navnet på variablen tilbage til den originale
  colnames(datasaet)[colnames(datasaet) == "variablen"] <- variablen

  return(datasaet)

}


#' Filtrerer datasæt efter periode
#' @encoding UTF-8
#'
#' @param datasaet Datasættet som skal filtreres (dataframe)
#' @param variablen Navnet på variablen som filtrering tager udgangspunkt i (character) - variablen i datasaet skal være af typen posixct
#' @param fra dato og tid (posixct) som angiver starten af perioden (fra og med) som datasættet skal dække med udgangspunkt i den valgte variabel
#' @param til dato og til (posixct) som angiver slutningen af perioden (til og med)
#' @param tilladNAIVariablen TRUE/FALSE variabel der angiver om der må filtreres med udgangspunkt i 'variablen' når denne indeholder NA.
#'     Default er FALSE - Hvis NA findes i 'variablen' melder funktionen fejl.
#'
#' @return datasaet hvor kun de rækker, hvor 'variablen' er indenfor perioden
#'
#' @examples
#' datasaetEksempel <- retOgFormaterDatasaet(datasaettet = datasaetFormateringsfunktioner, datasaetFormatFilen = formatfilFormateringsfunktioner)
#' filtrereDatasaetPeriode(datasaet = datasaetEksempel,
#'                         variablen = "registreringstidspunkt",
#'                         fra = as.POSIXct(strptime("01-01-2018", format = "%d-%m-%Y"), tz = "UTC"),
#'                         til = as.POSIXct(strptime("30-04-2018", format = "%d-%m-%Y"), tz = "UTC"),
#'                         tilladNAIVariablen = TRUE)
filtrereDatasaetPeriode <- function(datasaet, # Datasættet som skal filtreres (dataframe)
                                    variablen, # Navnet på variablen filtrering tager udgangspunkt i (character) - variablen skal være af typen posixct
                                    fra, # dato og tid (posixct) som angiver starten af perioden (fra og med) som datasættet skal dække med udgangspunkt i den valgte variabel
                                    til, # dato og til (posixct) som angiver slutningen af perioden (til og med)
                                    tilladNAIVariablen = FALSE){

  # Tjekker om variablene er angivet korrekt
  if(!("data.frame" %in% class(datasaet)) | class(variablen) != "character" | length(variablen) != 1 | !(class(fra)[1] == "POSIXct" | is.na(fra)) | !(class(til)[1] == "POSIXct" | is.na(til))){

    stop("Mindst et af inputs til filtrereDatasaet funktionen er ikke angivet korrekt.
         'datasaet' skal være en data.frame
         'variablen' skal være af typen character med en enkelt værdi/entry
         'fra' og 'til' skal være af typen POSIXct")

  }

  # Tjekker til at starte med om variablen findes i datasættet
  if(!(variablen %in% colnames(datasaet))){

    stop("Den valgte variabel findes ikke i datasættet.")

  }

  # Da til ikke må være mindre end fra tjekker vi om dette er overholdt
  if(!is.na(fra) & !is.na(til) & til < fra){

    stop("Den valgte 'til' (slutningen af perioden) er mindre end den valgte 'fra' (start af perioden). Der kan ikke filtreres med udgangspunkt i de angivne til og fra værdier.")

  }

  # Ændrer navnet på variablen i datasættet til 'variablen'
  colnames(datasaet)[colnames(datasaet) == variablen] <- "variablen"

  # Tjekker om variablen er af typen 'POSIXct'
  if(class(datasaet$variablen)[1] != "POSIXct"){

    stop("Den valgte variabel er ikke at typen POSIXct. Der kan ikke filtreres med udgangspunkt i denne variabel.")

  }

  if(!tilladNAIVariablen){
    # Tjekker om variablen indeholder NA' values
    if(sum(is.na(datasaet$variablen)) != 0){

      stop("Den valgte variabel indeholder NA'er. Der kan ikke filtreres med udgangspunkt i denne variabel.")

    }

  }

  if(is.na(fra)){fra <- as.POSIXct(strptime("01-01-1900", format = "%d-%m-%Y"), tz = "UTC")}
  if(is.na(til)){til <- as.POSIXct(strptime("01-01-2999", format = "%d-%m-%Y"), tz = "UTC")}

  datasaet <- datasaet%>%
    dplyr::filter(variablen >= fra & variablen <= til)

  # Ændrer navnet på variablen tilbage til den originale
  colnames(datasaet)[colnames(datasaet) == "variablen"] <- variablen

  # Melder fejl hvis ingen data i perioden
  if(nrow(datasaet) == 0){

    stop("Der er ingen data i den valgte periode. Der kan ikke filtreres.")

  }

  return(datasaet)

}
