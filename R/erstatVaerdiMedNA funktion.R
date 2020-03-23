##### Funktion til at erstatte en brugerdefineret værdi med NA i et datasæt #####

#' Erstat en brugerdefineret værdi i datasættet med NA i et datasæt
#' @encoding UTF-8
#'
#' @description Funktion til at erstatte en brugerdefineret værdi (character) med NA i alle kolonner med typen 'character' i et datasæt.
#'
#' @param datasaet Det datasæt, hvor værdien skal findes og erstattes med NA
#' @param vaerdi Værdien der skal erstattes med NA (af typen character med længden en)
#'
#' @details Funktionen kan kun bruges på værdier/variabler af typen character.
#'     Funktionen kigger på alle varibler af typen character i datasættet for værdien, hvis vaerdien findes erstattes denne med NA.
#'
#' @return 'datasaet' med NA i stedet for den vaerdi der skulle erstattes
#'
#' @examples datasaetMedNAIstedetForNull <- erstatVaerdiMedNA(datasaetFormateringsfunktioner, "NULL")
erstatVaerdiMedNA<-function(datasaet,
                            vaerdi){

  # Angiver navnet for datasættet
  datasaetNavn <- deparse(substitute(datasaet))

  # Danner kopi af datasættet, som skal bruges til test
  datasaetOrg <- datasaet

  # Finder hvilken type variablene har
  varType <- data.frame(lapply(datasaet,class), stringsAsFactors = FALSE)[1,]

  # Laver en liste over de variable der er character
  characterVariabelListe <- colnames(varType)[varType == "character"]

  if(!is_empty(characterVariabelListe)){

    # Tjekker om der findes NULL'er i character variable
    if(any(datasaet[characterVariabelListe] == vaerdi, na.rm = TRUE)){

      # Erstatter NULL'er med NA
      datasaet <- datasaet %>%
        mutate_if(is.character, list(~na_if(., vaerdi)))

      # Tjekker om der er sket en fejl
      if((sum(datasaetOrg[characterVariabelListe] == vaerdi, na.rm = TRUE) + sum(is.na(datasaetOrg))) != sum(is.na(datasaet))){

        cat("Fejl ved konvertering af NULL'er til NA'er i ", datasaetNavn, ".\n", sep = "")
        stop(paste0("Fejl ved konvertering af NULL'er til NA'er i ", datasaetNavn, "."))

      }

    }

  }

  return(datasaet)

}
