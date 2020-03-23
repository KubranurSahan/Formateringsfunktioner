### Funktion til at rette origin hvis excel automatisk har formateret den.###
#' Ret origin i formatfil
#' @encoding UTF-8
#'
#' @description Funktion til at rette origin i formatfilen, hvis denne står forkert.
#'
#' @param formatFilen er et datasæt, der bruges som input til at formetere et datasæt.\cr
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
#' @details Funktionen er også en del af funktionen "retOgFormaterDatasaet".
#'
#' @return formatFilen, hvor origin er rettet til så den har den rigtige form 'yyyy-mm-dd'
#'
#' @seealso \code{\link{retOgFormaterDatasaet}} og \code{\link{formatfilFormateringsfunktioner}}
#'
#' @examples
#' formatfilFormateringsfunktioner[2,]
#' formatFilMedRettetOrigin <- retOriginIFormatFil(formatfilFormateringsfunktioner)
#' formatFilMedRettetOrigin[2,]
retOriginIFormatFil <- function(formatFilen){

  # Definere navnet på formatfilen
  formatFilNavn <- deparse(substitute(formatFilen))

  # Finder antallet af origin
  antalOrigin <- sum(!is.na(formatFilen$origin))

  # Starter med at se nærmere på de udfyldte origins medmindre der ikke er udfyldt origin i variablen
  if(antalOrigin != 0){

    # Ser på om der findes origin med manglende oplysninger ved at se på om antallet og typen af tegn brugt passer (alt andet end nummer, kolon, mellemrum og bindestreg)
    if(any(!(nchar(formatFilen$origin[!is.na(formatFilen$origin)]) %in% c(10,16,19))) & any(grepl(pattern = "[^0-9: -]", formatFilen$origin[!is.na(formatFilen$origin)]))){

      # Melder fejl hvis der findes origin med færre end 10/16/19 eller andet tegn end tal,":"," " og "-".
      stop(paste0("Fejl: Længden eller tegntypen af en eller flere origin input passer ikke. Ret ", formatFilNavn, "."))

    } else {

      # Tjekker om der findes origin, hvor tegnene ikke står rigtigt
      # Første del af mønster dvs. "^[0-9]{4}\\b[-]\\b[0-9]{2}\\b[-]\\b[0-9]{2}$" ser om format er "yyyy-mm-dd"
      # Anden del efter færste "|", dvs. "^[0-9]{4}\\b[-]\\b[0-9]{2}\\b[-]\\b[0-9]{2}\\b[ ]\\b[0-9]{2}\\b[:]\\b[0-9]{2}$" ser på om format er "yyyy-mm-dd hh:mm"
      # Sidste del efter "|" ser på om format er "yyyy-mm-dd hh:mm:ss"
      moenster <- "^[0-9]{4}\\b[-]\\b[0-9]{2}\\b[-]\\b[0-9]{2}$|^[0-9]{4}\\b[-]\\b[0-9]{2}\\b[-]\\b[0-9]{2}\\b[ ]\\b[0-9]{2}\\b[:]\\b[0-9]{2}$|^[0-9]{4}\\b[-]\\b[0-9]{2}\\b[-]\\b[0-9]{2}\\b[ ]\\b[0-9]{2}\\b[:]\\b[0-9]{2}\\b[:]\\b[0-9]{2}$"
      if(any(!grepl(moenster, formatFilen$origin[!is.na(formatFilen$origin)], perl = TRUE))){

        # Gennemgår hvert enkelt origin for at finde dem som ikke står i den rigtige format og retter det til
        for(variabelNummer in 1:nrow(formatFilen)){

          # Da Excel formatere origin fra "yyyy-mm-dd" til "dd-mm-yyyy", tjekker jeg kun for denne type fejl (resten vil før til et NA value og funktionen vil melde fejl lidt længere nede)
          if(!is.na(formatFilen$origin[variabelNummer]) & substring(formatFilen$origin[variabelNummer],3,3) == "-"){

            # Hvis variablen,som origin tilhører, skal konverteres fra dato og tid
            if(formatFilen$type[variabelNummer] == "POSIXct"){

              # Ser på antallet af kolon for at tjekke om sekunder er med (ikke solid men nem løsning)
              if(length(gregexpr(":",formatFilen$origin[variabelNummer],fixed = TRUE)[[1]])==1){

                # Formatere variablen fra "dd-mm-yyyy tt:mm"
                formatFilen$origin[variabelNummer] <- as.character(as.POSIXct(strptime(formatFilen$origin[variabelNummer], format="%d-%m-%Y %H:%M"), tz="UTC"))

              } else {

                # Formatere variablen fra "dd-mm-yyyy tt:mm:ss"
                formatFilen$origin[variabelNummer] <- as.character(as.POSIXct(strptime(formatFilen$origin[variabelNummer], format="%d-%m-%Y %H:%M:%S"), tz="UTC"))

              }

            } else {

              # Formatere variablen fra "dd-mm-yyyy"
              formatFilen$origin[variabelNummer] <- as.character(as.Date(formatFilen$origin[variabelNummer], format="%d-%m-%Y"))

            }

          }

        }

        # Tjekker om antallet af origin passer med den initiale antal - hvis ikke må der være sket fejl under konvertering
        if(sum(!is.na(formatFilen$origin)) != antalOrigin){

          stop(paste0("Fejl ved konvertering af origin i ", formatFilNavn, "."))

        }

      }

    }

  }

  # Returnere formatfilen
  return(formatFilen)

}



### Funktion til at rette og formatere et datasæt med udgangspunkt i en tilhørende formatfil ###
# Funktionen sortere i variable, erstatter NULL'er med NA'er og konvertere variable til den type iflg. formatfilen fra den type de har.
# Bemærk at funktionen "formaterDatasaet" er en del af denne funktion og derfor bør være indlæst for at denne funktion virker.
#' Ret og formatere datasæt
#' @encoding UTF-8
#'
#' @param datasaettet et datasæt der ønskes rettet og formateret med udgangspunkt i et formatfil.
#' @param datasaetFormatFilen formatfilen som fortæller, hvad og hvordan datasættet skal formateres.
#'     formatFilen er et datasæt, der bruges som input til at formetere et datasæt.\cr
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
#' @return et formateret datasaettet med de variable der er med i formatfilen og i den format variablen ønskes i iflg. formatfilen
#'
#' @examples
#' datasaetEksempel <- retOgFormaterDatasaet(datasaettet = datasaetFormateringsfunktioner, datasaetFormatFilen = formatfilFormateringsfunktioner)
retOgFormaterDatasaet <- function(datasaettet, datasaetFormatFilen){

  # Starter med at rette origin i formatfilen
  datasaetFormatFilen <- retOriginIFormatFil(datasaetFormatFilen)

  # Definere navnet på datasættet
  datasaettetNavn <- deparse(substitute(datasaettet))

  # Beholder kun de variable der er med i formatdatasættet
  datasaettet <- datasaettet %>%
    select(datasaetFormatFilen$variabelNavn)

  # Tjekker om alle kolonner er med og ligger i samme rækkefølge
  if(ncol(datasaettet) != nrow(datasaetFormatFilen) | any(colnames(datasaettet) != datasaetFormatFilen$variabelNavn)){

    stop(paste0("Noget gik galt ved frasortering af variablene i ",datasaettetNavn,"."))

  }

  # Ændrer factor type variable til character
  datasaettet <- datasaettet %>%
    mutate_if(is.factor, list(~as.character(.)))

  # Danner kopi af datasættet, som skal bruges til test
  datasaettetOrg <- datasaettet

  # Finder hvilken type variablene skal oversættes fra
  datTypeFra <- data.frame(lapply(datasaettet,class), stringsAsFactors = FALSE)

  # Konveretere datTypeFra til longformat med udgangspunkt i første række af variabeltyper
  datTypeFraA <- datTypeFra[1,] %>%
    melt(id.vars = NULL, na.rm = TRUE, value.name = "fraType", variable.name = "variabelNavn")%>%
    mutate(variabelNavn = as.character(variabelNavn))

  # Kobler typen som variablen skal konverteres fra til formatfilen
  datasaetFormatFilenA <- datasaetFormatFilen %>%
    left_join(datTypeFraA, by="variabelNavn")

  # Laver en liste over de variable der er character
  characterVariabelListe <- as.character(datTypeFraA[datTypeFraA$fraType == "character",]$variabelNavn)

  # Tjekker om der findes NULL'er i character variable
  if(any(datasaettet[characterVariabelListe] == "NULL", na.rm=TRUE)){

    # Erstatter NULL'er med NA
    datasaettet <- datasaettet%>%
      mutate_if(is.character, list(~na_if(.,"NULL")))

    # Tjekker om der er sket en fejl
    if((sum(datasaettetOrg[characterVariabelListe] == "NULL", na.rm = TRUE) + sum(is.na(datasaettetOrg))) != sum(is.na(datasaettet))){

      stop(paste0("Fejl ved konvertering af NULL'er til NA'er i ", datasaettetNavn, "."))

    }

  }

  # Ændrer navnene på kolonnerne i datasaettet til det de skal hedde
  names(datasaettet) <- datasaetFormatFilenA$nytVarNavn

  # Formaterer datasættet, så variablene står i de rigtige formatter
  datasaetNy <- formaterDatasaet(datasaet = datasaettet, datasaetFormat = datasaetFormatFilenA, datasaetNavn = datasaettetNavn)

  return(datasaetNy)

}



### Funktion til at formatere et datasæt med udgangspunkt i en tilhørende formatfil ###
# Funktionen konveretere variabeltypen for alle variable i en datasæt fra den type den har til den type der ønskes at den skal have iflg. formatfilen
# Funktionen er også en del af funktionen "retOgFormaterDatasaet"
#' Formatere datasæt
#' @encoding UTF-8
#'
#' @param datasaet et datasæt der ønskes rettet og formateret med udgangspunkt i et formatfil.
#' @param datasaetFormat formatfilen som fortæller, hvad og hvordan datasættet skal formateres.
#'     formatFilen er et datasæt, der bruges som input til at formetere et datasæt.\cr
#'     Hver række i formatFilen svarer til en variabel, man ønsker at beholde og formatere i datasættet.\cr
#'     formatFilen består af følgende fem variable:\cr
#'     \itemize{
#'     \item 'variabelNavn' navnet på variablen man ønsker at formatere og beholde i datasættet
#'     \item 'type' den ønskede variabeltype for variablen
#'     \item 'datoFormat' datoformatet som variablen skal konverteres fra (eks. \%d-\%m-\%Y eller '\%d-\%m-\%Y \%H:\%M')
#'     \item 'nytVarNavn' navnet på variablen fremover
#'     \item 'origin' origin for dato (dato og tid) variablen, når denne er numerisk. Default for SQL er '1960-01-01' og '1970-01-01' for R.
#'     }
#' @param datasaetNavn navnet for datasættet der formateres. Hvis ikke angivet vil objektnavnet for datasættet tages i brug.
#'
#' @details Hvis man ønsker at formatere et datasæt med udgangspunkt i en formatfil så bør man bruge retOgFormaterDatasaet-funktionen,
#'     da retOgFormaterDatasaet laver tjeks og gør datasættet klar til at 'formaterDatasaet' kan køres prolemfrit på datasaettet.
#'
#' @return et formateret datasaettet med de variable der er med i formatfilen og i den format variablen ønskes i iflg. formatfilen
#'
#'
#' @seealso \code{\link{retOgFormaterDatasaet}}
formaterDatasaet <- function(datasaet, datasaetFormat, datasaetNavn = NA){

  # Definere navnet på datasættet, hvis denne ikke er angivet
  if(is.na(datasaetNavn)){

    datasaetNavn <- deparse(substitute(datasaet))

  }

  # Danner en 'kopi' af datasættet til sammenligning
  datasaetIndlende <- datasaet

  # Ændrer typen af datasaet for at koden skal virke som den skal
  datasaet <- data.frame(datasaet, stringsAsFactors = FALSE)

  # Danner en variabel, der skal samle på eventuelle fejl undervejs og melde fejl ved evetuelle fejl til sidst i koden
  fejlIndsamlet<-character(0)

  # Starter med at ændre variabletypen i hver kolonne
  for(i in 1:ncol(datasaet)){

    ### Til Character
    if(datasaetFormat$type[i] == "character"){

      # Tjekker om kolonnen i forvejen er af typen character
      if(datasaetFormat$fraType[i] == "character"){

        # Hvis variablen i forvejen er er typen character trimmer jeg for overflødige mellemrum
        datasaet[,i] <- as.character(trimws(datasaet[,i]))

      } else {

        # Konvertere kolonnen i tilfælde af at det ikke er at typen character i forvejen
        datasaet[,i] <- trimws(as.character(datasaet[,i]))# i tilfælde af factor kan vi have brug for trimws

      }

      # Tjekker om antallet af NA'er og NULL'er matcher hinanden
      if(sum(is.na(datasaetIndlende[,i])) != sum(is.na(datasaet[,i]))){

        fejlIndsamlet<-c(fejlIndsamlet, paste0("Problemer med omkonvertering af variabel ", datasaetNavn, "$", names(datasaet[i]), " til character."))

      }

    }

    ### Til Numeric
    if(datasaetFormat$type[i] == "numeric"){

      # Gør ikke noget hvis variablen i forvejen er numeric/integer
      if(datasaetFormat$fraType[i] == "numeric" | datasaetFormat$fraType[i] == "integer"){

      } else if(datasaetFormat$fraType[i] == "character" | datasaetFormat$fraType[i] == "factor"){

        if(datasaetFormat$fraType[i] == "factor"){

          # Konvertere variablen først til character for at tallet og ikke level på factoren skal med
          datasaet[,i] <- as.character(datasaet[,i])

        }

        # Tjekker om antlalet af NA'er og NUll'er vil matche når det variablen konverteres til numeric (det vil være tegn på '.' og ',' forskelle)
        if(sum(is.na(datasaetIndlende[,i]) | datasaetIndlende[,i] == "Null" | datasaetIndlende[,i] == "NULL") != sum(is.na(as.numeric(datasaet[,i])))){

          # Ændrer kommaer til punktummer
          datasaet[,i] <- gsub(",", ".", gsub(".", "", datasaet[,i], fixed = TRUE), fixed = TRUE)

        }

        # Konvertere variablen til numeric
        datasaet[,i] <- as.numeric(datasaet[,i])

      } else {

        # Melder fejl hvis variablen skal konverteres til numeric fra en anden type end numeric/integer/character
        fejlIndsamlet<-c(fejlIndsamlet, paste0("Problemer med omkonvertering af variabel ", datasaetNavn, "$", names(datasaet[i]), " som er en ", datasaetFormat$fraType[i], "."))

      }

      if(sum(is.na(datasaetIndlende[,i])) != sum(is.na(datasaet[,i]))){

        fejlIndsamlet<-c(fejlIndsamlet, paste0("Problemer med omkonvertering af variabel ", datasaetNavn, "$", names(datasaet[i]), " til numeric."))

      }

    }

    ### Til Date
    if(datasaetFormat$type[i] == "Date" | datasaetFormat$type[i] == "date"){

      # Gør ingenting hvis variablen i forvejen står som Date
      if(datasaetFormat$fraType[i] == "Date"){

        # Fra numeric
      } else if(datasaetFormat$fraType[i] == "numeric" | datasaetFormat$fraType[i] == "integer"){

        if(is.na(datasaetFormat$origin[i])){

          fejlIndsamlet<-c(fejlIndsamlet, paste0("Origin for dato variablen ", datasaetNavn, "$", names(datasaet[i]), " er ikke angivet i formatfilen. Variablen kan ikke konverteres til dato fra numeric/numeric-character."))

        }

        # Konvertere fra numeric til Date med baseret på origin fra formatfilen
        datasaet[,i] <- as.Date(datasaet[,i], origin = datasaetFormat$origin[i], tz = "UTC")

        # Fra character/factor
      } else if(datasaetFormat$fraType[i] == "character" | datasaetFormat$fraType[i] == "factor"){

        if(datasaetFormat$fraType[i] == "factor"){

          # Konvertere variablen først til character for at level på factoren ikke skal forstyrre
          datasaet[,i] <- as.character(datasaet[,i])

        }

        # Tjekker om der er andre tegn end numeriske i variablen
        if(any(grepl("\\D", datasaet[,i]))){

          if(is.na(datasaetFormat$datoFormat[i])){

            fejlIndsamlet<-c(fejlIndsamlet, paste0("Dato formatet for variablen ", datasaetNavn, "$", names(datasaet[i]), " er ikke angivet i formatfilen. Variablen kan ikke konverteres til dato fra character."))

          }

          # Konvertere variablen med udgangspunkt i datoFormat fra formatfilen
          datasaet[,i] <- as.Date(datasaet[,i], format = datasaetFormat$datoFormat[i])

        } else {

          if(is.na(datasaetFormat$origin[i])){

            fejlIndsamlet<-c(fejlIndsamlet, paste0("Origin for dato variablen ", datasaetNavn, "$", names(datasaet[i]), " er ikke angivet i formatfilen. Variablen kan ikke konverteres til dato fra numeric/numeric-character."))

          }

          # Hvis dato består kun af numeriske værdier så konveretes variablen med udgangspunkt i origin fra formatfilen
          datasaet[,i] <- as.Date(as.numeric(datasaet[,i]), origin = datasaetFormat$origin[i], tz = "UTC")

        }

        # Fra anden type
      } else {

        # Melder fejl hvis variablen skal konverteres til Date fra en anden type end Date/numeric/integer/character
        fejlIndsamlet<-c(fejlIndsamlet, paste0("Problemer med omkonvertering af variabel ", datasaetNavn, "$", names(datasaet[i]), ".\n",
                                               "Variablen ", datasaetNavn, "$", names(datasaet[i]), " skal konvereteres til Date fra ", datasaetFormat$fraType[i], ".\n"))

      }

      if(sum(is.na(datasaetIndlende[,i])) != sum(is.na(datasaet[,i]))){

        fejlIndsamlet<-c(fejlIndsamlet, paste0("Problemer med omkonvertering af variabel ", datasaetNavn, "$", names(datasaet[i]), " til Date."))

      }

    }

    ### Til POSIXct
    if(datasaetFormat$type[i] == "POSIXct"){

      # Gør ingenting hvis variablen i forvejen står som POSIXct
      if(datasaetFormat$fraType[i] == "POSIXct"){

        # Fra numeric
      } else if(datasaetFormat$fraType[i] == "numeric" | datasaetFormat$fraType[i] == "integer"){

        if(is.na(datasaetFormat$origin[i])){

          fejlIndsamlet<-c(fejlIndsamlet, paste0("Origin for dato og tids variablen ", datasaetNavn, "$", names(datasaet[i]), " er ikke angivet i formatfilen. Variablen kan ikke konverteres til POSIXct fra numeric/numeric-character."))

        }

        # Konvertere fra numeric til POSIXct med baseret på origin fra formatfilen
        datasaet[,i] <- as.POSIXct(datasaet[,i], origin = datasaetFormat$origin[i], tz = "UTC")

      } else if(datasaetFormat$fraType[i] == "character" | datasaetFormat$fraType[i] == "factor"){

        if(datasaetFormat$fraType[i] == "factor"){

          # Konvertere variablen først til character for atlevel på factoren ikke skal forstyrre
          datasaet[,i] <- as.character(datasaet[,i])

        }

        # Tjekker om det er andre tegn end numeriske i variablen
        if(any(grepl("\\D", datasaet[,i]))){

          if(is.na(datasaetFormat$datoFormat[i])){

            fejlIndsamlet<-c(fejlIndsamlet, paste0("Dato formattet for variablen ", datasaetNavn, "$", names(datasaet[i]), " er ikke angivet i formatfilen. Variablen kan ikke konverteres til POSIXct fra character."))

          }

          # Konvertere variablen med udgangspunkt i datoFormat fra formatfilen
          datasaet[,i] <- as.POSIXct(strptime(datasaet[,i], format = datasaetFormat$datoFormat[i]), tz = "UTC")

        } else {

          if(is.na(datasaetFormat$origin[i])){

            fejlIndsamlet<-c(fejlIndsamlet, paste0("Origin for dato og tids variablen ", datasaetNavn, "$", names(datasaet[i]), " er ikke angivet i formatfilen. Variablen kan ikke konverteres til POSIXct fra numeric/numeric-character."))

          }

          # Hvis datotid består kun af numeriske værdier så konveretes variablen med udgangspunkt i origin fra formatfilen
          datasaet[,i] <- as.POSIXct(as.numeric(datasaet[,i]), origin = datasaetFormat$origin[i], tz = "UTC")

        }



        # Fra anden type
      } else {

        # Melder fejl hvis variablen skal konverteres til Date fra en anden type end Date/numeric/integer/character
        fejlIndsamlet<-c(fejlIndsamlet, paste0("Problemer med omkonvertering af variabel ", datasaetNavn, "$", names(datasaet[i]), " til POSIXct."))

      }

      if(sum(is.na(datasaetIndlende[,i])) != sum(is.na(datasaet[,i]))){

        fejlIndsamlet<-c(fejlIndsamlet, paste0("Problemer med omkonvertering af variabel ", datasaetNavn, "$", names(datasaet[i]), " til POSIXct."))

      }

    }

  }

  if(length(fejlIndsamlet) != 0){

    stop("\n", paste0(rep(1:length(fejlIndsamlet)), ": ", fejlIndsamlet, collapse = "\n"))

  }

  return(datasaet)

}



### Funktion til at opsætte en formatfil for en datasæt ###
# Bruges til at gemme informationer om datasæt inden de bliver gemt i databasen
opsaetFormatFil <- function(datasaet){

  # Finder hvilken type variablene er
  datasaetFormatFilA <- data.frame(lapply(datasaet, class), stringsAsFactors = FALSE)

  # Konveretere datasaetFormatFilA til longformat med udgangspunkt i første række af variabeltyper
  datasaetFormatFil <- datasaetFormatFilA[1,] %>%
    melt(id.vars = NULL, na.rm = TRUE, value.name = "type", variable.name = "variabelNavn") %>%
    mutate(datoFormat = ifelse(type == "Date",
                               "%Y-%m-%d",
                               ifelse(type == "POSIXct",
                                      "%Y-%m-%d %H:%M:%OS",
                                      NA)),
           nytVarNavn = variabelNavn,
           origin = NA) %>%
    mutate_if(is.factor, list(~as.character(.)))

  return(datasaetFormatFil)

}
