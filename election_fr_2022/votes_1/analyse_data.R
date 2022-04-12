
#### Aim of script: Analyse data first round

rm(list = ls())
options(max.print = 500)

library(data.table)
library(stringi)
library(terra)

#### Load and clean data
## Downloaded the 11th April
data = fread("./resultats-par-niveau-subcom-t1-france-entiere.csv")

## Cleaning the colnames
setnames(data, new = tolower(stri_trans_general(str = colnames(data), id = "Latin-ASCII")))

cols_to_keep = c("code du departement", "libelle du departement", "code de la commune", "libelle de la commune",
	"inscrits", "abstentions", "votants", "blancs", "nuls", "exprimes",
	"nom", "prenom", "voix")

# Pattern = [N°Panneau, Sexe, Nom, Prénom, Voix, % Voix/Ins, % Voix/Exp], [N°Panneau, Sexe, ...], ... 12 times, for the 12 candidates
nCandidates = 12
nCommunes = data[, .N]
colPatterns = character(length = 3*(nCandidates - 1))
col_start = 29

for (i in 1:(nCandidates - 1))
	colPatterns[(3*(i - 1) + 1):(3*i)] = paste0("v", col_start:(col_start + 2) + 7*(i - 1))

cols_to_keep = c(cols_to_keep, colPatterns)

data = data[, ..cols_to_keep]

col_surname = c("nom", colPatterns[seq(1, length(colPatterns), by = 3)])
col_firstname = c("prenom", colPatterns[seq(2, length(colPatterns), by = 3)])
col_votes = c("voix", colPatterns[seq(3, length(colPatterns), by = 3)])

data = data.table::melt(data = data, measure = list(col_surname, col_firstname, col_votes), value.name = c("nom", "prenom", "voix"),asdfsdfknsfokwo
	variable.name = "id_candidate")

if (data[, .N]/nCandidates != nCommunes)
	stop("Check that the melting occurs the way you wanted it!")

setnames(data, old = names(data), new = c("code_dpt", "name_dpt", "code_district", "name_district",
	"registered", "abstention", "voting", "neutral", "invalid", "expressed", "id_candidate", "surname", "firstname", "votes"))

## Load shapefile for coordinates district
france = vect("~/Nextcloud/shapefiles/France_shapefile/FRA_adm/FRA_adm5.shp")

jpeg("toto.jpg", quality = 100, width = 1080, height = 1080)
plot(france)
dev.off()

## Merge coordinates and vote data
