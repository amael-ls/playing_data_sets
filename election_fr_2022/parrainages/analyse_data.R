
#### Aim of script: to have some fun with the elections when elections are not fun...
# The parrainages data where downloaded the third March 2022 at the following url:
# https://www.data.gouv.fr/fr/datasets/r/a6f0a65c-720c-4297-869e-cb8cfbc10ca9
#

#### Load packages and clear memory
rm(list = ls())
graphics.off()

library(data.table)
library(stringi)
library(terra)

options(max.print = 500)

#### Load data
## Parrainages https://presidentielle2022.conseil-constitutionnel.fr/les-parrainages/tous-les-parrainages-valides.html
parrainages = fread("./parrainagestotal.csv")

## Load shapefile france
france_dpt = vect("/Users/mistral/Nextcloud/shapefiles/France_shapefile/FRA_adm/FRA_adm2.shp")
france_reg = vect("/Users/mistral/Nextcloud/shapefiles/France_shapefile/FRA_adm/FRA_adm1.shp")

france_reg = simplifyGeom(france_reg, tolerance = 0.01)
france_dpt = simplifyGeom(france_dpt, tolerance = 0.01)

#### Clean data
## Change the names of columns
# Remove accents
setnames(parrainages, new = stri_trans_general(str = names(parrainages), id = "Latin-ASCII"))

# Remove capital letters
setnames(parrainages, new = tolower(names(parrainages)))

# Replace spaces in names by underscores
setnames(parrainages, new = stri_replace_all(str = names(parrainages), replacement = "_", regex = " "))

## List of running candidates
threshold = 500
ls_candidates = parrainages[, .N, by = candidat]
setkey(ls_candidates, candidat)
running_candidates = ls_candidates[N >= threshold, candidat]
ls_candidates = ls_candidates[N >= threshold]
setorder(ls_candidates, candidat)
n_candidates = ls_candidates[, .N]

## Colours of candidates. Those colours were selected from:
# https://www.lemonde.fr/les-decodeurs/article/2022/02/22/presidentielle-2022-le-tableau-de-bord-des-parrainages-sondages-et-temps-de-parole_6114816_4355770.html
ls_candidates[, col := c("#881E19", "#00258D", "#F0657F", "#47AB42", "#5A6F6A", "#7E5728",
	"#F5992C", "#EE4031", "#BB3328", "#2E83BA", "#DF2B20", "#573D1C")]

## Remove overseas territories (apologise, but it is easier for plotting the data on a map...)
ls_dpt_raster = unique(france_dpt$NAME_2)
ls_dpt_data = parrainages[, unique(departement)]

any(!(ls_dpt_raster %in% ls_dpt_data))
any(!(ls_dpt_data %in% ls_dpt_raster))

discarded = ls_dpt_data[!(ls_dpt_data %in% ls_dpt_raster)]
cat(paste0("Discarded places:\n - ", paste0(discarded, collapse = "\n - ")))

parrainages_metrop = parrainages[!(departement %in% discarded)]
parrainages_metrop = parrainages_metrop[candidat %in% running_candidates]

#### Compute some values of interest
## Dynamics of the parrainages (time series only for running candidates)
dynamics = parrainages[, .N, by = .(candidat, date_de_publication)]
dynamics[, date_de_publication := as.Date(date_de_publication, format = "%d/%m/%Y")]

time_series = vector(mode = "list", length = n_candidates)
names(time_series) = ls_candidates[, candidat]

max_cumulative = 0

for (person in ls_candidates[, candidat])
{
	ls_dates = dynamics[candidat == person, date_de_publication]
	n_dates = length(ls_dates)
	time_series[[person]] = data.table(date_de_publication = ls_dates, cumulative = integer(n_dates))
	count = 0
	for (i in 1:n_dates)
	{
		count = count + dynamics[candidat == person & date_de_publication == ls_dates[i], N]
		time_series[[person]][i, cumulative := count]
	}
	if (count > max_cumulative)
		max_cumulative = count
}

## Dynamics
x_min = as.Date(min(dynamics[, date_de_publication]), format = "%d/%m/%Y")
x_max = as.Date(max(dynamics[, date_de_publication]), format = "%d/%m/%Y")

pdf("dynamics.pdf")
plot(time_series[[ls_candidates[1, candidat]]][, date_de_publication], time_series[[ls_candidates[1, candidat]]][, cumulative],
	col = ls_candidates[1, col], xlab = "Day", ylab = "Cumulative total", type = "l", lwd = 3,
	xlim = c(x_min, x_max), ylim = c(0, max_cumulative))

points(time_series[[ls_candidates[1, candidat]]][, date_de_publication], time_series[[ls_candidates[1, candidat]]][, cumulative], pch = 19,
	 col = ls_candidates[1, col])

for (i in 2:n_candidates)
{
	lines(time_series[[ls_candidates[i, candidat]]][, date_de_publication], time_series[[ls_candidates[i, candidat]]][, cumulative],
		col = ls_candidates[i, col], lwd = 3)

	points(time_series[[ls_candidates[i, candidat]]][, date_de_publication], time_series[[ls_candidates[i, candidat]]][, cumulative], pch = 19,
		col = ls_candidates[i, col])
}
abline(h = 500, lty = "dashed")
legend(x = "topleft", legend = ls_candidates[, candidat], fill = ls_candidates[, col], box.lwd = 0)
dev.off()

## Map for candidates
names(france_dpt)[names(france_dpt) == "NAME_2"] = "departement"
centro = centroids(france_dpt)
coords = setDT(geom(centro, df = TRUE)[, c("x", "y")])
coords[, departement := centro$departement]

parrainages_metrop = parrainages_metrop[coords, on = "departement"]
parrainages_metrop[, totPerDpt := .N, by = .(candidat, departement)]

parrainages_metrop_rs = vect(parrainages_metrop, geom=c("x", "y"), crs = crs(france_dpt))

departement_success = unique(parrainages_metrop[, .(candidat, departement, totPerDpt)])
departement_success[, totPerDpt_prop := totPerDpt/max(totPerDpt), by = candidat]
departement_success[, col := colorspace::adjust_transparency(ls_candidates[candidat, col], alpha = totPerDpt_prop), by = candidat]
setkey(departement_success, candidat)

# Plot maps for each candidate
for (i in 1:n_candidates)
{
	pdf(paste0(ls_candidates[i, candidat], ".pdf"))
	coords_candidate = parrainages_metrop_rs[parrainages_metrop_rs$candidat == ls_candidates[i, candidat], c("x", "y")]
	plot(france_dpt, axes = FALSE, main = ls_candidates[i, candidat])
	polys(france_dpt[coords_candidate], col = departement_success[ls_candidates[i, candidat], col])
	dev.off()
	print(paste(ls_candidates[i, candidat], "done"))
}
