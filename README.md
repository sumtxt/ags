The R package 'ags' provides a set of tools to work with the _Amtlicher Gemeindeschlüssel_ of Germany. The AGS is a structured ID that identifies administrative units in the territory of Germany. The primary function of this package is to allow users to create panels of counties and municipalities over time. 

__ Early development stage __ 

# Creating a Panel

The primary difficulty in creating a panel of counties or municipalities is that these units change over time. This package approach is simple: We define a baseline year (typically the most recent year) and then identify the set of units that used to be part of today's unit. Each of these old units' contribution to the new unit is measured by a weight calculated from the proportion of area that the old unit contributes to the new. 

These calculations are done using the 1:250.000 Verwaltungsgebiete (VG) maps provided by the 
[Geodatenzentrum](http://www.geodatenzentrum.de/geodaten/gdz_rahmen.gdz_div?gdz_spr=deu&gdz_akt_zeile=5&gdz_anz_zeile=0&gdz_user_id=0) since 1997. 