The package 'ags' provides a set of tools to work with the _Amtlicher Gemeindeschlüssel_ of Germany. The AGS is a structured ID that identifies administrative units in the territory of Germany. The primary function of this package is to allow users to create panels of counties and muncipalities over time. 

# Creating a Panel

The primary difficulty in creating a panel of counties or munciaplities is that these units change over time. Typically, but not exclusively, units are merged into larger units. These _Gebietsreformen_ are systematically listed by the Federal Statistics office. However, these lists are not sufficient since they do not contain information about how mergers or splits are done. 

This package approach is simple: We define a baseline year (typically the most recent year) and then for each county we identify the set of counties or mauncipalities that used to be part of today's county. Each of these old counties comes with a weight that is calculated by the share of the area from the old county contained in today's county. 

These calculations are done using the 1:250.000 Verwaltungsgebiete (VG) maps provided by the 
[Geodatenzentrum](http://www.geodatenzentrum.de/geodaten/gdz_rahmen.gdz_div?gdz_spr=deu&gdz_akt_zeile=5&gdz_anz_zeile=0&gdz_user_id=0) since 1997. 

An example clarifies this logic. Suppose 

id_base | id | weight 
--------------------------
		 | 	 |

id15 is the baseline panel of IDs existing in 2015. For each of these IDs there are potentially many observation in any other year before 2015. The weight 
of these observations can be determined either by the number of people living in the merging areas or the size of the mergining areas. 

Merging case: 

id15 | id | year | weight | value1
-----------------------------------
1	 | 1a | 2014 | 	1	  | 
1	 | 1b | 2014 | 	1 	  |

Splitting case: 

id15 | id | year | weight | value1
-----------------------------------
1a	 | 1 | 2014 | 	0.x	  | 
1b	 | 1 | 2014 | 	1-0.x |

What do we do if the changes are not made on the 31.12. but say 6.6. of a year? 