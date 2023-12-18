clear all
set more off

gl wd "C:\Users\Dell\Documents\Primo demandeur"
gl base "$wd\analyses"
gl resultat "$wd\analyses\resultats"

use "$base\primo_demandeur.dta", clear


*==========			Statistiques descriptives 		=========*

preserve
keep if age>=15 & age<=35 & nivinst != 0
save "Primo_demandeur", replace
restore
use "Primo_demandeur", clear


ssc install tabout
svyset [pw=Poids]

tabout primo_demandeur grp_age using "$resultat\age.xls", c(freq) f(0c) clab(Effectif) svy replace 
  
tabout primo_demandeur grp_age using "$resultat\ageP.xls", c(col) f(4c) replace svy

gen duree_chomage = 1*(duree_noemp<=6) + 2*(duree_noemp>6 & age<=12) + 3*(duree_noemp>12 & age<=24) + 4*(duree_noemp>24)             
lab var duree_chomage  "Catégorie de durée de chômage"
cap label define duree_chomage 1 " moins de 6 mois " 2 "6 mois à 1 an" 3 "1 à 2 ans" 4 " Plus de 2 ans" 
label values grp_age_p grp_age_p
*** Distribution des variables qualitatives


putexcel


*lien à primo demandeur
foreach var in sexe sit_matrimoniale milieu nationalite niveau_instruction type_enseignement formation_prof diplome handicap act_parent emploi_parent cat_parent {
  svy: ta primo_demandeur `var', col   
}
*lien des variables entre elles
foreach var1 in sexe grp_age sit_matrimoniale milieu nationalite niveau_instruction type_enseignement formation_prof diplome handicap act_parent emploi_parent cat_parent {
	foreach var2 in sexe grp_age sit_matrimoniale milieu nationalite niveau_instruction type_enseignement formation_prof diplome handicap act_parent emploi_parent cat_parent{
		
	if `var1'!= `var2'{
		capture noisily svy: ta `var1' `var2', col 
	}
   
  }
}

  

*** Durée moyenne
mean duree_emp 
mean duree_noemp
* La durée du chomâge est plus élevée moyennes*

*** lien à durée au chômage
* Test de comparaison des variances
foreach var in sexe grp_age sit_matrimoniale milieu nationalite niveau_instruction type_enseignement formation_prof diplome handicap act_parent emploi_parent cat_parent  { 
	codebook `var' 
	}
*-> 2 modalités
foreach var in sexe type_enseignement formation_prof handicap emploi_parent grp_age C10B C11 C12A {
	display("`var'") 
	capture noisily sdtest duree_noemp, by (`var') 
	}
*-> 3 modalités
foreach var in milieu nationalite C7A	{
	display("`var'") 
forvalues i=1/3{
	forvalues j=1/3{
		if `i'!=`j'{
			capture noisily sdtest duree_noemp if (`var'==`i' | `var'==`j'), by(`var')
		}
	}
}
}
*-> 4 modalités
foreach var in niveau_instruction act_parent C7B	{
	display("`var'") 
forvalues i=1/4{
	forvalues j=1/4{
		if `i'!=`j'{
			capture noisily sdtest duree_noemp if (`var'==`i' | `var'==`j'), by(`var')
		}
	}
}
}
*-> 5 modalités
forvalues i=1/5{
	forvalues j=1/5{
		if `i'!=`j'{
			capture noisily sdtest duree_noemp if (sit_matrimoniale==`i' | sit_matrimoniale==`j'), by(sit_matrimoniale)
		}
	}
}
*-> 6 modalités
forvalues i=1/6{
	forvalues j=1/6{
		if `i'!=`j'{
			capture noisily sdtest duree_noemp if (cat_parent==`i' | cat_parent==`j'), by(cat_parent)
		}
	}
}
*-> 7 modalités
forvalues i=1/7{
	forvalues j=1/7{
		if `i'!=`j'{
			capture noisily sdtest duree_noemp if (diplome==`i' | diplome==`j'), by(diplome)
		}
	}
}

*-> 10 modalités
forvalues i=1/10{
	forvalues j=1/10{
		if `i'!=`j'{
			capture noisily sdtest duree_noemp if (C7D==`i' | C7D==`j'), by(C7D)
		}
	}
}
*-> 12 modalités
forvalues i=1/12{
	forvalues j=1/12{
		if `i'!=`j'{
			capture noisily sdtest duree_noemp if (C5==`i' | C5==`j'), by(C5)
		}
	}
}

*La durée au chômage varie en variance selon : type d'enseignement, strates d'habitation.

* Test de normalité des résidus
sktest duree_noemp
* Les résidus ne sont pas distribuées normalement, on va appliquer un test de kruska-wallis pour comparer les moyennes

*Test de comparaison des moyennes
foreach var in sexe grp_age sit_matrimoniale milieu nationalite niveau_instruction type_enseignement formation_prof diplome handicap act_parent emploi_parent cat_parent C5 C7A C7B C7D C10B C11 C12A{
	display("`var'")
	capture noisily kwallis duree_noemp, by (`var') 
	}

	***Distribution des variables quantitatives
pwcorr duree_noemp C10A