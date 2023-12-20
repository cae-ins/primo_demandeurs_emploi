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


*ssc install tabout
svyset [pw=Poids]

/*tabout primo_demandeur grp_age using "$resultat\age.xls", c(freq) f(0c) clab(Effectif) svy replace 
  
tabout primo_demandeur grp_age using "$resultat\ageP.xls", c(col) f(4c) replace svy*/

*remplacer les outliers par le mode=12: outliers identifiés par la méthodes des IQR pour duree_emp>84

replace duree_emp=12 if (duree_emp>84 & duree_emp<.)
tab duree_emp
*Catégoriser le nombre d'heures au chomage
cap drop duree_chomage
gen duree_chomage = 1*(duree_emp>0 & duree_emp<6) + 2*(duree_emp>=6 & duree_emp<=12) + 3*(duree_emp>12 & duree_emp<=24) + 4*(duree_emp>24 & duree_emp<.)            
lab var duree_chomage  "Catégories de durée de chômage"
cap label define duree_chomage 1 "moins de 6 mois" 2 "6 mois à 1 an" 3 "Plus d'un à 2 ans" 4 "Plus de 2 ans"
label values duree_chomage duree_chomage
tab duree_chomage if duree_chomage>0
* catégoriser la prétention salariale
cap drop salaire
gen salaire = 1*(AP13B>0 & AP13B<60000) + 2*(AP13B>=60000 & AP13B<=150000) + 3*(AP13B>150000 & AP13B<=300000) + 4*(AP13B>300000 & AP13B<.)             
lab var salaire  "Catégories de salaire"
cap drop label salarial
cap label define salarial 1 " Moins de 60 000 " 2 "[60 000 - 150 000]" 3 "]150 000 - 300 000]" 4 "Plus de 300 000"
label values salaire salarial
tab salaire if salaire>0
tabulate salaire duree_chomage if duree_chomage>0 
*recoder variable "diplome"

cap drop degree
gen degree= 1*(diplome==1) + 2*(diplome==2) + 3*(diplome==3) + 4*(diplome>3)
lab var degree "Diplôme"
cap drop label diplom
cap label define diplom 1 " Aucun" 2 "CEP/CFEPCEF/CFEPD" 3 "DEF/BEPC/CAP/BT" 4 "Baccalauréat et plus"
label values degree diplom
tab degree

*** Distribution des variables qualitatives
 
/*lien à primo demandeur
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

  */

*** Durée moyenne
 mean duree_emp
* La durée du chomâge est plus élevée moyennes*

*** lien à durée au chômage
* Test de comparaison des variances
foreach var in sexe grp_age sit_matrimoniale milieu nationalite niveau_instruction type_enseignement diplome handicap act_parent emploi_parent cat_parent salaire AP4 AP8A41 AP8A6 AP8B AP9B typemp_ap   { 
	codebook `var' 
	}
*-> 2 modalités
foreach var in sexe type_enseignement handicap emploi_parent grp_age AP8A41 typemp_ap {
	display("`var'") 
	capture noisily sdtest duree_emp if duree_emp>0, by (`var') 
	}
*-> 3 modalités
foreach var in milieu nationalite	{
	display("`var'") 
forvalues i=1/3{
	forvalues j=1/3{
		if `i'!=`j'{
			capture noisily sdtest duree_emp  if (`var'==`i' | `var'==`j') & duree_emp>0, by(`var')
		}
	}
}
}
*-> 4 modalités
foreach var in niveau_instruction act_parent  degree	{
	display("`var'") 
forvalues i=1/4{
	forvalues j=1/4{
		if `i'!=`j'{
			capture noisily sdtest duree_emp if (`var'==`i' | `var'==`j') & duree_emp>0, by(`var')
		}
	}
}
}
forvalues i=1/4{
	forvalues j=1/4{
		if `i'!=`j'{
			capture noisily sdtest duree_emp if (salaire==`i' | salaire==`j') & duree_emp>0 & salaire>0, by(salaire)
		}
	}
}
*-> 5 modalités
foreach var in sit_matrimoniale	{
	display("`var'")
forvalues i=1/5{
	forvalues j=1/5{
		if `i'!=`j'{
			capture noisily sdtest duree_emp if (`var'==`i' | `var'==`j') & duree_emp>0, by(`var')
		}
	}
}
}
*-> 6 modalités
forvalues i=1/6{
	forvalues j=1/6{
		if `i'!=`j'{
			capture noisily sdtest duree_emp if (cat_parent==`i' | cat_parent==`j') & duree_emp>0, by(cat_parent)
		}
	}
}

*-> 8 modalités
forvalues i=1/8{
	forvalues j=1/8{
		if `i'!=`j'{
			capture noisily sdtest duree_emp if (AP4==`i' | AP4==`j') & duree_emp>0, by(AP4)
		}
	}
}
*-> 9 modalités
forvalues i=1/9{
	forvalues j=1/9{
		if `i'!=`j'{
			capture noisily sdtest duree_emp if (AP8B==`i' | AP8B==`j') & duree_emp>0, by(AP8B)
		}
	}
}

*La durée au chômage varie en variance selon : type d'enseignement, strates d'habitation et à la tranche d'âge.

* Test de normalité des résidus
sktest duree_emp if duree_emp>0
* Les résidus ne sont pas distribuées normalement, on va appliquer un test de kruska-wallis pour comparer les moyennes

*Test de comparaison des moyennes
foreach var in sexe grp_age sit_matrimoniale milieu nationalite niveau_instruction type_enseignement degree handicap act_parent emploi_parent cat_parent AP4 AP8A41 AP8A6 AP8B  typemp_ap {
	display("`var'")
	capture noisily kwallis duree_emp if duree_emp>0, by (`var') 
	}
display("salaire")
capture noisily kwallis duree_emp if (duree_emp>0 & salaire>0), by (salaire) 	
*Test de khi-deux
foreach var in sexe grp_age sit_matrimoniale milieu nationalite niveau_instruction type_enseignement degree handicap act_parent emploi_parent cat_parent AP4 AP8A41 AP8A6 AP8B  typemp_ap{
	display("`var'")
	tabulate `var' duree_chomage if duree_chomage>0, chi2
	}	
display("salaire")
tabulate salaire duree_chomage if (duree_chomage>0 & salaire>0), chi2
*La durée de chômage en variable qualitative est liée à : nationalité, niveau d'instruction, plus haut diplôme obtenu, 	
	
	
*La durée au chômage varie en moyenne selon : nationalité, activité des parents,la prétention 
	
*Calcul des moyennes
foreach var in sexe grp_age sit_matrimoniale milieu nationalite niveau_instruction type_enseignement degree handicap act_parent emploi_parent cat_parent AP4 AP8A41 AP8A6 AP8B  typemp_ap {
	display("`var'")
	mean duree_emp if duree_emp>0, over(`var') 
	}
display("salaire")
mean duree_emp if (duree_emp>0 & salaire>0), over(`var')
