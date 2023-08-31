# EmissionsIndicators

Construction d'indicateurs carbone selon plusieurs définitions et pour un ensemble de pays distinguées dans les données d'entrées. 
Les données d'entrée doivent être sous un format input-output mondial avec distinction de régions (Exiobase, WIOD, EORA...)
Effectue les changements de nomenclatures à partir de ces données d'entrée en distinguant la composante produit/secteur de la composante pays. 


## Comment executer le code 

### Base de données (![`01_exio3.Pre_loader.R`](https://github.com/OFCE/EmissionsIndicators/blob/master/src/01_exio3.Pre_loader.R)) 

Les données doivent être chargées localement dans le dossier `data_in/IOT/[year]`  au format `.txt` et doivent être placées dans un sous dossier appelé `IOT_{year}_{ixi/pxp}` ou `ixi` indique une décomposition secteur par secteur et `pxp` une décomposition par produit. 

Pour la MRIO Exiobase, les données peuvent être chargées depuis le dépôt ![Zenodo](https://zenodo.org/) ![suivant](https://zenodo.org/record/5589597)

Cette étape permet de formater les données pour les enregistrer dans un format `.rds` qui sera nécessaire pour ensuite effectuer les calculs.

### Formater les données (![`02_exio3.loader.R`](https://github.com/OFCE/EmissionsIndicators/blob/master/src/02_exio3.loader.R)) 

Cette étape permet d'aggréger les données selon un bridge qui défini les correspondances entre la nomenclature d'entrée, et celle souhaitée en sortie (plusieurs modèles existent dans le dossier `data_in/bridges`). De la même manière, un bridge entre régions couvertes est possible. 
Le résultat obtenu sont des bases de données distinguées selon la nomenclature de sortie de produits/secteurs et de celle des régions.  

### Bridge et processus d'aggrégation (![`03_IO.analysis.R`](https://github.com/OFCE/EmissionsIndicators/blob/master/src/03_IO.analysis.R)`) 

Cette étape procède à l'analyse input-output sur les données obtenues à partir de l'étape précédente. Les principaux résultats obtenus sont ceux de l'empreinte carbone pour un pays donné et selon les gaz à effet de serre considérés (par défaut les six principaux GES sont distingués, à savoir: CO2, CH4, N2O, HFC, PFC, SF6.

Les résultats sont ensuite sauvé dans un dossier `data_out/IOT_{year}_{ixi/pxp}/{NOM_NOMENCLATURE_SORTIE}/{NOM_BRIDGEREGIONS}` au format `.rds`

### Exportation vers ThreeME (![`04_export.ThreeME.R`](https://github.com/OFCE/EmissionsIndicators/blob/master/src/04_export.ThreeME.R)`) 

Cette étape permet d'exporter les résultats sous un format `.xlsx` lisible par le modèle ![ThreeME](https://www.threeme.org/) pour sa calibration et qui intègre la dimension des émissions importées à un modèle macroéconomique national. Cette version est accessible sur le dépôt Github ![ThreeME-ImportedEmissions]()
