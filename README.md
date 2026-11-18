# Rémunérations RH – application R Shiny

Ibrahima FALL et Boubacar DIALLO

Application de visualisation interactive pour **suivre et comparer les rémunérations
des salariés** d'une entreprise fictive selon la tranche d'âge, le sexe, l'état civil
et le type de contrat (projet 4 « RH » – cours Applications de visualisation R Shiny, UBS).

**Application en ligne :** *lien shinyapps.io à ajouter après le déploiement*

## Fonctionnalités (consignes du sujet)

| Consigne | Où dans l'application |
|---|---|
| Exclure les salariés sans contrat | `nettoyer_donnees()` : jointure interne (`inner_join`) salariés × contrats |
| Nettoyer les aberrations / incohérences | `nettoyer_donnees()` : doublons, état civil manquant, enfants = 54, durées > 48 h, salaires manquants, salaires atypiques repérés |
| Comparer selon âge, sexe, état civil, contrat | Sélecteur « Comparer les salaires selon » + boîtes à moustaches, barres, tableau |
| Filtrer sur ces mêmes critères | Cases à cocher du panneau latéral (+ temps de travail, nombre d'enfants) |
| Test statistique de différence | Wilcoxon / Kruskal-Wallis (non paramétrique) ou Student / ANOVA de Welch (paramétrique) |

Consignes complémentaires du prof : **packages du tidyverse uniquement** pour le
traitement des données (dplyr, forcats, purrr, readr, readxl…), **thème avec bslib**
(`bs_theme()` personnalisé), **graphiques ggplot2 rendus interactifs avec ggiraph**
(infobulles et surlignage au survol, export PNG). Seuls `shiny`, `bslib` et `ggiraph`
s'ajoutent au tidyverse.

Bonus : croisement de deux critères, salaire équivalent temps plein, exclusion
optionnelle des salaires atypiques, page « Profil des salariés » (distribution des
salaires et répartition selon les critères), page « Données » avec export CSV,
page « À propos » (auteurs, journal du nettoyage, choix méthodologiques).

**Auteurs :** Ibrahima FALL et Boubacar DIALLO – Master 1 Statistique, Data Science et
modélisation statistique, Université Bretagne Sud.

## Structure du projet

```
├── global.R     # exécuté une fois au démarrage : packages, fonctions, données
├── ui.R         # interface (bslib : bs_theme, page_navbar, sidebar, cards)
├── server.R     # logique réactive uniquement
└── data/        # RH_Salaries.xlsx, RH_Contrats.xlsx
```

`global.R` est organisé en cinq parties :

1. packages ;
2. import et nettoyage des données ;
3. filtres, statistiques et tests ;
4. graphiques ggplot2 + ggiraph ;
5. chargement des données au démarrage.

Les parties 2 à 4 définissent les **fonctions « métier »**, écrites et testables en
R classique, sans Shiny ; `server.R` ne fait que relier les inputs à ces fonctions.

## Lancer l'application

```r
install.packages(c("shiny", "bslib", "tidyverse", "ggiraph"))
shiny::runApp()            # depuis le dossier du projet
```

Dans RStudio : ouvrir `ui.R` ou `server.R`, puis cliquer sur **Run App**.

## Publier

1. **GitHub** : créer un dépôt public, puis sur la page du dépôt, cliquer sur
   *uploading an existing file* et y glisser `global.R`, `ui.R`, `server.R`,
   `README.md`, `.gitignore` et le dossier `data/`.
2. **shinyapps.io** : relier RStudio au compte en tapant une seule fois dans la
   console `rsconnect::setAccountInfo(name = ..., token = ..., secret = ...)`
   (identifiants : shinyapps.io > Account > Tokens ; ne jamais les écrire dans
   un fichier du dépôt, qui est public), puis :

   ```r
   rsconnect::deployApp(appName = "remunerations-rh")
   ```

   ou bouton *Publish* de RStudio.

## Nettoyage des données (résumé)

| Étape | Lignes |
|---|---|
| Salariés dans RH_Salaries.xlsx | 254 |
| Doublons exacts supprimés | 2 |
| Salariés sans contrat exclus | 8 |
| Contrats sans salarié ignorés (dont toutes les durées de 52 h) | 40 |
| État civil manquant → « Non renseigné » | 76 |
| Nombre d'enfants aberrant (54) → NA | 1 |
| Salaire manquant (exclus des comparaisons) | 2 |
| Salaires atypiques repérés (Tukey, exclusion au choix) | 6 |
| **Salariés retenus / avec salaire** | **244 / 242** |
