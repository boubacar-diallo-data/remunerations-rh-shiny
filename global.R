# =============================================================================
# global.R
# -----------------------------------------------------------------------------
# Script exécuté UNE SEULE FOIS au démarrage de l'application.
# Tout ce qui est défini ici est visible depuis ui.R et server.R.
#
# Le fichier est organisé en 5 parties :
#   1. Packages
#   2. Import et nettoyage des données        (fonctions)
#   3. Filtres, statistiques et tests          (fonctions)
#   4. Graphiques ggplot2 + ggiraph            (fonctions)
#   5. Chargement des données au démarrage     (exécution des fonctions)
#
# Les parties 2 à 4 ne font que DÉFINIR des fonctions "métier" (non
# réactives) : elles peuvent être testées dans la console, sans Shiny.
# La partie 5 les utilise pour préparer la table de l'application.
# server.R se contente ensuite de relier les inputs à ces fonctions.
#
# Projet R Shiny - Données RH
# Ibrahima FALL et Boubacar DIALLO - Master 1, Université Bretagne Sud
# =============================================================================


# #############################################################################
# 1. PACKAGES
#    Consigne : manipulation des données uniquement avec le tidyverse,
#    thème avec bslib, graphiques ggplot2 rendus interactifs avec ggiraph
# #############################################################################

library(shiny)
library(bslib)
library(tidyverse)   # dplyr, ggplot2, forcats, purrr, readr, stringr, tibble...
library(ggiraph)


# #############################################################################
# 2. IMPORT ET NETTOYAGE DES DONNÉES
#    Fonctions appelées une seule fois, dans la partie 5.
# #############################################################################

# Constantes de nettoyage ------------------------------------------------------

DUREE_TEMPS_PLEIN <- 35   # durée hebdomadaire légale (France)
DUREE_HEBDO_MAX   <- 48   # durée maximale légale : au-delà = valeur aberrante
ENFANTS_MAX       <- 15   # au-delà, le nombre d'enfants est jugé aberrant

# Tranches d'âge : âges observés entre 22 et 59 ans -> 4 tranches de 10 ans
# d'effectifs suffisants pour comparer (et pour les tests statistiques)
BORNES_AGE  <- c(-Inf, 30, 40, 50, Inf)
LIBELLES_AGE <- c("Moins de 30 ans", "30-39 ans", "40-49 ans", "50 ans et plus")


#' Importe les deux fichiers Excel bruts
#'
#' @param chemin_salaries chemin vers RH_Salaries.xlsx
#' @param chemin_contrats chemin vers RH_Contrats.xlsx
#' @return une liste de deux data frames : salaries et contrats
importer_donnees <- function(chemin_salaries = "data/RH_Salaries.xlsx",
                             chemin_contrats = "data/RH_Contrats.xlsx") {
  list(
    salaries = readxl::read_excel(chemin_salaries, sheet = "Salaries"),
    # Le fichier contrats contient aussi un onglet "Salaries" (copie moins
    # propre : sexe codé "Hommes"/"Femmes"...) : on ne lit que l'onglet Contrat,
    # la référence pour les salariés restant RH_Salaries.xlsx
    contrats = readxl::read_excel(chemin_contrats, sheet = "Contrat")
  )
}


#' Nettoie et assemble les données salariés + contrats
#'
#' @param salaries data frame brut des salariés
#' @param contrats data frame brut des contrats
#' @return une liste : `donnees` (table propre, une ligne par salarié avec
#'   contrat) et `journal` (étapes de nettoyage et lignes concernées)
nettoyer_donnees <- function(salaries, contrats) {

  journal <- tibble::tibble(etape = character(), detail = character(),
                            lignes = integer())
  ajouter <- function(journal, etape, detail, lignes) {
    tibble::add_row(journal, etape = etape, detail = detail,
                    lignes = as.integer(lignes))
  }

  journal <- ajouter(journal, "Import",
                     "Lignes du fichier RH_Salaries.xlsx", nrow(salaries))
  journal <- ajouter(journal, "Import",
                     "Lignes du fichier RH_Contrats.xlsx (onglet Contrat)",
                     nrow(contrats))

  # Harmonisation des noms de colonnes (sans accents ni espaces) -------------
  salaries <- salaries |>
    dplyr::rename(id = `id_salarié`, sexe = Sexe,
                  annee_naissance = `Année naissance`,
                  etat_civil = `Etat Civil`, enfants = Enfants,
                  salaire = Salaire) |>
    dplyr::select(id, sexe, annee_naissance, etat_civil, enfants, salaire,
                  dplyr::any_of("Age"))
  contrats <- contrats |>
    dplyr::rename(id = `id_salarié`, contrat = Contrat,
                  duree_hebdo = `Durée hebdo`)

  # 1. Doublons exacts --------------------------------------------------------
  n_avant <- nrow(salaries)
  salaries <- dplyr::distinct(salaries)
  journal <- ajouter(journal, "Doublons",
                     "Lignes en double supprimées (fichier salariés)",
                     n_avant - nrow(salaries))

  # Si un identifiant restait en double avec des valeurs différentes, on garde
  # la première occurrence (sécurité : aucun cas dans les données actuelles)
  salaries <- dplyr::distinct(salaries, id, .keep_all = TRUE)
  contrats <- dplyr::distinct(contrats, id, .keep_all = TRUE)

  # 2. Jointure : on ne garde que les salariés ayant un contrat --------------
  sans_contrat <- dplyr::anti_join(salaries, contrats, by = "id")
  contrats_orphelins <- dplyr::anti_join(contrats, salaries, by = "id")
  journal <- ajouter(journal, "Jointure",
                     "Salariés exclus car sans contrat", nrow(sans_contrat))
  journal <- ajouter(journal, "Jointure",
                     "Contrats ignorés car sans salarié correspondant",
                     nrow(contrats_orphelins))

  donnees <- dplyr::inner_join(salaries, contrats, by = "id")

  # 3. Sexe : libellés homogènes ---------------------------------------------
  donnees <- donnees |>
    dplyr::mutate(sexe = dplyr::case_when(
      toupper(substr(sexe, 1, 1)) == "H" ~ "Homme",
      toupper(substr(sexe, 1, 1)) == "F" ~ "Femme",
      TRUE ~ NA_character_
    ))

  # 4. Âge : recalculé à partir de l'année de naissance ----------------------
  # La colonne Age fournie correspond à 2023 - année de naissance : on garde
  # cette année de référence pour rester cohérent avec le fichier source
  annee_ref <- if ("Age" %in% names(donnees)) {
    stats::median(donnees$Age + donnees$annee_naissance, na.rm = TRUE)
  } else {
    2023
  }
  donnees <- donnees |>
    dplyr::mutate(
      age = as.integer(annee_ref - annee_naissance),
      tranche_age = cut(age, breaks = BORNES_AGE, labels = LIBELLES_AGE,
                        right = FALSE)
    ) |>
    dplyr::select(-dplyr::any_of("Age"))

  # 5. État civil : valeurs manquantes rendues explicites --------------------
  n_ec_na <- sum(is.na(donnees$etat_civil))
  donnees <- donnees |>
    dplyr::mutate(etat_civil = factor(
      dplyr::coalesce(etat_civil, "Non renseigné"),
      levels = c("Célibataire", "Marié", "Non renseigné")
    ))
  journal <- ajouter(journal, "État civil",
                     "Valeurs manquantes recodées en « Non renseigné »",
                     n_ec_na)

  # 6. Nombre d'enfants : valeur aberrante (ex. 54) -> manquante -------------
  n_enf_aberrant <- sum(donnees$enfants > ENFANTS_MAX, na.rm = TRUE)
  donnees <- donnees |>
    dplyr::mutate(
      enfants = dplyr::if_else(enfants > ENFANTS_MAX, NA_real_, enfants),
      classe_enfants = factor(
        dplyr::case_when(
          is.na(enfants) ~ "Non renseigné",
          enfants >= 3   ~ "3 et plus",
          TRUE           ~ as.character(enfants)
        ),
        levels = c("0", "1", "2", "3 et plus", "Non renseigné")
      )
    )
  journal <- ajouter(journal, "Enfants",
                     paste0("Nombre d'enfants aberrant (> ", ENFANTS_MAX,
                            ") remplacé par une valeur manquante"),
                     n_enf_aberrant)

  # 7. Contrat et durée hebdomadaire ----------------------------------------
  n_duree_aberrante <- sum(donnees$duree_hebdo > DUREE_HEBDO_MAX |
                             donnees$duree_hebdo <= 0, na.rm = TRUE)
  donnees <- donnees |>
    dplyr::mutate(
      contrat = factor(toupper(trimws(contrat)), levels = c("CDI", "CDD")),
      duree_hebdo = dplyr::if_else(
        duree_hebdo > DUREE_HEBDO_MAX | duree_hebdo <= 0,
        NA_real_, as.numeric(duree_hebdo)),
      temps_travail = factor(
        dplyr::case_when(
          is.na(duree_hebdo)                 ~ NA_character_,
          duree_hebdo >= DUREE_TEMPS_PLEIN   ~ "Temps plein",
          TRUE                               ~ "Temps partiel"
        ),
        levels = c("Temps plein", "Temps partiel")
      )
    )
  journal <- ajouter(journal, "Durée hebdo",
                     paste0("Durées hebdomadaires aberrantes (> ",
                            DUREE_HEBDO_MAX, " h) remplacées par NA"),
                     n_duree_aberrante)

  # 8. Salaire ---------------------------------------------------------------
  n_sal_na <- sum(is.na(donnees$salaire) | donnees$salaire <= 0)
  donnees <- donnees |>
    dplyr::mutate(
      salaire = dplyr::if_else(salaire <= 0, NA_real_, salaire),
      salaire = round(salaire, 2),
      # Salaire ramené à un temps plein (35 h) pour comparer temps partiels
      # et temps pleins sur une même base
      salaire_etp = round(salaire * DUREE_TEMPS_PLEIN / duree_hebdo, 2)
    )
  journal <- ajouter(journal, "Salaire",
                     "Salariés sans salaire renseigné (exclus des comparaisons)",
                     n_sal_na)

  # Repérage des salaires atypiques (règle de Tukey : > Q3 + 1,5 x IQR ou
  # < Q1 - 1,5 x IQR). Ils ne sont pas supprimés : l'utilisateur choisit
  # de les exclure ou non dans l'application.
  donnees <- donnees |>
    dplyr::mutate(salaire_atypique = est_atypique(salaire))
  journal <- ajouter(journal, "Salaire",
                     "Salaires atypiques repérés (règle de Tukey, exclusion optionnelle)",
                     sum(donnees$salaire_atypique, na.rm = TRUE))

  # Mise en forme finale -----------------------------------------------------
  donnees <- donnees |>
    dplyr::mutate(sexe = factor(sexe, levels = c("Femme", "Homme"))) |>
    dplyr::select(id, sexe, age, tranche_age, etat_civil, enfants,
                  classe_enfants, contrat, duree_hebdo, temps_travail,
                  salaire, salaire_etp, salaire_atypique) |>
    dplyr::arrange(id)

  journal <- ajouter(journal, "Résultat",
                     "Salariés retenus dans l'application", nrow(donnees))
  journal <- ajouter(journal, "Résultat",
                     "… dont salariés avec un salaire exploitable",
                     sum(!is.na(donnees$salaire)))

  list(donnees = donnees, journal = journal)
}


#' Repère les valeurs atypiques selon la règle de Tukey (boîte à moustaches)
#'
#' @param x vecteur numérique
#' @param k coefficient multiplicateur de l'écart interquartile (1,5 par défaut)
#' @return vecteur logique (NA si x est NA)
est_atypique <- function(x, k = 1.5) {
  q <- stats::quantile(x, c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[[2]] - q[[1]]
  x < q[[1]] - k * iqr | x > q[[2]] + k * iqr
}


# #############################################################################
# 3. FILTRES, STATISTIQUES ET TESTS
#    Fonctions de filtrage, de statistiques descriptives et de tests,
#    appelées par server.R à chaque changement des paramètres.
# #############################################################################

# Dictionnaires : libellé affiché  ->  nom de colonne -------------------------

CRITERES <- c(
  # Apostrophe typographique (’) : ggiraph n'accepte pas « ' » dans les infobulles
  "Tranche d’âge"    = "tranche_age",
  "Sexe"             = "sexe",
  "État civil"       = "etat_civil",
  "Type de contrat"  = "contrat",
  "Temps de travail" = "temps_travail",
  "Nombre d’enfants" = "classe_enfants"
)

INDICATEURS <- c(
  "Salaire mensuel"                      = "salaire",
  "Salaire équivalent temps plein (35 h)" = "salaire_etp"
)

#' Libellé lisible d'une colonne (critère ou indicateur)
libelle <- function(colonne) {
  tout <- c(CRITERES, INDICATEURS)
  nom <- names(tout)[tout == colonne]
  if (length(nom) == 0) colonne else nom[1]
}


#' Filtre les salariés selon les critères choisis
#'
#' @param donnees table propre (sortie de nettoyer_donnees)
#' @param filtres liste nommée : nom de colonne -> modalités à conserver.
#'   Une entrée NULL signifie « pas de filtre » sur ce critère.
#' @param exclure_atypiques TRUE pour retirer les salaires atypiques (Tukey)
#' @return la table filtrée, limitée aux salariés ayant un salaire renseigné
filtrer_donnees <- function(donnees, filtres = list(),
                            exclure_atypiques = FALSE) {
  res <- dplyr::filter(donnees, !is.na(salaire))

  for (col in names(filtres)) {
    valeurs <- filtres[[col]]
    if (!is.null(valeurs)) {
      res <- dplyr::filter(res, as.character(.data[[col]]) %in% valeurs)
    }
  }

  if (exclure_atypiques) {
    res <- dplyr::filter(res, !salaire_atypique)
  }
  res
}


#' Statistiques descriptives de l'indicateur par groupe
#'
#' @param df table filtrée
#' @param indicateur "salaire" ou "salaire_etp"
#' @param critere colonne de regroupement
#' @param croisement (facultatif) seconde colonne de regroupement
#' @return tibble : une ligne par modalité (ou couple de modalités) avec
#'   effectif, moyenne, médiane, quartiles...
resumer_salaires <- function(df, indicateur, critere, croisement = NULL) {
  df <- df |>
    dplyr::filter(!is.na(.data[[indicateur]]), !is.na(.data[[critere]]))

  if (is.null(croisement)) {
    df <- dplyr::group_by(df, groupe = .data[[critere]])
  } else {
    df <- df |>
      dplyr::filter(!is.na(.data[[croisement]])) |>
      dplyr::group_by(groupe = .data[[critere]],
                      sous_groupe = .data[[croisement]])
  }

  df |>
    dplyr::summarise(
      effectif   = dplyr::n(),
      moyenne    = mean(.data[[indicateur]]),
      mediane    = stats::median(.data[[indicateur]]),
      q1         = stats::quantile(.data[[indicateur]], 0.25),
      q3         = stats::quantile(.data[[indicateur]], 0.75),
      ecart_type = stats::sd(.data[[indicateur]]),
      minimum    = min(.data[[indicateur]]),
      maximum    = max(.data[[indicateur]]),
      .groups = "drop"
    )
}


#' Test de comparaison des niveaux de rémunération entre groupes
#'
#' - 2 groupes  : test de Student (Welch) ou test de Wilcoxon-Mann-Whitney
#' - 3 groupes+ : ANOVA de Welch ou test de Kruskal-Wallis
#' Les modalités « Non renseigné » et les groupes de moins de `n_min`
#' salariés sont écartés du test.
#'
#' @param df table filtrée
#' @param indicateur colonne numérique comparée
#' @param critere colonne définissant les groupes
#' @param methode "non_parametrique" (médianes / rangs) ou "parametrique"
#' @param alpha seuil de significativité
#' @param n_min effectif minimal par groupe
#' @return liste décrivant le test et sa conclusion
tester_difference <- function(df, indicateur, critere,
                              methode = c("non_parametrique", "parametrique"),
                              alpha = 0.05, n_min = 3) {
  methode <- match.arg(methode)

  d <- df |>
    dplyr::filter(!is.na(.data[[indicateur]]), !is.na(.data[[critere]]),
                  as.character(.data[[critere]]) != "Non renseigné") |>
    dplyr::transmute(y = .data[[indicateur]],
                     g = droplevels(as.factor(.data[[critere]])))

  effectifs <- table(d$g)
  exclus <- names(effectifs)[effectifs < n_min]
  d <- dplyr::filter(d, !(g %in% exclus)) |>
    dplyr::mutate(g = droplevels(g))
  k <- nlevels(d$g)

  if (k < 2) {
    return(list(
      ok = FALSE, exclus = exclus,
      message = paste0("Au moins deux groupes d'au moins ", n_min,
                       " salariés sont nécessaires pour réaliser un test.")
    ))
  }

  if (k == 2) {
    if (methode == "parametrique") {
      t <- stats::t.test(y ~ g, data = d, var.equal = FALSE)
      nom <- "Test de Student (Welch)"
      h0 <- "les salaires moyens sont égaux"
    } else {
      t <- stats::wilcox.test(y ~ g, data = d, exact = FALSE)
      nom <- "Test de Wilcoxon-Mann-Whitney"
      h0 <- "les distributions de salaires sont identiques"
    }
  } else {
    if (methode == "parametrique") {
      t <- stats::oneway.test(y ~ g, data = d, var.equal = FALSE)
      nom <- "ANOVA à un facteur (Welch)"
      h0 <- "les salaires moyens sont égaux"
    } else {
      t <- stats::kruskal.test(y ~ g, data = d)
      nom <- "Test de Kruskal-Wallis"
      h0 <- "les distributions de salaires sont identiques"
    }
  }

  p <- unname(t$p.value)
  significatif <- p < alpha
  conclusion <- if (significatif) {
    paste0("Différence significative au seuil de ", alpha * 100,
           "\u00a0% : on rejette l'hypothèse selon laquelle ", h0, ".")
  } else {
    paste0("Pas de différence significative au seuil de ", alpha * 100,
           "\u00a0% : on ne peut pas rejeter l'hypothèse selon laquelle ", h0, ".")
  }

  list(
    ok = TRUE, test = nom, h0 = h0, k = k, n = nrow(d),
    statistique = unname(t$statistic), p_value = p,
    significatif = significatif, conclusion = conclusion,
    exclus = exclus, alpha = alpha
  )
}


# Formats français -------------------------------------------------------------

# Espace insécable (\u00a0) : « 2 540 € » ne sera jamais coupé en fin de ligne
format_nombre <- function(x, decimales = 0) {
  formatC(x, format = "f", digits = decimales, big.mark = "\u00a0",
          decimal.mark = ",")
}

format_euros <- function(x, decimales = 0) {
  paste0(format_nombre(x, decimales), "\u00a0€")
}

format_pvalue <- function(p) {
  if (is.na(p)) return("–")
  if (p < 0.001) return("< 0,001")
  formatC(p, format = "f", digits = 3, decimal.mark = ",")
}


#' Profil des salariés : répartition (effectif et %) selon plusieurs critères
#'
#' @param df table filtrée
#' @param criteres colonnes à décrire (par défaut : les 4 critères du sujet)
#' @return tibble long : critere, modalite, effectif, part
calculer_profil <- function(df,
                            criteres = c("tranche_age", "sexe", "etat_civil",
                                         "contrat")) {
  purrr::map(criteres, \(col) {
    df |>
      dplyr::count(modalite = .data[[col]], name = "effectif") |>
      dplyr::filter(!is.na(modalite)) |>
      dplyr::mutate(critere = libelle(col),
                    modalite = as.character(modalite),
                    part = effectif / sum(effectif))
  }) |>
    purrr::list_rbind() |>
    dplyr::mutate(critere = factor(critere, levels = purrr::map_chr(criteres, libelle)))
}


#' Classes de salaire pour l'histogramme (largeur fixe, en euros)
#'
#' @param df table filtrée
#' @param indicateur colonne numérique
#' @param largeur largeur des classes en euros
#' @return tibble : debut, fin, effectif, part
classer_salaires <- function(df, indicateur, largeur = 500) {
  df |>
    dplyr::filter(!is.na(.data[[indicateur]])) |>
    dplyr::mutate(debut = floor(.data[[indicateur]] / largeur) * largeur) |>
    dplyr::count(debut, name = "effectif") |>
    dplyr::mutate(fin = debut + largeur, part = effectif / sum(effectif))
}


# #############################################################################
# 4. GRAPHIQUES (ggplot2 + ggiraph)
#    Chaque geom_xxx() est remplacé par son équivalent
#    geom_xxx_interactive(), qui accepte en plus les esthétiques :
#      - tooltip : texte de l'infobulle affichée au survol
#      - data_id : identifiant utilisé pour surligner les éléments survolés
#    Les fonctions renvoient un objet ggplot (testable dans la console) ;
#    rendre_interactif() le transforme en widget girafe pour Shiny.
# #############################################################################

# Palette catégorielle (ordre fixe, lisible par les daltoniens) ---------------
PALETTE <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300")
GRIS_NR <- "#9a9893"   # couleur réservée à la modalité « Non renseigné »

#' Associe une couleur fixe à chaque modalité d'une variable
#'
#' La couleur dépend de la modalité (et non de sa position après filtrage) :
#' « Femme » garde la même couleur quels que soient les filtres appliqués.
#'
#' @param niveaux toutes les modalités possibles de la variable
#' @return vecteur nommé modalité -> couleur
couleurs_modalites <- function(niveaux) {
  niveaux <- as.character(niveaux)
  autres <- setdiff(niveaux, "Non renseigné")
  coul <- purrr::set_names(PALETTE[seq_along(autres)], autres)
  if ("Non renseigné" %in% niveaux) coul["Non renseigné"] <- GRIS_NR
  coul[niveaux]
}

#' Thème ggplot2 commun à tous les graphiques de l'application
theme_rh <- function() {
  ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = "#2b2b2b"),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(colour = "#52514e", size = 11,
                                           margin = ggplot2::margin(r = 8)),
      axis.text = ggplot2::element_text(colour = "#52514e"),
      axis.text.x = ggplot2::element_text(size = 12, colour = "#2b2b2b"),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "#ecebe7"),
      legend.position = "top",
      legend.justification = "left",
      legend.title = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(5, 10, 5, 5)
    )
}

#' Axe des ordonnées en euros, au format français (2 500 €)
echelle_euros <- function(...) {
  ggplot2::scale_y_continuous(
    labels = function(x) format_euros(x),
    n.breaks = 6,
    expand = ggplot2::expansion(mult = c(0, 0.08)),
    ...
  )
}


#' Boîtes à moustaches de l'indicateur par groupe, avec les salariés en points
#'
#' @param df table filtrée
#' @param indicateur colonne numérique ("salaire" ou "salaire_etp")
#' @param critere colonne en abscisse
#' @param croisement (facultatif) colonne définissant la couleur
#' @param niveaux_couleur modalités complètes de la variable colorée
#' @return objet ggplot (avec couches interactives ggiraph)
graphique_distribution <- function(df, indicateur, critere, croisement = NULL,
                                   niveaux_couleur = NULL) {
  var_couleur <- if (is.null(croisement)) critere else croisement
  if (is.null(niveaux_couleur)) niveaux_couleur <- levels(df[[var_couleur]])

  d <- df |>
    dplyr::filter(!is.na(.data[[indicateur]]), !is.na(.data[[critere]]),
                  !is.na(.data[[var_couleur]])) |>
    dplyr::mutate(
      x = forcats::fct_drop(.data[[critere]]),
      couleur = factor(.data[[var_couleur]], levels = niveaux_couleur),
      y = .data[[indicateur]],
      # Infobulle de chaque point (un salarié)
      infobulle = paste0(
        "<b>Salarié ", id, "</b><br>",
        sexe, ", ", age, " ans – ", etat_civil, "<br>",
        contrat, ", ", duree_hebdo, " h / semaine<br>",
        libelle(indicateur), " : ", format_euros(y)
      )
    ) |>
    # Infobulle de chaque boîte (un groupe) : effectif et quartiles
    dplyr::group_by(x, couleur) |>
    dplyr::mutate(
      infobulle_boite = paste0(
        "<b>", x, if (!is.null(croisement)) paste0(" – ", couleur) else "",
        "</b><br>Effectif : ", dplyr::n(),
        "<br>Médiane : ", format_euros(stats::median(y)),
        "<br>Q1 – Q3 : ", format_euros(stats::quantile(y, 0.25)), " – ",
        format_euros(stats::quantile(y, 0.75))
      )
    ) |>
    dplyr::ungroup()

  # Positions : boîtes côte à côte (dodge) si un critère de croisement existe
  decalage <- ggplot2::position_dodge(width = 0.8)
  position_points <- if (is.null(croisement)) {
    ggplot2::position_jitter(width = 0.18, height = 0, seed = 1)
  } else {
    ggplot2::position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8,
                                  seed = 1)
  }

  ggplot2::ggplot(d, ggplot2::aes(x = x, y = y, fill = couleur,
                                  colour = couleur)) +
    ggiraph::geom_boxplot_interactive(
      ggplot2::aes(tooltip = infobulle_boite,
                   data_id = paste(x, couleur)),
      position = decalage, width = 0.7, alpha = 0.18, linewidth = 0.7,
      outlier.shape = NA            # les points sont déjà tous affichés
    ) +
    ggiraph::geom_point_interactive(
      ggplot2::aes(tooltip = infobulle, data_id = id),
      position = position_points, size = 1.8, alpha = 0.65, shape = 16
    ) +
    ggplot2::scale_fill_manual(values = couleurs_modalites(niveaux_couleur)) +
    ggplot2::scale_colour_manual(values = couleurs_modalites(niveaux_couleur)) +
    echelle_euros(limits = c(0, NA)) +
    ggplot2::labs(y = libelle(indicateur)) +
    theme_rh() +
    ggplot2::theme(legend.position = if (is.null(croisement)) "none" else "top")
}


#' Diagramme en barres de la moyenne ou de la médiane par groupe
#'
#' @param resume sortie de resumer_salaires()
#' @param statistique "mediane" ou "moyenne"
#' @param indicateur colonne de l'indicateur (pour le titre de l'axe)
#' @param niveaux_couleur modalités complètes de la variable colorée
#' @return objet ggplot (avec couches interactives ggiraph)
graphique_niveaux <- function(resume, statistique = c("mediane", "moyenne"),
                              indicateur = "salaire",
                              niveaux_couleur = NULL) {
  statistique <- match.arg(statistique)
  nom_stat <- if (statistique == "mediane") "Salaire médian" else "Salaire moyen"
  croise <- "sous_groupe" %in% names(resume)
  var_couleur <- if (croise) "sous_groupe" else "groupe"
  if (is.null(niveaux_couleur)) niveaux_couleur <- levels(resume[[var_couleur]])

  d <- resume |>
    dplyr::mutate(
      valeur = .data[[statistique]],
      groupe = forcats::fct_drop(groupe),
      couleur = factor(.data[[var_couleur]], levels = niveaux_couleur),
      etiquette = format_euros(valeur),
      infobulle = paste0(
        "<b>", groupe, if (croise) paste0(" – ", sous_groupe) else "",
        "</b><br>", nom_stat, " : ", format_euros(valeur),
        "<br>Effectif : ", effectif, " salarié", ifelse(effectif > 1, "s", ""),
        "<br>Q1 – Q3 : ", format_euros(q1), " – ", format_euros(q3)
      )
    )

  decalage <- ggplot2::position_dodge(width = 0.8)

  ggplot2::ggplot(d, ggplot2::aes(x = groupe, y = valeur, fill = couleur)) +
    ggiraph::geom_col_interactive(
      ggplot2::aes(tooltip = infobulle, data_id = paste(groupe, couleur)),
      position = decalage, width = 0.7, colour = "white", linewidth = 0.6
    ) +
    # Étiquette de valeur au-dessus de chaque barre
    ggplot2::geom_text(ggplot2::aes(label = etiquette),
                       position = decalage, vjust = -0.6, size = 3.6,
                       colour = "#2b2b2b") +
    ggplot2::scale_fill_manual(values = couleurs_modalites(niveaux_couleur)) +
    echelle_euros(limits = c(0, max(d$valeur) * 1.12)) +
    ggplot2::labs(y = paste(nom_stat, "–", tolower(libelle(indicateur)))) +
    theme_rh() +
    ggplot2::theme(legend.position = if (croise) "top" else "none")
}


#' Histogramme de la distribution des salaires, avec la médiane en repère
#'
#' @param df table filtrée
#' @param indicateur colonne numérique ("salaire" ou "salaire_etp")
#' @param largeur largeur des classes en euros
#' @return objet ggplot (avec couches interactives ggiraph)
graphique_histogramme <- function(df, indicateur = "salaire", largeur = 500) {
  classes <- classer_salaires(df, indicateur, largeur) |>
    dplyr::mutate(
      milieu = debut + largeur / 2,
      infobulle = paste0(
        "<b>", format_euros(debut), " – ", format_euros(fin), "</b><br>",
        effectif, " salarié", ifelse(effectif > 1, "s", ""),
        " (", format_nombre(100 * part, 1), "\u00a0%)"
      )
    )
  mediane <- stats::median(df[[indicateur]], na.rm = TRUE)

  ggplot2::ggplot(classes, ggplot2::aes(x = milieu, y = effectif)) +
    ggiraph::geom_col_interactive(
      ggplot2::aes(tooltip = infobulle, data_id = debut),
      width = largeur, fill = PALETTE[1], colour = "white", linewidth = 0.6
    ) +
    # Repère vertical : salaire médian
    ggplot2::geom_vline(xintercept = mediane, linetype = "dashed",
                        colour = "#1f1f1e", linewidth = 0.5) +
    # annotate("text") plutôt que "label" : le paramètre label.size n'existe
    # plus dans les versions récentes de ggplot2 (avertissement à chaque
    # affichage et cadre noir autour du texte)
    ggplot2::annotate("text", x = mediane, y = max(classes$effectif) * 1.06,
                      label = paste("Médiane :", format_euros(mediane)),
                      hjust = -0.05, size = 3.6, colour = "#1f1f1e") +
    ggplot2::scale_x_continuous(labels = function(x) format_euros(x),
                                n.breaks = 8) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(y = "Nombre de salariés") +
    theme_rh() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 11,
                                                       colour = "#52514e"))
}


#' Profil des salariés : une barre par modalité, un panneau par critère
#'
#' @param profil sortie de calculer_profil()
#' @return objet ggplot (avec couches interactives ggiraph)
graphique_profil <- function(profil) {
  d <- profil |>
    dplyr::mutate(
      # Ordre des modalités conservé de haut en bas dans chaque panneau
      modalite = forcats::fct_rev(forcats::fct_inorder(modalite)),
      etiquette = paste0(format_nombre(100 * part), "\u00a0%"),
      infobulle = paste0("<b>", critere, " : ", modalite, "</b><br>",
                         effectif, " salarié", ifelse(effectif > 1, "s", ""),
                         " (", format_nombre(100 * part, 1), "\u00a0%)")
    )

  ggplot2::ggplot(d, ggplot2::aes(x = part, y = modalite)) +
    ggiraph::geom_col_interactive(
      ggplot2::aes(tooltip = infobulle, data_id = paste(critere, modalite)),
      width = 0.65, fill = PALETTE[1]
    ) +
    ggplot2::geom_text(ggplot2::aes(label = etiquette), hjust = -0.15,
                       size = 3.6, colour = "#2b2b2b") +
    ggplot2::facet_wrap(~ critere, ncol = 2, scales = "free_y") +
    ggplot2::scale_x_continuous(limits = c(0, 1.12), breaks = NULL,
                                expand = ggplot2::expansion(0)) +
    theme_rh() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 11, colour = "#2b2b2b"),
      panel.grid.major.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 12, hjust = 0,
                                         colour = "#1f1f1e"),
      panel.spacing = ggplot2::unit(1.2, "lines")
    )
}


#' Transforme un ggplot interactif en widget girafe (affichable dans Shiny)
#'
#' @param graphique objet ggplot contenant des couches *_interactive
#' @param hauteur hauteur du graphique (en pouces, pour une largeur de 10)
#' @return widget htmlwidget girafe
rendre_interactif <- function(graphique, hauteur = 4.6) {
  ggiraph::girafe(
    ggobj = graphique,
    width_svg = 10, height_svg = hauteur,
    options = list(
      ggiraph::opts_tooltip(
        css = paste("background:white; color:#2b2b2b; padding:8px 10px;",
                    "border-radius:6px; border:1px solid #d7d6d1;",
                    "font-family:system-ui,sans-serif; font-size:13px;",
                    "box-shadow:0 2px 6px rgba(0,0,0,.12);"),
        opacity = 1, use_fill = FALSE
      ),
      ggiraph::opts_hover(css = "stroke:#1f1f1e; stroke-width:1.5px;"),
      ggiraph::opts_hover_inv(css = "opacity:0.35;"),
      ggiraph::opts_toolbar(saveaspng = TRUE, position = "topright"),
      ggiraph::opts_sizing(rescale = TRUE)
    )
  )
}


# #############################################################################
# 5. CHARGEMENT DES DONNÉES AU DÉMARRAGE
#    Utilisation des fonctions des parties 2 et 3.
# #############################################################################

# Import + nettoyage des données (chemins relatifs : pas de setwd()) ----------
brut <- importer_donnees("data/RH_Salaries.xlsx", "data/RH_Contrats.xlsx")
prepa <- nettoyer_donnees(brut$salaries, brut$contrats)

DONNEES <- prepa$donnees          # table propre utilisée par l'application
JOURNAL <- prepa$journal          # journal du nettoyage (page Méthodologie)
N_TOTAL <- sum(!is.na(DONNEES$salaire))

# Modalités de chaque critère : servent aux filtres et aux couleurs fixes -----
MODALITES <- map(CRITERES, \(col) levels(DONNEES[[col]])) |> set_names(CRITERES)
