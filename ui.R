# =============================================================================
# ui.R
# -----------------------------------------------------------------------------
# Interface de l'application, construite avec bslib :
#   - page_navbar : barre de navigation avec 3 pages
#   - sidebar     : panneau latéral commun (paramètres d'analyse + filtres)
#   - card / value_box / layout_columns : mise en page de la page principale
#   - bs_theme() : thème personnalisé (couleurs, police, arrondis)
# =============================================================================

# Petite fonction utilitaire : une case à cocher multiple par critère de filtre,
# avec toutes les modalités cochées par défaut
filtre_criteres <- function(colonne) {
  checkboxGroupInput(
    inputId = paste0("filtre_", colonne),
    label = libelle(colonne),
    choices = MODALITES[[colonne]],
    selected = MODALITES[[colonne]]
  )
}

# Thème de l'application -----------------------------------------------------
# Thème bslib personnalisé (couleurs, police, arrondis). Il est nommé
# theme_application pour ne pas le confondre avec theme_rh(), le thème des
# graphiques ggplot2 défini dans global.R.
theme_application <- bs_theme(
  version = 5,
  bg = "#fcfcfb", fg = "#1f1f1e",
  primary = "#2a78d6", secondary = "#52514e",
  base_font = font_google("Inter", local = FALSE),
  heading_font = font_google("Inter", local = FALSE),
  "border-radius" = "0.6rem"
)

# Quelques ajustements visuels (règles CSS), insérés dans l'en-tête de la page
# avec tags$style() : ils remplacent l'ancien fichier www/styles.css
styles_application <- "
  .bslib-value-box .value-box-title { font-size: 0.85rem; opacity: 0.85; }
  .bslib-value-box .value-box-value { font-size: 1.6rem; font-weight: 600; }
  .bslib-value-box p { font-size: 0.8rem; margin-bottom: 0; opacity: 0.85; }

  .sidebar .form-label, .sidebar .control-label { font-weight: 500; font-size: 0.9rem; }
  .sidebar .shiny-input-container { margin-bottom: 0.6rem; }
  .sidebar .accordion-button { font-size: 0.9rem; padding: 0.5rem 0.75rem; }

  .card-header { font-weight: 600; }
  .card-header .fa, .card-header svg { margin-right: 0.35rem; opacity: 0.7; }

  .shiny-output-error-validation { color: #52514e; font-style: italic; padding: 1rem; }

  /* Tableau de la page Données : pas de retour à la ligne, en-tête fixe */
  #table_donnees table { font-size: 0.88rem; }
  #table_donnees td, #table_donnees th { white-space: nowrap; }
  #table_donnees thead th { position: sticky; top: 0; background: var(--bs-body-bg); }
  #table_resume th { white-space: nowrap; }
  .card-body h6 { font-weight: 600; margin-top: 0.9rem; margin-bottom: 0.35rem; }
  .bslib-value-box .value-box-showcase { opacity: 0.8; }

  /* Pastille avec les initiales des auteurs (page À propos) */
  .auteur-initiales {
    width: 48px; height: 48px; border-radius: 50%; flex-shrink: 0;
    display: flex; align-items: center; justify-content: center;
    background: #2a78d6; color: white; font-weight: 600; font-size: 1.05rem;
  }
"

# Panneau latéral ------------------------------------------------------------
panneau_lateral <- sidebar(
  width = 300,

  h6("Analyse", class = "text-uppercase text-muted small fw-semibold"),
  selectInput("critere", "Comparer les salaires selon",
              choices = CRITERES, selected = "tranche_age"),
  selectInput("croisement", "Croiser avec (facultatif)",
              choices = c("Aucun" = "aucun", CRITERES[-1])),
  radioButtons("indicateur", "Indicateur de rémunération",
               choices = INDICATEURS, selected = "salaire"),
  radioButtons("methode", "Test statistique",
               choices = c("Non paramétrique (médianes)" = "non_parametrique",
                           "Paramétrique (moyennes)" = "parametrique")),
  checkboxInput("exclure_atypiques",
                "Exclure les salaires atypiques", value = FALSE),

  hr(class = "my-1"),
  h6("Filtres", class = "text-uppercase text-muted small fw-semibold"),
  accordion(
    # Le premier panneau (les 4 critères du sujet) est ouvert au démarrage
    open = "Âge, sexe, état civil, contrat", multiple = TRUE,
    accordion_panel("Âge, sexe, état civil, contrat", icon = icon("filter"),
                    filtre_criteres("tranche_age"),
                    filtre_criteres("sexe"),
                    filtre_criteres("etat_civil"),
                    filtre_criteres("contrat")),
    accordion_panel("Autres filtres", icon = icon("sliders"),
                    filtre_criteres("temps_travail"),
                    filtre_criteres("classe_enfants"))
  ),
  actionButton("reinitialiser", "Réinitialiser les filtres",
               icon = icon("rotate-left"),
               class = "btn-outline-secondary btn-sm")
)

# Page 1 : comparaison des rémunérations (page principale) -------------------
page_comparaison <- nav_panel(
  title = "Comparer les rémunérations", icon = icon("scale-balanced"),

  # Indicateurs clés : sans icône "showcase" et avec des valeurs courtes, les
  # boîtes sont moins hautes et laissent plus de place au graphique
  layout_columns(
    fill = FALSE, col_widths = c(3, 3, 3, 3),
    value_box(title = "Salariés analysés", value = textOutput("vb_effectif"),
              theme = "primary",
              p(textOutput("vb_effectif_detail", inline = TRUE))),
    value_box(title = "Salaire médian", value = textOutput("vb_mediane"),
              theme = "light",
              p(textOutput("vb_ecart", inline = TRUE))),
    value_box(title = "Salaire moyen", value = textOutput("vb_moyenne"),
              theme = "light",
              p(textOutput("vb_indicateur", inline = TRUE))),
    value_box(title = "Écart significatif ?", value = textOutput("vb_test"),
              theme = "light",
              p(textOutput("vb_pvalue", inline = TRUE)))
  ),

  # Graphiques
  navset_card_tab(
    full_screen = TRUE,
    title = textOutput("titre_graphique"),
    nav_panel("Distribution", girafeOutput("graph_distribution", height = "auto")),
    nav_panel(
      "Médiane / moyenne",
      radioButtons("statistique", NULL, inline = TRUE,
                   choices = c("Médiane" = "mediane", "Moyenne" = "moyenne")),
      girafeOutput("graph_niveaux", height = "auto")
    )
  ),

  # Test statistique + tableau récapitulatif
  layout_columns(
    col_widths = c(4, 8), fill = FALSE,
    card(card_header(icon("flask"), "Test statistique"),
         uiOutput("resultat_test")),
    card(card_header(icon("table"), "Statistiques par groupe"),
         # text-nowrap : les libellés restent sur une ligne ;
         # table-responsive : défilement horizontal plutôt qu'un tableau coupé
         div(class = "table-responsive text-nowrap",
             tableOutput("table_resume")))
  )
)

# Page 2 : profil des salariés (vue d'ensemble du périmètre filtré) ---------
page_profil <- nav_panel(
  title = "Profil des salariés", icon = icon("chart-column"),

  layout_columns(
    fill = FALSE, col_widths = c(3, 3, 3, 3),
    value_box(title = "Salariés", value = textOutput("pf_effectif"),
              theme = "primary",
              p("avec contrat et salaire renseigné")),
    value_box(title = "Part de femmes", value = textOutput("pf_femmes"),
              theme = "light", p(textOutput("pf_femmes_n", inline = TRUE))),
    value_box(title = "Part en CDI", value = textOutput("pf_cdi"),
              theme = "light", p(textOutput("pf_cdi_n", inline = TRUE))),
    value_box(title = "Part à temps partiel", value = textOutput("pf_partiel"),
              theme = "light", p(textOutput("pf_partiel_n", inline = TRUE)))
  ),

  card(full_screen = TRUE,
       card_header(icon("chart-column"), textOutput("titre_histogramme", inline = TRUE)),
       girafeOutput("graph_histogramme", height = "auto")),

  card(full_screen = TRUE,
       card_header(icon("users"), "Répartition des salariés selon les critères étudiés"),
       girafeOutput("graph_profil", height = "auto"))
)

# Page 3 : données détaillées ------------------------------------------------
page_donnees <- nav_panel(
  title = "Données", value = "donnees", icon = icon("table-list"),
  card(
    full_screen = TRUE,
    card_header(
      class = "d-flex justify-content-between align-items-center",
      span(icon("table-list"), "Salariés du périmètre sélectionné"),
      downloadButton("telecharger", "Télécharger (CSV)",
                     class = "btn-sm btn-primary")
    ),
    p(class = "small text-muted mb-2",
      "Le tableau suit les filtres du panneau latéral."),
    tableOutput("table_donnees")
  )
)

# Page 4 : à propos (projet, auteurs, méthodologie) --------------------------

# Carte de présentation d'un auteur (initiales en pastille)
carte_auteur <- function(prenom, nom) {
  div(class = "d-flex align-items-center gap-3",
      div(class = "auteur-initiales",
          paste0(substr(prenom, 1, 1), substr(nom, 1, 1))),
      div(div(class = "fw-semibold", paste(prenom, toupper(nom))),
          div(class = "small text-muted",
              "Master 1 Statistique – Data Science et modélisation statistique")))
}

page_apropos <- nav_panel(
  title = "À propos", icon = icon("circle-info"),
  layout_columns(
    col_widths = c(7, 5), fill = FALSE,
    card(card_header("Le projet"),
         p("Application réalisée dans le cadre du cours « Applications de
           visualisation R Shiny » (Université Bretagne Sud, Vannes)."),
         p(strong("Objectif : "), "suivre et comparer les rémunérations des
           salariés d'une entreprise fictive selon l'âge, le sexe, l'état
           civil et le type de contrat, et tester si les écarts observés
           sont statistiquement significatifs."),
         p(class = "small text-muted mb-0",
           "Données fictives à visée pédagogique (RH_Salaries.xlsx et
           RH_Contrats.xlsx). Outils : R, Shiny, tidyverse, bslib, ggiraph.")),
    card(card_header("Auteurs"),
         div(class = "d-flex flex-column gap-3",
             carte_auteur("Ibrahima", "Fall"),
             carte_auteur("Boubacar", "Diallo")))
  ),
  layout_columns(
    col_widths = c(6, 6),
    card(card_header("Nettoyage des données"),
         p("Étapes appliquées au démarrage de l'application
           (fonction ", code("nettoyer_donnees()"), ") :"),
         tableOutput("table_journal"),
         p(class = "small text-muted",
           "Le fichier RH_Contrats.xlsx contient aussi un onglet « Salaries »
           (copie moins propre : sexe codé « Hommes » / « Femmes », une
           incohérence de sexe). Le fichier RH_Salaries.xlsx sert de référence.")),
    card(card_header("Choix méthodologiques"),
         h6("Indicateurs", class = "mt-0"),
         tags$ul(
           tags$li(strong("Salaire mensuel"), " : salaire tel que fourni."),
           tags$li(strong("Salaire équivalent temps plein"),
                   " : salaire × 35 / durée hebdomadaire, pour comparer
                   temps partiels et temps pleins sur une même base.")
         ),
         h6("Tranches d'âge"),
         p("Les âges observés vont de 22 à 59 ans (année de référence 2023).
           Quatre tranches de 10 ans (moins de 30, 30-39, 40-49, 50 et plus)
           donnent des effectifs suffisants dans chaque groupe."),
         h6("Salaires atypiques"),
         p("Repérés par la règle de Tukey (au-delà de Q3 + 1,5 × écart
           interquartile). Ils ne sont pas supprimés d'office : une case à
           cocher permet de vérifier si les conclusions changent sans eux."),
         h6("Tests statistiques"),
         tags$ul(
           tags$li(strong("2 groupes"), " : Wilcoxon-Mann-Whitney
                   (non paramétrique) ou Student-Welch (paramétrique)."),
           tags$li(strong("3 groupes ou plus"), " : Kruskal-Wallis
                   (non paramétrique) ou ANOVA de Welch (paramétrique).")
         ),
         p("Le test non paramétrique est proposé par défaut car les salaires
           ont une distribution asymétrique (quelques salaires très élevés).
           Les modalités « Non renseigné » et les groupes de moins de 3
           salariés sont exclus du test. Seuil retenu : 5 %."))
  )
)

# Assemblage -----------------------------------------------------------------
ui <- page_navbar(
  title = span(icon("users-gear"), "Rémunérations RH"),
  theme = theme_application,
  header = tags$head(tags$style(HTML(styles_application))),
  sidebar = panneau_lateral,
  fillable = "donnees",   # seule la page Données remplit la hauteur
  page_comparaison,
  page_profil,
  page_donnees,
  page_apropos
)
