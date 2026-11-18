# =============================================================================
# server.R
# -----------------------------------------------------------------------------
# Partie serveur : uniquement la logique réactive (liens entre inputs et
# outputs). Les calculs eux-mêmes sont faits par les fonctions de global.R.
# =============================================================================

server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # 1. Mises à jour de l'interface
  # ---------------------------------------------------------------------------

  # Le critère de croisement ne peut pas être identique au critère principal
  observeEvent(input$critere, {
    choix <- CRITERES[CRITERES != input$critere]
    selection <- if (input$croisement %in% choix) input$croisement else "aucun"
    updateSelectInput(session, "croisement",
                      choices = c("Aucun" = "aucun", choix),
                      selected = selection)
  })

  # Bouton « Réinitialiser » : toutes les modalités sont recochées
  observeEvent(input$reinitialiser, {
    walk(CRITERES, \(col) updateCheckboxGroupInput(
      session, paste0("filtre_", col), selected = MODALITES[[col]]
    ))
    updateCheckboxInput(session, "exclure_atypiques", value = FALSE)
  })

  # ---------------------------------------------------------------------------
  # 2. Données réactives
  # ---------------------------------------------------------------------------

  # Données filtrées selon les cases cochées
  donnees_filtrees <- reactive({
    # Une case vide renvoie NULL : on le remplace par character(0)
    # (= aucune modalité retenue) avec %||% (purrr)
    filtres <- map(CRITERES, \(col) input[[paste0("filtre_", col)]] %||% character(0)) |>
      set_names(CRITERES)

    filtrer_donnees(DONNEES, filtres = filtres,
                    exclure_atypiques = input$exclure_atypiques)
  })

  # Croisement éventuel (NULL si « Aucun »)
  croisement <- reactive({
    if (is.null(input$croisement) || input$croisement == "aucun" ||
        input$croisement == input$critere) NULL else input$croisement
  })

  # Données non vides : sinon, message à la place des graphiques
  donnees_valides <- reactive({
    d <- donnees_filtrees()
    validate(need(nrow(d) > 0,
                  "Aucun salarié ne correspond aux filtres sélectionnés."))
    d
  })

  resume <- reactive({
    resumer_salaires(donnees_valides(), input$indicateur, input$critere)
  })

  resume_croise <- reactive({
    resumer_salaires(donnees_valides(), input$indicateur, input$critere,
                     croisement = croisement())
  })

  test <- reactive({
    tester_difference(donnees_valides(), input$indicateur, input$critere,
                      methode = input$methode)
  })

  # ---------------------------------------------------------------------------
  # 3. Indicateurs clés (value boxes)
  # ---------------------------------------------------------------------------

  output$vb_effectif <- renderText(nrow(donnees_filtrees()))
  output$vb_effectif_detail <- renderText(
    paste0("sur ", N_TOTAL, " salariés avec contrat et salaire")
  )
  # Les value boxes affichent « – » si aucun salarié n'est sélectionné
  output$vb_mediane <- renderText({
    d <- donnees_filtrees()
    if (nrow(d) == 0) "–" else format_euros(median(d[[input$indicateur]], na.rm = TRUE))
  })
  output$vb_moyenne <- renderText({
    d <- donnees_filtrees()
    if (nrow(d) == 0) "–" else format_euros(mean(d[[input$indicateur]], na.rm = TRUE))
  })
  output$vb_indicateur <- renderText(names(INDICATEURS)[INDICATEURS == input$indicateur])
  output$vb_ecart <- renderText({
    if (nrow(donnees_filtrees()) == 0) return("Aucun salarié sélectionné")
    # Comme pour le test : on ignore « Non renseigné » et les groupes < 3 salariés
    r <- resume() |> filter(groupe != "Non renseigné", effectif >= 3)
    if (nrow(r) < 2) return("Un seul groupe comparable")
    # Affiché sous la boîte « Salaire médian »
    paste0("Écart entre groupes : ",
           format_euros(max(r$mediane) - min(r$mediane)))
  })
  # Réponse courte (« Oui » / « Non ») à la question posée par le titre de la
  # boîte : elle tient sur une seule ligne, quelle que soit la largeur d'écran
  output$vb_test <- renderText({
    if (nrow(donnees_filtrees()) == 0) return("–")
    t <- test()
    if (!t$ok) "–"
    else if (t$significatif) "Oui"
    else "Non"
  })
  output$vb_pvalue <- renderText({
    if (nrow(donnees_filtrees()) == 0) return("Aucun salarié sélectionné")
    t <- test()
    if (!t$ok) "Pas assez de groupes" else
      # "\u00a0" = espace insécable : les deux-points restent collés à « p-value »
      paste0(libelle(input$critere), " – p-value\u00a0: ", format_pvalue(t$p_value))
  })

  # ---------------------------------------------------------------------------
  # 4. Graphiques
  # ---------------------------------------------------------------------------

  output$titre_graphique <- renderText({
    titre <- paste(names(INDICATEURS)[INDICATEURS == input$indicateur],
                   "selon :", tolower(libelle(input$critere)))
    if (!is.null(croisement())) {
      titre <- paste(titre, "et", tolower(libelle(croisement())))
    }
    titre
  })

  # Variable qui porte la couleur : le critère, ou le croisement s'il existe
  var_couleur <- reactive(croisement() %||% input$critere)

  output$graph_distribution <- renderGirafe({
    graphique_distribution(donnees_valides(), input$indicateur, input$critere,
                           croisement = croisement(),
                           niveaux_couleur = MODALITES[[var_couleur()]]) |>
      rendre_interactif()
  })

  output$graph_niveaux <- renderGirafe({
    graphique_niveaux(resume_croise(), statistique = input$statistique,
                      indicateur = input$indicateur,
                      niveaux_couleur = MODALITES[[var_couleur()]]) |>
      rendre_interactif()
  })

  # ---------------------------------------------------------------------------
  # 5. Test statistique et tableau récapitulatif
  # ---------------------------------------------------------------------------

  output$resultat_test <- renderUI({
    t <- test()
    if (!t$ok) {
      return(div(class = "alert alert-secondary mb-0", t$message))
    }
    # La conclusion a la forme « verdict : explication ». On la coupe en deux
    # au niveau des deux-points (str_split_fixed, package stringr) pour mettre
    # le verdict en gras sans l'écrire deux fois.
    conclusion <- str_split_fixed(t$conclusion, "\\s:\\s", n = 2)

    tagList(
      div(class = paste("alert mb-3",
                        if (t$significatif) "alert-primary" else "alert-light border"),
          strong(conclusion[1]), br(), conclusion[2]),
      tags$dl(
        class = "row mb-0 small",
        tags$dt(class = "col-5", "Critère"),
        tags$dd(class = "col-7", libelle(input$critere)),
        tags$dt(class = "col-5", "Test"), tags$dd(class = "col-7", t$test),
        tags$dt(class = "col-5", "Hypothèse nulle (H0)"),
        tags$dd(class = "col-7", t$h0),
        tags$dt(class = "col-5", "Groupes comparés"),
        tags$dd(class = "col-7", paste0(t$k, " (", t$n, " salariés)")),
        tags$dt(class = "col-5", "Statistique"),
        tags$dd(class = "col-7", format_nombre(t$statistique, 2)),
        tags$dt(class = "col-5", "p-value"),
        tags$dd(class = "col-7", format_pvalue(t$p_value))
      ),
      if (length(t$exclus) > 0 || "Non renseigné" %in% resume()$groupe) {
        p(class = "small text-muted mt-2 mb-0",
          "Exclus du test : « Non renseigné » et groupes de moins de 3 salariés.")
      },
      p(class = "small text-muted mt-2 mb-0",
        "Un écart significatif ne prouve pas une discrimination : d'autres
        facteurs non disponibles (poste, ancienneté, diplôme…) peuvent
        l'expliquer.")
    )
  })

  output$table_resume <- renderTable({
    resume() |>
      # Montants mis au format « 2 540 € » (across = même fonction sur
      # plusieurs colonnes)
      mutate(across(c(mediane, moyenne, q1, q3), format_euros)) |>
      # Minimum et maximum ne sont plus affichés : avec 8 colonnes, le tableau
      # dépassait de sa carte. Les salaires extrêmes restent visibles sur le
      # graphique. Pour les remettre : les ajouter dans across() et transmute(),
      # et passer align à "lrrrrrrr".
      transmute(
        Groupe = groupe, Effectif = effectif,
        `Médiane` = mediane, Moyenne = moyenne, Q1 = q1, Q3 = q3
      )
  }, striped = TRUE, hover = TRUE, spacing = "s", width = "100%",
     align = "lrrrrr")

  # ---------------------------------------------------------------------------
  # 6. Page Profil des salariés
  # ---------------------------------------------------------------------------

  # Part (en %) et effectif d'une modalité dans le périmètre filtré
  part_modalite <- function(colonne, modalite) {
    d <- donnees_filtrees()
    n <- sum(d[[colonne]] == modalite, na.rm = TRUE)
    list(n = n, pct = if (nrow(d) == 0) NA else 100 * n / nrow(d))
  }
  afficher_pct <- function(x) {
    if (is.na(x$pct)) "–" else paste0(format_nombre(x$pct), "\u00a0%")
  }
  afficher_n <- function(x, libelle_n) paste(x$n, libelle_n)

  output$pf_effectif <- renderText(nrow(donnees_filtrees()))
  output$pf_femmes   <- renderText(afficher_pct(part_modalite("sexe", "Femme")))
  output$pf_femmes_n <- renderText(afficher_n(part_modalite("sexe", "Femme"), "femmes"))
  output$pf_cdi      <- renderText(afficher_pct(part_modalite("contrat", "CDI")))
  output$pf_cdi_n    <- renderText(afficher_n(part_modalite("contrat", "CDI"), "salariés en CDI"))
  output$pf_partiel  <- renderText(afficher_pct(part_modalite("temps_travail", "Temps partiel")))
  output$pf_partiel_n <- renderText(afficher_n(part_modalite("temps_travail", "Temps partiel"),
                                               "salariés à temps partiel"))

  output$titre_histogramme <- renderText(
    paste("Distribution du", tolower(libelle(input$indicateur)),
          "(classes de 500 €)")
  )

  output$graph_histogramme <- renderGirafe({
    graphique_histogramme(donnees_valides(), input$indicateur) |>
      rendre_interactif(hauteur = 4.2)
  })

  output$graph_profil <- renderGirafe({
    calculer_profil(donnees_valides()) |>
      graphique_profil() |>
      rendre_interactif(hauteur = 5.2)
  })

  # ---------------------------------------------------------------------------
  # 7. Page Données : tableau détaillé et téléchargement
  # ---------------------------------------------------------------------------

  tableau_export <- reactive({
    donnees_filtrees() |>
      transmute(
        `Identifiant` = id, Sexe = sexe, `Âge` = age,
        `Tranche d'âge` = tranche_age, `État civil` = etat_civil,
        Enfants = enfants, Contrat = contrat,
        `Durée hebdo (h)` = duree_hebdo, `Temps de travail` = temps_travail,
        `Salaire mensuel` = salaire, `Salaire ETP` = salaire_etp,
        `Salaire atypique` = if_else(salaire_atypique, "Oui", "Non")
      )
  })

  output$table_donnees <- renderTable({
    tableau_export() |>
      mutate(across(c(`Salaire mensuel`, `Salaire ETP`),
                    \(x) format_euros(x, decimales = 2)))
  }, striped = TRUE, hover = TRUE, spacing = "xs", width = "100%",
     na = "–", digits = 0)

  output$telecharger <- downloadHandler(
    filename = function() paste0("salaries_rh_", Sys.Date(), ".csv"),
    content = function(fichier) {
      # CSV « à la française » (séparateur ; et virgule décimale), lisible
      # directement par Excel : fonction write_excel_csv2() du package readr
      write_excel_csv2(tableau_export(), fichier, na = "")
    }
  )

  # ---------------------------------------------------------------------------
  # 8. Page À propos (méthodologie)
  # ---------------------------------------------------------------------------

  output$table_journal <- renderTable(
    JOURNAL |> rename(`Étape` = etape, `Détail` = detail, Lignes = lignes),
    striped = TRUE, hover = TRUE, spacing = "s", width = "100%"
  )
}
