# CLAUDE.md

## Projet

- **Nom** : "Climat : Normal ou pas ?"
- App R Shiny interactive explorant les données climatiques ERA5-Land pour 30 villes françaises (1950-2025)
- **Stack** : R 4.4, Shiny, ggplot2, plotly, bslib (Bootstrap 5), PostgreSQL (pool/DBI/RPostgres/dbplyr)
- **Repo** : https://github.com/maximepilorge/normal_ou_pas

## Architecture

- Structure classique Shiny : `global.R` (init + utils), `ui.R` (interface), `server.R` (orchestration)
- 3 modules Shiny dans `modules/` :
  - `mod_quiz.R` — Quiz interactif + boxplot + horizons projetés + carte de partage
  - `mod_comparer.R` — Comparaison année vs normale + carte des villes (fusion des anciens Comparaison + Carte)
  - `mod_analyse.R` — Tendances long terme + forte chaleur vs gel (observé + projeté)
- `utils/` — Pipeline données (téléchargement ERA5 → préparation → transfert → déploiement) + utilitaires partagés purs (`helpers.R`, `villes_reference.R`, `fenetre_glissante.R`, `render_partage.R`)
- `tests/` — Tests `testthat` des fonctions pures (`Rscript tests/run_tests.R`)
- `data/` — Fichiers RDS locaux (cache), `www/` — Assets web (social_preview.png)
- Voir `README.md` pour l'onboarding détaillé (architecture, schéma des tables, pipeline).

## Base de données

- PostgreSQL via pool de connexions initialisé dans `global.R`
- Credentials dans `.Renviron` : `DB_HOST`, `DB_PORT`, `DB_NAME`, `DB_USER`, `DB_PASS` (+ variantes `_PROD`)
- Tables principales :
  - `temperatures_max` — Températures journalières par ville (PK: ville, date)
  - `stats_normales` — Normales climatiques pré-calculées par période de référence (PK: ville, mois, jour_mois, periode_ref)
  - `quiz_data_precalculee` — Questions quiz pré-calculées avec catégories (PK: ville, date, periode_ref)
  - `analytics_visits` — Analytics anonymes des sessions utilisateurs
- Requêtes lazy via `dbplyr`/`tbl()`, données pré-calculées pour la performance

## Conventions

- **Langue** : code, commentaires et UI en français
- **Modules** : fichiers `mod_<nom>.R` avec fonctions `mod_<nom>_ui()` et `mod_<nom>_server()`
- **Indentation** : 2 espaces
- **Commits** : format `(type): Description` en français (ex: `(fix): Correction quiz`, `(feat): Ajout fonctionnalité`)
- **Patterns réactifs** : `reactiveVal` pour l'état mutable, `reactive()` avec `bindCache()` pour les calculs coûteux, `observeEvent` pour les actions utilisateur
- **Performance** : `plotlyProxy` pour les mises à jour dynamiques sans re-render, données pré-calculées en BDD, index stratégiques

## Déploiement

- **Docker** : image `rocker/shiny:4.4`, port 5000
- **Production** : Render.com (base PostgreSQL managée)
- **Pipeline données** : `utils/telecharger_data.R` → `preparer_data.R` → `transferer_data.R` → `deployer_production.R` (+ `deploy.sql`)

## Commandes utiles

```bash
# Lancer l'app localement
Rscript -e "shiny::runApp(host='0.0.0.0', port=5000)"

# Docker
docker build -t normal_ou_pas .
docker run -p 5000:5000 --env-file .Renviron normal_ou_pas

# Vérifier les données
Rscript utils/verifier_donnees_stats_normales.R
Rscript utils/verifier_extraction_cds.R
```

## Ne pas faire

- Ne jamais committer `.Renviron` (contient les secrets BDD et clé API CDS)
- Ne pas modifier les tables BDD directement — utiliser le pipeline `utils/`
- Ne pas ajouter de dépendances R sans vérifier la compatibilité Docker (Dockerfile)
- Les fichiers RDS dans `data/` sont des caches locaux — la source de vérité est la BDD PostgreSQL

## TODOs connus

- Fixer la barre d'onglets en haut de la page lors du scroll (`position = "fixed-top"`)
- Ajouter la comparaison multi-années sur un même graphique (onglet « Comparer »)

(Faits récemment : masquage de la courbe de tendance sur mobile ; titres dynamiques
ville/année dans les en-têtes de carte ; fusion Comparaison + Carte.)
