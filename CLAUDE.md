# CLAUDE.md

Conventions R / Shiny transverses (nommage, modules, patterns réactifs) :
@~/.claude/r-shiny.md

## Projet

- **Nom** : "Climat : Normal ou pas ?"
- App R Shiny interactive explorant les données climatiques ERA5-Land pour 30 villes françaises (1950-2025)
- **Stack** : R 4.4, Shiny, ggplot2, plotly, bslib (Bootstrap 5), PostgreSQL (pool/DBI/RPostgres/dbplyr)
- **Repo** : https://github.com/maximepilorge/normal_ou_pas

## Architecture

- Structure classique Shiny : `global.R` (init + utils), `ui.R` (interface), `server.R` (orchestration)
- 4 modules Shiny dans `modules/` :
  - `mod_quiz.R` — Quiz en **séries de 10** (machine à états accueil/jeu/résultats) : paramétrage, 10 manches avec révélation + boxplot repliable, bilan (score /10, commentaire variable, meilleur score, partage), rejouer
  - `mod_comparer.R` — Comparaison année vs normale + carte des villes (fusion des anciens Comparaison + Carte)
  - `mod_jour.R` — Analyse d'un jour précis (rang, fréquence, distribution, partage)
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
  - `quiz_candidats` — Agrégat pré-calculé (min/max/normale par periode_ref, ville, jour, catégorie) pour le démarrage rapide d'une série de quiz ; dérivé de `quiz_data_precalculee` (cf. `utils/quiz_candidats.sql`). Sans lui, l'agrégation à la volée est lente en prod (~24 s).
  - `analytics_visits` — Analytics anonymes des sessions utilisateurs
  - `quiz_series_scores` — Un score par série de quiz jouée (table **runtime**, créée à la main hors pipeline : `utils/quiz_series_scores.sql` ; cf. `analytics_visits`)
- Requêtes lazy via `dbplyr`/`tbl()`, données pré-calculées pour la performance

## Conventions

> Style R, modules, patterns réactifs et format de commit : voir `~/.claude/r-shiny.md`
> (importé en tête) et le `CLAUDE.md` global. Spécificités du projet ci-dessous.

- **Langue** : code, commentaires et UI en français.
- **Performance** : données pré-calculées en BDD + index stratégiques ;
  `plotlyProxy` pour les mises à jour dynamiques sans re-render.

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
- Ne pas modifier les tables **« données »** directement (temperatures_max, stats_normales, quiz_data_precalculee…) — utiliser le pipeline `utils/`. Exception : les tables **runtime** alimentées par l'app (`analytics_visits`, `quiz_series_scores`) sont créées à la main et écrites via `dbAppendTable` — ne pas les confier au pipeline (il les écraserait)
- Ne pas ajouter de dépendances R sans vérifier la compatibilité Docker (Dockerfile)
- Les fichiers RDS dans `data/` sont des caches locaux — la source de vérité est la BDD PostgreSQL

## TODOs connus

- Fixer la barre d'onglets en haut de la page lors du scroll (`position = "fixed-top"`)
- Ajouter la comparaison multi-années sur un même graphique (onglet « Comparer »)

(Faits récemment : refonte du quiz en **séries de 10** — paramétrage, révélation
par manche, bilan /10 + commentaire variable + partage + meilleur score, table
runtime `quiz_series_scores` ; masquage de la courbe de tendance sur mobile ;
titres dynamiques ville/année dans les en-têtes de carte ; fusion Comparaison + Carte.
Branche `feat/parcours-grand-public` : permaliens URL (`?onglet=…&ville=…`),
boucle inter-onglets (bilan quiz → Évolution, journée → quiz), **défi de série**
par lien (`?defi=…`, sérialisation validée), sous-titres d'ancienneté des époques
de référence dans les menus, entrée « jour de ma naissance », carte de partage
« rayures climatiques ».)
