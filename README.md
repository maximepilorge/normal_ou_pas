# Climat : Normal ou pas ?

Application **R Shiny** interactive qui sensibilise le grand public au changement
climatique en montrant à quel point nos repères de température sont décalés par
rapport au climat passé. Elle s'appuie sur les données de réanalyse **ERA5-Land**
(1950 → aujourd'hui) pour 30 villes françaises, et sur les projections **DRIAS /
Explore2** alignées sur la trajectoire **TRACC** (+2,7 °C en 2050, +4 °C en 2100).

🔗 Production : <https://normal-ou-pas.com> · Dépôt : <https://github.com/maximepilorge/normal_ou_pas>

---

## Ce que fait l'application

Quatre onglets, en montée pédagogique :

| Onglet | Module | Idée |
|---|---|---|
| **Le Quiz** | `modules/mod_quiz.R` | **Séries de 10 questions** : on paramètre la série (période, ville, saison), on répond à 10 températures (sous / dans / au-dessus des normales) avec révélation immédiate + boxplot repliable (repères présent / 2050 / 2100), puis un bilan affiche le score /10, un commentaire variable, le meilleur score, une carte de série partageable (PNG) et un **défi** : un lien `?defi=…` qui fait rejouer exactement la même série à un ami, score à battre inclus. |
| **Comparaison** | `modules/mod_comparer.R` | « Dans l'année » : courbe d'une année vs la normale. « Entre les villes » : carte Leaflet des écarts à une normale ancienne + trajectoire d'une ville, avec curseur d'année. L'époque de référence (réglage d'expert) est repliée dans « Réglages avancés ». |
| **Une journée** | `modules/mod_jour.R` | Analyse d'un jour précis (ville + date) : valeur, écart à la normale, rang (« jour le plus chaud autour du… depuis 1950 ») et fréquence, distribution ±7 j, carte partageable. Entrée « **jour de votre naissance** » : réchauffement vécu depuis (moyennes sur 15 ans). |
| **Évolution** | `modules/mod_analyse.R` | Écart annuel à la normale (barres), analyse du réchauffement sur 30 ans, jours de forte chaleur vs jours de gel (observé + projeté TRACC), carte partageable « **rayures climatiques** » de la ville. |
| **Méthodo** | `ui.R` (inline) | Sources, définition des normales (percentiles, fenêtre ±7 j), méthode des projections, limites. |

Transverse : l'état de l'app vit dans l'URL (**permaliens** `?onglet=…&ville=…&date=…`,
copiables à tout moment) ; les onglets se renvoient l'un à l'autre (bilan du quiz →
Évolution, Une journée → Quiz, **année cliquée** dans le graphe d'Évolution →
Comparaison « Dans l'année ») ; les périodes de référence portent un sous-titre
d'ancienneté dans les menus (« 1951-1980 — il y a environ 60 ans »).

---

## Architecture

Structure Shiny classique. `global.R` est chargé **automatiquement** avant `ui.R`
et `server.R`.

```
global.R            Init : pool BDD, sourcing utils, listes dynamiques (villes,
                    périodes, bornes d'années), anomalies pré-calculées (carte).
ui.R                Interface (page_navbar 4 onglets), CSS responsive, JS cookies.
server.R            Orchestration des modules + analytics de session anonymes.
modules/            mod_quiz.R, mod_comparer.R, mod_analyse.R (mod_<nom>_ui/_server).
utils/              Pipeline de données + utilitaires partagés (voir plus bas).
www/                Assets web (partage.js, social_preview.png).
data/               Caches RDS locaux (gitignorés ; la BDD est la source de vérité).
docs/               Notes de conception (plan projections TRACC).
```

### Utilitaires partagés (`utils/`, sans effet de bord, sourçables)
- `helpers.R` — `get_season_info()`, `largeur_sous_seuil()` (détection petit écran),
  `log_debug()` (logs de debug conditionnels), permaliens (`construire_query_string`),
  défi de série (`serialiser_defi`/`deserialiser_defi`), `soustitres_periodes()`
  (ancienneté des normales) et `rechauffement_depuis()` (naissance, rayures).
  Sourcé par `global.R`.
- `villes_reference.R` — **source de vérité unique** des 30 villes (nom, lat/lon, INSEE).
- `fenetre_glissante.R` — machinerie de fenêtre ±N jours, partagée entre la
  préparation des normales et le calcul des projections (définition identique).
- `render_partage.R` — génération des cartes de partage en ggplot pur : manche de
  quiz, journée précise, rayures climatiques d'une ville.

---

## Base de données (PostgreSQL)

Connexion par pool (`global.R`), credentials dans `.Renviron`. Variantes `_PROD`
pour la base de production.

| Table | Clé | Contenu |
|---|---|---|
| `temperatures_max` | (ville, date) | Tmax/tmin journalières par ville. |
| `stats_normales` | (ville, mois, jour_mois, periode_ref) | Normales pré-calculées par période de référence (moy, quartiles, seuils p10/p90 lissés ±7 j). |
| `quiz_data_precalculee` | (ville, date, periode_ref) | Questions du quiz avec catégorie (sous/dans/au-dessus). |
| `quiz_candidats` | (periode_ref, ville, mois, jour_mois, categorie) | Agrégat pré-calculé (min/max/normale) dérivé de `quiz_data_precalculee`, pour le démarrage rapide d'une série (évite d'agréger ~4 M lignes à chaque « Lancer la série »). Reconstruit par le pipeline / `utils/quiz_candidats.sql`. |
| `indicateurs_annuels` | (ville, annee) | Jours de gel, jours de forte chaleur, records, etc. *(table optionnelle)* |
| `stats_normales_projetees` | (ville, mois, jour_mois) | Deltas TRACC par niveau de réchauffement (quiz). *(optionnelle)* |
| `extremes_projetes` | (ville, niveau) | Jours de forte chaleur / gel projetés. *(optionnelle)* |
| `analytics_visits` | — | Sessions anonymes (durée, appareil, score quiz). |
| `quiz_series_scores` | — | Un score par série de quiz jouée. Table **runtime** créée à la main (`utils/quiz_series_scores.sql`), hors pipeline, comme `analytics_visits`. |

Les tables marquées *optionnelles* sont testées au démarrage (`table_existe()`) :
en leur absence, les onglets concernés affichent un message d'indisponibilité au
lieu de planter. Requêtes lazy via `dbplyr`/`tbl()`.

---

## Lancer en local

Prérequis : R ≥ 4.4, une base PostgreSQL alimentée (voir pipeline), un fichier
`.Renviron` à la racine :

```
DB_HOST=...        DB_PORT=5432   DB_NAME=...   DB_USER=...   DB_PASS=...
DB_HOST_PROD=...   DB_PORT_PROD=...  DB_NAME_PROD=...  DB_USER_PROD=...  DB_PASS_PROD=...
CDSAPI_KEY=...     # clé Copernicus, pour le pipeline de téléchargement uniquement
```

```bash
Rscript -e "shiny::runApp(host='0.0.0.0', port=5000)"
# Logs de debug du quiz : options(normaloupas.debug = TRUE) ou NORMALOUPAS_DEBUG=1
```

### Docker

```bash
docker build -t normal_ou_pas .
docker run -p 5000:5000 --env-file .Renviron normal_ou_pas
```

Le `Dockerfile` ne copie de `utils/` que ce qui est nécessaire au runtime
(`render_partage.R`, `helpers.R`, `villes_reference.R`) ; le reste de `utils/`
est le pipeline, exécuté hors conteneur.

---

## Pipeline de données (`utils/`, hors conteneur)

Exécuté manuellement lors d'une mise à jour des données. Chaîne principale :

```
telecharger_data.R     ERA5-Land via l'API Copernicus (CDS) → max journalier par
                       maille → moyenne spatiale pondérée par commune → data/*.rds
   ↓
preparer_data.R        Normales (fenêtre ±7 j), table quiz, indicateurs annuels →
                       écrit les tables dans la base locale.
   ↓
transferer_data.R      Copie les tables locales vers le schéma 'preparation' (prod).
   ↓
deployer_production.R  Promotion transactionnelle 'preparation' → 'public'.
```

Volet **projections TRACC** (optionnel) :

```
telecharger_drias.R → calculer_projections.R → deployer_projections.R
```

> ⚠️ Ne pas modifier les tables directement : passer par le pipeline. La liste des
> villes se modifie en un seul endroit (`utils/villes_reference.R`).

Scripts de vérification / QA : `utils/verifier_*.R`, `utils/inspecter_donnees.R`.

---

## Tests

Tests unitaires des fonctions pures (`testthat`) dans `tests/testthat/` :

```bash
Rscript tests/run_tests.R
```

Couvrent : saison d'une date, fenêtre glissante, seuil de forte chaleur,
indicateurs annuels, les cartes de partage (quiz, journée, rayures), les
permaliens, la (dé)sérialisation du défi de série, les sous-titres d'époques et
le réchauffement depuis une année. Pas de dépendance à la BDD.

---

## Conventions

- **Langue** : code, commentaires et UI en français.
- **Modules** : `mod_<nom>.R` exposant `mod_<nom>_ui()` et `mod_<nom>_server()`.
- **Indentation** : 2 espaces.
- **Commits** : `(type): Description` en français (ex. `(fix): …`, `(feat): …`).
- **Réactivité** : `reactiveVal` pour l'état, `reactive()` + `bindCache()` pour les
  calculs coûteux, `observeEvent` pour les actions, `plotlyProxy`/`leafletProxy`
  pour les mises à jour sans re-render.

Voir `CLAUDE.md` pour les détails et les TODOs connus.
