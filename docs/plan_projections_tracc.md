# Plan d'implémentation — Projections TRACC dans l'app

> Statut : plan validé (décisions clés actées le 2026-06-24). Reste à confirmer
> les fenêtres temporelles exactes par niveau de réchauffement (cf. §11).

## 1. Objectif

Donner à voir l'évolution **future** des températures aux horizons TRACC
(**+2,7 °C / 2050** et **+4,0 °C / 2100**) à deux endroits de l'app :

- **Quiz** (`mod_quiz.R`) : superposer au boxplot de feedback la moyenne de
  saison projetée + déplacer la bande « normal » (p10/p90) vers sa position
  future, avec un récit (« cette température sera dans la normale vers 2050 »).
- **Graphique canicules** (`mod_analyse.R`) : ajouter les jours de canicule
  projetés par horizon, à côté des barres observées par décennie.

## 2. Principe directeur

Deux choix méthodo structurants, **différents selon la feature** — volontairement.

| Feature | Méthode | Pourquoi |
|---|---|---|
| Quiz (normales, p10/p90) | **Delta / change factor** : `normale_ERA5 + (DRIAS_futur − DRIAS_réf)` | On superpose sur les normales ERA5 observées → la continuité prime. Un décalage de moyenne se transporte bien par delta. |
| Canicules (seuils absolus) | **Séries journalières DRIAS absolues** (corrigées ADAMONT) passées dans `calculer_canicules()` | Le comptage de franchissement de seuils dépend de la **variabilité** et de la **persistance** des séries, que le delta plat n'introduit pas. ADAMONT est débiaisé précisément pour cet usage. *(Décision actée : Absolu DRIAS.)* |

Dans les deux cas : réutiliser le **fenêtrage ±7 j existant**
(`preparer_data.R:76-131`) et **pré-calculer en BDD** — aucun calcul lourd dans l'app.

## 3. Données sources — DRIAS / TRACC

- **Produit** : projections journalières **corrigées ADAMONT** (grille 8 km,
  débiaisées SAFRAN). À confirmer au prototypage : ensemble **DRIAS-2020**
  (12 simulations) vs **Explore2-ADAMONT** (17 simulations) — la doc TRACC
  référence les deux selon les produits.
- **Variables** : `tasmax` (quiz + canicules) et `tasmin` (canicules).
- **Approche** : **par niveau de réchauffement** (Global Warming Level), PAS par
  scénario-horizon. RCP8.5 sert uniquement de **réservoir de simulations** pour
  échantillonner le climat à chaque niveau (seul scénario atteignant +3 °C
  mondial dans la période simulée, ensemble le plus fourni ; hypothèse
  d'indépendance au scénario à niveau donné). La **trajectoire de référence**
  TRACC (~+3 °C mondial / +4 °C France en 2100) ≈ tendanciel, ce n'est PAS
  « RCP8.5 à 2100 » (qui serait +4-5 °C mondial). Le climat de chaque niveau est
  extrait de la **fenêtre 20-30 ans où chaque modèle franchit le niveau**.
- ⚠️ **UI/disclaimers** : ne PAS écrire « scénario pessimiste RCP8.5 ». Parler de
  « niveau de réchauffement +2,7 °C / +4 °C — trajectoire de référence TRACC ».
  Sinon on induit la confusion +4 °C France vs +4 °C mondial.
- **Ensemble** : **médiane d'ensemble** (lisibilité ; fourchette min/max possible en v2).
- **Période de référence historique TRACC** : **1976-2005** (≠ 1991-2020).
  ⚠️ Impact delta : voir §11.3.
- **Fenêtres par niveau de réchauffement** : centrées sur l'atteinte du niveau
  (~20-30 ans). Valeurs exactes à figer depuis la doc technique DRIAS (§11.1).
- **Extraction spatiale** : maille 8 km la plus proche de chaque ville.
  Réutiliser l'infra `sf` + pondération surface communale de
  `telecharger_data.R` / `definir_mailles_communes.R`.

## 4. Ré-étude du graphique des vagues de chaleur ⭐

Point décisif rendu possible par le **journalier** : `calculer_canicules()`
(`calculer_indicateurs.R:73`) est une **fonction pure** prenant
`(donnees journalières tmin/tmax, villes_insee, seuils départementaux)`.

> On applique **exactement la même définition officielle Météo-France**
> (IBM 3 j glissants vs seuils départementaux) aux séries DRIAS journalières.
> **Le piège de discontinuité de définition disparaît** : observé et projeté
> comptent les canicules à l'identique. Réutilisation **verbatim** de la
> fonction, zéro réécriture de logique métier.

Plan canicules concret :
1. Extraire les séries DRIAS journalières `tasmin`/`tasmax` (médiane d'ensemble)
   sur la fenêtre de chaque niveau, pour les 30 villes.
2. Les passer telles quelles dans
   `calculer_canicules(donnees_drias, villes_insee, seuils_canicule)`.
3. Agréger en **jours de canicule moyens par décennie** (total fenêtre ÷ nb
   décennies) pour comparabilité avec les barres observées.
4. Stocker dans `canicules_projetees`, **superposer** au graphe : barres
   observées (décennies réelles) + 2 barres projetées (2050, 2100)
   **visuellement distinctes** (hachures/couleur + séparateur vertical), légende
   « Projection TRACC ».

Garde-fou (footer) : observé = ERA5-Land, projeté = DRIAS-corrigé (médiane
d'ensemble) → léger décalage de référentiel possible ; on illustre une
**tendance, pas une prévision datée**.

## 5. Modèle de données — nouvelles tables

Mirror du pattern existant (`stats_normales`, `canicules`) :

```sql
-- Quiz : delta journalier fenêtré ±7 j, par ville × jour calendaire × niveau
stats_normales_projetees(
  ville, mois, jour_mois, niveau_rechauffement,   -- "2050_+2.7" | "2100_+4.0"
  delta_t_moy, delta_p10, delta_p90               -- °C à ajouter aux normales ERA5
)  -- ~30 × 366 × 2 ≈ 22 k lignes

-- Canicules : épisodes projetés (schéma de `canicules` + niveau)
canicules_projetees(
  ville, departement, niveau_rechauffement,
  date_debut, date_fin, duree_jours, intensite_max, depassement_max
)
```

- Quiz : lit `stats_normales_projetees` comme il lit déjà les seuils
  (`mod_quiz.R:251-262`) et additionne le delta.
- Canicules : lit `canicules_projetees` en miroir de `mod_analyse.R:444-454`.

## 6. Pipeline — fichier par fichier

**Principe de séparation (préférence actée)** : `utils/telecharger_data.R`
(ERA5-Land) **reste inchangé**. Toute source **autre qu'ERA5-Land** a son propre
script de téléchargement, à responsabilité unique (un `telecharger_<source>.R`
par source ; ici DRIAS). Les helpers spatiaux communs
(`definir_mailles_communes.R`, agrégation `sf` pondérée par surface communale)
sont **sourcés**, pas dupliqués — comme le fait déjà `telecharger_data.R`.

| Étape | Fichier | Action |
|---|---|---|
| Téléchargement | `utils/telecharger_drias.R` *(nouveau, séparé d'ERA5)* | NetCDF DRIAS journaliers corrigés (tasmax/tasmin, 2 niveaux + réf historique) aux mailles des 30 villes → cache `data/drias_*.rds`. Source les helpers spatiaux communs. |
| Calcul | `utils/calculer_projections.R` *(nouveau)* | (a) deltas fenêtrés ±7 j moy/p10/p90 (réutiliser `fenetre_map`/`ref_jours`) ; (b) `calculer_canicules()` sur séries DRIAS. Fonctions pures, sourcé par `preparer_data.R`. |
| Intégration | `utils/preparer_data.R` | Étapes supplémentaires : `dbWriteTable("stats_normales_projetees")` et `dbWriteTable("canicules_projetees")`, sur le modèle des lignes 179-192. |
| Contraintes | `utils/preparer_data.R` (bloc index) | PK `uc_proj_ville_jour_niveau (ville, mois, jour_mois, niveau_rechauffement)` ; index ville sur canicules_projetees ; `VACUUM ANALYZE`. |
| Déploiement | `utils/transferer_data.R` / `deployer_production.R` | Ajouter les 2 tables à la liste transférée en prod. |
| Vérif | `utils/verifier_projections.R` *(nouveau)* | Sanity checks (cf. §8). |

## 7. Modifications de l'app

**`modules/mod_quiz.R`** (dans `observeEvent(submit_answer_btn)`) :
- Lookup `stats_normales_projetees` pour `(ville, mois, jour_mois)` × 2 niveaux.
- Construire `points_projetes` (moy + delta) ajouté au `points_specifiques`
  existant (`mod_quiz.R:347`) → 2 `geom_point` (forme/couleur dédiées).
- Optionnel fort : 2 bandes p10/p90 décalées (réutiliser le bloc
  `annotate("rect")` `mod_quiz.R:402-404`), transparence dégradée.
- Texte feedback : « Aux normales **2050 (+2,7 °C)**, cette température serait
  *dans / sous* les normales ; en **2100 (+4 °C)**, … ».

**`modules/mod_analyse.R`** :
- Nouveau `reactive canicules_projetees_ville()` calqué sur les lignes 444-454 +
  flag `projections_disponibles`.
- Dans `canicules_plot` (`mod_analyse.R:468`) : `bind_rows` observé + projeté,
  `geom_col` avec `aes(fill/linetype)` distinguant les deux, séparateur, légende.
- Mettre à jour le `card_footer` méthodo.

## 8. Garde-fous & vérifications (`verifier_projections.R`)

- Deltas dans une fourchette plausible (≈ +1 à +6 °C, plus forts l'été).
- Monotonie : projeté 2050 < projeté 2100 ; p10/p90 cohérents.
- Jours de canicule projetés ≥ observés récents.
- UI : disclaimers explicites (médiane d'ensemble, ERA5 vs DRIAS, « tendance
  pas prévision »), cohérents avec le `card_footer` existant
  (`mod_analyse.R:57-66`).
- Repli gracieux si tables absentes (pattern `etat_indisponible` /
  `canicules_disponibles` déjà en place).

## 9. Ordonnancement (lots)

1. **Lot A — Données** : `telecharger_drias.R` + `calculer_projections.R`,
   validés sur **1 ville** (chaîne bout-en-bout + sanity checks).
   - ✅ `utils/telecharger_drias.R` créé : inspecteur + lecteur NetCDF
     introspectif (layouts A régulier / B curvilinéaire / C points aplatis),
     extraction point ville + médiane d'ensemble.
   - ✅ **Validé sur données RÉELLES** (Paris, CNRM-CM5/ALADIN63 RCP8.5,
     2006-2100, ADAMONT-SAFRAN). **Format DRIAS confirmé = Layout B** : dims
     `x`/`y` **projetées (mètres, Lambert Paris II)** + variables `lat`/`lon`
     **2D** ; variable `tasmaxAdjust`/`tasminAdjust` en **Kelvin**, temps `days
     since 1950-01-01`. Le lecteur priorise les variables lat/lon sur les dims
     (sinon il prenait les mètres pour des degrés — bug corrigé).
   - ✅ **Signal climatique cohérent** : été tmax +3,5 °C (2006-2035 → 2071-2100) ;
     jours ≥30 °C 13→47/an ; nuits tropicales 12→60/an.
   - ✅ `utils/calculer_projections.R` créé et **validé end-to-end** sur la série
     Paris 1-sim : (1) `calculer_deltas_fenetres()` → deltas ±7 j moy/p10/p90 par
     ville × jour × niveau (saisonnalité capturée : été ≠ hiver, ce qui valide le
     rejet du delta annuel plat) ; (2) `calculer_canicules_projetees()` → réutilise
     `calculer_canicules()` verbatim sur séries DRIAS absolues. Résultats Paris :
     delta moyen +1,2 °C (2050) / +3,1 °C (2100) vs 2006-2035 ; canicules ~8→22 j/an.
   - 🐛 **Bug corrigé en passant** : `data/seuils_canicule_departements.csv`
     (working tree) avait perdu sa ligne d'en-tête `departement,nom_departement,smin,smax`
     → `read.csv` mis-parsait tout (cassait `calculer_canicules`, existant ET projeté).
     En-tête réintégrée. **À committer avec les edits du CSV.**
   - ⚠️ **Caveat 1-sim** : CNRM-CM5/ALADIN63 est un modèle « chaud » (pic tmax 48 °C
     en 2091) → les extrêmes/queues sont surestimés ; la médiane d'ensemble les
     atténuera fortement.
   - ⏳ Reste Lot A → B : récupérer les **autres simulations** (médiane d'ensemble),
     figer les **fenêtres officielles de niveau de réchauffement**, étendre aux
     **30 villes**, intégrer à `preparer_data.R` (écriture des 2 tables).
2. **Lot B — Pipeline complet** : 30 villes, écriture BDD, contraintes, prod.
3. **Lot C — Quiz** : points + bande projetés + texte.
4. **Lot D — Canicules** : superposition + légende + footer.
5. **Lot E — Méthodo** : section explicative TRACC (cohérente avec la section
   méthodo récente, commit `7cc3b26`).

## 10. Risques

- **Offset ERA5/SAFRAN** sur les canicules (assumé + disclaimer).
- **Delta plat** dans le quiz : pas de changement de variabilité (acceptable
  pour une moyenne, à documenter).
- **Volumétrie DRIAS** au téléchargement (NetCDF lourds) → restreindre aux
  mailles des 30 villes.
- **Dépendances Docker** : extraction NetCDF (`ncdf4`/`terra`) — `ncdf4` + `sf`
  déjà présents (cf. `telecharger_data.R`), vérifier la compat Dockerfile.

## 11. Décisions

- ✅ **Canicules : Absolu DRIAS** (séries corrigées dans `calculer_canicules()`).
- ✅ **Quiz : méthode delta** (fenêtre ±7 j).
- ✅ **Médiane d'ensemble** seule en v1.

Restant à confirmer :
1. **Fenêtres temporelles exactes** par niveau (+2,7 °C, +4,0 °C) — doc technique
   DRIAS/TRACC. Référence historique annoncée : **1976-2005**, scénario RCP8.5.
2. **Ensemble** : DRIAS-2020 (12 sim.) vs Explore2-ADAMONT (17 sim.).
3. **Référentiel du delta (quiz)** : la réf TRACC est 1976-2005, mais le quiz
   propose 1991-2020. Comme on part du **journalier brut**, recalculer le delta
   en `DRIAS(fenêtre niveau) − DRIAS(1991-2020)` pour s'aligner sur la période de
   réf. choisie dans le quiz (plutôt que de réutiliser l'anomalie pré-calculée
   vs 1976-2005).

## Sources

- TRACC — Ministère Transition écologique :
  https://www.ecologie.gouv.fr/politiques-publiques/trajectoire-rechauffement-reference-ladaptation-changement-climatique-tracc
- Climadiag Commune — Météo-France : https://meteofrance.com/climadiag-commune
- DRIAS, les futurs du climat : https://www.drias-climat.fr/
- Le climat futur de la France selon la TRACC :
  https://www.drias-climat.fr/accompagnement/section/402
- Évolution des vagues de chaleur selon la TRACC :
  https://www.drias-climat.fr/accompagnement/sections/417
- Données climat futur par degrés de réchauffement (OEB) :
  https://doc-data-oeb.readthedocs.io/fr/latest/climat/donnees_tracc_doc/
