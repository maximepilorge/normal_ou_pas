-- ====================================================================
-- quiz_candidats : agrégat pré-calculé pour le démarrage d'une série de quiz
-- ====================================================================
-- Dérivé de quiz_data_precalculee : pour chaque (periode_ref, ville, jour
-- calendaire, catégorie), les bornes min/max de la tmax observée et la normale.
-- ~165k lignes (léger) contre ~4 M lignes dans quiz_data_precalculee.
--
-- POURQUOI : « Lancer la série » (Toutes villes / Toutes saisons) agrégeait
-- ~832k lignes à la volée (Seq Scan d'une table de 1,4 Go) -> ~24 s sur la base
-- managée de production. Ici l'app ne fait plus qu'une lecture filtrée/indexée
-- (< 1 s). Le passage d'une question à l'autre n'était pas concerné (petites
-- requêtes indexées).
--
-- À (RE)CONSTRUIRE après tout (re)déploiement de quiz_data_precalculee :
--   psql "$DATABASE_URL" -f utils/quiz_candidats.sql
-- (Le pipeline le régénère aussi : preparer_data.R / recalculer_normales_depuis_bdd.R
--  en local, deployer_production.R sur la prod.)
--
-- L'app dégrade proprement si la table est absente (global.R :
-- quiz_candidats_disponibles) : elle retombe sur l'agrégation à la volée.
-- Idempotent.
-- ====================================================================

DROP TABLE IF EXISTS public.quiz_candidats;

CREATE TABLE public.quiz_candidats AS
SELECT periode_ref, ville, mois, jour_mois, categorie,
       MIN(tmax_celsius) AS min_temp,
       MAX(tmax_celsius) AS max_temp,
       MIN(t_moy)        AS normale_moy
FROM public.quiz_data_precalculee
GROUP BY periode_ref, ville, mois, jour_mois, categorie;

-- Couvre les filtres de charger_candidats_quiz() : periode_ref (+ ville, + mois).
CREATE INDEX idx_quiz_candidats ON public.quiz_candidats (periode_ref, ville, mois);

VACUUM ANALYZE public.quiz_candidats;
