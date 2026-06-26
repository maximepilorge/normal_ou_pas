-- ====================================================================
-- SCRIPT DE DEPLOIEMENT (RÉFÉRENCE — NON EXÉCUTÉ AUTOMATIQUEMENT)
-- ====================================================================
-- Depuis la refonte, utils/deployer_production.R est AUTONOME : il génère
-- lui-même ces commandes à partir de sa liste `tables_a_deployer` et ne lit
-- plus ce fichier. Il est conservé uniquement à titre de documentation / pour
-- un déploiement manuel.
--
-- Stratégie (par table, dans une transaction) :
--   1. Supprime l'ancienne version de production (public.<table>).
--   2. Promeut la nouvelle version (preparation.<table> -> public.<table>).
-- La table est *déplacée* : après déploiement, 'preparation' est vidé ; il faut
-- relancer transferer_data.R avant tout nouveau déploiement.
-- ====================================================================

-- --- Table: temperatures_max ---
DROP TABLE IF EXISTS public.temperatures_max CASCADE;
ALTER TABLE preparation.temperatures_max SET SCHEMA public;

-- --- Table: stats_normales ---
DROP TABLE IF EXISTS public.stats_normales CASCADE;
ALTER TABLE preparation.stats_normales SET SCHEMA public;

-- --- Table: quiz_data_precalculee ---
DROP TABLE IF EXISTS public.quiz_data_precalculee CASCADE;
ALTER TABLE preparation.quiz_data_precalculee SET SCHEMA public;

-- --- Table: indicateurs_annuels ---
DROP TABLE IF EXISTS public.indicateurs_annuels CASCADE;
ALTER TABLE preparation.indicateurs_annuels SET SCHEMA public;

-- --- Table: stats_normales_projetees (projections TRACC — quiz) ---
DROP TABLE IF EXISTS public.stats_normales_projetees CASCADE;
ALTER TABLE preparation.stats_normales_projetees SET SCHEMA public;

-- --- Table: extremes_projetes (projections TRACC — forte chaleur + gel) ---
DROP TABLE IF EXISTS public.extremes_projetes CASCADE;
ALTER TABLE preparation.extremes_projetes SET SCHEMA public;
