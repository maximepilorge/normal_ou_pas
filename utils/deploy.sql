-- ====================================================================
-- SCRIPT DE DEPLOIEMENT
-- Met en production les tables du schéma 'preparation'.
-- Stratégie :
-- 1. Nettoie l'ancien backup (_old).
-- 2. Archive la version actuelle (public -> _old).
-- 3. Déploie la nouvelle version (preparation -> public).
-- ====================================================================
-- Note : la transaction (BEGIN/COMMIT/ROLLBACK) est gérée par
-- deployer_production.R, pas ici, pour éviter un double commit.

-- --- Table: temperatures_max ---
DROP TABLE IF EXISTS public.temperatures_max;
ALTER TABLE preparation.temperatures_max SET SCHEMA public;

-- --- Table: stats_normales ---
DROP TABLE IF EXISTS public.stats_normales CASCADE;
ALTER TABLE preparation.stats_normales SET SCHEMA public;


-- --- Table: quiz_data_precalculee ---
DROP TABLE IF EXISTS public.quiz_data_precalculee;
ALTER TABLE preparation.quiz_data_precalculee SET SCHEMA public;

-- --- Table: indicateurs_annuels ---
DROP TABLE IF EXISTS public.indicateurs_annuels;
ALTER TABLE preparation.indicateurs_annuels SET SCHEMA public;

-- --- Table: canicules ---
DROP TABLE IF EXISTS public.canicules;
ALTER TABLE preparation.canicules SET SCHEMA public;