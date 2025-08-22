-- deploy.sql
-- Script pour mettre en production les nouvelles tables
-- préparées dans le schéma "preparation".

-- Début de la transaction
BEGIN;

-- --- Table: temperatures_max ---
ALTER TABLE public.temperatures_max RENAME CONSTRAINT temperatures_max_pkey TO temperatures_max_pkey_old;
ALTER TABLE public.temperatures_max RENAME CONSTRAINT uc_temperatures_max_ville_date TO uc_temperatures_max_ville_date_old;
ALTER TABLE public.temperatures_max RENAME TO temperatures_max_old;
ALTER TABLE preparation.temperatures_max SET SCHEMA public;

-- --- Table: stats_normales ---
ALTER TABLE public.stats_normales RENAME CONSTRAINT stats_normales_pkey TO stats_normales_pkey_old;
ALTER TABLE public.stats_normales RENAME CONSTRAINT uc_stats_normales_ville_jour_periode TO uc_stats_normales_ville_jour_periode_old;
ALTER INDEX public.idx_stats_ville_periode RENAME TO idx_stats_ville_periode_old;
ALTER TABLE public.stats_normales RENAME TO stats_normales_old;
ALTER TABLE preparation.stats_normales SET SCHEMA public;

-- --- Table: quiz_data_precalculee ---
ALTER TABLE public.quiz_data_precalculee RENAME CONSTRAINT quiz_data_precalculee_pkey TO quiz_data_precalculee_pkey_old;
ALTER TABLE public.quiz_data_precalculee RENAME CONSTRAINT uc_quiz_data_ville_date_periode TO uc_quiz_data_ville_date_periode_old;
ALTER INDEX public.idx_quiz_main RENAME TO idx_quiz_main_old;
ALTER TABLE public.quiz_data_precalculee RENAME TO quiz_data_precalculee_old;
ALTER TABLE preparation.quiz_data_precalculee SET SCHEMA public;

-- Validation de toutes les opérations ci-dessus
COMMIT;

-- --- NETTOYAGE ---
-- DROP TABLE public.temperatures_max_old;
-- DROP TABLE public.stats_normales_old;
-- DROP TABLE public.quiz_data_precalculee_old;