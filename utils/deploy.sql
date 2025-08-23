-- ====================================================================
-- SCRIPT DE DEPLOIEMENT ATOMIQUE (BLUE/GREEN)
-- Met en production les tables du schéma 'preparation'.
-- Stratégie :
-- 1. Nettoie l'ancien backup (_old).
-- 2. Archive la version actuelle (public -> _old).
-- 3. Déploie la nouvelle version (preparation -> public).
-- ====================================================================

-- Début de la transaction. Tout ou rien.
BEGIN;

-- --- Table: temperatures_max ---
RAISE NOTICE 'Déploiement de la table temperatures_max...';
-- 1. Supprimer l'ancienne table de backup (CASCADE supprime aussi séquences, index et contraintes associés)
DROP TABLE IF EXISTS public.temperatures_max_old CASCADE;
-- 2. Archiver la table de production actuelle en la renommant (si elle existe)
ALTER TABLE IF EXISTS public.temperatures_max RENAME TO temperatures_max_old;
-- 3. Mettre en production la nouvelle table en la déplaçant du schéma 'preparation'
ALTER TABLE preparation.temperatures_max SET SCHEMA public;


-- --- Table: stats_normales ---
RAISE NOTICE 'Déploiement de la table stats_normales...';
-- 1. Supprimer l'ancienne table de backup
DROP TABLE IF EXISTS public.stats_normales_old CASCADE;
-- 2. Archiver la table de production actuelle
ALTER TABLE IF EXISTS public.stats_normales RENAME TO stats_normales_old;
-- 3. Mettre en production la nouvelle table
ALTER TABLE preparation.stats_normales SET SCHEMA public;


-- --- Table: quiz_data_precalculee ---
RAISE NOTICE 'Déploiement de la table quiz_data_precalculee...';
-- 1. Supprimer l'ancienne table de backup
DROP TABLE IF EXISTS public.quiz_data_precalculee_old CASCADE;
-- 2. Archiver la table de production actuelle
ALTER TABLE IF EXISTS public.quiz_data_precalculee RENAME TO quiz_data_precalculee_old;
-- 3. Mettre en production la nouvelle table
ALTER TABLE preparation.quiz_data_precalculee SET SCHEMA public;

RAISE NOTICE 'Déploiement terminé avec succès !';
-- Validation de toutes les opérations ci-dessus
COMMIT;