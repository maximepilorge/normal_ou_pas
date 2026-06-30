-- ====================================================================
-- Table RUNTIME : un score par SÉRIE de quiz jouée
-- ====================================================================
-- À créer MANUELLEMENT sur la base PostgreSQL, au même titre qu'analytics_visits.
-- N'est PAS gérée par le pipeline utils/ (transferer_data.R / deployer_production.R
-- DROP/recréent les tables « données » et effaceraient les scores accumulés).
-- Idempotent : exécutable plusieurs fois sans risque.
--
--   psql "$DATABASE_URL" -f utils/quiz_series_scores.sql
--
-- L'app dégrade proprement tant que la table n'existe pas (global.R :
-- quiz_scores_disponibles <- table_existe("quiz_series_scores")) : elle
-- n'enregistre simplement pas les scores et masque le « meilleur score ».
-- ====================================================================

CREATE TABLE IF NOT EXISTS quiz_series_scores (
  id            BIGSERIAL    PRIMARY KEY,
  visitor_id    TEXT         NOT NULL,                 -- même cookie UUID qu'analytics_visits
  played_ts     TIMESTAMPTZ  NOT NULL DEFAULT now(),   -- fin de série (score figé)
  score         SMALLINT     NOT NULL CHECK (score >= 0),
  nb_questions  SMALLINT     NOT NULL DEFAULT 10 CHECK (nb_questions > 0),
  periode_ref   TEXT         NOT NULL,
  ville_filtre  TEXT         NOT NULL DEFAULT 'Toutes les villes',
  saison_filtre TEXT         NOT NULL DEFAULT 'Toutes les saisons',
  device_type   TEXT,
  duree_seconds INTEGER      CHECK (duree_seconds IS NULL OR duree_seconds >= 0),
  CONSTRAINT chk_score_le_nb CHECK (score <= nb_questions)
);

-- Lecture principale : meilleur score / historique d'un visiteur (récent d'abord).
CREATE INDEX IF NOT EXISTS idx_quiz_series_visitor
  ON quiz_series_scores (visitor_id, played_ts DESC);

-- Agrégats temporels (séries par jour/semaine).
CREATE INDEX IF NOT EXISTS idx_quiz_series_played_ts
  ON quiz_series_scores (played_ts);
