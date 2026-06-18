# deploy_production.R
# Objectif : Exécuter le script SQL de déploiement sur la base de production.

# --- Chargement des librairies ---
library(DBI)
library(RPostgres)
library(readr)
library(dotenv)
library(here)

# --- Configuration ---
load_dot_env(file = here::here(".Renviron")) 

# --- Connexion et exécution ---
cat("Connexion à la base de données de production...\n")
con_prod <- NULL

tryCatch({
  
  con_prod <- dbConnect(
    RPostgres::Postgres(),
    host = Sys.getenv("DB_HOST_PROD"),
    port = as.integer(Sys.getenv("DB_PORT_PROD")),
    dbname = Sys.getenv("DB_NAME_PROD"),
    user = Sys.getenv("DB_USER_PROD"),
    password = Sys.getenv("DB_PASS_PROD")
  )
  
  # 1. Lire le contenu du fichier SQL
  sql_path <- here::here("utils", "deploy.sql")
  if (!file.exists(sql_path)) {
    stop("Le fichier SQL de déploiement est introuvable : ", sql_path)
  }
  
  cat("Lecture du fichier SQL...\n")
  sql_content <- readr::read_file(sql_path)
  
  # 2. Séparer le contenu en commandes individuelles
  #    On sépare par ';' suivi d'un éventuel retour à la ligne.
  commands <- strsplit(sql_content, ";\\s*\\n")[[1]]
  
  # 3. Démarrer la transaction
  dbBegin(con_prod)
  
  cat("Exécution des commandes de déploiement dans une transaction...\n")
  
  # 4. Exécuter chaque commande individuellement
  for (cmd in commands) {
    clean_cmd <- trimws(cmd)
    # Ignorer les lignes vides ou les commentaires
    if (nchar(clean_cmd) > 0 && !startsWith(clean_cmd, "--")) {
      cat(paste(" -> Exécution:", substr(cmd, 1, 70), "...\n"))
      dbExecute(con_prod, cmd)
    }
  }
  
  # 5. Si tout s'est bien passé, valider la transaction
  dbCommit(con_prod)
  
  cat("\n✅ Déploiement terminé et transaction validée avec succès !\n")
  
}, error = function(e) {
  cat("\nERREUR : Le déploiement a échoué. Annulation de la transaction (ROLLBACK).\n")
  print(e$message)
  
  # 6. En cas d'erreur, annuler la transaction
  if (!is.null(con_prod) && dbIsValid(con_prod)) {
    dbRollback(con_prod)
  }
  
}, finally = {
  if (!is.null(con_prod) && dbIsValid(con_prod)) {
    cat("Fermeture de la connexion.\n")
    dbDisconnect(con_prod)
  }
})