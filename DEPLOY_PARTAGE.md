# Partage des résultats du quiz — déploiement

Deux niveaux, indépendants.

## 1. Partage par image (actif)

Le bouton **« Partager mon résultat »** du quiz génère une image PNG 1200×630
côté serveur ([utils/render_partage.R](utils/render_partage.R)) et l'affiche dans
un **modal d'aperçu**. L'utilisateur y choisit son canal ([www/partage.js](www/partage.js)) :

- **Copier l'image** — presse-papiers (ClipboardItem ; bureau Chrome/Edge).
- **Télécharger** — toujours disponible (nécessaire pour Instagram, qui exige un
  import manuel).
- **Partager (mobile)** — Web Share API : feuille native du système (mobile,
  Safari macOS) → Instagram, WhatsApp, LinkedIn, etc. C'est le seul vrai partage
  direct vers Instagram.
- **LinkedIn / Facebook / Instagram** — ouvrent le réseau et **téléchargent**
  l'image, à importer via le bouton photo de la publication (les éditeurs de ces
  réseaux n'acceptent pas le collage d'image : upload de fichier requis).

Limites des plateformes : aucune API web ne permet de *publier* une image
directement sur LinkedIn/Facebook/Instagram depuis un bouton, ni d'y *coller* une
image dans l'éditeur. Un **aperçu social riche** (carte image dans le fil) à
partir d'un simple lien nécessite le sidecar Open Graph ci-dessous.

Rien à configurer ; fonctionne dans l'app telle quelle (contexte HTTPS requis
pour le presse-papiers et le partage natif).

## 2. Aperçu social Open Graph dynamique (optionnel, non activé)

Pour qu'un lien partagé affiche un aperçu riche (image + texte) sur les réseaux
sociaux, les meta `og:*` doivent être servies dans le HTML (les crawlers
n'exécutent pas le JS de Shiny). Le sidecar [utils/share_api.R](utils/share_api.R)
(plumber) fournit ces routes **sans état** (paramètres dans l'URL, image
régénérée à la volée) :

- `GET /s?ville=…&date=…&temp=…&normale=…&periode=…&cat=…` → page HTML + meta OG ;
- `GET /s/img.png?<mêmes params>` → l'image PNG.

### Activation

1. Définir `PUBLIC_BASE_URL` (ex. `https://normal-ou-pas.com`).
2. Lancer le sidecar en parallèle de Shiny dans le conteneur, p. ex. en
   remplaçant le `CMD` du Dockerfile par un petit launcher :
   ```sh
   Rscript -e "plumber::pr_run(plumber::pr('utils/share_api.R'), host='0.0.0.0', port=8000)" &
   Rscript -e "shiny::runApp('.', host='0.0.0.0', port=5000)"
   ```
3. Router `/s` et `/s/img.png` vers le port 8000 (règle de reverse proxy / Render),
   le reste vers Shiny (5000).
4. Faire pointer le bouton « Partager » vers `"/s?" + <params>` (au lieu du
   téléchargement) pour bénéficier de l'aperçu social.

> Laissé désactivé par défaut : ajouter un 2ᵉ process et une règle de routage doit
> être validé sur l'environnement Render avant mise en production, pour ne pas
> perturber le service Shiny existant.
