// www/partage.js
// Helpers du modal de partage de la carte de résultat du quiz.
// Le modal (construit côté serveur) contient l'image (#apercu-partage-img) et
// son texte d'accompagnement (#partage-zone[data-texte]).
//
// Note Firefox/Safari : l'écriture d'une image dans le presse-papiers doit se
// faire pendant l'« activation utilisateur » du clic. On évite donc tout
// fetch()/await avant clipboard.write() : le blob est construit SYNCHRONEMENT à
// partir de la data-URI. En cas d'échec ou de navigateur non compatible, on
// retombe sur le téléchargement (toujours un retour visible).

(function () {
  function dataUriToBlob(uri) {
    var parts = uri.split(",");
    var mime = (parts[0].match(/:(.*?);/) || [])[1] || "image/png";
    var bin = atob(parts[1].replace(/\s/g, ""));
    var arr = new Uint8Array(bin.length);
    for (var i = 0; i < bin.length; i++) arr[i] = bin.charCodeAt(i);
    return new Blob([arr], { type: mime });
  }

  function imgBlob() {
    var img = document.getElementById("apercu-partage-img");
    return img ? dataUriToBlob(img.src) : null;
  }

  function texte() {
    var z = document.getElementById("partage-zone");
    return z ? (z.dataset.texte || "") : "";
  }

  function toast(t) {
    var d = document.createElement("div");
    d.textContent = t;
    d.style.cssText =
      "position:fixed;bottom:24px;left:50%;transform:translateX(-50%);" +
      "background:#343a40;color:#fff;padding:10px 18px;border-radius:6px;" +
      "z-index:3000;font-size:.9rem;box-shadow:0 2px 8px rgba(0,0,0,.3)";
    document.body.appendChild(d);
    setTimeout(function () { d.remove(); }, 2800);
  }

  function telecharger(blob) {
    var u = URL.createObjectURL(blob);
    var a = document.createElement("a");
    a.href = u; a.download = "normal-ou-pas.png";
    document.body.appendChild(a); a.click(); a.remove();
    URL.revokeObjectURL(u);
  }

  // Copie un blob image dans le presse-papiers. Renvoie une promesse qui rejette
  // sur TOUTE erreur, y compris une exception synchrone du constructeur
  // ClipboardItem (certains navigateurs refusent le type image ainsi), afin que
  // l'appelant puisse toujours retomber sur le téléchargement.
  function copierBlob(blob) {
    return new Promise(function (resolve, reject) {
      if (!(navigator.clipboard && window.ClipboardItem)) {
        reject(new Error("ClipboardItem non supporté"));
        return;
      }
      try {
        navigator.clipboard.write([new ClipboardItem({ "image/png": blob })])
          .then(resolve, reject);
      } catch (e) {
        reject(e);
      }
    });
  }

  window.partageCopier = function () {
    var blob = imgBlob();
    if (!blob) { toast("Image indisponible."); return; }
    copierBlob(blob)
      .then(function () { toast("Image copiée ✔"); })
      .catch(function (e) {
        console.error("Copie image impossible :", e);
        toast("Copie impossible sur ce navigateur — image téléchargée à la place.");
        telecharger(blob);
      });
  };

  window.partagePartager = function () {
    var blob = imgBlob();
    if (!blob) { toast("Image indisponible."); return; }
    var file = new File([blob], "normal-ou-pas.png", { type: "image/png" });

    // Le partage natif avec image (Web Share API niveau 2) n'existe qu'en contexte
    // sécurisé (HTTPS) et surtout sur mobile / Chrome-Edge desktop. On tente d'abord
    // ce partage ; en l'absence de support OU en cas d'échec, on RETOMBE TOUJOURS sur
    // le téléchargement, pour que le bouton ne soit jamais sans effet (desktop Firefox,
    // http local, etc.). Les annulations volontaires (AbortError) ne déclenchent rien.
    var peutPartagerFichier =
      navigator.share && navigator.canShare && navigator.canShare({ files: [file] });

    if (peutPartagerFichier) {
      navigator.share({ files: [file], title: "Climat : Normal ou pas ?", text: texte() })
        .catch(function (e) {
          if (e && e.name === "AbortError") return; // l'utilisateur a fermé la feuille
          console.error("Partage natif impossible :", e);
          telecharger(blob);
          toast("Partage impossible sur ce navigateur — image téléchargée à la place.");
        });
      return;
    }

    // Pas de partage natif disponible : repli sur le téléchargement (jamais muet).
    telecharger(blob);
    toast("Partage natif indisponible ici (surtout mobile/HTTPS) — image téléchargée. " +
          "Sur ordinateur, utilisez « Copier l'image » ou les boutons réseau.");
  };

  // --- Partage d'un LIEN (défi de série) : copie ou partage natif ------------
  window.partageCopierLien = function (inputId) {
    var el = document.getElementById(inputId);
    if (!el) { toast("Lien indisponible."); return; }
    var lien = el.value || el.textContent || "";
    function repli() {
      try { el.select(); document.execCommand("copy"); toast("Lien copié ✔"); }
      catch (e) { toast("Copie impossible — sélectionnez le lien à la main."); }
    }
    if (navigator.clipboard && navigator.clipboard.writeText) {
      navigator.clipboard.writeText(lien).then(function () { toast("Lien copié ✔"); }, repli);
    } else { repli(); }
  };

  window.partagePartagerLien = function (inputId, texteMsg) {
    var el = document.getElementById(inputId);
    var lien = el ? (el.value || "") : window.location.href;
    if (navigator.share) {
      navigator.share({ title: "Climat : Normal ou pas ?", text: texteMsg || "", url: lien })
        .catch(function (e) {
          if (e && e.name === "AbortError") return; // fermeture volontaire
          window.partageCopierLien(inputId);
        });
      return;
    }
    window.partageCopierLien(inputId); // pas de partage natif : repli copie
  };

  window.partageReseau = function (reseau) {
    var urls = {
      linkedin: "https://www.linkedin.com/feed/?shareActive=true",
      facebook: "https://www.facebook.com/",
      instagram: "https://www.instagram.com/"
    };
    var noms = { linkedin: "LinkedIn", facebook: "Facebook", instagram: "Instagram" };
    // Ouverture SYNCHRONE (dans le geste du clic) pour éviter le blocage popup.
    window.open(urls[reseau], "_blank", "noopener");
    var blob = imgBlob();
    if (!blob) return;
    // Les éditeurs de publication de ces réseaux n'acceptent PAS le collage d'une
    // image (upload de fichier requis) : on télécharge donc l'image, à importer
    // via le bouton photo de la publication.
    telecharger(blob);
    toast("Image téléchargée — importez-la via le bouton photo de votre publication " +
          (noms[reseau] || "") + ".");
  };
})();
