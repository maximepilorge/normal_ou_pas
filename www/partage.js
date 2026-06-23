// www/partage.js
// Partage de la carte de résultat du quiz, déclenché côté serveur via
// session$sendCustomMessage("partage_resultat", {image, filename, titre, texte}).
// Stratégie en cascade : 1) partage natif (Web Share API avec fichier image),
// 2) copie dans le presse-papiers, 3) repli téléchargement.

(function () {
  function dataUrlVersBlob(dataUrl) {
    return fetch(dataUrl).then(function (r) { return r.blob(); });
  }

  function telecharger(dataUrl, filename) {
    var a = document.createElement("a");
    a.href = dataUrl;
    a.download = filename;
    document.body.appendChild(a);
    a.click();
    a.remove();
  }

  function toast(texte) {
    var t = document.createElement("div");
    t.textContent = texte;
    t.style.cssText =
      "position:fixed;bottom:24px;left:50%;transform:translateX(-50%);" +
      "background:#343a40;color:#fff;padding:10px 18px;border-radius:6px;" +
      "z-index:2000;font-size:0.9rem;box-shadow:0 2px 8px rgba(0,0,0,.3)";
    document.body.appendChild(t);
    setTimeout(function () { t.remove(); }, 2600);
  }

  function gererPartage(msg) {
    dataUrlVersBlob(msg.image).then(function (blob) {
      var file = new File([blob], msg.filename, { type: "image/png" });

      // 1) Partage natif avec fichier (mobile, Safari macOS…)
      if (navigator.canShare && navigator.canShare({ files: [file] })) {
        navigator
          .share({ files: [file], title: msg.titre, text: msg.texte })
          .catch(function (e) {
            if (e && e.name === "AbortError") return; // l'utilisateur a annulé
            copierOuTelecharger(blob, msg);
          });
        return;
      }
      // 2) Sinon, presse-papiers / téléchargement
      copierOuTelecharger(blob, msg);
    }).catch(function () {
      telecharger(msg.image, msg.filename);
    });
  }

  function copierOuTelecharger(blob, msg) {
    if (navigator.clipboard && window.ClipboardItem) {
      navigator.clipboard
        .write([new ClipboardItem({ "image/png": blob })])
        .then(function () { toast("Image copiée dans le presse-papiers ✔"); })
        .catch(function () { telecharger(msg.image, msg.filename); });
    } else {
      telecharger(msg.image, msg.filename);
    }
  }

  // Enregistrement du handler dès que Shiny est prêt.
  function enregistrer() {
    if (window.Shiny && Shiny.addCustomMessageHandler) {
      Shiny.addCustomMessageHandler("partage_resultat", gererPartage);
    }
  }
  if (window.Shiny && Shiny.addCustomMessageHandler) {
    enregistrer();
  } else {
    document.addEventListener("shiny:connected", enregistrer);
  }
})();
