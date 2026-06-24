// www/partage.js
// Helpers du modal de partage de la carte de résultat du quiz.
// Le modal (construit côté serveur) contient l'image (#apercu-partage-img) et
// son texte d'accompagnement (#partage-zone[data-texte]). Ces fonctions, appelées
// par les boutons du modal, en déduisent l'action choisie.
//
// Réalités des plateformes :
//  - presse-papiers / partage natif (Web Share API) : vrais partages d'image ;
//  - LinkedIn / Facebook : pas d'upload d'image via le web -> on copie l'image et
//    on ouvre le réseau pour que l'utilisateur la colle (un aperçu OG riche
//    nécessiterait le sidecar utils/share_api.R déployé) ;
//  - Instagram : aucune publication web -> téléchargement puis import manuel
//    (ou partage natif sur mobile).

(function () {
  function imgBlob(cb) {
    var img = document.getElementById("apercu-partage-img");
    if (!img) return;
    fetch(img.src).then(function (r) { return r.blob(); }).then(cb)
      .catch(function () { toast("Action impossible sur cet appareil."); });
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

  // Copie l'image dans le presse-papiers. `after(ok)` permet d'enchaîner.
  function copier(after) {
    imgBlob(function (blob) {
      if (navigator.clipboard && window.ClipboardItem) {
        navigator.clipboard.write([new ClipboardItem({ "image/png": blob })])
          .then(function () { after ? after(true) : toast("Image copiée ✔"); })
          .catch(function () { after ? after(false) : toast("Copie impossible — utilisez Télécharger."); });
      } else {
        after ? after(false) : toast("Copie non supportée — utilisez Télécharger.");
      }
    });
  }

  window.partageCopier = function () { copier(null); };

  window.partagePartager = function () {
    imgBlob(function (blob) {
      var file = new File([blob], "normal-ou-pas.png", { type: "image/png" });
      if (navigator.canShare && navigator.canShare({ files: [file] })) {
        navigator.share({ files: [file], title: "Climat : Normal ou pas ?", text: texte() })
          .catch(function (e) { if (!(e && e.name === "AbortError")) toast("Partage impossible."); });
      } else {
        toast("Partage natif indisponible sur cet appareil — utilisez Copier ou Télécharger.");
      }
    });
  };

  window.partageReseau = function (reseau) {
    var urls = {
      linkedin: "https://www.linkedin.com/feed/?shareActive=true",
      facebook: "https://www.facebook.com/",
      instagram: "https://www.instagram.com/"
    };
    // On ouvre le réseau SYNCHRONEMENT (dans le geste du clic) pour éviter le
    // blocage des popups, puis on prépare l'image.
    window.open(urls[reseau], "_blank", "noopener");
    if (reseau === "instagram") {
      imgBlob(telecharger);
      toast("Image téléchargée — importez-la dans Instagram.");
    } else {
      copier(function (ok) {
        toast(ok ? "Image copiée — collez-la dans votre publication."
                 : "Ajoutez l'image téléchargée à votre publication.");
      });
    }
  };
})();
