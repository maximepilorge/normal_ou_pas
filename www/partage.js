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
    if (navigator.canShare && navigator.canShare({ files: [file] })) {
      navigator.share({ files: [file], title: "Climat : Normal ou pas ?", text: texte() })
        .catch(function (e) { if (!(e && e.name === "AbortError")) toast("Partage impossible."); });
    } else {
      toast("Partage natif indisponible sur cet appareil — utilisez Copier ou Télécharger.");
    }
  };

  window.partageReseau = function (reseau) {
    var urls = {
      linkedin: "https://www.linkedin.com/feed/?shareActive=true",
      facebook: "https://www.facebook.com/",
      instagram: "https://www.instagram.com/"
    };
    // Ouverture SYNCHRONE (dans le geste du clic) pour éviter le blocage popup.
    window.open(urls[reseau], "_blank", "noopener");
    var blob = imgBlob();
    if (!blob) return;
    if (reseau === "instagram") {
      telecharger(blob);
      toast("Image téléchargée — importez-la dans Instagram.");
    } else {
      copierBlob(blob)
        .then(function () { toast("Image copiée — collez-la dans votre publication."); })
        .catch(function () { telecharger(blob); toast("Image téléchargée — ajoutez-la à votre publication."); });
    }
  };
})();
