/* Infratelier — progressive enhancements. The site works without JS. */
(function () {
  "use strict";

  var doc = document.documentElement;

  /* Mobile navigation ---------------------------------------------------- */
  var toggle = document.querySelector(".nav-toggle");
  var nav = document.getElementById("site-nav");
  if (toggle && nav) {
    var setOpen = function (open) {
      nav.classList.toggle("is-open", open);
      toggle.setAttribute("aria-expanded", open ? "true" : "false");
    };
    toggle.addEventListener("click", function () {
      setOpen(toggle.getAttribute("aria-expanded") !== "true");
    });
    document.addEventListener("keydown", function (e) {
      if (e.key === "Escape" && nav.classList.contains("is-open")) {
        setOpen(false);
        toggle.focus();
      }
    });
    nav.addEventListener("click", function (e) {
      if (e.target.closest("a")) setOpen(false);
    });
  }

  /* Header shadow once the page scrolls ---------------------------------- */
  var header = document.querySelector(".site-header");
  if (header) {
    var onScroll = function () {
      header.classList.toggle("is-scrolled", window.scrollY > 8);
    };
    onScroll();
    window.addEventListener("scroll", onScroll, { passive: true });
  }

  /* Welcome dialog promoting "The Art of Kubernetes" ----------------------
     Shown once on arrival, then not again for DIALOG_SNOOZE_DAYS after the
     visitor closes it. Never shown on pages whose <body> has
     data-book-dialog="off", nor to visitors coming from the book's site. */
  var DIALOG_KEY = "infratelier:book-dialog:closed-at";
  var DIALOG_SNOOZE_DAYS = 30;
  var DIALOG_DELAY_MS = 1200;

  var dialog = document.getElementById("book-dialog");
  if (!dialog || typeof dialog.showModal !== "function") return;
  if (document.body.getAttribute("data-book-dialog") === "off") return;

  var storage = null;
  try {
    storage = window.localStorage;
    storage.getItem(DIALOG_KEY);
  } catch (err) {
    storage = null;
  }

  var params = new URLSearchParams(window.location.search);
  var forced = params.has("livre"); // ?livre forces the dialog (handy for tests)

  var recentlyClosed = function () {
    if (!storage) return false;
    var closedAt = parseInt(storage.getItem(DIALOG_KEY) || "0", 10);
    return closedAt && Date.now() - closedAt < DIALOG_SNOOZE_DAYS * 864e5;
  };
  var fromBookSite = /theartofkubernetes\.com/i.test(document.referrer || "");

  if (!forced && (recentlyClosed() || fromBookSite || params.has("nodialog"))) return;

  var remember = function () {
    if (!storage) return;
    try { storage.setItem(DIALOG_KEY, String(Date.now())); } catch (err) { /* ignore */ }
  };

  dialog.addEventListener("close", remember);
  dialog.addEventListener("click", function (e) {
    // click on the backdrop (outside the dialog box) closes it
    if (e.target === dialog) dialog.close("backdrop");
  });
  Array.prototype.forEach.call(dialog.querySelectorAll("[data-dialog-close]"), function (btn) {
    btn.addEventListener("click", function () { dialog.close("dismiss"); });
  });
  var cta = dialog.querySelector("[data-dialog-cta]");
  if (cta) cta.addEventListener("click", remember);

  window.setTimeout(function () {
    if (dialog.open || document.querySelector("dialog[open]")) return;
    dialog.showModal();
    // Focus the dialog itself (announced via aria-labelledby) rather than
    // its first button, so no focus ring flashes for mouse/touch users.
    dialog.setAttribute("tabindex", "-1");
    dialog.focus({ preventScroll: true });
    doc.classList.add("has-dialog");
  }, forced ? 0 : DIALOG_DELAY_MS);

  dialog.addEventListener("close", function () { doc.classList.remove("has-dialog"); });
})();
