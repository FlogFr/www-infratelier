# Decision log

Newest first. One entry per non-obvious choice: context, decision, consequence.

## 2026-11-08 — Redesign and consolidation

- **main is the single source of truth.** The live site came from branch
  `master` (Pelican, 2024) while `main` still held the 2019 Haskell app. Both
  histories were merged into `main`; the Haskell code was removed from the
  tree (still in history, commit `f93e3fc`). `master` is now obsolete.
- **Committed secrets in history.** The 2019 tree contained a GitLab registry
  deploy token (`charts/infratelier/values.yaml`), a Ceph key, htpasswd hashes
  and a form "secret". They are gone from the tree but remain in git history
  of a public repository → **the owner should revoke/rotate them** (GitLab
  deploy token, Ceph key, any reused password). Rewriting history was not done
  (destructive, needs owner approval).
- **Custom theme instead of the commercial "eduon" theme.** The ThemeForest
  theme shipped ~27 CSS/JS files with jQuery, demo pages and placeholder
  content, and a marketplace licence typically does not allow redistributing
  the theme sources in a public repository.
  Replaced by a small hand-written theme (one CSS, one JS).
- **Pelican kept** (not a JS framework): content is mostly static, the owner
  already used Pelican, the output is plain HTML — ideal for SEO.
- **No contact form.** GitHub Pages is static; a form would need a third-party
  service (privacy, GDPR, cost). E-mail links with pre-filled subjects instead.
- **Self-hosted fonts, no analytics, no cookies.** Avoids GDPR consent banners
  and third-party requests (Google Fonts are considered a personal-data
  transfer by EU courts). If analytics are wanted later, prefer a
  cookie-less, EU-hosted tool and update the legal page.
- **Welcome dialog kept but made non-intrusive** (owner requirement): delayed,
  remembered 30 days, bottom sheet on mobile, disabled on contact/legal/404.
- **Structured content in `site_data.py`** so visible content and JSON-LD come
  from one place.
- **Custom sitemap template** instead of the `pelican-sitemap` plugin: full
  control (exclude noindex pages, `lastmod` from `Modified:`), no dependency.
- **Legal notice fields**: SIREN `920 555 083` and registered office
  (80 rue des Hauts-Pavés, 44000 Nantes) provided by the owner; RCS shown as
  "RCS de Nantes" (registry of the registered office). Share capital and VAT
  number are still unknown and stay hidden until filled in `LEGAL`. The
  street address is published only on the legal page and in JSON-LD (local
  SEO, NAP consistency), not in the footer.
- **Testimonials are verbatim**, including their original wording.
