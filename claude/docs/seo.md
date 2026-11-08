# SEO

Goal: be found by French companies looking for **Kubernetes / Linux / Cloud
training** and for an **independent infrastructure expert** (Nantes and
remote, all of France), and convert them to an e-mail contact.

## Keyword map (one primary intent per page)

| Page | Primary intent | Secondary |
| --- | --- | --- |
| `/` | consultant / expert infrastructure Cloud indépendant | formation Kubernetes, Linux, Nantes, sur mesure |
| `/formations/` | formation Kubernetes, formation Linux, formation Cloud | GCP, AWS, OpenTofu, Terraform, Prometheus, GitOps, intra-entreprise, à distance |
| `/accompagnement/` | conseil / optimisation infrastructure Cloud | audit, coûts cloud (FinOps), migration Kubernetes, SRE, sécurité |
| `/a-propos/` | Florian Grignon | artisan, auteur The Art of Kubernetes |
| `/contact/` | contact Infratelier | devis formation |

Titles and descriptions live in each page's `Meta_title` / `Description`
metadata; see [../rules/seo.md](../rules/seo.md) for the constraints.

## Technical SEO in place

- Static HTML, no render-blocking third-party resources; one CSS (~32 KB),
  one deferred JS (~4 KB), 3 self-hosted WOFF2 fonts (2 preloaded).
- Clean, stable URLs with trailing slash; absolute `rel=canonical` on every page.
- Unique `<title>`, meta description, exactly one `<h1>` per page (enforced by `make check`).
- Open Graph + Twitter Card with a 1200×630 image.
- JSON-LD graph: ProfessionalService, Person, WebSite, WebPage variants,
  BreadcrumbList, Book, Course ItemList, FAQ, Service catalogue (see architecture.md).
- `/sitemap.xml` (custom template, with `lastmod` from `Modified:` metadata),
  `/robots.txt` with `Sitemap:` line; drafts disallowed and not published.
- Thin listing pages (tags, categories, authors, archives) are not generated;
  `/blog/` is `noindex` while there is no article.
- `404.html` is `noindex` and links back to key pages.
- French typography and `lang="fr"`, `og:locale=fr_FR`.
- Images: explicit dimensions (no CLS), `loading="lazy"` below the fold,
  hero portrait preloaded with `fetchpriority="high"`.
- Accessibility (also a ranking/UX factor): skip link, landmarks, visible
  focus, AA contrast, `prefers-reduced-motion`, accessible dialog.
- Welcome dialog is delayed (1.2 s), dismissible, remembered 30 days, shown as
  a bottom sheet on phones — avoids Google's "intrusive interstitial" pattern.

## What changed vs. the 2024 site (for reference)

The 2024 site had no meta description, no canonical, no structured data,
generic `alt="Image"`, links to non-existent pages (courses.html, cart.html,
login…), fake counters ("Over 200+ courses"), English placeholder sections, a
preloader and ~27 CSS/JS files including jQuery. All removed.

## Backlog (owner actions, not doable from the repo)

1. **Google Search Console** + **Bing Webmaster Tools**: verify the domain
   (DNS TXT), submit `https://infratelier.com/sitemap.xml`, request indexing.
2. **Google Business Profile** for Infratelier (Nantes) — strong local signal.
3. Fill the remaining `LEGAL` fields: share capital (`capital`) and, if VAT
   registered, the intra-EU VAT number (`vat`). SIREN and address are done.
4. Add public profiles to `BUSINESS.same_as` (LinkedIn, Malt, GitHub…) and
   link back to infratelier.com from them.
5. Ask theartofkubernetes.com to link to infratelier.com (author bio).
6. Publish articles (blog) on long-tail topics: e.g. "réduire sa facture GCP",
   "Kubernetes : quand ne pas l'utiliser", "migrer vers OpenTofu",
   "formation Kubernetes : par où commencer". 1 good article/month beats volume.
7. Collect more testimonials, ideally naming the company.
8. Optional: a dedicated page per flagship training (e.g. `/formations/kubernetes/`)
   once there is enough unique content (programme, prerequisites, duration).
