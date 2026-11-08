# Overview

## Business context

**Infratelier SASU** (French single-shareholder simplified joint-stock company)
is the freelance company of **Florian Grignon**, based in **Nantes (44), France**.
The name merges *infrastructure* and *atelier* (workshop).

What the business sells:

1. **Trainings** (formations) on Cloud, Kubernetes and Linux technologies —
   tailor-made programmes, on-site anywhere in France or remote.
2. **Consulting / support** (accompagnement) — helping a few clients design,
   harden and optimise their infrastructure according to *their* criteria
   (budget, performance, resilience, security, compliance, maintainability).

Core message: **craftsmanship** — custom work done end to end ("de A à Z"),
one single contact, knowledge transfer so the client stays autonomous.

Florian also wrote ***The Art of Kubernetes*** (French edition, ~400 pages,
self-published). Its own showcase site is <https://theartofkubernetes.com>
(source: GitHub repo `FlogFr/www-theartofkubernetes`). This site must keep a
**welcome dialog** redirecting visitors interested in Kubernetes to the book
site (see [design.md](design.md#book-dialog)).

Contact: `contact@infratelier.com` (no contact form: static hosting, no
third-party services).

## Site map

| URL | Source | Purpose / main keywords |
| --- | --- | --- |
| `/` | `content/pages/accueil.md` + `home.html` | Brand, both offers, approach, proof |
| `/formations/` | `formations.md` + `formations.html` | Training catalogue, FAQ |
| `/accompagnement/` | `accompagnement.md` + `services.html` | Consulting offer |
| `/a-propos/` | `a-propos.md` + `about.html` | Florian, values, book |
| `/contact/` | `contact.md` + `contact.html` | E-mail CTA |
| `/mentions-legales/` | `mentions-legales.md` + `legal.html` | Legal notice, privacy |
| `/404.html` | `404.md` + `404.html` | Not-found page (noindex) |
| `/blog/` | `index.html` (direct template) | Article list; noindex while empty |

## History

| Period | Site | Where |
| --- | --- | --- |
| 2019 | Haskell/Servant app (Mustache templates, Bulma, Postgres, Helm chart for Kubernetes) | `main` up to commit `f93e3fc` |
| Oct 2024 | Pelican site using the commercial "eduon" ThemeForest theme, published with `make github` | branch `master` (commit `9c3cadf`), output on `gh-pages` |
| Nov 2026 | Consolidated on `main`, custom lightweight theme, SEO + content rewrite, CI deploy | `main` |

The legacy Haskell code and the eduon theme were removed from the tree; they
remain in git history. The 2019 tree contained committed credentials (see
[decisions.md](decisions.md)).

## Branches

- `main` — **single source of truth**. Default branch. Pushing here deploys.
- `gh-pages` — generated site only (written by CI with `ghp-import`). Never edit by hand.
- `master` — obsolete 2024 Pelican source, already merged into `main`. Can be deleted by the owner.

## Repository layout

```
.
├── CLAUDE.md                 entry point for Claude
├── claude/docs/ claude/rules/
├── content/
│   ├── pages/                one .md per page
│   ├── blog/                 articles (Status: draft until ready)
│   ├── assets/img/           images (portraits, logos, book cover, OG image)
│   └── extra/                CNAME, robots.txt, favicon.ico, site.webmanifest
├── themes/infratelier/
│   ├── templates/            Jinja templates (+ partials/)
│   └── static/               css/site.css, js/site.js, fonts/, img/
├── plugins/french_typography.py
├── site_data.py  seo.py  pelicanconf.py  publishconf.py
├── scripts/check_site.py
├── Makefile  requirements.txt
└── .github/workflows/deploy.yml
```
