# Editing content

Site copy is **French**. Follow [../rules/content.md](../rules/content.md).

## Pages (`content/pages/*.md`)

Metadata header, blank line, Markdown body:

```
Title: Formations                      # short name (breadcrumb, nav fallback)
Meta_title: Formations Kubernetes, Linux, Cloud sur mesure | Infratelier   # <title>, ≤ 60 chars
Heading: Formations Kubernetes, Linux et Cloud, sur mesure                     # visible <h1> (defaults to Title)
Lead: …                                 # optional intro paragraph under the h1 (about, contact, generic pages)
Description: …                          # meta description, 120–160 chars, unique
Slug: formations                        # URL: /formations/ — never change once published
Template: formations                    # template in themes/infratelier/templates
Modified: 2026-11-08                    # bump when content changes (sitemap lastmod, JSON-LD dateModified)
Book_dialog: off                        # optional: no welcome dialog on this page
Robots: noindex, follow                 # optional
Status: hidden                          # optional: not listed (404)
```

Where the Markdown body appears depends on the template: the hero lead on
home / formations / accompagnement, the main prose on about / legal / generic
pages, the instructions under the e-mail on contact.

### Add a new page

1. Create `content/pages/<slug>.md` with the metadata above (`Template: page`
   for a simple prose page).
2. Link to it (nav: add `(slug, label)` to `NAV` in `site_data.py`; footer
   "Le site" column uses `NAV` too).
3. `make check`, then update `claude/docs/overview.md` (site map) and
   `claude/docs/seo.md` (keyword map).

## Structured content (`site_data.py`)

| Name | Shown on |
| --- | --- |
| `BUSINESS` | everywhere (footer, JSON-LD); `street_address` only in JSON-LD + legal page; `same_as` = public profiles (LinkedIn, Malt…) |
| `LEGAL` | mentions légales — `address`, `siren`, `rcs_city` filled; `capital` and `vat` still empty (hidden until set) |
| `NAV` | header and footer navigation |
| `BOOK` | dialog, book section, hero badge, about |
| `TRAININGS` | formations catalogue (+ footer, + JSON-LD Course list). `id` = anchor `/formations/#id` |
| `TRAINING_FORMATS`, `TRAINING_FAQ` | formations page (FAQ also in JSON-LD) |
| `SERVICES` | accompagnement page (+ JSON-LD) |
| `PROCESS_STEPS`, `CRITERIA` | home and accompagnement |
| `TESTIMONIALS` | home and accompagnement — quotes are **verbatim** |
| `TECHNOLOGIES` | logo grid; logos are 200×40 webp in `content/assets/img/` |
| `EXPERTISE` | JSON-LD `knowsAbout` |

Strings are plain text (no HTML). Apostrophes and French spacing are fixed
automatically at render time.

### Add a training

Append a dict to `TRAININGS` (`id`, `short`, `family`, `title`, `summary`,
`audience`, `topics`). The `id` must be unique and URL-safe; the footer lists
the first five trainings.

### Add a testimonial

Append to `TESTIMONIALS` with the exact quote, name, role and a 65×65 webp
portrait in `content/assets/img/`. Only with the client's consent.

## Blog articles (`content/blog/*.md`)

```
Title: …
Date: 2026-11-08
Modified: 2026-11-08
Slug: mon-article
Category: Blog
Summary: one or two sentences (used as list summary)
Description: meta description, 120–160 chars
Status: draft        # remove (or set to published) when ready
```

Drafts are rendered locally under `/drafts/` (`make html`) but never
published. When the first article is published, the "Blog" link appears in
the header/footer, `/blog/` becomes indexable and articles join the sitemap
and the Atom feed automatically.

Existing draft: `j-ai-ecrit-un-livre-autour-de-kubernetes.md` (raw notes on
writing the book — needs writing before publication).

## Images

- Put files in `content/assets/img/` (served at `/assets/img/...`). Prefer
  WebP, give explicit `width`/`height`, meaningful French `alt`.
- Open Graph image: `content/assets/img/og-infratelier.jpg` (1200×630). It was
  rendered from HTML with Playwright; regenerate it if the tagline changes.
- Theme icons (favicon.svg, PNG icons) live in `themes/infratelier/static/img/`.
