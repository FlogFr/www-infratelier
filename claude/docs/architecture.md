# Architecture

## Build pipeline

```
content/pages/*.md ─┐
content/blog/*.md  ─┤                 ┌─ templates (themes/infratelier/templates)
site_data.py ───────┼─> pelicanconf ──┤                                   ┌─> output/ ─> gh-pages
seo.py ─────────────┘   (settings)    └─ Jinja render ─> french_typography ┘
                                                          plugin (post-write)
```

- `pelicanconf.py` — development settings (`SITEURL = ""`, root-relative links).
- `publishconf.py` — production overrides: `SITEURL = "https://infratelier.com"`,
  Atom feed, drafts not written, clean output directory.
- `SITE_ORIGIN = "https://infratelier.com"` is always used for canonical URLs,
  Open Graph, JSON-LD and the sitemap, even in development.

## Settings worth knowing (pelicanconf.py)

| Setting | Value | Why |
| --- | --- | --- |
| `PAGE_URL` / `PAGE_SAVE_AS` | `{slug}/` → `{slug}/index.html` | Clean URLs |
| `ARTICLE_URL` | `blog/{slug}/` | Blog under /blog/ |
| `INDEX_SAVE_AS` | `blog/index.html` | Home is a page, not the article list |
| `DIRECT_TEMPLATES` | `["index", "sitemap"]` | Blog list + custom XML sitemap |
| `*_SAVE_AS = ""` | archives, tags, categories, authors | No thin duplicate pages |
| `JINJA_ENVIRONMENT.autoescape` | `True` | Safe by default; use `|safe` for trusted HTML |
| `JINJA_GLOBALS` | `jsonld` | Structured data builder |
| `JINJA_FILTERS` | `absolute`, `fr` | Absolute URLs, French typography |
| `PLUGINS` | `["french_typography"]` | Post-processes every HTML file |
| `ASSET_VERSION` | build timestamp | Cache busting `?v=` on CSS/JS |

All UPPERCASE names of `site_data.py` are imported into the settings, so they
are template globals (`BUSINESS`, `LEGAL`, `NAV`, `BOOK`, `TRAININGS`,
`TRAINING_FORMATS`, `TRAINING_FAQ`, `SERVICES`, `PROCESS_STEPS`, `CRITERIA`,
`TESTIMONIALS`, `TECHNOLOGIES`, `EXPERTISE`).

## Templates

`base.html` renders `<head>` (title, description, canonical, robots, OG,
Twitter, preloads, icons, JSON-LD), header, footer and the book dialog.

Leaf templates may set these variables at top level to override defaults:

| Variable | Default | Meaning |
| --- | --- | --- |
| `seo_title` | `Meta_title` metadata, else `Title | Infratelier` | `<title>` and og:title |
| `seo_description` | `Description` metadata, else `BUSINESS.description` | meta description |
| `seo_path` | `page.url` | canonical path |
| `seo_kind` | `Kind` metadata, else `page` | JSON-LD variant: home, formations, services, about, contact, blog, article |
| `seo_crumbs` | none | `[(label, path)]` breadcrumb after "Accueil" |
| `seo_robots` | `Robots` metadata, else `index, follow, …` | meta robots |
| `seo_image` | `Image` metadata, else `BUSINESS.og_image` | OG image |
| `book_dialog` | on | `"off"` disables the welcome dialog (also `Book_dialog: off` metadata) |

| Template | Used by |
| --- | --- |
| `home.html` | `accueil.md` (saved as `/index.html`) |
| `formations.html` | `formations.md` |
| `services.html` | `accompagnement.md` |
| `about.html` | `a-propos.md` |
| `contact.html` | `contact.md` |
| `legal.html` | `mentions-legales.md` (legal fields from `LEGAL`) |
| `page.html` | any other page (generic prose) |
| `404.html` | `404.md` |
| `article.html`, `index.html` | blog article / blog list |
| `sitemap.html` | `/sitemap.xml` |

Partials: `header`, `footer`, `book_dialog`, `testimonials`, `technologies`,
`book_section`, `cta` (accepts `cta_title` / `cta_text`), `macros`
(`icon()`, `brand()`, `breadcrumb()`).

## JSON-LD (seo.py)

Every page outputs one `@graph` with stable `@id`s:

- `/#business` — `ProfessionalService` (name, legal name, full postal address, SIREN `identifier`, email, logo, areaServed FR, knowsAbout)
- `/a-propos/#florian-grignon` — `Person` (founder, worksFor business)
- `/#website` — `WebSite`
- `<url>#webpage` — `WebPage` / `AboutPage` / `ContactPage` / `CollectionPage` / `WebPage+FAQPage` (formations)
- `<url>#breadcrumb` — `BreadcrumbList` on inner pages
- home & about: `Book` (`/#the-art-of-kubernetes`)
- formations: `ItemList` of `Course` (from `TRAININGS`) + FAQ `mainEntity` (from `TRAINING_FAQ`)
- accompagnement: `OfferCatalog` of `Service` (from `SERVICES`)
- articles: `BlogPosting`

Because visible content and JSON-LD come from the same `site_data.py`
entries, they cannot drift apart.

## French typography

`seo.french_typography()` (also the `fr` filter) works on text nodes only and
is idempotent: narrow no-break space before `; : ! ?` and inside `« »`,
typographic apostrophe `’`, "A à Z" kept on one line. Tags, attributes,
`<script>`, `<style>`, `<code>`, `<pre>` are untouched. The plugin runs it on
every written `.html` file; the `fr` filter is still needed for attribute
values (meta description, title).

## JavaScript (themes/infratelier/static/js/site.js)

Progressive enhancement only (`html.no-js` → `html.js`):
mobile menu toggle, header shadow on scroll, book dialog. No dependencies.

## Quality gate (scripts/check_site.py)

Fails the build on: missing/duplicate title, missing description, not exactly
one `<h1>`, missing/non-absolute canonical, missing OG tags, `<img>` without
`alt`, invalid JSON-LD, broken internal link/asset/anchor, sitemap URL that
does not exist, missing robots `Sitemap:` line or CNAME. Warns on title > 65
chars and description outside 70–170 chars (indexable pages only).
