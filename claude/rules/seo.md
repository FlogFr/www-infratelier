# SEO rules (invariants)

Every change must keep these true. `make check` enforces most of them.

1. **One `<h1>` per page**, containing the page's primary topic.
2. **Unique `<title>`** (≤ 60 chars ideally, ≤ 65 max) and **unique meta
   description** (120–160 chars) per indexable page.
3. **Stable URLs.** Never change a published `Slug`. GitHub Pages cannot do
   301 redirects; if a URL must move, keep a page at the old URL that links to
   the new one (and set its `Robots: noindex, follow`), and update internal links.
4. **Absolute canonical** on every page (`SITE_ORIGIN` + path) — automatic,
   do not override unless consolidating duplicates.
5. **Structured data stays in sync**: business facts, trainings, services, FAQ
   and testimonials come from `site_data.py`; do not hand-write JSON-LD in
   templates or content. Extend `seo.jsonld()` instead.
6. **Sitemap**: indexable pages appear automatically. Pages with `noindex`
   must not appear (handled by `sitemap.html`). Bump `Modified:` when content changes.
7. **No thin or duplicate pages**: keep tag/category/author/archive pages
   disabled; keep `/blog/` `noindex` while empty (automatic).
8. **Internal linking**: every new page is linked from the navigation, the
   footer or a relevant page body, with descriptive anchor text (not "cliquez ici").
9. **Performance budget** (Core Web Vitals): no new render-blocking
   third-party resource; images with `width`/`height`; `loading="lazy"` below
   the fold; keep total CSS < 50 KB and JS < 10 KB unminified.
10. **The welcome dialog must stay non-intrusive**: delayed, dismissible,
    remembered, bottom sheet on mobile, off on contact/legal/404. Never show
    it immediately on load or on every page view.
11. **Language**: `lang="fr"`, `og:locale=fr_FR`; no mixed-language headings
    except product names.
12. Do not add `nofollow` to links to theartofkubernetes.com (owner's own site).
