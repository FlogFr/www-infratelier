# Content rules (French copy)

## Voice and message

- First person singular: **"je"** (Florian speaks), the reader is **"vous"**.
  Infratelier is one person; never "nous" for the company (except "nous
  construisons ensemble" = Florian + the client).
- Core message to reinforce everywhere: **artisanat** — sur mesure, de A à Z,
  un seul interlocuteur, écoute, transmission/autonomie, vos critères.
- Two offers, always named the same way: **Formations** (Cloud, Kubernetes,
  Linux…) and **Accompagnement** (audit, optimisation, fiabilisation…).
- Tone: warm, precise, confident without hype. No buzzword stacking, no
  superlatives that can't be backed ("le meilleur", "n°1").
- Prefer concrete benefits (facture cloud, disponibilité, autonomie de
  l'équipe) over technology lists.

## Facts

- No invented numbers, durations, prices, certifications, client logos or
  quotes. Only state what the owner said or what is already on the site.
- Testimonials are **verbatim**: never rephrase, shorten or "fix" a quote
  without the client's agreement.
- Book facts in use: *The Art of Kubernetes*, French Edition, ~400 pages,
  written in French, self-published, by Florian Grignon.

## Typography

- Write normal French with straight apostrophes and normal spaces: the build
  converts to `’`, adds narrow no-break spaces before `; : ! ?` and inside
  `« »`, and keeps "A à Z" together. Use `« »` for quotes, `…` for ellipsis.
- Do not type invisible Unicode characters in source files; use escapes in
  Python (`"\u202f"`) when really needed.
- Capitalise only the first word of headings (French convention), except
  proper nouns (Kubernetes, Linux, Google Cloud…).

## Page metadata

Every page in `content/pages/` must have: `Title`, `Meta_title` (≤ 60 chars,
ends with `| Infratelier`), `Description` (120–160 chars, unique, contains the
page's primary keyword and a benefit), `Slug`, `Template`, `Modified`.
See [seo.md](seo.md).

## Images

- `alt` in French, describing the image ("Portrait dessiné de Florian
  Grignon"); empty `alt=""` only for purely decorative images next to text
  that already says the same thing.
