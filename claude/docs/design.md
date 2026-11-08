# Design system

Concept: **the workshop bench** ("atelier"). Warm paper background, a faint
cutting-mat grid behind heroes, the portrait pinned with masking tape, the
brand gradient used as a thin accent line. Calm, crafted, legible — the
opposite of a generic template.

## Tokens (`themes/infratelier/static/css/site.css`, `:root`)

| Token | Value | Use |
| --- | --- | --- |
| `--paper` / `--paper-alt` / `--surface` | `#fbf8f3` / `#f3eee4` / `#fff` | page, alternate sections, cards |
| `--ink` / `--ink-soft` / `--ink-faint` | `#13232b` / `#46565e` / `#5d6a70` | text levels (all ≥ 4.8:1 on paper) |
| `--teal` | `#007184` | links, eyebrows (≥ 4.9:1) |
| `--teal-bright`, `--green` | `#00a2be`, `#00be13` | logo colours, decorative only |
| `--amber` / `--amber-deep` | `#ffb607` / `#e9a200` | primary CTA background (ink text, 9:1) |
| `--night` | `#10212a` | dark sections (book, footer), dialog visual |
| `--brand-gradient` | teal → amber → green | thin bars, decorative |
| `--brand-gradient-text` | darker stops | gradient text (`.accent`), ≥ 3.3:1 for large text |

Logo colours come from `content/assets/img/Infratelier-Icon*.svg`
(`#00a2be`, `#ffb607`, `#00be13`). The wordmark is rebuilt in HTML/SVG by the
`brand()` macro: "infra" + gradient triangle + "telier".

## Typography

- Headings: **Fraunces** (variable, weights 100–900, normal + italic), self-hosted.
- Body: **Inter** (variable), self-hosted.
- Fluid scale `--step--1` … `--step-4` with `clamp()`.
- `.accent` = italic Fraunces with gradient text (one per heading, max).

## Components (CSS classes)

`.container`, `.section`, `.section-alt`, `.section-head`, `.eyebrow`,
`.grid-2/3/4`, `.card`, `.offer`, `.steps` (numbered process, horizontal on
desktop / vertical on mobile), `.criteria`, `.testimonial`, `.tech-grid`,
`.book` (dark section), `.cta-band`, `.training`, `.faq` (`<details>`),
`.prose`, `.split`, `.facts`, `.btn` + `.btn-primary|dark|ghost|light|small`,
`.link-arrow`, `.checklist`, `.tag`, `.breadcrumb`, `.page-hero`, `.hero`.

## Book dialog

Requirement from the owner: when visitors arrive on the site, a dialog offers
to go to **theartofkubernetes.com** if they are interested in Kubernetes.

Implementation: `partials/book_dialog.html` + `site.js`.

- Native `<dialog>` opened with `showModal()` 1.2 s after load.
- Once closed (button, ✕, Escape, backdrop click) or once the CTA is clicked,
  it is not shown again for **30 days** (`localStorage` key
  `infratelier:book-dialog:closed-at`; page works if storage is blocked).
- Not shown when the visitor comes from theartofkubernetes.com, on pages with
  `Book_dialog: off` (contact) or `book_dialog = "off"` (legal, 404),
  or with `?nodialog`. `?livre` forces it (testing).
- Desktop: centred card (cover on the left). ≤ 600 px: bottom sheet, the page
  stays visible above it.
- Links carry UTM parameters (`utm_campaign=welcome-dialog` / `book-section`).

## Responsive checkpoints

Verify at **390 px** (phone), **768 px** and **1440 px**. Mobile nav collapses
under 880 px (only when JS is on; without JS links stay visible).

## Screenshots

To review visually, build (`make html`), serve `output/` and use the
pre-installed Playwright (see `deployment.md` → "Local preview").
