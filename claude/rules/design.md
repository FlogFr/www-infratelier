# Design, front-end and privacy rules

## CSS

- Single stylesheet `themes/infratelier/static/css/site.css`, organised in the
  numbered sections at its top. Add rules to the matching section.
- Use the `:root` tokens (colours, type scale, radius, spacing). No new raw
  colour values without adding a token and documenting it in
  `claude/docs/design.md`.
- Text contrast ≥ **4.5:1** (≥ 3:1 for text ≥ 24 px / bold ≥ 19 px). The logo
  colours `--teal-bright`, `--amber`, `--green` are decorative: never use them
  for text on light backgrounds (amber is a background for ink text).
- Mobile first in thinking: check 390 px. No horizontal scroll at any width.
- Respect `prefers-reduced-motion`; animations are subtle and short.

## HTML and accessibility

- Semantic landmarks (`header`, `nav`, `main#contenu`, `footer`), one `h1`,
  logical heading order, `aria-labelledby` on sections with headings.
- Keyboard: everything reachable, visible `:focus-visible`, Escape closes the
  dialog and the mobile menu.
- Icons are inline SVG via the `icon()` macro, `aria-hidden="true"`.

## JavaScript

- Vanilla, progressive enhancement only: the site must be fully usable with
  JS disabled (except the dialog, which simply does not appear).
- No dependencies, no build step. Keep `site.js` small and commented.
- Wrap every `localStorage` access in try/catch.

## Privacy (GDPR) — hard rules

- **No third-party requests** from pages: no Google Fonts, CDN scripts,
  analytics, tag managers, embeds or tracking pixels. Fonts and images are
  self-hosted. If the owner asks for analytics, propose a cookie-less EU tool
  and update `mentions-legales.md` in the same change.
- No cookies. The only client-side storage is the dialog's
  `infratelier:book-dialog:closed-at` key (documented on the legal page).

## Brand

- Logo colours and wordmark are fixed (see `claude/docs/design.md`). Use the
  `brand()` macro, not an image, in the header/footer.
- Fonts: Fraunces (headings), Inter (body). Do not add other families.
