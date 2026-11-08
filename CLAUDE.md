# CLAUDE.md — infratelier.com

Showcase website of **Infratelier SASU**, the French freelance company of
Florian Grignon: independent IT infrastructure expert, Cloud / Kubernetes /
Linux trainer and consultant, author of *The Art of Kubernetes*.
Positioning: **craftsmanship** ("artisanat") — tailor-made work, from A to Z.

- Stack: [Pelican](https://getpelican.com) 4.12 static site, custom theme
  `themes/infratelier` (hand-written HTML/CSS/vanilla JS, no framework).
- Hosting: GitHub Pages, branch `gh-pages`, custom domain `infratelier.com`.
- Deploy: push to `main` → GitHub Actions builds, checks, publishes.
- Site language: **French**. Project docs and rules: **English**.

## Commands

```bash
make install      # .venv + dependencies (then: . .venv/bin/activate)
make devserver    # http://localhost:8000, rebuilds on change
make check        # production build + SEO/link checks (must pass before pushing)
make github       # manual deploy to gh-pages (normally CI does it)
```

## Where things are

| Path | What |
| --- | --- |
| `content/pages/*.md` | One Markdown file per page (metadata = SEO fields) |
| `site_data.py` | Structured content: trainings, services, testimonials, book, legal info |
| `seo.py` | JSON-LD builder, `absolute` and `fr` (French typography) Jinja filters |
| `plugins/french_typography.py` | Applies French typography to every generated HTML file |
| `themes/infratelier/` | Templates, CSS, JS, fonts, icons |
| `scripts/check_site.py` | Quality gate used by `make check` and CI |
| `.github/workflows/deploy.yml` | Build + deploy pipeline |
| `claude/docs/` | Project documentation (read before working) |
| `claude/rules/` | Rules Claude must follow in this repo |

## Documentation (claude/docs)

- [overview.md](claude/docs/overview.md) — business context, history, branches, repo layout.
- [architecture.md](claude/docs/architecture.md) — how the build works, templates, data flow.
- [content.md](claude/docs/content.md) — how to edit pages, trainings, testimonials, articles.
- [seo.md](claude/docs/seo.md) — SEO strategy, keywords per page, technical SEO, backlog.
- [design.md](claude/docs/design.md) — design system, components, the book dialog.
- [deployment.md](claude/docs/deployment.md) — CI, GitHub Pages, domain, troubleshooting.
- [decisions.md](claude/docs/decisions.md) — log of notable decisions and why.

## Rules (claude/rules) — always apply

- [general.md](claude/rules/general.md) — workflow, git, keeping docs up to date.
- [content.md](claude/rules/content.md) — French copywriting rules, no invented facts.
- [seo.md](claude/rules/seo.md) — SEO invariants every change must preserve.
- [design.md](claude/rules/design.md) — CSS/JS/accessibility/privacy constraints.

**Maintenance duty:** any change to behaviour, structure, content model or
process must update the matching file in `claude/docs/` (and `claude/rules/`
when a rule changes) in the same commit. Add an entry to `decisions.md` for
non-obvious choices.
