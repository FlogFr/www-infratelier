# General rules

## Languages

- Website content (pages, data strings, alt texts, meta tags): **French**.
- Everything under `claude/`, code comments, commit messages: **English**.
- Talk to the owner in the language they use (usually French).

## Before changing anything

1. Read `CLAUDE.md`, then the docs relevant to the task in `claude/docs/`.
2. Check which branch holds the truth: **`main`**. Never edit `gh-pages` by
   hand; `master` is obsolete.

## While working

- Keep the stack small: Pelican + hand-written theme. Do not add a CSS/JS
  framework, a bundler, Node tooling or a Pelican plugin unless the owner asks.
- Structured content goes in `site_data.py`; page copy in `content/pages/*.md`;
  layout in templates. Do not hard-code business facts in several places.
- Never commit `output/` or `.venv/`.

## Before committing

1. `make check` must pass (0 errors). Fix warnings when reasonable.
2. Visually check changed pages at 390 px and 1440 px when the layout changed
   (see `claude/docs/deployment.md` → Local preview).
3. **Update the docs in the same commit**: any change to structure, content
   model, process, design tokens or deployment must be reflected in
   `claude/docs/*.md`; new or changed constraints go in `claude/rules/*.md`;
   non-obvious choices get an entry in `claude/docs/decisions.md`.
4. Bump `Modified:` in the metadata of every page whose content changed.

## Git and deployment

- The owner authorised committing and pushing directly to `main`; a push to
  `main` deploys the live site through GitHub Actions. Treat every push as a
  production release.
- Clear commit messages (imperative subject ≤ 72 chars, body explaining why).
- Never force-push `main` or rewrite history without explicit approval.
- After pushing, confirm the "Build and deploy" workflow succeeded and that
  `gh-pages` received a new "Publish site from <sha>" commit.

## Facts and honesty

- Never invent facts about the business (prices, durations, certifications
  such as Qualiopi, client names, years of experience, statistics). If a
  fact is needed and unknown, ask the owner or leave a clearly marked TODO in
  `claude/docs/seo.md` backlog — not in the published site.
- Never publish secrets. The repo is public.
