# Build and deployment

## Hosting

- **GitHub Pages**, repository `FlogFr/www-infratelier`, source branch
  **`gh-pages`** (root). Custom domain **infratelier.com** via the `CNAME`
  file (`content/extra/CNAME`, copied to the output root).
- `.nojekyll` is added by `ghp-import -n`, so Pages serves files as-is.
- HTTPS is handled by GitHub Pages ("Enforce HTTPS" in repo settings).

## Continuous deployment (`.github/workflows/deploy.yml`)

On every push to `main` (and manual `workflow_dispatch`):

1. `pip install -r requirements.txt` (Python 3.12)
2. `make check` = `pelican … -s publishconf.py --fatal warnings` + `scripts/check_site.py`
3. `ghp-import --no-jekyll --push --branch gh-pages output` — a normal
   fast-forward commit on `gh-pages` ("Publish site from <sha>").

Pull requests run steps 1–2 only. The workflow needs `contents: write`
(declared in the file). GitHub Pages then rebuilds automatically (1–2 min).

## Local development

```bash
make install && . .venv/bin/activate
make devserver            # http://localhost:8000 with auto-rebuild
make check                # what CI runs; must be green before pushing
```

### Local preview / screenshots (Claude cloud sessions)

```bash
make html && (cd output && python3 -m http.server 8765 &)
# then with the pre-installed Playwright (node):
#   const { chromium } = require('playwright'); ... page.goto('http://localhost:8765/?nodialog')
# use ?livre to force the book dialog; scroll the page before full-page
# screenshots so lazy images load.
```

## Manual deploy (fallback)

```bash
make github   # check + ghp-import -n + git push origin gh-pages
```

## Troubleshooting

| Symptom | Fix |
| --- | --- |
| CI push to gh-pages rejected | Ensure Settings → Actions → General → Workflow permissions allows read/write, or that `permissions: contents: write` is kept |
| Site shows 404 after deploy | Settings → Pages: source = "Deploy from a branch", branch `gh-pages` / root; custom domain `infratelier.com` |
| Domain lost after deploy | `content/extra/CNAME` must exist (checked by `check_site.py`) |
| Pelican warning fails the build | `--fatal warnings` is intentional; fix the warning (often bad metadata) |
| Old CSS after deploy | `ASSET_VERSION` changes each build; hard-refresh / wait for CDN (max ~10 min) |
