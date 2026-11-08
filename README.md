# infratelier.com

Source of [infratelier.com](https://infratelier.com), the website of
**Infratelier SASU** — Florian Grignon, independent IT infrastructure expert:
Cloud, Kubernetes and Linux trainings, and tailor-made infrastructure consulting.

Static site built with [Pelican](https://getpelican.com) and a hand-written
theme, published to GitHub Pages (`gh-pages` branch) by GitHub Actions on
every push to `main`.

```bash
make install && . .venv/bin/activate
make devserver   # http://localhost:8000
make check       # production build + SEO/link checks
```

Documentation: [CLAUDE.md](CLAUDE.md) and [claude/docs/](claude/docs/).
