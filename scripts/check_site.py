#!/usr/bin/env python3
"""Quality gate for the generated site (run by `make check` and in CI).

Usage: python3 scripts/check_site.py output/

Checks every indexable HTML page for the SEO essentials and verifies that
internal links and assets resolve to generated files. Exits 1 on any error;
warnings are printed but do not fail the build. Standard library only.
"""

import json
import re
import sys
from html.parser import HTMLParser
from pathlib import Path
from urllib.parse import urlparse, unquote

ORIGIN = "https://infratelier.com"
TITLE_MAX = 65
DESC_MIN, DESC_MAX = 70, 170


class PageParser(HTMLParser):
    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.title = ""
        self._in_title = False
        self._in_jsonld = False
        self.meta = {}
        self.links = []  # (tag, attr, url)
        self.canonical = None
        self.h1 = 0
        self.imgs_without_alt = []
        self.jsonld = []
        self._buf = []
        self.ids = set()

    def handle_starttag(self, tag, attrs):
        a = dict(attrs)
        if "id" in a:
            self.ids.add(a["id"])
        if tag == "title":
            self._in_title = True
        elif tag == "meta":
            key = a.get("name") or a.get("property")
            if key:
                self.meta[key] = a.get("content", "")
        elif tag == "link":
            if a.get("rel") == "canonical":
                self.canonical = a.get("href")
            if a.get("href"):
                self.links.append(("link", "href", a["href"]))
        elif tag == "a" and a.get("href"):
            self.links.append(("a", "href", a["href"]))
        elif tag in ("img", "script") and a.get("src"):
            self.links.append((tag, "src", a["src"]))
            if tag == "img" and "alt" not in a:
                self.imgs_without_alt.append(a["src"])
        elif tag == "h1":
            self.h1 += 1
        if tag == "script" and a.get("type") == "application/ld+json":
            self._in_jsonld = True
            self._buf = []

    def handle_endtag(self, tag):
        if tag == "title":
            self._in_title = False
        if tag == "script" and self._in_jsonld:
            self._in_jsonld = False
            self.jsonld.append("".join(self._buf))

    def handle_data(self, data):
        if self._in_title:
            self.title += data
        if self._in_jsonld:
            self._buf.append(data)


def resolve(out: Path, page: Path, url: str):
    """Return the file a site-internal URL points to, or None if external."""
    parsed = urlparse(url)
    if parsed.scheme in ("mailto", "tel", "javascript", "data"):
        return None
    if parsed.scheme in ("http", "https"):
        if f"{parsed.scheme}://{parsed.netloc}" != ORIGIN:
            return None
        path = parsed.path or "/"
    else:
        path = parsed.path
    path = unquote(path)
    if not path:
        return page  # pure fragment
    if path.startswith("/"):
        target = out / path.lstrip("/")
    else:
        target = (page.parent / path).resolve()
    if path.endswith("/") or target.is_dir():
        target = target / "index.html"
    return target


def main(out_dir):
    out = Path(out_dir).resolve()
    errors, warnings = [], []
    pages = sorted(out.rglob("*.html"))
    if not pages:
        print(f"No HTML files found in {out}")
        return 1

    titles = {}
    for page in pages:
        rel = page.relative_to(out).as_posix()
        p = PageParser()
        p.feed(page.read_text(encoding="utf-8"))
        robots = p.meta.get("robots", "")
        indexable = "noindex" not in robots

        def err(msg):
            errors.append(f"{rel}: {msg}")

        def warn(msg):
            warnings.append(f"{rel}: {msg}")

        # --- SEO essentials --------------------------------------------
        if not p.title.strip():
            err("missing <title>")
        elif len(p.title) > TITLE_MAX:
            warn(f"title is {len(p.title)} chars (> {TITLE_MAX}): {p.title!r}")
        desc = p.meta.get("description", "")
        if not desc:
            err("missing meta description")
        elif indexable and not DESC_MIN <= len(desc) <= DESC_MAX:
            warn(f"meta description is {len(desc)} chars (aim {DESC_MIN}-{DESC_MAX})")
        if p.h1 != 1:
            err(f"expected exactly one <h1>, found {p.h1}")
        if not p.canonical or not p.canonical.startswith(ORIGIN):
            err(f"canonical missing or not absolute: {p.canonical!r}")
        for key in ("og:title", "og:description", "og:image", "og:url"):
            if not p.meta.get(key):
                err(f"missing {key}")
        if p.imgs_without_alt:
            err(f"<img> without alt: {p.imgs_without_alt}")
        if not p.jsonld:
            err("missing JSON-LD")
        for block in p.jsonld:
            try:
                json.loads(block)
            except ValueError as exc:
                err(f"invalid JSON-LD: {exc}")
        if indexable:
            if p.title in titles:
                err(f"duplicate title with {titles[p.title]}")
            titles[p.title] = rel

        # --- Internal links and assets ----------------------------------
        for tag, attr, url in p.links:
            target = resolve(out, page, url)
            if target is None:
                continue
            frag = urlparse(url).fragment
            if not target.exists():
                err(f"broken {tag}[{attr}] -> {url}")
            elif frag and target.suffix == ".html":
                tp = p if target == page else PageParser()
                if tp is not p:
                    tp.feed(target.read_text(encoding="utf-8"))
                if frag not in tp.ids:
                    err(f"missing anchor #{frag} in {url}")

    # --- Sitemap and robots ------------------------------------------------
    sitemap = out / "sitemap.xml"
    if not sitemap.exists():
        errors.append("sitemap.xml missing")
    else:
        locs = re.findall(r"<loc>([^<]+)</loc>", sitemap.read_text(encoding="utf-8"))
        if not locs:
            errors.append("sitemap.xml has no <loc>")
        for loc in locs:
            target = resolve(out, out / "index.html", loc)
            if target is None or not target.exists():
                errors.append(f"sitemap.xml: {loc} does not exist")
    robots = out / "robots.txt"
    if not robots.exists() or "Sitemap:" not in robots.read_text(encoding="utf-8"):
        errors.append("robots.txt missing or without a Sitemap: line")
    if not (out / "CNAME").exists():
        errors.append("CNAME missing (GitHub Pages custom domain)")

    for w in warnings:
        print(f"WARN  {w}")
    for e in errors:
        print(f"ERROR {e}")
    print(f"Checked {len(pages)} HTML files: {len(errors)} error(s), {len(warnings)} warning(s).")
    return 1 if errors else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1] if len(sys.argv) > 1 else "output"))
