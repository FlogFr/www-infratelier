"""Pelican plugin: apply French typography to every generated HTML file.

Runs `seo.french_typography` on each written .html file, so literal text in
templates gets the same treatment as content: narrow no-break spaces before
; : ! ? and inside « », typographic apostrophes, "A à Z" kept together.
Only text nodes are touched (never tags, attributes, <script>, <style>,
<code> or <pre>), and the transformation is idempotent.
"""

import logging

from pelican import signals

import seo

log = logging.getLogger(__name__)


def _process(path, context):
    if not str(path).endswith(".html"):
        return
    with open(path, encoding="utf-8") as fh:
        html = fh.read()
    fixed = seo.french_typography(html)
    if fixed != html:
        with open(path, "w", encoding="utf-8") as fh:
            fh.write(fixed)
        log.debug("french_typography: fixed %s", path)


def register():
    signals.content_written.connect(_process)
