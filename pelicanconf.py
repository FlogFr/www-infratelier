"""Pelican configuration for infratelier.com (development defaults).

`publishconf.py` imports this file and overrides what differs in production.
Structured site content (trainings, services, testimonials...) lives in
`site_data.py`; SEO helpers (JSON-LD) live in `seo.py`.
"""

import os
import sys
from datetime import date, datetime

sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from site_data import *  # noqa: E402,F401,F403  (UPPERCASE names become template globals)
import seo  # noqa: E402

AUTHOR = "Florian Grignon"
SITENAME = "Infratelier"
# Root-relative links in development; publishconf.py sets the real domain.
SITEURL = ""
# Absolute URL used for canonical links, Open Graph, JSON-LD and the sitemap.
# Always the production domain, even when building locally.
SITE_ORIGIN = "https://infratelier.com"

PATH = "content"
THEME = "themes/infratelier"
TIMEZONE = "Europe/Paris"
DEFAULT_LANG = "fr"
LOCALE = ("fr_FR.UTF-8", "fr_FR", "C.UTF-8", "C")
DEFAULT_DATE_FORMAT = "%d/%m/%Y"
CURRENT_YEAR = date.today().year

# --- Content layout -------------------------------------------------------
PAGE_PATHS = ["pages"]
ARTICLE_PATHS = ["blog"]
STATIC_PATHS = ["assets", "extra"]
EXTRA_PATH_METADATA = {
    "extra/CNAME": {"path": "CNAME"},
    "extra/robots.txt": {"path": "robots.txt"},
    "extra/favicon.ico": {"path": "favicon.ico"},
    "extra/site.webmanifest": {"path": "site.webmanifest"},
}

# --- Clean URLs (/formations/ instead of /pages/formations.html) ----------
PAGE_URL = "{slug}/"
PAGE_SAVE_AS = "{slug}/index.html"
ARTICLE_URL = "blog/{slug}/"
ARTICLE_SAVE_AS = "blog/{slug}/index.html"
DRAFT_URL = "drafts/{slug}/"
DRAFT_SAVE_AS = "drafts/{slug}/index.html"
DRAFT_PAGE_URL = "drafts/pages/{slug}/"
DRAFT_PAGE_SAVE_AS = "drafts/pages/{slug}/index.html"

# The homepage is a page (content/pages/accueil.md saved as index.html);
# the article listing lives under /blog/.
INDEX_SAVE_AS = "blog/index.html"
INDEX_URL = "blog/"
DIRECT_TEMPLATES = ["index", "sitemap"]
SITEMAP_SAVE_AS = "sitemap.xml"

# Thin, duplicate listing pages hurt SEO: do not generate them.
ARCHIVES_SAVE_AS = ""
AUTHOR_SAVE_AS = ""
AUTHORS_SAVE_AS = ""
CATEGORY_SAVE_AS = ""
CATEGORIES_SAVE_AS = ""
TAG_SAVE_AS = ""
TAGS_SAVE_AS = ""

DEFAULT_PAGINATION = False
SUMMARY_MAX_LENGTH = 40

# Feeds are only generated in production (see publishconf.py).
FEED_ALL_ATOM = None
CATEGORY_FEED_ATOM = None
TRANSLATION_FEED_ATOM = None
AUTHOR_FEED_ATOM = None
AUTHOR_FEED_RSS = None

MARKDOWN = {
    "extension_configs": {
        "markdown.extensions.extra": {},
        "markdown.extensions.toc": {"permalink": False},
        "markdown.extensions.smarty": {},
    },
    "output_format": "html5",
}

# Pages set an explicit `Slug:` so URLs never change when a title changes.
SLUGIFY_SOURCE = "basename"

# --- Plugins -------------------------------------------------------------
PLUGIN_PATHS = ["plugins"]
PLUGINS = ["french_typography"]

# --- Template helpers ------------------------------------------------------
JINJA_ENVIRONMENT = {
    "trim_blocks": True,
    "lstrip_blocks": True,
    # Escape everything by default; templates mark trusted HTML with |safe.
    "autoescape": True,
}
JINJA_GLOBALS = {
    "jsonld": seo.jsonld,
}
JINJA_FILTERS = {
    "absolute": seo.absolute_url,
    "fr": seo.french_typography,
}
# Cache-busting query string appended to the CSS/JS URLs.
ASSET_VERSION = datetime.now().strftime("%Y%m%d%H%M")

# Uncomment to get document-relative URLs when opening files directly.
# RELATIVE_URLS = True
