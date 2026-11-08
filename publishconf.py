"""Production settings, used by `make publish` and the GitHub Actions deploy."""

import os
import sys

sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from pelicanconf import *  # noqa: E402,F401,F403

SITEURL = "https://infratelier.com"
RELATIVE_URLS = False

FEED_ALL_ATOM = "feeds/all.atom.xml"

DELETE_OUTPUT_DIRECTORY = True

# Drafts are previewed locally only (`make html`), never published.
DRAFT_SAVE_AS = ""
DRAFT_PAGE_SAVE_AS = ""
