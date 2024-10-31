AUTHOR = 'Florian Grignon'
SITENAME = 'Infratelier'
SITEURL = "https://infratelier.com"

PATH = "content"

TIMEZONE = 'Europe/Paris'

DEFAULT_LANG = 'fr'

# Feed generation is usually not desired when developing
FEED_ALL_ATOM = None
CATEGORY_FEED_ATOM = None
TRANSLATION_FEED_ATOM = None
AUTHOR_FEED_ATOM = None
AUTHOR_FEED_RSS = None

THEME = "themes/eduon"

FORMATTED_FIELDS = []

# Blogroll
LINKS = (
    ("Formation et Consulting autour de Kubernetes et des technologies Cloud", "#"),
)

# Social widget
SOCIAL = (
    ("You can add links in your config file", "#"),
    ("Another social link", "#"),
)

DEFAULT_PAGINATION = False

# Uncomment following line if you want document-relative URLs when developing
# RELATIVE_URLS = True

# Uncomment following line if you want document-relative URLs when developing
# RELATIVE_URLS = True
STATIC_PATHS = ['images', 'assets', 'extra']
EXTRA_PATH_METADATA = {'extra/CNAME': {'path': 'CNAME'},'extra/robots.txt': {'path': 'robots.txt'},}

SITEMAP = {
    "format": "xml",
    "priorities": {
        "articles": 0.5,
        "indexes": 0.5,
        "pages": 0.5
    },
    "changefreqs": {
        "articles": "weekly",
        "indexes": "daily",
        "pages": "monthly"
    },
    "exclude": [
        "^/noindex/",  # starts with "/noindex/"
        "/tag/",       # contains "/tag/"
        "\.json$",     # ends with ".json"
    ]
}
