"""SEO helpers exposed to the Jinja templates (see pelicanconf.py).

- `jsonld(...)`      builds the schema.org JSON-LD graph of a page.
- `absolute_url`     filter: turns "/path" into "https://infratelier.com/path".
- `fr`               filter: French typography (narrow no-break spaces,
                     typographic apostrophes), applied to text nodes only.
"""

import json
import re

from markupsafe import Markup

import site_data as d

NNBSP = "\u202f"  # narrow no-break space, used before ; : ! ? and inside « »


# --------------------------------------------------------------------------
# French typography
# --------------------------------------------------------------------------
_TAG_RE = re.compile(r"(<[^>]+>)")
_SKIP_TAGS = ("code", "pre", "script", "style", "textarea")
_RULES = [
    # Typographic apostrophe: l'atelier -> l’atelier (raw or HTML-escaped)
    (re.compile(r"(?<=\w)(?:'|&#39;|&#x27;)(?=\w)"), "’"),
    # Keep "de A à Z" on one line
    (re.compile(r"\bA à Z\b"), "A\u00a0à\u00a0Z"),
    # Narrow no-break space before high punctuation (regular or no-break space before it)
    (re.compile("[ \u00a0]+([;:!?»])"), NNBSP + r"\1"),
    (re.compile("«[ \u00a0]+"), "«" + NNBSP),
    # A guillemet glued to a word still gets its space.
    (re.compile(r"«(?=\w)"), "«" + NNBSP),
    (re.compile(r"(?<=\w)»"), NNBSP + "»"),
]


def _fix_text(text):
    for pattern, repl in _RULES:
        text = pattern.sub(repl, text)
    return text


def french_typography(value):
    """Apply French spacing rules to text nodes only (tags, code untouched)."""
    if value is None:
        return ""
    was_markup = isinstance(value, Markup)
    parts = _TAG_RE.split(str(value))
    skip_depth = 0
    out = []
    for part in parts:
        if part.startswith("<") and part.endswith(">"):
            tag = part[1:].lstrip("/").split(None, 1)[0].rstrip(">").lower() if len(part) > 2 else ""
            if tag in _SKIP_TAGS:
                if part.startswith("</"):
                    skip_depth = max(0, skip_depth - 1)
                elif not part.endswith("/>"):
                    skip_depth += 1
            out.append(part)
        elif skip_depth or not part:
            out.append(part)
        else:
            out.append(_fix_text(part))
    result = "".join(out)
    return Markup(result) if was_markup else result


# --------------------------------------------------------------------------
# URLs
# --------------------------------------------------------------------------
def absolute_url(path, origin="https://infratelier.com"):
    """Return an absolute URL for a site path ("" or "/x/" or "x/")."""
    if path is None:
        path = ""
    path = str(path)
    if path.startswith(("http://", "https://")):
        return path
    return origin.rstrip("/") + "/" + path.lstrip("/")


# --------------------------------------------------------------------------
# JSON-LD
# --------------------------------------------------------------------------
def _ids(origin):
    return {
        "business": absolute_url("/#business", origin),
        "person": absolute_url("/a-propos/#florian-grignon", origin),
        "website": absolute_url("/#website", origin),
        "book": absolute_url("/#the-art-of-kubernetes", origin),
    }


def _business(origin, ids):
    b = d.BUSINESS
    node = {
        "@type": "ProfessionalService",
        "@id": ids["business"],
        "name": b["name"],
        "legalName": b["legal_name"],
        "slogan": b["tagline"],
        "description": b["description"],
        "url": absolute_url("/", origin),
        "email": b["email"],
        "logo": absolute_url(b["logo"], origin),
        "image": absolute_url(b["og_image"], origin),
        "address": {
            "@type": "PostalAddress",
            "streetAddress": b["street_address"],
            "addressLocality": b["locality"],
            "postalCode": b["postal_code"],
            "addressRegion": b["region"],
            "addressCountry": b["country"],
        },
        "areaServed": {"@type": "Country", "name": b["country_name"]},
        "founder": {"@id": ids["person"]},
        "knowsAbout": d.EXPERTISE,
        "knowsLanguage": "fr",
    }
    if d.LEGAL.get("siren"):
        node["identifier"] = {
            "@type": "PropertyValue",
            "propertyID": "SIREN",
            "value": d.LEGAL["siren"].replace(" ", ""),
        }
    if b.get("same_as"):
        node["sameAs"] = b["same_as"]
    return node


def _person(origin, ids):
    b = d.BUSINESS
    return {
        "@type": "Person",
        "@id": ids["person"],
        "name": b["founder"],
        "jobTitle": b["founder_title"],
        "image": absolute_url(b["portrait"], origin),
        "url": absolute_url("/a-propos/", origin),
        "worksFor": {"@id": ids["business"]},
        "knowsAbout": d.EXPERTISE,
        "address": {
            "@type": "PostalAddress",
            "addressLocality": b["locality"],
            "addressCountry": b["country"],
        },
    }


def _website(origin, ids):
    return {
        "@type": "WebSite",
        "@id": ids["website"],
        "url": absolute_url("/", origin),
        "name": d.BUSINESS["name"],
        "description": d.BUSINESS["description"],
        "inLanguage": "fr-FR",
        "publisher": {"@id": ids["business"]},
    }


def _book(origin, ids):
    bk = d.BOOK
    return {
        "@type": "Book",
        "@id": ids["book"],
        "name": bk["title"],
        "bookEdition": bk["edition"],
        "inLanguage": bk["language"],
        "author": {"@id": ids["person"]},
        "url": bk["url"],
        "image": absolute_url(bk["cover"], origin),
        "description": bk["pitch"],
    }


def _breadcrumb(origin, url, crumbs):
    items = [{"name": "Accueil", "url": absolute_url("/", origin)}]
    items += [{"name": n, "url": absolute_url(p, origin)} for n, p in crumbs]
    return {
        "@type": "BreadcrumbList",
        "@id": url + "#breadcrumb",
        "itemListElement": [
            {"@type": "ListItem", "position": i, "name": it["name"], "item": it["url"]}
            for i, it in enumerate(items, start=1)
        ],
    }


_PAGE_TYPES = {
    "home": "WebPage",
    "page": "WebPage",
    "formations": ["WebPage", "FAQPage"],
    "services": "WebPage",
    "about": "AboutPage",
    "contact": "ContactPage",
    "blog": "CollectionPage",
}


def jsonld(origin, path, name, description, kind="page", crumbs=None,
           date_published=None, date_modified=None, image=None):
    """Return a <script type="application/ld+json"> block for a page.

    origin  -- SITE_ORIGIN, e.g. "https://infratelier.com"
    path    -- page path relative to the site root ("" for the homepage)
    kind    -- home | page | formations | services | about | contact | blog | article
    crumbs  -- [(label, path), ...] after "Accueil", for the breadcrumb
    """
    ids = _ids(origin)
    url = absolute_url(path or "/", origin)
    graph = [_business(origin, ids), _person(origin, ids), _website(origin, ids)]

    if kind == "article":
        page = {
            "@type": "BlogPosting",
            "@id": url + "#article",
            "headline": name,
            "description": description,
            "url": url,
            "mainEntityOfPage": url,
            "inLanguage": "fr-FR",
            "author": {"@id": ids["person"]},
            "publisher": {"@id": ids["business"]},
            "isPartOf": {"@id": ids["website"]},
            "image": absolute_url(image or d.BUSINESS["og_image"], origin),
        }
        if date_published:
            page["datePublished"] = date_published
        if date_modified:
            page["dateModified"] = date_modified
    else:
        page = {
            "@type": _PAGE_TYPES.get(kind, "WebPage"),
            "@id": url + "#webpage",
            "url": url,
            "name": name,
            "description": description,
            "inLanguage": "fr-FR",
            "isPartOf": {"@id": ids["website"]},
            "about": {"@id": ids["business"]},
            "primaryImageOfPage": absolute_url(image or d.BUSINESS["og_image"], origin),
        }
        if date_modified:
            page["dateModified"] = date_modified

    if crumbs:
        bc = _breadcrumb(origin, url, crumbs)
        page["breadcrumb"] = {"@id": bc["@id"]}
        graph.append(bc)

    if kind in ("home", "about"):
        graph.append(_book(origin, ids))
        if kind == "about":
            page["mainEntity"] = {"@id": ids["person"]}

    if kind == "formations":
        page["mainEntity"] = [
            {
                "@type": "Question",
                "name": item["q"],
                "acceptedAnswer": {"@type": "Answer", "text": item["a"]},
            }
            for item in d.TRAINING_FAQ
        ]
        graph.append({
            "@type": "ItemList",
            "@id": url + "#catalogue",
            "name": "Catalogue des formations Infratelier",
            "itemListElement": [
                {
                    "@type": "ListItem",
                    "position": i,
                    "item": {
                        "@type": "Course",
                        "@id": url + "#" + t["id"],
                        "url": url + "#" + t["id"],
                        "name": t["title"],
                        "description": t["summary"],
                        "inLanguage": "fr",
                        "provider": {"@id": ids["business"]},
                        "teaches": t["topics"],
                        "audience": {"@type": "Audience", "audienceType": t["audience"]},
                    },
                }
                for i, t in enumerate(d.TRAININGS, start=1)
            ],
        })

    if kind == "services":
        graph.append({
            "@type": "OfferCatalog",
            "@id": url + "#services",
            "name": "Accompagnement en infrastructure",
            "itemListElement": [
                {
                    "@type": "Offer",
                    "itemOffered": {
                        "@type": "Service",
                        "@id": url + "#" + s["id"],
                        "name": s["title"],
                        "description": s["text"],
                        "provider": {"@id": ids["business"]},
                        "areaServed": {"@type": "Country", "name": d.BUSINESS["country_name"]},
                    },
                }
                for s in d.SERVICES
            ],
        })

    graph.insert(3, page)
    payload = json.dumps({"@context": "https://schema.org", "@graph": graph},
                         ensure_ascii=False, indent=None, separators=(",", ":"))
    # Neutralise "</" so the JSON can never close the <script> element.
    payload = payload.replace("</", "<\\/")
    return Markup('<script type="application/ld+json">' + payload + "</script>")
