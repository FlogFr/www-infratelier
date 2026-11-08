"""Structured content for infratelier.com.

Every UPPERCASE name defined here is imported by `pelicanconf.py` and becomes
available in all Jinja templates. The same data feeds the visible HTML and
the JSON-LD structured data (see `seo.py`), so editing a training or a
service here updates both at once.

Writing rules (see claude/rules/content.md):
- French, "je" voice (Florian speaks to the visitor), "vous" for the visitor.
- Plain text only: no HTML in these strings. French typography (narrow
  no-break spaces before ; : ! ?) is applied at render time by the `fr` filter.
- Never invent facts (durations, prices, certifications, client names).
"""

# --- Business identity ----------------------------------------------------
BUSINESS = {
    "name": "Infratelier",
    "legal_name": "Infratelier SASU",
    "tagline": "Artisan de l'infrastructure Cloud & Linux",
    "description": (
        "Infratelier est le cabinet indépendant de Florian Grignon, expert en "
        "infrastructure informatique : formations Cloud, Kubernetes et Linux, "
        "et accompagnement sur mesure pour concevoir, fiabiliser et optimiser "
        "votre infrastructure."
    ),
    "email": "contact@infratelier.com",
    "locality": "Nantes",
    "postal_code": "44000",
    "region": "Pays de la Loire",
    "country": "FR",
    "country_name": "France",
    "area_served": "France (sur site ou à distance)",
    "founder": "Florian Grignon",
    "founder_title": "Expert indépendant en infrastructure Cloud & Linux, formateur et auteur",
    "logo": "/assets/img/Infratelier-Icon.png",
    "portrait": "/assets/img/Florian-Grignon-Portrait.webp",
    "og_image": "/assets/img/og-infratelier.jpg",
    # Public profiles that belong to Infratelier / Florian Grignon.
    # Add LinkedIn, Malt, GitHub... here: they feed `sameAs` in JSON-LD and the footer.
    "same_as": [],
}

# Legal notice data (mentions légales, required by French law - LCEN art. 6).
# Fill in the missing values: fields left to None are simply not displayed.
LEGAL = {
    "form": "Société par actions simplifiée unipersonnelle (SASU)",
    "capital": None,  # e.g. "1 000 €"
    "address": None,  # registered office, e.g. "1 rue Exemple, 44000 Nantes"
    "siren": None,  # e.g. "123 456 789"
    "rcs": None,  # e.g. "RCS Nantes 123 456 789"
    "vat": None,  # e.g. "FR00123456789"
    "publisher": "Florian Grignon, Président",
    "host_name": "GitHub, Inc. (GitHub Pages)",
    "host_address": "88 Colin P. Kelly Jr. Street, San Francisco, CA 94107, États-Unis",
    "host_url": "https://github.com",
}

# --- Main navigation (slug of the target page, label) ---------------------
NAV = [
    ("formations", "Formations"),
    ("accompagnement", "Accompagnement"),
    ("a-propos", "À propos"),
]

# --- The Kubernetes book (promoted by the welcome dialog) -----------------
BOOK = {
    "title": "The Art of Kubernetes",
    "edition": "French Edition",
    "url": "https://theartofkubernetes.com/",
    "cover": "/assets/img/TheArtOfKubernetes-FirstPage.webp",
    "cover_width": 315,
    "cover_height": 400,
    "pitch": (
        "Près de 400 pages pour comprendre Kubernetes en profondeur : ses "
        "concepts, son architecture et les bonnes pratiques pour l'exploiter "
        "sereinement en production."
    ),
    "language": "fr",
    # UTM parameters let theartofkubernetes.com measure traffic coming from here.
    "utm_dialog": "?utm_source=infratelier.com&utm_medium=referral&utm_campaign=welcome-dialog",
    "utm_section": "?utm_source=infratelier.com&utm_medium=referral&utm_campaign=book-section",
}

# --- Training catalogue ---------------------------------------------------
TRAININGS = [
    {
        "id": "kubernetes-utilisateur",
        "short": "Kubernetes pour les développeurs",
        "family": "Kubernetes",
        "title": "Kubernetes pour les développeurs et utilisateurs",
        "summary": (
            "Déployer, exposer et dépanner vos applications sur Kubernetes, "
            "en toute autonomie."
        ),
        "audience": "Développeurs, équipes produit, DevOps débutants",
        "topics": [
            "Conteneurs, Pods, Deployments et Services",
            "Configuration : ConfigMaps, Secrets, variables d'environnement",
            "Exposition des applications : Ingress, réseau, DNS",
            "Packaging avec Helm",
            "Observer et dépanner une application",
        ],
    },
    {
        "id": "kubernetes-administrateur",
        "short": "Kubernetes pour les administrateurs",
        "family": "Kubernetes",
        "title": "Kubernetes pour les administrateurs",
        "summary": (
            "Installer, sécuriser et opérer des clusters Kubernetes fiables, "
            "du poste de travail à la production."
        ),
        "audience": "Administrateurs systèmes, SRE, équipes plateforme",
        "topics": [
            "Architecture du control plane et des nœuds",
            "Réseau (CNI), stockage (CSI) et ressources",
            "Sécurité : RBAC, politiques réseau, gestion des secrets",
            "Haute disponibilité, sauvegardes et mises à jour",
            "Distributions : Kubernetes « vanilla », OpenShift, Rancher",
        ],
    },
    {
        "id": "linux",
        "short": "Administration Linux",
        "family": "Linux",
        "title": "Administration système Linux",
        "summary": (
            "Maîtriser le socle de toute infrastructure : le système Linux, "
            "de la ligne de commande au diagnostic en production."
        ),
        "audience": "Développeurs, techniciens, futurs administrateurs",
        "topics": [
            "Shell, fichiers, droits et processus",
            "Services avec systemd, journaux et planification",
            "Réseau, stockage et systèmes de fichiers",
            "Sécurisation d'un serveur",
            "Automatisation avec Bash",
        ],
    },
    {
        "id": "cloud-public",
        "short": "Google Cloud et AWS",
        "family": "Cloud",
        "title": "Cloud public : Google Cloud et AWS",
        "summary": (
            "Concevoir une infrastructure cloud solide et maîtrisée en coûts "
            "sur Google Cloud Platform ou Amazon Web Services."
        ),
        "audience": "Équipes techniques qui migrent ou démarrent dans le cloud",
        "topics": [
            "Identités et accès (IAM)",
            "Réseau, calcul et stockage",
            "Services managés : bases de données, Kubernetes managé",
            "Supervision et maîtrise des coûts",
        ],
    },
    {
        "id": "infrastructure-as-code",
        "short": "OpenTofu et Terraform",
        "family": "Automatisation",
        "title": "Infrastructure as Code avec OpenTofu et Terraform",
        "summary": (
            "Décrire, versionner et faire évoluer votre infrastructure comme "
            "du code, en équipe et en toute confiance."
        ),
        "audience": "Administrateurs, DevOps, développeurs",
        "topics": [
            "Providers, ressources et state",
            "Modules réutilisables",
            "Workflows d'équipe et revue de code",
            "Intégration continue de l'infrastructure",
        ],
    },
    {
        "id": "observabilite",
        "short": "Prometheus, Grafana et Loki",
        "family": "Observabilité",
        "title": "Observabilité avec Prometheus, Grafana et Loki",
        "summary": (
            "Voir ce qui se passe vraiment dans votre infrastructure, être "
            "alerté au bon moment et anticiper vos besoins."
        ),
        "audience": "SRE, administrateurs, équipes d'exploitation",
        "topics": [
            "Métriques et requêtes PromQL",
            "Tableaux de bord Grafana",
            "Centralisation des logs avec Loki",
            "Alerting et objectifs de service (SLO)",
        ],
    },
    {
        "id": "ci-cd-gitops",
        "short": "CI/CD et GitOps",
        "family": "Automatisation",
        "title": "CI/CD et GitOps",
        "summary": (
            "Livrer plus souvent et plus sereinement grâce à des pipelines "
            "fiables et au déploiement GitOps."
        ),
        "audience": "Développeurs, DevOps, équipes plateforme",
        "topics": [
            "Pipelines GitLab CI et Jenkins",
            "Construction et publication d'images de conteneurs",
            "Déploiement continu sur Kubernetes avec Argo CD",
            "Stratégies de déploiement et retour arrière",
        ],
    },
]

TRAINING_FORMATS = [
    {
        "title": "Sur mesure",
        "text": (
            "Chaque programme est construit avec vous, à partir de votre "
            "contexte, du niveau des participants et de vos objectifs."
        ),
    },
    {
        "title": "Tous niveaux",
        "text": (
            "Du premier contact avec Linux à l'exploitation avancée de "
            "Kubernetes : la formation part de là où en est votre équipe."
        ),
    },
    {
        "title": "Présentiel ou à distance",
        "text": (
            "Dans vos locaux, partout en France, ou en visioconférence. "
            "Les supports restent accessibles après la formation."
        ),
    },
    {
        "title": "Orientée pratique",
        "text": (
            "Une large place aux exercices sur de vrais environnements, "
            "pour repartir avec des réflexes, pas seulement des slides."
        ),
    },
]

TRAINING_FAQ = [
    {
        "q": "Les formations sont-elles adaptées aux débutants ?",
        "a": (
            "Oui. Chaque formation est ajustée au niveau des participants. "
            "Nous faisons le point ensemble avant de démarrer pour que "
            "personne ne soit perdu, ni ne s'ennuie."
        ),
    },
    {
        "q": "Peut-on suivre une formation à distance ?",
        "a": (
            "Oui. Toutes les formations peuvent se dérouler en présentiel, "
            "dans vos locaux, ou en visioconférence."
        ),
    },
    {
        "q": "Peut-on construire un programme spécifique à notre équipe ?",
        "a": (
            "C'est même la règle : je construis le programme à partir de "
            "vos outils, de votre infrastructure et de vos objectifs, et "
            "je peux combiner plusieurs thèmes du catalogue."
        ),
    },
    {
        "q": "Comment obtenir un devis ?",
        "a": (
            "Écrivez-moi à contact@infratelier.com en précisant le thème, "
            "le nombre de participants et leur niveau. Je vous réponds "
            "personnellement avec une proposition de programme."
        ),
    },
]

# --- Consulting / support offer --------------------------------------------
SERVICES = [
    {
        "id": "audit",
        "title": "Audit et diagnostic",
        "text": (
            "Un état des lieux honnête de votre infrastructure : "
            "architecture, sécurité, fiabilité, coûts. Vous savez où vous "
            "en êtes et par où commencer."
        ),
    },
    {
        "id": "couts",
        "title": "Optimisation des coûts cloud",
        "text": (
            "Dimensionnement au juste besoin, suppression du gaspillage, "
            "choix des bons services : votre facture cloud reflète enfin "
            "l'usage réel."
        ),
    },
    {
        "id": "kubernetes",
        "title": "Plateformes Kubernetes",
        "text": (
            "Conception, migration ou remise à niveau de clusters et de "
            "plateformes Kubernetes, sur le cloud ou sur vos serveurs."
        ),
    },
    {
        "id": "fiabilite",
        "title": "Fiabilité et observabilité",
        "text": (
            "Supervision, alerting, gestion des incidents et haute "
            "disponibilité, pour un produit disponible quand vos clients "
            "en ont besoin."
        ),
    },
    {
        "id": "securite",
        "title": "Sécurité et conformité",
        "text": (
            "Durcissement des systèmes, gestion des accès et des secrets, "
            "sauvegardes et plan de reprise : vos données sont protégées."
        ),
    },
    {
        "id": "automatisation",
        "title": "Automatisation",
        "text": (
            "Infrastructure as Code, pipelines CI/CD et GitOps : moins de "
            "tâches manuelles, des changements traçables et reproductibles."
        ),
    },
]

# The craft approach, shared by trainings and consulting.
PROCESS_STEPS = [
    {
        "title": "Écouter",
        "text": (
            "Je prends le temps de comprendre votre produit, votre équipe "
            "et vos contraintes. Vos critères deviennent le cahier des "
            "charges."
        ),
    },
    {
        "title": "Diagnostiquer",
        "text": (
            "J'analyse l'existant, sans a priori technologique, pour "
            "identifier ce qui compte vraiment."
        ),
    },
    {
        "title": "Concevoir",
        "text": (
            "Je dessine une solution taillée pour vous, pas une solution "
            "sur étagère, et je vous explique chaque choix."
        ),
    },
    {
        "title": "Réaliser",
        "text": (
            "Je mets les mains dans le cambouis : mise en œuvre, tests, "
            "mise en production. Un seul interlocuteur, de A à Z."
        ),
    },
    {
        "title": "Transmettre",
        "text": (
            "Documentation et formation de vos équipes : vous restez "
            "autonomes et maîtres de votre infrastructure."
        ),
    },
]

# "Your criteria": the qualities a client can prioritise.
CRITERIA = [
    ("Adaptée à votre budget", "Chaque euro dépensé doit servir votre produit."),
    ("Performante", "Des temps de réponse à la hauteur de vos clients."),
    ("Résiliente", "Elle encaisse les pannes sans que vos clients le voient."),
    ("Fiable", "Des déploiements et une exploitation sans mauvaise surprise."),
    ("Sécurisée", "Vos données et vos accès sont protégés."),
    ("Conforme", "Elle respecte les exigences de votre secteur."),
    ("Flexible", "Elle évolue au rythme de votre activité."),
    ("Simple à maintenir", "Votre équipe la comprend et la fait vivre."),
]

# --- Social proof ------------------------------------------------------------
TESTIMONIALS = [
    # Quotes are verbatim: never rewrite what a client said.
    {
        "quote": (
            "Florian est quelqu'un de très sérieux et agréable. Nous avions "
            "une infrastructure sur GCP qui méritait une révision et il a "
            "rendu l'ensemble bien plus cohérent et scalable. Sans compter "
            "les améliorations de performances nous avons également vu une "
            "baisse significative sur le prix de nos services cloud "
            "(optimisation des ressources efficace). Si c'était à refaire je "
            "confierai directement la gestion de notre infrastructure à "
            "Florian !"
        ),
        "name": "Laurent Gilly",
        "role": "Engineering Manager",
        "photo": "/assets/img/Laurent-Gilly-Portrait.webp",
    },
    {
        "quote": (
            "Nous devions mettre à jour notre pipeline CI/CD complexe ainsi "
            "que notre infrastructure. Dès le premier jour, Florian Grignon a "
            "su proposer une solution technique adaptée et l'a mise en œuvre "
            "efficacement. Son expertise sur les meilleures pratiques en "
            "matière de Kubernetes et d'infrastructure nous a pleinement "
            "satisfaits. Travailler avec lui est une décision que vous ne "
            "regretterez pas."
        ),
        "name": "Iñigo Imaz Chacón",
        "role": "Main Software Developer",
        "photo": "/assets/img/Inigo-Imaz-Chacon-Portrait.webp",
    },
    {
        "quote": (
            "Au cours de notre collaboration, Florian s'est avéré être un SRE "
            "compétent et fiable. Grâce à son approche proactive de la "
            "gestion d'incidents ainsi que sa capacité à travailler sous "
            "pression, il a démontré être un acteur clé au sein de son "
            "équipe. En plus de cela, il a également fait preuve d'un "
            "excellent esprit d'équipe et d'une grande capacité à communiquer "
            "de manière claire et efficace. J'ai hâte de travailler de "
            "nouveau avec lui"
        ),
        "name": "Mehdi Brebion",
        "role": "Security Architect",
        "photo": "/assets/img/Mehdi-Brebion-Portrait.webp",
    },
]

# --- Technologies (logo files are 200x40 webp in content/assets/img) ------
TECHNOLOGIES = [
    ("Kubernetes", "Logo-Kubernetes.webp"),
    ("OpenShift", "Logo-OpenShift.webp"),
    ("Rancher", "Logo-Rancher.webp"),
    ("Google Cloud", "Logo-GCP.webp"),
    ("AWS", "Logo-AWS.webp"),
    ("OpenTofu", "Logo-OpenTofu.webp"),
    ("Terraform", "Logo-Terraform.webp"),
    ("Argo CD", "Logo-ArgoCD.webp"),
    ("GitLab", "Logo-Gitlab.webp"),
    ("Jenkins", "Logo-Jenkins.webp"),
    ("Prometheus", "Logo-Prometheus.webp"),
    ("Grafana", "Logo-Grafana.webp"),
    ("Loki", "Logo-Loki.webp"),
    ("MinIO", "Logo-MinIO.webp"),
]

# Topics used for `knowsAbout` in JSON-LD.
EXPERTISE = [
    "Infrastructure informatique",
    "Cloud computing",
    "Kubernetes",
    "Linux",
    "Google Cloud Platform",
    "Amazon Web Services",
    "Infrastructure as Code",
    "OpenTofu",
    "Terraform",
    "DevOps",
    "Site Reliability Engineering",
    "Observabilité",
    "Optimisation des coûts cloud",
]
