<!DOCTYPE html>
<html lang="en" dir="ltr"
  xmlns:schema="http://schema.org/"
  xmlns:og="http://ogp.me/ns#"
  xmlns:article="http://ogp.me/ns/article#"
  xmlns:content="http://purl.org/rss/1.0/modules/content/"
  xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
  class="no-js">

<head>
{{> part.head.html}}

</head>

<body>
    {{> part.nav.header.html}}

    <section class="hero is-large header-mountains">
        <div class="hero-body m-t-xl">
            <div class="container content has-text-right">
                <h1 class="title has-text-white has-text-classy has-text-weight-semibold is-1">Reliable Infrastructure Consultant</h1>
                <h2 class="subtitle has-text-white has-text-weight-light is-4">We guide your product to be more resilient, performant, and secure</h2>
            </div>
        </div>
    </section>

    <section class="section header-world">
        <div class="container">
            <div class="columns">
                <div class="column is-half is-offset-half content box">
                    <h1 class="title is-3 has-text-classy">Your product includes
                        your infrastructure</h1>
                    <p class="has-text-justified" style="line-height: 2rem;" >
                    Most of the companies realize the importance of the
                    infrastructure too late. Your product includes also your
                    infrastructure, and your infrastructure is your product.
                    Thinking of the infrastructure as important as your product
                    from day one saves you money and time.
                    </p>
                </div>
            </div>
            <div class="columns">
                <div class="column is-half content box">
                    <h1 class="title is-3 has-text-classy">Your product up and
                        running 24h a day, 7 days a week</h1>
                    <h2 class="subtitle has-text-primary has-text-weight-light
                    is-5"><i class="uil uil-cog"></i> High Availability</h2>
                    <p class="has-text-justified" style="line-height: 2rem;" >
                    Relying on well-proven technologies, we augment the
                    availability of your product by setting up HA Proxy along the
                    right algorithm and process to ensure tested failover.
                    </p>
                </div>
            </div>
            <div class="columns">
                <div class="column is-half is-offset-half content box">
                    <h1 class="title is-3 has-text-classy">Keep your data safe</h1>
                    <h2 class="subtitle has-text-primary has-text-weight-light
                        is-5"><i class="uil uil-shield-check"></i> Security</h2>
                    <p class="has-text-justified" style="line-height: 2rem;" >
                    Data is key for almost every product. If this is the case,
                    you should be careful about how you process them. We to
                    review thoroughly the pipeline of the data processing, and
                    we give you a detailed report on how to improve it.
                    </p>
                </div>
            </div>
            <div class="columns">
                <div class="column is-half content box">
                    <h1 class="title is-3 has-text-classy">Nobody like to wait, even more your clients</h1>
                    <h2 class="subtitle has-text-primary has-text-weight-light
                        is-5"><i class="uil uil-analysis"></i> Performance</h2>
                    <p class="has-text-justified" style="line-height: 2rem;" >
                    We listen carefuly to your client expectation from your
                    product. If your product does not have any contractual SLA,
                    we help you writing them down, and make sure you're
                    respecting them.
                    </p>
                </div>
            </div>
            <div class="columns">
                <div class="column is-half is-offset-half content box">
                    <h1 class="title is-3 has-text-classy">Control your future</h1>
                    <h2 class="subtitle has-text-primary has-text-weight-light is-5"><i class="uil uil-chart"></i> Monitoring & Capacity Planning</h2>
                    <p class="has-text-justified" style="line-height: 2rem;" >
                    Thanks to all the monitoring metrics setup for your
                    infrastructure, you will have a perfect insight of your
                    infrastructure to take the right decision for your future.
                    </p>
                </div>
            </div>
        </div>
    </section>


    <section class="hero is-medium header-invitation">
        <div class="hero-body m-t-xl">
            <div class="container columns content has-text-right">
                <div class="column is-half is-offset-half">
                    <h1 class="title has-text-white has-text-classy has-text-weight-light is-1">Newsletter</h1>
                    <h2 class="subtitle has-text-white has-text-weight-light
                        is-4">Be the first one to know all about the infrastructure</h2>

                    <form class="form is-marginless" role="form" action="/newsletter" method="post">
                        <div class="secret-input" style="display: none;"></div>
                        <div class="field has-addons">
                            <div class="control has-icons-left" style="100%" >
                                <input class="input is-medium" type="email" name="email" placeholder="email.adress@gmail.com">
                                <span class="icon is-small is-left">
                                    <i class="uil uil-envelope"></i>
                                </span>
                            </div>
                            <div class="control">
                                <button class="button is-medium" type="submit"><i class="uil uil-check"></i> Subscribe</button>
                            </div>
                        </div>
                    </form>
                </div>
            </div>
        </div>
    </section>


    {{> part.footer.html}}

    <script>
let backendVariables = {{{frontEndVariables}}};
    </script>
    <script src="{{staticPath}}/dist/index.bundle.js"></script>
</body>
</html>
