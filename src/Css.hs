module Css where

import Protolude hiding (rotate, rem, div, (**), not)

import Control.Concurrent.STM.TMVar
import Data.HashMap.Strict
import Clay
import qualified Clay.Media as Q
import Clay.Stylesheet
import Data.Settings
import SharedEnv
import HandlerM


-- Colors
primaryColor :: Color
primaryColor = rgb 255 168 0

lightPrimaryColor :: Color
lightPrimaryColor = lighten 0.5 primaryColor

lightLightPrimaryColor :: Color
lightLightPrimaryColor = lighten 0.5 lightPrimaryColor

darkPrimaryColor :: Color
darkPrimaryColor = darken 0.5 primaryColor

darkDarkPrimaryColor :: Color
darkDarkPrimaryColor = darken 0.5 darkPrimaryColor

secondaryColor :: Color
secondaryColor = rgb 251 180 46

lightSecondaryColor :: Color
lightSecondaryColor = lighten 0.5 secondaryColor

lightLightSecondaryColor :: Color
lightLightSecondaryColor = lighten 0.5 lightSecondaryColor

darkSecondaryColor :: Color
darkSecondaryColor = darken 0.1 secondaryColor

darkDarkSecondaryColor :: Color
darkDarkSecondaryColor = darken 0.5 darkSecondaryColor

facebookBlueColor :: Color
facebookBlueColor = rgb 66 103 178

blackColor :: Color
blackColor = rgb 0 0 0

blackTransparentColor :: Color
blackTransparentColor = rgba 0 0 0 0

greyColor :: Color
greyColor = rgb 80 80 80

darkGreyColor :: Color
darkGreyColor = rgb 65 65 65

lightGreyColor :: Color
lightGreyColor = lighten 0.5 greyColor

lightLightGreyColor :: Color
lightLightGreyColor = lighten 0.5 lightGreyColor

whiteColor :: Color
whiteColor = rgb 255 255 255

textAlignLast :: TextAlign -> Css
textAlignLast = key "text-align-last"

gridArea :: Text -> Css
gridArea = key "grid-area"

gridTemplateColumns :: Text -> Css
gridTemplateColumns = key "grid-template-columns"

gridTemplateRows :: Text -> Css
gridTemplateRows = key "grid-template-rows"

gridTemplateAreas :: Text -> Css
gridTemplateAreas = key "grid-template-areas"

gridGap :: Size LengthUnit -> Css
gridGap = key "grid-gap"

fonts :: Css
fonts = do
  fontFace $ do
    fontFamily ["Futura"] []
    fontFaceSrc
      [ FontFaceSrcUrl "/static/fonts/futura-md-bt_251.ttf" (Just TrueType)
      ]
  fontFace $ do
    fontFamily ["Roboto"] []
    fontFaceSrc
      [ FontFaceSrcUrl "/static/fonts/KFOmCnqEu92Fr1Mu72xKOzY.woff2" (Just WOFF2)
      ]
  fontFace $ do
    fontFamily ["Lato"] []
    fontFaceSrc
      [ FontFaceSrcUrl "/static/fonts/S6uyw4BMUTPHjxAwXjeu.woff2" (Just WOFF2)
      ]
  fontFace $ do
    fontFamily ["Open Sans"] []
    fontFaceSrc
      [ FontFaceSrcUrl "/static/fonts/mem8YaGs126MiZpBA-UFVZ0b.woff2" (Just WOFF2)
      ]
  fontFace $ do
    fontFamily ["Merriweather"] []
    fontFaceSrc
      [ FontFaceSrcUrl "/static/fonts/Merriweather.woff2" (Just WOFF2)
      ]
  fontFace $ do
    fontFamily ["SansForgetica"] []
    fontFaceSrc
      [ FontFaceSrcUrl "/static/fonts/SansForgetica-Regular.otf" (Just OpenType)
      ]
  fontFace $ do
    fontFamily ["Courgette"] []
    fontFaceSrc
      [ FontFaceSrcUrl "/static/fonts/Courgette.woff2" (Just WOFF2)
      ]

iconCSS :: Css
iconCSS = do
  star # byClass "icon-left" ? do
    iconCSS'
  star # byClass "icon-right" ? do
    iconCSS'
  star # byClass "icon-python" ? do
    iconCSS'
  star # byClass "icon-haskell" ? do
    iconCSS'
  star # byClass "icon-postgresql" ? do
    iconCSS'
  star # byClass "icon-react" ? do
    iconCSS'
  star # byClass "icon-elm" ? do
    iconCSS'
  where iconCSS' :: Css
        iconCSS' = do
          position relative
          display inlineBlock
          backgroundImage (url "/static/icons.png")
          backgroundSize cover
          verticalAlign middle
          overflow hidden
          whiteSpace nowrap
          textIndent (indent (pct 100))

fromCarouselAppearIn :: Css
fromCarouselAppearIn = do
  left (pct (-110))
  visibility hidden

midCarouselAppearIn :: Css
midCarouselAppearIn = do
  left (pct 0)
  visibility visible

toCarouselAppearIn :: Css
toCarouselAppearIn = do
  left (pct 110)
  visibility hidden

visibleAnimations :: Css
visibleAnimations =
  keyframes "carousel-appear-in" [
    (0 / nbElementInCarousel, fromCarouselAppearIn)
    , (15 / nbElementInCarousel, midCarouselAppearIn)
    , (85 / nbElementInCarousel, midCarouselAppearIn)
    , (100 / nbElementInCarousel, toCarouselAppearIn)
    , (100, toCarouselAppearIn)]
  where nbElementInCarousel = 4


carouselAnimations :: Css
carouselAnimations = do
  animations [
    ("carousel-appear-in", (sec 40), linear, (sec 0), infinite, normal, backwards)
    ]

defaultFonts :: Css
defaultFonts = do
  h1 ? do
    fontFamily ["Roboto"] [sansSerif]
    fontSize (rem 1.2)
    lineHeight (rem 2)
    color blackColor
    padding (rem 0) (rem 0.5) (rem 0) (rem 0.5)
    textTransform capitalize
  h2 ? do
    fontFamily ["Roboto"] [sansSerif]
    fontSize (rem 1.0)
    lineHeight (rem 4)
    color greyColor
    padding (rem 0) (rem 0.5) (rem 0) (rem 0.5)
    textTransform capitalize
  h3 ? do
    fontFamily ["Roboto"] [sansSerif]
    fontSize (rem 1.0)
    lineHeight (rem 4)
    color greyColor
    padding (rem 0) (rem 0.5) (rem 0) (rem 0.5)
    textTransform capitalize
  h4 ? do
    fontFamily ["Roboto"] [sansSerif]
    fontSize (rem 1.0)
    lineHeight (rem 4)
    color greyColor
    padding (rem 0) (rem 0.5) (rem 0) (rem 0.5)
    textTransform capitalize
  li ? do
    fontFamily ["Roboto"] [sansSerif]
    fontSize (rem 1)
    lineHeight (rem 1.5)
  th ? do
    fontFamily ["Open Sans"] [sansSerif]
    fontSize (rem 1)
    lineHeight (rem 1.2)
    color greyColor
    padding (rem 0) (rem 0.5) (rem 0) (rem 0.5)
    textTransform capitalize
  td ? do
    fontFamily ["Open Sans"] [sansSerif]
    fontSize (rem 1)
    lineHeight (rem 1.2)
    color greyColor
    padding (rem 0) (rem 0.5) (rem 0) (rem 0.5)
    textTransform capitalize
  p ? do
    fontFamily ["Open Sans"] [sansSerif]
    fontSize (rem 1)
    lineHeight (rem 1.2)
    color greyColor
    padding (rem 0) (rem 0.5) (rem 0) (rem 0.5)
  label ? do
    fontFamily ["Open Sans"] [sansSerif]
    fontSize (rem 1)
    lineHeight (rem 1.2)
    color greyColor
    padding (rem 0) (rem 0.5) (rem 0) (rem 0.5)
  a ? do
    textDecoration none

listCSS :: Css
listCSS = do
  ul ? do
    li ? do
      a ? do
        color black
        fontWeight bold

tableCSS :: Css
tableCSS = do
  table ? do
    borderCollapse collapse
    tr |> td ? do
      padding (rem 0.3) (rem 0.3) (rem 0.3) (rem 0.3)
      margin (rem 0) (rem 0) (rem 0) (rem 0)
      border solid (rem 0.1) lightGreyColor
    span # byClass "logo-delete" ? do
      height (rem 1.5)
      width (rem 1.5)
      backgroundPosition (positioned (rem (-27)) (rem 0))
    span # byClass "logo-locator" ? do
      height (rem 1.5)
      width (rem 1.5)
      backgroundPosition (positioned (rem (-28.5)) (rem 0))
    span # byClass "logo-document" ? do
      height (rem 1.5)
      width (rem 1.5)
      backgroundPosition (positioned (rem (-30)) (rem 0))
    span # byClass "logo-update" ? do
      height (rem 1.5)
      width (rem 1.5)
      backgroundPosition (positioned (rem (-31.5)) (rem 0))
    span # byClass "logo-sell" ? do
      height (rem 1.5)
      width (rem 1.5)
      backgroundPosition (positioned (rem (-33)) (rem 0))

defaultCSS :: Css
defaultCSS = do
  defaultFonts
  listCSS
  iconCSS
  tableCSS
  ul ? do
    margin (rem 0) (rem 0) (rem 0) (rem 0)
    padding (rem 0) (rem 0) (rem 0) (rem 0)
  li ? do
    margin (rem 0) (rem 0) (rem 0) (rem 0)
    padding (rem 0) (rem 0) (rem 0) (rem 0)
  a # hover ? do
    cursor pointer
  td ? do
    position relative
  a # byClass "anchor" ? do
    display block
    position relative
    top (rem (-3))
    visibility hidden
  div ? do
    position relative

containerImgTagCSS :: Css
containerImgTagCSS = do
  div # byClass "container-img-tag" ? do
    display inlineBlock
    overflow hidden
    width (rem 5)
    height (rem 5)
    margin (rem 0.5) (rem 0.5) (rem 0.5) (rem 0.5)
    borderRadius (rem 5) (rem 5) (rem 5) (rem 5)
    border solid (rem 0.1) greyColor
    boxShadow . pure $ bsColor primaryColor $ shadowWithSpread (rem 0) (rem 0) (rem 0) (rem 0.2)
  div # byClass "container-img-tag" # hover ? do
    border solid (rem 0.1) primaryColor
    boxShadow . pure $ bsColor greyColor $ shadowWithSpread (rem 0) (rem 0) (rem 0) (rem 0.2)
  div # byClass "container-img-tag" |> img ? do
    width (pct 100)
    height (pct 100)

containerImgProfileCSS :: Css
containerImgProfileCSS = do
  div # byClass "container-img-profile" ? do
    display inlineBlock
    overflow hidden
    width (rem 3)
    height (rem 3)
    margin (rem 0.5) (rem 0.5) (rem 0.5) (rem 0.5)
    borderRadius (rem 3) (rem 3) (rem 3) (rem 3)
    border solid (rem 0.1) greyColor
    boxShadow . pure $ bsColor primaryColor $ shadowWithSpread (rem 0) (rem 0) (rem 0) (rem 0.2)
  div # byClass "container-img-profile" # hover ? do
    border solid (rem 0.1) primaryColor
    boxShadow . pure $ bsColor greyColor $ shadowWithSpread (rem 0) (rem 0) (rem 0) (rem 0.2)
  div # byClass "container-img-profile" |> img ? do
    width (pct 100)
    height (pct 100)

emailLinkCss :: Css
emailLinkCss = do
  div # byClass "container-email-link" ? do
    zIndex (-100)
    position fixed
    right (rem 1)
    bottom (rem (-2))
  div # byClass "container-email-link" # hover ? do
    -- left (rem 2)
    -- animation for the email link
    keyframes "email-link" [
      (    0, bottom (rem (-2)))
      , (100, bottom (rem 0))]
    animations [
      ("email-link", (ms 100), linear, (sec 0), iterationCount 1, normal, backwards)
      ]
    bottom (rem 0)
  div # byClass "container-email-link" |> a ? do
    span ? do
      height (rem 4)
      width (rem 4)
      backgroundPosition (positioned (rem 0) (rem 0))

navigationCSS :: Css
navigationCSS = do
  div # byClass "navigation" ? do
    zIndex 100
    gridArea "navigation"
    position relative
    div # byClass "container-side-text" ? do
      position absolute
      left (rem 3)
      top (rem 0)
      query Q.screen [Q.minWidth (px 640)] $ do
        transform $ rotate (deg (-90))
        height (rem 6)
        width (rem 36)
        top (rem 9)
        left (rem (-10))
      h1 ? do
        -- margin (rem 25) (rem 0) (rem 0) (rem 0)
        fontFamily ["Futura"] [sansSerif]
        fontSize (rem 3)
        color $ setA 0.7 primaryColor
        query Q.screen [Q.minWidth (px 640)] $ do
          fontSize (rem 6)
  emailLinkCss

structureCSS :: Css
structureCSS = do
  body ? do
    display grid
    background $ rgb 224 224 224
    gridTemplateColumns "1fr"
    gridTemplateRows "3rem auto 8rem"
    gridTemplateAreas "\"header\" \"main\" \"footer\""
    gridGap (px 0)

eventsCss :: Css
eventsCss = do
  borderRadius (rem 1) (rem 1) (rem 1) (rem 1)
  border solid (rem 0.1) greyColor
  h1 ? do
    textAlign center
  table ? do
    width (pct 100)
    tr |> td # hover ? do
      background lightLightPrimaryColor
    tr |> td ? do
      textAlign (alignSide sideCenter)

maccaronCss :: Css
maccaronCss = do
  span # byClass "maccaron-pro" ? do
    fontSize (rem 0.8)
    fontColor black
    fontFamily ["Merriweather"] [sansSerif]
    fontWeight normal
    lineHeight (rem 1.2)
    textTransform none
    backgroundColor secondaryColor
    position absolute
    top (rem (-0.8))
    right (rem (-0.5))
    padding (rem 0.1) (rem 0.5) (rem 0.1) (rem 0.5)
    borderRadius (rem 0.3) (rem 0.3) (rem 0.3) (rem 0.3)
    transform $ rotate (deg (45))

wheelCss :: Css
wheelCss = do
  div # byId "the-wheel-of-foodtrucks" ? do
    position relative
    canvas ? do
      display inlineBlock
      fontSize (pct 100)
      lineHeight (pct 100)
    input ? do
      verticalAlign vAlignTop
    button ? do
      verticalAlign vAlignTop

subMenuCSS :: Css
subMenuCSS = do
  div # byClass "container-submenu" ? do
    position relative
    marginTop (rem 5)
    nav |> ul ? do
      padding (rem 0) (rem 0) (rem 0) (rem 0)
      textAlign justify
      textAlignLast justify
      li ? do
        display inlineBlock
        position relative
      li |> a ? do
        fontWeight bold
        display inlineBlock
        fontFamily ["Roboto"] [sansSerif]
        fontSize (rem 1.5)
        lineHeight (rem 2)
        color primaryColor

popupCSS :: Css
popupCSS = do
  keyframes "popup-appear-in" [
      (0, display none)
    , (100, display block)]
  animations [
    ("popup-appear-in", (sec 5), linear, (sec 0), infinite, normal, backwards)
    ]
  div # byId "container-popup" ? do
    display none
    position absolute
    right (rem 0)
    width (pct 80)
    height (pct 100)
    zIndex 400
    background $ rgb 242 242 242
    a # byClass "close-icon" ? do
      width (rem 2)
      height (rem 2)
      top (rem 0.5)
      right (rem 0.5)
      display block
      position absolute
      zIndex 500
      borderRadius (rem 1) (rem 1) (rem 1) (rem 1)
      span ? do
        content $ stringContent " "
        position absolute
        width (pct 80)
        height (px 4)
        top (pct 45)
        left (pct 10)
        background darkPrimaryColor
        transform $ rotate (deg (-45))
      span # after ? do
        content $ stringContent " "
        position absolute
        width (pct 100)
        height (px 4)
        background darkPrimaryColor
        transform $ rotate (deg 90)

loginCss :: Css
loginCss = do
  h1 ? do
    fontFamily ["Roboto"] [sansSerif]
    fontSize (rem 1.5)
    lineHeight (rem 4)
    color blackColor
    textTransform capitalize
  h2 ? do
    a ? do
      fontFamily ["Roboto"] [sansSerif]
      fontSize (rem 1.5)
      lineHeight (rem 4)
      color greyColor
      textTransform capitalize
      img ? do
        verticalAlign middle
        width (rem 4)
        height (rem 4)

conceptRegister :: Css
conceptRegister = do
  textAlign justify
  h1 ? do
    fontFamily ["Roboto"] [sansSerif]
    fontSize (rem 1.2)
    lineHeight (rem 2)
    color greyColor
    textTransform capitalize
  p ? do
    fontFamily ["Lato"] [sansSerif]
    fontSize (rem 0.7)
    lineHeight (rem 1.4)
    color blackColor
    marginTop (rem 1)
  p # byClass "quote" ? do
    width (pct 60)
    margin auto auto auto auto
    marginTop (rem 2)
    marginBottom (rem 2)
    fontStyle italic
    fontFamily ["Merriweather"] [sansSerif]
  ul ? do
    listStyleType none
    li ? do
      display inlineBlock
      a ? do
        fontFamily ["Lato"] [sansSerif]
        fontSize (rem 1)
        fontWeight bold
        color blackColor
        paddingLeft (rem 4)

foodEventCss :: Css
foodEventCss = do
  textAlign justify
  textAlignLast justify
  div # byClass "food-event-card" ? do
    position relative
    display inlineBlock
    width (px 150)
    height (px 175)
    margin (rem 1) (rem 1) (rem 1) (rem 1)
    overflow hidden
    img ? do
      position absolute
      width (pct 100)
      height (pct 100)
      zIndex 50
    h2 ? do
      position relative
      overflow hidden
      visibility hidden
      background (setA 0.7 white)
      top (rem 2)
      width (pct 60)
      lineHeight (rem 1)
      zIndex 150
    p ? do
      position absolute
      overflow hidden
      visibility hidden
      background (setA 0.7 white)
      bottom (rem 0)
      zIndex 150
  div # byClass "food-event-card" # hover ? do
    img ? do
      top (px 10)
    h2 ? do
      visibility visible
    p ? do
      visibility visible

contactCSS :: Css
contactCSS = do
  ul ? do
    listStyleType none
    li ? do
      display inlineBlock
      a ? do
        color black
        fontWeight bold
        fontFamily ["Roboto"] [sansSerif]
        fontSize (rem 1.0)
        lineHeight (rem 4)
        span ? do
          display none
          query Q.screen [Q.minWidth (px 640)] $ do
            display inlineBlock
        span # byClass "logo-facebook" ? do
          height (rem 2)
          width (rem 2)
          backgroundPosition (positioned (rem (-14)) (rem 0))
        span # byClass "logo-instagram" ? do
          height (rem 2)
          width (rem 2)
          backgroundPosition (positioned (rem (-22)) (rem 0))
        span # byClass "logo-reddit" ? do
          height (rem 2)
          width (rem 2)
          backgroundPosition (positioned (rem (-2)) (rem 0))
        span # byClass "logo-email" ? do
          height (rem 2)
          width (rem 2)
          backgroundPosition (positioned (rem 0) (rem 0))

facebookLoginP :: Css
facebookLoginP = do
  p # byClass "facebook-login" ? do
    a ? do
      lineHeight (rem 2)
      color facebookBlueColor
      span # byClass "logo-facebook-blue" ? do
        height (rem 2)
        width (rem 2)
        backgroundPosition (positioned (rem (-12)) (rem 0))

leafletCSS :: Css
leafletCSS = do
  div # byClass "leaflet-container" ? do
    position  relative
    display block
    width (pct 100)
    height (pct 100)
    borderBottom solid (rem 0.2) greyColor
    button # byClass "btn-close-map" ? do
      position absolute
      width (rem 2)
      height (rem 2)
      borderRadius (rem 1) (rem 1) (rem 1) (rem 1)
      top (rem 1)
      right (rem 1)
      zIndex 400
      background lightSecondaryColor
    button # byClass "btn-close-map" # hover ? do
      cursor pointer
      background secondaryColor

formCSS :: Css
formCSS = do
  input # ("type" @= "text") ? do
    border solid (rem 0) white
    borderBottom solid (rem 0.1) (setA 0.2 black)
    background (setA 0.3 white)
    fontFamily ["Roboto"] [sansSerif]
    fontSize (rem 1)
  textarea ? do
    border solid (rem 0) white
    borderBottom solid (rem 0.1) (setA 0.2 black)
    background (setA 0.3 white)
    fontFamily ["Roboto"] [sansSerif]
    fontSize (rem 1)
  input # ("type" @= "submit") # hover ? do
    cursor pointer
  button # byId "btnAddMap" ? do
    width (pct 100)
    height (rem 5)

formAddMenuCSS :: Css
formAddMenuCSS = do
  input # ("type" @= "text") ? do
    width (pct 100)
  textarea ? do
    width (pct 100)
  div # byId "datetime-start" ? do
    display inlineBlock
    marginRight (rem 1)
    width (pct 40)

formPlanMenuCSS :: Css
formPlanMenuCSS = do
  div # byId "container-availabilities-inputs" ? do
    position relative
    div # byClass "container-availability" ? do
      width (pct 80)
      margin (rem 2) (rem 0) (rem 0) (rem 0)
    button # byClass "btnAddAvailability" ? do
      position absolute
      bottom (rem 1)
      right (rem 1)
      width (rem 2)
      height (rem 4)

mainPartBlogCategoryListingCSS :: Css
mainPartBlogCategoryListingCSS = do
  padding (rem 2) (rem 0) (rem 0) (rem 0)

mainBreadcrumbCSS :: Css
mainBreadcrumbCSS = do
  ul ? do
    listStyleType none
    li # not (":last-child") ? do
      a # after ? do
        content $ stringContent ">"
    li ? do
      display inlineBlock
      fontFamily ["Roboto"] [sansSerif]
      fontSize (rem 1)
      a ? do
        img ? do
          width (rem 1)
          height (rem 1)
        span # byClass "logo-home" ? do
          height (rem 1.5)
          width (rem 1.5)
          backgroundPosition (positioned (rem (-4.5)) (rem 0))

mainSquareFeatureContainerCSS :: Css
mainSquareFeatureContainerCSS = do
  position relative
  display inlineBlock
  width (rem 15)
  height (rem 15)
  a # hover ? do
    background secondaryColor
  a ? do
    background primaryColor
    position absolute
    width (pct 100)
    height (pct 100)
    h1 ? do
      margin (rem 2) (rem 0) (rem 0) (rem 0)
      fontSize (rem 2)
      fontWeight bold
      color white

mainPartHeatmapCSS :: Css
mainPartHeatmapCSS = do
  div # byId "map" ? do
    height (rem 25)
    width (pct 100)
    div # byClass "leaflet-container" ? do
      width (pct 100)
      height (pct 100)

processCSS :: Css
processCSS = do
  display block
  width (px 800)
  height (px 640)
  background red


methodContainerCSS :: Css
methodContainerCSS = do
  div # byClass "method-container" ? do
    position relative
    display block
    width (px 800)
    height (px 640)
    backgroundImage (url "/static/process.wave.png")
    backgroundSize cover
    span # byClass "icon-left" ? do
      display block
      position absolute
      top (px 250)
      left (px 0)
      height (rem 2)
      width (rem 2)
      backgroundPosition (positioned (rem 0) (rem 0))
    span # byClass "icon-right" ? do
      display block
      position absolute
      top (px 250)
      right (px 0)
      height (rem 2)
      width (rem 2)
      backgroundPosition (positioned (rem (-2)) (rem 0))
    div # byId "explanation-container" ? do
      position absolute
      top (px 120)
      left (px 250)
      height (rem 3)
      width (rem 20)
      background white
    div # byClass "method-button" ? do
      position absolute
      background white
      borderRadius (px 15) (px 15) (px 15) (px 15)
      height (px 15)
      width (px 15)
    div # byId "method-button-step-1" ? do
      left (px 65)
      top (px 475)
    div # byId "method-button-step-2" ? do
      left (px 213)
      top (px 312)
    div # byId "method-button-step-3" ? do
      left (px 340)
      top (px 433)
    div # byId "method-button-step-4" ? do
      left (px 579)
      top (px 323)
    div # byId "method-button-step-5" ? do
      left (px 719)
      top (px 160)

mainPartTechnologiesCSS :: Css
mainPartTechnologiesCSS = do
  span # byClass "icon-python" ? do
    height (rem 2)
    width (rem 2)
    backgroundPosition (positioned (rem (-14)) (rem 0))
  span # byClass "icon-haskell" ? do
    height (rem 2)
    width (rem 4)
    backgroundPosition (positioned (rem (-10)) (rem 0))
  span # byClass "icon-postgresql" ? do
    height (rem 2)
    width (rem 2)
    backgroundPosition (positioned (rem (-4)) (rem 0))
  span # byClass "icon-elm" ? do
    height (rem 2)
    width (rem 2)
    backgroundPosition (positioned (rem (-6)) (rem 0))
  span # byClass "icon-react" ? do
    height (rem 2)
    width (rem 2)
    backgroundPosition (positioned (rem (-8)) (rem 0))

mainPartCSS :: Css
mainPartCSS = do
  div # byClass "main-part" ? do
    display grid
    gridTemplateColumns "1fr 3fr 1fr"
    gridTemplateRows "auto"
    gridTemplateAreas "\"leftvoid main-part rightvoid\""
    gridGap (px 0)
    div # byClass "main-part-article" ? do
      position relative
      gridArea "main-part"
      -- all specific css for the main parts
      methodContainerCSS
      mainPartTechnologiesCSS
  div # byClass "main-part-process" ? do
    background primaryColor

mainCSS :: Css
mainCSS = do
  div # byClass "main" ? do
    overflow hidden
    gridArea "main"
    position relative
    width (pct 100)
    minHeight (rem 40)
    height (pct 100)
    zIndex 100
    popupCSS
    mainPartCSS
    div # byClass "process-container" ? do
      processCSS
    a # byId "aRefreshMapArea" ? do
      top (rem 1)
      right (rem 1)
      position absolute
      padding (rem 0) (rem 0) (rem 0) (rem 0)
      borderRadius (rem 1) (rem 1) (rem 1) (rem 1)
      zIndex 500
      background lightLightPrimaryColor
      span # byClass "logo-refresh" ? do
        height (rem 2)
        width (rem 2)
        backgroundPosition (positioned (rem (-4)) (rem 0))
      span ? do
        fontSize (rem 0.6)
    div # byClass "main-part-submenu" ? do
      subMenuCSS
    div # byClass "main-part-testimonial" ? do
      minHeight (rem 10)
    div # byClass "main-part-events" ? do
      padding (rem 0) (rem 0) (rem 2) (rem 0)
      eventsCss
    div # byClass "main-part-login" ? do
      loginCss
    div # byClass "main-part-concept" ? do
      conceptRegister
    div # byClass "main-part-register" ? do
      conceptRegister
    div # byClass "main-part-food-events" ? do
      foodEventCss
    div # byClass "main-part-contact" ? do
      contactCSS
    div # byClass "main-part-add-menu" ? do
      leafletCSS
      formAddMenuCSS
      formPlanMenuCSS
    div # byClass "breadcrumb-container" ? do
      mainBreadcrumbCSS
    div # byClass "square-feature-container" ? do
      mainSquareFeatureContainerCSS
    div # byClass "main-part-heatmap" ? do
      mainPartHeatmapCSS

mainBlogCSS :: Css
mainBlogCSS = do
  div # byClass "main-part-blog-category-listing" ? do
    mainPartBlogCategoryListingCSS

mapCSS :: Css
mapCSS = do
  div # byId "map-container" ? do
    position relative
    width (pct 80)
    height (rem 20)
    margin (rem 1) (rem 0) (rem 0) (rem 0)
    zIndex 120
    div # byClass "top-description" ? do
      margin (rem 4) (rem 0) (rem 4) (rem 0)
      h1 ? do
        fontFamily ["Roboto"] [sansSerif]
        fontSize (rem 1.5)
        color white
        textShadow (rem (0.1)) (rem (0.1)) (rem (0.1)) black
        textAlign center
    div # byId "map" ? do
      width (pct 100)
      height (rem 20)
      div # byClass "leaflet-container" ? do
        position relative
        width (pct 100)
        height (pct 100)
      button ? do
        zIndex 500
        position absolute
        top (rem 0.5)
        right (rem 0.5)
        img ? do
          width (rem 1)
          height (rem 1)
      div # byClass "leaflet-popup-content" ? do
        h1 ? do
          fontFamily ["Roboto"] [sansSerif]
          fontSize (rem 0.6)
          color black
        p # lastChild ? do
          marginLeft (rem 0.1)
        p ? do
          display inlineBlock
          padding (rem 0) (rem 0) (rem 0) (rem 0)
          fontSize (rem 0.6)
          color greyColor
          a ? do
            display inlineBlock
            color darkPrimaryColor
            img ? do
              display inlineBlock
              height (rem 1)
        form ? do
          img ? do
            display inlineBlock
            height (rem 1)
          input # ("type" @= "submit") ? do
            key "background" (Value "none")
            key "border" (Value "none")
            fontFamily ["Roboto"] [sansSerif]
            fontSize (rem 0.6)
            color black


diaporamaCSS :: Css
diaporamaCSS = do
  div # byClass "container-diaporama" ? do
    textAlign justify

headerCSS :: Css
headerCSS = do
  header ? do
    gridArea "header"
    position fixed
    width (pct 100)
    height (rem 3)
    zIndex 200
    background darkGreyColor
    boxShadow . pure $ bsInset . bsColor lightPrimaryColor $ shadow (rem 0) (rem (0.15))
    div ? do
      position relative
      display inlineBlock
      img ? do
        position relative
        display inlineBlock
        top (rem 0.2)
        left (rem 1)
        width (rem 4)
        height (rem 2.8)
    nav ? do
      position relative
      display inlineBlock
      verticalAlign vAlignTop
      height (pct 100)
      left (rem 5)
      ul ? do
        display inlineBlock
        a # hover ? do
          li ? do
            background lightPrimaryColor
            color black
        a ? do
          li ? do
            display inlineBlock
            height (pct 100)
            padding (rem 0) (rem 0.5) (rem 0) (rem 0.5)
            fontWeight normal
            fontSize (rem 0.8)
            lineHeight (rem 3)
            verticalAlign middle
            color white


carouselCSS :: Css
carouselCSS = do
  div # byClass "container-carousel" ? do
    position relative
    height (rem 2)
  div # byClass "container-carousel-item" ? do
    display block
    position absolute
    width (pct 90)
    padding (rem 0) (rem 1) (rem 0) (rem 1)
  div # byClass "container-carousel-item" |> h3 ? do
    fontFamily ["Courgette"] [serif]
    display inlineBlock
    width (pct 80)
    paddingRight (rem 1)
    textAlign end
  div # byClass "container-carousel" |> div ? do
    left (pct (-110))
    carouselAnimations
  div # byClass "container-carousel" |> div # nthChild "1" ? do
    animationDelay (sec 0)
  div # byClass "container-carousel" |> div # nthChild "2" ? do
    animationDelay (sec 10)
  div # byClass "container-carousel" |> div # nthChild "3" ? do
    animationDelay (sec 20)
  div # byClass "container-carousel" |> div # nthChild "4" ? do
    animationDelay (sec 30)

footerCSS :: Css
footerCSS = do
  footer ? do
    gridArea  "footer"
    overflow hidden
    zIndex 100
    position relative
    background darkGreyColor
    boxShadow . pure $ bsInset . bsColor lightPrimaryColor $ shadow (rem 0) (rem (-0.15))
    h3 ? do
      display inlineBlock
      position absolute
      right (rem 0)
      bottom (rem 0)
      fontSize (rem 0.5)
      lineHeight (rem 1)
    ul ? do
      display inlineBlock
      li ? do
        display inlineBlock
        lineHeight (rem 3)
        a ? do
          fontFamily ["Roboto"] [sansSerif]
          fontSize (rem 0.8)
          paddingLeft (rem 2)
          color darkGreyColor
          span ? do
            display none
            query Q.screen [Q.minWidth (px 640)] $ do
              display inlineBlock
          img ? do
            width (rem 1.5)
            height (rem 1.5)
            verticalAlign middle
          span # byClass "logo-world" ? do
            height (rem 2)
            width (rem 2)
            backgroundPosition (positioned (rem (-16)) (rem 0))
          span # byClass "logo-facebook" ? do
            height (rem 2)
            width (rem 2)
            backgroundPosition (positioned (rem (-14)) (rem 0))
          span # byClass "logo-instagram" ? do
            height (rem 2)
            width (rem 2)
            backgroundPosition (positioned (rem (-22)) (rem 0))
          span # byClass "logo-reddit" ? do
            height (rem 2)
            width (rem 2)
            backgroundPosition (positioned (rem (-2)) (rem 0))
          span # byClass "logo-email" ? do
            height (rem 2)
            width (rem 2)
            backgroundPosition (positioned (rem 0) (rem 0))

messagesCSS :: Css
messagesCSS = do
  div # byClass "messages-top" ? do
    display none
    zIndex 500
    position absolute
    top (rem 2)
    left (pct 30)
    width (pct 40)
    borderRadius (rem 1) (rem 1) (rem 1) (rem 1)
    background secondaryColor
    ul ? do
      li ? do
        listStyleType none
        color greyColor

haskraftCSS :: Css
haskraftCSS = do
  element ":root" ? do
    fontSize (pct 125)
    h1 ? do
      margin (rem 0) (rem 0) (rem 0) (rem 0)
    h2 ? do
      margin (rem 0) (rem 0) (rem 0) (rem 0)
    h3 ? do
      margin (rem 0) (rem 0) (rem 0) (rem 0)
    h4 ? do
      margin (rem 0) (rem 0) (rem 0) (rem 0)
    p ? do
      margin (rem 0) (rem 0) (rem 0) (rem 0)
    span ? do
      margin (rem 0) (rem 0) (rem 0) (rem 0)
    defaultCSS
    containerImgTagCSS
    containerImgProfileCSS
    structureCSS
    headerCSS
    navigationCSS
    messagesCSS
    mainCSS
    mapCSS
    diaporamaCSS
    carouselCSS
    maccaronCss
    wheelCss
    footerCSS

haskraftCSSText :: HandlerM Text
haskraftCSSText = do
  sharedEnv <- ask
  case production . settings $ sharedEnv of
    True -> do
      let tCache = cache sharedEnv

      cache' <- liftIO $ atomically $ readTMVar tCache

      let cacheKey = "CSS-main"

      let mCacheValue = lookup cacheKey cache'

      case mCacheValue of
        -- if the key exists in the cache', return it
        Just cacheValue -> return $ decodeUtf8 cacheValue
        -- if the key doesnt exists, "compile" the CSS
        -- save it in the cache, and return the content
        Nothing -> do
          let cssTxt = renderWith compact [] $ do
                         fonts
                         visibleAnimations
                         haskraftCSS

          let newCache = insert cacheKey (encodeUtf8 . toStrict $ cssTxt) cache'
          _ <- liftIO $ atomically $ swapTMVar tCache newCache
          return $ (toStrict cssTxt)

    False -> return $ toStrict $ renderWith pretty [] $ do
                                   fonts
                                   visibleAnimations
                                   haskraftCSS
