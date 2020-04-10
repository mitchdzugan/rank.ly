module UI.Resume where

import Debug.Trace
import Prelude hiding (div)
import App
import Data.Array
import Data.Maybe
import Data.Symbol
import Data.String as S
import Impulse.DOM
import Impulse.FRP as FRP
import Impulse.Util.Foldable
import Scroll as Scroll

p_modal = SProxy :: SProxy "modal"
p_vodIndex = SProxy :: SProxy "vodIndex"

vidIndicator :: forall e c. Array { vod :: String, desc :: String } -> AppDOM e c Unit
vidIndicator vods = do
  keyed (S.joinWith "" $ vods <#> _.vod) $ e_collectAndReduce p_modal (\_ next -> next) false do
    d_button <- div (className "vid-indicator") do
      span_ (className "button is-rounded") do
        span_ (className "icon is-large") do
          i_ (className "fas fa-photo-video") dnil
    e_emit p_modal $ onClick d_button <#> const true
    s_modal <- getEnv p_modal
    s_bindDOM_ s_modal \modal -> do
      case modal of
        false -> dnil
        true -> do
          e_collectAndReduce p_vodIndex (\_ next -> next) 0 do
            s_vodIndex <- getEnv p_vodIndex
            s_bindDOM_ s_vodIndex \vodIndex -> do
              let m_vod = vods !! vodIndex
                  isFirst = vodIndex == 0
                  isLast = vodIndex + 1 == length vods
              case m_vod of
                Nothing -> dnil
                Just { vod, desc } -> do
                  div_ (className "modal is-active") do
                    d_background <- div (className "modal-background") dnil
                    e_emit p_modal $ onClick d_background <#> const false
                    div_ (className "modal-content") do
                      keyed ("modalVod " <> (show vodIndex)) $ div_ (className "card") do
                        div_ (className "card-content") do
                          video_ (className "video-content" *> controls true) do
                            source_ (src vod *> attr_type "video/mp4") dnil
                            text "video tag unsupported by browser"
                          div_ (className "content") do
                            text desc
                        div_ (className "card-footer") do
                          div_ (className "card-footer-item") do
                            div_ (className "buttons has-addons") do
                              d_prev <- button (className "button" *> disabled isFirst) do
                                span_ (className "icon is-large") do
                                  i_ (className "fas fa-caret-left") dnil
                              forMi_ vods \ind _ -> do
                                keyed ("vodSel " <> (show ind)) do
                                  d_sel <- button (className "button") do
                                    span_ (if (ind == vodIndex)
                                           then className "has-text-dark"
                                           else className "has-text-grey-lighter") do
                                      text "•"
                                  e_emit p_vodIndex $ onClick d_sel <#> const ind
                              d_next <- button (className "button" *> disabled isLast) do
                                span_ (className "icon is-large") do
                                  i_ (className "fas fa-caret-right") dnil
                              e_emit p_vodIndex $ onClick d_prev <#> const (vodIndex - 1)
                              e_emit p_vodIndex $ onClick d_next <#> const (vodIndex + 1)
                    d_close <- button (className "modal-close is-large") dnil
                    e_emit p_modal $ onClick d_close <#> const false

resume :: forall e c. AppDOM e c Unit
resume = div_ (className "resume-page") do
  div_ (className "resume-nav") resumeNav
  resumeContent
  div_ (className "resume-nav") dnil

newtype NavItem = NavItem { title :: String
                          , id :: String
                          , children :: Array NavItem
                          }

navItem :: forall e c. String -> NavItem -> DOM e c Unit
navItem topId (NavItem { title, id, children }) = do
  div_ (className "navItem") do
    navLink <- div (classNames do cn "navLink"
                                  whenM (pure $ id == topId) $ cn "has-text-weight-bold") do
      text title
    _ <- _eff $ FRP.consume (\_ -> Scroll.scrollToId id) $ onClick navLink
    forM_ children $ navItem topId
  pure unit

resumeNav :: forall e c. AppDOM e c Unit
resumeNav = do
  div_ (className "content nav-content") do
    div_ (className "navWrapper") do
      let e_topId = (FRP.mkEvent Scroll.attachScrollWatch)
      initTopId <- _eff Scroll.getTopId
      s_topId <- s_use $ FRP.s_dedup =<< FRP.s_from e_topId initTopId
      s_bindDOM_ s_topId \topId -> do
        navItem topId $ NavItem { title: "Education"
                                , id: "education"
                                , children: []
                                }
        navItem topId $ NavItem { title: "Technical Work Experience"
                                , id: "work-exp"
                                , children: [ NavItem { title: "smash.gg"
                                                      , id: "smashgg"
                                                      , children: []
                                                      }
                                            , NavItem { title: "EveryMove"
                                                      , id: "everymove"
                                                      , children: []
                                                      }
                                            , NavItem { title: "Microsoft"
                                                      , id: "msft"
                                                      , children: []
                                                      }
                                            ]
                                }
        navItem topId $ NavItem { title: "Personal Project Work"
                                , id: "projects"
                                , children: [ NavItem { title: "Impulse"
                                                      , id: "impulse"
                                                      , children: []
                                                      }
                                            , NavItem { title: "Quarantine Liverpool"
                                                      , id: "liverpool"
                                                      , children: []
                                                      }
                                            , NavItem { title: "Super Smash Bros. Melee Projects"
                                                      , id: "ssbm"
                                                      , children: []
                                                      }
                                            , NavItem { title: "Secret Santa"
                                                      , id: "santa"
                                                      , children: []
                                                      }
                                            , NavItem { title: "Messenger++"
                                                      , id: "mpp"
                                                      , children: []
                                                      }
                                            ]
                                }
        navItem topId $ NavItem { title: "Honors, Awards, and Involvements"
                                , id: "etc"
                                , children: []
                                }

resumeContent :: forall e c. AppDOM e c Unit
resumeContent = div_ (className "content" *> id "resume") do
  section_ (className "section has-text-centered") do
    h1_ anil $ text "Mitch Dzugan"
    p_ anil do
      a_ (href "mailto://mitchdzugan@gmail.com") $ text "mitchdzugan@gmail.com"
      span_ anil $ text " - (630) 946-9282 - "
      a_ (href "https://github.com/mitchdzugan") $ text "github"
      span_ anil $ text " - "
      a_ (href "http://mdzugan.com") $ text "website"
  section_ (className "section") do
    h2_ (id "education") $ text "Education"
    p_ (anil) do
      strong_ anil $ text "University of Illinois at Urbana-Champaign"
      text " | "
      strong_ anil $ text "Bachelor of Science Computer Engineering"
      text "; Spring 2014 | "
      strong_ anil $ text "Technical GPA: "
      text "3.76"
  section_ (className "section") do
    h2_ (id "work-exp") $ text "Technical Work Experience"
    section_ (className "section" *> id "smashgg") do
      p_ (anil) do
        span_ (className "is-size-5 has-text-weight-bold") $ text "smash.gg: "
        span_ (className "has-text-weight-bold is-italic") $ text "Software Engineer -> Senior Software Engineer "
        span_ anil $ text "San Francisco, CA: "
        span_ (className "is-italic") $ text "Oct. '17 - March '20"
        span_ (className "is-italic has-text-weight-bold") $ text " (javascript, php, react, redux, apollo, graphql, webpack, babel, css, html)"
      ul_ anil do
        li_ anil $ text "Led development and managed a team of 5 engineers in building a widget-based system for page customization. Feature-set included:"
        ul_ anil do
          vidIndicator []
          vidIndicator [ { vod: "/widgets_1.mp4"
                         , desc: "This video demonstrates the minimap view of content, how it can be dragged around the screen to get a better view of what is being edited, and how to edit content."
                         }
                       , { vod: "/widgets_2.mp4"
                         , desc: "This video demonstrates the ability to control background color of the page (can be similarly edited at the row and widget level) and how content responds to ensure readability on all colors."
                         }
                       , { vod: "/widgets_3.mp4"
                         , desc: "This video demonstrates the ability to drag and drop content around the page using the minimap view as well as how the page scrolls in response to ensure whatever you are taking action on stays in view."
                         }
                       ]
          li_ anil $ text "Layouting in a row/column-based grid with automatic responsive mobile support"
          li_ anil $ text "Complete undo/redo history and automatic progress saving via local storage"
          li_ anil $ text "Collapsible minimap editor for quickly viewing page layout, page navigation, drag-and-drop for moving widgets within the layout and individual widget customization"
        li_ anil do
          text "Led development and managed a team of 3 engineers that built a system for automatically reporting scores to a tournament for games with API support. Feature-set included:"
        ul_ anil do
          vidIndicator [ { vod: "/api_report_1.mp4"
                         , desc: "This video demonstrates how the user is able to search the external API for the game they wish to report."
                         }
                       , { vod: "/api_report_2.mp4"
                         , desc: "This video demonstrates how the user is able to link a game they found using the API to be reported as a game in our site and how they can edit the API reported scores if necessary"
                         }
                       ]
          li_ anil $ text "Ability to search for and inspect stats from games played"
          li_ anil $ text "Automatic matching of players in game with users on site"
          li_ anil $ text "Ability to alter API reported scores when necessary and view differences between API results and manually changed results"
          li_ anil $ text "UI implemented in high level abstractions over API such that games with vastly different APIs were supported using the same system"
        li_ anil $ text "Led development on several other projects where responsibilities included:"
        ul_ anil do
          li_ anil $ text "Independently fleshing out high level product goals into prototypes that the team used to align on final product spec before moving forward with feature development as a team"
          li_ anil $ text "Writing an engineering spec and test plan"
          li_ anil $ text "Planning work for other engineers by means of creating Jira tickets and maintaining a Jira board"
          li_ anil $ text "Scheduling and leading planning and retro meetings"
        li_ anil $ text "Built incremental server side rendering engine and refactored core render paths as part of effort to improve key “time to interactive” performance metrics by 40% for average users"
        li_ anil $ text "Domain expert for multiple systems including bracketing algorithms and core front-end infrastructure"
        li_ anil $ text "Helped improve company wide code quality through code review, mentorship and pair programming"
        li_ anil $ text "Primary driver behind “Lunch ‘n Learn” initiative in which engineers shared internal domain knowledge as well as any other topics they thought were interesting/useful. Gave talks on topics including:"
        ul_ anil do
          li_ anil $ text "Redux"
          li_ anil $ text "React Context"
          li_ anil $ text "3 part series on Category Theory and how it can inform javascript development"
    section_ (className "section" *> id "everymove") do
      p_ (anil) do
        span_ (className "is-size-5 has-text-weight-bold") $ text "EveryMove: "
        span_ (className "has-text-weight-bold is-italic") $ text "Software Engineer "
        span_ anil $ text "Seattle, WA: "
        span_ (className "is-italic") $ text "Dec. '14 - Aug '17"
        span_ (className "is-italic has-text-weight-bold") $ text " (c#, Xamarin, javascript, ember.js, css, html)"
      ul_ anil do
        li_ anil $ text "Managed $500k contract project, which included:"
        ul_ anil do
          li_ anil $ text "Resurrecting 2-year-old codebase"
          li_ anil $ text "Managing and implementing client feature requests"
          li_ anil $ text "Writing an app for the Microsoft Band"
        li_ anil $ text "Built a web app for monitoring testing code coverage"
        li_ anil $ text "Assorted feature work and bug fixes. The stack was .NET backend, with frontends for web, Android native, iOS native and Xamarin for Android and iOS (we have 2 apps). I worked on all parts of the stack."
    section_ (className "section" *> id "msft") do
      p_ (anil) do
        span_ (className "is-size-5 has-text-weight-bold") $ text "Microsoft: "
        span_ (className "has-text-weight-bold is-italic") $ text "Summer Internship -> Software Development Engineer "
        span_ anil $ text "Redmond, WA: "
        span_ (className "is-italic") $ text "Jun. '13 - Nov '14"
        span_ (className "is-italic has-text-weight-bold") $ text " (c#)"
      ul_ anil do
        li_ anil $ text "Developed a statistical model to condense overwhelming amounts of data into consumable reports that showed trends and spotted anomalies"
        li_ anil $ text "Built an infrastructure to automatically generate and distribute said reports"
        li_ anil $ text "Developed bare essentials version of Visual Studio to allow for UI Testing to be done without large application start-up time costs"
  section_ (className "section") do
    h2_ (id "projects") $ text "Personal Project Work"
    section_ (className "section" *> id "impulse") do
      p_ (anil) do
        span_ (className "is-size-5 has-text-weight-bold") $ text "Impulse: Web UI Framework"
        span_ (className "has-text-weight-bold is-italic") $ text " (purescript) "
        a_ (href "https://github.com/mitchdzugan/purescript-impulse") $ text "https://github.com/mitchdzugan/purescript-impulse"
      ul_ anil do
        li_ anil do
          text "Monadic, functional reactive programming based UI framework largely inspired by "
          a_ (href "https://github.com/reflex-frp/reflex-dom") $ text "reflex-dom"
        li_ anil $ text "Supports server-side rendering and interactions with all web APIs such as localStorage and history"
        li_ anil do
          text "Powers my website "
          a_ (href "http://mdzugan.com") $ text "http://mdzugan.com"
          text ". Early introductory guide for javascript developers is "
          a_ (href "http://www.mdzugan.com/impulse") $ text "here"
          text " and additional documentation is hosted on "
          a_ (href "https://pursuit.purescript.org/packages/purescript-impulse/3.0.0/docs/Impulse.DOM") $ text "Pursuit"
    section_ (className "section" *> id "liverpool") do
      p_ (anil) do
        span_ (className "is-size-5 has-text-weight-bold") $ text "Quarantine Liverpool"
        span_ (className "has-text-weight-bold is-italic") $ text " (javascript) "
        a_ (href "https://github.com/mitchdzugan/liverpool") $ text "https://github.com/mitchdzugan/liverpool"
      ul_ anil do
        vidIndicator [ { vod: "/liverpool.mp4"
                       , desc: "This video shows a typical hand being played."
                       }
                     ]
        li_ anil do
          text "Online version of Liverpool card game made so family can continue playing weekly games during COVID-19 crisis. Playable "
          a_ (href "http://quarantine-liverpool.herokuapp.com") $ text "here"
        li_ anil do
          text "Implemented using React and Socket.IO"
        li_ anil $ text "Responsive design supports being played on mobile or desktop"
    section_ (className "section" *> id "ssbm") do
      p_ (anil) do
        span_ (className "is-size-5 has-text-weight-bold") $ text "Super Smash Bros. Melee Projects"
        ul_ anil do
          li_ anil do
            a_ (href "https://github.com/mitchdzugan/osx-wiiu-gcc-adapter") $ text "https://github.com/mitchdzugan/osx-wiiu-gcc-adapter"
          ul_ anil do
            li_ anil $ text "First OSX driver written for the official Nintendo GameCube usb adapter, on which all modern community accepted drivers are built from"
          li_ anil do
            a_ (href "https://github.com/mitchdzugan/cvmelee") $ text "https://github.com/mitchdzugan/cvmelee"
          ul_ anil do
            li_ anil $ text "Uses openCV to pull statistics from videos of gameplay"
            li_ anil $ text "This program is able to locate where the gameplay is in melee videos and then pull frame by frame data, regardless of the use of third party overlays"
          li_ anil do
            a_ (href "https://github.com/mitchdzugan/dolphin") $ text "https://github.com/mitchdzugan/dolphin"
          ul_ anil do
            li_ anil $ text "Fork of the dolphin gamecube emulator that I injected a server into so I can programmatically control it while it is running. Allows one to find specific frame/input sequences that most had assumed were within too large of a search space to find"
          li_ anil do
            a_ (href "https://github.com/mitchdzugan/MeleeSSToModel") $ text "https://github.com/mitchdzugan/MeleeSSToModel"
          ul_ anil do
            vidIndicator [ { vod: "/falco_model.mp4"
                           , desc: "This video demonstrates the algorithm used to create the model. Screenshots were generated using my dolphin fork that allows you to send controller inputs over TCP connection by pausing on the desired frame on a hacked version of the game where all background textures are green, rotating the camera to various angles via scripted inputs, and inspecting the emulator process RAM to determine exact camera position."
                           }
                         , { vod: "/falco_printed.mp4"
                           , desc: "This is the printed version of the model (coloring done by hand)"
                           }
                         ]
            li_ anil $ text "python script that converts screenshots from a bunch of an animation frame from a bunch of different angles into a printable 3d model"
    section_ (className "section" *> id "santa") do
      p_ (anil) do
        span_ (className "is-size-5 has-text-weight-bold") $ text "Secret Santa"
        span_ (className "has-text-weight-bold is-italic") $ text " (javascript) "
        a_ (href "https://github.com/mitchdzugan/secret-santa") $ text "https://github.com/mitchdzugan/secret-santa"
      ul_ anil do
        li_ anil $ text "Randomly assigns people to be another’s Santa with customizable constraints like avoiding roommates"
        li_ anil $ text "Ability to upload and edit wish list which is made only visible to your randomly assigned santa"
        li_ anil $ text "Ability to anonymously chat with your Santa/person you are shopping to clarify confusion in wish list"
    section_ (className "section" *> id "mpp") do
      p_ (anil) do
        span_ (className "is-size-5 has-text-weight-bold") $ text "Messenger++"
        span_ (className "has-text-weight-bold is-italic") $ text " (clojurescript) "
        a_ (href "https://github.com/mitchdzugan/secret-santa") $ text "https://github.com/mitchdzugan/messenger_plus_plus"
      ul_ anil do
        li_ anil $ text "smashgg originally used Facebook Messenger to manage inbound support. I created this Chrome extension to improve the quality of life of the support team, which included the following features:"
        ul_ anil do
          vidIndicator [ { vod: "/mpp_1.mp4"
                         , desc: "These videos were taken on the day we finally transitioned from facebook support chats to Intercom. On this day the whole support team joined a call and used my extension to leave all of the support chats and return their facebooks to a usable state. Their excitement and gratitude easily made this was one of my favorite day's ever as an engineer."
                         }
                       , { vod: "/mpp_2.mp4"
                         , desc: "These videos were taken on the day we finally transitioned from facebook support chats to Intercom. On this day the whole support team joined a call and used my extension to leave all of the support chats and return their facebooks to a usable state. Their excitement and gratitude easily made this was one of my favorite day's ever as an engineer."
                         }
                       ]
          li_ anil $ text "Bulk adding to/removing from multiple people from multiple chats"
          li_ anil $ text "Bulk messaging chats"
          li_ anil $ text "Viewing unread chats based on certain filters"
          li_ anil $ text "Automatically removed users from all 7000+ chats once the company moved to Intercom"
  section_ (className "section") do
    h2_ (id "etc") $ text "Honors, Awards, and Involvements"
    ul_ anil do
      li_ anil $ text "Dean’s list of the College of Engineering"
      li_ anil $ text "University-employed tutor for Math, Physics, Computer Science, and Computer Engineering"
      li_ anil $ text "University-sponsored speaker for academic probation recovery events"
      li_ anil $ text "Selected by students/teachers to deliver the commencement speech at High School graduation"
      li_ anil $ text "Developed and taught an intensive curriculum for post-grad Biology major with minimal programming experience to learn software engineering who now has a full-time software engineering job"
  div_ (className "spacer") dnil
