module Examples.Message exposing (Msg, State, example)

import Accessibility.Styled as Html exposing (..)
import Category exposing (Category(..))
import CommonControls
import Css exposing (..)
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled.Attributes as Attributes exposing (css, href)
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Message.V3 as Message
import ViewHelpers exposing (viewExamples)


type alias State =
    { show : Bool
    , control : Control (List ( String, Message.Attribute Msg ))
    }


init : State
init =
    { show = True
    , control =
        ControlExtra.list
            |> ControlExtra.optionalListItem "theme" controlTheme
            |> ControlExtra.listItem "content"
                (CommonControls.content
                    { moduleName = "Message"
                    , plaintext = Message.plaintext
                    , markdown = Message.markdown
                    , html = Message.html
                    , httpError = Just Message.httpError
                    }
                )
            |> ControlExtra.optionalListItem "role" controlRole
            |> ControlExtra.optionalBoolListItem "dismissable"
                ( "Message.onDismiss Dismiss", Message.onDismiss Dismiss )
            |> CommonControls.iconNotCheckedByDefault "Message" Message.icon
            |> ControlExtra.optionalBoolListItem "hideIconForMobile"
                ( "Message.hideIconForMobile", Message.hideIconForMobile )
            |> CommonControls.css
                { moduleName = "Message"
                , use = Message.css
                }
            |> CommonControls.mobileCss
                { moduleName = "Message"
                , use = Message.mobileCss
                }
            |> CommonControls.quizEngineMobileCss
                { moduleName = "Message"
                , use = Message.quizEngineMobileCss
                }
            |> CommonControls.notMobileCss
                { moduleName = "Message"
                , use = Message.notMobileCss
                }
    }


controlTheme : Control ( String, Message.Attribute msg )
controlTheme =
    Control.choice
        [ ( "tip", Control.value ( "Message.tip", Message.tip ) )
        , ( "error", Control.value ( "Message.error", Message.error ) )
        , ( "alert", Control.value ( "Message.alert", Message.alert ) )
        , ( "success", Control.value ( "Message.success", Message.success ) )
        , ( "customTheme", controlCustomTheme )
        ]


controlCustomTheme : Control ( String, Message.Attribute msg )
controlCustomTheme =
    Control.record
        (\( aStr, a ) ( bStr, b ) ->
            ( "Message.customTheme { color = " ++ aStr ++ ", backgroundColor = " ++ bStr ++ " }"
            , Message.customTheme { color = a, backgroundColor = b }
            )
        )
        |> Control.field "color"
            (CommonControls.choice "Colors"
                [ ( "aquaDark", Colors.aquaDark )
                ]
            )
        |> Control.field "backgroundColor"
            (CommonControls.choice "Colors"
                [ ( "gray92", Colors.gray92 )
                ]
            )


controlRole : Control ( String, Message.Attribute msg )
controlRole =
    CommonControls.choice "Message"
        [ ( "alertRole", Message.alertRole )
        , ( "alertDialogRole", Message.alertDialogRole )
        ]


type Msg
    = Dismiss
    | UpdateControl (Control (List ( String, Message.Attribute Msg )))


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        Dismiss ->
            ( { state | show = False }, Cmd.none )

        UpdateControl newControl ->
            ( { state | control = newControl }, Cmd.none )


example : Example State Msg
example =
    { name = "Message"
    , version = 3
    , categories = [ Messaging ]
    , keyboardSupport = []
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        [ Message.view [ Message.plaintext "Tiny tip" ]
        , Message.view [ Message.success, Message.plaintext "Tiny success" ]
        , Message.view [ Message.error, Message.plaintext "Tiny error" ]
        ]
    , view =
        \state ->
            let
                attributes =
                    List.map Tuple.second (Control.currentValue state.control)

                orDismiss view =
                    if state.show then
                        view

                    else
                        text "Nice! The messages were dismissed. 👍"
            in
            [ ControlView.view
                { update = UpdateControl
                , settings = state.control
                , toExampleCode =
                    \settings ->
                        let
                            toCode maybeSize =
                                "Message.view\n\t[ "
                                    ++ (maybeSize
                                            :: List.map (Tuple.first >> Just) settings
                                            |> List.filterMap identity
                                            |> String.join "\n\t, "
                                       )
                                    ++ "\n\t]"
                        in
                        [ { sectionName = "Tiny"
                          , code = toCode Nothing
                          }
                        , { sectionName = "Large"
                          , code = toCode (Just "Message.large")
                          }
                        , { sectionName = "Banner"
                          , code = toCode (Just "Message.banner")
                          }
                        ]
                }
            , orDismiss <|
                viewExamples
                    [ ( "tiny", Message.view attributes )
                    , ( "large", Message.view (Message.large :: attributes) )
                    , ( "banner", Message.view (Message.banner :: attributes) )
                    ]
            , Heading.h3
                [ Heading.css
                    [ Css.marginTop (Css.px 20)
                    , Css.borderTop3 (Css.px 2) Css.solid Colors.gray96
                    , Css.paddingTop (Css.px 20)
                    ]
                ]
                [ text "Message.somethingWentWrong" ]
            , Message.somethingWentWrong exampleRailsError
            ]
    }


exampleRailsError : String
exampleRailsError =
    """web : Completed 500 Internal Server Error in 273.5ms
web :
web : ActionView::MissingTemplate - Missing template teach/assignables/blueprint, teach/base/blueprint, application/blueprint with {:locale=>[:en], :formats=>[:json, :js, :html, :text, :js, :css, :ics, :csv, :png, :jpeg, :gif, :bmp, :tiff, :mpeg, :xml, :rss, :atom, :yaml, :multipart_form, :url_encoded_form, :json, :pdf, :zip, :xlsx], :handlers=>[:erb, :builder, :axlsx, :coffee, :haml, :rabl, :hamlc]}. Searched in:
web :   * "/Users/avh4/workspace/NoRedInk/app/views"
web :   * "/Users/avh4/.gem/ruby/2.3.3/gems/sextant-0.2.4/app/views"
web :   * "/Users/avh4/.gem/ruby/2.3.3/gems/configurable_engine-0.4.8/app/views"
web :   * "/Users/avh4/.gem/ruby/2.3.3/gems/kaminari-0.17.0/app/views"
web :   * "/Users/avh4/workspace/NoRedInk/app/assets/javascripts/templates"
web : :
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/actionpack/lib/action_view/path_set.rb:58:in `find'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/actionpack/lib/action_view/lookup_context.rb:122:in `find'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/actionpack/lib/action_view/renderer/abstract_renderer.rb:3:in `find_template'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/actionpack/lib/action_view/renderer/template_renderer.rb:35:in `determine_template'
web :   newrelic_rpm (4.0.0.332) lib/new_relic/agent/instrumentation/rails3/action_controller.rb:149:in `render_with_newrelic'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/actionpack/lib/action_view/renderer/renderer.rb:43:in `render_template'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/actionpack/lib/action_view/renderer/renderer.rb:24:in `render'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/actionpack/lib/abstract_controller/rendering.rb:111:in `_render_template'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/actionpack/lib/action_controller/metal/streaming.rb:225:in `_render_template'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/actionpack/lib/abstract_controller/rendering.rb:104:in `render_to_body'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/actionpack/lib/action_controller/metal/renderers.rb:28:in `render_to_body'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/actionpack/lib/action_controller/metal/compatibility.rb:50:in `render_to_body'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/actionpack/lib/abstract_controller/rendering.rb:89:in `render'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/actionpack/lib/action_controller/metal/rendering.rb:16:in `render'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/actionpack/lib/action_controller/metal/instrumentation.rb:40:in `block (2 levels) in render'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/activesupport/lib/active_support/core_ext/benchmark.rb:5:in `block in ms'
web :   /Users/avh4/.rubies/ruby-2.3.3/lib/ruby/2.3.0/benchmark.rb:308:in `realtime'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/activesupport/lib/active_support/core_ext/benchmark.rb:5:in `ms'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/actionpack/lib/action_controller/metal/instrumentation.rb:40:in `block in render'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/actionpack/lib/action_controller/metal/instrumentation.rb:83:in `cleanup_view_runtime'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/activerecord/lib/active_record/railties/controller_runtime.rb:24:in `cleanup_view_runtime'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/actionpack/lib/action_controller/metal/instrumentation.rb:39:in `render'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/actionpack/lib/action_controller/metal/implicit_render.rb:10:in `default_render'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/actionpack/lib/action_controller/metal/implicit_render.rb:5:in `send_action'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/actionpack/lib/abstract_controller/base.rb:167:in `process_action'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/actionpack/lib/action_controller/metal/rendering.rb:10:in `process_action'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/actionpack/lib/abstract_controller/callbacks.rb:18:in `block in process_action'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/activesupport/lib/active_support/callbacks.rb:502:in `_run__46453218882797464__process_action__3621715315305983900__callbacks'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/activesupport/lib/active_support/callbacks.rb:405:in `__run_callback'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/activesupport/lib/active_support/callbacks.rb:385:in `_run_process_action_callbacks'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/activesupport/lib/active_support/callbacks.rb:81:in `run_callbacks'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/actionpack/lib/abstract_controller/callbacks.rb:17:in `process_action'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/actionpack/lib/action_controller/metal/rescue.rb:29:in `process_action'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/actionpack/lib/action_controller/metal/instrumentation.rb:30:in `block in process_action'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/activesupport/lib/active_support/notifications.rb:123:in `block in instrument'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/activesupport/lib/active_support/notifications/instrumenter.rb:20:in `instrument'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/activesupport/lib/active_support/notifications.rb:123:in `instrument'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/actionpack/lib/action_controller/metal/instrumentation.rb:29:in `process_action'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/actionpack/lib/action_controller/metal/params_wrapper.rb:207:in `process_action'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/activerecord/lib/active_record/railties/controller_runtime.rb:18:in `process_action'
web :   newrelic_rpm (4.0.0.332) lib/new_relic/agent/instrumentation/rails3/action_controller.rb:30:in `block in process_action'
web :   newrelic_rpm (4.0.0.332) lib/new_relic/agent/instrumentation/controller_instrumentation.rb:362:in `perform_action_with_newrelic_trace'
web :   newrelic_rpm (4.0.0.332) lib/new_relic/agent/instrumentation/rails3/action_controller.rb:25:in `process_action'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/actionpack/lib/abstract_controller/base.rb:121:in `process'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/actionpack/lib/abstract_controller/rendering.rb:46:in `process'
web :   rack-mini-profiler (0.10.2) lib/mini_profiler/profiling_methods.rb:102:in `block in profile_method'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/actionpack/lib/action_controller/metal.rb:203:in `dispatch'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/actionpack/lib/action_controller/metal/rack_delegation.rb:14:in `dispatch'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/actionpack/lib/action_controller/metal.rb:246:in `block in action'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/actionpack/lib/action_dispatch/routing/route_set.rb:73:in `dispatch'
web :    () Users/avh4/.gem/ruby/2.3.3/bundler/gems/rails-e17e25cd23e8/actionpack/lib/action_dispatch/routing/route_set.rb:36:in `call'"""
