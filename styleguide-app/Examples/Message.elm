module Examples.Message exposing (Msg, State, example)

import Accessibility.Styled as Html exposing (..)
import AtomicDesignType exposing (AtomicDesignType(..))
import Category exposing (Category(..))
import Css exposing (..)
import Debug.Control as Control exposing (Control)
import Example exposing (Example)
import Html.Styled exposing (styled)
import Html.Styled.Attributes as Attributes exposing (href)
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Message.V2 as Message
import Nri.Ui.Pennant.V2 as Pennant
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.UiIcon.V1 as UiIcon


type alias State =
    { show : Bool
    , control : Control ExampleConfig
    }


type alias ExampleConfig =
    { size : List (Message.Attribute Msg) -> Html Msg
    , theme : Maybe (Message.Attribute Msg)
    , content : Maybe (Message.Attribute Msg)
    , role : Maybe (Message.Attribute Msg)
    , dismissable : Maybe (Message.Attribute Msg)
    }


init : State
init =
    { show = True
    , control =
        Control.record ExampleConfig
            |> Control.field "size" controlSize
            |> Control.field "theme" controlTheme
            |> Control.field "content" controlContent
            |> Control.field "role" controlRole
            |> Control.field "dismissable" controlDismissable
    }


controlSize : Control (List (Message.Attribute Msg) -> Html Msg)
controlSize =
    Control.choice
        [ ( "banner", Control.value Message.banner )
        , ( "large", Control.value Message.large )
        , ( "tiny", Control.value Message.tiny )
        ]


controlTheme : Control (Maybe (Message.Attribute msg))
controlTheme =
    Control.choice
        [ ( "not set", Control.value Nothing )
        , ( "tip", Control.value (Just Message.tip) )
        , ( "error", Control.value (Just Message.error) )
        , ( "alert", Control.value (Just Message.alert) )
        , ( "success", Control.value (Just Message.success) )
        , ( "customTheme (aquaDark, gray92, premiumFlag)"
          , Message.customTheme
                { color = Colors.aquaDark
                , backgroundColor = Colors.gray92
                , icon = Pennant.premiumFlag
                }
                |> Just
                |> Control.value
          )
        ]


controlContent : Control (Maybe (Message.Attribute msg))
controlContent =
    Control.choice
        [ ( "not set"
          , Control.value Nothing
          )
        , ( "plain text (short)"
          , Control.string "Comic books do count as literature."
                |> Control.map Message.plaintext
                |> Control.map Just
          )
        , ( "plain text (long)"
          , Control.stringTextarea "Share this link with students as an easy shortcut to join Jeffy's Favorite Class (no class code needed). The link works for students new to NoRedInk and those with existing accounts. Students only need to use this link once to join."
                |> Control.map Message.plaintext
                |> Control.map Just
          )
        , ( "markdown"
          , Control.string "_Katie's dad suggests:_ Don't tip too much, or your waitress will **fall over**!"
                |> Control.map Message.markdown
                |> Control.map Just
          )
        , ( "HTML (short)"
          , Control.value
                (Message.html
                    [ code [] [ text "git status" ]
                    , text " ⇄ "
                    , Html.em [] [ text "tries again" ]
                    ]
                    |> Just
                )
          )
        , ( "HTML (long)"
          , Control.value
                (Message.html
                    [ text "Click "
                    , a [ href "http://www.noredink.com", Attributes.target "_blank" ]
                        [ text "here, yes, HERE, right here on this very long success message. "
                        , text "Wow, how successful! You're the biggest success I've ever seen! "
                        , text "You should feel great about yourself! Give yourself a very big round of applause! "
                        , styled div
                            [ display inlineBlock
                            , width (px 20)
                            ]
                            []
                            [ Svg.toHtml UiIcon.gear ]
                        ]
                    , text " to check out NoRedInk."
                    ]
                    |> Just
                )
          )
        ]


controlRole : Control (Maybe (Message.Attribute msg))
controlRole =
    Control.choice
        [ ( "not set", Control.value Nothing )
        , ( "alertRole", Control.value (Just Message.alertRole) )
        , ( "alertDialogRole", Control.value (Just Message.alertDialogRole) )
        ]


controlDismissable : Control (Maybe (Message.Attribute Msg))
controlDismissable =
    Control.maybe False <|
        Control.value (Message.onDismiss Dismiss)


type Msg
    = Dismiss
    | UpdateControl (Control ExampleConfig)


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        Dismiss ->
            ( { state | show = False }, Cmd.none )

        UpdateControl newControl ->
            ( { state | control = newControl }, Cmd.none )


example : Example State Msg
example =
    { name = "Nri.Ui.Message.V2"
    , categories = [ Messaging ]
    , atomicDesignType = Molecule
    , keyboardSupport = []
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view =
        \state ->
            let
                { size, role, theme, dismissable, content } =
                    Control.currentValue state.control

                attributes : List (Message.Attribute Msg)
                attributes =
                    List.filterMap identity
                        [ theme
                        , content
                        , role
                        , dismissable
                        ]

                orDismiss view =
                    if state.show then
                        view

                    else
                        text "Nice! The message was dismissed. 👍"
            in
            [ Control.view UpdateControl state.control
                |> Html.fromUnstyled
            , Heading.h3 [] [ text "Message.view" ]
            , orDismiss <| size attributes
            , Heading.h3 [] [ text "Message.somethingWentWrong" ]
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
