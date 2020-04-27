module Examples.Message exposing (Msg, State, example)

import Category exposing (Category(..))
import Css
import Example exposing (Example)
import Html.Styled as Html exposing (Html, a, div, h3, pre, text)
import Html.Styled.Attributes as Attributes
import Nri.Ui.Alert.V4 as Alert
import Nri.Ui.BannerAlert.V6 as BannerAlert
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Pennant.V2 as Pennant
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.UiIcon.V1 as UiIcon


type alias State =
    { show : Bool }


init : State
init =
    { show = True }


type Msg
    = NoOp
    | Dismiss


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        NoOp ->
            ( state, Cmd.none )

        Dismiss ->
            ( { state | show = False }, Cmd.none )


example : Example State Msg
example =
    { name = "Nri.Ui.Message.V1"
    , categories = [ Messaging ]
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view =
        \state ->
            [ Heading.h3 [] [ Html.text "Message.tiny" ]
            , Alert.error "This is an **error**"
            , Alert.warning "This is a **warning**"
            , Alert.tip "This is a **tip**"
            , Alert.success "This is a **success**"
            , Html.hr [] []
            , Heading.h3 [] [ Html.text "Message.internalError" ]
            , Alert.somethingWentWrong exampleRailsError
            , Html.hr [] []
            , Heading.h3 [] [ Html.text "Message.banner" ]
            , if state.show then
                div
                    []
                    [ h3 [] [ text "alert" ]
                    , BannerAlert.alert [ text "Dismiss this alert message to see a success message!" ] (Just Dismiss)
                    , pre [] [ text "BannerAlert.alert [ text \"Dismiss this alert message to see a success message!\" ] (Just Dismiss)" ]
                    ]

              else
                div
                    []
                    [ h3 [] [ text "success" ]
                    , BannerAlert.success [ text "Nice! The alert message was dismissed. 👍" ] Nothing
                    , pre [] [ text "BannerAlert.success [ text \"Nice! The alert message was dismissed. 👍\" ] Nothing" ]
                    ]
            , h3 [] [ text "error" ]
            , BannerAlert.error [ text "This is an error message!" ] Nothing
            , pre [] [ text "BannerAlert.error [ text \"This is an error message!\" ] Nothing" ]
            , h3 [] [ text "neutral" ]
            , BannerAlert.neutral [ text "This is a neutral message!" ] Nothing
            , pre [] [ text "BannerAlert.neutral [ text \"This is a neutral message!\" ] Nothing" ]
            , h3 [] [ text "custom" ]
            , BannerAlert.custom
                { color = Colors.aquaDark
                , backgroundColor = Colors.gray92
                , icon = Pennant.premiumFlag
                , content = [ text "This is a a custom message!" ]
                , dismiss = Nothing
                }
            , pre []
                [ text
                    """BannerAlert.custom
                  { color = Colors.aquaDark
                  , backgroundColor = Colors.gray92
                  , icon = Pennant.premiumFlag
                  , content = [ text "This is a a custom message!" ]
                  , dismiss = Nothing
                  }
                      """
                ]
            , h3 [] [ text "with multi-line link and icon" ]
            , BannerAlert.success
                [ text "Click "
                , a [ Attributes.href "http://www.noredink.com", Attributes.target "_blank" ]
                    [ text
                        """here, yes, HERE, right here on this very long success message.
                                  Wow, how successful! You're the biggest success I've ever seen!
                                  You should feel great about yourself! Give yourself a very big round of applause!
                                  """
                    , div [ Attributes.css [ Css.display Css.inlineBlock, Css.width (Css.px 20) ] ]
                        [ Svg.toHtml UiIcon.gear ]
                    ]
                , text " to check out NoRedInk."
                ]
                Nothing
            , pre []
                [ text
                    """              BannerAlert.success
                  [ text "Click "
                  , a [ Attributes.href "http://www.noredink.com", Attributes.target "_blank" ]
                      [ text
                          \"\"\"here, yes, HERE, right here on this very long success message.
                          Wow, how successful! You're the biggest success I've ever seen!
                          You should feel great about yourself! Give yourself a very big round of applause!
                          \"\"\"
                      , div [ Attributes.css [ Css.display Css.inlineBlock, Css.width (Css.px 20) ] ]
                          [ Svg.toHtml UiIcon.gear ]
                      ]
                  , text " to check out NoRedInk."
                  ]
                  Nothing
                  """
                ]
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
