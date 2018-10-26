/**
 * # CustomElement
 *
 * ## Importing
 * Just require the file to get the API
 * ```
 * CustomElements = require('CustomElements')
 * ```
 *
 * ## Functions
 * - `create`: Create and register a custom element
 * - `makeEvent`: Create a DOM event
 *
 * ## Creating a custom element
 * ```
 * CustomElements.create
 *   # This is where you specify the tag for your custom element. You would
 *   # use this custom element with `Html.node "my-custom-tag"`.
 *   tagName: 'my-cool-button'
 *
 *   # Initialize any local variables for the element or do whatever else you
 *   # might do in a class constructor. Takes no arguments.
 *   # NOTE: the element is NOT in the DOM at this point.
 *   initialize: ->
 *     @_hello = 'world'
 *     @_button = document.createElement('button')
 *
 *   # Do any setup work after the element has been inserted into the DOM.
 *   # Takes no arguments. This is a proxy for `connectedCallback` (see:
 *   # https://developer.mozilla.org/en-US/docs/Web/Web_Components/Using_custom_elements#Using_the_lifecycle_callbacks)
 *   onConnect: ->
 *     document.addEventListener('click', @_onDocClick)
 *     @_button.addEventListener('click', @_onButtonClick)
 *     @appendChild(@_button)
 *
 *   # Do any teardown work after the element has been removed from the DOM.
 *   # Takes no arguments. This is a proxy for `disconnectedCallback` (see:
 *   # https://developer.mozilla.org/en-US/docs/Web/Web_Components/Using_custom_elements#Using_the_lifecycle_callbacks)
 *   onDisconnect: ->
 *     document.removeEventListener('click', @_onDocClick)
 * *
 *   # Set up properties. These allow you to expose data to Elm's virtual DOM.
 *   # You can use any value that can be encoded as a `Json.Encode.Value`.
 *   # You'll often want to implement updates to some visual detail of your element
 *   # from within the setter of a property that controls it. Handlers will be
 *   # automatically bound to the correct value of `@`, so you don't need to worry
 *   # about method context.
 *   properties:
 *     hello:
 *       get: -> @_hello
 *       set: (value) ->
 *         @_hello = value
 *         @_button.textContent = value
 *
 *   # Set up methods that you can call from anywhere else in the configuration.
 *   # Methods will be automatically bound to the correct value of `@`, so you
 *   # don't need to worry about method context.
 *   methods:
 *     _onDocClick: ->
 *       alert('document clicked')
 *
 *     _onButtonClick: ->
 *       alert("clicked on #{@_hello} button")
 * ```
 *
 * ## Creating and triggering a custom event
 * Pass the string name of the event, like `"input"`, to `CustomElement.makeEvent`
 * along with some optional details. This purpose of this function is to normalize
 * the creation of a `CustomEvent` across browsers. If you pass any info to the
 * second argument, it will be available on `event.detail`. (see:
 * https://developer.mozilla.org/en-US/docs/Web/API/CustomEvent)
 * ```
 * CustomElement.create
 *   # ...
 *
 *   onConnect: ->
 *     @_textarea.addEventListener('input, @_onTextareaInput)
 *
 *   properties:
 *     textValue:
 *       get: -> @_textValue
 *       set: (value) -> @_textValue = value
 *
 *   methods:
 *     _onTextareaInput: ->
 *       @_textValue = @_textarea.value
 *       @dispatchEvent(CustomElement.makeEvent('textChanged', { text: @_textValue }))
 * ```
 */

exports.makeEvent = makeMakeEvent()

var makeClass = makeMakeClass()

exports.create = function create(config) {
  if (customElements.get(config.tagName)) {
    throw Error('Custom element with tag name ' + config.tagName + ' already exists.')
  }

  config.methods = config.methods || {}
  config.properties = config.properties || {}

  var Class = makeClass(config)
  Class.displayName = '<' + config.tagName + '> custom element'
  customElements.define(config.tagName, Class)
}

/**
 * Attempt to make an ES6 class using the Function constructor rather than
 * ordinary class syntax. The string we pass to the Function constructor is
 * static so there is no script injection risk. It allows us to catch
 * syntax errors at runtime for older browsers that don't support class and
 * fall back to an ES5 constructor function.
 */
function makeMakeClass() {
  try {
    return new Function('config', [
      "return class extends HTMLElement {",
      "  constructor() {",
      "    super()",
      "    for (var key in config.methods) {",
      "      if (!config.methods.hasOwnProperty(key)) continue",
      "      var method = config.methods[key]",
      "      if (typeof method !== 'function') continue",
      "      this[key] = method.bind(this)",
      "    }",
      "    if (typeof config.properties && config.properties !== null) {",
      "      Object.defineProperties(this, config.properties)",
      "    }",
      "    if (typeof config.initialize === 'function') {",
      "      config.initialize.call(this)",
      "    }",
      "  }",
      "  connectedCallback() {",
      "    if (typeof config.onConnect === 'function') return config.onConnect.call(this)",
      "  }",
      "  disconnectedCallback() {",
      "    if (typeof config.onDisconnect === 'function') return config.onDisconnect.call(this)",
      "  }",
      "}",
    ].join("\n"))
  } catch (e) {
    return function (config) {
      function CustomElementConstructor() {
        // This is the best we can do to trick modern browsers into thinking this
        // is a real, legitimate class constructor and not a plane old JS function.
        var _this = HTMLElement.call(this) || this
        for (var key in config.methods) {
          if (!config.methods.hasOwnProperty(key)) continue
          var method = config.methods[key]
          if (typeof method !== 'function') continue
          _this[key] = method.bind(_this)
        }
        Object.defineProperties(_this, config.properties)
        if (typeof config.initialize === 'function') {
          config.initialize.call(_this)
        }
        return _this
      }

      CustomElementConstructor.prototype = Object.create(HTMLElement.prototype)
      CustomElementConstructor.prototype.constructor = CustomElementConstructor
      CustomElementConstructor.prototype.connectedCallback = config.onConnect
      CustomElementConstructor.prototype.disconnectedCallback = config.onDisconnect

      return CustomElementConstructor
    }
  }
}

/**
 * Return a function for making an event based on what the browser supports.
 * IE11 doesn't support Event constructor, and uses the old Java-style
 * methods instead
 */ 
function makeMakeEvent() {
  try {
    // if calling Event with new works, do it that way
    var testEvent = new CustomEvent('myEvent', { detail: 1 })
    return function makeEventNewStyle(type, detail) {
      return new CustomEvent(type, { detail: detail })
    }
  } catch (_error) {
    // if calling CustomEvent with new throws an error, do it the old way
    return function makeEventOldStyle(type, detail) {
      var event = document.createEvent('CustomEvent')
      event.initCustomEvent(type, false, false, detail)
      return event
    }
  }
}
