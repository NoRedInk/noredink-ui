/**
 * @module CustomElement
 */

/**
 * Create a DOM Event without worrying about browser compatibility
 *
 * @param  {string} eventName The name of the event to create
 * @param {*} [detail] The optional details to attach to the event
 * @return {Event} A valid Event instance that can be dispatched on a DOM node
 * @example
 * var event = CustomElement.makeEvent('change', { name: this._secretInput.value })
 * this.dispatchEvent(event)
 */
exports.makeEvent = makeMakeEvent();

var makeClass = makeMakeClass();

function noOp() {}

/**
 * Register a custom element using some straightforward and convenient configuration
 *
 * @param {Object} config
 * @param {string} config.tagName The name of the tag for which to create a custom element. Must contain at least one `-` and must not have already been defined.
 * @param {Object<string, Object>} config.properties Getters and setters for properties on the custom element that can be accessed and set with `Html.Attributes.property`. These should map a property name onto a `get` function that returns a value and a `set` function that applies a value from the only argument.
 * @param {Object<string, function>} config.methods Functions that can be invoked by the custom element from anywhere. These methods are automatically bound to `this`.
 * @param {function} config.initialize Method invoked during setup that can be used to initialize internal state. This function is called whenever a new instance of the element is created.
 * @param {function} config.onConnect Method invoked whenever the element is inserted into the DOM.
 * @param {function} config.onDisconnect Method invoked whenever the element is removed from the DOM.
 * @param {function} config.onAttributeChange Method invoked whenever an observed attribute changes. Takes 3 arguments: the name of the attribute, the old value, and the new value.
 * @param {string[]} config.observedAttributes List of attributes that are watched such that onAttributeChange will be invoked when they change.
 * @example
 * CustomElements.create({
 *   // This is where you specify the tag for your custom element. You would
 *   // use this custom element with `Html.node "my-custom-tag"`.
 *   tagName: 'my-cool-button',
 *
 *   // Initialize any local variables for the element or do whatever else you
 *   // might do in a class constructor. Takes no arguments.
 *   // NOTE: the element is NOT in the DOM at this point.
 *   initialize: function() {
 *     this._hello = 'world'
 *     this._button = document.createElement('button')
 *   },
 *
 *   // Let the custom element runtime know that you want to be notified of
 *   // changes to the `hello` attribute
 *   observedAttributes: ['hello'],
 *
 *   // Do any updating when an attribute changes on the element. Note the
 *   // difference between attributes and properties of an element (see:
 *   // https://javascript.info/dom-attributes-and-properties). This is a
 *   // proxy for `attributeChangedCallback` (see:
 *   // https://developer.mozilla.org/en-US/docs/Web/Web_Components/Using_custom_elements#Using_the_lifecycle_callbacks).
 *   // Takes the name of the attribute that changed, the previous string value,
 *   // and the new string value.
 *   onAttributeChange: function(name, previous, next) {
 *     if (name === 'hello') this._hello = next
 *   },
 *
 *   // Do any setup work after the element has been inserted into the DOM.
 *   // Takes no arguments. This is a proxy for `connectedCallback` (see:
 *   // https://developer.mozilla.org/en-US/docs/Web/Web_Components/Using_custom_elements#Using_the_lifecycle_callbacks)
 *   onConnect: function() {
 *     document.addEventListener('click', this._onDocClick)
 *     this._button.addEventListener('click', this._onButtonClick)
 *     this.appendChild(this._button)
 *   },
 *
 *   // Do any teardown work after the element has been removed from the DOM.
 *   // Takes no arguments. This is a proxy for `disconnectedCallback` (see:
 *   // https://developer.mozilla.org/en-US/docs/Web/Web_Components/Using_custom_elements#Using_the_lifecycle_callbacks)
 *   onDisconnect: function() {
 *     document.removeEventListener('click', this._onDocClick)
 *   },
 *
 *   // Set up properties. These allow you to expose data to Elm's virtual DOM.
 *   // You can use any value that can be encoded as a `Json.Encode.Value`.
 *   // You'll often want to implement updates to some visual detail of your element
 *   // from within the setter of a property that controls it. Handlers will be
 *   // automatically bound to the correct value of `@`, so you don't need to worry
 *   // about method context.
 *   properties: {
 *     hello: {
 *       get: function() {
 *        return this._hello
 *       },
 *       set: function(value) {
 *         this._hello = value
 *         this._button.textContent = value
 *       }
 *     }
 *   },
 *
 *   // Set up methods that you can call from anywhere else in the configuration.
 *   // Methods will be automatically bound to the correct value of `@`, so you
 *   // don't need to worry about method context.
 *   methods: {
 *     _onDocClick: function() {
 *       alert('document clicked')
 *     },
 *     _onButtonClick: function() {
 *       alert('clicked on ' + this._hello + ' button')
 *     }
 *   }
 * })
 */
exports.create = function create(config) {
  if (customElements.get(config.tagName)) {
    throw Error(
      "Custom element with tag name " + config.tagName + " already exists."
    );
  }

  var observedAttributes = config.observedAttributes || [];
  var methods = config.methods || {};
  var properties = config.properties || {};
  var initialize = config.initialize || noOp;
  var onConnect = config.onConnect || noOp;
  var onDisconnect = config.onDisconnect || noOp;
  var onAttributeChange = config.onAttributeChange || noOp;

  var Class = makeClass();

  for (var key in methods) {
    if (!methods.hasOwnProperty(key)) continue;
    Class.prototype[key] = methods[key];
  }

  Object.defineProperties(Class.prototype, properties);

  Class.prototype.connectedCallback = onConnect;
  Class.prototype.disconnectedCallback = onDisconnect;
  Class.prototype.attributeChangedCallback = onAttributeChange;
  if (Array.isArray(observedAttributes)) {
    Object.defineProperty(Class, "observedAttributes", {
      get: function () {
        return observedAttributes;
      },
    });
  }

  Class.displayName = "<" + config.tagName + "> custom element";
  customElements.define(config.tagName, Class);
};

/**
 * Attempt to make an ES6 class using the Function constructor rather than
 * ordinary class syntax. The string we pass to the Function constructor is
 * static so there is no script injection risk. It allows us to catch
 * syntax errors at runtime for older browsers that don't support class and
 * fall back to an ES5 constructor function.
 */
function makeMakeClass() {
  try {
    return new Function(
      [
        "return class extends HTMLElement {",
        "  constructor() {",
        "    super()",
        "    for (var key in this) {",
        "      var value = this[key]",
        "      if (typeof value !== 'function') continue",
        "      this[key] = value.bind(this)",
        "    }",
        "  }",
        "}",
      ].join("\n")
    );
  } catch (e) {
    return function () {
      function Class() {
        // This is the best we can do to trick modern browsers into thinking this
        // is a real, legitimate class constructor and not a plane old JS function.
        var _this = HTMLElement.call(this) || this;
        for (var key in _this) {
          var value = _this[key];
          if (typeof value !== "function") continue;
          _this[key] = value.bind(_this);
        }
        return _this;
      }
      Class.prototype = Object.create(HTMLElement.prototype);
      Class.prototype.constructor = Class;
      return Class;
    };
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
    var testEvent = new CustomEvent("myEvent", { detail: 1 });
    return function makeEventNewStyle(type, detail) {
      return new CustomEvent(type, { detail: detail });
    };
  } catch (_error) {
    // if calling CustomEvent with new throws an error, do it the old way
    return function makeEventOldStyle(type, detail) {
      var event = document.createEvent("CustomEvent");
      event.initCustomEvent(type, false, false, detail);
      return event;
    };
  }
}
