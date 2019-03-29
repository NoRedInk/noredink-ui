(function(){function r(e,n,t){function o(i,f){if(!n[i]){if(!e[i]){var c="function"==typeof require&&require;if(!f&&c)return c(i,!0);if(u)return u(i,!0);var a=new Error("Cannot find module '"+i+"'");throw a.code="MODULE_NOT_FOUND",a}var p=n[i]={exports:{}};e[i][0].call(p.exports,function(r){var n=e[i][1][r];return o(n||r)},p,p.exports,r,e,n,t)}return n[i].exports}for(var u="function"==typeof require&&require,i=0;i<t.length;i++)o(t[i]);return o}return r})()({1:[function(require,module,exports){
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
exports.makeEvent = makeMakeEvent()

var makeClass = makeMakeClass()

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
    throw Error('Custom element with tag name ' + config.tagName + ' already exists.')
  }

  var observedAttributes = config.observedAttributes || []
  var methods = config.methods || {}
  var properties = config.properties || {}
  var initialize = config.initialize || noOp
  var onConnect = config.onConnect || noOp
  var onDisconnect = config.onDisconnect || noOp
  var onAttributeChange = config.onAttributeChange || noOp

  var Class = makeClass()
  
  for (var key in methods) {
    if (!methods.hasOwnProperty(key)) continue
    Class.prototype[key] = methods[key]
  }

  Object.defineProperties(Class.prototype, properties)

  Class.prototype.connectedCallback = onConnect
  Class.prototype.disconnectedCallback = onDisconnect
  Class.prototype.attributeChangedCallback = onAttributeChange
  if (Array.isArray(observedAttributes)) {
    Object.defineProperty(Class, 'observedAttributes', {
      get: function () { return observedAttributes }
    })
  }

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
    return new Function([
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
    ].join("\n"))
  } catch (e) {
    return function () {
      function Class() {
        // This is the best we can do to trick modern browsers into thinking this
        // is a real, legitimate class constructor and not a plane old JS function.
        var _this = HTMLElement.call(this) || this
        for (var key in _this) {
          var value = _this[key]
          if (typeof value !== 'function') continue
          _this[key] = value.bind(_this)
        }
        return _this
      }
      Class.prototype = Object.create(HTMLElement.prototype)
      Class.prototype.constructor = Class    
      return Class
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

},{}],2:[function(require,module,exports){
CustomElement = require('../CustomElement')

CustomElement.create({
  tagName: 'nri-textarea-v3',

  initialize: function() {
    this._autoresize = false
  },

  onConnect: function() {
    this._textarea = this.querySelector('textarea')
    this._updateListener()
  },

  observedAttributes: ['data-autoresize'],

  onAttributeChange: function(name, previous, next) {
    if (name === 'data-autoresize') {
      this._autoresize = next !== null
      if (!this._textarea) return
      this._updateListener()
    }
  },

  methods: {
    _updateListener: function() {
      if (this._autoresize) {
        this._textarea.addEventListener('input', this._resize)
        this._resize()
      } else {
        this._textarea.removeEventListener('input', this._resize)
      }
    },

    _resize: function() {
      var minHeight = null
      if (this._textarea.style.minHeight) {
        minHeight = parseInt(this._textarea.style.minHeight, 10)
      } else {
        minHeight = parseInt(window.getComputedStyle(this._textarea).minHeight, 10)
      }
      if (minHeight === 0) {
        minHeight = parseInt(window.getComputedStyle(this._textarea).height, 10)
      }
    
      this._textarea.style.overflowY = 'hidden'
      this._textarea.style.minHeight = minHeight + 'px'
      this._textarea.style.transition = 'none'
      if (this._textarea.scrollHeight > minHeight) {
        this._textarea.style.height = minHeight + 'px'
        this._textarea.style.height = this._textarea.scrollHeight + 'px'
      } else {
        this._textarea.style.height = minHeight + 'px'
      }
    }
  }
})

},{"../CustomElement":1}],3:[function(require,module,exports){
CustomElement = require('../CustomElement')

CustomElement.create({
  tagName: 'nri-textarea-v4',

  initialize: function() {
    this._autoresize = false
  },

  onConnect: function() {
    this._textarea = this.querySelector('textarea')
    this._updateListener()
  },

  observedAttributes: ['data-autoresize'],

  onAttributeChange: function(name, previous, next) {
    if (name === 'data-autoresize') {
      this._autoresize = next !== null
      if (!this._textarea) return
      this._updateListener()
    }
  },

  methods: {
    _updateListener: function() {
      if (this._autoresize) {
        this._textarea.addEventListener('input', this._resize)
        this._resize()
      } else {
        this._textarea.removeEventListener('input', this._resize)
      }
    },

    _resize: function() {
      var minHeight = null
      if (this._textarea.style.minHeight) {
        minHeight = parseInt(this._textarea.style.minHeight, 10)
      } else {
        minHeight = parseInt(window.getComputedStyle(this._textarea).minHeight, 10)
      }
      if (minHeight === 0) {
        minHeight = parseInt(window.getComputedStyle(this._textarea).height, 10)
      }

      this._textarea.style.overflowY = 'hidden'
      this._textarea.style.minHeight = minHeight + 'px'
      this._textarea.style.transition = 'none'
      if (this._textarea.scrollHeight > minHeight) {
        this._textarea.style.height = minHeight + 'px'
        this._textarea.style.height = this._textarea.scrollHeight + 'px'
      } else {
        this._textarea.style.height = minHeight + 'px'
      }
    }
  }
})

},{"../CustomElement":1}],4:[function(require,module,exports){
require('./TextArea/V3')
require('./TextArea/V4')

exports.CustomElement = require('./CustomElement')

},{"./CustomElement":1,"./TextArea/V3":2,"./TextArea/V4":3}],5:[function(require,module,exports){
/*!
 * clipboard.js v2.0.4
 * https://zenorocha.github.io/clipboard.js
 * 
 * Licensed MIT © Zeno Rocha
 */
(function webpackUniversalModuleDefinition(root, factory) {
	if(typeof exports === 'object' && typeof module === 'object')
		module.exports = factory();
	else if(typeof define === 'function' && define.amd)
		define([], factory);
	else if(typeof exports === 'object')
		exports["ClipboardJS"] = factory();
	else
		root["ClipboardJS"] = factory();
})(this, function() {
return /******/ (function(modules) { // webpackBootstrap
/******/ 	// The module cache
/******/ 	var installedModules = {};
/******/
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/
/******/ 		// Check if module is in cache
/******/ 		if(installedModules[moduleId]) {
/******/ 			return installedModules[moduleId].exports;
/******/ 		}
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = installedModules[moduleId] = {
/******/ 			i: moduleId,
/******/ 			l: false,
/******/ 			exports: {}
/******/ 		};
/******/
/******/ 		// Execute the module function
/******/ 		modules[moduleId].call(module.exports, module, module.exports, __webpack_require__);
/******/
/******/ 		// Flag the module as loaded
/******/ 		module.l = true;
/******/
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/
/******/
/******/ 	// expose the modules object (__webpack_modules__)
/******/ 	__webpack_require__.m = modules;
/******/
/******/ 	// expose the module cache
/******/ 	__webpack_require__.c = installedModules;
/******/
/******/ 	// define getter function for harmony exports
/******/ 	__webpack_require__.d = function(exports, name, getter) {
/******/ 		if(!__webpack_require__.o(exports, name)) {
/******/ 			Object.defineProperty(exports, name, { enumerable: true, get: getter });
/******/ 		}
/******/ 	};
/******/
/******/ 	// define __esModule on exports
/******/ 	__webpack_require__.r = function(exports) {
/******/ 		if(typeof Symbol !== 'undefined' && Symbol.toStringTag) {
/******/ 			Object.defineProperty(exports, Symbol.toStringTag, { value: 'Module' });
/******/ 		}
/******/ 		Object.defineProperty(exports, '__esModule', { value: true });
/******/ 	};
/******/
/******/ 	// create a fake namespace object
/******/ 	// mode & 1: value is a module id, require it
/******/ 	// mode & 2: merge all properties of value into the ns
/******/ 	// mode & 4: return value when already ns object
/******/ 	// mode & 8|1: behave like require
/******/ 	__webpack_require__.t = function(value, mode) {
/******/ 		if(mode & 1) value = __webpack_require__(value);
/******/ 		if(mode & 8) return value;
/******/ 		if((mode & 4) && typeof value === 'object' && value && value.__esModule) return value;
/******/ 		var ns = Object.create(null);
/******/ 		__webpack_require__.r(ns);
/******/ 		Object.defineProperty(ns, 'default', { enumerable: true, value: value });
/******/ 		if(mode & 2 && typeof value != 'string') for(var key in value) __webpack_require__.d(ns, key, function(key) { return value[key]; }.bind(null, key));
/******/ 		return ns;
/******/ 	};
/******/
/******/ 	// getDefaultExport function for compatibility with non-harmony modules
/******/ 	__webpack_require__.n = function(module) {
/******/ 		var getter = module && module.__esModule ?
/******/ 			function getDefault() { return module['default']; } :
/******/ 			function getModuleExports() { return module; };
/******/ 		__webpack_require__.d(getter, 'a', getter);
/******/ 		return getter;
/******/ 	};
/******/
/******/ 	// Object.prototype.hasOwnProperty.call
/******/ 	__webpack_require__.o = function(object, property) { return Object.prototype.hasOwnProperty.call(object, property); };
/******/
/******/ 	// __webpack_public_path__
/******/ 	__webpack_require__.p = "";
/******/
/******/
/******/ 	// Load entry module and return exports
/******/ 	return __webpack_require__(__webpack_require__.s = 0);
/******/ })
/************************************************************************/
/******/ ([
/* 0 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


var _typeof = typeof Symbol === "function" && typeof Symbol.iterator === "symbol" ? function (obj) { return typeof obj; } : function (obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; };

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _clipboardAction = __webpack_require__(1);

var _clipboardAction2 = _interopRequireDefault(_clipboardAction);

var _tinyEmitter = __webpack_require__(3);

var _tinyEmitter2 = _interopRequireDefault(_tinyEmitter);

var _goodListener = __webpack_require__(4);

var _goodListener2 = _interopRequireDefault(_goodListener);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; }

/**
 * Base class which takes one or more elements, adds event listeners to them,
 * and instantiates a new `ClipboardAction` on each click.
 */
var Clipboard = function (_Emitter) {
    _inherits(Clipboard, _Emitter);

    /**
     * @param {String|HTMLElement|HTMLCollection|NodeList} trigger
     * @param {Object} options
     */
    function Clipboard(trigger, options) {
        _classCallCheck(this, Clipboard);

        var _this = _possibleConstructorReturn(this, (Clipboard.__proto__ || Object.getPrototypeOf(Clipboard)).call(this));

        _this.resolveOptions(options);
        _this.listenClick(trigger);
        return _this;
    }

    /**
     * Defines if attributes would be resolved using internal setter functions
     * or custom functions that were passed in the constructor.
     * @param {Object} options
     */


    _createClass(Clipboard, [{
        key: 'resolveOptions',
        value: function resolveOptions() {
            var options = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : {};

            this.action = typeof options.action === 'function' ? options.action : this.defaultAction;
            this.target = typeof options.target === 'function' ? options.target : this.defaultTarget;
            this.text = typeof options.text === 'function' ? options.text : this.defaultText;
            this.container = _typeof(options.container) === 'object' ? options.container : document.body;
        }

        /**
         * Adds a click event listener to the passed trigger.
         * @param {String|HTMLElement|HTMLCollection|NodeList} trigger
         */

    }, {
        key: 'listenClick',
        value: function listenClick(trigger) {
            var _this2 = this;

            this.listener = (0, _goodListener2.default)(trigger, 'click', function (e) {
                return _this2.onClick(e);
            });
        }

        /**
         * Defines a new `ClipboardAction` on each click event.
         * @param {Event} e
         */

    }, {
        key: 'onClick',
        value: function onClick(e) {
            var trigger = e.delegateTarget || e.currentTarget;

            if (this.clipboardAction) {
                this.clipboardAction = null;
            }

            this.clipboardAction = new _clipboardAction2.default({
                action: this.action(trigger),
                target: this.target(trigger),
                text: this.text(trigger),
                container: this.container,
                trigger: trigger,
                emitter: this
            });
        }

        /**
         * Default `action` lookup function.
         * @param {Element} trigger
         */

    }, {
        key: 'defaultAction',
        value: function defaultAction(trigger) {
            return getAttributeValue('action', trigger);
        }

        /**
         * Default `target` lookup function.
         * @param {Element} trigger
         */

    }, {
        key: 'defaultTarget',
        value: function defaultTarget(trigger) {
            var selector = getAttributeValue('target', trigger);

            if (selector) {
                return document.querySelector(selector);
            }
        }

        /**
         * Returns the support of the given action, or all actions if no action is
         * given.
         * @param {String} [action]
         */

    }, {
        key: 'defaultText',


        /**
         * Default `text` lookup function.
         * @param {Element} trigger
         */
        value: function defaultText(trigger) {
            return getAttributeValue('text', trigger);
        }

        /**
         * Destroy lifecycle.
         */

    }, {
        key: 'destroy',
        value: function destroy() {
            this.listener.destroy();

            if (this.clipboardAction) {
                this.clipboardAction.destroy();
                this.clipboardAction = null;
            }
        }
    }], [{
        key: 'isSupported',
        value: function isSupported() {
            var action = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : ['copy', 'cut'];

            var actions = typeof action === 'string' ? [action] : action;
            var support = !!document.queryCommandSupported;

            actions.forEach(function (action) {
                support = support && !!document.queryCommandSupported(action);
            });

            return support;
        }
    }]);

    return Clipboard;
}(_tinyEmitter2.default);

/**
 * Helper function to retrieve attribute value.
 * @param {String} suffix
 * @param {Element} element
 */


function getAttributeValue(suffix, element) {
    var attribute = 'data-clipboard-' + suffix;

    if (!element.hasAttribute(attribute)) {
        return;
    }

    return element.getAttribute(attribute);
}

module.exports = Clipboard;

/***/ }),
/* 1 */
/***/ (function(module, exports, __webpack_require__) {

"use strict";


var _typeof = typeof Symbol === "function" && typeof Symbol.iterator === "symbol" ? function (obj) { return typeof obj; } : function (obj) { return obj && typeof Symbol === "function" && obj.constructor === Symbol && obj !== Symbol.prototype ? "symbol" : typeof obj; };

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _select = __webpack_require__(2);

var _select2 = _interopRequireDefault(_select);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

/**
 * Inner class which performs selection from either `text` or `target`
 * properties and then executes copy or cut operations.
 */
var ClipboardAction = function () {
    /**
     * @param {Object} options
     */
    function ClipboardAction(options) {
        _classCallCheck(this, ClipboardAction);

        this.resolveOptions(options);
        this.initSelection();
    }

    /**
     * Defines base properties passed from constructor.
     * @param {Object} options
     */


    _createClass(ClipboardAction, [{
        key: 'resolveOptions',
        value: function resolveOptions() {
            var options = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : {};

            this.action = options.action;
            this.container = options.container;
            this.emitter = options.emitter;
            this.target = options.target;
            this.text = options.text;
            this.trigger = options.trigger;

            this.selectedText = '';
        }

        /**
         * Decides which selection strategy is going to be applied based
         * on the existence of `text` and `target` properties.
         */

    }, {
        key: 'initSelection',
        value: function initSelection() {
            if (this.text) {
                this.selectFake();
            } else if (this.target) {
                this.selectTarget();
            }
        }

        /**
         * Creates a fake textarea element, sets its value from `text` property,
         * and makes a selection on it.
         */

    }, {
        key: 'selectFake',
        value: function selectFake() {
            var _this = this;

            var isRTL = document.documentElement.getAttribute('dir') == 'rtl';

            this.removeFake();

            this.fakeHandlerCallback = function () {
                return _this.removeFake();
            };
            this.fakeHandler = this.container.addEventListener('click', this.fakeHandlerCallback) || true;

            this.fakeElem = document.createElement('textarea');
            // Prevent zooming on iOS
            this.fakeElem.style.fontSize = '12pt';
            // Reset box model
            this.fakeElem.style.border = '0';
            this.fakeElem.style.padding = '0';
            this.fakeElem.style.margin = '0';
            // Move element out of screen horizontally
            this.fakeElem.style.position = 'absolute';
            this.fakeElem.style[isRTL ? 'right' : 'left'] = '-9999px';
            // Move element to the same position vertically
            var yPosition = window.pageYOffset || document.documentElement.scrollTop;
            this.fakeElem.style.top = yPosition + 'px';

            this.fakeElem.setAttribute('readonly', '');
            this.fakeElem.value = this.text;

            this.container.appendChild(this.fakeElem);

            this.selectedText = (0, _select2.default)(this.fakeElem);
            this.copyText();
        }

        /**
         * Only removes the fake element after another click event, that way
         * a user can hit `Ctrl+C` to copy because selection still exists.
         */

    }, {
        key: 'removeFake',
        value: function removeFake() {
            if (this.fakeHandler) {
                this.container.removeEventListener('click', this.fakeHandlerCallback);
                this.fakeHandler = null;
                this.fakeHandlerCallback = null;
            }

            if (this.fakeElem) {
                this.container.removeChild(this.fakeElem);
                this.fakeElem = null;
            }
        }

        /**
         * Selects the content from element passed on `target` property.
         */

    }, {
        key: 'selectTarget',
        value: function selectTarget() {
            this.selectedText = (0, _select2.default)(this.target);
            this.copyText();
        }

        /**
         * Executes the copy operation based on the current selection.
         */

    }, {
        key: 'copyText',
        value: function copyText() {
            var succeeded = void 0;

            try {
                succeeded = document.execCommand(this.action);
            } catch (err) {
                succeeded = false;
            }

            this.handleResult(succeeded);
        }

        /**
         * Fires an event based on the copy operation result.
         * @param {Boolean} succeeded
         */

    }, {
        key: 'handleResult',
        value: function handleResult(succeeded) {
            this.emitter.emit(succeeded ? 'success' : 'error', {
                action: this.action,
                text: this.selectedText,
                trigger: this.trigger,
                clearSelection: this.clearSelection.bind(this)
            });
        }

        /**
         * Moves focus away from `target` and back to the trigger, removes current selection.
         */

    }, {
        key: 'clearSelection',
        value: function clearSelection() {
            if (this.trigger) {
                this.trigger.focus();
            }

            window.getSelection().removeAllRanges();
        }

        /**
         * Sets the `action` to be performed which can be either 'copy' or 'cut'.
         * @param {String} action
         */

    }, {
        key: 'destroy',


        /**
         * Destroy lifecycle.
         */
        value: function destroy() {
            this.removeFake();
        }
    }, {
        key: 'action',
        set: function set() {
            var action = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : 'copy';

            this._action = action;

            if (this._action !== 'copy' && this._action !== 'cut') {
                throw new Error('Invalid "action" value, use either "copy" or "cut"');
            }
        }

        /**
         * Gets the `action` property.
         * @return {String}
         */
        ,
        get: function get() {
            return this._action;
        }

        /**
         * Sets the `target` property using an element
         * that will be have its content copied.
         * @param {Element} target
         */

    }, {
        key: 'target',
        set: function set(target) {
            if (target !== undefined) {
                if (target && (typeof target === 'undefined' ? 'undefined' : _typeof(target)) === 'object' && target.nodeType === 1) {
                    if (this.action === 'copy' && target.hasAttribute('disabled')) {
                        throw new Error('Invalid "target" attribute. Please use "readonly" instead of "disabled" attribute');
                    }

                    if (this.action === 'cut' && (target.hasAttribute('readonly') || target.hasAttribute('disabled'))) {
                        throw new Error('Invalid "target" attribute. You can\'t cut text from elements with "readonly" or "disabled" attributes');
                    }

                    this._target = target;
                } else {
                    throw new Error('Invalid "target" value, use a valid Element');
                }
            }
        }

        /**
         * Gets the `target` property.
         * @return {String|HTMLElement}
         */
        ,
        get: function get() {
            return this._target;
        }
    }]);

    return ClipboardAction;
}();

module.exports = ClipboardAction;

/***/ }),
/* 2 */
/***/ (function(module, exports) {

function select(element) {
    var selectedText;

    if (element.nodeName === 'SELECT') {
        element.focus();

        selectedText = element.value;
    }
    else if (element.nodeName === 'INPUT' || element.nodeName === 'TEXTAREA') {
        var isReadOnly = element.hasAttribute('readonly');

        if (!isReadOnly) {
            element.setAttribute('readonly', '');
        }

        element.select();
        element.setSelectionRange(0, element.value.length);

        if (!isReadOnly) {
            element.removeAttribute('readonly');
        }

        selectedText = element.value;
    }
    else {
        if (element.hasAttribute('contenteditable')) {
            element.focus();
        }

        var selection = window.getSelection();
        var range = document.createRange();

        range.selectNodeContents(element);
        selection.removeAllRanges();
        selection.addRange(range);

        selectedText = selection.toString();
    }

    return selectedText;
}

module.exports = select;


/***/ }),
/* 3 */
/***/ (function(module, exports) {

function E () {
  // Keep this empty so it's easier to inherit from
  // (via https://github.com/lipsmack from https://github.com/scottcorgan/tiny-emitter/issues/3)
}

E.prototype = {
  on: function (name, callback, ctx) {
    var e = this.e || (this.e = {});

    (e[name] || (e[name] = [])).push({
      fn: callback,
      ctx: ctx
    });

    return this;
  },

  once: function (name, callback, ctx) {
    var self = this;
    function listener () {
      self.off(name, listener);
      callback.apply(ctx, arguments);
    };

    listener._ = callback
    return this.on(name, listener, ctx);
  },

  emit: function (name) {
    var data = [].slice.call(arguments, 1);
    var evtArr = ((this.e || (this.e = {}))[name] || []).slice();
    var i = 0;
    var len = evtArr.length;

    for (i; i < len; i++) {
      evtArr[i].fn.apply(evtArr[i].ctx, data);
    }

    return this;
  },

  off: function (name, callback) {
    var e = this.e || (this.e = {});
    var evts = e[name];
    var liveEvents = [];

    if (evts && callback) {
      for (var i = 0, len = evts.length; i < len; i++) {
        if (evts[i].fn !== callback && evts[i].fn._ !== callback)
          liveEvents.push(evts[i]);
      }
    }

    // Remove event from queue to prevent memory leak
    // Suggested by https://github.com/lazd
    // Ref: https://github.com/scottcorgan/tiny-emitter/commit/c6ebfaa9bc973b33d110a84a307742b7cf94c953#commitcomment-5024910

    (liveEvents.length)
      ? e[name] = liveEvents
      : delete e[name];

    return this;
  }
};

module.exports = E;


/***/ }),
/* 4 */
/***/ (function(module, exports, __webpack_require__) {

var is = __webpack_require__(5);
var delegate = __webpack_require__(6);

/**
 * Validates all params and calls the right
 * listener function based on its target type.
 *
 * @param {String|HTMLElement|HTMLCollection|NodeList} target
 * @param {String} type
 * @param {Function} callback
 * @return {Object}
 */
function listen(target, type, callback) {
    if (!target && !type && !callback) {
        throw new Error('Missing required arguments');
    }

    if (!is.string(type)) {
        throw new TypeError('Second argument must be a String');
    }

    if (!is.fn(callback)) {
        throw new TypeError('Third argument must be a Function');
    }

    if (is.node(target)) {
        return listenNode(target, type, callback);
    }
    else if (is.nodeList(target)) {
        return listenNodeList(target, type, callback);
    }
    else if (is.string(target)) {
        return listenSelector(target, type, callback);
    }
    else {
        throw new TypeError('First argument must be a String, HTMLElement, HTMLCollection, or NodeList');
    }
}

/**
 * Adds an event listener to a HTML element
 * and returns a remove listener function.
 *
 * @param {HTMLElement} node
 * @param {String} type
 * @param {Function} callback
 * @return {Object}
 */
function listenNode(node, type, callback) {
    node.addEventListener(type, callback);

    return {
        destroy: function() {
            node.removeEventListener(type, callback);
        }
    }
}

/**
 * Add an event listener to a list of HTML elements
 * and returns a remove listener function.
 *
 * @param {NodeList|HTMLCollection} nodeList
 * @param {String} type
 * @param {Function} callback
 * @return {Object}
 */
function listenNodeList(nodeList, type, callback) {
    Array.prototype.forEach.call(nodeList, function(node) {
        node.addEventListener(type, callback);
    });

    return {
        destroy: function() {
            Array.prototype.forEach.call(nodeList, function(node) {
                node.removeEventListener(type, callback);
            });
        }
    }
}

/**
 * Add an event listener to a selector
 * and returns a remove listener function.
 *
 * @param {String} selector
 * @param {String} type
 * @param {Function} callback
 * @return {Object}
 */
function listenSelector(selector, type, callback) {
    return delegate(document.body, selector, type, callback);
}

module.exports = listen;


/***/ }),
/* 5 */
/***/ (function(module, exports) {

/**
 * Check if argument is a HTML element.
 *
 * @param {Object} value
 * @return {Boolean}
 */
exports.node = function(value) {
    return value !== undefined
        && value instanceof HTMLElement
        && value.nodeType === 1;
};

/**
 * Check if argument is a list of HTML elements.
 *
 * @param {Object} value
 * @return {Boolean}
 */
exports.nodeList = function(value) {
    var type = Object.prototype.toString.call(value);

    return value !== undefined
        && (type === '[object NodeList]' || type === '[object HTMLCollection]')
        && ('length' in value)
        && (value.length === 0 || exports.node(value[0]));
};

/**
 * Check if argument is a string.
 *
 * @param {Object} value
 * @return {Boolean}
 */
exports.string = function(value) {
    return typeof value === 'string'
        || value instanceof String;
};

/**
 * Check if argument is a function.
 *
 * @param {Object} value
 * @return {Boolean}
 */
exports.fn = function(value) {
    var type = Object.prototype.toString.call(value);

    return type === '[object Function]';
};


/***/ }),
/* 6 */
/***/ (function(module, exports, __webpack_require__) {

var closest = __webpack_require__(7);

/**
 * Delegates event to a selector.
 *
 * @param {Element} element
 * @param {String} selector
 * @param {String} type
 * @param {Function} callback
 * @param {Boolean} useCapture
 * @return {Object}
 */
function _delegate(element, selector, type, callback, useCapture) {
    var listenerFn = listener.apply(this, arguments);

    element.addEventListener(type, listenerFn, useCapture);

    return {
        destroy: function() {
            element.removeEventListener(type, listenerFn, useCapture);
        }
    }
}

/**
 * Delegates event to a selector.
 *
 * @param {Element|String|Array} [elements]
 * @param {String} selector
 * @param {String} type
 * @param {Function} callback
 * @param {Boolean} useCapture
 * @return {Object}
 */
function delegate(elements, selector, type, callback, useCapture) {
    // Handle the regular Element usage
    if (typeof elements.addEventListener === 'function') {
        return _delegate.apply(null, arguments);
    }

    // Handle Element-less usage, it defaults to global delegation
    if (typeof type === 'function') {
        // Use `document` as the first parameter, then apply arguments
        // This is a short way to .unshift `arguments` without running into deoptimizations
        return _delegate.bind(null, document).apply(null, arguments);
    }

    // Handle Selector-based usage
    if (typeof elements === 'string') {
        elements = document.querySelectorAll(elements);
    }

    // Handle Array-like based usage
    return Array.prototype.map.call(elements, function (element) {
        return _delegate(element, selector, type, callback, useCapture);
    });
}

/**
 * Finds closest match and invokes callback.
 *
 * @param {Element} element
 * @param {String} selector
 * @param {String} type
 * @param {Function} callback
 * @return {Function}
 */
function listener(element, selector, type, callback) {
    return function(e) {
        e.delegateTarget = closest(e.target, selector);

        if (e.delegateTarget) {
            callback.call(element, e);
        }
    }
}

module.exports = delegate;


/***/ }),
/* 7 */
/***/ (function(module, exports) {

var DOCUMENT_NODE_TYPE = 9;

/**
 * A polyfill for Element.matches()
 */
if (typeof Element !== 'undefined' && !Element.prototype.matches) {
    var proto = Element.prototype;

    proto.matches = proto.matchesSelector ||
                    proto.mozMatchesSelector ||
                    proto.msMatchesSelector ||
                    proto.oMatchesSelector ||
                    proto.webkitMatchesSelector;
}

/**
 * Finds the closest parent that matches a selector.
 *
 * @param {Element} element
 * @param {String} selector
 * @return {Function}
 */
function closest (element, selector) {
    while (element && element.nodeType !== DOCUMENT_NODE_TYPE) {
        if (typeof element.matches === 'function' &&
            element.matches(selector)) {
          return element;
        }
        element = element.parentNode;
    }
}

module.exports = closest;


/***/ })
/******/ ]);
});
},{}],6:[function(require,module,exports){
var ClipboardJS = require('clipboard')

// Used to configure the copyToClipboard button
clipboard = new ClipboardJS("#clipboard-container button");

},{"clipboard":5}],7:[function(require,module,exports){
(function () {
  var hack = document.documentElement.doScroll;
  var loaded = (hack ? /^loaded|^c/ : /^loaded|^i|^c/).test(document.readyState);

  function onLoad() {
    var container = document.createElement("div");
    container.style.display = "none";
    container.id = "icon-sprite";
    container.setAttribute("data-build-datetime", "2018-05-02 08:12:17.887294");
    container.innerHTML =
      '<svg><symbol id="icon-checkmark" viewBox="0 0 21.7 17.1"><path d="M7.6 17.1c-.5 0-1-.2-1.4-.6L.6 11.1c-.8-.8-.8-2-.1-2.8.8-.8 2-.8 2.8-.1l4.1 4L18.2.7c.8-.8 2-.9 2.8-.1s.9 2 .1 2.8l-12 13c-.4.5-.9.7-1.5.7.1 0 0 0 0 0"/></symbol><symbol id="icon-skip" viewBox="0 0 40 25"><path d="M24.8 23.437c0 .416-.16.811-.447 1.104-.286.293-.676.46-1.08.46h-.882c-.845 0-1.53-.7-1.53-1.564v-8.874L4.406 24.58l-.23.14h-.105a2.158 2.158 0 0 1-.917.203c-1.134.035-2.088-.859-2.154-2.016V2.063C1.04.89 2.003-.034 3.154.001c.341.006.676.09.977.252h.106l.138.079L20.86 10.407V1.566C20.86.7 21.545 0 22.39 0h.882c.404 0 .794.167 1.08.46.287.293.447.689.447 1.105l.001 21.87z" fill-rule="evenodd"/></symbol><symbol id="icon-arrow-down" viewBox="0 0 25 15"><path d="M19.268 20.732a2.5 2.5 0 1 1-3.536 3.536l-10-10a2.5 2.5 0 0 1 0-3.536l10-10a2.5 2.5 0 1 1 3.536 3.536L11.036 12.5l8.232 8.232z" fill-rule="evenodd" transform="rotate(-90 10 10)"/></symbol><symbol id="icon-bulb" viewBox="0 0 23 25"><path d="M21.6 12.5H19c-.3 0-.6.3-.6.6s.3.6.6.6h2.6c.3 0 .6-.3.6-.6s-.3-.6-.6-.6zM18.1 9.3c.1 0 .2 0 .3-.1l2.3-1.4c.2-.1.3-.5.1-.8-.2-.3-.5-.4-.8-.2l-2.3 1.4c-.2.1-.3.4-.3.7.2.2.4.4.7.4zM17.1 2c-.3-.2-.6-.1-.8.2l-1.5 2.2c-.1.1-.1.3-.1.5s.1.3.2.4c.1.1.3.1.4.1.2 0 .3-.1.4-.3l1.5-2.2c.2-.3.2-.7-.1-.9zM6.7 5.4c.2 0 .4-.1.5-.3.1-.2.1-.4 0-.6L5.7 2.2c-.1-.1-.3-.2-.4-.3-.2 0-.3 0-.4.1-.2.1-.3.3-.3.4 0 .2 0 .3.1.4L6.2 5c.1.3.3.4.5.4zM4 8.2L1.7 6.8c-.2-.1-.6 0-.7.2-.2.3-.1.6.2.8l2.3 1.4c.1.1.3.1.4.1l.3-.3c.1-.1.1-.3.1-.5-.1-.1-.2-.3-.3-.3zM20.6 17.8l-2.2-1.4c-.3-.2-.6-.1-.8.2-.2.3-.1.6.2.8l2.3 1.4c.3.1.6 0 .7-.2.2-.3.1-.6-.2-.8zM3.5 16.4l-2.3 1.4c-.1 0-.2.2-.3.3 0 .2 0 .3.1.5.1.1.2.2.4.3.1 0 .3 0 .4-.1L4 17.4c.3-.2.3-.5.2-.8-.1-.2-.5-.3-.7-.2zM3.7 13.1c0-.3-.3-.6-.6-.6H.6c-.3 0-.6.3-.6.6s.3.6.6.6h2.6c.1 0 .3-.1.4-.2.1-.1.1-.3.1-.4zM10.7 3.9c.3 0 .6-.3.6-.6V.6c0-.3-.3-.6-.6-.6s-.6.3-.6.6v2.7c0 .2.1.3.2.4s.3.2.4.2zM13.4 20.2H8.9c-.3 0-.6.3-.6.6s.3.6.6.6h4.5c.3 0 .6-.3.6-.6s-.3-.6-.6-.6zM10 23.5v.3c0 .4.3.7.6.7h.9c.4 0 .6-.3.6-.7v-.3c.7 0 1.3-.7 1.3-1.4H8.8c.1.7.6 1.3 1.2 1.4zM11.2 6.7c-3.1 0-5.6 2.7-5.6 6 0 .8.1 1.5.4 2.3 0 .1.1.2.1.3.2.6.6 1.1 1 1.6l1.4 2.3h5.4l1.4-2.3c.4-.5.7-1 1-1.6 0-.1.1-.2.1-.3.3-.7.4-1.5.4-2.3 0-3.3-2.5-6-5.6-6zM10.9 9c-.6 0-1.2.2-1.7.5-1.1.7-1.6 1.9-1.7 3.5 0 .3-.3.6-.6.6-.1 0-.3-.1-.4-.2-.1-.1-.2-.3-.2-.4 0-2.7 1.3-4 2.3-4.6.7-.4 1.4-.6 2.2-.7.3 0 .6.3.6.6.1.4-.2.7-.5.7z" transform="translate(.208 .052)"/></symbol><symbol id="icon-flipper" viewBox="0 0 21 18"><path d="M6 12.59h8.59V4H6v8.59zm.955-.954h6.681V4.955H6.955v6.681zm-.273 4.568a.477.477 0 1 1 0 .955H5.25a3.345 3.345 0 0 1-3.341-3.34v-2.19L.815 12.724a.477.477 0 1 1-.675-.675l1.909-1.91a.477.477 0 0 1 .675 0l1.909 1.91a.477.477 0 1 1-.675.675l-1.094-1.095v2.19a2.388 2.388 0 0 0 2.386 2.385h1.432zM20.86 4.435a.477.477 0 0 0-.675 0L19.091 5.53V3.34A3.345 3.345 0 0 0 15.75 0h-1.432a.477.477 0 1 0 0 .955h1.432a2.388 2.388 0 0 1 2.386 2.386V5.53l-1.094-1.095a.477.477 0 1 0-.675.675l1.91 1.91a.475.475 0 0 0 .674 0l1.91-1.91a.477.477 0 0 0 0-.675" fill-rule="evenodd"/><path d="M10 12h.716V4H10z" fill-rule="evenodd"/><path d="M6.92 8.716h7.16V8H6.92z" fill-rule="evenodd"/></symbol><symbol id="icon-clever" viewBox="0 0 87 20"><g fill-rule="evenodd"><path d="M20.476 13.846a3.623 3.623 0 0 1-1.303-.726l.407-.836c.38.308.777.532 1.188.671.41.14.872.209 1.386.209.586 0 1.04-.11 1.363-.33.323-.22.485-.532.485-.935 0-.337-.15-.592-.451-.764-.301-.173-.778-.332-1.43-.479-.624-.132-1.133-.284-1.53-.457-.396-.172-.707-.403-.934-.692-.228-.29-.341-.662-.341-1.117 0-.455.12-.856.363-1.205.242-.348.584-.62 1.028-.813.444-.195.955-.292 1.534-.292.55 0 1.066.084 1.546.253.48.169.882.407 1.204.715l-.407.836c-.359-.3-.73-.522-1.11-.666a3.449 3.449 0 0 0-1.221-.214c-.565 0-1.009.12-1.331.358a1.146 1.146 0 0 0-.485.973c0 .352.142.62.424.803.282.183.735.348 1.358.495.653.147 1.181.3 1.584.462.404.161.726.381.968.66.243.279.363.638.363 1.078 0 .455-.12.85-.363 1.188-.242.337-.588.6-1.039.786-.451.188-.98.281-1.59.281-.608 0-1.164-.08-1.666-.242zm10.698-2.596h-3.96c.036 1.298.626 1.947 1.77 1.947.639 0 1.221-.209 1.75-.627l.34.792c-.249.22-.566.394-.951.522a3.65 3.65 0 0 1-1.16.193c-.888 0-1.584-.255-2.09-.764-.507-.51-.76-1.209-.76-2.096 0-.565.112-1.067.336-1.507.224-.44.537-.781.94-1.023a2.62 2.62 0 0 1 1.376-.363c.748 0 1.336.242 1.765.726.429.484.643 1.155.643 2.013v.187zm-3.41-1.727c-.265.25-.433.605-.506 1.067h2.937c-.045-.47-.187-.827-.43-1.072-.242-.246-.568-.369-.979-.369-.418 0-.758.125-1.023.374zm5.637 4.202a2.352 2.352 0 0 1-.952-.995c-.22-.43-.33-.93-.33-1.502s.116-1.078.347-1.518c.231-.44.555-.781.974-1.023.418-.242.905-.363 1.463-.363.388 0 .762.064 1.122.193.359.128.648.302.869.522l-.341.803c-.521-.41-1.045-.616-1.573-.616-.536 0-.954.174-1.255.522-.3.349-.45.838-.45 1.469 0 .63.15 1.116.45 1.457.301.342.72.512 1.255.512.542 0 1.066-.205 1.573-.616l.34.803c-.234.22-.533.392-.896.517a3.48 3.48 0 0 1-1.139.187c-.557 0-1.043-.117-1.457-.352zm9.355-5.269V14h-1.078v-.924a1.84 1.84 0 0 1-.725.742 2.103 2.103 0 0 1-1.046.259c-1.334 0-2.001-.74-2.001-2.222V8.456h1.1v3.388c0 .455.093.79.28 1.007.187.216.471.324.852.324.455 0 .82-.147 1.095-.44.275-.293.413-.682.413-1.166V8.456h1.11zm4.917-.055l-.022 1.012a1.855 1.855 0 0 0-.649-.11c-.506 0-.885.152-1.138.456-.253.305-.38.688-.38 1.15V14h-1.1v-3.982c0-.58-.029-1.1-.087-1.562h1.034l.098 1.001c.147-.367.374-.647.682-.841a1.898 1.898 0 0 1 1.035-.292c.168 0 .344.026.527.077zm5.468 2.849h-3.96c.036 1.298.626 1.947 1.77 1.947.639 0 1.221-.209 1.75-.627l.34.792c-.249.22-.566.394-.951.522a3.65 3.65 0 0 1-1.16.193c-.888 0-1.584-.255-2.09-.764-.507-.51-.76-1.209-.76-2.096 0-.565.112-1.067.336-1.507.224-.44.537-.781.94-1.023a2.62 2.62 0 0 1 1.375-.363c.749 0 1.337.242 1.766.726.429.484.644 1.155.644 2.013v.187zm-3.41-1.727c-.265.25-.433.605-.507 1.067h2.938c-.045-.47-.187-.827-.43-1.072-.242-.246-.568-.369-.978-.369-.419 0-.76.125-1.023.374zm8.618 4.323a3.623 3.623 0 0 1-1.303-.726l.407-.836c.38.308.777.532 1.188.671.41.14.872.209 1.386.209.586 0 1.04-.11 1.364-.33.322-.22.483-.532.483-.935 0-.337-.15-.592-.45-.764-.301-.173-.778-.332-1.43-.479-.624-.132-1.133-.284-1.53-.457-.396-.172-.707-.403-.934-.692-.228-.29-.342-.662-.342-1.117 0-.455.121-.856.363-1.205.243-.348.585-.62 1.029-.813.444-.195.955-.292 1.535-.292.55 0 1.065.084 1.545.253.48.169.882.407 1.205.715l-.407.836c-.36-.3-.73-.522-1.111-.666a3.449 3.449 0 0 0-1.221-.214c-.565 0-1.009.12-1.331.358a1.146 1.146 0 0 0-.485.973c0 .352.142.62.424.803.282.183.735.348 1.358.495.653.147 1.181.3 1.584.462.404.161.726.381.968.66.243.279.364.638.364 1.078 0 .455-.121.85-.364 1.188-.242.337-.588.6-1.039.786-.451.188-.98.281-1.59.281-.608 0-1.164-.08-1.666-.242zm10.929-5.39l-2.586 5.995c-.286.653-.643 1.131-1.072 1.435-.429.305-.959.508-1.59.611l-.242-.858c.521-.117.915-.27 1.183-.457s.482-.467.644-.841l.198-.462-2.333-5.423h1.178l1.737 4.29 1.772-4.29h1.11zm5.796 2.09V14h-1.11v-3.388c0-.484-.094-.836-.281-1.056-.187-.22-.486-.33-.897-.33-.469 0-.845.147-1.127.44-.282.293-.424.686-.424 1.177V14h-1.1v-3.982c0-.58-.029-1.1-.088-1.562h1.035l.099.957c.176-.352.43-.621.764-.809.334-.187.713-.28 1.139-.28 1.327 0 1.99.74 1.99 2.222zm2.536 3.179a2.352 2.352 0 0 1-.951-.995c-.22-.43-.33-.93-.33-1.502s.115-1.078.346-1.518c.231-.44.555-.781.974-1.023.418-.242.905-.363 1.463-.363.388 0 .762.064 1.122.193.359.128.648.302.869.522l-.341.803c-.521-.41-1.045-.616-1.573-.616-.536 0-.954.174-1.254.522-.301.349-.451.838-.451 1.469 0 .63.15 1.116.45 1.457.301.342.72.512 1.255.512.542 0 1.066-.205 1.573-.616l.34.803c-.234.22-.533.392-.896.517a3.48 3.48 0 0 1-1.139.187c-.557 0-1.043-.117-1.457-.352zM5 9.656C5 6.533 7.322 4 10.65 4c2.044 0 3.266.684 4.273 1.678l-1.517 1.756c-.836-.761-1.688-1.227-2.771-1.227-1.827 0-3.143 1.522-3.143 3.387 0 1.896 1.285 3.45 3.143 3.45 1.238 0 1.997-.498 2.848-1.275L15 13.308c-1.115 1.196-2.353 1.942-4.443 1.942C7.368 15.25 5 12.78 5 9.656z"/></g></symbol><symbol id="icon-open-close" viewBox="0 0 14 12"><path d="M8.3 10.6c0 .2-.1.9.3.9h2.9c1.4 0 2.6-1.2 2.6-2.6V2.6C14 1.2 12.8 0 11.4 0H8.6c-.2 0-.3.1-.3.3 0 .3-.1.9.3.9h2.9c.8 0 1.4.6 1.4 1.4v6.3c0 .8-.6 1.4-1.4 1.4H8.9c-.3 0-.6-.1-.6.3zM0 5.7c0 .2.1.3.2.4L5 11c.1.1.2.2.4.2.3 0 .6-.3.6-.6V8h4c.3 0 .6-.3.6-.6V4c0-.3-.3-.6-.6-.6H6V.9c0-.3-.3-.6-.6-.6-.1 0-.3 0-.4.2L.2 5.3c-.1.1-.2.3-.2.4z"/></symbol><symbol id="icon-writing-assignment" viewBox="0 0 70 64"><path d="M.3 4C.3 2 1.9.3 4 .3h7.5c2 0 3.6 1.7 3.6 3.7L15 50.7c0 .7-.2 1.5-.5 2.1L8.6 63.3c-.2.4-.6.7-1.1.7-.4 0-.9-.3-1.1-.6L.7 52.8c-.3-.6-.5-1.4-.5-2.1L.3 4zm4 .3c-.7.2-1.1.7-1.1 1.4v45.2c0 .3.1.6.3.8l.1.1 2.6 4.8H9l2.8-5.1c.2-.3.2-.5.2-.8v-.9c-1.3.3-2.7.5-4 .5H5.8V5.6c0-.3-.1-.7-.4-.9-.2-.3-.5-.4-.9-.4h-.2zM23.4 61.1c-1.5 0-2.7-1.2-2.7-2.7 0-1.5 1.2-2.7 2.7-2.7h16.1c2 0 2.7 1.4 2.7 2.7 0 1.2-.7 2.7-2.7 2.7H23.4zM23.4 44.5c-1.5 0-2.7-1.2-2.7-2.7 0-1.5 1.2-2.7 2.7-2.7h43.8c2 0 2.7 1.4 2.7 2.7 0 1.2-.7 2.7-2.7 2.7H23.4zM23.4 27.3c-1.5 0-2.7-1.2-2.7-2.7 0-1.5 1.2-2.7 2.7-2.7h43.8c2 0 2.7 1.4 2.7 2.7s-.7 2.7-2.7 2.7H23.4zM23.4 10.8c-1.4 0-2.5-1.1-2.7-2.4v-.3c0-1.5 1.2-2.7 2.7-2.7h43.8c2 0 2.7 1.4 2.7 2.7s-.7 2.7-2.7 2.7H23.4z"/></symbol><symbol id="icon-x" viewBox="0 0 25 25"><path d="M1.067 6.015c-1.423-1.422-1.423-3.526 0-4.948 1.422-1.423 3.526-1.423 4.948 0l6.371 6.37 6.371-6.37c1.422-1.423 3.783-1.423 5.176 0 1.423 1.422 1.423 3.782 0 5.176l-6.37 6.37 6.37 6.372c1.423 1.422 1.423 3.526 0 4.948-1.422 1.423-3.526 1.423-4.948 0l-6.371-6.37-6.371 6.37c-1.422 1.423-3.783 1.423-5.176 0-1.423-1.422-1.423-3.782 0-5.176l6.37-6.143-6.37-6.599z"/></symbol><symbol id="icon-lock" viewBox="0 0 18 24"><path d="M15.17 10.292V6.147C15.17 2.757 12.402 0 9 0 5.597 0 2.83 2.758 2.83 6.147v4.145h-.882A1.944 1.944 0 0 0 0 12.232v9.828C0 23.132.872 24 1.948 24h14.105A1.943 1.943 0 0 0 18 22.06v-9.828c0-1.072-.872-1.94-1.947-1.94h-.883zm-5.574 7.463v2.305a.595.595 0 0 1-1.192 0v-2.305a1.744 1.744 0 0 1-1.156-1.639A1.75 1.75 0 0 1 9 14.371a1.75 1.75 0 0 1 1.752 1.745c0 .756-.483 1.397-1.156 1.64zm3.238-7.463H5.166V6.147c0-2.106 1.72-3.82 3.834-3.82s3.834 1.714 3.834 3.82v4.145z" fill-rule="evenodd"/></symbol><symbol id="icon-key" viewBox="0 0 64 71"><path d="M61.3 2.8c2.9 2.9 3.4 7.3 1.7 10.7 1.4 2.6 1.5 5.7.1 8.3-2.2 4.4-7.6 6.2-12 4-1.8-.9-3.2-2.5-4.1-4.2L45.6 23l.4 2c.4 1.8-1.1 3.6-2.9 3.6h-2.9c.5 1.1.3 2.6-.6 3.4s-2.3 1.1-3.4.5L14.9 53.9l.7.7c1.2-1.2 2.3-2.4 3.5-3.5 2.4 2.5 5 5.1 7.1 7.2l-3.5 3.5c-.7-.7-1.4-1.4-2.1-2.2l-.7.7c.7.7 1.4 1.4 2.1 2.2-1.4 1.5-3 3-4.2 4.3-.7-.7-1.4-1.4-2.1-2.2l-.7.7c.7.7 1.4 1.4 2.1 2.2L13.6 71c-2.4-2.5-5-5.1-7.1-7.2 1.2-1.2 2.3-2.4 3.5-3.5l-.7-.7-4.2 4.2C4 64.9 2 64.9.8 63.8c-1.1-1.1-1.1-3.2 0-4.3l31-31.3c-.6-1.1-.3-2.6.5-3.4.9-.9 2.3-1.2 3.4-.6v-2.9c0-1.8 1.8-3.3 3.6-2.9l1.9.4 1.3-1.3c-1.6-.9-3-2.2-3.9-3.9-2.2-4.5-.4-9.9 3.9-12.2 2.5-1.3 5.5-1.3 8 0 3.5-2.3 8-1.5 10.8 1.4zM57.1 7c-1.2-1.2-3-1.2-4.2 0-.5.5-1.1.8-1.8.8-.9.1-1.8-.2-2.4-.8-.9-.8-2.2-1-3.3-.5-1.5.8-2.1 2.5-1.3 4.1.8 1.5 2.5 2.1 4 1.4 1.1-.7 2.7-.5 3.7.5 1 .9 1.3 2.6.6 3.8-.8 1.5-.2 3.3 1.3 4.1s3.3.2 4-1.3c.5-1.1.3-2.3-.4-3.2-.9-1-1.1-1.9-1-3 .1-.7.5-1.1.9-1.6 1.1-1.3 1-3-.1-4.3z"/></symbol><symbol id="icon-seemore" viewBox="0 0 30 30"><ellipse cx="8.1" cy="15" rx="2.3" ry="2.2"/><ellipse cx="15" cy="15" rx="2.3" ry="2.2"/><ellipse cx="21.5" cy="15" rx="2.3" ry="2.2"/><path d="M28.3 0H1.9C1.1 0 .7.1.4.4.1.7 0 1.1 0 1.9v26.2C0 29.7.4 30 1.8 30H28.6c.1 0 .3 0 .4-.1.8-.2 1-.6 1-1.7V1.9C30.1.4 29.7 0 28.3 0zm-1.5 27H3.1V3.1H27V27h-.2z"/></symbol><symbol id="icon-download" viewBox="0 0 21 21"><path d="M17.719 12.467H21v5.25a1.968 1.968 0 0 1-1.969 1.97H1.97A1.968 1.968 0 0 1 0 17.716v-5.25h3.281v3.938H17.72v-3.938zM5.647 9.17h.001a1.024 1.024 0 0 1-.082-.332.967.967 0 0 1 .046-.352A1.037 1.037 0 0 1 6 7.962c.08-.057.166-.104.257-.14a1.642 1.642 0 0 1 .597-.115h1.462l-.167-5.163a1.148 1.148 0 0 1 .347-.865 1.307 1.307 0 0 1 .906-.365h2.18a1.32 1.32 0 0 1 .907.365 1.148 1.148 0 0 1 .347.865l-.168 5.165h1.529c.12 0 .24.015.354.043.114.027.225.07.328.127a1.058 1.058 0 0 1 .45.453.985.985 0 0 1 .076.69 1.065 1.065 0 0 1-.06.166 1.01 1.01 0 0 1-.2.302l-3.676 4.064a1.05 1.05 0 0 1-.194.17 1.432 1.432 0 0 1-1.326.126 1.29 1.29 0 0 1-.236-.126 1.073 1.073 0 0 1-.197-.17L5.845 9.5a1.183 1.183 0 0 1-.207-.318l.009-.013z" fill-rule="evenodd"/></symbol><symbol id="icon-submitting" viewBox="0 0 25 25"><path d="M0 1.875v1.406h22.5V1.875H0zm0 3.867v1.406h24.32V5.742H0zM0 9.61v1.407h15.117V9.609H0zm0 5.625v1.407h24.32v-1.407H0zm0 3.868v1.406h23.125v-1.406H0zm0 3.867v1.406h13.75V22.97H0z" fill-opacity=".5"/><path d="M13.15 23.552l.867-5.111 6.325-12.527 1.15-.069.71-1.69 1.909.877-.702 1.715.338.924-6.827 12.878-3.178 3.069z"/><path d="M24.32 5.78a.906.906 0 0 0-.405-1.249l-1.181-.602c-.237-.12-.444-.151-.711-.064-.268.087-.417.234-.538.47l-.481.945c-.178.058-.297-.002-.475.056a.852.852 0 0 0-.537.47l-.662 1.3-.945-.482a.453.453 0 0 0-.624.203l-3.01 5.906a.453.453 0 0 0 .203.624c.058.179.236.12.325.092.09-.03.268-.087.328-.205l2.74-5.523.472.24-5.477 10.75c-.06.118-.031.208-.091.326l-.625 4.146c-.062.414.143.742.526 1.012.236.12.444.151.711.064.178-.058.268-.087.328-.205l2.987-2.942c.089-.029.15-.147.21-.265l6.62-12.995a.997.997 0 0 0-.169-1.127l.482-.945zm-2.008-1.024l1.182.602-.482.945-1.181-.602.481-.945zm-8.739 18.612l.591-3.642 2.127 1.083-2.718 2.559zm3.228-3.415L14.44 18.75l4.695-9.214 2.362 1.204-4.695 9.214zm5.116-10.041l-2.362-1.204 1.264-2.48 2.363 1.203-1.265 2.48z"/></symbol><symbol id="icon-leaderboard" viewBox="0 0 25 25"><path d="M15.038 5.296c0 1.17-1.065 2.236-2.236 2.236-1.17 0-2.236-1.064-2.236-2.236-.001-1.172 1.066-2.239 2.236-2.239 1.172 0 2.236 1.067 2.236 2.239m-4.7 3.124a2.01 2.01 0 0 0-2.014 2.014c0 .125.01.25.03.37l1.654 8.387a1.994 1.994 0 0 0 1.953 1.527h1.683c.943 0 1.774-.683 1.953-1.527l1.65-8.387a2.019 2.019 0 0 0-1.99-2.385h-1.63a.103.103 0 0 1 0 .027l-.504 2.447c.367.143.652.514.652.912 0 .509-.464.972-.973.972-.509 0-.972-.463-.972-.972 0-.398.284-.77.651-.912l-.503-2.447a.103.103 0 0 1 0-.026h-1.64zm1.91 0l.495 2.422c.02-.002.04-.01.06-.01.021 0 .04.008.061.01l.495-2.422h-1.111zm11.179 2.233c-.001 1.088-.99 2.077-2.079 2.077-1.088 0-2.08-.99-2.08-2.077 0-1.09.992-2.08 2.08-2.08 1.09 0 2.08.991 2.079 2.08m-3.861 2.905a1.874 1.874 0 0 0-1.843 2.22l.706 3.526c.172.826.938 1.414 1.814 1.414h2.205c.877 0 1.628-.588 1.814-1.414l.707-3.528a1.874 1.874 0 0 0-1.843-2.219h-.982l-.477 2.292c.367.142.65.513.65.911 0 .509-.462.972-.971.972-.51 0-.973-.463-.973-.972 0-.398.285-.769.652-.911l-.478-2.292-.98.001zm1.259 0l.46 2.239c.02-.001.04-.01.06-.01.021 0 .04.009.061.01l.46-2.239h-1.041zM1.712 8.978c.001 1.089.99 2.078 2.079 2.078 1.088 0 2.08-.99 2.08-2.078 0-1.09-.992-2.08-2.08-2.08-1.09 0-2.08.992-2.079 2.08m.163 2.906A1.874 1.874 0 0 0 .03 14.103l.846 5.202c.145.775.937 1.415 1.814 1.415h2.205c.876 0 1.639-.588 1.814-1.415l.845-5.202a1.874 1.874 0 0 0-1.843-2.219h-1.12l-.477 2.292c.366.142.65.513.65.911 0 .509-.463.972-.972.972-.509 0-.972-.463-.972-.972 0-.398.284-.77.651-.911l-.477-2.292h-1.12zm1.397 0l.46 2.239c.021-.002.04-.01.061-.01.02 0 .04.008.061.01l.46-2.239H3.272z" fill-rule="evenodd"/></symbol><symbol id="icon-practice" viewBox="0 0 72 71"><path d="M51.07 23l-1.87-.27 2.89-2.89 1.87.27zM35.46 53.55a18.38 18.38 0 0 1-10.26-3.14 1 1 0 0 0-.89-.1l-6.66 2.53 2.53-6.65a1 1 0 0 0-.1-.89A18.48 18.48 0 0 1 45 19.2l-3.34 3.34a14 14 0 1 0 6.28 6.27l3.34-3.34a18.5 18.5 0 0 1-15.82 28.08zm11-26l-1.87-.27 2.89-2.89 1.87.27zM46.09 23l-2.89 2.89-.26-1.89 2.89-2.89zM41 35a5.53 5.53 0 1 1-2.37-4.53L35.2 33.9a1.13 1.13 0 1 0 1.38 1.37L40 31.89A5.53 5.53 0 0 1 41 35zm0-10.6l.46 3.23L40 29.12a7.5 7.5 0 1 0 1.38 1.38l1.48-1.5 3.24.46A12 12 0 1 1 41 24.4zm9.42-7.86l.27 1.87-2.87 2.89-.27-1.88zm6.51 2.52a1 1 0 0 0-.79-.66l-3.54-.5-.51-3.56a1 1 0 0 0-1.66-.55l-4 4A20.42 20.42 0 0 0 18.19 46l-3.13 8.22A1 1 0 0 0 16 55.5a.94.94 0 0 0 .34-.06l8.22-3.13a20.43 20.43 0 0 0 28.15-28.24l4-4a1 1 0 0 0 .24-1z"/></symbol><symbol id="icon-preview" viewBox="0 5 25 15"><path d="M12.5 5C18.056 5 25 12.5 25 12.5S18.056 20 12.5 20 0 12.5 0 12.5C3.264 9.266 7.624 5.2 12.5 5zm0 3.482c-2.302 0-4.167 1.798-4.167 4.018 0 2.22 1.865 4.018 4.167 4.018 2.302 0 4.167-1.798 4.167-4.018 0-2.22-1.865-4.018-4.167-4.018zm0 6.027c-1.15 0-2.083-.9-2.083-2.009s.933-2.009 2.083-2.009c1.15 0 2.083.9 2.083 2.009s-.933 2.009-2.083 2.009z"/></symbol><symbol id="icon-newspaper" viewBox="0 0 25 25"><path d="M20.9.5v21.6c0 1.3 1.1 2.4 2.4 2.4H2.4c-1.3 0-2.4-1.1-2.4-2.4V.5h20.9zm-3.3 3.4h-6.8v4.8h6.8V3.9zm-9.5 0H3.4c-.4 0-.7.3-.7.7 0 .4.3.7.7.7h4.7c.4 0 .7-.3.7-.7 0-.4-.3-.7-.7-.7zM25 8.7v13.4c0 1-.7 1.7-1.7 1.7-.9 0-1.7-.8-1.7-1.7V8.7H25zM8.1 7.4H3.4c-.4 0-.7.3-.7.6 0 .4.3.7.7.7h4.7c.4 0 .7-.3.7-.7 0-.3-.3-.6-.7-.6zm9.5 4.1H3.4c-.4 0-.7.3-.7.7 0 .4.3.7.7.7h14.2c.4 0 .7-.3.7-.7-.1-.4-.4-.7-.7-.7zm0 3.7H3.4c-.4 0-.7.3-.7.7 0 .4.3.7.7.7h14.2c.4 0 .7-.3.7-.7-.1-.4-.4-.7-.7-.7zm0 3.8H3.4c-.4 0-.7.3-.7.7 0 .4.3.7.7.7h14.2c.4 0 .7-.3.7-.7-.1-.4-.4-.7-.7-.7z"/></symbol><symbol id="icon-diagnostic" viewBox="0 0 72 71"><path d="M46.46 56a4.17 4.17 0 0 1-2.89-1.14 3.86 3.86 0 0 1-.18-5.46l.18-.18a2.37 2.37 0 0 0 .45-.59H26.41l-.1-.07-.09-.09-.07-.1L26 48a.82.82 0 0 1 0-.13.69.69 0 0 1 0-.08v-.11a3.9 3.9 0 0 1 1.2-2.44 2.22 2.22 0 0 0 0-3.27 2.63 2.63 0 0 0-3.59 0 2.21 2.21 0 0 0-.15 3.12l.15.15a3.91 3.91 0 0 1 1.2 2.4.81.81 0 0 1-.6 1H14.8a.81.81 0 0 1-.8-.81v-11a.54.54 0 0 1 0-.07v-.36l.05-.06h.75a3.8 3.8 0 0 1 2.38 1.22 2.17 2.17 0 0 0 1.6.73 2.14 2.14 0 0 0 1.59-.73 2.77 2.77 0 0 0 0-3.67 2.11 2.11 0 0 0-3.19 0 3.78 3.78 0 0 1-2.48 1.24h-.48l-.08-.13-.07-.07-.07-.07v-.1a.84.84 0 0 1 0-.12V23.19a.81.81 0 0 1 .8-.81h8.11a2.36 2.36 0 0 0-.45-.6 3.85 3.85 0 0 1-.2-5.44l.2-.2a4.24 4.24 0 0 1 5.79 0 3.85 3.85 0 0 1 .2 5.44l-.2.2a2.37 2.37 0 0 0-.46.61H45.57l.09.08a.8.8 0 0 1 .16.68 3.92 3.92 0 0 1-1.19 2.41 2.31 2.31 0 0 0 0 3.39 2.63 2.63 0 0 0 3.59 0 2.21 2.21 0 0 0 .15-3.12l-.15-.15A3.9 3.9 0 0 1 47 23.25a1.28 1.28 0 0 0 0-.25.81.81 0 0 1 .8-.81H57a.81.81 0 0 1 .8.81v11a.76.76 0 0 1 0 .11v.2l-.08.08-.1.07-.12.06H57a3.79 3.79 0 0 1-2.4-1.23 2.17 2.17 0 0 0-1.6-.73 2.14 2.14 0 0 0-1.59.73 2.77 2.77 0 0 0 0 3.67 2.1 2.1 0 0 0 3.19 0 3.78 3.78 0 0 1 2.4-1.09.81.81 0 0 1 .8.75.79.79 0 0 1 0 .14.81.81 0 0 1 0 .12v10.88a.81.81 0 0 1-.8.81h-8.09a2.38 2.38 0 0 0 .45.59 3.85 3.85 0 0 1 .2 5.44l-.2.2a4.15 4.15 0 0 1-2.9 1.2zm-.6-8a3.92 3.92 0 0 1-1.19 2.41 2.21 2.21 0 0 0-.15 3.12l.15.15a2.63 2.63 0 0 0 3.59 0 2.21 2.21 0 0 0 .15-3.12l-.15-.15a3.92 3.92 0 0 1-1.2-2.41.79.79 0 0 1 0-.17.82.82 0 0 1 .19-.52l.08-.08.09-.06h8.72v-9.2a2.3 2.3 0 0 0-.57.45 3.69 3.69 0 0 1-5.55 0 4.43 4.43 0 0 1 0-5.88 3.7 3.7 0 0 1 5.55 0 2.34 2.34 0 0 0 .57.45v-9h-7.33a2.35 2.35 0 0 0 .55.8 3.85 3.85 0 0 1 .2 5.44l-.2.2a4.24 4.24 0 0 1-5.79 0 3.85 3.85 0 0 1-.2-5.44l.2-.2A2.38 2.38 0 0 0 44 24h-7.29v8.79a2.31 2.31 0 0 0 .57-.45 3.7 3.7 0 0 1 5.55 0 4.43 4.43 0 0 1 0 5.88 3.72 3.72 0 0 1-2.77 1.25 3.77 3.77 0 0 1-2.79-1.25 2.32 2.32 0 0 0-.57-.45V47h8.43a.79.79 0 0 1 .38.14l.08.07v.14a.81.81 0 0 1 .1.39.78.78 0 0 1 .13.2zm-10.75-1V36.72a.8.8 0 0 1 .75-.85H36a3.78 3.78 0 0 1 2.48 1.23 2.17 2.17 0 0 0 1.6.73 2.14 2.14 0 0 0 1.59-.73 2.77 2.77 0 0 0 0-3.67 2.11 2.11 0 0 0-3.19 0A3.78 3.78 0 0 1 36 34.67h-.06a.81.81 0 0 1-.81-.75.6.6 0 0 1 0-.09V24h-8.39a.81.81 0 0 1-.8-.81 3.9 3.9 0 0 1 1.21-2.52 2.21 2.21 0 0 0 .15-3.12l-.15-.15a2.63 2.63 0 0 0-3.59 0 2.21 2.21 0 0 0-.15 3.12l.15.15a3.91 3.91 0 0 1 1.2 2.33.79.79 0 0 1 0 .17.81.81 0 0 1-.76.83h-8.39v9a2.31 2.31 0 0 0 .57-.45 3.7 3.7 0 0 1 5.55 0 4.43 4.43 0 0 1 0 5.88A3.72 3.72 0 0 1 19 39.67a3.77 3.77 0 0 1-2.78-1.25 2.31 2.31 0 0 0-.57-.45v9H23a2.34 2.34 0 0 0-.45-.6 3.85 3.85 0 0 1-.2-5.44l.2-.2a4.24 4.24 0 0 1 5.79 0 3.85 3.85 0 0 1 .19 5.44l-.19.19a2.37 2.37 0 0 0-.45.6z"/></symbol><symbol id="icon-writingcycle" viewBox="0 0 50 53"><g fill-rule="evenodd"><path d="M15.56 13.56h-3A1.48 1.48 0 0 0 11.1 15v18.68c-.001.293.07.582.21.84l2.31 4.2a.48.48 0 0 0 .85 0l2.32-4.19a1.74 1.74 0 0 0 .21-.84V15a1.48 1.48 0 0 0-1.44-1.44zm.24 20.15a.52.52 0 0 1-.06.25l-1.12 2h-1.15l-1-1.91a.51.51 0 0 1-.11-.31V15.68a.54.54 0 0 1 .42-.54h.1a.51.51 0 0 1 .51.51v17.88h.88a7 7 0 0 0 1.61-.19l-.08.37zM20.35 37.85a1.07 1.07 0 1 1 0-2.14h6.45a1.07 1.07 0 1 1 0 2.14h-6.45zm0-6.62a1.07 1.07 0 1 1 0-2.14h17.53a1.07 1.07 0 1 1 0 2.14H20.35zm0-6.88a1.07 1.07 0 1 1 0-2.14h17.53a1.07 1.07 0 1 1 0 2.14H20.35zm0-6.62a1.07 1.07 0 0 1-1.06-1v-.1a1.07 1.07 0 0 1 1.06-1.07h17.53a1.07 1.07 0 1 1 0 2.14l-17.53.03zM38.519 6.632C30.626.822 17.743 1.195 10.49 7.75h-.01a1 1 0 0 1-1.44.09c-.3-.3-.45-.89.2-1.48 7.89-7.119 21.68-7.552 30.286-1.299L39.41 3.58a.933.933 0 1 1 1.86-.15l.279 3.55a1 1 0 0 1 .02.263l.001.007a.936.936 0 0 1 .005.075.933.933 0 0 1-.855 1.005l-3.81.3a.933.933 0 0 1-.15-1.86l1.759-.138zM43.298 43.532a.933.933 0 0 1-1.038-.852L42 38.86a.933.933 0 1 1 1.86-.15l.117 1.718c5.75-7.925 5.367-20.801-1.147-28.068v-.01a1 1 0 0 1-.09-1.45c.3-.31.88-.45 1.47.2 7.097 7.914 7.532 21.732 1.304 30.377L47 41.36a.933.933 0 0 1 .15 1.86l-3.48.274a1 1 0 0 1-.372.038zM8.312 45.736a.933.933 0 0 1 .848-1.066l3.81-.3a.933.933 0 0 1 .15 1.86l-1.759.138c7.893 5.81 20.776 5.437 28.029-1.118h.01a1 1 0 0 1 1.44-.09c.3.3.45.89-.2 1.48-7.886 7.116-21.668 7.552-30.276 1.306l.116 1.474a.933.933 0 0 1-1.86.15l-.274-3.49a1 1 0 0 1-.034-.344zM6.043 13.234C.273 21.157.65 34.054 7.17 41.33v.01a1 1 0 0 1 .09 1.45c-.3.31-.88.45-1.47-.2-7.1-7.917-7.532-21.745-1.295-30.39L3 12.33a.933.933 0 1 1-.15-1.86l3.48-.274a1 1 0 0 1 .345-.04A.933.933 0 0 1 7.74 11l.3 3.82a.933.933 0 0 1-1.86.15l-.137-1.736z"/></g></symbol><symbol id="icon-class" viewBox="0 0 21 21"><path d="M2.66 20.576v-7.1l.002-.063v-2.535l-.847 1.65c-.25.487-.834.689-1.304.447-.47-.24-.648-.833-.398-1.32l1.66-3.236c.03-.056.062-.109.099-.156.149-.432.56-.744 1.044-.744h2.95l-1.487 2.896c-.485.949-.16 2.151.823 2.657a1.92 1.92 0 0 0 1.85-.053l.037-.024v7.581H5.153v-6.095H4.65v6.095H2.66zm5.32 0v-7.743l.001-.072V9.968l-.933 1.817c-.276.54-.92.76-1.439.495-.518-.266-.713-.92-.438-1.457l1.83-3.566c.032-.062.068-.119.109-.172.164-.477.618-.82 1.15-.82h4.385c.534 0 .987.343 1.15.82.04.053.077.11.11.172l1.829 3.566c.277.538.08 1.191-.438 1.457-.519.265-1.162.044-1.438-.495l-.933-1.816v2.751a.918.918 0 0 1-.005.106v7.75h-2.207v-6.638h-.527v6.638H7.98zm5.831 0V12.99l.043.03a1.92 1.92 0 0 0 1.85.052c.984-.506 1.308-1.708.822-2.657L15.04 7.52h2.949c.484 0 .895.312 1.044.744.036.049.07.1.099.156l1.66 3.235c.25.488.072 1.08-.398 1.321-.47.242-1.054.04-1.305-.448l-.846-1.649v2.497c0 .032-.001.064-.004.097v7.104h-1.99V14.48h-.502v6.095H13.81zM10.491 1a2.182 2.182 0 0 0 .001 4.363A2.182 2.182 0 0 0 10.494 1h-.004zm-7.53 3.722a1.978 1.978 0 1 1 3.957 0 1.978 1.978 0 0 1-3.957 0zm11.096 0a1.978 1.978 0 1 1 3.957 0 1.978 1.978 0 0 1-3.957 0z"/></symbol><symbol id="icon-document" viewBox="0 0 21 21"><path d="M13.41.219H4.742a2.703 2.703 0 0 0-2.699 2.699V18.08a2.703 2.703 0 0 0 2.7 2.699h11.051c1.488 0 2.7-1.212 2.7-2.7V5.899L13.41.218zm.356 2.327l2.644 2.956h-2.644V2.546zm2.026 16.949H4.742A1.414 1.414 0 0 1 3.33 18.08V2.918c0-.779.634-1.414 1.412-1.414h7.739v5.282h4.725V18.08c0 .78-.634 1.414-1.414 1.414z"/><path d="M6.355 10.072V8.785h7.824v1.287H6.355zm0 2.964V11.75h7.824v1.286H6.355zm0 2.965v-1.287h7.824v1.287H6.355z"/></symbol><symbol id="icon-sort-arrow" viewBox="0 0 8 6"><path d="M0 6l4-6 4 6H0z"/></symbol><symbol id="icon-gear" viewBox="0 0 21 21"><path d="M2.757 12.385a8.05 8.05 0 0 0 1.039 2.417l-1.308 1.314c-.588.587-.582.937-.065 1.454L3.7 18.846c.522.523.878.512 1.454-.064l1.373-1.379a8.049 8.049 0 0 0 2.09.84v1.68c0 .834.252 1.077.98 1.077h1.809c.737 0 .98-.26.98-1.077v-1.68a8.049 8.049 0 0 0 2.089-.84l1.405 1.41c.582.577.931.588 1.453.07l1.282-1.28c.51-.511.521-.862-.07-1.453l-1.347-1.347a7.91 7.91 0 0 0 1.045-2.417h1.777c.738-.001.981-.26.981-1.078V9.692c0-.798-.21-1.077-.98-1.077h-1.777a8.049 8.049 0 0 0-.84-2.089l1.282-1.276c.565-.565.614-.916.07-1.454l-1.282-1.282c-.511-.512-.878-.506-1.454.07l-1.217 1.212a8.05 8.05 0 0 0-2.417-1.04v-1.68c0-.797-.21-1.076-.98-1.076h-1.81c-.727 0-.98.263-.98 1.077v1.68a8.05 8.05 0 0 0-2.417 1.039L4.981 2.584c-.576-.576-.943-.582-1.454-.07L2.245 3.796c-.544.538-.495.889.07 1.454l1.276 1.276a8.202 8.202 0 0 0-.834 2.09H.98c-.728 0-.98.263-.98 1.076v1.616c0 .834.252 1.077.98 1.077h1.777zM7.419 10.5a3.08 3.08 0 1 1 6.159 0 3.08 3.08 0 0 1-6.159 0z" fill-rule="evenodd"/></symbol><symbol id="icon-speedometer" viewBox="0 0 25 25"><g fill-rule="evenodd"><path d="M10.968 9.383a9.071 9.071 0 0 0-4.242 1.047l-1.7-1.7a12.494 12.494 0 0 1 6.852-2.45v3.148c-.3-.03-.603-.045-.91-.045zm9.972.141l-3.05 3.048a9.07 9.07 0 0 1 2.144 5.042H25a12.527 12.527 0 0 0-4.06-8.09zM0 17.614h1.902a9.101 9.101 0 0 1 3.738-6.51l-1.58-1.58A12.53 12.53 0 0 0 0 17.614z"/><path d="M19.951 8.752l-8.388 9.433a1.539 1.539 0 0 1-.18.19 1.492 1.492 0 0 1-2.11-2.104 1.54 1.54 0 0 1 .192-.183l6.122-5.446a9.03 9.03 0 0 0-2.465-1.002V6.28c2.015.098 3.903.671 5.558 1.609l2.596-2.309a.56.56 0 0 1 .791.791L19.971 8.73h.002l-.022.023z"/><path d="M10.932 16.718a.763.763 0 0 0-1.077 1.077.763.763 0 0 0 1.077-1.077z"/></g></symbol><symbol id="icon-quiz" viewBox="0 0 72 71"><path d="M52.86 17.12l.91.08-.91 8.53zm-1.1-.12v38.75h-31.9V16h31.9zM18.3 54.15l.46-3.07v3.11zm3.7-39.6l4.13.35H22zM52.86 16v-1.11H39.42L21 13.35l-.13 1.54h-2.11v22.8l-1.65 17.47 1.65.14v1.56h18.71L51.1 58l.09-1.14h1.67V39.11L55 16.19z"/><path d="M26.58 21.43l-2.28 2.75-.69-1a.55.55 0 0 0-.92.61l1.1 1.66a.56.56 0 0 0 .43.24.53.53 0 0 0 .42-.2l2.75-3.32a.56.56 0 0 0-.07-.78.55.55 0 0 0-.77.07zM26.58 27.52l-2.28 2.75-.69-1a.55.55 0 0 0-.92.61l1.1 1.66a.56.56 0 0 0 .43.24.53.53 0 0 0 .42-.2l2.75-3.32a.56.56 0 0 0-.07-.78.54.54 0 0 0-.35-.13.55.55 0 0 0-.39.17zM26.58 39.7l-2.28 2.75-.69-1a.55.55 0 0 0-.76-.15.56.56 0 0 0-.15.77l1.1 1.66a.56.56 0 0 0 .43.24.53.53 0 0 0 .42-.2l2.75-3.32a.56.56 0 0 0-.07-.78.54.54 0 0 0-.33-.17.55.55 0 0 0-.42.2zM26.58 45.79l-2.28 2.75-.69-1a.55.55 0 0 0-.92.61l1.1 1.66a.56.56 0 0 0 .43.25.53.53 0 0 0 .42-.2l2.75-3.32a.56.56 0 0 0-.07-.78.55.55 0 0 0-.77.07zM23 34.16a.58.58 0 0 0 .07.78l1.07 1-1.07 1a.59.59 0 0 0-.07.78.49.49 0 0 0 .4.2.51.51 0 0 0 .33-.13l1.22-1.09 1.22 1.09a.51.51 0 0 0 .33.13.5.5 0 0 0 .4-.2.58.58 0 0 0-.07-.78l-1.07-1 1.07-1a.59.59 0 0 0 .07-.78.49.49 0 0 0-.68-.11L25 35.14l-1.22-1.09a.49.49 0 0 0-.38-.05.5.5 0 0 0-.4.16zM30.25 49.24a1 1 0 1 1 0-2h6.22a1 1 0 1 1 0 2zM30.25 42.87a1 1 0 1 1 0-2h16.89a1 1 0 1 1 0 2zM30.25 36.25a1 1 0 1 1 0-2h16.89a1 1 0 1 1 0 2zM30.25 29.89a1 1 0 0 1-1-.93v-.09a1 1 0 0 1 1-1h16.89a1 1 0 1 1 0 2zM30.25 23.41a1 1 0 0 1-1-.93v-.09a1 1 0 0 1 1-1h16.89a1 1 0 1 1 0 2z"/></symbol><symbol id="icon-revising" viewBox="0 0 25 25"><path d="M23.056 12.001h.58C23.637 5.893 18.64.926 12.5.926a11.169 11.169 0 0 0-10.214 6.66L0 7.128 1.652 12 5.07 8.142l-2.206-.44C4.93 3.184 9.831.71 14.668 1.74c4.837 1.03 8.32 5.291 8.388 10.261zM12.5 23.49a10.593 10.593 0 0 0 9.637-6.215l-2.206-.441 3.417-3.858L25 17.845l-2.286-.457a11.169 11.169 0 0 1-10.214 6.66c-6.14 0-11.137-4.968-11.137-11.073h.581c.03 5.833 4.754 10.537 10.556 10.514z"/><path d="M5.833 13.348v1.068h13.555v-1.068H5.833zm0 2.938v1.068h12.652v-1.068H5.833zm0 2.938v1.068H11.4v-1.068H5.833z" fill-opacity=".5"/><path d="M10.958 19.656l.648-3.88 4.769-9.505.87-.051.533-1.282 1.445.668-.529 1.301.257.702-5.147 9.77-2.4 2.328z"/><path d="M19.382 6.173a.691.691 0 0 0-.308-.948l-.894-.458a.642.642 0 0 0-.537-.05.642.642 0 0 0-.406.356l-.363.717c-.135.044-.224-.002-.359.042a.642.642 0 0 0-.406.356l-.499.986-.715-.366a.341.341 0 0 0-.472.153l-2.268 4.482a.346.346 0 0 0 .154.474c.044.135.179.091.246.07.067-.022.202-.066.248-.156l2.065-4.19.357.183-4.129 8.156c-.045.09-.023.158-.068.247l-.468 3.148c-.046.315.11.564.4.769a.642.642 0 0 0 .537.05c.135-.044.202-.066.247-.156L14 17.808c.067-.022.112-.112.158-.201l4.99-9.86a.76.76 0 0 0-.129-.856l.363-.718zm-1.52-.779l.895.459-.363.717-.894-.459.363-.717zm-6.585 14.122l.442-2.765 1.61.826-2.052 1.94zm2.436-2.589l-1.787-.917 3.539-6.99 1.788.916-3.54 6.991zM17.57 9.31l-1.788-.917.953-1.883 1.788.917-.953 1.883z"/></symbol><symbol id="icon-rating" viewBox="0 0 25 25"><path d="M4.961 19.785h-1.59C1.526 19.785 0 18.258 0 16.415V6.079c0-1.845 1.527-3.371 3.371-3.371H13.77c.318 0 .573.255.573.573a.571.571 0 0 1-.573.573H3.37c-1.21 0-2.195.986-2.195 2.195v10.336c0 1.21.986 2.195 2.195 2.195h2.194c.318 0 .574.255.574.573v3.56l4.165-4.006a.604.604 0 0 1 .415-.159h9.603c.891 0 1.718-.573 2.067-1.4a.586.586 0 0 1 .764-.318.586.586 0 0 1 .318.764 3.425 3.425 0 0 1-3.149 2.13h-9.35l-4.992 4.77a.604.604 0 0 1-.415.16c-.062 0-.159 0-.221-.032-.257-.035-.383-.257-.383-.48v-4.357zm20.036-11.99v.159l-.923 5.724c-.19 1.463-1.24 2.323-2.8 2.323h-6.806a2.36 2.36 0 0 1-1.652-.668 1.13 1.13 0 0 1-.733.255H9.857a1.14 1.14 0 0 1-1.144-1.144V7.319a1.14 1.14 0 0 1 1.144-1.144h2.226c.35 0 .636.159.858.381h.032c.096-.032 2.513-.732 2.513-2.766V1.024c0-.255.159-.477.413-.54.064-.032 1.463-.446 2.482.318.636.477.985 1.272.985 2.385v2.29h3.212c1.368-.067 2.419.983 2.419 2.318zm-12.88-.509H9.922v7.124h2.195V7.286zm11.733.509c0-.7-.54-1.24-1.24-1.24h-3.785a.572.572 0 0 1-.573-.574V3.12c0-.699-.16-1.209-.509-1.431-.318-.222-.732-.255-1.017-.222v2.289c0 2.925-3.212 3.848-3.339 3.88h-.032v5.946c0 .667.54 1.24 1.241 1.24h6.806c1.494 0 1.622-.985 1.653-1.303v-.032l.795-5.691z" fill-rule="evenodd"/></symbol><symbol id="icon-edit-writing" viewBox="0 0 66 66"><path d="M23.13 40.13l2.74 2.74-3.07.35zM51 9.68L56.32 15l-26 26L25 35.67zM2 7.52a2.25 2.25 0 0 0-2 2.25v54A2.25 2.25 0 0 0 2.25 66h54a2.25 2.25 0 0 0 2.25-2.25V27a2.25 2.25 0 1 0-4.5 0v34.5H4.5V12H39a2.25 2.25 0 1 0 0-4.5H2zm53.27-2.08l5.3 5.32-1.06 1.06-5.3-5.3zm0-5.44H55a2.26 2.26 0 0 0-1.34.66L19.73 34.62a2.27 2.27 0 0 0-.65 1.38L18 45.49A2.25 2.25 0 0 0 20.51 48L30 46.92a2.26 2.26 0 0 0 1.36-.63l33.98-33.94a2.25 2.25 0 0 0 0-3.19L56.86.68A2.25 2.25 0 0 0 55.24 0z"/></symbol><symbol id="icon-edit" viewBox="0 0 30 30"><path d="M27.3 7.9l-5.2-5.2L24.4.4c.5-.5 1.2-.5 1.7 0L29.7 4c.5.5.5 1.2 0 1.7l-2.4 2.2zm-1.4 1.5L8.6 26.6l-5.2-5.2L20.6 4.1l5.3 5.3zM0 30l1.9-7L7 28.1 0 30z"/><path d="M-753.8-401V715h1024V-401h-1024z" fill="none"/><path d="M-775.9-385.9v1116h1024v-1116h-1024z" fill="none"/></symbol><symbol id="icon-performance" viewBox="0 0 30 30"><path d="M22.1 24.6V8.4l-4.3-1.3v17.5zM24.2 7.7v16.9h4.3V5l-2.2-1.5zM5 15.6v9h4.3V12.3l-3.5 4.2zM11.4 24.6h4.3V6.5l-1.2-.3-3.1 3.6z"/><path d="M33.6 26.9H30v1.2c0 1.6-.4 1.8-1.8 1.8H1.8C.4 30 0 29.7 0 28.2V1.9C0 .5.3.1 1.8.1H3v-5.5h30.6v32.3z" fill="none"/><path d="M3.1 26.9V.1H1.8C.3.1 0 .5 0 1.9v26.2C0 29.7.4 30 1.8 30h26.3c1.5 0 1.8-.3 1.8-1.8V27H3.1v-.1z"/><path d="M-715-401V715H309V-401H-715z" fill="none"/><path d="M-737.2-385.9v1116h1024v-1116h-1024z" fill="none"/></symbol><symbol id="icon-compass" viewBox="0 0 27 27"><g fill-rule="evenodd"><path d="M13.39.105C6.061.105.106 6.059.106 13.389c0 7.33 5.955 13.284 13.284 13.284 7.33 0 13.284-5.955 13.284-13.284C26.674 6.059 20.72.105 13.39.105zm0 1.328c6.611 0 11.956 5.345 11.956 11.956 0 6.61-5.345 11.955-11.956 11.955A11.946 11.946 0 0 1 1.435 13.39c0-6.61 5.345-11.956 11.955-11.956z"/><path d="M14.719 13.39a1.326 1.326 0 0 1-1.329 1.329 1.328 1.328 0 1 1 1.329-1.329z"/><path d="M6.715 7.414a.662.662 0 0 0-.535 1.007l4.515 7.44c.079.13.2.23.343.281l8.769 3.186a.666.666 0 0 0 .797-.966l-4.42-7.393v.002a.668.668 0 0 0-.34-.284L6.976 7.452h-.001a.67.67 0 0 0-.26-.038zM8.298 9.35l6.868 2.506 3.339 5.586-6.795-2.47-3.412-5.623v.001z"/><path d="M12.726 5.418h1.329V2.761h-1.329zM21.36 14.053h2.658v-1.328H21.36zM12.726 24.016h1.329v-2.657h-1.329zM2.763 14.053H5.42v-1.328H2.763z"/></g></symbol><symbol id="icon-clock" viewBox="0 0 15 15"><path d="M7.5 0C3.4 0 0 3.4 0 7.5S3.4 15 7.5 15 15 11.6 15 7.5 11.6 0 7.5 0zm0 13.5c-3.3 0-6-2.7-6-6s2.7-6 6-6 6 2.7 6 6-2.7 6-6 6z"/><path d="M7.4 9.3c-.4 0-.8-.4-.8-.9v-4c0-.5.3-.9.8-.9.4 0 .7.4.7.8v-.8 5c0 .4-.4.8-.7.8z"/><path d="M6.7 8.4c.2-.4.7-.6 1.1-.5l2.3.9c.5.1.7.7.6 1-.1.3-.6.6-1.1.5l-2.2-.9c-.5-.1-.8-.6-.7-1z"/></symbol><symbol id="icon-share" viewBox="0 0 30 30"><path d="M18.5.1H1.8C.3.1 0 .5 0 1.9v26.2C0 29.7.4 30 1.8 30h26.3c1.5 0 1.8-.3 1.8-1.8V12.3l-3.1 2.6v12H3.1V3.2h12.4l3-3.1zM23 9.9v4.2l7-7-7-7v3.5C6.8 4.2 6.8 19.7 6.8 19.7s4.8-9.1 16.2-9.8z"/><path d="M18.5.1H1.8C.3.1 0 .5 0 1.9v26.2C0 29.7.4 30 1.8 30h26.3c1.5 0 1.8-.3 1.8-1.8V12.3l-3.1 2.6v12H3.1V3.2h12.4l3-3.1zM23 9.9v4.2l7-7-7-7v3.5C6.8 4.2 6.8 19.7 6.8 19.7s4.8-9.1 16.2-9.8z" fill="none"/><path d="M-793.3-401V715h1024V-401h-1024z" fill="none"/><path d="M-815.4-385.9v1116h1024v-1116h-1024z" fill="none"/></symbol><symbol id="icon-sort" viewBox="0 0 21 21"><path d="M0 5.048h21V2H0v3.048zm0 7.4h14.438V9.402H0v3.048zm0 7.402h7v-3.048H0v3.048z" fill-rule="evenodd"/></symbol><symbol id="icon-activity" viewBox="0 0 20 20"><path d="M20 10v.357h-3.642L15 5.607c-.071-.142-.179-.25-.357-.25-.142 0-.286.108-.357.25l-2.143 6.5-1.786-5.108c-.07-.106-.215-.213-.357-.213a.342.342 0 0 0-.32.25l-1.5 4.5L6.786 8.43c-.07-.107-.178-.215-.286-.215a.374.374 0 0 0-.32.179L4.642 10.75l-.785-1.287a.416.416 0 0 0-.25-.178.407.407 0 0 0-.286.107l-.964.964H0V10C0 4.464 4.464 0 10 0s10 4.464 10 10m-4.286.822L14.606 6.93l-2.142 6.392a.342.342 0 0 1-.321.25.342.342 0 0 1-.32-.25l-1.787-5.108-1.465 4.357a.344.344 0 0 1-.32.25.392.392 0 0 1-.357-.215l-1.5-3.322-1.429 2.286a.382.382 0 0 1-.322.216c-.108 0-.25-.071-.286-.179l-.822-1.392-.75.75c-.106.07-.177.106-.285.106H.071A9.961 9.961 0 0 0 10 20a9.961 9.961 0 0 0 9.929-8.929H16.07c-.142 0-.286-.107-.357-.25" fill-rule="evenodd"/></symbol><symbol id="icon-calendar" viewBox="0 0 21 21"><path d="M19.483 5.097v13.709c-.151 1.053-.848 1.909-1.756 2.194H2.7C1.67 20.677.913 19.621.913 18.376V5.526c0-1.5 1.101-2.725 2.449-2.725h2.515V.725c0-.4.293-.725.652-.725.36 0 .652.326.652.725v2.076h6.065V.725c0-.4.293-.725.652-.725.359 0 .651.326.651.725v2.076h2.516c1.216 0 2.232.998 2.418 2.296zM3.362 4.25h2.515v1c0 .4.293.725.652.725.36 0 .652-.325.652-.725v-1h6.043v1c0 .4.292.725.652.725.359 0 .651-.325.651-.725v-1h2.516c.629 0 1.145.576 1.145 1.275v2.424H2.217V5.525c0-.7.515-1.275 1.145-1.275zm13.703 15.401H3.362c-.63 0-1.145-.575-1.145-1.275v-8.95H18.21v8.95c0 .7-.516 1.275-1.145 1.275z" fill-rule="evenodd"/><path d="M3.652 10.957h1.826v1.826H3.652zM3.652 13.696h1.826v1.826H3.652zm2.739-2.739h1.826v1.826H6.391zm0 2.739h1.826v1.826H6.391zm2.739-2.739h1.826v1.826H9.13zm0 2.739h1.826v1.826H9.13zm2.74-2.739h1.826v1.826H11.87zm0 2.739h1.826v1.826H11.87zm2.739-2.739h1.826v1.826h-1.826zm0 2.739h1.826v1.826h-1.826zM3.652 16.435h1.826v1.826H3.652zm2.739 0h1.826v1.826H6.391zm2.739 0h1.826v1.826H9.13zm2.74 0h1.826v1.826H11.87zm2.739 0h1.826v1.826h-1.826z"/></symbol><symbol id="icon-footsteps" viewBox="0 0 15 20"><path d="M1.773 11.792l4.134-.763c-.218 1.51 1.502 4.26-.45 5.019-2.696 1.047-3.534-1.37-3.684-4.256zM.177 7.066c.282 1.371 1.24 2.676 1.517 4.037l4.46-.823C7.604 3.972 6.187.9 4.188.127 2.04-.705-.73 2.66.177 7.067zm13.05 8.43l-4.133-.764c.217 1.51-1.503 4.26.45 5.019 2.696 1.047 3.533-1.37 3.683-4.256zm.079-.69c.278-1.362 1.235-2.666 1.517-4.037C15.73 6.365 12.96 3 10.811 3.83c-1.998.774-3.415 3.846-1.965 10.153l4.46.823z"/></symbol><symbol id="icon-exclamation" viewBox="0 0 15 15"><path d="M15 7.5a7.5 7.5 0 1 0-15 0 7.5 7.5 0 0 0 15 0zM7.488 3C6.691 2.993 6.032 3.585 6 4.336c0 .317.66 4.007.66 4.007.07.384.427.663.84.657h.036c.413.006.77-.273.84-.657C8.446 7.96 9 4.66 9 4.336 8.968 3.596 8.327 3.008 7.542 3h-.054zm.012 9a1 1 0 1 0 0-2 1 1 0 0 0 0 2z" fill-rule="evenodd"/></symbol><symbol id="icon-mastery-badge" viewBox="0 0 20 20"><path fill="#146AFF" d="M15.304 1.274A11.631 11.631 0 0 0 10 0C8.177 0 6.355.424 4.696 1.274L1.09 3.12a.165.165 0 0 0-.09.148v7.423a7.455 7.455 0 0 0 3.886 6.56l5.034 2.73a.17.17 0 0 0 .158 0l5.036-2.73A7.453 7.453 0 0 0 19 10.69V3.266a.16.16 0 0 0-.088-.146l-3.608-1.846z"/><path fill="#FFF" d="M10.89 4.716l.744 1.45a1 1 0 0 0 .743.533l1.603.239a1 1 0 0 1 .548 1.708l-1.126 1.088a1 1 0 0 0-.291.885l.26 1.55a1 1 0 0 1-1.437 1.058l-1.483-.75a1 1 0 0 0-.903 0l-1.482.75a1 1 0 0 1-1.437-1.058l.26-1.55a1 1 0 0 0-.29-.885L5.471 8.646a1 1 0 0 1 .548-1.708l1.603-.239a1 1 0 0 0 .743-.532l.745-1.451a1 1 0 0 1 1.78 0z"/></symbol><symbol id="icon-peer-review" viewBox="0 0 57 58"><g fill-rule="evenodd"><path d="M16.441 43.288v-10.59l.002-.095v-3.781l-1.263 2.46c-.373.728-1.244 1.029-1.945.668-.7-.36-.966-1.243-.593-1.97l2.476-4.826c.044-.083.092-.162.147-.232a1.648 1.648 0 0 1 1.558-1.11h4.399l-2.218 4.32c-.723 1.414-.238 3.208 1.228 3.963.919.47 1.944.392 2.758-.08l.056-.036v11.309H20.16v-9.093h-.751v9.093h-2.968zm.45-23.648a2.95 2.95 0 1 1 5.903 0 2.953 2.953 0 0 1-2.952 2.952 2.952 2.952 0 0 1-2.95-2.952z"/><path d="M24.376 43.288v-11.55c0-.035 0-.07.002-.108V27.464l-1.392 2.71c-.412.805-1.373 1.135-2.146.739-.774-.396-1.064-1.371-.653-2.174l2.729-5.319c.047-.091.102-.176.162-.255a1.818 1.818 0 0 1 1.716-1.224h6.54c.796 0 1.472.512 1.717 1.224.06.079.114.164.161.255l2.73 5.32c.412.802.12 1.777-.654 2.173-.773.396-1.734.066-2.144-.739l-1.392-2.708v4.104c0 .054-.002.106-.008.158v11.56h-3.291v-9.901h-.786v9.9h-3.291zm3.744-29.2h.006a3.255 3.255 0 0 1 3.251 3.253 3.255 3.255 0 0 1-3.254 3.254 3.255 3.255 0 0 1-3.254-3.254 3.255 3.255 0 0 1 3.251-3.253z"/><path d="M33.073 43.288V31.972l.065.044c.815.47 1.84.55 2.758.079 1.468-.755 1.951-2.549 1.228-3.963l-2.217-4.32h4.398c.722 0 1.336.464 1.558 1.11.053.072.103.149.147.232l2.476 4.825c.373.728.108 1.612-.593 1.97-.7.362-1.572.06-1.947-.667l-1.261-2.46v3.725c0 .048-.002.095-.007.143V43.288h-2.967v-9.093h-.75v9.093h-2.888zm.37-23.648a2.951 2.951 0 1 1 5.902 0 2.953 2.953 0 0 1-2.951 2.952 2.953 2.953 0 0 1-2.952-2.952zM52.869 45.376c.773-.062 1.091.482 1.13.966.038.483-.191 1.072-.965 1.133l-4.29.343c-.01 0-.02-.005-.032-.004-.017.005-.035.012-.052.013-.483.038-1.07-.192-1.13-.968l-.342-4.303a1.053 1.053 0 0 1 .962-1.133 1.052 1.052 0 0 1 1.13.966l.155 1.953c6.516-8.931 6.1-23.482-1.259-31.686-.599-.663-.442-1.286-.099-1.634.336-.344.997-.505 1.661.226 8.007 8.926 8.49 24.528 1.453 34.262l1.678-.134zm-6.36-37.96c.004.018.011.035.012.053.039.484-.19 1.068-.966 1.13l-4.305.343a1.054 1.054 0 0 1-1.132-.963 1.051 1.051 0 0 1 .964-1.13l1.955-.155C34.105.178 19.554.593 11.35 7.954c-.662.6-1.285.441-1.633.097-.345-.335-.506-.994.227-1.66 8.925-8.008 24.527-8.489 34.26-1.452l-.133-1.678c-.062-.774.481-1.092.965-1.13.485-.04 1.072.19 1.133.963l.344 4.292c0 .01-.004.02-.004.03zm-.573 42.24c.343.34.503 1.001-.227 1.668-8.898 8.032-24.453 8.514-34.157 1.457l.133 1.683c.062.775-.479 1.094-.961 1.134-.483.038-1.068-.192-1.13-.967l-.342-4.304c-.001-.012.004-.022.003-.033-.002-.017-.011-.035-.012-.052-.039-.485.192-1.072.963-1.133l4.292-.343a1.052 1.052 0 0 1 .168 2.098l-1.932.154c8.905 6.53 23.398 6.119 31.573-1.26.66-.602 1.282-.445 1.63-.101zM6.848 15.622l-.154-1.954C.178 22.598.594 37.15 7.953 45.352c.6.663.442 1.288.099 1.634-.335.345-.996.506-1.66-.227-8.008-8.925-8.49-24.526-1.453-34.26l-1.679.134c-.772.062-1.091-.482-1.13-.966-.038-.485.19-1.071.964-1.133l4.292-.343c.01-.001.02.003.03.003.019 0 .036-.01.054-.012.483-.037 1.068.192 1.13.966l.342 4.306a1.052 1.052 0 0 1-.964 1.131 1.052 1.052 0 0 1-1.13-.964z"/></g></symbol><symbol id="icon-quick-write" viewBox="0 0 48 37"><g fill-rule="evenodd"><path d="M33.427 6.4l-2.666 2.667H0V6.4h33.427zm-5.333 5.333L25.427 14.4H5.867v-2.667h22.227zm-5.333 5.334l-2.667 2.666H2.134v-2.666H22.76zm-4.8 4.8l-2.667 2.666h-8.36v-2.666H17.96zM13.244 29.2l5.556 5.556-7.777 2.221 2.22-7.777zM47.717 5.854l-3.692 3.692-5.5-5.5L42.217.355l5.5 5.5zM14.935 27.558L36.933 5.561l5.5 5.5-21.998 21.997-5.5-5.5z"/></g></symbol><symbol id="icon-self-review" viewBox="0 0 64 84"><defs><path d="M31.99955,44.85935 C20.03055,44.85935 10.29255,33.27335 10.29255,19.03535 C10.29255,4.79735 20.03055,-6.78865 31.99955,-6.78865 C43.96855,-6.78865 53.70655,4.79735 53.70655,19.03535 C53.70655,33.27335 43.96855,44.85935 31.99955,44.85935 Z M31.99955,-2.59765 C21.73355,-2.59765 13.38255,7.10535 13.38255,19.03535 C13.38255,30.96435 21.73355,40.66835 31.99955,40.66835 C42.26555,40.66835 50.61655,30.96535 50.61655,19.03535 C50.61655,7.10535 42.26555,-2.59765 31.99955,-2.59765 Z" id="path-1"></path></defs><g fill-rule="evenodd"><g transform="translate(0.000000, 7.000000)"><path d="M62.16365,17.60935 C62.57365,17.60935 62.97265,17.76935 63.26565,18.06235 C63.55865,18.35535 63.72265,18.75035 63.72565,19.16435 C63.72565,38.98435 50.33265,55.21435 33.56165,56.16835 L33.56165,73.66435 L40.98365,73.66435 C41.84765,73.66435 42.54665,74.36335 42.54665,75.22635 C42.54665,76.08935 41.84765,76.78935 40.98365,76.78935 L22.82065,76.78935 C21.95665,76.78935 21.25765,76.08935 21.25765,75.22635 C21.25765,74.36335 21.95665,73.66435 22.82065,73.66435 L30.43765,73.66435 L30.43765,56.16835 C13.66465,55.21435 0.27365,38.98835 0.27365,19.16435 C0.27765,18.75035 0.44165,18.35535 0.73465,18.06235 C1.02765,17.76935 1.42565,17.60935 1.83565,17.60935 L8.17165,17.60935 L8.17165,20.73435 L3.42965,20.73435 C4.12065,38.70335 16.67165,53.10535 31.99965,53.10535 C47.32765,53.10535 59.87865,38.70335 60.56965,20.73435 L55.43665,20.73435 L55.43665,17.60935 L62.16365,17.60935 Z" id="Clip-2"></path><path d="M31.99955,37.54295 C23.45655,37.54295 16.50755,29.24195 16.50755,19.03495 C16.50755,8.83195 23.45655,0.52695 31.99955,0.52695 C40.54255,0.52695 47.49155,8.83195 47.49155,19.03495 C47.49155,29.23795 40.54255,37.54295 31.99955,37.54295 Z M35.83955,15.34395 L35.83955,15.34795 C36.44855,14.73495 36.44855,13.74695 35.83955,13.13695 C35.22655,12.52795 34.23755,12.52795 33.62855,13.13695 L27.34755,19.41795 C26.73755,20.02795 26.73755,21.01595 27.34755,21.62595 C27.96055,22.23895 28.94855,22.23895 29.55855,21.62595 L35.83955,15.34395 Z M20.73755,12.80495 C20.12755,13.41795 20.12755,14.40695 20.73755,15.01595 C21.34655,15.62595 22.33855,15.62595 22.94855,15.01595 L29.22955,8.73895 C29.83855,8.12595 29.83855,7.13695 29.22955,6.52795 C28.61655,5.91795 27.62755,5.91795 27.01855,6.52795 L20.73755,12.80495 Z M21.83855,18.31295 L21.83855,18.31695 C21.22955,18.92595 21.22955,19.91495 21.83855,20.52395 C22.44855,21.13695 23.44055,21.13695 24.04955,20.52395 L34.73755,9.83995 C35.34755,9.23095 35.34755,8.23795 34.73755,7.62895 C34.12855,7.01995 33.13655,7.01995 32.52655,7.62895 L21.83855,18.31295 Z" id="Clip-5"></path><mask id="mask-2"><use xlink:href="#path-1"></use></mask><use id="Clip-8" xlink:href="#path-1"></use></g></g></symbol><symbol id=\"icon-guided-draft\" viewBox=\"0 0 98 72\"><path d=\"M97.801 68.699c.3.602.199 1.3-.2 1.8-.3.5-.898.802-1.5.802h-94.3C1.199 71.3.6 71 .3 70.5 0 69.898-.102 69.3.199 68.699L18.5 28.597c.3-.703 1-1.102 1.7-1.102h21.5l2.698 5.2C45.2 34.49 47 35.593 49 35.593c1.902 0 3.703-1.098 4.602-2.898.199-.301 1.3-2.5 2.699-5.2h21.5c.8 0 1.398.403 1.699 1.102l18.301 40.102zM24.602 47.801h1.2c1.1 0 2-.898 2-2 0-1.102-.9-2-2-2h-1.2c-1.102 0-2 .898-2 2 0 1.102.898 2 2 2zm9.2 8.3c.3-.702-.102-1.402-.798-1.706-.101 0-.5-.2-.601-.2-1.399-.699-2.7-1.597-3.7-2.898-.097 0-.296-.398-.398-.5-.402-.601-1.2-.7-1.8-.3-.602.402-.7 1.199-.301 1.8.101.102.5.5.5.602 1.199 1.699 2.8 2.898 4.699 3.699.101 0 .699.3.8.3.098.102.297.102.399.102.5 0 1-.398 1.2-.898zm11.3-.3c.598-.3.899-1.102.399-1.602-.301-.601-1.098-.8-1.7-.5-.101.102-.601.301-.601.301-1.2.602-2.5.903-4 1.102-.2 0-1.297.101-1.399.101-.699 0-1.199.602-1.199 1.301 0 .7.602 1.2 1.301 1.2 0 0 .7-.102.8-.102.298 0 .598-.102.9-.102 1.8-.199 3.402-.601 4.8-1.3 0-.098.598-.297.7-.399zm8.2-9.602c.398-.597.199-1.398-.297-1.601-.602-.399-1.403-.301-1.801.3-.098.102-.399.5-.399.602-.402.5-.8 1.098-1.101 1.7-.602.902-1.2 1.902-1.801 2.8 0 0-.297.399-.398.5-.5.602-.403 1.403.199 1.801.3.2.5.3.8.3.399 0 .7-.198 1-.5.102-.1.5-.5.5-.6.7-1 1.297-2 1.899-3 .3-.598.7-1.2 1-1.7.098-.102.297-.5.398-.602zm9.3-4.597c.7 0 1.297-.602 1.492-1.098 0-.7-.601-1.3-1.3-1.3h-.801c-.797 0-1.598.097-2.399.199-1.297.199-2.398.5-3.398 1 0 0-.598.296-.7.398-.6.3-.898 1.098-.5 1.7.204.402.704.6 1.102.6.203 0 .403-.097.602-.198 0-.102.5-.301.5-.301.8-.403 1.601-.7 2.601-.801.704-.098 1.403-.2 2.102-.2h.7zm4.7.898c.097 0 .5.301.5.403 2 1 2.902 2.398 3.3 3.199 0 .097.2.398.2.398.199.5.699.801 1.199.801.097 0 .296 0 .398-.102.7-.203 1-.902.8-1.601 0 0-.3-.598-.3-.7-.5-1-1.8-2.902-4.3-4.3 0-.098-.598-.297-.7-.399-.598-.296-1.398 0-1.7.602-.296.598 0 1.398.602 1.7zM76.7 56c.5-.601.5-1.398-.203-1.902l-1.7-1.5 1.399-1.7c.402-.5.3-1.3-.2-1.8-.5-.402-1.3-.3-1.8.2l-1.301 1.6-1.398-1.3c-.5-.5-1.301-.399-1.801.101s-.399 1.301.101 1.801l1.5 1.399-1.398 1.699c-.402.5-.3 1.3.2 1.8.198.2.5.301.8.301.398 0 .7-.199 1-.5l1.3-1.601 1.602 1.5c.297.199.598.3.899.3.398 0 .699-.097 1-.398z\"/><path d=\"M47.602 31.199L39.7 15.695c-2.5-4.898-.5-11.297 4.602-13.898 5.097-2.598 11.296-.5 13.898 4.601 1.5 3 1.5 6.403 0 9.301-1.5 3-7.899 15.5-7.899 15.5-.5 1.102-2.097 1.102-2.699 0zM49 5.601c-3 0-5.398 2.5-5.398 5.398 0 3 2.398 5.399 5.398 5.399 3 0 5.399-2.399 5.399-5.399S52 5.601 49 5.601z\"/></symbol></svg>\n';
    document.body.insertBefore(container, document.getElementById("svg-target"));
  }

  if (!loaded) {
    document.addEventListener('DOMContentLoaded', function listener() {
      document.removeEventListener('DOMContentLoaded', listener);
      onLoad();
    });
  } else {
    onLoad();
  }
}())

},{}],8:[function(require,module,exports){
require('../lib/index.js')
svgs = require('./assets/generated_svgs.js')
require('./assets/clipboard-setup.js')


},{"../lib/index.js":4,"./assets/clipboard-setup.js":6,"./assets/generated_svgs.js":7}]},{},[8]);
