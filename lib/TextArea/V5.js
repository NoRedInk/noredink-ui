"use strict";

var CustomElement = require("../CustomElement");

CustomElement.create({
  tagName: "nri-textarea-v5",

  initialize: function () {
    this._autoresize = false;
    this._singleline = false;
  },

  onConnect: function () {
    this._textarea = this.querySelector("textarea");
    this._updateListener();

    // In Firefox, for some reason, the `scrollHeight` property isn't correct
    // when the component is connected. It is correct when the window loads, though,
    // so we also perform a resize when that happens.
    // We end up doing this in all browsers, but it's fine: the second resize is a no-op.
    var self = this;
    window.addEventListener("load", function () {
      if (self._autoresize) {
        self._resize();
      }
    });
  },

  observedAttributes: ["data-autoresize", "data-singleline"],

  onAttributeChange: function (name, previous, next) {
    if (name === "data-autoresize") {
      this._autoresize = next !== null;
      if (!this._textarea) return;
      this._updateListener();
    }
    if (name === "data-singleline") {
      this._singleline = next !== null;
      if (!this._textarea) return;
      this._resize();
    }
  },

  methods: {
    _updateListener: function () {
      if (this._autoresize) {
        this._textarea.addEventListener("input", this._resize);
        this._resize();
      } else {
        this._textarea.removeEventListener("input", this._resize);
      }
    },

    _resizeAsSingleLine: function () {
      this._textarea.style.overflowY = "hidden";
      this._textarea.style.minHeight = 1.5 + "em";
      this._textarea.style.lineHeight = 1.5 + "em";
      this._textarea.style.transition = "none";
      this._textarea.style.resize = "none";
      this._textarea.style.height = 3 + "em";
    },

    _resize: function () {
      if (this._singleline && this._textarea.value.length === 0) {
        this._resizeAsSingleLine();
        return;
      }

      var minHeight = null;
      var computedStyles = window.getComputedStyle(this._textarea);

      if (this._textarea.style.minHeight) {
        minHeight = parseInt(this._textarea.style.minHeight, 10);
      } else {
        minHeight = parseInt(computedStyles.minHeight, 10);
      }

      if (minHeight === 0) {
        minHeight = parseInt(computedStyles.height, 10);
      }

      this._textarea.style.overflowY = "hidden";
      this._textarea.style.minHeight = minHeight + "px";
      this._textarea.style.transition = "none";

      // the browser does not include border widths in `.scrollHeight`, but we
      // sometimes use `box-sizing: border-box` on these elements so we need to
      // take it into account when setting the CSS `height`.
      var borderOffset = 0;
      if (computedStyles.boxSizing === "border-box") {
        borderOffset =
          parseInt(computedStyles.borderTopWidth, 10) +
          parseInt(computedStyles.borderBottomWidth, 10);
      }

      this._textarea.style.height =
        Math.max(minHeight, this._textarea.scrollHeight + borderOffset) + "px";
    },
  },
});
