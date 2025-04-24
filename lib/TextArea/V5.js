"use strict";

var CustomElement = require("../CustomElement");

CustomElement.create({
  tagName: "nri-textarea-v5",

  initialize: function () {
    this._autoresize = false;
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

    _resize: function () {
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

      // NOTE: scrollHeight is very convenient to autoresize because it grows
      // automatically as content is added to the textarea. However, the
      // scrollHeight doesn't get smaller as content is deleted. That's not
      // great because we would want our autoresized boxed to shrink
      // automatically too.
      //
      // We achieve this by temporarily setting height to "auto" before looking
      // at the scrollHeight. This will cause the scrollHeight to re-adjust to
      // the contents so we can then re-set the height property.
      this._textarea.style.height = "auto";
      const scrollHeight = this._textarea.scrollHeight;
      this._textarea.style.height =
        Math.max(minHeight, scrollHeight + borderOffset) + "px";
    },
  },
});
