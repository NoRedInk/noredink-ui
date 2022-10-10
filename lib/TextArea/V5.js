CustomElement = require("../CustomElement");

CustomElement.create({
  tagName: "nri-textarea-v45",

  initialize: function () {
    this._autoresize = false;
  },

  onConnect: function () {
    this._textarea = this.querySelector("textarea");
    this._updateListener();
  },

  observedAttributes: ["data-autoresize"],

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

      this._textarea.style.height =
        Math.max(minHeight, this._textarea.scrollHeight + borderOffset) + "px";
    },
  },
});
