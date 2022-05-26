CustomElement = require("../CustomElement");

CustomElement.create({
  tagName: "nri-textarea-v3",

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
      if (this._textarea.style.minHeight) {
        minHeight = parseInt(this._textarea.style.minHeight, 10);
      } else {
        minHeight = parseInt(
          window.getComputedStyle(this._textarea).minHeight,
          10
        );
      }
      if (minHeight === 0) {
        minHeight = parseInt(
          window.getComputedStyle(this._textarea).height,
          10
        );
      }

      this._textarea.style.overflowY = "hidden";
      this._textarea.style.minHeight = minHeight + "px";
      this._textarea.style.transition = "none";
      if (this._textarea.scrollHeight > minHeight) {
        this._textarea.style.height = minHeight + "px";
        this._textarea.style.height = this._textarea.scrollHeight + "px";
      } else {
        this._textarea.style.height = minHeight + "px";
      }
    },
  },
});
