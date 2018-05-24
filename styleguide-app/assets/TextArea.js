function AutoresizeTextArea() {
  var _this = HTMLElement.call(this) || this;
  _this._onInput = _this._onInput.bind(_this);
  _this._textarea = document.createElement("textarea");
  return _this;
}

AutoresizeTextArea.prototype = Object.create(HTMLElement.prototype);
AutoresizeTextArea.prototype.constructor = AutoresizeTextArea;

Object.defineProperties(AutoresizeTextArea.prototype, {
  defaultValue: {
    get: function() {
      return this._textarea.defaultValue;
    },
    set: function(value) {
      this._textarea.defaultValue = value;
    }
  },
  autofocus: {
    get: function() {
      return this._textarea.autofocus;
    },
    set: function(value) {
      this._textarea.autofocus = value;
    }
  },
  placeholder: {
    get: function() {
      return this._textarea.placeholder;
    },
    set: function(value) {
      this._textarea.placeholder = value;
    }
  },
  value: {
    get: function() {
      return this._textarea.value;
    },
    set: function(value) {
      this._textarea.value = value;
    }
  }
});

AutoresizeTextArea.prototype._resize = function() {
  var minHeight = null;
  if (this._textarea.style.minHeight) {
    minHeight = parseInt(this._textarea.style.minHeight, 10);
  } else {
    minHeight = parseInt(window.getComputedStyle(this._textarea).height, 10);
  }

  this._textarea.style.overflowY = "hidden";
  this._textarea.style.minHeight = minHeight + "px";
  if (this._textarea.scrollHeight > minHeight) {
    this._textarea.style.height = "auto";
    this._textarea.style.height = this._textarea.scrollHeight + "px";
  } else {
    this._textarea.style.height = minHeight + "px";
  }
};

AutoresizeTextArea.prototype._onInput = function() {
  this._resize();
  this.dispatchEvent(new Event("input"));
};

AutoresizeTextArea.prototype._reflectAttributes = function() {
  while (this.attributes.length) {
    var attribute = this.attributes[0];
    this.removeAttributeNode(attribute);
    this._textarea.setAttributeNode(attribute);
  }
  for (var k in this.dataset) {
    this._textarea.dataset[k] = this.dataset[k];
    delete this.dataset[k];
  }
};

AutoresizeTextArea.prototype.attributeChangedCallback = function(
  attribute,
  previous,
  next
) {
  if (previous && !next) {
    this._textarea.removeAttribute(attribute);
  } else {
    this._textarea.setAttribute(attribute, next);
  }
};

AutoresizeTextArea.prototype.connectedCallback = function() {
  this._textarea.addEventListener("input", this._onInput);
  this._reflectAttributes();
  this.appendChild(this._textarea);
  this._resize();
};

AutoresizeTextArea.prototype.disconnectedCallback = function() {
  this._textarea.removeEventListener("input", this._onInput);
};

customElements.define("autoresize-textarea", AutoresizeTextArea);
