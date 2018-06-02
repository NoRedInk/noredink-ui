window.Nri.CustomElement.create({
  tagName: 'nri-textarea-v3',

  initialize: function() {
    this._autoresize = false
  },

  onConnect: function() {
    this._textarea = this.querySelector('textarea')
    this._updateListener()
  },

  properties: {
    autoresize: {
      get: function() {
        return this._autoresize
      },
      set: function(value) {
        if (value === this._autoresize) return
        this._autoresize = value
        if (this._textarea) this._updateListener()
      }
    }
  },

  methods: {
    _updateListener: function() {
      if (this._autoresize) {
        this._textarea.addEventListener('input', this._onInput)
      } else {
        this._textarea.removeEventListener('input', this._onInput)
      }
    },

    _onInput: function() {
      var minHeight = this._textarea.style.minHeight ?
          parseInt(this._textarea.style.minHeight, 10) :
          parseInt(window.getComputedStyle(this._textarea).height, 10)
      
      this._textarea.style.overflowY = 'hidden'
      this._textarea.style.minHeight = minHeight + 'px'
      if (this._textarea.scrollHeight > minHeight) {
        this._textarea.style.height = 'auto'
        this._textarea.style.height = this._textarea.scrollHeight + 'px'
      } else {
        this._textarea.style.height = minHeight + 'px'
      }
    }
  }
})
