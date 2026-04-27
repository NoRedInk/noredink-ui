"use strict";

/**
 * Custom element that powers `Nri.Ui.Tooltip.V4`'s auto-flip mode.
 *
 * The Elm view renders a `<nri-tooltip-auto>` wrapper around the trigger
 * and tooltip when auto-positioning is enabled. This element observes the
 * trigger's bounding rect and the viewport, and writes
 * `data-position="top|bottom|left|right"` and
 * `data-align="start|middle|end"` onto the tooltip element so that
 * attribute-keyed CSS can flip the rendered position without round-tripping
 * through Elm.
 *
 * Inputs (attributes on the custom element):
 * - `data-trigger-id`     — id of the trigger element to measure against.
 * - `data-tooltip-id`     — id of the tooltip element whose data-* attrs
 *                           we mutate.
 * - `data-preferred-position` — "top" | "bottom" | "left" | "right".
 *                               Default: "top".
 * - `data-preferred-align`    — "start" | "middle" | "end".
 *                               Default: "middle".
 * - `data-offset`         — numeric pixel gap between trigger and tooltip.
 *                           Default: 8.
 *
 * Behaviour:
 * - On connect, after the next paint, and on every viewport / scroll /
 *   resize event, recompute. If the tooltip's natural bounding box at the
 *   preferred position would clip the viewport, flip to the opposite side
 *   (and shift align if the tail would otherwise point off the trigger).
 * - If the preferred side fits, leave data-position alone — meaning a
 *   tooltip explicitly positioned `above` only ever moves to `below` if it
 *   would clip.
 */

var CustomElement = require("../CustomElement");

var OPPOSITE = {
  top: "bottom",
  bottom: "top",
  left: "right",
  right: "left",
};

CustomElement.create({
  tagName: "nri-tooltip-auto",

  initialize: function () {
    this._scheduled = false;
    this._onScrollOrResize = this._onScrollOrResize.bind(this);
    this._update = this._update.bind(this);
  },

  observedAttributes: [
    "data-trigger-id",
    "data-tooltip-id",
    "data-preferred-position",
    "data-preferred-align",
    "data-offset",
  ],

  onAttributeChange: function () {
    this._schedule();
  },

  onConnect: function () {
    window.addEventListener("scroll", this._onScrollOrResize, true);
    window.addEventListener("resize", this._onScrollOrResize);

    if (typeof ResizeObserver === "function") {
      this._resizeObserver = new ResizeObserver(this._onScrollOrResize);
      var trigger = this._getTrigger();
      var tooltip = this._getTooltip();
      if (trigger) this._resizeObserver.observe(trigger);
      if (tooltip) this._resizeObserver.observe(tooltip);
    }

    this._schedule();
  },

  onDisconnect: function () {
    window.removeEventListener("scroll", this._onScrollOrResize, true);
    window.removeEventListener("resize", this._onScrollOrResize);
    if (this._resizeObserver) {
      this._resizeObserver.disconnect();
      this._resizeObserver = null;
    }
  },

  methods: {
    _onScrollOrResize: function () {
      this._schedule();
    },

    _schedule: function () {
      if (this._scheduled) return;
      this._scheduled = true;
      var self = this;
      window.requestAnimationFrame(function () {
        self._scheduled = false;
        self._update();
      });
    },

    _getTrigger: function () {
      var id = this.getAttribute("data-trigger-id");
      return id ? document.getElementById(id) : null;
    },

    _getTooltip: function () {
      var id = this.getAttribute("data-tooltip-id");
      return id ? document.getElementById(id) : null;
    },

    _update: function () {
      var trigger = this._getTrigger();
      var tooltip = this._getTooltip();
      if (!trigger || !tooltip) return;

      var preferredPos = this.getAttribute("data-preferred-position") || "top";
      var preferredAlign =
        this.getAttribute("data-preferred-align") || "middle";
      var offset = Number(this.getAttribute("data-offset")) || 8;

      var triggerRect = trigger.getBoundingClientRect();
      // Measure tooltip natural size while neutralizing transforms by
      // reading offsetWidth/Height; getBoundingClientRect would include
      // any transform we apply for the current data-position.
      var tooltipWidth = tooltip.offsetWidth;
      var tooltipHeight = tooltip.offsetHeight;

      var viewportWidth =
        document.documentElement.clientWidth || window.innerWidth;
      var viewportHeight =
        document.documentElement.clientHeight || window.innerHeight;

      var fits = function (pos) {
        switch (pos) {
          case "top":
            return triggerRect.top - offset - tooltipHeight >= 0;
          case "bottom":
            return (
              triggerRect.bottom + offset + tooltipHeight <= viewportHeight
            );
          case "left":
            return triggerRect.left - offset - tooltipWidth >= 0;
          case "right":
            return triggerRect.right + offset + tooltipWidth <= viewportWidth;
          default:
            return true;
        }
      };

      var resolvedPos = preferredPos;
      if (!fits(preferredPos) && fits(OPPOSITE[preferredPos])) {
        resolvedPos = OPPOSITE[preferredPos];
      }

      // Align: shift if the tail would point off the trigger because the
      // tooltip got clamped against the viewport edge along the cross axis.
      var resolvedAlign = preferredAlign;
      var crossAxisIsHorizontal =
        resolvedPos === "top" || resolvedPos === "bottom";
      if (crossAxisIsHorizontal) {
        var triggerCenterX = (triggerRect.left + triggerRect.right) / 2;
        var halfTooltip = tooltipWidth / 2;
        if (
          preferredAlign === "middle" &&
          triggerCenterX - halfTooltip < 0
        ) {
          resolvedAlign = "start";
        } else if (
          preferredAlign === "middle" &&
          triggerCenterX + halfTooltip > viewportWidth
        ) {
          resolvedAlign = "end";
        }
      } else {
        var triggerCenterY = (triggerRect.top + triggerRect.bottom) / 2;
        var halfTooltipY = tooltipHeight / 2;
        if (
          preferredAlign === "middle" &&
          triggerCenterY - halfTooltipY < 0
        ) {
          resolvedAlign = "start";
        } else if (
          preferredAlign === "middle" &&
          triggerCenterY + halfTooltipY > viewportHeight
        ) {
          resolvedAlign = "end";
        }
      }

      if (tooltip.getAttribute("data-position") !== resolvedPos) {
        tooltip.setAttribute("data-position", resolvedPos);
      }
      if (tooltip.getAttribute("data-align") !== resolvedAlign) {
        tooltip.setAttribute("data-align", resolvedAlign);
      }
    },
  },
});
