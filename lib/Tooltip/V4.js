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

function rectsEqual(a, b) {
  if (!a || !b) return false;
  return (
    a.top === b.top &&
    a.left === b.left &&
    a.right === b.right &&
    a.bottom === b.bottom
  );
}

CustomElement.create({
  tagName: "nri-tooltip-auto",

  initialize: function () {
    this._rafId = null;
    this._lastTriggerRect = null;
    this._tick = this._tick.bind(this);
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
    this._lastTriggerRect = null;
  },

  onConnect: function () {
    // requestAnimationFrame loop: cheap (a couple of layout reads + maybe a
    // setAttribute) and catches every kind of position change, including
    // ones we can't observe directly (e.g. an ancestor's `left`/`top`
    // changing during a drag, layout reflows from sibling content, etc.).
    // Only updates the DOM when the trigger's bounding rect actually
    // changes, so the cost is mostly the rect read.
    this._rafId = window.requestAnimationFrame(this._tick);
  },

  onDisconnect: function () {
    if (this._rafId !== null) {
      window.cancelAnimationFrame(this._rafId);
      this._rafId = null;
    }
    this._lastTriggerRect = null;
  },

  methods: {
    _tick: function () {
      var trigger = this._getTrigger();
      if (trigger) {
        var rect = trigger.getBoundingClientRect();
        if (!rectsEqual(rect, this._lastTriggerRect)) {
          this._lastTriggerRect = rect;
          this._update(rect);
        }
      }
      this._rafId = window.requestAnimationFrame(this._tick);
    },

    _getTrigger: function () {
      var id = this.getAttribute("data-trigger-id");
      return id ? document.getElementById(id) : null;
    },

    _getTooltip: function () {
      var id = this.getAttribute("data-tooltip-id");
      return id ? document.getElementById(id) : null;
    },

    _update: function (triggerRect) {
      var tooltip = this._getTooltip();
      if (!triggerRect || !tooltip) return;

      var preferredPos = this.getAttribute("data-preferred-position") || "top";
      var preferredAlign =
        this.getAttribute("data-preferred-align") || "middle";
      var offset = Number(this.getAttribute("data-offset")) || 12;
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
