"use strict";

const CustomElement = require("../CustomElement");

const MOBILE_BREAKPOINT_PX = 1000;
const DEFAULT_GAP_PX = 12;
const TAIL_SIZE_PX = 8;
const DEFAULT_VIEWPORT_PADDING_PX = 12;
const OFF_CENTER_OFFSET_PX = 20;

function clamp(value, min, max) {
  return Math.min(Math.max(value, min), max);
}

function placementAxis(placement) {
  return placement === "left" || placement === "right" ? "vertical" : "horizontal";
}

function oppositePlacement(placement) {
  switch (placement) {
    case "top":
      return "bottom";
    case "bottom":
      return "top";
    case "left":
      return "right";
    default:
      return "left";
  }
}

function alignmentAnchor(start, middle, end, alignment) {
  switch (alignment) {
    case "start":
      return start;
    case "end":
      return end;
    default:
      return middle;
  }
}

function numberFromAttribute(element, name, fallback) {
  const value = Number(element.getAttribute(name));

  return Number.isFinite(value) ? value : fallback;
}

function fitsPlacement(placement, triggerRect, overlayRect, gapPx, viewportPaddingPx) {
  switch (placement) {
    case "top":
      return overlayRect.height + gapPx + TAIL_SIZE_PX <= triggerRect.top - viewportPaddingPx;
    case "bottom":
      return (
        overlayRect.height + gapPx + TAIL_SIZE_PX <=
        window.innerHeight - triggerRect.bottom - viewportPaddingPx
      );
    case "left":
      return overlayRect.width + gapPx + TAIL_SIZE_PX <= triggerRect.left - viewportPaddingPx;
    default:
      return (
        overlayRect.width + gapPx + TAIL_SIZE_PX <=
        window.innerWidth - triggerRect.right - viewportPaddingPx
      );
  }
}

function placementScore(placement, triggerRect, viewportPaddingPx) {
  switch (placement) {
    case "top":
      return triggerRect.top - viewportPaddingPx;
    case "bottom":
      return window.innerHeight - triggerRect.bottom - viewportPaddingPx;
    case "left":
      return triggerRect.left - viewportPaddingPx;
    default:
      return window.innerWidth - triggerRect.right - viewportPaddingPx;
  }
}

function firstFocusableWithin(root) {
  return root.querySelector(
    [
      "a[href]",
      "button:not([disabled])",
      "input:not([disabled])",
      "select:not([disabled])",
      "textarea:not([disabled])",
      "[tabindex]:not([tabindex='-1'])",
    ].join(","),
  );
}

CustomElement.create({
  tagName: "nri-anchored-overlay-v1",
  initialize: function () {
    this._positionFrame = null;
    this._resizeObserver = null;
    this._restoreFocusOnClose = false;
    this._isConnected = false;
    this._listenersAttached = false;
  },
  observedAttributes: [
    "open",
    "data-trigger-id",
    "data-placement",
    "data-alignment",
    "data-mobile-placement",
    "data-mobile-alignment",
    "data-mode",
    "data-gap",
    "data-viewport-padding",
  ],
  methods: {
    _applyPopoverMode: function () {
      this.setAttribute("popover", "manual");
    },

    _getBubble: function () {
      return this.querySelector("[data-nri-anchored-overlay-bubble]");
    },

    _getTrigger: function () {
      const triggerId = this.getAttribute("data-trigger-id");
      return triggerId ? document.getElementById(triggerId) : null;
    },

    _isWithinTrigger: function (target) {
      const trigger = this._getTrigger();

      return Boolean(trigger && target && (target === trigger || trigger.contains(target)));
    },

    _requestedPlacement: function () {
      const usesMobile =
        window.innerWidth <= MOBILE_BREAKPOINT_PX && this.hasAttribute("data-mobile-placement");

      return (
        this.getAttribute(usesMobile ? "data-mobile-placement" : "data-placement") || "top"
      );
    },

    _requestedAlignment: function () {
      const usesMobile =
        window.innerWidth <= MOBILE_BREAKPOINT_PX && this.hasAttribute("data-mobile-alignment");

      return (
        this.getAttribute(usesMobile ? "data-mobile-alignment" : "data-alignment") || "middle"
      );
    },

    _gapPx: function () {
      return numberFromAttribute(this, "data-gap", DEFAULT_GAP_PX);
    },

    _viewportPaddingPx: function () {
      return numberFromAttribute(this, "data-viewport-padding", DEFAULT_VIEWPORT_PADDING_PX);
    },

    _resolvedPlacement: function (preferredPlacement, triggerRect, overlayRect) {
      const gapPx = this._gapPx();
      const viewportPaddingPx = this._viewportPaddingPx();

      if (fitsPlacement(preferredPlacement, triggerRect, overlayRect, gapPx, viewportPaddingPx)) {
        return preferredPlacement;
      }

      const flippedPlacement = oppositePlacement(preferredPlacement);
      if (fitsPlacement(flippedPlacement, triggerRect, overlayRect, gapPx, viewportPaddingPx)) {
        return flippedPlacement;
      }

      return ["top", "bottom", "left", "right"].reduce((bestPlacement, candidate) =>
        placementScore(candidate, triggerRect, viewportPaddingPx) >
        placementScore(bestPlacement, triggerRect, viewportPaddingPx)
          ? candidate
          : bestPlacement,
      );
    },

    _position: function () {
      this._positionFrame = null;

      if (!this.hasAttribute("open")) {
        return;
      }

      const trigger = this._getTrigger();
      const bubble = this._getBubble();

      if (!trigger || !bubble) {
        return;
      }

      const triggerRect = trigger.getBoundingClientRect();
      const bubbleRect = bubble.getBoundingClientRect();
      const preferredPlacement = this._requestedPlacement();
      const preferredAlignment = this._requestedAlignment();
      const resolvedPlacement = this._resolvedPlacement(preferredPlacement, triggerRect, bubbleRect);
      const gapPx = this._gapPx();
      const viewportPaddingPx = this._viewportPaddingPx();

      let left;
      let top;

      if (placementAxis(resolvedPlacement) === "horizontal") {
        const unclampedLeft = alignmentAnchor(
          triggerRect.left,
          triggerRect.left + (triggerRect.width - bubbleRect.width) / 2,
          triggerRect.right - bubbleRect.width,
          preferredAlignment,
        );

        left = clamp(
          unclampedLeft,
          viewportPaddingPx,
          Math.max(viewportPaddingPx, window.innerWidth - bubbleRect.width - viewportPaddingPx),
        );

        top =
          resolvedPlacement === "top"
            ? triggerRect.top - bubbleRect.height - gapPx - TAIL_SIZE_PX
            : triggerRect.bottom + gapPx + TAIL_SIZE_PX;

        top = clamp(
          top,
          viewportPaddingPx,
          Math.max(viewportPaddingPx, window.innerHeight - bubbleRect.height - viewportPaddingPx),
        );

        const tailAnchorX = alignmentAnchor(
          triggerRect.left + Math.min(triggerRect.width / 2, OFF_CENTER_OFFSET_PX),
          triggerRect.left + triggerRect.width / 2,
          triggerRect.right - Math.min(triggerRect.width / 2, OFF_CENTER_OFFSET_PX),
          preferredAlignment,
        );

        const tailOffset = clamp(
          tailAnchorX - left,
          TAIL_SIZE_PX + 6,
          Math.max(TAIL_SIZE_PX + 6, bubbleRect.width - TAIL_SIZE_PX - 6),
        );

        this.style.setProperty("--nri-overlay-tail-offset", tailOffset + "px");
      } else {
        const unclampedTop = alignmentAnchor(
          triggerRect.top,
          triggerRect.top + (triggerRect.height - bubbleRect.height) / 2,
          triggerRect.bottom - bubbleRect.height,
          preferredAlignment,
        );

        top = clamp(
          unclampedTop,
          viewportPaddingPx,
          Math.max(viewportPaddingPx, window.innerHeight - bubbleRect.height - viewportPaddingPx),
        );

        left =
          resolvedPlacement === "left"
            ? triggerRect.left - bubbleRect.width - gapPx - TAIL_SIZE_PX
            : triggerRect.right + gapPx + TAIL_SIZE_PX;

        left = clamp(
          left,
          viewportPaddingPx,
          Math.max(viewportPaddingPx, window.innerWidth - bubbleRect.width - viewportPaddingPx),
        );

        const tailAnchorY = alignmentAnchor(
          triggerRect.top + Math.min(triggerRect.height / 2, OFF_CENTER_OFFSET_PX),
          triggerRect.top + triggerRect.height / 2,
          triggerRect.bottom - Math.min(triggerRect.height / 2, OFF_CENTER_OFFSET_PX),
          preferredAlignment,
        );

        const tailOffset = clamp(
          tailAnchorY - top,
          TAIL_SIZE_PX + 6,
          Math.max(TAIL_SIZE_PX + 6, bubbleRect.height - TAIL_SIZE_PX - 6),
        );

        this.style.setProperty("--nri-overlay-tail-offset", tailOffset + "px");
      }

      this.style.left = left + "px";
      this.style.top = top + "px";

      this.setAttribute("data-resolved-placement", resolvedPlacement);
      this.setAttribute("data-resolved-alignment", preferredAlignment);
    },

    _queuePosition: function () {
      if (this._positionFrame != null) {
        cancelAnimationFrame(this._positionFrame);
      }

      this._positionFrame = requestAnimationFrame(this._position);
    },

    _openIfNeeded: function () {
      this.style.display = "block";
      this._applyPopoverMode();

      if (typeof this.showPopover === "function" && !this.matches(":popover-open")) {
        try {
          this.showPopover();
        } catch (_error) {
          // Ignore browsers with partial popover support; the overlay will still be displayed.
        }
      }

      this._queuePosition();
    },

    _closeIfNeeded: function () {
      const didUseNativeHide =
        typeof this.hidePopover === "function" && this.matches(":popover-open");

      if (didUseNativeHide) {
        this.hidePopover();
      }

      this.style.display = "none";

      if (!didUseNativeHide && this._restoreFocusOnClose) {
        this._restoreFocusOnClose = false;

        const trigger = this._getTrigger();
        if (trigger && typeof trigger.focus === "function") {
          trigger.focus();
        }
      }
    },

    _syncOpenState: function () {
      if (this.hasAttribute("open")) {
        this._openIfNeeded();
        this._startObservers();
      } else {
        this._stopObservers();
        this._closeIfNeeded();
      }
    },

    _onToggle: function (event) {
      if (
        event.newState === "closed" &&
        this.hasAttribute("open") &&
        this.getAttribute("data-mode") === "popover"
      ) {
        this.dispatchEvent(CustomElement.makeEvent("request-close"));
      }

      if (event.newState === "closed" && this._restoreFocusOnClose) {
        this._restoreFocusOnClose = false;

        const trigger = this._getTrigger();
        if (trigger && typeof trigger.focus === "function") {
          trigger.focus();
        }
      }
    },

    _onDocumentKeyDown: function (event) {
      if (event.key !== "Escape") {
        return;
      }

      const activeElement = document.activeElement;

      if (this._isWithinTrigger(activeElement) || this.contains(activeElement)) {
        this._restoreFocusOnClose = true;
      }

      if (
        this.getAttribute("data-mode") === "popover" &&
        this.hasAttribute("open") &&
        (this._isWithinTrigger(activeElement) || this.contains(activeElement))
      ) {
        event.preventDefault();
        event.stopPropagation();
        if (typeof event.stopImmediatePropagation === "function") {
          event.stopImmediatePropagation();
        }
        this.dispatchEvent(CustomElement.makeEvent("request-close"));
      }
    },

    _onDocumentPointerDown: function (event) {
      if (this.getAttribute("data-mode") !== "popover" || !this.hasAttribute("open")) {
        return;
      }

      if (this._isWithinTrigger(event.target) || this.contains(event.target)) {
        return;
      }

      this.dispatchEvent(CustomElement.makeEvent("request-close"));
    },

    _onDocumentFocusIn: function (event) {
      if (this.getAttribute("data-mode") !== "popover" || !this.hasAttribute("open")) {
        return;
      }

      if (this._isWithinTrigger(event.target) || this.contains(event.target)) {
        return;
      }

      this.dispatchEvent(CustomElement.makeEvent("request-close"));
    },

    _onBubbleMouseEvent: function (event) {
      if (this.getAttribute("data-mode") !== "popover") {
        return;
      }

      const bubble = this._getBubble();

      if (!bubble || !bubble.contains(event.target)) {
        return;
      }

      event.stopPropagation();
      if (typeof event.stopImmediatePropagation === "function") {
        event.stopImmediatePropagation();
      }
    },

    _onWindowScroll: function () {
      if (this.hasAttribute("open")) {
        this._queuePosition();
      }
    },

    _onWindowResize: function () {
      if (this.hasAttribute("open")) {
        this._queuePosition();
      }
    },

    _startObservers: function () {
      if (this._listenersAttached) {
        return;
      }

      if (this._resizeObserver == null) {
        const bubble = this._getBubble();
        const trigger = this._getTrigger();

        this._resizeObserver = new ResizeObserver(this._queuePosition);
        if (bubble) {
          this._resizeObserver.observe(bubble);
        }
        if (trigger) {
          this._resizeObserver.observe(trigger);
        }
      }

      window.addEventListener("scroll", this._onWindowScroll, true);
      window.addEventListener("resize", this._onWindowResize);
      document.addEventListener("keydown", this._onDocumentKeyDown, true);
      document.addEventListener("mousedown", this._onDocumentPointerDown, true);
      document.addEventListener("focusin", this._onDocumentFocusIn, true);
      this._listenersAttached = true;
    },

    _stopObservers: function () {
      if (!this._listenersAttached) {
        return;
      }

      if (this._resizeObserver != null) {
        this._resizeObserver.disconnect();
        this._resizeObserver = null;
      }

      window.removeEventListener("scroll", this._onWindowScroll, true);
      window.removeEventListener("resize", this._onWindowResize);
      document.removeEventListener("keydown", this._onDocumentKeyDown, true);
      document.removeEventListener("mousedown", this._onDocumentPointerDown, true);
      document.removeEventListener("focusin", this._onDocumentFocusIn, true);
      this._listenersAttached = false;
    },
  },
  onConnect: function () {
    this._isConnected = true;
    this._applyPopoverMode();
    this.addEventListener("toggle", this._onToggle);
    this.addEventListener("mousedown", this._onBubbleMouseEvent);
    this.addEventListener("mouseup", this._onBubbleMouseEvent);
    this.addEventListener("click", this._onBubbleMouseEvent);
    this._syncOpenState();
  },
  onDisconnect: function () {
    this._isConnected = false;
    this._stopObservers();
    this.removeEventListener("toggle", this._onToggle);
    this.removeEventListener("mousedown", this._onBubbleMouseEvent);
    this.removeEventListener("mouseup", this._onBubbleMouseEvent);
    this.removeEventListener("click", this._onBubbleMouseEvent);
  },
  onAttributeChange: function () {
    if (!this._isConnected) {
      return;
    }

    this._syncOpenState();
  },
});
