const fs = require("fs");
const puppeteer = require("puppeteer");
const httpServer = require("http-server");
const percySnapshot = require("@percy/puppeteer");

const platform = require("os").platform();
const puppeteerArgs = (
  /^win/.test(platform) ? [] : ["--single-process"]
).concat(
  // https://stackoverflow.com/questions/50662388/running-headless-chrome-puppeteer-with-no-sandbox
  /^linux/.test(platform) ? ["--no-sandbox"] : [],
);
const PORT = process.env.PORT_NUMBER || 8000;

const { AxePuppeteer } = require("@axe-core/puppeteer");
const assert = require("assert");

describe("UI tests", function () {
  let page;
  let server;
  let browser;

  before(async () => {
    let root = process.env.ROOT || `${__dirname}/../public`;

    if (!fs.existsSync(root)) {
      assert.fail(
        `Root was specified as ${root}, but that path does not exist.`,
      );
    }

    if (!fs.existsSync(`${root}/index.html`)) {
      assert.fail(
        `Root was specified as ${root}, but does not contain an index.html.`,
      );
    }

    server = httpServer.createServer({ root });
    server.listen(PORT);

    browser = await puppeteer.launch({
      headless: true,
      timeout: 10000,
      args: puppeteerArgs,
    });
  });

  after(() => {
    server.close();
  });

  const hasText = async (xPathSelector = "//html", text) => {
    let [node] = await page.$$(`xpath/.${xPathSelector}`);
    let innerText = await page.evaluate((el) => el.innerText, node);
    assert.equal(innerText, text);
  };

  const handlePageErrors = function (page) {
    page.on("pageerror", (err) => {
      console.log("Error from page:", err.toString());
    });
  };

  const injectReducedMotion = async (page) => {
    await page.addStyleTag({
      content: `
        @media (prefers-reduced-motion: reduce) {
          *, *::before, *::after {
            animation: none !important;
            transition: none !important;
            scroll-behavior: auto !important;
          }
        }
      `,
    });
  };

  const handleAxeResults = function (name, results) {
    const violations = results["violations"];
    if (violations.length > 0) {
      violations.map(function (violation) {
        console.log("\n\n", violation["id"], ":", violation["description"]);
        console.log(violation["help"]);
        console.log(violation["helpUrl"]);

        console.table(violation["nodes"], ["html"]);
      });
      assert.fail(
        `Expected no axe violations in ${name} but got ${violations.length} violations`,
      );
    }
  };

  const goToExample = async (name, location) => {
    await page.goto(location, { waitUntil: "load" });
    await injectReducedMotion(page);
    await page.waitForSelector(
      `xpath/.//h1[contains(., 'Nri.Ui.${name}') and @aria-current='page']`,
    );
  };

  const defaultProcessing = async (name, location) => {
    await goToExample(name, location);
    await percySnapshot(page, name);

    const results = await new AxePuppeteer(page)
      .disableRules(skippedRules[name] || [])
      .analyze();
    handleAxeResults(name, results);
  };

  const defaultUsageExampleProcessing = async (testName, name, location) => {
    await page.goto(location, { waitUntil: "load" });
    await injectReducedMotion(page);
    await page.waitForSelector(
      `xpath/.//h1[contains(., '${name}') and @aria-current='page']`,
    );
    await percySnapshot(page, name);

    const results = await new AxePuppeteer(page)
      .disableRules(skippedRules[testName] || [])
      .analyze();
    handleAxeResults(name, results);
  };

  const messageProcessing = async (name, location) => {
    await goToExample(name, location);
    await percySnapshot(page, name);

    var axe = await new AxePuppeteer(page)
      .disableRules(skippedRules[name] || [])
      .analyze();
    handleAxeResults(name, axe);
  };

  const modalProcessing = async (name, location) => {
    await goToExample(name, location);

    await page.click("#launch-modal");
    await page.waitForSelector('[role="dialog"]');
    await percySnapshot(page, `${name} - info`);

    axe = await new AxePuppeteer(page).analyze();

    await page.click('[aria-label="Close modal"]');

    handleAxeResults(`${name} - info`, axe);
  };

  const pageProcessing = async (name, location) => {
    await goToExample(name, location);

    var axe = await new AxePuppeteer(page)
      .disableRules(skippedRules[name] || [])
      .analyze();
    handleAxeResults(name, axe);

    await percySnapshot(page, name, {
      scope: "[data-page-container='']",
    });
  };

  const iconProcessing = async (name, location) => {
    await page.goto(location);
    await injectReducedMotion(page);
    await page.waitForSelector(`#${name}`);
    await percySnapshot(page, name);

    // visible icon names snapshot
    await page.click("#show-icon-name-checkbox");
    await page.waitForSelector("#show-icon-name-checkbox[aria-checked=true]");
    await percySnapshot(page, `${name} - display icon names`);

    const results = await new AxePuppeteer(page)
      .disableRules(skippedRules[name] || [])
      .analyze();
    handleAxeResults(name, results);
  };

  const clickableCardWithTooltipProcessing = async (
    testName,
    name,
    location,
  ) => {
    const hasParentClicks = async (count) => {
      await page.waitForSelector(`text/Parent Clicks: ${count}`, {
        timeout: 5000,
      });
    };

    await defaultUsageExampleProcessing(testName, name, location);

    await hasParentClicks(0);
    await page.waitForSelector("[data-tooltip-visible=false]");

    // Opening and closing the tooltip doesn't trigger the container effects
    await page.hover('[aria-label="Tooltip trigger"]');
    await page.waitForSelector("[data-tooltip-visible=true]");

    await page.click('[aria-label="Tooltip trigger"]');
    await page.waitForSelector("[data-tooltip-visible=false]");

    await hasParentClicks(0);

    // Clicking the button does trigger container effects
    const [button] = await page.$$("xpath/.//button[contains(., 'Click me')]");
    await button.click();
    await button.click();

    await page.waitForSelector("[data-tooltip-visible=false]");
    await hasParentClicks(1);

    // Clicking the container does trigger container effects
    await page.click("#container-element");
    await hasParentClicks(2);
  };

  const hintTooltipProcessing = async (name, location) => {
    await goToExample(name, location);

    await page.hover("#hint-tooltip-trigger");
    await page.waitForSelector("#hint-tooltip-demo[open]");

    const clippingDemo = await page.evaluate(() => {
      const clip = document
        .querySelector("#hint-tooltip-clipping-demo")
        .getBoundingClientRect();
      const overlay = document
        .querySelector("#hint-tooltip-demo")
        .getBoundingClientRect();

      return {
        overlayVisible: overlay.width > 0 && overlay.height > 0,
        escapesClip: overlay.top < clip.top,
      };
    });

    assert.equal(clippingDemo.overlayVisible, true);
    assert.equal(clippingDemo.escapesClip, true);

    await page.hover("#hint-tooltip-scroll-trigger");
    await page.waitForSelector("#hint-tooltip-scroll[open]");

    const scrollDemo = await page.evaluate(() => {
      const clip = document
        .querySelector("#hint-tooltip-scroll-demo")
        .getBoundingClientRect();
      const overlay = document
        .querySelector("#hint-tooltip-scroll")
        .getBoundingClientRect();

      return {
        overlayVisible: overlay.width > 0 && overlay.height > 0,
        escapesClip: overlay.top < clip.top,
      };
    });

    assert.equal(scrollDemo.overlayVisible, true);
    assert.equal(scrollDemo.escapesClip, true);

    await page.focus("#hint-tooltip-trigger");
    await page.keyboard.press("Escape");
    await page.waitForSelector("#hint-tooltip-demo:not([open])");

    await page.$eval("#hint-tooltip-flip-trigger", (node) =>
      node.scrollIntoView({ block: "start" }),
    );
    await page.focus("#hint-tooltip-flip-trigger");
    await page.waitForSelector("#hint-tooltip-flip[open]");
    await page.waitForFunction(
      () =>
        document
          .querySelector("#hint-tooltip-flip")
          .getAttribute("data-resolved-placement") === "bottom",
    );

    await page.$eval("#hint-tooltip-edge-trigger", (node) =>
      node.scrollIntoView({ block: "end" }),
    );
    await page.focus("#hint-tooltip-edge-trigger");
    await page.waitForSelector("#hint-tooltip-edge[open]");

    const edgePlacement = await page.evaluate(() => {
      const overlay = document
        .querySelector("#hint-tooltip-edge")
        .getBoundingClientRect();

      return {
        top: overlay.top,
        bottom: overlay.bottom,
        left: overlay.left,
        right: overlay.right,
        viewportHeight: window.innerHeight,
        viewportWidth: window.innerWidth,
      };
    });

    assert.ok(edgePlacement.top >= 0);
    assert.ok(edgePlacement.bottom <= edgePlacement.viewportHeight);
    assert.ok(edgePlacement.left >= 0);
    assert.ok(edgePlacement.right <= edgePlacement.viewportWidth);

    await percySnapshot(page, name);

    const results = await new AxePuppeteer(page)
      .disableRules(skippedRules[name] || [])
      .analyze();
    handleAxeResults(name, results);
  };

  const infoPopoverProcessing = async (name, location) => {
    await goToExample(name, location);

    await page.click("#info-popover-trigger");
    await page.waitForSelector("#info-popover-demo[open]");

    const clippingDemo = await page.evaluate(() => {
      const clip = document
        .querySelector("#info-popover-clipping-demo")
        .getBoundingClientRect();
      const overlay = document
        .querySelector("#info-popover-demo")
        .getBoundingClientRect();

      return {
        overlayVisible: overlay.width > 0 && overlay.height > 0,
        escapesClip: overlay.top < clip.top,
      };
    });

    assert.equal(clippingDemo.overlayVisible, true);
    assert.equal(clippingDemo.escapesClip, true);

    await page.click("#info-popover-scroll-trigger");
    await page.waitForSelector("#info-popover-scroll[open]");

    const scrollDemo = await page.evaluate(() => {
      const clip = document
        .querySelector("#info-popover-scroll-demo")
        .getBoundingClientRect();
      const overlay = document
        .querySelector("#info-popover-scroll")
        .getBoundingClientRect();

      return {
        overlayVisible: overlay.width > 0 && overlay.height > 0,
        escapesClip: overlay.top < clip.top,
      };
    });

    assert.equal(scrollDemo.overlayVisible, true);
    assert.equal(scrollDemo.escapesClip, true);

    await page.click("body", { offset: { x: 8, y: 8 } });
    await page.waitForSelector("#info-popover-demo:not([open])");

    await page.click("#info-popover-trigger");
    await page.waitForSelector("#info-popover-demo[open]");
    await page.keyboard.press("Escape");
    await page.waitForSelector("#info-popover-demo:not([open])");

    const activeId = await page.evaluate(() => document.activeElement.id);
    assert.equal(activeId, "info-popover-trigger");

    await page.$eval("#info-popover-flip-trigger", (node) =>
      node.scrollIntoView({ block: "start" }),
    );
    await page.click("#info-popover-flip-trigger");
    await page.waitForSelector("#info-popover-flip[open]");
    await page.waitForFunction(
      () =>
        document
          .querySelector("#info-popover-flip")
          .getAttribute("data-resolved-placement") === "bottom",
    );

    await page.$eval("#info-popover-edge-trigger", (node) =>
      node.scrollIntoView({ block: "end" }),
    );
    await page.click("#info-popover-edge-trigger");
    await page.waitForSelector("#info-popover-edge[open]");

    const edgePlacement = await page.evaluate(() => {
      const overlay = document
        .querySelector("#info-popover-edge")
        .getBoundingClientRect();

      return {
        top: overlay.top,
        bottom: overlay.bottom,
        left: overlay.left,
        right: overlay.right,
        viewportHeight: window.innerHeight,
        viewportWidth: window.innerWidth,
      };
    });

    assert.ok(edgePlacement.top >= 0);
    assert.ok(edgePlacement.bottom <= edgePlacement.viewportHeight);
    assert.ok(edgePlacement.left >= 0);
    assert.ok(edgePlacement.right <= edgePlacement.viewportWidth);

    await percySnapshot(page, name);

    const results = await new AxePuppeteer(page)
      .disableRules(skippedRules[name] || [])
      .analyze();
    handleAxeResults(name, results);
  };

  const skippedRules = {
    Block: ["scrollable-region-focusable"],
    // Loading's color contrast check seems to change behavior depending on whether Percy snapshots are taken or not
    Loading: ["color-contrast"],
    Outline: ["color-contrast"],
    RadioButton: ["duplicate-id"],
  };

  const specialProcessing = {
    HintTooltip: hintTooltipProcessing,
    InfoPopover: infoPopoverProcessing,
    Message: messageProcessing,
    Modal: modalProcessing,
    Page: pageProcessing,
    AssignmentIcon: iconProcessing,
    UiIcon: iconProcessing,
    Logo: iconProcessing,
    Pennant: iconProcessing,
  };

  const specialUsageProcessing = {
    ClickableCardwithTooltip: clickableCardWithTooltipProcessing,
  };

  it("All", async function () {
    if (process.env.ONLYDOODAD == "default") {
      page = await browser.newPage();

      await page.emulateMediaFeatures([
        { name: "prefers-reduced-motion", value: "reduce" },
      ]);

      handlePageErrors(page);
      await page.goto(`http://localhost:${PORT}`, { waitUntil: "load" });
      await injectReducedMotion(page);
      await page.$("#maincontent");
      await percySnapshot(page, this.test.fullTitle());

      const results = await new AxePuppeteer(page)
        .disableRules([
          "aria-hidden-focus",
          "color-contrast",
          "duplicate-id-aria",
          "duplicate-id",
        ])
        .analyze();

      page.close();

      handleAxeResults("index view", results);
    }
  });

  it("Doodads", async function () {
    page = await browser.newPage();

    await page.emulateMediaFeatures([
      { name: "prefers-reduced-motion", value: "reduce" },
    ]);

    handlePageErrors(page);
    await page.goto(`http://localhost:${PORT}`);
    await injectReducedMotion(page);

    await page.$("#maincontent");
    let links = await page.evaluate(() => {
      let nodes = Array.from(
        document.querySelectorAll("[data-nri-description='doodad-link']"),
      );
      return nodes.map((node) => [node.text, node.href]);
    });

    await links.reduce((acc, [name, location]) => {
      return acc.then(() => {
        if (
          process.env.ONLYDOODAD == "default" ||
          process.env.ONLYDOODAD == name
        ) {
          console.log(`Testing ${name}`);
          let handler = specialProcessing[name] || defaultProcessing;
          return handler(name, location);
        }
      });
    }, Promise.resolve());

    page.close();
  });

  it("Usage examples", async function () {
    page = await browser.newPage();

    await page.emulateMediaFeatures([
      { name: "prefers-reduced-motion", value: "reduce" },
    ]);

    handlePageErrors(page);
    await page.goto(`http://localhost:${PORT}`);
    await injectReducedMotion(page);

    await page.$("#maincontent");

    const [usageTab] = await page.$$(
      "xpath/.//button[contains(., 'Usage Examples')]",
    );
    await usageTab.click();

    let links = await page.evaluate(() => {
      let nodes = Array.from(
        document.querySelectorAll(
          "[data-nri-description='usage-example-link']",
        ),
      );
      return nodes.map((node) => [node.text, node.href]);
    });

    await links.reduce((acc, [name, location]) => {
      return acc.then(() => {
        let testName = name.replaceAll(" ", "");
        if (
          process.env.ONLYDOODAD == "default" ||
          process.env.ONLYDOODAD == testName
        ) {
          console.log(`Testing Usage Example ${testName}`);
          let handler =
            specialUsageProcessing[testName] || defaultUsageExampleProcessing;
          return handler(testName, name, location);
        }
      });
    }, Promise.resolve());

    page.close();
  });
});
