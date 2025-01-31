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
    await page.waitForSelector(`#${name}`);
    await percySnapshot(page, name);

    // visible icon names snapshot
    await page.click("label");
    await page.waitForSelector("[aria-checked=true]");
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

  const skippedRules = {
    Block: ["scrollable-region-focusable"],
    // Loading's color contrast check seems to change behavior depending on whether Percy snapshots are taken or not
    Loading: ["color-contrast"],
    Outline: ["color-contrast"],
    RadioButton: ["duplicate-id"],
  };

  const specialProcessing = {
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
