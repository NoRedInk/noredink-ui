const expect = require("expect");
const puppeteer = require("puppeteer");
const httpServer = require("http-server");
const percySnapshot = require("@percy/puppeteer");

const platform = require("os").platform();
// We need to change the args passed to puppeteer based on the platform they're using
const puppeteerArgs = /^win/.test(platform) ? [] : ["--single-process"];
const PORT = process.env.PORT_NUMBER || 8000;

const { AxePuppeteer } = require("@axe-core/puppeteer");
const assert = require("assert");

describe("UI tests", function () {
  let page;
  let server;
  let browser;

  before(async () => {
    server = httpServer.createServer({ root: `${__dirname}/../public` });
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
        `Expected no axe violations in ${name} but got ${violations.length} violations`
      );
    }
  };

  const defaultProcessing = async (name, location) => {
    await page.goto(location);
    await page.waitFor(`#${name.replace(".", "-")}`);
    await percySnapshot(page, name);

    const results = await new AxePuppeteer(page)
      .disableRules(skippedRules[name] || [])
      .analyze();
    handleAxeResults(name, results);
  };

  const iconProcessing = async (name, location) => {
    await page.goto(location);
    await page.waitFor(`#${name}`);
    await percySnapshot(page, name);

    // visible icon names snapshot
    await page.click("label");
    await page.waitForSelector(".checkbox-V5__Checked");
    await percySnapshot(page, `${name} - display icon names`);

    const results = await new AxePuppeteer(page)
      .disableRules(skippedRules[name] || [])
      .analyze();
    handleAxeResults(name, results);
  };

  const skippedRules = {
    // Loading's color contrast check seems to change behavior depending on whether Percy snapshots are taken or not
    Loading: ["color-contrast"],
    RadioButton: ["duplicate-id"],
  };

  const specialProcessing = {
    Modal: async (name, location) => {
      await page.goto(location);
      await page.waitFor(`#${name}`);
      await page.click("#launch-modal");
      await page.waitFor('[role="dialog"]');
      await percySnapshot(page, "Full Info Modal");

      const results = await new AxePuppeteer(page)
        .disableRules(skippedRules[name] || [])
        .analyze();
      handleAxeResults(name, results);

      await page.click('[aria-label="Close modal"]');
      await page.select("select", "warning");
      await page.click("#launch-modal");
      await page.waitFor('[role="dialog"]');
      await percySnapshot(page, "Full Warning Modal");
      await page.click('[aria-label="Close modal"]');
    },
    AssignmentIcon: iconProcessing,
    UiIcon: iconProcessing,
    Logo: iconProcessing,
    Pennant: iconProcessing,
  };

  it("All", async function () {
    page = await browser.newPage();
    await page.goto(`http://localhost:${PORT}`);
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
  });

  it("Doodads", async function () {
    page = await browser.newPage();
    await page.goto(`http://localhost:${PORT}`);

    await page.$("#maincontent");
    let links = await page.evaluate(() => {
      let nodes = Array.from(
        document.querySelectorAll("[data-nri-description='doodad-link']")
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
});
