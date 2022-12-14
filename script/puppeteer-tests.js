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

  const goTo = async (name, location) => {
    await page.goto(location, { waitUntil: "load" });
    await page.waitForXPath(
      `//h1[contains(., 'Nri.Ui.${name}') and @aria-current='page']`,
      200
    );
  };

  const defaultProcessing = async (name, location) => {
    await goTo(name, location);
    await percySnapshot(page, name);

    const results = await new AxePuppeteer(page)
      .disableRules(skippedRules[name] || [])
      .analyze();
    handleAxeResults(name, results);
  };

  const forAllOptions = async (labelName, callback) => {
    await page.waitForXPath(
      `//label[contains(., '${labelName}')]//select`,
      200
    );
    const [select] = await page.$x(
      `//label[contains(., '${labelName}')]//select`
    );
    const options = await select.$x(
      `//label[contains(., '${labelName}')]//option`
    );
    // Actually doing the select was super flakey.
    // Temporarily just using the first element to get
    // CI consistent again. We can cover all the cases separately...
    const optionEl = options[0];
    const option = await page.evaluate((el) => el.innerText, optionEl);
    await page.select("select", option);
    await callback(option);
  };

  const messageProcessing = async (name, location) => {
    await goTo(name, location);
    await percySnapshot(page, name);

    var axe = await new AxePuppeteer(page)
      .disableRules(skippedRules[name] || [])
      .analyze();
    handleAxeResults(name, axe);

    const [theme] = await page.$x("//label[contains(., 'theme')]");
    await theme.click();

    await forAllOptions("theme", async (option) => {
      await percySnapshot(page, `${name} - ${option}`);
      axe = await new AxePuppeteer(page)
        .withRules(["color-contrast"])
        .analyze();
      handleAxeResults(`${name} - ${option}`, axe);
    });
  };

  const modalProcessing = async (name, location) => {
    await goTo(name, location);

    await page.click("#launch-modal");
    await page.waitForSelector('[role="dialog"]');
    await percySnapshot(page, `${name} - info`);

    axe = await new AxePuppeteer(page).analyze();

    await page.click('[aria-label="Close modal"]');

    handleAxeResults(`${name} - info`, axe);
  };

  const pageProcessing = async (name, location) => {
    await goTo(name, location);

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
    await page.waitForSelector(".checkbox-V7__Checked");
    await percySnapshot(page, `${name} - display icon names`);

    const results = await new AxePuppeteer(page)
      .disableRules(skippedRules[name] || [])
      .analyze();
    handleAxeResults(name, results);
  };

  const skippedRules = {
    // See https://github.com/dequelabs/axe-core/issues/3649 -- we may be able to remove the Highlighter, Mark, and Block skipped rule
    Highlighter: ["aria-roledescription"],
    Block: ["aria-roledescription"],
    QuestionBox: ["aria-roledescription"],
    // Loading's color contrast check seems to change behavior depending on whether Percy snapshots are taken or not
    Loading: ["color-contrast"],
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
