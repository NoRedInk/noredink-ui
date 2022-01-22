const expect = require('expect');
const puppeteer = require('puppeteer');
const httpServer = require('http-server');
const percySnapshot = require('@percy/puppeteer');

const platform = require('os').platform();
// We need to change the args passed to puppeteer based on the platform they're using
const puppeteerArgs = /^win/.test(platform) ? [] : ['--single-process'];
const PORT = process.env.PORT_NUMBER || 8000;

describe('Visual tests', function () {
  this.timeout(30000);
  let page;
  let server;
  let browser;

  before(async () => {
    server = httpServer.createServer({ root: `${__dirname}/../public` });
    server.listen(PORT);

    browser = await puppeteer.launch({
      headless: true,
      timeout: 10000,
      args: puppeteerArgs
    });
  });

  after(() => {
    server.close();
  });

  const defaultProcessing = async (name, location) => {
    await page.goto(location)
    await page.waitFor(`#${name}`)
    await percySnapshot(page, name)
    console.log(`Snapshot complete for ${name}`)
  }

  const specialProcessing = {
    'Modal': async (name, location) => {
      await page.goto(location)
      await page.waitFor(`#${name}`)
      await page.click('#launch-modal')
      await page.waitFor('[role="dialog"]')
      await percySnapshot(page, 'Full Info Modal')
      await page.click('[aria-label="Close modal"]')
      await page.select('select', 'warning')
      await page.click('#launch-modal')
      await page.waitFor('[role="dialog"]')
      await percySnapshot(page, 'Full Warning Modal')
      await page.click('[aria-label="Close modal"]')
    },
    'Text.Writing': () => {}
  }

  it('All', async function () {
    page = await browser.newPage();
    await page.goto(`http://localhost:${PORT}`);
    await page.$('#maincontent');
    await percySnapshot(page, this.test.fullTitle());
    page.close();
  });

  it('Doodads', async function () {
    page = await browser.newPage();
    await page.goto(`http://localhost:${PORT}`);

    await page.$('#maincontent');
    let links = await page.evaluate(() => {
      let nodes = Array.from(document.querySelectorAll("[data-nri-description='doodad-link']"));
      return nodes.map(node => [node.text, node.href])
    })

    await links.reduce((acc, [name, location]) => {
      return acc.then(() => {
          let handler = specialProcessing[name] || defaultProcessing
          return handler(name, location)
        }
      )
    }, Promise.resolve())
  })
});