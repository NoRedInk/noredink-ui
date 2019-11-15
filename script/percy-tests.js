const PercyScript = require('@percy/script')

PercyScript.run(async (page, percySnapshot) => {
  await page.goto('https://noredink-ui.netlify.com/')//('http://localhost:8000/');
  // ensure the page has loaded before capturing a snapshot
  await page.waitFor('.module-example__Nri-Ui-Alert-V4');
  await percySnapshot('homepage');
})
