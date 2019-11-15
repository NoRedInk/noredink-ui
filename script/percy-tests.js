const PercyScript = require('@percy/script')

PercyScript.run(async (page, percySnapshot) => {
  await page.goto('http://localhost:8000/');
  // ensure the page has loaded before capturing a snapshot
  await page.waitFor('#Nri-Ui-TextInput-Writing-onBlur-demonstration!');
  await percySnapshot('homepage');
})
