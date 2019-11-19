const PercyScript = require('@percy/script')

PercyScript.run(async (page, percySnapshot) => {
  await page.goto('http://localhost:8000/')
  // ensure the page has loaded before capturing a snapshot
  await page.waitFor('#categories')

  await page.goto('http://localhost:8000/#category/Animations')
  await page.waitFor('.Animations')
  await percySnapshot('Animations')

  await page.goto('http://localhost:8000/#category/Buttons')
  await page.waitFor('.Buttons')
  await percySnapshot('Buttons')

  await page.goto('http://localhost:8000/#category/Colors')
  await page.waitFor('.Colors')
  await percySnapshot('Colors')

  await page.goto('http://localhost:8000/#category/Icons')
  await page.waitFor('.Icons')
  await percySnapshot('Icons')

  await page.goto('http://localhost:8000/#category/Inputs')
  await page.waitFor('.Inputs')
  await percySnapshot('Inputs')

  await page.goto('http://localhost:8000/#category/Layout')
  await page.waitFor('.Layout')
  await percySnapshot('Layout')

  await page.goto('http://localhost:8000/#category/Modals')
  await page.waitFor('.Modals')
  await percySnapshot('Modals')
  await page.click('#launch-info-modal')
  await percySnapshot('Full Info Modal')
  await page.click('[aria-label="Close modal"]')
  await page.click('#launch-warning-modal')
  await percySnapshot('Full Warning Modal')
  await page.click('[aria-label="Close modal"]')

  await page.goto('http://localhost:8000/#category/Pages')
  await page.waitFor('.Error')
  await percySnapshot('Pages')

  await page.goto('http://localhost:8000/#category/Tables')
  await page.waitFor('.Tables')
  await percySnapshot('Tables')

  await page.goto('http://localhost:8000/#category/Text')
  await page.waitFor('.Text')
  await percySnapshot('Text')

  await page.goto('http://localhost:8000/#category/Widgets')
  await page.waitFor('.Widgets')
  await percySnapshot('Widgets')

  await page.goto('http://localhost:8000/#category/Messaging')
  await page.waitFor('.Alerts')
  await percySnapshot('Messaging')
})
