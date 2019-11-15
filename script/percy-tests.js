const PercyScript = require('@percy/script')

PercyScript.run(async (page, percySnapshot) => {
  await page.goto('http://localhost:8000/')
  // ensure the page has loaded before capturing a snapshot
  await page.waitFor('#categories')

  await page.goto('http://localhost:8000/#category/Animations')
  await percySnapshot('Animations')

  await page.goto('http://localhost:8000/#category/Buttons')
  await percySnapshot('Buttons')

  await page.goto('http://localhost:8000/#category/Colors')
  await percySnapshot('Colors')

  await page.goto('http://localhost:8000/#category/Icons')
  await percySnapshot('Icons')

  await page.goto('http://localhost:8000/#category/Inputs')
  await percySnapshot('Inputs')

  await page.goto('http://localhost:8000/#category/Layout')
  await percySnapshot('Layout')

  await page.goto('http://localhost:8000/#category/Modals')
  await percySnapshot('Modals')
  await page.click('#launch-info-modal')
  await percySnapshot('Full Info Modal')
  await page.click('[aria-label="Close modal"]')
  await page.click('#launch-warning-modal')
  await percySnapshot('Full Warning Modal')
  await page.click('[aria-label="Close modal"]')

  await page.goto('http://localhost:8000/#category/Pages')
  await percySnapshot('Pages')

  await page.goto('http://localhost:8000/#category/Tables')
  await percySnapshot('Tables')

  await page.goto('http://localhost:8000/#category/Text')
  await percySnapshot('Text')

  await page.goto('http://localhost:8000/#category/Widgets')
  await percySnapshot('Widgets')

  await page.goto('http://localhost:8000/#category/Messaging')
  await percySnapshot('Messaging')
})
