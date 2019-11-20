const PercyScript = require('@percy/script')

PercyScript.run(async (page, percySnapshot) => {
  await page.goto('http://localhost:8000/#category/Animations')
  await page.waitFor('#animations')
  await percySnapshot('Animations')

  await page.goto('http://localhost:8000/#category/Buttons')
  await page.waitFor('#buttons-and-links')
  await percySnapshot('Buttons')

  await page.goto('http://localhost:8000/#category/Colors')
  await page.waitFor('#colors')
  await percySnapshot('Colors')

  await page.goto('http://localhost:8000/#category/Icons')
  await page.waitFor('#icons')
  await percySnapshot('Icons')

  await page.goto('http://localhost:8000/#category/Inputs')
  await page.waitFor('#inputs')
  await percySnapshot('Inputs')

  await page.goto('http://localhost:8000/#category/Layout')
  await page.waitFor('#layout')
  await percySnapshot('Layout')

  await page.goto('http://localhost:8000/#category/Modals')
  await page.waitFor('#modals')
  await percySnapshot('Modals')
  await page.click('#launch-info-modal')
  await page.waitFor('[role="dialog"]')
  await percySnapshot('Full Info Modal')
  await page.click('[aria-label="Close modal"]')
  await page.click('#launch-warning-modal')
  await page.waitFor('[role="dialog"]')
  await percySnapshot('Full Warning Modal')
  await page.click('[aria-label="Close modal"]')

  await page.goto('http://localhost:8000/#category/Pages')
  await page.waitFor('#error-pages')
  await percySnapshot('Pages')

  await page.goto('http://localhost:8000/#category/Tables')
  await page.waitFor('#tables')
  await percySnapshot('Tables')

  await page.goto('http://localhost:8000/#category/Text')
  await page.waitFor('#text-and-fonts')
  await percySnapshot('Text')

  await page.goto('http://localhost:8000/#category/Widgets')
  await page.waitFor('#widgets')
  await percySnapshot('Widgets')

  await page.goto('http://localhost:8000/#category/Messaging')
  await page.waitFor('#alerts-and-messages')
  await percySnapshot('Messaging')
})
