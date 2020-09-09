const PercyScript = require('@percy/script')

PercyScript.run(async (page, percySnapshot) => {
  const defaultProcessing = async (name, location) => {
    await page.goto(location)
    await page.waitFor(`#${name}`)
    await percySnapshot(name)
    console.log(`Snapshot complete for ${name}`)
  }
  const specialProcessing = {
    'Modal': async (name, location) => {
      await defaultProcessing(name, location)
      await page.click('#launch-modal')
      await page.waitFor('[role="dialog"]')
      await percySnapshot('Full Info Modal')
      await page.click('[aria-label="Close modal"]')
      await page.select('select', 'warning')
      await page.click('#launch-modal')
      await page.waitFor('[role="dialog"]')
      await percySnapshot('Full Warning Modal')
      await page.click('[aria-label="Close modal"]')
    }
  }
  await page.goto('http://localhost:8000')
  await page.waitFor('.module-example__doodad-link').then(doodads => {
    return doodads.$$('a').then(links => {
      return links.reduce((acc, link) => {
        return acc.then(_ => {
            return link.evaluate(node => [node["dataset"]["percyName"], node.href]).then(([name, location]) => {
                let handler = specialProcessing[name] || defaultProcessing
                return handler(name, location)
              }
            )
          }
        )
      }, Promise.resolve())
    })
  })
})
