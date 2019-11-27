const PercyScript = require('@percy/script')

async function openLink(target) {
  return target.click();
}

PercyScript.run(async (page, percySnapshot) => {
  const defaultProcessing = async (name, id, location) => {
    await page.goto(location)
    await page.waitFor(`#${id}`)
    await percySnapshot(name)
    console.log(`Snapshot complete for ${name}`)
  }
  const specialProcessing = {
    'All': async (_) => {
    },
    'Modals': async (name, id, location) => {
      await defaultProcessing(name, id, location)
      await page.click('#launch-info-modal')
      await page.waitFor('[role="dialog"]')
      await percySnapshot('Full Info Modal')
      await page.click('[aria-label="Close modal"]')
      await page.click('#launch-warning-modal')
      await page.waitFor('[role="dialog"]')
      await percySnapshot('Full Warning Modal')
      await page.click('[aria-label="Close modal"]')
    }
  }
  await page.goto('http://localhost:8000')
  await page.waitFor('#categories').then(category => {
    return category.$$('a').then(links => {
      return links.reduce((acc, link) => {
        return acc.then(_ => {
              return link.evaluate(node => [node.innerText, node.innerText.toLowerCase().replace(/ /g, "-"), node.href]).then(([name, id, location]) => {
                let handler = specialProcessing[name] || defaultProcessing
                return handler(name, id, location)
              })
            }
        )
      }, Promise.resolve())
    })
  })
})
