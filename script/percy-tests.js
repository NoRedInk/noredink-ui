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
    'All': async (_) => {},
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
      }, Promise.resolve(    ))
    })
  })

  /*

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

   */
}, {headless: false})
