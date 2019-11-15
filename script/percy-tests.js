const PercyScript = require('@percy/script')

PercyScript.run(async (page, percySnapshot) => {
  await page.goto('http://localhost:8000/')
  // ensure the page has loaded before capturing a snapshot
  await page.waitFor('.module-example__Nri-Ui-Alert-V4')

  await page.evaluate ((percySnapshot) => {
    let categories = Array.from(document.querySelectorAll('#categories a'))
    for (let category of categories)
      category.click()
      percySnapshot(category.innerText)
  })
})
