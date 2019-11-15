const PercyScript = require('@percy/script')

PercyScript.run(async (page, percySnapshot) => {
  await page.goto('http://localhost:8000/')
  // ensure the page has loaded before capturing a snapshot
  await page.waitFor('.module-example__Nri-Ui-Alert-V4')

  const categories = Array.from(document.querySelectorAll('#categories a'))

  categories.forEach((category) => {
    await page.click(category)
    await percySnapshot(category.innerText)
  })
})
