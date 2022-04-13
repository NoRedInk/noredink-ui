describe('switch v1', () => {
  it('works', () => {
    cy.visit('/#/doodad/Switch')
    cy.percySnapshot()
    cy.injectAxe()
    assertInteractive()
    assertAlwaysOn()
    assertAlwaysOff()
  })

  function assertInteractive () {
    cy.checkA11y('[aria-controls=switch-interactive]')
    cy.get('[aria-controls=switch-interactive]')
      .should('have.attr', 'aria-checked', 'true')
      .should('contain.text', 'On')
      .click()
      .should('have.attr', 'aria-checked', 'false')
      .should('contain.text', 'Off')
      .click()
      .should('have.attr', 'aria-checked', 'true')
      .should('contain.text', 'On')
  }

  function assertAlwaysOn () {
    cy.checkA11y('[aria-controls=switch-disabled-on]')
    cy.get('[aria-controls=switch-disabled-on]')
      .should('have.attr', 'aria-checked', 'true')
      .should('contain.text', 'Permanently on')
      .click()
      .should('have.attr', 'aria-checked', 'true')
      .should('contain.text', 'Permanently on')
  }

  function assertAlwaysOff () {
    cy.checkA11y('[aria-controls=switch-disabled-off]')
    cy.get('[aria-controls=switch-disabled-off]')
      .should('have.attr', 'aria-checked', 'false')
      .should('contain.text', 'Permanently off')
      .click()
      .should('have.attr', 'aria-checked', 'false')
      .should('contain.text', 'Permanently off')
  }
})