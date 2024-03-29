Please use the template that's relevant for your situation and delete the other templates. If this is just a noredink-ui repo doc change, you don't need to follow a template.

# :label: Bump for version `VERSION_NUMBER`

## What changes does this release include?

The easiest and most reliable way to find the changes that this release includes is to go through the changes between the last version number and master: `https://github.com/NoRedInk/noredink-ui/compare/VERSION_NUMBER...master`.

Include links to the relevant PRs in a list in this PR. Be sure to note any risky changes or changes that you will want to alert QA to when upgrading to the new version of noredink-ui.

Please update the PR's name to include a quick note about every linked PR.

## How has the API changed?

Please paste the output of `elm diff` run on latest master in the code block:

```

```

## Releasing

After this PR merges, and you've pulled down latest master, finish following the [publishing process](https://github.com/NoRedInk/noredink-ui/blob/master/README.md#publishing-a-new-version).

---

# :star2: Adding a new component

## About the component

What is the purpose of the new component? When and where will it be used? How will the reviewer know if this component meets the success criteria?

Please link to any relevant context and stories.

## :framed_picture: What does this change look like?

- If there are user facing changes, you will be asked later to add a designer as a reviewer. Please fill in this section by following [the guidelines for an optimal design review](https://paper.dropbox.com/doc/Guidelines-for-Sharing-User-Facing-Changes-with-Design--BpL8hpJLMugy6033aT5m0JdaAg-bdKGQtYH9qO9I00hUkA6k).
- If there are not user facing changes, include any screenshots or videos that would be helpful to reviewers.

## Component completion checklist

- [ ] I've gone through the relevant sections of the [Development Accessibility guide](https://paper.dropbox.com/doc/Accessibility-guide-4-Development--BiIVdijSaoijjOuhz3iTCJJ1Ag-rGoHpC91pFg3zTrYpvOCQ) with this component in mind
- [ ] Component has clear documentation
- [ ] Component is in the Component Catalog
  - [ ] I've used [the script to add the component to the Component Catalog](https://github.com/NoRedInk/noredink-ui?tab=readme-ov-file#-adding-a-component-to-the-component-catalog).
  - [ ] Component is categorized reasonably (see [Category](https://github.com/NoRedInk/noredink-ui/blob/master/component-catalog-app/Category.elm) for all the currently available categories). The component can be in multiple categories, if appropriate.
  - [ ] Component has a representative preview for the Component Catalog cards (bonus points for making it delightful!)
  - [ ] Component has a customizable example. Aim for having _every_ possible supported version of the component displayable through the configuration on this page. (Protip: This is handy for testing expected behavior!)
  - [ ] The customizable example produces sample code
  - [ ] The component's example page includes keyboard behavior, if any
  - [ ] The component's example page includes guidance around how to use the component, if necessary
- [ ] Component is tested
  - axe checks and percy snapshots will be automatically added for every component. However, if you want to customize _when_ the checks and snapshots are made, you will need to make changes to [script/puppeteer-tests.js](https://github.com/NoRedInk/noredink-ui/blob/master/script/puppeteer-tests.js).
  - there are 2 ways to add Elm tests:
    - if you want to test the Component Catalog example directly, add ProgramTest-style tests under `component-catalog/tests`
    - if you want to test the component directly, add tests under `tests/Spec`. Historically, this has been the more popular Elm testing strategy for noredink-ui.
- [ ] Component API follows standard patterns in noredink-ui
  - e.g., as a dev, I can conveniently add an `nriDescription`
  - and adding a new feature to the component will _not_ require major API changes to the component
- [ ] Please assign the following reviewers:
  - [ ] Someone from your team who can review your PR in full and review requirements from your team's perspective.
  - [ ] Component library owner - Someone from this group will review your PR for accessibility and adherence to component library foundations.
  - [ ] If there are user-facing changes, a designer. (You may want to direct your designer to the [deploy preview](https://github.com/NoRedInk/noredink-ui#reviews--preview-environments) for easy review):
    - For writing-related component changes, add Stacey (staceyadams)
    - For quiz engine-related components, add Ravi (ravi-morbia)
    - For a11y-related changes to general components, add Ben (bendansby)
    - For general component-related changes or if you’re not sure about something, add the Design group (NoRedInk/design)

---

# :wrench: Modifying a component

## Context

Please link to any relevant context and stories.

## :framed_picture: What does this change look like?

- If there are user facing changes, you will be asked later to add a designer as a reviewer. Please fill in this section by following [the guidelines for an optimal design review](https://paper.dropbox.com/doc/Guidelines-for-Sharing-User-Facing-Changes-with-Design--BpL8hpJLMugy6033aT5m0JdaAg-bdKGQtYH9qO9I00hUkA6k).
- If there are not user facing changes, include any screenshots or videos that would be helpful to reviewers.

## Component completion checklist

- [ ] I've gone through the relevant sections of the [Development Accessibility guide](https://paper.dropbox.com/doc/Accessibility-guide-4-Development--BiIVdijSaoijjOuhz3iTCJJ1Ag-rGoHpC91pFg3zTrYpvOCQ) with the changes I made to this component in mind
- [ ] Changes are clearly documented
  - [ ] Component docs include a changelog
  - [ ] Any new exposed functions or properties have docs
- [ ] Changes extend to the Component Catalog
  - [ ] The Component Catalog is updated to use the newest version, if appropriate
  - [ ] The Component Catalog example version number is updated, if appropriate
  - [ ] Any new customizations are available from the Component Catalog
  - [ ] The component example still has:
    - an accurate preview
    - valid sample code
    - correct keyboard behavior
    - correct and comprehensive guidance around how to use the component
- [ ] Changes to the component are tested/the new version of the component is tested
- [ ] Component API follows standard patterns in noredink-ui
  - e.g., as a dev, I can conveniently add an `nriDescription`
  - and adding a new feature to the component will _not_ require major API changes to the component
- [ ] If this is a new major version of the component, our team has stories created to upgrade all instances of the old component. Here are links to the stories:
  - add your story links here OR just write this is not a new major version
- [ ] Please assign the following reviewers:
  - [ ] Someone from your team who can review your PR in full and review requirements from your team's perspective.
  - [ ] Component library owner - Someone from this group will review your PR for accessibility and adherence to component library foundations.
  - [ ] If there are user-facing changes, a designer. (You may want to direct your designer to the [deploy preview](https://github.com/NoRedInk/noredink-ui#reviews--preview-environments) for easy review):
    - For writing-related component changes, add Stacey (staceyadams)
    - For quiz engine-related components, add Ravi (ravi-morbia)
    - For a11y-related changes to general components, add Ben (bendansby)
    - For general component-related changes or if you’re not sure about something, add the Design group (NoRedInk/design)
