# Ownership, policies, & key concepts

NoRedInk’s accessibility team, the Accessibilibats, own `noredink-ui`/the shared component library.
While others may contribute to the component library and are encouraged to do so, the Accessibilibats (a.k.a A11ybats) are responsible for oversight of the foundational aspects of the component library, a.k.a. “**[Component Library Foundations](#component-library-foundations)**.”

Given this ownership and responsibility, A11ybats will provide guidance and support to developers and designers who are building new components or working with existing components.

[The shared component library app can be found here](https://noredink-ui.netlify.app/).

## Component Library Foundations
- Adherence to the component library [accessibility policy](#accessibility-policy)
- Adherence to [UI Style Guide (and Caveats)](https://paper.dropbox.com/doc/UI-Style-Guide-and-Caveats-PvOLxeX3oyujYEzdJx5pu) and [Guidelines for Sharing User-Facing Changes with Design](https://paper.dropbox.com/doc/Guidelines-for-Sharing-User-Facing-Changes-with-Design-bdKGQtYH9qO9I00hUkA6k) 
- Interoperability and consistency of components with each other and with the NoRedInk app
- Quality and consistency of API design
- Quality of internal code
- Existence and quality of code documentation for each component
- Existence, quality, and organization of component example pages in [the shared component library app](https://noredink-ui.netlify.app/)
- Test coverage and testability of each component and of [the shared component library app](https://noredink-ui.netlify.app/)

## Accessibility policy
- No new components will be added to the component library if they do not conform to [WCAG 2.1 AA accessibility guidelines](https://www.w3.org/WAI/standards-guidelines/wcag/). Similarly, no existing components will be modified such that the component falls out of conformance with these guidelines.
- For new components, UX designers & stakeholders are responsible for making their best faith effort to follow the [Accessibility Guidelines for Product Development](https://paper.dropbox.com/doc/PlcoE22OpOhB6eWCF4rFj?noDesktopRedirect=1) to include accessibility details in their spec and code. A11ybats will help fill in any gaps, but your team is responsible for the first pass.
- Existing components that do not conform to [WCAG 2.1 AA accessibility guidelines](https://www.w3.org/WAI/standards-guidelines/wcag/) are being updated by A11ybats to be conformant. (We believe we have a comprehensive backlog of updates to make, but feel free to ask us if you think you spot an accessibility issue. 🙏 )
- Components in the NoRedInk app which are NOT in the component library but which are shared or could be shared across multiple interfaces may be added to the component library over time by A11ybats or by any team, ideally with any necessary accessibility improvements, in accordance with the versioning policy elsewhere in this readme.

# Development guide

## Getting Started
1. Setup your [development environment](#developing-with-nix)
2. Run some [tests](#tests)
3. Check out [some examples](https://noredink-ui.netlify.app/)

## Developing with Nix

You can develop this package without installing anything globally by using Nix.
To get started, install nix from [nixos.org/nix](https://nixos.org/nix/).

After that's set up in your shell (just follow the instructions at the end of the installation script) you can run `nix-shell` to get a development environment with everything you need.

If you find that inconvenient, try using [`direnv`](https://direnv.net/).
Once that's set up, `echo use nix > .envrc` and then `direnv allow`.
Anytime you enter the project your shell will automatically pick up the right dependencies.

If you find that `direnv` loads too slow, [there are faster loading strategies than the default in their wiki](https://github.com/direnv/direnv/wiki/Nix).

### Working with upstream dependencies

We use `niv` to manage Nix dependencies.
It is automatically loaded in the Nix environment.

Here are some things you might need to do:

| Task | Command |
|------|---------|
| Add a non-npm, non-Elm dependency packaged with Nix | Look if it's in nixpkgs, or `niv add github.com/user/repo` |
| Update Nixpkgs | `niv update nixpkgs` |
| See all our dependencies | Look in `shell.nix` |
| See all our sources | `niv show` |

## Tests

Run tests with
- `shake test`
- `elm-test`

You can run the Puppeteer tests for only one component by passing the name of the component to the test script, for example: `./script/puppeteer-tests-no-percy.sh Button`

### CI (Travis)

Travis will run `shake ci` to verify everything looks good.
You can run this locally to catch errors before you push!

## Examples

This repo contains an app showcasing all of these UI widgets.

To see them locally:

```
script/develop.sh
```

And go to http://localhost:8000/

If you'd like to test your widget in the monolith before publishing, run `script/test-elm-package.py ../path_to_this_repo` from the monolith's directory.

## Deploying

Once your PR is merged, you can publish `master` as a new version:

Run the following to bump && publish the version in `elm.json`:

```
elm bump
```

If you get something like this:

```
-- PROBLEM LOADING DOCS --------------------------------------------------------

I need the docs for 12.17.0 to compute the next version number, so I fetched:

    https://package.elm-lang.org/packages/NoRedInk/noredink-ui/12.17.0/docs.json

I got the data back, but it was not what I was expecting. The response body
contains 195076 bytes. Here is the beginning:

    [{"name":"Nri.Ui","comment":" A collection of helpers for working with No...

Does this error keep showing up? Maybe there is something weird with your
internet connection. We have gotten reports that schools, businesses, airports,
etc. sometimes intercept requests and add things to the body or change its
contents entirely. Could that be the problem?
```

Then run it with 0.19.0 explicitly (0.19.1 has some problems with big docs):

```
npx -p elm@0.19.0-no-deps elm bump
```

Commit and push your changes in a PR. Once it's approved and merged,
then:

```
git tag -a 5.10.0 -m "release version 5.10.0"
git push origin 5.10.0
elm publish
```

You can also add a tag in https://github.com/NoRedInk/noredink-ui/releases/new if you want to add more detail.

Once you've published, you should see the latest version at <https://package.elm-lang.org/packages/NoRedInk/noredink-ui/>.

## Versioning policy

We try to avoid breaking changes and the associated major version bumps in this package. The reason for that is to avoid the following scenario:

```
  |
  x   4.6.0: Adding RadioButton widget
  |
  x   5.0.0: Breaking change in the TextArea widget
  |
  x   5.0.1: Styling fix in the Checkbox widget
  |
```

Suppose you just released version `5.0.1`, a small styling fix in the checkbox widget, for a story you're working on. If the project you're working in currently pulls in `noredink-ui` at version `4.x`, then getting to your styling fix means pulling in a new major version of `noredink-ui`. This breaks all `TextArea` widgets across the project, so those will need to be fixed before you can do anything else, potentially a big effort.

To prevent these big Yaks from suddenly showing up in seemingly trivial tasks we prefer to avoid breaking changes in the package. Instead when we need to make a breaking change in a widget, we create a new module for it `Nri.Ui.MyWidget.VX`. Similarly, when we build custom elements in JavaScript we create a file `lib/MyWidget/VX.js` and define a custom element `nri-mywidget-vX`.

That said, we may prune unused modules occasionally.

We should change this process if we feel it's not working for us!

## Moving Widgets to `noredink-ui`

If you are moving in a widget from the monolith:
- Copy the contents of `Nri.SomeModule` and its tests to `Nri.Ui.SomeModule.V1` in `noredink-ui`
- Publish!
- If you feel confident upgrading pre-existing usages of the widget, switch over to it everywhere!
- If the new version introduces big changes and you'd rather keep the old one around for now, rename `Nri.SomeModule` to `Nri.DEPRECATEDSomeModule` in the monolith and start using `Nri.Ui.SomeModule.V1` where you need it


## Phasing out old versions

Our goal is to gradually move to the newest version of each widget, and remove the old versions when they are no longer used.

This means:
  - We should avoid introducing new references to old versions of a widget
  - When touching code that uses a widget, prefer upgrading to the latest version
  - If you introduce a new version of a widget, please consider taking the time to upgrade all previous usages
    - If for some reason this isn't feasible, create a story in your team's backlog so that you can prioritize it separately without disrupting your current work
  - You can delete an old version of a widget when there are no usages left
    - Currently, `noredink-ui` is used by the monolith, CCS and tutorials
    - Note: this will be a major version bump, so you may want to batch deletions together
