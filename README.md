# noredink-ui

UI widgets we use.

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

We should change this process if we feel it's not working for us!

## Moving Widgets to `noredink-ui`

If you are moving in a widget from the monolith:
- Copy the contents of `Nri.SomeModule` and its tests to `Nri.Ui.SomeModule.V1` in `noredink-ui`
- Publish!
- If `Nri.Ui.SomeModule.V1` is a direct copy of `Nri.SomeModule`, switch over to it everywhere!
- If `Nri.Ui.SomeModule.V1` makes changes, rename `Nri.SomeModule` to `Nri.DEPRECATEDSomeModule` in the monolith and start using `Nri.Ui.SomeModule.V1` where you need it


## Phasing out old versions

Our goal is to gradually move to the newest version of each widget, and remove the old versions when they are no longer used.

Currently, `noredink-ui` is used by the monolith and by CCS. When neither of these repos use a version of a widget, it can be deleted. (Note: this will be a major version bump, so you may want to batch deletions together.)

When touching code that uses a widget, prefer upgrading to the latest version.


## Examples

This repo contains an app showcasing all of these UI widgets.

To see them locally:

```
script/develop.sh
```

If you'd like to test your widget in the monolith before publishing, run `script/test-elm-package.py ../path_to_this_repo` from the monolith's directory.

## Tests

Run tests with

```
make test
```

### CI (Travis)

Travis will run `make ci` to verify everything looks good.
You can run this locally to catch errors before you push!

## Deploying

Once your PR is merged, you can publish `master` as a new version:

Run the following to bump && publish the version in `elm.json`:

```
elm bump
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
