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

## Examples

This repo contains an app showcasing all of these UI widgets.

To see them locally:

```
make styleguide-app/elm.js
```

Open `styleguide-app/index.html` in your browser.

Alternatively, you may use elm-reactor. Please be aware that you'll need to globally
install fonts (in particular, Muli) if you go this route.

## Tests

Run tests with

```
make test
```

### CI (Travis)

Travis will run `make ci` to verify everything looks good.
You can run this locally to catch errors before you push!

## Deploying

Once you are ready to publish `master` as a new version, run the following to bump the version in `elm-packages.json`:

```
elm-package bump
```

Commit to a branch and open a pull request. Announce the PR in #eng-front-ops-changes to have it reviewed and merged to `master`. Now you can publish the package!

```
elm-package publish # requires a tag, see below
```

This will require a tag, so follow the instructions it provides. It should be something like this:

```
Verifying NoRedInk/noredink-ui 5.10.0 ...
Version number 5.10.0 verified (MINOR change, 5.9.1 => 5.10.0)
Error: Libraries must be tagged in git, but tag 5.10.0 was not found.

These tags make it possible to find this specific version on GitHub.
To tag the most recent commit and push it to GitHub, run this:

    git tag -a 5.10.0 -m "release version 5.10.0"
    git push origin 5.10.0
```

You can also add a tag in https://github.com/NoRedInk/noredink-ui/releases/new if you want to add more detail.

Once you've published, you should see the latest version at the top of http://package.elm-lang.org/packages/NoRedInk/noredink-ui/latest.
