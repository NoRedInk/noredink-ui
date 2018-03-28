# noredink-ui

UI widgets we use.

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
