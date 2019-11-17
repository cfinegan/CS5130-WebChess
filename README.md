# chess

A [re-frame](https://github.com/Day8/re-frame) application designed to play chess in the browser.

## Development Mode

### Run application:

```
lein clean
lein dev
```

shadow-cljs will automatically push cljs changes to the browser.

Wait a bit, then browse to [http://localhost:8280](http://localhost:8280).

## Production Build

Coming Soon!

## TODO

- Add caching for valid moves so they're only calculated once per selection (low priority)
- replace the text/table board with a graphical one (probably want to still use an HTML table, but remove all the borders/padding and put the tile IMGs inside)
- Add style to the page (!!!)
- Finish debugging server logic
- Finish client logic for re-frame application
- Add selection of rules for re-frame application
