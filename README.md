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

### Start the chess server:

```
lein serv
```

### Compile the client app:

```
lein prod
```

### Test the client app:

Open `resources/public/index.html` in your browser. Each instance of this app will be a unique client for the server, so you can play against yourself if you want to test the functionality.


## TODO

- Add caching for valid moves so they're only calculated once per selection (low priority)
- replace the text/table board with a graphical one (probably want to still use an HTML table, but remove all the borders/padding and put the tile IMGs inside)
- Add style to the page (!!!)
- Finish debugging server logic
- Finish client logic for re-frame application
- Add selection of rules for re-frame application
