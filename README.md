# chess

A [re-frame](https://github.com/Day8/re-frame) application designed to play chess in the browser.

## Development Mode

### Run application:

Run `lein serv` in one terminal (to start the server) and then `lein dev` to start shadow-cljs.

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

Open `resources/public/index.html` in your browser.


## TODO

- Make the lobby use HTTP rather than WebSocket.
- Change the lobby to display open games, rather than do matchmaking.
- Nail down what rules we want to be configurable when creating a match.
- Add routing to both the server & client.
- Add server & client logic for synchronizing game state between the opponents.
- Better graphics for the game board.
- Better styling overall.