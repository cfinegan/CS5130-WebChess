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

- Lobby System
- HTTP interface
  - Right now, we use WebSockets for everything, but we should only use them for game
	logic, and implement other systems (like joining a game, fetching the list of games)
	as a REST interface.
- Routing for the client.
  - We should be generating page content based on the URL whenever possible, so that
	the back/forward/refresh buttons behave as expacted, and so that players can share
	links to games.


## Technical Debt

- "Bad selection" animations are implemented outside of the React lifecycle
  - Currently this is done with a CSS animation and a JS `setTimeout` that sets the
	tile's style after the animation is finished.
	- This is prone to async bugs, and is currently causing an issue where the invalid
	selection animation cannot be played twice in a row for the same tile (because the
	app db doesn't change, re-frame doesn't re-draw the board for the second click).
	- [Reanimated](https://github.com/timothypratley/reanimated) might be a good candidate
	for fixing this, but we need to make sure we're not tanking performance by re-drawing
	board repeatedly!
- Clean up the chess engine code significantly
  - Pieces should *not* need IDs.
	- `valid-moves` shouldn't need the whole board history.
	- `valid-moves`, `check?`, etc., shouldn't need a reference to a `rules` object.
	- en-passant being an optional rule should be a special case if that's what we want.
	- clean up `valid-moves` to be more readable.