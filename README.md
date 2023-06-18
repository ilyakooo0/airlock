# Airlock

![](res/airlock.jpeg)

## Usage

### FFI

Urbit subscriptions require [server-side event (SSE)](https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events).
Unfortunately, Elm does not provide a native implementation.
This means that we will need to use [JS FFI (aka ports)](https://guide.elm-lang.org/interop/ports.html).

#### Setting up the Elm side

You would need to declare two ports for Airlock to use:

1. `port createEventSource : String -> Cmd msg`

	 This sets up the SSE channel.
2. `port onEventSourceMessage : (JD.Value -> msg) -> Sub msg`
	 This recieves the actual messages from the SSE channel.

These two functions need to be passed to one of the functions from `Ur.Run` module. 
The functions in `Ur.Run` are the same as functions from `Browser`, but have a couple of extra urbit-specific fields.

#### Setting up the JS side

The SSE JS code is neatly packed into [example/fetch-event-source.js](example/fetch-event-source.js). 
All you need to do is pass them to Elm ports like in [example/script.js](example/script.js).

You can see a full working example in [example](example).

