# %sink

%sink is a state replication system between your %gall agent and your Elm airlock.

%sink allows you to avoid having to manually write subscriptions which send semantic diffs between your %gall agent and the frontend.

## How it works

%sink consists of two parts:
1. The %sink library you import into your %gall agent and call appropriate functions at the right points in your agent lifecycle. More on this in a bit.
2. The `Ur.Sub.sink` urbit subscription which reconciles the state and hands you the latest version of %gall agent state that you can then `Deconstruct` into Elm data structures.

The whole system works by saving the previous version your %gall agent state and diffing the raw nouns that compose your old state and your new state.
On first initialization of the %sink system the whole state you supplied is sent to the frontend. Any subsequent changes are sent as a diff between your old state and your new state.

## How to use it

### The %gall agent part

#### Copying library files

In order to use %sink you have to copy the following two files into your agent desk into the `lib` directory:
1. [example/urbit/lib/noun-diff.hoon](../example/urbit/lib/noun-diff.hoon) -- contains the algorithm for diffing raw nouns.
2. [example/urbit/lib/sink.hoon](../example/urbit/lib/sink.hoon) -- contains the logic of syncing the state between your agent and your frontend.

#### Adding sync points into your agent

First you need to import the %sink library into your agent:

```hoon
/+  *sink
```

Next you need to initialize the sink. Put the following declaration somewhere before your agent door:

```hoon
::
:: A deferred expression of the state you want to sync to the frontend.
=*  entries  (tap:j-orm journal.stat)
::
:: Replace /sync with whatever path you want to use for syncing your state.
=/  snik  (sink ~[/sync])
::
:: Next you initialize your sink with your initial agent state.
=/  sink  (snik entries)
```

You can have multiple sinks in the same application to sync different parts of your state on different paths.

Don't forget to reinitialize your `sink` when your restore your agent state in the `++on-load` arm:

```hoon
++  on-load
  |=  old-vase=vase
  ^-  (quip card _this)
  =/  state  !<(versioned-state old-vase)
  ::
  :: the `sink (snik entries)` is the important bit.
  `this(state state, sink (snik entries))
```
  
Lastly, you need to send sink updates whenever you change your state. Most likely this will be in your `++on-poke` arm:

```hoon
::
:: This line generates a `card` that you need to pass to arvo and updates 
:: the `sink` to reference the latest state of your agent.
=^  card  sink  (sync:sink entries)
```

You can look at the [journal.hoon](../example/urbit/app/journal.hoon) for a full example.

#### Adding sink the the frontend

For your frontend to recieve %sink updates you need to pass the result of calling `Ur.Sub.sink` to the `urbitSubscriptions` field of `Ur.Run.application` or similar function from `Ur.Run`:

```elm
main : Ur.Run.Program Model Msg
main =
    Ur.Run.application
      {
        -- ...
          urbitSubscriptions =
        -- ...
            Ur.Sub.sink
              { ship = ship
              , app = "journal"
              , path = [ "sync" ]
              , deconstructor =
                  D.list (D.cell D.bigint D.cord |> D.map (\a b -> ( a, b )))
                      |> D.map GotListings
              }
        -- ...
      }
```

In the `deconstructor` field you specify a `Deconstructor` the deconstructs _the whole_ state that is being synced form the %gall agent.

You do not have to deal with diffs. It is handled automatically

You can look at [example/src/Sink.elm](../example/src/Sink.elm) for a full example.
