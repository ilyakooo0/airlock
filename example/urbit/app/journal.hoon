/-  *journal
/+  default-agent, dbug, agentio, *sink
|%
+$  versioned-state
    $%  state-0
    ==
+$  state-0  [%0 =journal =log]
+$  card  card:agent:gall
++  j-orm  ((on id txt) gth)
++  log-orm  ((on @ action) lth)
++  unique-time
  |=  [=time =log]
  ^-  @
  =/  unix-ms=@
    (unm:chrono:userlib time)
  |-
  ?.  (has:log-orm log unix-ms)
    unix-ms
  $(time (add unix-ms 1))
-- 

%-  agent:dbug
=/  state  *state-0
=/  snik  
  %+  sink  ~[/sync]  
  |=(stat=versioned-state (tap:j-orm journal.stat))
=/  sink  (snik state)
^-  agent:gall

|_  =bowl:gall
+*  this  .
    def   ~(. (default-agent this %|) bowl)
    io    ~(. agentio bowl)
++  on-init  on-init:def
++  on-save
  ^-  vase
  !>(state)
::
++  on-load
  |=  old-vase=vase
  ^-  (quip card _this)
  =/  state  !<(versioned-state old-vase)
  `this(state state, sink (snik state))
::
++  on-poke
  |=  [=mark =vase]
  ^-  (quip card _this)
  |^
  ?>  (team:title our.bowl src.bowl)
  ?.  ?=(%journal-action mark)  (on-poke:def mark vase)
  =/  now=@  (unique-time now.bowl log.state)
  =/  act  !<(action vase)
  =.  state  (poke-action act)
  =^  card  sink  (sync:sink state)
  :_  this(log.state (put:log-orm log.state now act))
  ~[(fact:io journal-update+!>(`update`[now act]) ~[/updates]) card]
  ::
  ++  poke-action
    |=  act=action
    ^-  _state
    ?-    -.act
        %add
      ?<  (has:j-orm journal.state id.act)
      state(journal (put:j-orm journal.state id.act txt.act))
    ::
        %edit
      ?>  (has:j-orm journal.state id.act)
      state(journal (put:j-orm journal.state id.act txt.act))
    ::
        %del
      ?>  (has:j-orm journal.state id.act)
      state(journal +:(del:j-orm journal.state id.act))
    ==
  --
::
++  on-watch
  |=  =path
  ^-  (quip card _this)
  ?>  (team:title our.bowl src.bowl)
  ?+  path  (on-watch:def path)
    [%updates ~]  `this
    [%sync ~]  [~[flush:sink] this]
  ==
::
++  on-peek
  |=  =path
  ^-  (unit (unit cage))
  ?>  (team:title our.bowl src.bowl)
  =/  now=@  (unm:chrono:userlib now.bowl)
  ?+    path  (on-peek:def path)
      [%x %entries *]
    ?+    t.t.path  (on-peek:def path)
        [%all ~]
      :^  ~  ~  %journal-update
      !>  ^-  update
      [now %jrnl (tap:j-orm journal.state)]
    ::
        [%before @ @ ~]
      =/  before=@  (rash i.t.t.t.path dem)
      =/  max=@  (rash i.t.t.t.t.path dem)
      :^  ~  ~  %journal-update
      !>  ^-  update
      [now %jrnl (tab:j-orm journal.state `before max)]
    ::
        [%between @ @ ~]
      =/  start=@
        =+  (rash i.t.t.t.path dem)
        ?:(=(0 -) - (sub - 1))
      =/  end=@  (add 1 (rash i.t.t.t.t.path dem))
      :^  ~  ~  %journal-update
      !>  ^-  update
      [now %jrnl (tap:j-orm (lot:j-orm journal.state `end `start))]
    ==
  ::
      [%x %updates *]
    ?+    t.t.path  (on-peek:def path)
        [%all ~]
      :^  ~  ~  %journal-update
      !>  ^-  update
      [now %logs (tap:log-orm log.state)]
    ::
        [%since @ ~]
      =/  since=@  (rash i.t.t.t.path dem)
      :^  ~  ~  %journal-update
      !>  ^-  update
      [now %logs (tap:log-orm (lot:log-orm log.state `since ~))]
    ==
  ==
::
++  on-leave  on-leave:def
++  on-agent  on-agent:def
++  on-arvo   on-arvo:def
++  on-fail   on-fail:def
--
