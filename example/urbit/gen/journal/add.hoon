/-  *journal, *sole
/+  *generators
:-  %ask
|=  [[now=@da * bek=beak] *]
^-  (sole-product (cask action))
=|  acc=wain
=/  off=tape
  =/  blit
    =+  [our=(scot %p p.bek) now=(scot %da p.r.bek)]
    .^(blit:dill %dx /[our]//[now]/sessions//line) 
  ?.  ?=(%lin -.blit)  !!
  %+  scan  (tufa p.blit)
  ;~  sfix
    %+  cook
    |=(t=tape (reap (add 2 (lent t)) ' '))
    (star ;~(less gar next))
    (star next)
  ==
|^
%+  print  leaf+"{off}-------------------------------------\0a"
%+  print  leaf+"{off}|                                   |"
%+  print  leaf+"{off}| (\\\\\\ to submit, <BKSP> to cancel) |"
%+  print  leaf+"{off}|         New Journal Entry         |"
%+  print  leaf+"{off}|                                   |"
%+  print  leaf+"\0a{off}-------------------------------------"
line
++  line
  %+  prompt   [%& %prompt ""]
  |=  t=tape
  ?:  =(t "\\\\\\")
    %+  produce  %journal-action
    :+  %add
      (unm:chrono:userlib now)
    (of-wain:format (flop acc))
  %+  print  leaf+"{off}{t}"
  line(acc [(crip t) acc])
--
