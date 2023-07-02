|%
+$  id  @
+$  txt  @t
+$  action
  $%  [%add =id =txt]
      [%edit =id =txt]
      [%del =id]
  ==
+$  entry  [=id =txt]
+$  logged  (pair @ action)
+$  update
  %+  pair  @
  $%  action
      [%jrnl list=(list entry)]
      [%logs list=(list logged)]
  ==
+$  journal  ((mop id txt) gth)
+$  log  ((mop @ action) lth)
--
