  $ cp "$TESTDIR/config.json" config.json

edit group
  $ "$TESTDIR/vbu.sh" edit test -p "/edited" -g none
  Name: test
  Path: /test/group/path -> /edited
  Glob: 
  

  $ "$TESTDIR/vbu.sh" edit test -p "/edited"
  Name: test
  Path: /edited
  

  $ "$TESTDIR/vbu.sh" edit another -n new -p "/edited" -g ".*"
  Name: another -> new
  Path: /another/path -> /edited
  Glob: save* -> .*
  

  $ "$TESTDIR/vbu.sh" edit new -g ""
  Name: new
  Path: /edited
  Glob: .* -> 
  