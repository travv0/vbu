  $ cp "$TESTDIR/config.json" config.json

edit game
  $ "$TESTDIR/sbu.sh" edit test -p "/edited" -g none
  Name: test
  Save path: /test/game/path -> /edited
  Save glob: 
  

  $ "$TESTDIR/sbu.sh" edit test -p "/edited"
  Name: test
  Save path: /edited
  

  $ "$TESTDIR/sbu.sh" edit another -n new -p "/edited" -g ".*"
  Name: another -> new
  Save path: /another/path -> /edited
  Save glob: save* -> .*
  

  $ "$TESTDIR/sbu.sh" edit new -g ""
  Name: new
  Save path: /edited
  Save glob: .* -> 
  