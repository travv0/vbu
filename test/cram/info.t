  $ cp "$TESTDIR/config.json" config.json

list info for all games
  $ "$TESTDIR/sbu.sh" info
  Name: another
  Save path: /another/path
  Save glob: save*
  
  Name: test
  Save path: /test/game/path
  

list info for selected games
  $ "$TESTDIR/sbu.sh" info another
  Name: another
  Save path: /another/path
  Save glob: save*
  

  $ "$TESTDIR/sbu.sh" info another new
  Warning: No game named `new'
  
  Name: another
  Save path: /another/path
  Save glob: save*
  
