  $ cp "$TESTDIR/config.json" config.json

list info for all groups
  $ "$TESTDIR/vbu.sh" info
  Name: another
  Path: /another/path
  Glob: save*
  
  Name: test
  Path: /test/group/path
  

list info for selected groups
  $ "$TESTDIR/vbu.sh" info another
  Name: another
  Path: /another/path
  Glob: save*
  

  $ "$TESTDIR/vbu.sh" info another new
  Warning: No group named `new'
  
  Name: another
  Path: /another/path
  Glob: save*
  
