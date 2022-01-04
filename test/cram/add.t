  $ cp "$TESTDIR/config.json" config.json

add games to config
  $ "$TESTDIR/sbu.sh" add new -p "/new" -g "*"
  Game added successfully:
  
  Name: new
  Save path: /new
  Save glob: *
  
  $ "$TESTDIR/sbu.sh" add asdf -p "/asdf"
  Game added successfully:
  
  Name: asdf
  Save path: /asdf
  