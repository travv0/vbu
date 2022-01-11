  $ cp "$TESTDIR/config.json" config.json

add groups to config
  $ "$TESTDIR/vbu.sh" add new -p "/new" -g "*"
  Group added successfully:
  
  Name: new
  Path: /new
  Glob: *
  
  $ "$TESTDIR/vbu.sh" add asdf -p "/asdf"
  Group added successfully:
  
  Name: asdf
  Path: /asdf
  