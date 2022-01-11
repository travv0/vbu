  $ cp "$TESTDIR/config.json" config.json

remove group
  $ echo n | "$TESTDIR/vbu.sh" remove test
  Are you sure you want to remove test? (y/N)  (no-eol)

  $ "$TESTDIR/vbu.sh" list
  another
  test

  $ echo y | "$TESTDIR/vbu.sh" remove test
  Are you sure you want to remove test? (y/N) Removed test

  $ "$TESTDIR/vbu.sh" list
  another

  $ "$TESTDIR/vbu.sh" remove another -y
  Removed another

  $ "$TESTDIR/vbu.sh" list
