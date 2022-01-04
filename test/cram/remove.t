  $ cp "$TESTDIR/config.json" config.json

remove game
  $ echo n | "$TESTDIR/sbu.sh" remove test
  Are you sure you want to remove test? (y/N)  (no-eol)

  $ "$TESTDIR/sbu.sh" list
  another
  test

  $ echo y | "$TESTDIR/sbu.sh" remove test
  Are you sure you want to remove test? (y/N) Removed test

  $ "$TESTDIR/sbu.sh" list
  another

  $ "$TESTDIR/sbu.sh" remove another -y
  Removed another

  $ "$TESTDIR/sbu.sh" list
