  $ cp "$TESTDIR/config.json" config.json
  $ cp -r "$TESTDIR/files" files

backup files
  $ "$TESTDIR/vbu.sh" add files -p files -g "**/*.txt" | sed -E -e 's/[^[:space:]]+cramtests-[^/]+/$TMP_DIR/'
  Group added successfully:
  
  Name: files
  Path: $TMP_DIR/backup.t/files
  Glob: **/*.txt
  


  $ "$TESTDIR/vbu.sh" config -p backups | sed -E -e 's/[^[:space:]]+cramtests-[^/]+/$TMP_DIR/'
  Backup path: ./.vbu-backups -> $TMP_DIR/backup.t/backups
  Backup frequency (in minutes): 15
  Number of backups to keep: 3
  

  $ "$TESTDIR/vbu-backup.sh"
  Warning: Path set for another doesn't exist: /another/path
  $TMP_DIR/backup.t/files/files/b.txt ==>
  \t$TMP_DIR/backup.t/backups/files/files/b.txt (esc)
  $TMP_DIR/backup.t/files/files/c.txt ==>
  \t$TMP_DIR/backup.t/backups/files/files/c.txt (esc)
  $TMP_DIR/backup.t/files/files/files/[0-9].txt ==>
  \t$TMP_DIR/backup.t/backups/files/files/files/[0-9].txt (esc)
  $TMP_DIR/backup.t/files/a.txt ==>
  \t$TMP_DIR/backup.t/backups/files/a.txt (esc)
  
  Finished backing up 4 files for files in $SECONDSs on $DATE_AND_TIME
  
  Warning: Path set for test doesn't exist: /test/group/path

check backup directory contents
  $ ls backups/files
  a.txt
  files

  $ ls backups/files/files
  b.txt
  c.txt
  files

  $ ls backups/files/files/files
  [0-9].txt

check versioning
  $ sleep 1 && touch files/files/b.txt && touch files/files/files/\[0-9\].txt

  $ "$TESTDIR/vbu-backup.sh"
  Warning: Path set for another doesn't exist: /another/path
  $TMP_DIR/backup.t/files/files/b.txt ==>
  \t$TMP_DIR/backup.t/backups/files/files/b.txt (esc)
  $TMP_DIR/backup.t/files/files/files/[0-9].txt ==>
  \t$TMP_DIR/backup.t/backups/files/files/files/[0-9].txt (esc)
  
  Finished backing up 2 files for files in $SECONDSs on $DATE_AND_TIME
  
  Warning: Path set for test doesn't exist: /test/group/path

  $ sleep 1 && touch files/files/b.txt && touch files/files/files/\[0-9\].txt

  $ "$TESTDIR/vbu-backup.sh"
  Warning: Path set for another doesn't exist: /another/path
  $TMP_DIR/backup.t/files/files/b.txt ==>
  \t$TMP_DIR/backup.t/backups/files/files/b.txt (esc)
  $TMP_DIR/backup.t/files/files/files/[0-9].txt ==>
  \t$TMP_DIR/backup.t/backups/files/files/files/[0-9].txt (esc)
  
  Finished backing up 2 files for files in $SECONDSs on $DATE_AND_TIME
  
  Warning: Path set for test doesn't exist: /test/group/path

  $ sleep 1 && touch files/a.txt && touch files/files/b.txt && touch files/files/c.txt && touch files/files/files/\[0-9\].txt

  $ "$TESTDIR/vbu-backup.sh"
  Warning: Path set for another doesn't exist: /another/path
  $TMP_DIR/backup.t/files/files/b.txt ==>
  \t$TMP_DIR/backup.t/backups/files/files/b.txt (esc)
  Note: Deleting $TMP_DIR/backup.t/backups/files/files/b.txt.bak.$TIMESTAMP
  $TMP_DIR/backup.t/files/files/c.txt ==>
  \t$TMP_DIR/backup.t/backups/files/files/c.txt (esc)
  $TMP_DIR/backup.t/files/files/files/[0-9].txt ==>
  \t$TMP_DIR/backup.t/backups/files/files/files/[0-9].txt (esc)
  Note: Deleting $TMP_DIR/backup.t/backups/files/files/files/[0-9].txt.bak.$TIMESTAMP
  $TMP_DIR/backup.t/files/a.txt ==>
  \t$TMP_DIR/backup.t/backups/files/a.txt (esc)
  
  Finished backing up 4 files for files in $SECONDSs on $DATE_AND_TIME
  
  Warning: Path set for test doesn't exist: /test/group/path

  $ ls backups/files/files | sed -E -e 's/bak\.[0-9_]+/bak.$TIMESTAMP/'
  b.txt
  b.txt.bak.$TIMESTAMP
  b.txt.bak.$TIMESTAMP
  c.txt
  c.txt.bak.$TIMESTAMP
  files

  $ ls backups/files/files/files | sed -E -e 's/bak\.[0-9_]+/bak.$TIMESTAMP/'
  [0-9].txt
  [0-9].txt.bak.$TIMESTAMP
  [0-9].txt.bak.$TIMESTAMP

keep all
  $ "$TESTDIR/vbu.sh" config --keep 0 | sed -E -e 's/[^[:space:]]+cramtests-[^/]+/$TMP_DIR/' 
  Backup path: $TMP_DIR/backup.t/backups
  Backup frequency (in minutes): 15
  Number of backups to keep: 3 -> 0
  

  $ sleep 1 && touch files/files/b.txt && touch files/files/files/\[0-9\].txt

  $ "$TESTDIR/vbu-backup.sh"
  Warning: Path set for another doesn't exist: /another/path
  $TMP_DIR/backup.t/files/files/b.txt ==>
  \t$TMP_DIR/backup.t/backups/files/files/b.txt (esc)
  $TMP_DIR/backup.t/files/files/files/[0-9].txt ==>
  \t$TMP_DIR/backup.t/backups/files/files/files/[0-9].txt (esc)
  
  Finished backing up 2 files for files in $SECONDSs on $DATE_AND_TIME
  
  Warning: Path set for test doesn't exist: /test/group/path

  $ ls backups/files/files | sed -E -e 's/bak\.[0-9_]+/bak.$TIMESTAMP/'
  b.txt
  b.txt.bak.$TIMESTAMP
  b.txt.bak.$TIMESTAMP
  b.txt.bak.$TIMESTAMP
  c.txt
  c.txt.bak.$TIMESTAMP
  files

  $ ls backups/files/files/files | sed -E -e 's/bak\.[0-9_]+/bak.$TIMESTAMP/'
  [0-9].txt
  [0-9].txt.bak.$TIMESTAMP
  [0-9].txt.bak.$TIMESTAMP
  [0-9].txt.bak.$TIMESTAMP
