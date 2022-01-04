  $ cp "$TESTDIR/config.json" config.json
  $ cp -r "$TESTDIR/files" files

backup files
  $ "$TESTDIR/sbu.sh" add files -p files -g "**/*.txt" | sed -E -e 's/[^[:space:]]+cramtests-[^/]+/$TMP_DIR/'
  Game added successfully:
  
  Name: files
  Save path: $TMP_DIR/backup.t/files
  Save glob: **/*.txt
  


  $ "$TESTDIR/sbu.sh" config -p backups | sed -E -e 's/[^[:space:]]+cramtests-[^/]+/$TMP_DIR/'
  Backup path: ./.sbu-backups -> $TMP_DIR/backup.t/backups
  Backup frequency (in minutes): 15
  Number of backups to keep: 3
  

  $ "$TESTDIR/sbu.sh" backup -v | sed -E -e 's/[0-9]+\.[0-9]+/$SECONDS/' -E -e 's/(Finished backing up [0-9]+ files? for files in \$SECONDSs on).*/\1 $DATE_AND_TIME/' -E -e 's/[^[:space:]]+cramtests-[^/]+/$TMP_DIR/' | sort
  
  
  \t$TMP_DIR/backup.t/backups/files/a.txt (esc)
  \t$TMP_DIR/backup.t/backups/files/files/b.txt (esc)
  \t$TMP_DIR/backup.t/backups/files/files/c.txt (esc)
  $TMP_DIR/backup.t/files/a.txt ==>
  $TMP_DIR/backup.t/files/files/b.txt ==>
  $TMP_DIR/backup.t/files/files/c.txt ==>
  Finished backing up 3 files for files in $SECONDSs on $DATE_AND_TIME
  Warning: Path set for another doesn't exist: /another/path
  Warning: Path set for test doesn't exist: /test/game/path
