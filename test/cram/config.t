edit config
  $ "$TESTDIR/vbu.sh" config | sed -E -e "s#$HOME#\$HOME#"
  Creating new config file at `config.json'.
  Use the `config' command to update default values, which are:
  
  Backup path: $HOME/.vbu-backups
  Backup frequency (in minutes): 15
  Number of backups to keep: 20
  
  Backup path: $HOME/.vbu-backups
  Backup frequency (in minutes): 15
  Number of backups to keep: 20
  

  $ "$TESTDIR/vbu.sh" config -p /edited -f 5 -k 6 | sed -E -e "s#$HOME#\$HOME#"
  Backup path: $HOME/.vbu-backups -> /edited
  Backup frequency (in minutes): 15 -> 5
  Number of backups to keep: 20 -> 6
  

  $ "$TESTDIR/vbu.sh" config -f 15
  Backup path: /edited
  Backup frequency (in minutes): 5 -> 15
  Number of backups to keep: 6
  

  $ "$TESTDIR/vbu.sh" config
  Backup path: /edited
  Backup frequency (in minutes): 15
  Number of backups to keep: 6
  
