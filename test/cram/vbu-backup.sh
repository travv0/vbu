#!/usr/bin/env sh

"$TESTDIR/vbu.sh" backup -v | sed -E -e 's/[0-9]+\.[0-9]+/$SECONDS/' -E -e 's/(Finished backing up [0-9]+ files? for files in \$SECONDSs on).*/\1 $DATE_AND_TIME/' -E -e 's/[^[:space:]]+cramtests-[^/]+/$TMP_DIR/' -E -e 's/bak\.[0-9_]+/bak.$TIMESTAMP/'