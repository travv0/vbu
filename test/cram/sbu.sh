#!/usr/bin/env sh

dotnet run --project "$TESTDIR/../../sbu.fsproj" -- -c config.json "$@"
