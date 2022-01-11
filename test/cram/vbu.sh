#!/usr/bin/env sh

dotnet run --project "$TESTDIR/../../vbu.fsproj" -- -c config.json "$@"
