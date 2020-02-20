#!/bin/bash

# Check if the Scala build tool is present
if ! type sbt > /dev/null; then
  echo 'sbt is not intalled on this system' >&2
  exit 0
fi

# Generate the executable archive (fat jar)
sbt assembly

cat src/stub.sh target/scala-*/Canevas-assembly*.jar > ./canevas
chmod +x ./canevas