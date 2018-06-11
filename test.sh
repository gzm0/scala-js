for i in `seq 1 10`; do
  rm test-suite/js/.2.12/target/scala-2.12/scalajs-test-suite-test-fastopt.js
  sbt testSuite2_12/test:fastOptJS
done
