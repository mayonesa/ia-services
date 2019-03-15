name := "ia-services"

version := "1.0-SNAPSHOT"

scalaVersion := "2.12.6"

libraryDependencies ++= Seq(guice,
                            ws,
                            "net.logstash.logback" % "logstash-logback-encoder" % "5.1",
                            "com.github.pureconfig" %% "pureconfig" % "0.9.1",
                            "org.scalatestplus.play" %% "scalatestplus-play" % "3.1.2" % "test",
                            "org.mockito" % "mockito-core" % "2.21.0" % "test")

lazy val root = project.in(file(".")).enablePlugins(PlayScala)