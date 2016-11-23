Linux joystick events via a Kafka message broker
================================================


This package contains functions for passing [Linux joystick](https://www.kernel.org/doc/Documentation/input/joystick-api.txt) events to topics on a [Kafka message broker](https://kafka.apache.org/).


Clients
-------

The simple Kafka client that produces events from the joystick can be run, for example, as follows:

	cabal run kafka-device-joystick -- /dev/input/js0 joystick-client localhost 9092 events joystick

Also see https://hackage.haskell.org/package/kafka-device/.
