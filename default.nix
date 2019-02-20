{
  mkDerivation, stdenv
, aeson, base, binary, bytestring, cereal, kafka-device, yaml
}:

mkDerivation {
  pname = "kafka-device-joystick";
  version = "1.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base binary bytestring cereal kafka-device yaml
  ];
  executableHaskellDepends = [
  ];
  homepage = "https://bitbucket.org/functionally/kafka-device-joystick";
  description = "Linux joystick events via a Kafka message broker";
  license = stdenv.lib.licenses.mit;
}
