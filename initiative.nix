{ mkDerivation, aeson, array, base, case-insensitive, co-log
, hedgehog, hoauth2, hspec, hspec-core, hspec-discover
, hspec-hedgehog, http-client, http-conduit, http-types, lens, lib
, mtl, nonempty-containers, postgresql-simple, relude
, resource-pool, servant, servant-client, servant-server, text
, time, tomland, uri-bytestring, uuid, wai, wai-extra, warp, zlib
}:
mkDerivation {
  pname = "initiative";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array base case-insensitive co-log hedgehog hoauth2 hspec
    hspec-core http-client http-conduit http-types lens mtl
    nonempty-containers postgresql-simple relude resource-pool servant
    servant-client servant-server text time tomland uri-bytestring uuid
    wai wai-extra warp
  ];
  executableHaskellDepends = [
    base co-log hspec hspec-core mtl relude resource-pool time uuid
  ];
  executablePkgconfigDepends = [ zlib ];
  testHaskellDepends = [
    base hedgehog hspec hspec-core hspec-hedgehog mtl
    nonempty-containers relude
  ];
  testToolDepends = [ hspec-discover ];
  doHaddock = false;
  description = "Organizing TTRPG and other similar groups to find timeslots for games";
  license = lib.licenses.bsd3;
}
