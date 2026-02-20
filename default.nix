{ mkDerivation, aeson, base, case-insensitive, co-log, http-types
, lens, lib, mtl, postgresql-simple, relude, resource-pool, servant
, servant-client, servant-server, text, time, tomland, wai
, wai-extra, warp, zlib
}:
mkDerivation {
  pname = "initiative";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base case-insensitive co-log http-types lens mtl
    postgresql-simple relude resource-pool servant servant-client
    servant-server text time tomland wai wai-extra warp
  ];
  executableHaskellDepends = [ base relude ];
  executablePkgconfigDepends = [ zlib ];
  testHaskellDepends = [ base relude ];
  description = "Organizing TTRPG and other similar groups to find timeslots for games";
  license = lib.licenses.bsd3;
  mainProgram = "initiative";
}
