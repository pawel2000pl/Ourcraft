{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit OurcraftMain;

{$warn 5023 off : no warning about unused units}
interface

uses
  OurUtils, ProcessUtils, Freerer, Sorts, Models, CalcUtils, Chain, Locker, 
  Queues, WorldGenerator, SimpleCache, ArrayOfNumber, Collections, 
  CustomSaver, FileSaver, SaverPaths, LightTypes, ServerService, 
  ClientService, SocketCommands, NearestVectors, MainOurcraftDirectory, 
  TextureMode, OurGame, ThreeDimensionalArrayOfBoolean, UniversalImage, 
  DeterminedRandomGenerator, SimpleTypes, CollisionBoxes, ArchiveUtils, 
  DrawModes, PhisicalBox, PrefixSI, AsyncMicroTimer, AsyncMilliTimer, 
  Incrementations, synacode, synafpc, synaip, synautil, synsock, 
  DataAbstractions, TinyHashData, FastLZ77, ChunkLight, OurConstants, 
  LightCubes, JenkinsHash, threedimensionalarrayofanything, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('OurcraftMain', @Register);
end.
