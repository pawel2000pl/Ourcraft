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
  TextureMode, OurGame, UniversalImage, DeterminedRandomGenerator, 
  SimpleTypes, CollisionBoxes, ArchiveUtils, DrawModes, PhisicalBox, PrefixSI, 
  AsyncMicroTimer, AsyncMilliTimer, synacode, synafpc, synaip, synautil, 
  synsock, DataAbstractions, TinyHashData, FastLZ77, ChunkLight, OurConstants, 
  LightCubes, JenkinsHash, ThreeDimensionalArrayOfAnything, WavefrontModels, 
  BitStream, HuffmanTree, PostPreOperations, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('OurcraftMain', @Register);
end.
