unit SocketCommands;

{$mode objfpc}{$H+}

interface

const

  //////////////////////From Client to Server//////////////////////

  ///ask for anything: PathSize : Integer; Path : array of AnsiString;
  scAsk = 'Ask';

  ///ask server if file exists and could be downloaded
  scExists = 'Exists';

  ///try set block (response from server will be scLoad)
  scSetBlock = 'SetBlock';

  ////////////////////From Server to Client  //////////////////////

  ///next data will be data of chunk:
  ///PathSize : Integer; Path : array of AnsiString; Chunk : TStream;
  scLoad = 'Load';

  ///responses for scExists
  scTrue = 'True';
  scFalse = 'False';


implementation

end.

