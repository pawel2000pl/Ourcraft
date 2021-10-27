unit OurConstants;

{$mode objfpc}{$H+}

interface

uses
    LightTypes;

const
  ChunkSizeLog2 = 4;
  WorldSizeLog2 = ChunkSizeLog2 + 1;
  ChunkSize = 1 shl ChunkSizeLog2;
  WorldSize = 1 shl WorldSizeLog2; //size of HashTable
  ChunkSizeMask = ChunkSize - 1;
  WorldSizeMask = WorldSize - 1;

  MAX_BLOCK_TRANSPARENCY = MAX_LIGHT_LEVEL;
  LIGHT_STEPS_UNTIL_BLACK = 15;
  LENGTH_LIGHT_RESISTANCE = ((1+MAX_LIGHT_LEVEL) div (1+LIGHT_STEPS_UNTIL_BLACK));
  ADD_LIGHT_DEFAULT_DEPTH = 2*(1+MAX_LIGHT_LEVEL) div LENGTH_LIGHT_RESISTANCE;

  MAX_TICK_DELTA_TIME = 1000;

  MaxBlockTransparency : TLight = (MAX_BLOCK_TRANSPARENCY, MAX_BLOCK_TRANSPARENCY, MAX_BLOCK_TRANSPARENCY);

implementation

end.

