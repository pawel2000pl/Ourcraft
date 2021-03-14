unit TextureMode;

{$mode objfpc}{$H+}

interface

uses
  math, CalcUtils;

type
  //tmNorth: +x tmSouth: -x tmUp: +y tmDown: -y tmEast: +z tmWest: -z See: TextureModeSides
  TTextureMode = (tmNorth, tmSouth, tmUp, tmDown, tmEast, tmWest);
  TTextureDrawSides = set of TTextureMode;  
  TRectangleCorners = array[0..3] of TVector3;
  TCubeCorners = array[TTextureMode] of TRectangleCorners;

  TTexture2d = array[axisX..axisY] of Single;

  TTextureCorners = array[0..3] of TTexture2d;//from 0 to 1 - kolejność krawędzi
  TCubeTextureCorners = array[TTextureMode] of TTextureCorners;

const
  TextureStandardModeCoord : TCubeCorners = (
    ((1, 1, 1), (1, 1, 0), (1, 0, 0), (1, 0, 1)),
    ((0, 1, 0), (0, 1, 1), (0, 0, 1), (0, 0, 0)),
    ((1, 1, 0), (1, 1, 1), (0, 1, 1), (0, 1, 0)),
    ((1, 0, 1), (1, 0, 0), (0, 0, 0), (0, 0, 1)),
    ((0, 1, 1), (1, 1, 1), (1, 0, 1), (0, 0, 1)),
    ((1, 1, 0), (0, 1, 0), (0, 0, 0), (1, 0, 0)));

  AllTextureSides = [tmNorth, tmSouth, tmUp, tmDown, tmEast, tmWest];

  TextureStandardCorners : TTextureCorners = ((0, 0), (1, 0), (1, 1), (0, 1));

  OppositeSide : array[TTextureMode] of TTextureMode =
    (tmSouth, tmNorth, tmDown, tmUp, tmWest, tmEast);


  //TextureModeSides: Double and Integer
  TextureModeSidesD : array[TTextureMode] of TVector3 =
    ((1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1));
  TextureModeSidesI : array[TTextureMode] of TIntVector3 =
    ((1, 0, 0), (-1, 0, 0), (0, 1, 0), (0, -1, 0), (0, 0, 1), (0, 0, -1));

  TextureModeSidesAxis : array[TTextureMode] of record
    Axis : TAxis;
    Offset : Integer;
    end =
    ((Axis: AxisX; Offset:1), (Axis: AxisX; Offset:-1), (Axis: AxisY; Offset:1), (Axis: AxisY; Offset:-1), (Axis: AxisZ; Offset:1), (Axis: AxisZ; Offset:-1));

  DrawedSides : array[TValueSign, TValueSign, TValueSign] of set of TTextureMode = (
  (([tmNorth, tmUp, tmEast], [tmNorth, tmUp, tmEast, tmWest], [tmNorth, tmUp, tmWest]),
  ([tmNorth, tmUp, tmDown, tmEast], [tmNorth, tmUp, tmDown, tmEast, tmWest], [tmNorth, tmUp, tmDown, tmWest]),
  ([tmNorth, tmDown, tmEast], [tmNorth, tmDown, tmEast, tmWest], [tmNorth, tmDown, tmWest])),

  (([tmNorth, tmSouth, tmUp, tmEast], [tmNorth, tmSouth, tmUp, tmEast, tmWest], [tmNorth, tmSouth, tmUp, tmWest]),
  ([tmNorth, tmSouth, tmUp, tmDown, tmEast], [tmNorth, tmSouth, tmUp, tmDown, tmEast, tmWest], [tmNorth, tmSouth, tmUp, tmDown, tmWest]),
  ([tmNorth, tmSouth, tmDown, tmEast], [tmNorth, tmSouth, tmDown, tmEast, tmWest], [tmNorth, tmSouth, tmDown, tmWest])),

  (([tmSouth, tmUp, tmEast], [tmSouth, tmUp, tmEast, tmWest], [tmSouth, tmUp, tmWest]),
  ([tmSouth, tmUp, tmDown, tmEast], [tmSouth, tmUp, tmDown, tmEast, tmWest], [tmSouth, tmUp, tmDown, tmWest]),
  ([tmSouth, tmDown, tmEast], [tmSouth, tmDown, tmEast, tmWest], [tmSouth, tmDown, tmWest]))
  );

implementation

end.

