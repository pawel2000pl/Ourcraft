unit ThreeDimensionalArrayOfBoolean;

{$mode objfpc}{$H+}

{TODO: remove this unit?}

interface

uses
  ThreeDimensionalArrayOfAnything;

type

  TThreeDimensionalArrayOfBoolean = specialize TThreeDimensionalArrayOfAnything<Boolean>;
  TThreeDimensionalSignedArrayOfBoolean = specialize TThreeDimensionalSignedArrayOfAnything<Boolean>;

implementation

end.
