{
  Copyright (C) 2020 Paweł Bielecki pawelek24@op.pl / pbielecki2000@gmail.com

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
}

unit Incrementations;

{$mode objfpc}
{$Macro on}

interface

{$define Ordinal_Type := uint64}
{$include incrementation_headers.inc}
{$define Ordinal_Type := uint32}
{$include incrementation_headers.inc}
{$define Ordinal_Type := uint16}
{$include incrementation_headers.inc}
{$define Ordinal_Type := uint8}
{$include incrementation_headers.inc}

{$define Ordinal_Type := int64}
{$include incrementation_headers.inc}
{$define Ordinal_Type := int32}
{$include incrementation_headers.inc}
{$define Ordinal_Type := int16}
{$include incrementation_headers.inc}
{$define Ordinal_Type := int8}
{$include incrementation_headers.inc}

{$UnDef Ordinal_Type}

implementation

  {$define Ordinal_Type := uint64}
  {$include incrementations.inc}
  {$define Ordinal_Type := uint32}
  {$include incrementations.inc}
  {$define Ordinal_Type := uint16}
  {$include incrementations.inc}
  {$define Ordinal_Type := uint8}
  {$include incrementations.inc}

  {$define Ordinal_Type := int64}
  {$include incrementations.inc}
  {$define Ordinal_Type := int32}
  {$include incrementations.inc}
  {$define Ordinal_Type := int16}
  {$include incrementations.inc}
  {$define Ordinal_Type := int8}
  {$include incrementations.inc}
  {$UnDef Ordinal_Type}

end.
