(*****************************************************************************
The MIT License (MIT)

Copyright (c) 2020-2024 Laurent Meyer JsonX2@ea4d.com

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*****************************************************************************)

unit JsonX2.Conv;

{$IFDEF W3DCLIENT}
  {$DEFINE JSX_NOVAR}
{$ENDIF}

interface
uses
    JsonX2.Types
  ;

type

  TIStringListConv = class(TInterfacedObject, IJX2Converter)
    function  OnClone(AData: TJX2DataBlock): TObject;
    function  OnSerialize(AData: TJX2DataBlock): string;
    function  OnDeserialize(AData: TJX2DataBlock) : TObject;
    procedure OnDestroy(AData: TJX2DataBlock);
  end;

implementation
uses
    Classes
  , JsonX2.Obj
  ;

{$REGION 'Convert. Routines'}


{ TIStringListConv }

function TIStringListConv.OnClone(AData: TJX2DataBlock): TObject;
begin
  Result := TStringList.Create;
  for var LStr in TStringList(AData.SelfObj) do
    TStringList(Result).Add(LStr);
end;

function TIStringListConv.OnSerialize(AData: TJX2DataBlock): string;
var
  LArr: TJSonArray;
  LStr: string;
begin
  LArr := TJSonArray.Create;
  for LStr in TStringList(aData.SelfObj) do LArr.Add(LStr);
  Result := LArr.ToJSON();
  LArr.Free;
end;

function TIStringListConv.OnDeserialize(AData: TJX2DataBlock): TObject;
var
  LStr: string;
begin
  Result := TStringList.Create;
  for LStr in AData.JsonVal.ArrayValue do TStringList(Result).Add(LStr);
end;

procedure TIStringListConv.OnDestroy(AData: TJX2DataBlock);
begin
end;


end.

