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

unit W3DJsonX2.Conv;

{$IFDEF W3DCLIENT}
  {$DEFINE JSX_NOVAR}
{$ENDIF}

interface
uses
    W3DJsonX2.Types
  , W3DJsonX2.Obj
  , W3DJsonX2.RTTI
  {$IFNDEF JSX_NOVAR}
  , Variants
  {$ENDIF}
  , SysUtils
  , RTTI
  , System.Generics.Collections
  , Classes
  ;

type

  TIStringListConv = class(TInterfacedObject, IJX2Converter)
    function  OnSerialize(AData: TJX2DataBlock): string;
    function  OnDeserialize(AData: TJX2DataBlock) : TObject;
    function  OnClone(AData: TJX2DataBlock): TObject;
    procedure OnDestroy(AData: TJX2DataBlock);
  end;

  TIJX2ValueListConv = class(TInterfacedObject, IJX2Converter)
    function  OnSerialize(AData: TJX2DataBlock): string;
    function  OnDeserialize(AData: TJX2DataBlock) : TObject;
    function  OnClone(AData: TJX2DataBlock): TObject;
    procedure OnDestroy(AData: TJX2DataBlock);
  end;

  {$IFNDEF JSX_NOVAR}
  TIJX2VariantListConv = class(TInterfacedObject, IJX2Converter)
    function  OnSerialize(AData: TJX2DataBlock): string;
    function  OnDeserialize(AData: TJX2DataBlock) : TObject;
    function  OnClone(AData: TJX2DataBlock): TObject;
    procedure OnDestroy(AData: TJX2DataBlock);
  end;
  {$ENDIF}

  TIJX2ObjectListConv = class(TInterfacedObject, IJX2Converter)
    function  OnSerialize(AData: TJX2DataBlock): string;
    function  OnDeserialize(AData: TJX2DataBlock) : TObject;
    function  OnClone(AData: TJX2DataBlock): TObject;
    procedure OnDestroy(AData: TJX2DataBlock);
  end;

  {$IFNDEF JSX_NOVAR}
  function JsonToTVariant(AJValues: TJsonDataValueHelper): Variant;
  function VariantToJSONValue(AVariant: Variant; AForcedString: Boolean = False): string;
  {$ENDIF}
  function JsonToTValue(AJValues: TJsonDataValueHelper): TValue;
  function ValueToStr(aValue: TValue; var ASuccess: Boolean): string;
  function EscapeJSONStr(AStr: string): string;
  function StrToJSONValue(AString: string): string;
  function ValueIsString(AValue: TValue): Boolean;
  function ValueToJSONValue(AValue: TValue; AForcedString: Boolean = False): string;

 (*
  TIStringListConv = class(TInterfacedObject, IJX2Converter)
    function ToJson(AData: TJX2ToJsonDataBlock): string;
    function FromJson(AData: TJX2ToJsonDataBlock) : TObject;
    function Clone(AData: TJX2ToJsonDataBlock): TObject;
  end;

  {$IFNDEF JSX_NOVAR}
  TIJX2VariantListConv = class(TInterfacedObject, IJX2Converter)
    function ToJson(AData: TJX2ToJsonDataBlock): string;
    function FromJson(AData: TJX2ToJsonDataBlock) : TObject;
    function Clone(AData: TJX2ToJsonDataBlock): TObject;
  end;
  {$ENDIF}

  TIJX2ValueListConv = class(TInterfacedObject, IJX2Converter)
    function ToJson(AData: TJX2ToJsonDataBlock): string;
    function FromJson(AData: TJX2ToJsonDataBlock) : TObject;
    function Clone(AData: TJX2ToJsonDataBlock): TObject;
  end;

;

  TIJX2StrValueDicConv = class(TInterfacedObject, IJX2Converter)
    function ToJson(AData: TJX2ToJsonDataBlock): string;
    function FromJson(AData: TJX2ToJsonDataBlock) : TObject;
    function Clone(AData: TJX2ToJsonDataBlock): TObject;
  end;
*)
implementation
uses W3DJsonX2;

{$REGION 'Convert. Routines'}

function JsonToTValue(AJValues: TJsonDataValueHelper): TValue;
begin
  Result := nil;
  case AJValues.Typ of
    jdtNone:;
    jdtString: Result := AJValues.Value;
    jdtInt: Result := AJValues.IntValue;
    jdtLong: Result := AJValues.LongValue;
    jdtULong: Result := AJValues.ULongValue;
    jdtFloat: Result := AJValues.FloatValue;
    jdtDateTime: Result :=AJValues.DateTimeValue;
    jdtUtcDateTime: Result := AJValues.UtcDateTimeValue;
    jdtBool: Result := AJValues.BoolValue;
    jdtArray: raise Exception.Create('JsonTypeToValue cannot convert to Array');
    jdtObject: raise Exception.Create('JsonTypeToValue cannot convert to Objects');
  end;
end;

{$IFNDEF JSX_NOVAR}
function JsonToTVariant(AJValues: TJsonDataValueHelper): Variant; // TJsonDataValueHelper
begin
  Result := null;
  case AJValues.Typ of
    jdtNone:;
    jdtString: Result := AJValues.Value;
    jdtInt: Result := AJValues.IntValue;
    jdtLong: Result := AJValues.LongValue;
    jdtULong: Result := AJValues.ULongValue;
    jdtFloat: Result := AJValues.FloatValue;
    jdtDateTime: Result :=AJValues.DateTimeValue;
    jdtUtcDateTime: Result := AJValues.UtcDateTimeValue;
    jdtBool: Result := AJValues.BoolValue;
    jdtArray: raise Exception.Create('JsonTypeToValue cannot convert to Array');
    jdtObject: raise Exception.Create('JsonTypeToValue cannot convert to Objects');
  end;
end;
{$ENDIF}

function ValueToStr(aValue: TValue; var ASuccess: Boolean): string;
begin
  ASuccess := True;
  Result := '';
  case AValue.kind of
    tkUnknown: Result :='null';
    tkInteger: Result := IntToStr(AValue.AsInteger);
    tkChar: Result := AValue.asString;
    tkEnumeration: Result := '';
    tkFloat: Result := FloatToStr(AValue.AsExtended);
    tkString: Result := AValue.asString;
    tkSet: ASuccess := False;
    tkClass: ASuccess := False;
    tkMethod: ASuccess := False;
    tkWChar: Result := AValue.asString;
    tkLString: Result := AValue.asString;
    tkWString: Result := AValue.asString;
{$IFNDEF JSX_NOVAR}
    tkVariant: Result := VarToStr(AValue.AsVariant);
{$ENDIF}
    tkArray: ASuccess := False;
    tkRecord: ASuccess := False;
    tkInterface: ASuccess := False;
    tkInt64: Result := IntToStr(AValue.AsInt64);
    tkDynArray: ASuccess := False;
    tkUString:  Result := AValue.asString;
    tkClassRef: ASuccess := False;
    tkPointer: ASuccess := False;
    tkProcedure: ASuccess := False;
    tkMRecord: ASuccess := False;
  end;
end;

{$IFNDEF JSX_NOVAR}
function VariantToJSONValue(AVariant: Variant; AForcedString: Boolean = False): string;
begin
  if VarIsStr(AVariant) or AForcedString then
    Result := StrToJSONValue(AVariant)
  else
    Result := VarToStr(AVariant);
end;
{$ENDIF}


function EscapeJSONStr(AStr: string): string;
var
  LP: PChar;
  LEndP: PChar;
begin
  Result := '';
  if Astr.IsEmpty then
    exit;
  LP := PChar(Pointer(AStr));
  LEndP := LP + PInteger(@PByte(AStr)[-4])^;
  while LP < LendP do
  begin
    case LP^ of
      #0 .. #31, '\', '"':
          Result := Result + '\' + LP^;
      else
        Result := Result + LP^;
    end;
    Inc(LP);
  end;
end;

function StrToJSONValue(AString: string): string;
begin
  Result := '"' + EscapeJSONStr(AString) + '"';
end;

function ValueIsString(AValue: TValue): Boolean;
begin
  Result := (AValue.Kind in [tkChar, tkString, tkWChar, tkLString, tkWString, tkUString]);
end;

function ValueToJSONValue(AValue: TValue; AForcedString: Boolean = False): string;
var
  LSuccess: Boolean;
begin
  if AForcedString or ValueIsString(AValue) then
    Result := StrToJSONValue(ValueToStr(AValue, LSuccess))
  else
    Result := ValueToStr(AValue, LSuccess);
end;

{$ENDREGION 'Convert. Routines'}


(*
function TIStringListConv.Clone(ASelfObj: TObject): TObject;
begin
  Result := TStringList.Create;
  for var LStr in TStringList(ASelfObj) do
    TStringList(Result).Add(LStr);
end;

function TIStringListConv.FromJson(Field: TRTTIField; JsonObject: PJsonDataValue): TObject;
var
  LIdx: string;
begin
  Result := TStringList.Create;
  for LIdx in JsonObject.ArrayValue do
    TStringList(Result).Add(LIdx);
end;

function TIStringListConv.ToJson(ASelfObj: TObject): string;
var
  LStr: string;
  LArr: TJSONArray;
begin
  LArr := (TJSONArray).Create;
  for LStr in TStringList(ASelfObj) do LArr.Add(LStr);
  Result := LArr.ToJSON();
  LArr.Free;
end;

function TIJX2StrValueDicConv.Clone(ASelfObj: TObject): TObject;
var
  Lkv: TPair<string, TValue>;
begin
  Result := TDictionary<string,TValue>.Create;
  for Lkv in TDictionary<string, TValue>(ASelfObj) do
    TDictionary<string, TValue>(Result).Add(Lkv.Key, Lkv.Value);
end;

function TIJX2StrValueDicConv.FromJson(Field: TRTTIField; JsonObject: PJsonDataValue): TObject;
var
  LIdx: Integer;
begin
  Result := TJX2StrValueDic.Create;
  for LIdx := 0 to JsonObject.ObjectValue.count - 1 do
    TJX2StrValueDic(Result).Add(
        JsonObject.ObjectValue.Names[LIdx]
      , JsonTypeToTValue(JsonObject.ObjectValue.Values[JsonObject.ObjectValue.Names[LIdx]])
    );
end;

function TIJX2StrValueDicConv.ToJson(ASelfObj: TObject): string;
var
  LStrValueObj : TDictionary<string, TValue>;
  LSL: TStringList;
  LStrValue: TPair<string, TValue>;
begin
  LStrValueObj := TDictionary<string, TValue>(ASelfObj);
  LSL := TStringList.Create(#0, ',', [soStrictDelimiter]);
  LSL.Capacity := LStrValueObj.count;
  for LStrValue in LStrValueObj do
      LSL.Add(StrToJSONValue(LStrValue.Key) + ':' + ValueToJSONValue(LStrValue.Value));
  Result := '{' + LSL.DelimitedText + '}';
  LSL.Free;
end;

{$IFNDEF JSX_NOVAR}

{$REGION 'TIJX2VariantListConv' }

function TIJX2VariantListConv.Clone(ASelfObj: TObject): TObject;
var
  LValue: Variant;
begin
  try
    Result := TList<Variant>.Create;
    For LValue in TList<Variant>(ASelfObj) do
      TList<Variant>(Result).Add(LValue);
  except
    Result := nil;
  end;
end;

function TIJX2VariantListConv.FromJson(Field: TRTTIField; JsonObject: PJsonDataValue): TObject;
var
  LNewVarList: TList<Variant>;
  LIdx: Integer;
begin
  try
    Result := TList<Variant>.Create;
    TList<Variant>(Result).Capacity := JsonObject.ArrayValue.Count;
    for LIdx := 0 to JsonObject.ArrayValue.count - 1 do
      TList<Variant>(Result).Add(JsonObject.ArrayValue.V[LIdx]);
  except
    Result := nil;
  end;
end;

function TIJX2VariantListConv.ToJson(ASelfObj: TObject): string;
var
  LSL: TStringList;
  LIdx: Variant;
begin
  LSL := nil;
  try
  try
    LSL := TStringList.Create(#0, ',', [soStrictDelimiter]);
    LSL.Capacity := TList<Variant>(ASelfObj).Count;
    for LIdx in  TList<Variant>(ASelfObj) do
      LSL.Add(VariantToJSONValue(LIdx));
    Result := '[' + LSL.DelimitedText + ']';
  except
    Result := '';
  end;
  finally
    LSL.Free;
  end;
end;

{$ENDREGION 'TIJX2VariantListConv' }

{$ENDIF}

{$REGION 'TIJX2ValueListConv' }

function TIJX2ValueListConv.Clone(ASelfObj: TObject): TObject;
var
  LValue: TValue;
begin
  try
    Result := TList<TValue>.Create;
    For LValue in TList<TValue>(ASelfObj) do
      TList<TValue>(Result).Add(LValue);
  except
    Result := nil;
  end;
end;

function TIJX2ValueListConv.FromJson(Field: TRTTIField; JsonObject: PJsonDataValue): TObject;
var
  LNewVarList: TList<TValue>;
  LIdx: Integer;
begin
  try
    Result := TList<TValue>.Create;
    TList<TValue>(Result).Capacity := JsonObject.ArrayValue.Count;
    for LIdx := 0 to JsonObject.ArrayValue.count - 1 do
      TList<TValue>(Result).Add(JsonTypeToTValue(JsonObject.ArrayValue[LIdx]));
  except
    Result := nil;
  end;
end;

function TIJX2ValueListConv.ToJson(ASelfObj: TObject): string;
var
  LSL: TStringList;
  LIdx: TValue;
begin
  LSL := nil;
  try
  try
    LSL := TStringList.Create(#0, ',', [soStrictDelimiter]);
    LSL.Capacity := TList<Variant>(ASelfObj).Count;
    for LIdx in TList<TValue>(ASelfObj) do
      LSL.Add(ValueToJSONValue(LIdx));
    Result := '[' + LSL.DelimitedText + ']';
  except
    Result := '';
  end;
  finally
    LSL.Free;
  end;
end;

{$ENDREGION 'TIJX2ValueListConv' }
 *)


{$REGION 'TIJX2VariantListConv' }

{$IFNDEF JSX_NOVAR}

function TIJX2VariantListConv.OnClone(AData: TJX2DataBlock): TObject;
var
  LValue: Variant;
begin
  try
    Result := TList<Variant>.Create;
    For LValue in TList<Variant>(AData.SelfObj) do
      TList<Variant>(Result).Add(LValue);
  except
    Result := nil;
  end;
end;

function TIJX2VariantListConv.OnSerialize(AData: TJX2DataBlock): string;
var
  LSL: TStringList;
  LIdx: Variant;
begin
  LSL := nil;
  try
  try
    LSL := TStringList.Create(#0, ',', [soStrictDelimiter]);
    LSL.Capacity := TList<Variant>(AData.SelfObj).Count;
    for LIdx in TList<Variant>(AData.SelfObj) do
      LSL.Add(VariantToJSONValue(LIdx));
    Result := '[' + LSL.DelimitedText + ']';
  except
    Result := '';
  end;
  finally
    LSL.Free;
  end;
end;

function TIJX2VariantListConv.OnDeserialize(AData: TJX2DataBlock): TObject;
var
  LIdx: Integer;
begin
  Result := TList<Variant>.Create;
  for LIdx := 0 to AData.JsonVal.ArrayValue.count - 1 do
    TList<Variant>(Result).Add(
      AData.JsonVal.ArrayValue[LIdx].VariantValue
    );
end;

procedure TIJX2VariantListConv.OnDestroy(AData: TJX2DataBlock);
begin
end;

{$ENDIF}

{$ENDREGION 'TIJX2VariantListConv' }

{$REGION 'TIJX2ValueList' }

function TIJX2ValueListConv.OnClone(AData: TJX2DataBlock): TObject;
var
  LValue: TValue;
begin
  try
    Result := TList<TValue>.Create;
    For LValue in TList<TValue>(AData.SelfObj) do
      TList<TValue>(Result).Add(LValue);
  except
    Result := nil;
  end;
end;

function TIJX2ValueListConv.OnSerialize(AData: TJX2DataBlock): string;
var
  LSL: TStringList;
  LIdx: TValue;
begin
  LSL := nil;
  try
  try
    LSL := TStringList.Create(#0, ',', [soStrictDelimiter]);
    LSL.Capacity := TList<TValue>(AData.SelfObj).Count;
    for LIdx in TList<TValue>(AData.SelfObj) do
      LSL.Add(ValueToJSONValue(LIdx));
    Result := '[' + LSL.DelimitedText + ']';
  except
    Result := '';
  end;
  finally
    LSL.Free;
  end;
end;

function TIJX2ValueListConv.OnDeserialize(AData: TJX2DataBlock): TObject;
var
  LIdx: Integer;
begin
  Result := TList<TValue>.Create;
  for LIdx := 0 to AData.JsonVal.ArrayValue.count - 1 do
    TList<TValue>(Result).Add(
      JsonToTValue(AData.JsonVal.ArrayValue[LIdx])
    );
end;

procedure TIJX2ValueListConv.OnDestroy(AData: TJX2DataBlock);
begin
end;

{$ENDREGION 'TIJX2ValueList' }

{$REGION 'TIJX2ObjectListConv' }

function TIJX2ObjectListConv.OnClone(AData: TJX2DataBlock): TObject;
var
  Lo: TObject;
begin
  Result := TObjectList<TObject>.Create(True);
  for Lo in TObjectList<TObject>(AData.SelfObj) do
  begin
    TObjectList<TObject>(Result).Add(
      W3DJX2.Deserialize<TObjectList<TObject>>(W3DJX2.Serialize(TJX2(Lo).Clone))
    );
  end;
end;

function TIJX2ObjectListConv.OnDeserialize(AData: TJX2DataBlock): TObject;
var
  LAttr: JX2AttrConv;
  LNewChildObj: TObject;
begin
  Result := TObjectList<TObject>.Create(True);
  TObjectList<TObject>(Result).Capacity := AData.JsonVal.ArrayValue.Count;
  LAttr := JX2AttrConv(GetFieldAttribute(AData.Field, JX2AttrConv));
  for var LIdx := 0 to AData.JsonVal.ArrayValue.Count - 1 do
  begin
    LNewChildObj := LAttr.FClass.Create;
    W3DJX2.Deserialize(LNewChildObj, AData.JsonVal.ArrayValue.O[LIdx], AData.Settings);
    TObjectList<TObject>(Result).Add(LNewChildObj);
  end;
end;

function TIJX2ObjectListConv.OnSerialize(AData: TJX2DataBlock): string;
begin
  var LObjListClass := TObjectList<TObject>(AData.SelfObj);
  var LSL  := TStringList.Create(#0, ',', [soStrictDelimiter]);
  LSL.Capacity := LObjListClass.count;
  var LJsonObj := TJsonObject(TJsonObject.NewInstance);
  for var LObjLoopClass in LObjListClass do
  begin
    LJsonObj.Clear;
    W3DJX2.Serialize(LObjLoopClass, LJsonObj, AData.Patcher, AData.Settings);
    LSL.Add(LJsonObj.ToJSON(True));
  end;
  LJsonObj.Free;
  Result := '[' + LSL.DelimitedText + ']';
  LSL.Free;
end;

procedure TIJX2ObjectListConv.OnDestroy(AData: TJX2DataBlock);
begin
end;

{$ENDREGION 'TIJX2ObjectListConv' }


{ TIStringListConv }

function TIStringListConv.OnClone(AData: TJX2DataBlock): TObject;
begin
  Result := TStringList.Create;
  for var LStr in TStringList(AData.SelfObj) do
    TStringList(Result).Add(LStr);
end;

function TIStringListConv.OnDeserialize(AData: TJX2DataBlock): TObject;
var
  LIdx: string;
begin
  Result := TStringList.Create;
  for LIdx in AData.JsonVal.ArrayValue do
    TStringList(Result).Add(LIdx);
end;

procedure TIStringListConv.OnDestroy(AData: TJX2DataBlock);
begin

end;

function TIStringListConv.OnSerialize(AData: TJX2DataBlock): string;
var
  LStr: string;
  LArr: TJSONArray;
begin
  LArr := (TJSONArray).Create;
  for LStr in TStringList(AData.SelfObj) do LArr.Add(LStr);
  Result := LArr.ToJSON();
  LArr.Free
end;

end.

