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

unit W3DJsonX2;

{$IFDEF W3DCLIENT}
  {$DEFINE JSX_NOVAR}
{$ENDIF}

interface

uses
    Classes
    , RTTI
    , System.Generics.Collections
    , System.TypInfo
    , W3DJsonX2.Obj
    , W3DJsonX2.Types
    , W3DJsonX2.Conv
    , W3DJsonX2.Patch
    ;

const
  BStrL: array[Boolean] of string = ('false','true');
  BStrK: array[Boolean] of string = ('False','True');
  BStrU: array[Boolean] of string = ('FALSE','TRUE');

type

  TJsonX2 = class(TObject)
  public
    class var FInstance: TJsonX2;
    constructor Create; overload;
    destructor Destroy; override;

    function  Beautifier(const AJsonStr : string; Compact: Boolean = False): string;

    procedure Serialize(AObj: TObject; AJsonObj: TJsonObject; AJsonPatcher: TJX2Patcher; ASettings: TJX2Settings); overload;
    function  Serialize(Obj: TObject; ASettings: TJX2Settings = []): string; overload;
    function  Serialize(Intf: IInterface; ASettings: TJX2Settings = []): string; overload;
    procedure Deserialize(AObj: TObject; AJsonObj: TJsonObject; ASettings: TJX2Settings); overload;
    function  Deserialize<T: class, constructor>(const AJsonStr: string; ASettings: TJX2Settings = []): T; overload;
    function  Deserialize(AIntfClass: TClass; const AJsonStr: string; ASettings: TJX2Settings = []): IJX2; overload;
  end;

  function  JsonToTValue(AJValues: TJsonDataValueHelper): TValue;
  procedure SetJsonTValue(APJson: PJsonDataValue; AValue: TValue);
  procedure AddTValueToJsonArray(AJsonArr: TJsonArray; AValue: TValue);
{$IFNDEF JSX_NOVAR}
  procedure SetJsonVariant(APJson: PJsonDataValue; AValue: Variant);
  function  TValueToVariant(AValue: TValue): Variant;
{$ENDIF}

var
  W3DJX2 : TJsonX2;

implementation
uses
    DateUtils
  , SysUtils
{$IFNDEF JSX_NOVAR}
  , Variants
{$ENDIF}
  , W3DJsonX2.Utils
  , W3DJsonX2.RTTI
  ;

{$REGION 'Utils'}

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
procedure SetJsonVariant(APJson: PJsonDataValue; AValue: Variant);
begin
  case TVarData(AValue).VType of
    varEmpty    : APJson.ObjectValue := nil;
    varNull     : APJson.ObjectValue := nil;
    varSmallint : APJson.IntValue := AValue;
    varInteger  : APJson.IntValue := AValue;
    varSingle   : APJson.FloatValue := AValue;
    varDouble   : APJson.FloatValue := AValue;
    varCurrency : APJson.FloatValue := AValue;
    varDate     : APJson.FloatValue := AValue;
    varOleStr   : APJson.Value := AValue;
    varDispatch : APJson.ObjectValue := nil;
    varError    : APJson.ObjectValue := nil;
    varBoolean  : APJson.BoolValue := AValue;
    varVariant  : APJson.VariantValue := AValue;
    varUnknown  : APJson.ObjectValue := nil;
    varShortInt : APJson.IntValue := AValue;
    varByte     : APJson.IntValue := AValue;
    varWord     : APJson.IntValue := AValue;
    varUInt32   : APJson.ULongValue := AValue.AsUInt32;
    varInt64    : APJson.LongValue := AValue;
    varUInt64   : APJson.ULongValue := AValue;
    varRecord   : APJson.ObjectValue := nil;
    varStrArg   : APJson.Value := AValue.AsStrArg;
    varObject   : APJson.ObjectValue := nil;
    varUStrArg  : APJson.Value := AValue;
    varString   : APJson.Value := AValue;
    varAny      : APJson.Value := AValue;
    varUString  : APJson.Value := AValue;
  else
    APJson.ObjectValue := nil;
  end;
end;

function TValueToVariant(AValue: TValue): Variant;
begin
  case AValue.kind of
    tkUnknown: Result := null;
    tkInteger: Result := AValue.AsInteger;
    tkChar: Result := AValue.AsString;
    tkEnumeration: Result := null;
    tkFloat: Result := AValue.AsExtended;
    tkString: Result := AValue.AsString;
    tkSet: Result := null;
    tkClass: Result := null;
    tkMethod: Result := null;
    tkWChar: Result := AValue.AsString;
    tkLString: Result := AValue.AsString;
    tkWString: Result := AValue.AsString;
    tkVariant: Result := AValue.AsVariant;
    tkArray: Result := null;
    tkRecord: Result := null;
    tkInterface: Result := null;
    tkInt64: Result := AValue.AsInt64;
    tkDynArray: Result := null;
    tkUString: Result := AValue.asString;
    tkClassRef: Result := null;
    tkPointer: Result := null;
    tkProcedure: Result := null;
    tkMRecord: Result := null;
  end;
end;

{$ENDIF}

procedure SetJsonTValue(APJson: PJsonDataValue; AValue: TValue);
begin
  case AValue.kind of
    tkUnknown: APJson.ObjectValue := nil;
    tkInteger: APJson.IntValue := AValue.AsInteger;
    tkChar: APJson.Value := AValue.AsString;
    tkEnumeration: APJson.ObjectValue := nil;
    tkFloat: APJson.FloatValue := AValue.AsExtended;
    tkString: APJson.Value := AValue.AsString;
    tkSet: APJson.ObjectValue := nil;
    tkClass: APJson.ObjectValue := nil;
    tkMethod: APJson.ObjectValue := nil;
    tkWChar: APJson.Value := AValue.AsString;
    tkLString: APJson.Value := AValue.AsString;
    tkWString: APJson.Value := AValue.AsString;
{$IFNDEF JSX_NOVAR}
    tkVariant: SetJsonVariant(APJson, AValue.AsVariant);
{$ENDIF}
    tkArray: APJson.ObjectValue := nil;
    tkRecord: APJson.ObjectValue := nil;
    tkInterface: APJson.ObjectValue := nil;
    tkInt64: APJson.LongValue := AValue.AsInt64;
    tkDynArray: APJson.ObjectValue := nil;
    tkUString: APJson.Value := AValue.asString;
    tkClassRef: APJson.ObjectValue := nil;
    tkPointer: APJson.ObjectValue := nil;
    tkProcedure: APJson.ObjectValue := nil;
    tkMRecord: APJson.ObjectValue := nil;
  end;
end;

procedure AddTValueToJsonArray(AJsonArr: TJsonArray; AValue: TValue);
begin
  case AValue.kind of
    tkUnknown: AJsonArr.Add(TJsonObject(nil));
    tkInteger: AJsonArr.Add(AValue.AsInteger);
    tkChar: AJsonArr.Add(AValue.AsString);
    tkEnumeration: AJsonArr.Add(TJsonObject(nil));
    tkFloat: AJsonArr.Add(Double(AValue.AsExtended));
    tkString: AJsonArr.Add(AValue.AsString);
    tkSet: AJsonArr.Add(TJsonObject(nil));
    tkClass: AJsonArr.Add(TJsonObject(nil));
    tkMethod: AJsonArr.Add(TJsonObject(nil));
    tkWChar: AJsonArr.Add(AValue.AsString);
    tkLString: AJsonArr.Add(AValue.AsString);
    tkWString: AJsonArr.Add(AValue.AsString);
{$IFNDEF JSX_NOVAR}
    tkVariant: AJsonArr.Add(AValue.AsVariant);
{$ENDIF}
    tkArray: AJsonArr.Add(TJsonObject(nil));
    tkRecord: AJsonArr.Add(TJsonObject(nil));
    tkInterface: AJsonArr.Add(TJsonObject(nil));
    tkInt64: AJsonArr.Add(AValue.AsInt64);
    tkDynArray: AJsonArr.Add(TJsonObject(nil));
    tkUString: AJsonArr.Add(AValue.asString);
    tkClassRef: AJsonArr.Add(TJsonObject(nil));
    tkPointer: AJsonArr.Add(TJsonObject(nil));
    tkProcedure: AJsonArr.Add(TJsonObject(nil));
    tkMRecord: AJsonArr.Add(TJsonObject(nil));
  end;
end;

{$ENDREGION 'Utils'}

{$REGION 'TJsonX2'}

constructor TJsonX2.Create;
begin
  FInstance := Nil;
end;

destructor TJsonX2.Destroy;
begin
  FInstance.Free;
  inherited;
end;

procedure  TJsonX2.Serialize(
            AObj: TObject;
            AJsonObj: W3DJsonX2.Obj.TJsonObject;
            AJsonPatcher: TJX2Patcher;
            ASettings: TJX2Settings
          );
var

{$IFNDEF JSX_NOVAR}
  LVariant: variant;
  Lvar: variant;
  LVarLoop: variant;
  LStrVarDic: TPair<string, Variant>;
{$ENDIF}
  LJsonStr: string;
  LFields: TArray<TRTTIField>;
  LField: TRTTIField;
  LVal: TValue;
  LJsonName: string;
  LAttr: TCustomAttribute;
  LCurObj: TObject;
  LTypedObj: TObject;
  LJsonObj: TJsonObject;
  LJsonObjLoop: TJsonObject;
  LJsonArr: TJsonArray;
  LObjLoop: IJX2;
  LCurIntf: IJX2;
  LStrValue: TPair<string, TValue>;
  LStrObjPair: TPair<string, IJX2>;
  LStrObjLoopPair: TPair<string, TObject>;
  LObjLoopClass: TObject;
  LTValue: TValue;
  LDouble: Double;
  LSuccess: Boolean;
  LAttrIntf: IJX2Converter;

begin
  if (AObj = nil) then exit;
  LFields := GetFields(AObj);
  AJsonObj.Capacity := Length(LFields);
  for LField in LFields do
  begin

    LCurObj := nil;
    LJsonName := LField.Name;
    if LField.Name.StartsWith('_') then Continue;
    LAttr := GetFieldAttribute(LField, JX2AttrName);
    if not Assigned(LAttr) and (jxExplicitBinding in ASettings) then Continue;
    if Assigned( GetFieldAttribute(LField, JX2AttrExclude) ) then Continue;
    if Assigned(LAttr) then LJsonName :=  JX2AttrName(LAttr).FName;

{$IFNDEF JSX_NOVAR}
    if LField.FieldType.TypeKind in [tkVariant] then
    begin
      LVariant := LField.GetValue(AObj).AsVariant;
      case FindVarData(LVariant)^.VType of
        varEmpty, varNull:
          begin
            if not (jxoNullify in ASettings) then Continue;
            AJsonObj.InternAddItem(LJsonName).VariantValue := Null;
          end;
        varOleStr, varString, varUString:
          AJsonObj.InternAddItem(LJsonName).Value := LVariant;
        varSmallInt, varInteger, varShortInt, varByte, varWord, varLongWord:
          AJsonObj.InternAddItem(LJsonName).IntValue := LVariant;
        varDate:
          AJsonObj.InternAddItem(LJsonName).DateTimeValue := (LVariant);
        varBoolean:
          AJsonObj.InternAddItem(LJsonName).BoolValue := (LVariant);
        varInt64:
          AJsonObj.InternAddItem(LJsonName).LongValue := (LVariant);
        varUInt64:
          AJsonObj.InternAddItem(LJsonName).ULongValue := (LVariant);
        varSingle, varDouble, varCurrency:
          AJsonObj.InternAddItem(LJsonName).FloatValue := (LVariant);
      else
        if not (jxoNullify in ASettings) then Continue;
        AJsonObj.InternAddItem(LJsonName).VariantValue := Null;
      end;
      Continue;
    end;
{$ENDIF}

    if LField.FieldType.TypeKind in [tkRecord] then
    begin
      if LField.FieldType.Handle = TypeInfo(TValue) then
      begin
        if not LField.GetValue(AObj).TryAsType<TValue>(LTValue) then Continue;
        LSuccess := True;
        case LTValue.Kind of
          tkUnknown: LSuccess := False;
          tkInteger: AJsonObj.InternAddItem(LJsonName).IntValue := LTValue.AsInteger;
          tkChar: AJsonObj.InternAddItem(LJsonName).Value := LTValue.asString;
          tkEnumeration:
            begin
              if IsBoolType(LTValue.TypeInfo) then
                AJsonObj.InternAddItem(LJsonName).BoolValue := Boolean(LTValue.AsBoolean);
            end;
          tkFloat:
            begin
              LDouble := LTValue.AsExtended;
              AJsonObj.InternAddItem(LJsonName).FloatValue := LDouble;
            end;
          tkString: AJsonObj.InternAddItem(LJsonName).Value := LTValue.asString;
          tkSet: LSuccess := False;
          tkClass: LSuccess := False;
          tkMethod: LSuccess := False;
          tkWChar: AJsonObj.InternAddItem(LJsonName).Value := LTValue.asString;
          tkLString: AJsonObj.InternAddItem(LJsonName).Value := LTValue.asString;
          tkWString: AJsonObj.InternAddItem(LJsonName).Value := LTValue.asString;
{$IFNDEF JSX_NOVAR}
          tkVariant:
            begin
              Lvar := LTValue.AsVariant; // Do not remove, it will crash the Linux Compiler
              AJsonObj.InternAddItem(LJsonName).VariantValue := LVar;
            end;
{$ENDIF}
          tkArray: LSuccess := False;
          tkRecord: LSuccess := False;
          tkInterface: LSuccess := False;
          tkInt64: AJsonObj.InternAddItem(LJsonName).LongValue := LTValue.AsInt64;
          tkDynArray: LSuccess := False;
          tkUString: AJsonObj.InternAddItem(LJsonName).Value := LTValue.asString;
          tkClassRef: LSuccess := False;
          tkPointer: LSuccess := False;
          tkProcedure: LSuccess := False;
          tkMRecord: LSuccess := False;
        end;
        if not LSuccess then
        begin
          if not (jxoNullify in ASettings) then Continue;
          AJsonObj.InternAddItem(LJsonName).ObjectValue := nil;
        end;
      end;
      Continue;
    end;

    if LField.FieldType.TypeKind in [tkClass] then
    begin

      LCurObj := LField.GetValue(AObj).AsObject;

      if LCurObj = Nil then
      begin
        if not (jxoNullify in ASettings) then Continue;
        AJsonObj.InternAddItem(LJsonName).ObjectValue := nil;
        Continue;
      end;

      if LCurObj.ClassType = TJX2ValueList then
      begin
        LJsonArr := TJsonArray(TJsonArray.NewInstance);
        LJsonArr.Capacity :=  TJX2ValueList(LCurObj).Count;
        for LVal in  TJX2ValueList(LCurObj) do
          AddTValueToJsonArray(LJsonArr, LVal);
        aJsonObj.AddItem(LJsonName).ArrayValue := LJsonArr;
        if jxoInnerJson in ASettings then TJX2ValueList(LCurObj)._InnerJson:= LJsonArr.ToJSON();
        Continue;
      end;

{$IFNDEF JSX_NOVAR}
      if LCurObj.ClassType = TJX2VarList then
      begin
        LJsonArr := TJsonArray.Create;
        LJsonArr.Capacity := TJX2VarList(LCurObj).Count;
        for LVariant in TJX2VarList(LCurObj) do LJsonArr.Add(LVariant);
        aJsonObj.AddItem(LJsonName).ArrayValue := LJsonArr;
        if jxoInnerJson in ASettings then TJX2VarList(LCurObj)._InnerJson:= LJsonArr.ToJSON();
        Continue;
      end;
{$ENDIF}

      if LCurObj.ClassType = TJX2ObjList then
      begin
        LJsonArr := TJSonArray.Create;
        LJsonArr.Capacity := TJX2ObjList(LCurObj).Count;
        for LObjLoopClass in TJX2ObjList(LCurObj) do
        begin
          LJsonObj := TJsonObject(TJsonObject.NewInstance);
          Serialize(LObjLoopClass, LJsonObj, AJsonPatcher, ASettings);
          LJsonArr.Add(LJsonObj);
        end;
        AJsonObj.InternAddItem(LJsonName).ArrayValue := LJsonArr;
        if jxoInnerJson in ASettings then TJX2ObjList(LCurObj)._InnerJson:= LJsonArr.ToJSON();
        Continue;
      end;

{$IFNDEF JSX_NOVAR}
      if LCurObj.ClassType = TJX2StrVarDic then
      begin
        LJsonObj := TJsonObject(TJsonObject.NewInstance);
        LJsonObj.Capacity := TJX2StrVarDic(LCurObj).Count;
        for LStrVarDic in TJX2StrVarDic(LCurObj) do
          SetJsonVariant(LJsonObj.AddItem(LStrVarDic.Key), LStrVarDic.Value);
        aJsonObj.InternAddItem(LJsonName).ObjectValue := LJsonObj;
        if jxoInnerJson in ASettings then TJX2StrVarDic(LCurObj)._InnerJson:= LJsonObj.ToJSON();
        Continue;
      end;
{$ENDIF}

      if LCurObj.ClassType = TJX2StrValueDic then
      begin
        LJsonObj := TJsonObject(TJsonObject.NewInstance);
        LJsonObj.Capacity := TJX2StrValueDic(LCurObj).Count;
        for LStrValue in TJX2StrValueDic(LCurObj) do
          SetJsonTValue(LJsonObj.AddItem(LStrValue.Key), LStrValue.Value);
        aJsonObj.InternAddItem(LJsonName).ObjectValue := LJsonObj;
        if jxoInnerJson in ASettings then TJX2StrValueDic(LCurObj)._InnerJson:= LJsonObj.ToJSON();
        Continue;
      end;

      if LCurObj.ClassType = TJX2StrObjDic then
      begin
        LJsonObj := TJsonObject(TJsonObject.NewInstance);
        LJsonObj.Capacity := TJX2StrObjDic(LCurObj).Count;
        for LStrObjLoopPair in TJX2StrObjDic(LCurObj) do
        begin
          LJsonObjLoop := TJsonObject(TJsonObject.NewInstance);
          Serialize(LStrObjLoopPair.Value, LJsonObjLoop, AJsonPatcher, ASettings);
          LJsonObj.AddItem(LStrObjLoopPair.Key).ObjectValue := LJsonObjLoop;
        end;
        aJsonObj.AddItem(LJsonName).ObjectValue := LJsonObj;
        if jxoInnerJson in ASettings then TJX2StrObjDic(LCurObj)._InnerJson:= LJsonObj.ToJSON();
        Continue;
      end;

      LAttr := GetFieldAttribute(LField, JX2AttrConv);
     if Assigned(LAttr) then
      begin
        try
          if not Assigned(JX2AttrConv(LAttr).FConv) then Continue;
          if not Supports(JX2AttrConv(LAttr).FConv.Create, IJX2Converter, LAttrIntf) then Continue;
          LJsonStr := LAttrIntf.OnSerialize(TJX2DataBlock.Create(ASettings, LCurObj, LField, ''{JsonStr}, nil{JsonObj}, nil{AJsonVal}, AJsonPatcher));
          AJsonObj.AddItem(LJsonName).Value := AJsonPatcher.Encode(LJsonStr, '"', '"');
        except end;
        Continue;
      end;

      LJsonObj := TJsonObject(TJsonObject.NewInstance);
      Serialize(LCurObj, LJsonObj, AJsonPatcher, ASettings);
      aJsonObj.InternAddItem(LJsonName).ObjectValue := LJsonObj;
      if jxoInnerJson in ASettings then TJX2(LCurObj)._InnerJson:= LJsonObj.ToJson;
      Continue;

    end;

    if LField.FieldType.TypeKind in [tkInterface] then
    begin

      if LField.GetValue(AObj).AsInterface = Nil then
      begin
        if not (jxoNullify in ASettings) then Continue;
        AJsonObj.InternAddItem(LJsonName).ObjectValue := nil;
        Continue;
      end;

      if not Supports(LField.GetValue(AObj).AsInterface, IJX2, LCurIntf) then
      begin
        LAttr := GetFieldAttribute(LField, JX2AttrConv);
        if Assigned(LAttr) then
        begin
          try
           	if not Assigned(JX2AttrConv(LAttr).FConv) then Continue;
            if not Supports(JX2AttrConv(LAttr).FConv.Create, IJX2Converter, LAttrIntf) then Continue;
            LJsonStr := LAttrIntf.OnSerialize(TJX2DataBlock.Create(ASettings, LCurObj));
            AJsonObj.AddItem(LJsonName).Value := AJsonPatcher.Encode(LJsonStr, '"', '"');
          except end;
          Continue;
        end else begin
          if not (jxoNullify in ASettings) then Continue;
          AJsonObj.InternAddItem(LJsonName).ObjectValue := nil;
          Continue;
        end;
      end;

      LTypedObj  := LCurIntf as TObject;

      if Supports(LTypedObj, IJX2ValueList) then
      begin
        LJsonArr := TJsonArray(TJsonArray.NewInstance);
        LJsonArr.Capacity := TIJX2ValueList(LTypedObj).Count;
        for LVal in TIJX2ValueList(LTypedObj) do
          AddTValueToJsonArray(LJsonArr, LVal);
        aJsonObj.AddItem(LJsonName).ArrayValue := LJsonArr;
        if jxoInnerJson in ASettings then TIJX2ValueList(LTypedObj)._InnerJson := LJsonArr.ToJson;
		    Continue;
      end;

{$IFNDEF JSX_NOVAR}
      if Supports(LTypedObj, IJX2VarList) then
      begin
        LJsonArr := TJsonArray.Create;
        LJsonArr.Capacity := TIJX2VarList(LTypedObj).Count;
        for LVarLoop in  TIJX2VarList(LTypedObj) do LJsonArr.Add(LVarLoop);
        AJsonObj.AddItem(LJsonName).ArrayValue := LJsonArr;
        if jxoInnerJson in ASettings then TIJX2VarList(LTypedObj)._InnerJson := LJsonArr.ToJson;
		    Continue;
      end;
{$ENDIF}

      if Supports(LTypedObj, IJX2ObjList) then
      begin
        LJsonArr := TJSonArray.Create;
        LJsonArr.Capacity := TIJX2ObjList(LTypedObj).Count;
        for LObjLoop in TIJX2ObjList(LTypedObj)do
        begin
          LJsonObj := TJsonObject(TJsonObject.NewInstance);
          Serialize(TObject(LObjLoop), LJsonObj, AJsonPatcher, ASettings);
          LJsonArr.Add(LJsonObj);
        end;
        AJsonObj.InternAddItem(LJsonName).ArrayValue := LJsonArr;
        if jxoInnerJson in ASettings then TIJX2ObjList(LTypedObj)._InnerJson := LJsonArr.ToJSON();
        Continue;
      end;

{$IFNDEF JSX_NOVAR}
      if Supports(LTypedObj, IJX2StrVarDic) then
      begin
        LJsonObj := TJsonObject(TJsonObject.NewInstance);
        LJsonObj.Capacity := TIJX2StrVarDic(LTypedObj).Count;
        for LStrVarDic in TIJX2StrVarDic(LTypedObj) do
          SetJsonVariant(LJsonObj.AddItem(LStrVarDic.Key), LStrVarDic.Value);
        aJsonObj.InternAddItem(LJsonName).ObjectValue := LJsonObj;
        if jxoInnerJson in ASettings then TIJX2StrVarDic(LTypedObj)._InnerJson := LJsonObj.ToJSON();
		    Continue;
      end;
{$ENDIF}

      if Supports(LTypedObj, IJX2StrValueDic) then
      begin
        LJsonObj := TJsonObject(TJsonObject.NewInstance);
        LJsonObj.Capacity :=  TIJX2StrValueDic(LTypedObj).count;
        for LStrValue in  TIJX2StrValueDic(LTypedObj) do
          SetJsonTValue(LJsonObj.AddItem(LStrValue.Key), LStrValue.Value);
        aJsonObj.InternAddItem(LJsonName).ObjectValue := LJsonObj;
        if jxoInnerJson in ASettings then  TIJX2StrValueDic(LTypedObj)._InnerJson := LJsonObj.ToJSON();
		    Continue;
      end;

      if Supports(LTypedObj, IJX2StrObjDic) then
      begin
        LJsonObj := TJsonObject(TJsonObject.NewInstance);
        LJsonObj.Capacity := TIJX2StrObjDic(LTypedObj).Count;
        for LStrObjPair in TIJX2StrObjDic(LTypedObj) do
        begin
          LJsonObjLoop := TJsonObject(TJsonObject.NewInstance);
          Serialize(TObject(LStrObjPair.Value), LJsonObjLoop, AJsonPatcher, ASettings);
          LJsonObj.AddItem(LStrObjPair.Key).ObjectValue := LJsonObjLoop;
          if jxoInnerJson in ASettings then TIJX2StrObjDic(LTypedObj)._InnerJson := LJsonObjLoop.ToJSON();
        end;
        aJsonObj.AddItem(LJsonName).ObjectValue := LJsonObj;
    		Continue;
      end;

      if Supports(LTypedObj, IJX2) then
      begin
        LJsonObj := TJsonObject(TJsonObject.NewInstance);
        Serialize(LTypedObj, LJsonObj, AJsonPatcher, ASettings);
        AJsonObj.InternAddItem(LJsonName).ObjectValue := LJsonObj;
        if jxoInnerJson in ASettings then TIJX2(LTypedObj)._InnerJson := LJsonObj.ToJSON();
        Continue;
      end else begin
        if not (jxoNullify in ASettings) then Continue;
        AJsonObj.InternAddItem(LJsonName).ObjectValue := nil;
        Continue;
      end;

    end; // LField.FieldType.TypeKind in [tkInterface]

  end; // LField in LFields
end;

function TJsonX2.Serialize(Intf: IInterface; ASettings: TJX2Settings = []): string;
begin
  Result := Serialize(Intf as TObject, ASettings);
end;

function TJsonX2.Serialize(Obj: TObject; ASettings: TJX2Settings = []): string;
var
  LJsonObj: TJsonObject;
  LJsonPatcher: TJX2Patcher;
begin
  Result := '';
  LJsonObj := Nil;
  LJsonPatcher := Nil;
  try
    try
      LJsonObj := TJsonObject(TJsonObject.NewInstance);
      LJsonPatcher := TJX2Patcher.Create;
      Serialize(Obj, LJSonObj, LJsonPatcher, ASettings);
      Result :=  LJsonObj.ToJSON(True) ;
      LJsonPatcher.Decode(Result);
      if (jxoReturnEmptyJsonString in ASettings) and (LJsonObj.Count = 0) then Result := '';
    finally
      LJsonPatcher.Free;
      LJsonObj.Free;
    end;
  except
    on Ex: Exception do
      if (jxoRaiseException in ASettings) then raise Ex;
  end;
end;

procedure TJsonX2.Deserialize(
            AObj: TObject;
            AJsonObj: TJsonObject;
            ASettings: TJX2Settings);
var
{$IFNDEF JSX_NOVAR}
  LNewStrVar: TJX2StrVarDic;
  LNewVarList : TJX2VarList;
  LINewVarList: TIJX2VarList;
  LINewStrVarDic: TIJX2StrVarDic;
{$ENDIF}
  LIdx: Integer;
  LFields: TArray<TRTTIField>;
  LJIdx: Integer;
  LJValue: PJsonDataValue;
  LJName: string;
  LRTTIField: TRTTIField;
  LInstance: TRTTIInstanceType;
  LNewObj: TObject;
  LNewStrObj: TJX2StrObjDic;
  LPair: TJsonNameValuePair;
  LJsObj : TJsonObject;
  LNewValueList : TJX2ValueList;
  LNewObjList: TJX2ObjList;
  LNewStrValue: TJX2StrValueDic;
  LIntf: IJX2;
  LINewValList: TIJX2ValueList;
  LINewObjList: TIJX2ObjList;
  LINewStrValueDic: TIJX2StrValueDic;
  LINewStrObjDic: TIJX2StrObjDic;
  LTValue: TValue;
  LExplicit: Boolean;
  LAttr: TCustomAttribute;
  LAttrIntf: IJX2Converter;

  function GetFieldName(AFields: TArray<TRTTIField>; const AJsonName: string; var AExplicit: Boolean): TRTTIField; inline;
  var
    AAttr: JX2AttrName;
  begin
    AExplicit := False;
    for Result in AFields do
    begin
      AAttr := JX2AttrName(GetFieldAttribute(Result, JX2AttrName));
      AExplicit := (AAttr <> Nil) and (AAttr.FName = AJsonName);
      if AExplicit then Exit;
      if AJsonName = Result.Name then Exit;
    end;
    Result := Nil;
  end;

begin
  if (AJsonObj = nil) or (AObj = nil) then  Exit;
  LFields := GetFields(AObj);
  for LJIdx := AJsonObj.count - 1 downto 0 do
  begin
    LJValue := AJsonObj.Items[LJIdx];
    LJName := AJsonObj.Names[LJIdx];
    LRTTIField := GetFieldName(LFields, LJName, LExplicit);
    if LRTTIField = Nil then Continue;
    if (jxExplicitbinding in ASettings) and not LExplicit then Continue;
{$IFNDEF JSX_NOVAR}
    if LRTTIField.FieldType.TypeKind in [tkVariant] then
    begin
      LRTTIField.SetValue(AObj, TValue.FromVariant(LJValue.VariantValue));
      continue
    end else
{$ENDIF}
    if LRTTIField.FieldType.TypeKind in [tkRecord] then
    begin
      if LRTTIField.FieldType.Handle = TypeInfo(TValue) then
      begin
        case LJValue.Typ of
         jdtNone: LTValue := nil;
         jdtString: LTValue := LJValue.Value;
         jdtInt: LTValue := LJValue.IntValue;
         jdtLong: LTValue := LJValue.LongValue;
         jdtULong: LTValue := LJValue.ULongValue;
         jdtFloat: LTValue := LJValue.FloatValue;
         jdtDateTime: LTValue := DateToISO8601(LJValue.UtcDateTimeValue);
         jdtUtcDateTime: LTValue := DateToISO8601(LJValue.UtcDateTimeValue);
         jdtBool: LTValue := LJValue.BoolValue;
         jdtArray: LTValue := nil;
         jdtObject: LTValue := nil;
        end;
        LRTTIField.SetValue(AObj, LTValue);
        Continue;
      end;
    end else

    if LRTTIField.FieldType.TypeKind in [tkClass] then
    begin

      if LJValue.IsNull then
      begin
        LRTTIField.SetValue(AObj, Nil);
        Continue;
       end;

      LInstance := LRTTIField.FieldType.AsInstance;

      if LInstance.MetaclassType = TJX2StrObjDic then
      begin
        LAttr := GetFieldAttribute(LRTTIField, JX2AttrClass);
        if LAttr = Nil then
          raise Exception.Create('TJX2StrObjDic is missing JX2AttrClass : ' + LRTTIField.Name);
        if LJValue.ObjectValue = Nil then Continue;
        LNewStrObj := TJX2StrObjDic.Create([doOwnsValues]);
        LNewStrObj.Capacity := LJValue.ObjectValue.Count;
        LRTTIField.SetValue(AObj, LNewStrObj);
        for LPair in LJValue.ObjectValue do
        begin
          LNewObj := JX2AttrClass(LAttr).FClass.Create;
          Deserialize(LNewObj, LPair.Value.ObjectValue, ASettings);
          LNewStrObj.Add(LPair.Name, LNewObj);
        end;
        Continue;
      end else

      if LInstance.MetaclassType = TJX2ObjList then
      begin
        LAttr := GetFieldAttribute(LRTTIField, JX2AttrClass);
        if LAttr = Nil then
          raise Exception.Create('TJX2ObjList is missing JX2AttrClass : ' + LRTTIField.Name);
        LNewObjList := TJX2ObjList.Create(True);
        LRTTIField.SetValue(aObj, LNewObjList);
        LNewObjList.Capacity := LJValue.ArrayValue.Count;
        for LIdx := 0 to LJValue.ArrayValue.count - 1 do
        begin
          LNewObj := JX2AttrClass(LAttr).FClass.Create;
          LNewObjList.Add(LNewObj);
          Deserialize(LNewObj, LJValue.ArrayValue.O[LIdx], ASettings);
        end;
        Continue
      end else

      if LInstance.MetaclassType = TJX2ValueList then
      begin
        LNewValueList := TJX2ValueList.Create;
        LRTTIField.SetValue(AObj, LNewValueList);
        LNewValueList.Capacity := LJValue.ArrayValue.Count;
        for LIdx := 0 to LJValue.ArrayValue.count - 1 do
          LNewValueList.Add(JsonToTValue(LJValue.ArrayValue.Values[LIdx]));
        Continue;
      end else

{$IFNDEF JSX_NOVAR}
      if LInstance.MetaclassType = TJX2VarList then
      begin
        LNewVarList := TJX2VarList.Create;
        LRTTIField.SetValue(AObj, LNewVarList);
        LNewVarList.Capacity := LJValue.ArrayValue.Count;
        for LIdx := 0 to LJValue.ArrayValue.count - 1 do
          LNewVarList.Add(LJValue.ArrayValue.V[LIdx]);
        Continue
      end else

      if LInstance.MetaclassType = TJX2StrVarDic then
      begin
        LNewStrVar := TJX2StrVarDic.Create;
        LRTTIField.SetValue(AObj, LNewStrVar);
        LJsObj := LJValue.ObjectValue;
        for LIdx := 0 to LJsObj.count - 1 do
          LNewStrVar.Add(LJsObj.Names[LIdx],LJsObj.Values[LJsObj.Names[LIdx]]);
        Continue;
      end else
{$ENDIF}

      if LInstance.MetaclassType = TJX2StrValueDic then
      begin
        LNewStrValue := TJX2StrValueDic.Create;
        LRTTIField.SetValue(AObj, LNewStrValue);
        LJsObj := LJValue.ObjectValue;
        for LIdx := 0 to LJsObj.count - 1 do
          LNewStrValue.Add(LJsObj.Names[LIdx], JsonToTValue(LJsObj.Values[ LJsObj.Names[LIdx]]));
        Continue
      end else

{$IFNDEF JSX_NOVAR}
      if LInstance.MetaclassType = TJX2StrVarDic then
      begin
        LNewStrVar := TJX2StrVarDic.Create;
        LRTTIField.SetValue(AObj, LNewStrVar);
        LJsObj := LJValue.ObjectValue;
        for LIdx := 0 to LJsObj.count - 1 do
          LNewStrVar.Add(LJsObj.Names[LIdx], LJsObj.Items[LIdx].Value);
        Continue;
      end else
{$ENDIF}

      begin
        LAttr := GetFieldAttribute(LRTTIField, JX2AttrConv);
        if Assigned(LAttr) then
        begin
          try
           	if not Assigned(JX2AttrConv(LAttr)) then Continue;
            if not Supports(JX2AttrConv(LAttr).FConv.Create, IJX2Converter, LAttrIntf) then Continue;
            LNewObj := LAttrIntf.OnDeserialize(TJX2DataBlock.Create(ASettings, nil, LRTTIField, '', AJsonObj, LJValue));
            LRTTIField.SetValue(AObj, LNewObj);
          except end;
          Continue;
        end;

        LNewObj := LInstance.MetaclassType.Create;
        Deserialize(LNewObj, LJValue.ObjectValue, ASettings);
        LRTTIField.SetValue(AObj, LNewObj);
        Continue
      end;
    end;

    if LRTTIField.FieldType.TypeKind in [tkInterface] then
    begin

      if LJValue.IsNull then
      begin
        LRTTIField.SetValue(aObj, Nil);
        Continue;
      end;

      LAttr := GetFieldAttribute(LRTTIField, JX2AttrConv);
      if Assigned(LAttr) then
      begin
        try
          if not Assigned(JX2AttrConv(LAttr)) then Continue;
          if not Supports(JX2AttrConv(LAttr).FConv.Create, IJX2Converter, LAttrIntf) then Continue;
          LNewObj := LAttrIntf.OnDeserialize(TJX2DataBlock.Create(ASettings, Self, LRTTIField, '', AJsonObj));
          LRTTIField.SetValue(aObj, LNewObj);
        except end;
        Continue;
      end;

      LAttr := GetFieldAttribute(LRTTIField, JX2AttrClass);
      if LAttr = Nil then
        raise Exception.Create('Interface is missing JX2AttrClass : ' + LRTTIField.Name);

      if Supports(JX2AttrClass(LAttr).FClass, IJX2ObjList) then
      begin;
        LINewObjList := TIJX2ObjList.Create;
        LRTTIField.SetValue(AObj, LINewObjList);
        LINewObjList.Capacity := LJValue.ArrayValue.Count;
        for LIdx := 0 to LJValue.ArrayValue.count - 1 do
        begin
          LNewObj := JX2AttrClass(LAttr).FData1.Create;
          if Supports(LNewObj, IJX2, LIntf) then
          begin
            LINewObjList.Add(LIntf);
            Deserialize(LNewObj, LJValue.ArrayValue.O[LIdx], ASettings);
          end;
        end;
        Continue;
      end;

      if Supports(JX2AttrClass(LAttr).FClass, IJX2ValueList) then
      begin
        LINewValList := TIJX2ValueList.Create;
        LINewValList.Capacity := LJValue.ArrayValue.Count;
        LRTTIField.SetValue(AObj, LINewValList);
        for LIdx := 0 to LJValue.ArrayValue.count - 1 do
          LINewValList.Add(JsonToTValue(LJValue.ArrayValue.Values[LIdx]));
        Continue;
      end;

{$IFNDEF JSX_NOVAR}
      if Supports(JX2AttrClass(LAttr).FClass, IJX2VarList) then
      begin
        LINewVarList := TIJX2VarList.Create;
        LINewVarList.Capacity := LJValue.ArrayValue.Count;
        LRTTIField.SetValue(AObj, LINewVarList);
        for LIdx := 0 to LJValue.ArrayValue.count - 1 do
          LINewVarList.Add(LJValue.ArrayValue.V[LIdx]);
        Continue;
      end;

      if Supports(JX2AttrClass(LAttr).FClass, IJX2StrVarDic) then
      begin
        if LJValue.ObjectValue = Nil then Continue;
        LINewStrVarDic := TIJX2StrVarDic.Create;
        LRTTIField.SetValue(AObj, LINewStrVarDic);
        LJsObj := LJValue.ObjectValue;
        for LIdx := 0 to LJsObj.count - 1 do
          LINewStrVarDic.Add(LJsObj.Names[LIdx], LJsObj.Values[LJsObj.Names[LIdx]].VariantValue);
        Continue;
      end;
{$ENDIF}

      if Supports(JX2AttrClass(LAttr).FClass, IJX2StrValueDic) then
      begin
        if LJValue.ObjectValue = Nil then Continue;
        LINewStrValueDic := TIJX2StrValueDic.Create;
        LRTTIField.SetValue(AObj, LINewStrValueDic);
        LJsObj := LJValue.ObjectValue;
        for LIdx := 0 to LJsObj.count - 1 do
          LINewStrValueDic.Add(LJsObj.Names[LIdx], JsonToTValue(LJsObj.Values[LJsObj.Names[LIdx]]));
        Continue;
      end;

      if Supports(JX2AttrClass(LAttr).FClass, IJX2StrObjDic) then
      begin
        if LJValue.ObjectValue = Nil then Continue;
        LINewStrObjDic := TIJX2StrObjDic.Create;
        LINewStrObjDic.Capacity := LJValue.ObjectValue.Count;
        LRTTIField.SetValue(AObj, LINewStrObjDic);
        for LPair in LJValue.ObjectValue do
        begin
          LNewObj := JX2AttrClass(LAttr).FData1.Create;
          Deserialize(LNewObj, LPair.Value.ObjectValue, ASettings);
          Supports(LNewObj, IJX2, LIntf);
          LINewStrObjDic.Add(LPair.Name, LIntf);
        end;
        Continue;
      end;

      if LJValue.ObjectValue = Nil then Continue;
      LNewObj := JX2AttrClass(LAttr).FClass.Create;
      Deserialize(LNewObj, LJValue.ObjectValue, ASettings);
      LRTTIField.SetValue(AObj, LNewObj);

    end; // LRTTIField.FieldType.TypeKind in [tkInterface]

  end;
end;

function TJsonX2.Deserialize<T>(const AJsonStr: string; ASettings: TJX2Settings = []): T;
var
  LJsonObj: TJsonBaseObject;
  LObj: T;
begin
  Result := Nil;
  LJsonObj := Nil;
  LObj := Nil;
  try
    try
      LJsonObj := W3DJsonX2.Obj.TJsonObject.Parse(AJsonStr);
      LObj := T.Create;
      Deserialize(LObj, W3DJsonX2.Obj.TJsonObject(LJsonObj), ASettings);
      Result := T(LObj);
    finally
      LJsonObj.Free;
    end;
  except
    on Ex: Exception do
    begin
      LObj.Free;
      if (jxoRaiseException in ASettings) then raise Ex;
    end;
  end;
end;

function TJsonX2.Deserialize(AIntfClass: TClass; const AJsonStr: string; ASettings: TJX2Settings = []): IJX2;
var
  LJsonObj: TJsonBaseObject;
  LTIObj: TObject;
begin
  Result := Nil;
  LJsonObj := Nil;
  LTIObj := Nil;
  try
    try
      LJsonObj := W3DJsonX2.Obj.TJsonObject.Parse(AJsonStr);
      LTIObj := AIntfClass.Create as TObject;
      Deserialize(LTIObj, W3DJsonX2.Obj.TJsonObject(LJsonObj),  ASettings);
      supports(LTIObj, IJX2, Result);
    finally
      LJsonObj.Free;
    end;
  except
    on Ex: Exception do
    begin
      LTIObj.Free;
      if (jxoRaiseException in ASettings) then raise Ex;
    end;
  end;
end;

function TJsonX2.Beautifier(const AJsonStr : string; Compact: Boolean): string;
var
  LJsonObj: TJsonObject;
begin
  Result := '{}';
  LJsonObj := TJsonObject(TJsonObject.NewInstance);
  try
    LJsonObj.FromJSON(AJsonStr);
    Result := LJsonObj.ToJSon(Compact);
  finally
    LJsonObj.Free;
  end;
end;

{$ENDREGION 'TJsonX2'}

initialization
  W3DJX2 := TJsonX2.Create;
finalization
  W3DJX2.Free;
end.

