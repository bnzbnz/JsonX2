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

unit JsonX2;

{$IFDEF W3DCLIENT}
  {$DEFINE JSX_NOVAR}
{$ENDIF}

interface

uses
    Classes
    , System.Generics.Collections
    , JsonX2.Obj
    , JsonX2.Types
    , JsonX2.Patch
    ;

const
  BStrL: array[Boolean] of string = ('false','true');
  BStrK: array[Boolean] of string = ('False','True');
  BStrU: array[Boolean] of string = ('FALSE','TRUE');

type

  TJsonX2 = class(TObject)
  strict private
  class var
    FInstance: TJsonX2;
  protected
    procedure Serialize(AObj: TObject; AJsonObj: TJsonObject; AJsonPatcher: TJX2Patcher; ASettings: TJX2Settings; AStats: IJX2Stats); overload;
    procedure Deserialize(AObj: TObject; AJsonObj: TJsonObject; ASettings: TJX2Settings; AStats: IJX2Stats); overload;
  public
    constructor Create; overload;
    destructor  Destroy; override;
    class function Beautifier(const AJsonStr : string; Compact: Boolean = False): string;

    function  Serialize(AObj: TObject; ASettings: TJX2Settings = []; AStats: IJX2Stats = nil): string; overload;
    function  Serialize(Intf: IInterface; ASettings: TJX2Settings = []; AStats: IJX2Stats = nil): string; overload;
    function  Deserialize<T: class, constructor>(const AJsonStr: string; ASettings: TJX2Settings = []; AStats: IJX2Stats = nil): T; overload;
    function  Deserialize(AIntfClass: TClass; const AJsonStr: string; ASettings: TJX2Settings = []; AStats: IJX2Stats = nil): IJX2; overload;
  end;

var
  JX2 : TJsonX2;

implementation
uses
    RTTI
  , DateUtils
  , SysUtils
  , System.Diagnostics
  , System.TypInfo
{$IFNDEF JSX_NOVAR}
  , Variants
{$ENDIF}
  , JsonX2.Utils
  , JsonX2.RTTI
  , JsonX2.Conv
  ;

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

function CheckVisibility(AVisibility: TMemberVisibility; ASettings: TJX2Settings): Boolean; inline;
begin
  if (
    not (jxoPublishedBinding in ASettings)
    and not (jxoPublicBinding in ASettings)
    and not (jxoProtectedBinding in ASettings)
    and not (jxoPrivateBinding in ASettings)
  ) then Exit(True);

  if (jxoPublishedBinding in ASettings) and (AVisibility = mvPublished) then Exit(True);
  if (jxoPublicBinding in ASettings) and (AVisibility = mvPublic) then Exit(True);
  if (jxoProtectedBinding in ASettings) and (AVisibility = mvProtected) then Exit(True);
  if (jxoPrivateBinding in ASettings) and (AVisibility = mvPrivate) then Exit(True);
  Exit(False);
end;

procedure  TJsonX2.Serialize(
            AObj: TObject;
            AJsonObj: TJsonObject;
            AJsonPatcher: TJX2Patcher;
            ASettings: TJX2Settings;
            AStats: IJX2Stats
          );
var

{$IFNDEF JSX_NOVAR}
  LVariant: variant;
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
  LAttrConv: IJX2Converter;

begin
  if (AObj = nil) then exit;
  LFields := GetRTTIFields(AObj);
  AJsonObj.Capacity := Length(LFields);
  for LField in LFields do
  begin

    if Assigned(AStats) then AStats.IncOpsCount();

    LCurObj := nil;
    LJsonName := LField.Name;
    if LField.Name.StartsWith('_') then Continue;
    if not CheckVisibility(LField.Visibility, ASettings) then Continue;
    LAttr := GetRTTIAttribute(LField, JX2AttrName);
    if not Assigned(LAttr) and (jxExplicitBinding in ASettings) then Continue;
    if Assigned(GetRTTIAttribute(LField, JX2AttrExclude)) then Continue;
    if Assigned(LAttr) then LJsonName := JX2AttrName(LAttr).FName;

{$IFNDEF JSX_NOVAR}
    if LField.FieldType.TypeKind in [tkVariant] then
    begin
      if Assigned(AStats) then AStats.IncTValueCount();
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
        varBoolean:
          AJsonObj.InternAddItem(LJsonName).BoolValue := LVariant;
        varInt64:
          AJsonObj.InternAddItem(LJsonName).LongValue := LVariant;
        varUInt64:
          AJsonObj.InternAddItem(LJsonName).ULongValue := LVariant;
        varSingle, varDouble, varCurrency:
          AJsonObj.InternAddItem(LJsonName).FloatValue := LVariant;
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
        if Assigned(AStats) then AStats.IncTValueCount();
        if not LField.GetValue(AObj).TryAsType<TValue>(LTValue) then Continue;
        if not (jxoNullify in ASettings) and LTValue.IsEmpty then Continue;
        AJsonObj.InternAddItem(LJsonName).TValueValue := LTValue;
      end;
      Continue;
    end;

    if LField.FieldType.TypeKind in [tkClass] then
    begin

      LCurObj := LField.GetValue(AObj).AsObject;

      if LCurObj = nil then
      begin
        if not (jxoNullify in ASettings) then Continue;
        AJsonObj.InternAddItem(LJsonName).ObjectValue := nil;
        Continue;
      end;

      if LCurObj.ClassType = TJX2ValueList then
      begin
        LJsonArr := TJsonArray(TJsonArray.NewInstance);
        LJsonArr.Capacity :=  TJX2ValueList(LCurObj).Count;
        for LVal in  TJX2ValueList(LCurObj) do LJsonArr.Add(LVal);
        aJsonObj.AddItem(LJsonName).ArrayValue := LJsonArr;
        if jxoInnerJson in ASettings then TJX2ValueList(LCurObj)._InnerJson:= LJsonArr.ToJSON();
        Continue;
      end;

{$IFNDEF JSX_NOVAR}
      if LCurObj.ClassType = TJX2VarList then
      begin
        LJsonArr := TJsonArray(TJsonArray.NewInstance);
        LJsonArr.Capacity := TJX2VarList(LCurObj).Count;
        for LVariant in TJX2VarList(LCurObj) do LJsonArr.Add(LVariant);
        aJsonObj.AddItem(LJsonName).ArrayValue := LJsonArr;
        if jxoInnerJson in ASettings then TJX2VarList(LCurObj)._InnerJson:= LJsonArr.ToJSON();
        Continue;
      end;
{$ENDIF}

      if LCurObj.ClassType = TJX2ObjList then
      begin
        LJsonArr := TJsonArray(TJsonArray.NewInstance);
        LJsonArr.Capacity := TJX2ObjList(LCurObj).Count;
        for LObjLoopClass in TJX2ObjList(LCurObj) do
        begin
          LJsonObj := TJsonObject(TJsonObject.NewInstance);
          Serialize(LObjLoopClass, LJsonObj, AJsonPatcher, ASettings, AStats);
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
          LJsonObj.AddItem(LStrVarDic.Key).VariantValue := LStrVarDic.Value;
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
          LJsonObj.AddItem(LStrValue.Key).TValueValue := LStrValue.Value;
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
          Serialize(LStrObjLoopPair.Value, LJsonObjLoop, AJsonPatcher, ASettings, AStats);
          LJsonObj.AddItem(LStrObjLoopPair.Key).ObjectValue := LJsonObjLoop;
        end;
        aJsonObj.AddItem(LJsonName).ObjectValue := LJsonObj;
        if jxoInnerJson in ASettings then TJX2StrObjDic(LCurObj)._InnerJson:= LJsonObj.ToJSON();
        Continue;
      end;

      LAttr := GetRTTIAttribute(LField, JX2AttrConv);
      if Assigned(LAttr) then
      begin
        try
          if not Assigned(JX2AttrConv(LAttr).FConv) then Continue;
          if not Supports(JX2AttrConv(LAttr).FConv.Create, IJX2Converter, LAttrConv) then Continue;
          LJsonStr := LAttrConv.OnSerialize(TJX2DataBlock.Create(ASettings, LCurObj, LField, ''{JsonStr}, nil{JsonObj}, nil{AJsonVal}, AJsonPatcher));
          AJsonObj.AddItem(LJsonName).Value := AJsonPatcher.Encode(LJsonStr, '"', '"');
        except end;
        Continue;
      end;

      LJsonObj := TJsonObject(TJsonObject.NewInstance);
      Serialize(LCurObj, LJsonObj, AJsonPatcher, ASettings, AStats);
      aJsonObj.InternAddItem(LJsonName).ObjectValue := LJsonObj;
      if jxoInnerJson in ASettings then TJX2(LCurObj)._InnerJson:= LJsonObj.ToJson;
      Continue;

    end;

    if LField.FieldType.TypeKind in [tkInterface] then
    begin

      if LField.GetValue(AObj).AsInterface = nil then
      begin
        if not (jxoNullify in ASettings) then Continue;
        AJsonObj.InternAddItem(LJsonName).ObjectValue := nil;
        Continue;
      end;

      if not Supports(LField.GetValue(AObj).AsInterface, IJX2, LCurIntf) then
      begin
        LAttr := GetRTTIAttribute(LField, JX2AttrConv);
        if Assigned(LAttr) then
        begin
          try
           	if not Assigned(JX2AttrConv(LAttr).FConv) then Continue;
            if not Supports(JX2AttrConv(LAttr).FConv.Create, IJX2Converter, LAttrConv) then Continue;
            LJsonStr := LAttrConv.OnSerialize(TJX2DataBlock.Create(ASettings, LCurObj));
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
        for LVal in TIJX2ValueList(LTypedObj) do LJsonArr.Add(LVal);
        aJsonObj.AddItem(LJsonName).ArrayValue := LJsonArr;
        if jxoInnerJson in ASettings then TIJX2ValueList(LTypedObj)._InnerJson := LJsonArr.ToJson;
		    Continue;
      end;

{$IFNDEF JSX_NOVAR}
      if Supports(LTypedObj, IJX2VarList) then
      begin
        LJsonArr := TJsonArray(TJsonArray.NewInstance);
        LJsonArr.Capacity := TIJX2VarList(LTypedObj).Count;
        for LVarLoop in  TIJX2VarList(LTypedObj) do LJsonArr.Add(LVarLoop);
        AJsonObj.AddItem(LJsonName).ArrayValue := LJsonArr;
        if jxoInnerJson in ASettings then TIJX2VarList(LTypedObj)._InnerJson := LJsonArr.ToJson;
		    Continue;
      end;
{$ENDIF}

      if Supports(LTypedObj, IJX2ObjList) then
      begin
        LJsonArr := TJsonArray(TJsonArray.NewInstance);
        LJsonArr.Capacity := TIJX2ObjList(LTypedObj).Count;
        for LObjLoop in TIJX2ObjList(LTypedObj)do
        begin
          LJsonObj := TJsonObject(TJsonObject.NewInstance);
          Serialize(TObject(LObjLoop), LJsonObj, AJsonPatcher, ASettings, AStats);
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
          LJsonObj.AddItem(LStrVarDic.Key).VariantValue := LStrVarDic.Value;
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
          LJsonObj.AddItem(LStrValue.Key).TValueValue := LStrValue.Value;
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
          Serialize(TObject(LStrObjPair.Value), LJsonObjLoop, AJsonPatcher, ASettings, AStats);
          LJsonObj.AddItem(LStrObjPair.Key).ObjectValue := LJsonObjLoop;
          if jxoInnerJson in ASettings then TIJX2StrObjDic(LTypedObj)._InnerJson := LJsonObjLoop.ToJSON();
        end;
        aJsonObj.AddItem(LJsonName).ObjectValue := LJsonObj;
    		Continue;
      end;

      if Supports(LTypedObj, IJX2) then
      begin
        LJsonObj := TJsonObject(TJsonObject.NewInstance);
        Serialize(LTypedObj, LJsonObj, AJsonPatcher, ASettings, AStats);
        AJsonObj.InternAddItem(LJsonName).ObjectValue := LJsonObj;
        if jxoInnerJson in ASettings then TIJX2(LTypedObj)._InnerJson := LJsonObj.ToJSON();
        Continue;
      end else begin
        if not (jxoNullify in ASettings) then Continue;
        AJsonObj.InternAddItem(LJsonName).ObjectValue := nil;
        Continue;
      end;
    end;
  end; // for LField in LFields do
end;

function TJsonX2.Serialize(AObj: TObject; ASettings: TJX2Settings; AStats: IJX2Stats): string;
var
  LJsonObj: TJsonObject;
  LJsonPatcher: TJX2Patcher;
  LWatch: TStopwatch;
begin
  Result := '';
  LJsonObj := Nil;
  LJsonPatcher := Nil;
  try
    try
      if Assigned(AStats) then
      begin
        AStats.Clear;
        LWatch := TStopwatch.StartNew;
      end;
      LJsonObj := TJsonObject(TJsonObject.NewInstance);
      LJsonPatcher := TJX2Patcher.Create;
      Serialize(AObj, LJSonObj, LJsonPatcher, ASettings, AStats);
      Result :=  LJsonObj.ToJSON(True) ;
      LJsonPatcher.Decode(Result);
      if (jxoEmptyString in ASettings) and (LJsonObj.Count = 0) then Result := '';
    finally
      LJsonPatcher.Free;
      LJsonObj.Free;
      if Assigned(AStats) then
        AStats.SetDurationMS( LWatch.ElapsedMilliseconds) ;
    end;
  except
    on Ex: Exception do
      if (jxoRaiseException in ASettings) then raise Ex;
  end;
end;

function TJsonX2.Serialize(Intf: IInterface; ASettings: TJX2Settings; AStats: IJX2Stats): string;
begin
  Result := Serialize(Intf as TObject, ASettings, AStats);
end;

procedure TJsonX2.Deserialize(
            AObj: TObject;
            AJsonObj: TJsonObject;
            ASettings: TJX2Settings;
            AStats: IJX2Stats
          );
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
  LField: TRTTIField;
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
  LExplicitName: Boolean;
  LAttr: TCustomAttribute;
  LAttrIntf: IJX2Converter;

  function GetFieldName(AFields: TArray<TRTTIField>; const AJsonName: string; var AExplicit: Boolean): TRTTIField; inline;
  var
    AAttr: JX2AttrName;
  begin
    AExplicit := False;
    for Result in AFields do
    begin
      AAttr := JX2AttrName(GetRTTIAttribute(Result, JX2AttrName));
      AExplicit := (AAttr <> Nil) and (AAttr.FName = AJsonName);
      if AExplicit then Exit;
      if AJsonName = Result.Name then Exit;
    end;
    Result := Nil;
  end;

begin
  if (AJsonObj = nil) or (AObj = nil) then Exit;
  LFields := GetRTTIFields(AObj);
  for LJIdx := AJsonObj.count - 1 downto 0 do
  begin
    if Assigned(AStats) then AStats.IncOpsCount();
    LJValue := AJsonObj.Items[LJIdx];
    LJName := AJsonObj.Names[LJIdx];
    LField := GetFieldName(LFields, LJName, LExplicitName);
    if LField = nil then
    begin
      if jxoRaiseOnMissingField in ASettings then
        Raise Exception.CreateFmt('Missing RTTIField : %s', [LJName]);
      Continue;
    end;
     if not CheckVisibility(LField.Visibility, ASettings) then Continue;
    if (jxExplicitbinding in ASettings) and not LExplicitName then Continue;

    if LField.FieldType.TypeKind in [tkRecord] then
    begin
      if LField.FieldType.Handle = TypeInfo(TValue) then
      begin
        if Assigned(AStats) then AStats.IncTValueCount();
        LField.SetValue(AObj, LJValue.TValueValue);
      end;
      Continue;
    end;

{$IFNDEF JSX_NOVAR}
    if LField.FieldType.TypeKind in [tkVariant] then
    begin
      if Assigned(AStats) then AStats.IncVariantCount();
      LField.SetValue(AObj, TValue.FromVariant(LJValue.VariantValue));
      Continue
    end;
{$ENDIF}

    if LField.FieldType.TypeKind in [tkClass] then
    begin
      if LJValue.IsNull then
      begin
        LField.SetValue(AObj, Nil);
        Continue;
       end;

      LInstance := LField.FieldType.AsInstance;

      if LInstance.MetaclassType = TJX2StrObjDic then
      begin
        LAttr := GetRTTIAttribute(LField, JX2AttrClass);
        if LAttr = nil then
          raise Exception.Create('TJX2StrObjDic is missing JX2AttrClass : ' + LField.Name);
        if LJValue.ObjectValue = nil then Continue;
        LNewStrObj := TJX2StrObjDic.Create([doOwnsValues]);
        LNewStrObj.Capacity := LJValue.ObjectValue.Count;
        LField.SetValue(AObj, LNewStrObj);
        for LPair in LJValue.ObjectValue do
        begin
          LNewObj := JX2AttrClass(LAttr).FClass.Create;
          Deserialize(LNewObj, LPair.Value.ObjectValue, ASettings, AStats);
          LNewStrObj.Add(LPair.Name, LNewObj);
        end;
        Continue;
      end;

      if LInstance.MetaclassType = TJX2ObjList then
      begin
        LAttr := GetRTTIAttribute(LField, JX2AttrClass);
        if LAttr = nil then
          raise Exception.Create('TJX2ObjList is missing JX2AttrClass : ' + LField.Name);
        LNewObjList := TJX2ObjList.Create(True);
        LField.SetValue(aObj, LNewObjList);
        LNewObjList.Capacity := LJValue.ArrayValue.Count;
        for LIdx := 0 to LJValue.ArrayValue.count - 1 do
        begin
          LNewObj := JX2AttrClass(LAttr).FClass.Create;
          LNewObjList.Add(LNewObj);
          Deserialize(LNewObj, LJValue.ArrayValue.O[LIdx], ASettings, AStats);
        end;
        Continue
      end;

      if LInstance.MetaclassType = TJX2ValueList then
      begin
        LNewValueList := TJX2ValueList.Create;
        LField.SetValue(AObj, LNewValueList);
        LNewValueList.Capacity := LJValue.ArrayValue.Count;
        if Assigned(AStats) then AStats.IncOpsCount(LJValue.ArrayValue.Count);
        for LIdx := 0 to LJValue.ArrayValue.count - 1 do
          LNewValueList.Add(LJValue.ArrayValue.Values[LIdx].TValueValue);
        Continue;
      end;

{$IFNDEF JSX_NOVAR}
      if LInstance.MetaclassType = TJX2VarList then
      begin
        LNewVarList := TJX2VarList.Create;
        LField.SetValue(AObj, LNewVarList);
        LNewVarList.Capacity := LJValue.ArrayValue.Count;
        if Assigned(AStats) then AStats.IncOpsCount(LJValue.ArrayValue.Count);
        for LIdx := 0 to LJValue.ArrayValue.count - 1 do
          LNewVarList.Add(LJValue.ArrayValue.V[LIdx]);
        Continue
      end;

      if LInstance.MetaclassType = TJX2StrVarDic then
      begin
        LNewStrVar := TJX2StrVarDic.Create;
        LField.SetValue(AObj, LNewStrVar);
        LJsObj := LJValue.ObjectValue;
        if Assigned(AStats) then AStats.IncOpsCount(LJsObj.count);
        for LIdx := 0 to LJsObj.count - 1 do
          LNewStrVar.Add(LJsObj.Names[LIdx],LJsObj.Values[LJsObj.Names[LIdx]]);
        Continue;
      end;
{$ENDIF}

      if LInstance.MetaclassType = TJX2StrValueDic then
      begin
        LNewStrValue := TJX2StrValueDic.Create;
        LField.SetValue(AObj, LNewStrValue);
        LJsObj := LJValue.ObjectValue;
        if Assigned(AStats) then AStats.IncOpsCount(LJsObj.count);
        for LIdx := 0 to LJsObj.count - 1 do
          LNewStrValue.Add(LJsObj.Names[LIdx], LJsObj.Values[LJsObj.Names[LIdx]].TValueValue);
        Continue
      end;

{$IFNDEF JSX_NOVAR}
      if LInstance.MetaclassType = TJX2StrVarDic then
      begin
        LNewStrVar := TJX2StrVarDic.Create;
        LField.SetValue(AObj, LNewStrVar);
        LJsObj := LJValue.ObjectValue;
        if Assigned(AStats) then AStats.IncOpsCount(LJsObj.count);
        for LIdx := 0 to LJsObj.count - 1 do
          LNewStrVar.Add(LJsObj.Names[LIdx], LJsObj.Items[LIdx].Value);
        Continue;
      end;
{$ENDIF}

      begin
        LAttr := GetRTTIAttribute(LField, JX2AttrConv);
        if Assigned(LAttr) then
        begin
          try
           	if not Assigned(JX2AttrConv(LAttr)) then Continue;
            if not Supports(JX2AttrConv(LAttr).FConv.Create, IJX2Converter, LAttrIntf) then Continue;
            LNewObj := LAttrIntf.OnDeserialize(TJX2DataBlock.Create(ASettings, nil, LField, '', AJsonObj, LJValue));
            LField.SetValue(AObj, LNewObj);
          except end;
          Continue;
        end;

        LNewObj := LInstance.MetaclassType.Create;
        Deserialize(LNewObj, LJValue.ObjectValue, ASettings, AStats);
        LField.SetValue(AObj, LNewObj);
        Continue
      end;
    end;

    if LField.FieldType.TypeKind in [tkInterface] then
    begin

      if Assigned(AStats) then AStats.IncOpsCount();

      if LJValue.IsNull then
      begin
        LField.SetValue(aObj, Nil);
        Continue;
      end;

      LAttr := GetRTTIAttribute(LField, JX2AttrConv);
      if Assigned(LAttr) then
      begin
        try
          if not Assigned(JX2AttrConv(LAttr)) then Continue;
          if not Supports(JX2AttrConv(LAttr).FConv.Create, IJX2Converter, LAttrIntf) then Continue;
          LNewObj := LAttrIntf.OnDeserialize(TJX2DataBlock.Create(ASettings, Self, LField, '', AJsonObj));
          LField.SetValue(aObj, LNewObj);
        except end;
        Continue;
      end;

      LAttr := GetRTTIAttribute(LField, JX2AttrClass);
      if LAttr = nil then
        raise Exception.Create('Interface is missing JX2AttrClass : ' + LField.Name);

      if Supports(JX2AttrClass(LAttr).FClass, IJX2ObjList) then
      begin;
        LINewObjList := TIJX2ObjList.Create;
        LField.SetValue(AObj, LINewObjList);
        LINewObjList.Capacity := LJValue.ArrayValue.Count;
        for LIdx := 0 to LJValue.ArrayValue.count - 1 do
        begin
          if Assigned(AStats) then AStats.IncOpsCount();
          LNewObj := JX2AttrClass(LAttr).FData1.Create;
          if Supports(LNewObj, IJX2, LIntf) then
          begin
            LINewObjList.Add(LIntf);
            Deserialize(LNewObj, LJValue.ArrayValue.O[LIdx], ASettings, AStats);
          end;
        end;
        Continue;
      end;

      if Supports(JX2AttrClass(LAttr).FClass, IJX2ValueList) then
      begin
        LINewValList := TIJX2ValueList.Create;
        LINewValList.Capacity := LJValue.ArrayValue.Count;
        LField.SetValue(AObj, LINewValList);
        if Assigned(AStats) then AStats.IncOpsCount(LJValue.ArrayValue.Count);
        for LIdx := 0 to LJValue.ArrayValue.count - 1 do
          LINewValList.Add(LJValue.ArrayValue.Values[LIdx].TValueValue);
        Continue;
      end;

{$IFNDEF JSX_NOVAR}
      if Supports(JX2AttrClass(LAttr).FClass, IJX2VarList) then
      begin
        LINewVarList := TIJX2VarList.Create;
        LINewVarList.Capacity := LJValue.ArrayValue.Count;
        LField.SetValue(AObj, LINewVarList);
        if Assigned(AStats) then AStats.IncOpsCount(LJValue.ArrayValue.Count);
        for LIdx := 0 to LJValue.ArrayValue.count - 1 do
          LINewVarList.Add(LJValue.ArrayValue.V[LIdx]);
        Continue;
      end;

      if Supports(JX2AttrClass(LAttr).FClass, IJX2StrVarDic) then
      begin
        if LJValue.ObjectValue = nil then Continue;
        LINewStrVarDic := TIJX2StrVarDic.Create;
        LField.SetValue(AObj, LINewStrVarDic);
        LJsObj := LJValue.ObjectValue;
        if Assigned(AStats) then AStats.IncOpsCount(LJsObj.count);
        for LIdx := 0 to LJsObj.count - 1 do
          LINewStrVarDic.Add(LJsObj.Names[LIdx], LJsObj.Values[LJsObj.Names[LIdx]].VariantValue);
        Continue;
      end;
{$ENDIF}

      if Supports(JX2AttrClass(LAttr).FClass, IJX2StrValueDic) then
      begin
        if LJValue.ObjectValue = nil then Continue;
        LINewStrValueDic := TIJX2StrValueDic.Create;
        LField.SetValue(AObj, LINewStrValueDic);
        LJsObj := LJValue.ObjectValue;
        if Assigned(AStats) then AStats.IncOpsCount(LJsObj.Count);
        for LIdx := 0 to LJsObj.count - 1 do
          LINewStrValueDic.Add(LJsObj.Names[LIdx], LJsObj.Values[LJsObj.Names[LIdx]].TValueValue);
        Continue;
      end;

      if Supports(JX2AttrClass(LAttr).FClass, IJX2StrObjDic) then
      begin
        if LJValue.ObjectValue = nil then Continue;
        LINewStrObjDic := TIJX2StrObjDic.Create;
        LINewStrObjDic.Capacity := LJValue.ObjectValue.Count;
        LField.SetValue(AObj, LINewStrObjDic);
        for LPair in LJValue.ObjectValue do
        begin
          if Assigned(AStats) then AStats.IncOpsCount();
          LNewObj := JX2AttrClass(LAttr).FData1.Create;
          Deserialize(LNewObj, LPair.Value.ObjectValue, ASettings, AStats);
          Supports(LNewObj, IJX2, LIntf);
          LINewStrObjDic.Add(LPair.Name, LIntf);
        end;
        Continue;
      end;

      begin
        if Assigned(AStats) then AStats.IncOpsCount();
        if LJValue.ObjectValue = nil then Continue;
        LNewObj := JX2AttrClass(LAttr).FClass.Create;
        Deserialize(LNewObj, LJValue.ObjectValue, ASettings, AStats);
        LField.SetValue(AObj, LNewObj);
      end;

    end;
   end;
end;

function TJsonX2.Deserialize<T>(const AJsonStr: string; ASettings: TJX2Settings; AStats: IJX2Stats ): T;
var
  LJsonObj: TJsonBaseObject;
  LObj: T;
  LWatch: TStopWatch;
begin
  LJsonObj := Nil;
  LObj := Nil;
  try
    try
      if Assigned(AStats) then
      begin
        AStats.Clear;
        LWatch := TStopwatch.StartNew;
      end;
      LJsonObj := TJsonObject.Parse(AJsonStr);
      LObj := T.Create;
      Deserialize(LObj, TJsonObject(LJsonObj), ASettings, AStats);
      Result := T(LObj);
    finally
      LJsonObj.Free;
      if Assigned(AStats) then
         AStats.SetDurationMS( LWatch.ElapsedMilliseconds );
    end;
  except
    on Ex: Exception do
    begin
      LObj.Free;
      if (jxoRaiseException in ASettings) then raise Ex;
      Exit(nil);
    end;
  end;
end;

function TJsonX2.Deserialize(AIntfClass: TClass; const AJsonStr: string; ASettings: TJX2Settings; AStats: IJX2Stats): IJX2;
var
  LJsonObj: TJsonBaseObject;
  LTIObj: TObject;
  LWatch: TStopwatch;
begin
  Result := Nil;
  LJsonObj := Nil;
  LTIObj := Nil;
  try
    try
      if Assigned(AStats) then
      begin
        AStats.Clear;
        LWatch := TStopwatch.StartNew;
      end;
      LJsonObj := TJsonObject.Parse(AJsonStr);
      LTIObj := AIntfClass.Create as TObject;
      Deserialize(LTIObj, TJsonObject(LJsonObj), ASettings, AStats);
      supports(LTIObj, IJX2, Result);
    finally
      LJsonObj.Free;
      if Assigned(AStats) then
        AStats.SetDurationMS( LWatch.ElapsedMilliseconds );
    end;
  except
    on Ex: Exception do
    begin
      LTIObj.Free;
      if (jxoRaiseException in ASettings) then raise Ex;
      Exit(nil);
    end;
  end;
end;

class function TJsonX2.Beautifier(const AJsonStr : string; Compact: Boolean = False): string;
var
  LJsonObj: TJsonObject;
begin
  try
    LJsonObj := TJsonObject(TJsonObject.NewInstance);
    try
      LJsonObj.FromJSON(AJsonStr);
      Result := LJsonObj.ToJSon(Compact);
    finally
      LJsonObj.Free;
    end;
  except
    Result := '';
  end;
end;

{$ENDREGION 'TJsonX2'}

initialization
  JX2 := TJsonX2.Create;
finalization
  JX2.Free;
end.

