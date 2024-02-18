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

interface

uses
    Classes
    , System.Generics.Collections
    , System.TypInfo
    , W3DJsonX2.Obj
    , W3DJsonX2.Types
    ;

type

  // JsonX2Patcher

  TJsonXPatcher = class(TObject)
  private type
    TJsonXPatcherCtnr = class
    public
      PHdr, PJsn: string;
    end;
  private
    FParts: TObjectList<TJsonXPatcherCtnr>;
    FLowMemL2: Boolean;
  public
    constructor Create; overload;
    destructor Destroy; override;
    procedure Decode(var AJsonStr: string);
    function Encode(AJsonStr: string; AHeader: string = '"';  AFooter: string = '"'): string;
  end;

  TJsonX2 = class(TObject)
  private
    class var FInstance: TJsonX2;
    procedure InternalSerialize(
                AObj: TObject;
                AJsonObj: TJsonObject;
                AJsonPatcher: TJsonXPatcher;
                ASettings: TJX2Settings
              );
    procedure InternalDeserialize(
                AObj: TObject;
                AJsonObj: TJsonObject;
                ASettings: TJX2Settings
              );
  public
    constructor Create; overload;
    destructor Destroy; override;
    function Serialize(Obj: TObject; ASettings: TJX2Settings = []): string; overload;
    function Serialize(Intf: IInterface; ASettings: TJX2Settings = []): string; overload;
    function Deserialize<T: class, constructor>(AJsonStr: string; ASettings: TJX2Settings = []): T; overload;
    function Deserialize(AIntfClass: TClass; AJsonStr: string; ASettings: TJX2Settings = []): IJX2; overload;

  end;

var
  W3DJSX2 : TJsonX2;

implementation
uses
  RTTI
  , SyncObjs
  , Variants
  , SysUtils
  , W3DJsonX2.Utils
  , W3DJsonX2.RTTI
  ;

{$REGION 'Helpers'}

function LenStr(var AStr: string): Integer; inline;
begin
  Result := PInteger(@PByte(AStr)[-4])^
end;

function EscapeJSONStr(AStr: string): string;
var
  LP: PChar;
  LEndP: PChar;
begin
  Result := '';
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

function VariantToJSONValue(AV: Variant; AForcedString: Boolean = False): string;
begin
  if VarIsStr(AV) or AForcedString then
  begin
    Result := '"' + EscapeJSONStr(AV) + '"';
  end
  else
    Result := VarToStr(AV);
end;

{$REGION}

{$REGION 'TJsonXPatcher'}

constructor TJsonXPatcher.Create;
begin
  FParts := TObjectList<TJsonXPatcherCtnr>.Create(True);
end;

destructor TJsonXPatcher.Destroy;
begin
  FParts.Free;
  inherited;
end;

procedure TJsonXPatcher.Decode(var AJsonStr: string);
var
  Idx, Position: Integer;
begin
  Position :=  LenStr(AJsonStr);
  try
    for Idx := FParts.count - 1 downto 0 do
    begin
      Position := OnceFastReplaceStr(AJsonStr, TJsonXPatcherCtnr(FParts[Idx]).PHdr, TJsonXPatcherCtnr(FParts[Idx]).PJsn, Position, True);
      FParts.Delete(Idx);
    end;
  finally
    FParts.Clear;
  end
end;

function TJsonXPatcher.Encode(AJsonStr, AHeader, AFooter: string): string;
var
  Part: TJsonXPatcherCtnr;
  Len: Int64;
begin
  Result  := StringGUID;
  Part := TJsonXPatcherCtnr.Create;
  if Self.FLowMemL2 then
  begin
    Part.PHdr := AHeader + Result + AFooter;
    Part.PJsn := AJsonStr;
  end else begin
    Len := LenStr(AHeader) + LenStr(Result) + LenStr(AFooter) - LenStr(AJsonStr);
    if Len > 0 then  AJsonStr := AJsonStr + StringOfChar(' ', Len)
    else
    if Len < 0 then  Result := Result + StringOfChar(' ', -Len);
    Part.PHdr := AHeader + Result + AFooter;
    Part.PJsn := AJsonStr;
  end;
  FParts.Add(Part);
end;

{$REGION}

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

procedure  TJsonX2.InternalSerialize(
            AObj: TObject;
            AJsonObj: W3DJsonX2.Obj.TJsonObject;
            AJsonPatcher: TJsonXPatcher;
            ASettings: TJX2Settings
          );
var

  LJsonStr: string;
  LFields: TArray<TRTTIField>;
  LField: TRTTIField;
  LV: variant;
  LJsonName: string;
  LAttr: TCustomAttribute;
  LCurObj: TObject;
  LTypedObj: TObject;
  LVarListObj : TJX2VarList;
  LSL: TStringList;
  LObjListObj: TIJX2ObjList;
  LJsonObj: TJsonObject;
  LObjLoop: IJX2;
  LCurIntf: IJX2;
  LVarVarDicObj: TIJX2VarVarDic;
  LVVDic: TPair<Variant, Variant>;
  LVODic: TPair<Variant, IJX2>;
  LVarObjDicObj: TIJX2VarObjDic;
  LVarListClass: TJX2VarList;
  LObjListClass: TJX2ObjList;
  LObjLoopClass: TObject;
  LObjVarVarDic: TJX2VarVarDic;
  LObjVarObjDic: TJX2VarObjDic;
  LVarObjLoopClass: TPair<Variant, TObject>;
  LVarLoopClass: variant;

  procedure SetToNull(LJsonName: string; ASettings: TJX2Settings);
  begin
    if not (jxoUnassignedToNull in ASettings) then exit;
    if LJsonName.Trim.IsEmpty then exit;
    AJsonObj.InternAddItem(LJsonName).VariantValue := Null;
  end;

begin
  if (AObj = nil) then exit;
  LFields := GetFields(AObj);
  AJsonObj.Capacity := Length(LFields);
  for LField in LFields do
  begin
    LJsonName := LField.Name;
    LAttr := GetFieldAttribute(LField, JX2AttrName);
    if LAttr <> Nil then LJsonName :=  JX2AttrName(LAttr).FName;
    if LField.FieldType.TypeKind in [tkVariant] then
    begin
      LV := LField.GetValue(AObj).AsVariant;
      case FindVarData(LV)^.VType of
        varEmpty:
          SetToNull(LJsonName, ASettings);
        varNull:
          AJsonObj.InternAddItem(LJsonName).VariantValue := Null;
        varOleStr, varString, varUString:
          AJsonObj.InternAddItem(LJsonName).Value := LV;
        varSmallInt, varInteger, varShortInt, varByte, varWord, varLongWord:
          AJsonObj.InternAddItem(LJsonName).IntValue := (LV);
        varDate:
          AJsonObj.InternAddItem(LJsonName).DateTimeValue := (LV);
        varBoolean:
          AJsonObj.InternAddItem(LJsonName).BoolValue := (LV);
        varInt64:
          AJsonObj.InternAddItem(LJsonName).LongValue := (LV);
        varUInt64:
          AJsonObj.InternAddItem(LJsonName).ULongValue := (LV);
        varSingle, varDouble, varCurrency:
          AJsonObj.InternAddItem(LJsonName).FloatValue := (LV);
      else
        SetToNull(LJsonName, ASettings);
      end;
      Continue;
    end else
    if LField.FieldType.TypeKind in [tkClass] then
    begin
      LCurObj := LField.GetValue(AObj).AsObject;
      if LCurObj = Nil then begin SetToNull(LJsonName, ASettings); Continue; end;

      if LCurObj.ClassType = TJX2VarList then
      begin
        LVarListClass := TJX2VarList(LCurObj);
        LSL := TStringList.Create(#0, ',', [soStrictDelimiter]);
        LSL.Capacity := LVarListClass.count + 1;
        for LV in LVarListClass do
          LSL.Add(VariantToJSONValue(LV));
        aJsonObj.AddItem(LJsonName).Value := AJsonPatcher.Encode('"' + LJsonName + '":[' + LSL.DelimitedText + ']', '"' + LJsonName + '":"', '"');
        LSL.Free;
      end else

      if LCurObj.ClassType = TJX2ObjList then
      begin
        LObjListClass := TJX2ObjList(LCurObj);
        LSL := TStringList.Create(#0, ',', [soStrictDelimiter]);
        LSL.Capacity := LObjListClass.count;
        LJsonObj := W3DJsonX2.Obj.TJsonObject(W3DJsonX2.Obj.TJsonObject.NewInstance);
        for LObjLoopClass in LObjListClass do
        begin
          LJsonObj.Clear;
          InternalSerialize(LObjLoopClass, LJsonObj, AJsonPatcher, ASettings);
          LSL.Add(LJsonObj.ToJSON(True));
        end;
        LJsonObj.Free;
        AJsonObj.InternAddItem(LJsonName).Value := AJsonPatcher.Encode('[' + LSL.DelimitedText + ']');
        LSL.Free;
      end else

      if LCurObj.ClassType = TJX2VarVarDic then
      begin
        LObjVarVarDic := TJX2VarVarDic(LCurObj);
        LSL := TStringList.Create(#0, ',', [soStrictDelimiter]);
        LSL.Capacity := LObjVarVarDic.count;
        for LVVDic in LObjVarVarDic do
          LSL.Add(VariantToJSONValue(LVVDic.Key) + ':' + VariantToJSONValue(LVVDic.Value));
        aJsonObj.InternAddItem(LJsonName).Value := AJsonPatcher.Encode('{' + LSL.DelimitedText + '}');
        LSL.Free;
      end
      else

      if LCurObj.ClassType = TJX2VarObjDic then
      begin
        LObjVarObjDic := TJX2VarObjDic(LCurObj);
        LSL := TStringList.Create(#0, ',', [soStrictDelimiter]);
        LSL.Capacity := LObjVarObjDic.count;
        LJsonObj := W3DJsonX2.Obj.TJsonObject(W3DJsonX2.Obj.TJsonObject.NewInstance);
        for LVarObjLoopClass in LObjVarObjDic do
        begin
          LJsonObj.Clear;
          InternalSerialize(LVarObjLoopClass.Value, LJsonObj, AJsonPatcher, ASettings);
          LJsonStr := LJsonObj.ToJSON(True);
          LSL.Add(VariantToJSONValue( LVarObjLoopClass.Key, True)+ ':' + LJsonStr);
        end;
        AJsonObj.AddItem(LJsonName).Value := AJsonPatcher.Encode('"' + LJsonName + '":{' + LSL.DelimitedText + '}', '"' + LJsonName + '":"', '"');
        LJsonObj.Free;
        LSL.Free;
      end else

      begin
        LJsonObj := W3DJsonX2.Obj.TJsonObject(W3DJsonX2.Obj.TJsonObject.NewInstance);
        InternalSerialize(LCurObj, LJsonObj, AJsonPatcher, ASettings);
        aJsonObj.InternAddItem(LJsonName).Value := AJsonPatcher.Encode( LJsonObj.ToJSON(True) );
        LJsonObj.Free;
      end;

      SetToNull(LJsonName, ASettings);
      Continue;
    end else
    if LField.FieldType.TypeKind in [tkInterface] then
    begin
      LCurIntf := LField.GetValue(AObj).AsInterface as IJX2;
      if LCurIntf = Nil then begin SetToNull(LJsonName, ASettings); Continue; end;
      LTypedObj  := LCurIntf as TObject;

      if Supports(LTypedObj, IJX2VarList) then
      begin
        LVarListObj := TJX2VarList(LTypedObj);
        LSL := TStringList.Create(#0, ',', [soStrictDelimiter]);
        LSL.Capacity := LVarListObj.count;
        for LVarLoopClass in LVarListObj do LSL.Add(VariantToJSONValue(LVarLoopClass));
        AJsonObj.AddItem(LJsonName).Value := AJsonPatcher.Encode('"' + LJsonName + '":[' + LSL.DelimitedText + ']', '"' + LJsonName + '":"', '"');
        LSL.Free;
      end else

      if Supports(LTypedObj, IJX2ObjList) then
      begin
        LObjListObj := TIJX2ObjList(LTypedObj);
        LSL := TStringList.Create(#0, ',', [soStrictDelimiter]);
        LSL.Capacity := LObjListObj.count;
        LJsonObj := W3DJsonX2.Obj.TJsonObject(W3DJsonX2.Obj.TJsonObject.NewInstance);
        for LObjLoop in LObjListObj do
        begin
          LJsonObj.Clear;
          InternalSerialize(TObject(LObjLoop), LJsonObj, AJsonPatcher, ASettings);
          LSL.Add(LJsonObj.ToJSON(True));
        end;
        LJsonObj.Free;
        AJsonObj.InternAddItem(LJsonName).Value := AJsonPatcher.Encode('[' + LSL.DelimitedText + ']');
        LSL.Free;
      end else

      if Supports(LTypedObj, IJX2VarVarDic) then
      begin
        LVarVarDicObj := TIJX2VarVarDic(LTypedObj);
        LSL := TStringList.Create(#0, ',', [soStrictDelimiter]);
        LSL.Capacity := LVarVarDicObj.count;
        for LVVDic in LVarVarDicObj do
          LSL.Add(VariantToJSONValue(LVVDic.Key, True) + ':' + VariantToJSONValue(LVVDic.Value));
        AJsonObj.InternAddItem(LJsonName).Value := AJsonPatcher.Encode('{' + LSL.DelimitedText + '}');
        LSL.Free;
      end else

      if Supports(LTypedObj, IJX2VarObjDic) then
      begin
        LVarObjDicObj := TIJX2VarObjDic(LTypedObj);
        LSL := TStringList.Create(#0, ',', [soStrictDelimiter]);
        LSL.Capacity := LVarObjDicObj.count;
        LJsonObj := W3DJsonX2.Obj.TJsonObject(W3DJsonX2.Obj.TJsonObject.NewInstance);
        for LVODic in LVarObjDicObj do
        begin
          LJsonObj.Clear;
          InternalSerialize(TObject(LVODic.Value), LJsonObj, AJsonPatcher, ASettings);
          LSL.Add(VariantToJSONValue(LVODic.Key, True) + ':' +  LJsonObj.ToJSON(True));
        end;
        LJsonObj.Free;
        AJsonObj.AddItem(LJsonName).Value := AJsonPatcher.Encode('"' + LJsonName + '":{' + LSL.DelimitedText + '}', '"' + LJsonName + '":"', '"');
        LSL.Free;
      end else

      if Supports(LTypedObj, IJX2) then
      begin
        LJsonObj := W3DJsonX2.Obj.TJsonObject(W3DJsonX2.Obj.TJsonObject.NewInstance);
        InternalSerialize(LTypedObj, LJsonObj, AJsonPatcher, ASettings);
        AJsonObj.InternAddItem(LJsonName).Value := AJsonPatcher.Encode( LJsonObj.ToJSON(True));
        LJsonObj.Free;
       end

       else SetToNull(LJsonName, ASettings);
      Continue;
    end;
  end; // for LField in LFields do
end;

function TJsonX2.Serialize(Intf: IInterface; ASettings: TJX2Settings = []): string;
begin
  Result := Serialize(Intf as TObject, ASettings);
end;
function TJsonX2.Serialize(Obj: TObject; ASettings: TJX2Settings = []): string;
var
  LJsonObj: TJsonObject;
  LJsonPatcher: TJsonXPatcher;
begin
  Result := '';
  LJsonObj := Nil;
  LJsonPatcher := Nil;
  try
    try
      LJsonObj := W3DJsonX2.Obj.TJsonObject(W3DJsonX2.Obj.TJsonObject.NewInstance);
      LJsonPatcher := TJsonXPatcher.Create;
      InternalSerialize(Obj, LJSonObj, LJsonPatcher, ASettings);
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

{$REGION}

procedure TJsonX2.InternalDeserialize(
            AObj: TObject;
            AJsonObj: TJsonObject;
            ASettings: TJX2Settings);
var
  i: Integer;
  LDteTme: TDateTime;
  LFields: TArray<TRTTIField>;
  LJIdx: Integer;
  LJValue: PJsonDataValue;
  LJName: string;
  LRTTIField: TRTTIField;
  LInstance: TRTTIInstanceType;
  LAttr: TCustomAttribute;
  LNewObj: TObject;
  LNewVarObj: TJX2VarObjDic;
  LPair: TJsonNameValuePair;
  LNewVarVar: TJX2VarVarDic;
  LJsObj : TJsonObject;
  LNewVarList : TJX2VarList;
  LNewObjList: TJX2ObjList;
  LIntf: IJX2;
  LINewVarList: TIJX2VarList;
  LINewObjList: TIJX2ObjList;
  LINewVarVarDic: TIJX2VarVarDic;
  LINewVarObjDic: TIJX2VarObjDic;

  function GetFieldName(AJsonName: string): TRTTIField;
  var
    AAttr: JX2AttrName;
  begin
    for Result in LFields do
    begin
      if AJsonName = Result.Name then Exit;
      AAttr := JX2AttrName(GetFieldAttribute(Result, JX2AttrName));
      if (AAttr <> Nil) and (AAttr.FName = AJsonName) then Exit;
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
    LRTTIField := GetFieldName(LJName);
    if LRTTIField = Nil then Continue;

    if LRTTIField.FieldType.TypeKind in [tkVariant] then
    begin
      if (LJValue.Typ = jdtString) then
      begin
        LDteTme := LJValue.DateTimeValue;
        if (LDteTme <> 0) then
        begin
          LRTTIField.SetValue(aObj, TValue(LDteTme));
        end else
          LRTTIField.SetValue(aObj, TValue(LJValue.Value));
      end
      else
        LRTTIField.SetValue(AObj, TValue.FromVariant(LJValue.VariantValue));
    continue
    end else

    if LRTTIField.FieldType.TypeKind in [tkClass] then
    begin
      LInstance := LRTTIField.FieldType.AsInstance;

      if LInstance.MetaclassType = TJX2VarObjDic then
      begin
        if LJValue.IsNull then
        begin
           LRTTIField.SetValue(AObj, Nil);
           Continue;
        end;
        LAttr := LRTTIField.GetAttribute<JX2AttrClass>;
        if LAttr = Nil then
          raise Exception.Create('TJX2VarObjDic is missing JX2AttrClass : ' + LRTTIField.Name);
        if LJValue.ObjectValue = Nil then Continue;
        LNewVarObj := TJX2VarObjDic.Create([doOwnsValues]);
        LNewVarObj.Capacity := LJValue.ObjectValue.Count;
        LRTTIField.SetValue(AObj, LNewVarObj);
        for LPair in LJValue.ObjectValue do
        begin
          LNewObj := JX2AttrClass(LAttr).FClass.Create;
          InternalDeserialize(LNewObj, LPair.Value.ObjectValue, ASettings);
          LNewVarObj.Add(LPair.Name, LNewObj);
        end;
        Continue;
      end else

      if LInstance.MetaclassType = TJX2ObjList then
      begin
        if LJValue.isNull then
        begin
           LRTTIField.SetValue(AObj, Nil);
           Continue;
        end;
        LAttr := LRTTIField.GetAttribute<JX2AttrClass>;
        if LAttr = Nil then
          raise Exception.Create('TJX2ObjList is missing JX2AttrClass : ' + LRTTIField.Name);
        if (LJValue.IsNull) then Continue;
        LNewObjList := TJX2ObjList.Create(True);
        LRTTIField.SetValue(aObj, LNewObjList);
        LNewObjList.Capacity := LJValue.ArrayValue.Count;
        for i := 0 to LJValue.ArrayValue.count - 1 do
        begin
          LNewObj := JX2AttrClass(LAttr).FClass.Create;
          LNewObjList.Add(LNewObj);
          InternalDeserialize(LNewObj, LJValue.ArrayValue.O[i], ASettings);
        end;
        Continue
      end else

      if LInstance.MetaclassType = TJX2VarList then
      begin
        if LJValue.isNull then
        begin
           LRTTIField.SetValue(AObj, Nil);
           Continue;
        end;
        LNewVarList := TJX2VarList.Create;
        LRTTIField.SetValue(AObj, LNewVarList);
        LNewVarList.Capacity := LJValue.ArrayValue.Count;
        for i := 0 to LJValue.ArrayValue.count - 1 do
          LNewVarList.Add(LJValue.ArrayValue.V[i]);
        Continue
      end else

      if LInstance.MetaclassType = TJX2VarVarDic then
      begin
        if LJValue.isNull then
        begin
           LRTTIField.SetValue(AObj, Nil);
           Continue;
        end;
        LNewVarVar := TJX2VarVarDic.Create;
        LRTTIField.SetValue(AObj, LNewVarVar);
        LJsObj := LJValue.ObjectValue;
        for i := 0 to LJsObj.count - 1 do
          LNewVarVar.Add(LJsObj.Names[i], LJsObj.Items[i].Value);
        Continue
      end else

      begin
        if LJValue.isNull then
        begin
           LRTTIField.SetValue(AObj, Nil);
           Continue;
        end;
        LNewObj := LInstance.MetaclassType.Create;
        InternalDeserialize(LNewObj, LJValue.ObjectValue, ASettings);
        LRTTIField.SetValue(AObj, LNewObj);
        Continue
      end;

    end else

    if LRTTIField.FieldType.TypeKind in [tkInterface] then
    begin

      LAttr := LRTTIField.GetAttribute<JX2AttrClass>;
      if LAttr = Nil then
        raise Exception.Create('Interface is missing JX2AttrClass : ' + LRTTIField.Name);
      if LJValue.IsNull then
      begin
        LRTTIField.SetValue(aObj, Nil);
        Continue;
      end;

      if Supports(JX2AttrClass(LAttr).FClass, IJX2ObjList) then
      begin;
        LINewObjList := TIJX2ObjList.Create;
        LRTTIField.SetValue(aObj, LINewObjList);
        LINewObjList.Capacity := LJValue.ArrayValue.Count;
        for i := 0 to LJValue.ArrayValue.count - 1 do
        begin
          LNewObj := JX2AttrClass(LAttr).FData1.Create;
          Supports(LNewObj, IJX2, LIntf);
          LINewObjList.Add(LIntf);
          InternalDeserialize(LNewObj, LJValue.ArrayValue.O[i], ASettings);
        end;
      end else

      if Supports(JX2AttrClass(LAttr).FClass, IJX2VarList) then
      begin
        LINewVarList := TIJX2VarList.Create;
        LINewVarList.Capacity := LJValue.ArrayValue.Count;
        LRTTIField.SetValue(AObj, LINewVarList);
        for i := 0 to LJValue.ArrayValue.count - 1 do
          LINewVarList.Add(LJValue.ArrayValue.V[i]);
      end else

      if Supports(JX2AttrClass(LAttr).FClass, IJX2VarVarDic) then
      begin
        if LJValue.ObjectValue = Nil then Continue;
        LINewVarVarDic := TIJX2VarVarDic.Create;
        LRTTIField.SetValue(AObj, LINewVarVarDic);
        LJsObj := LJValue.ObjectValue;
        for i := 0 to LJsObj.count - 1 do
          LINewVarVarDic.Add(LJsObj.Names[i], LJsObj.Values[LJsObj.Names[i]]);
      end else

      if Supports(JX2AttrClass(LAttr).FClass, IJX2VarObjDic) then
      begin
        if LJValue.ObjectValue = Nil then Continue;
        LINewVarObjDic := TIJX2VarObjDic.Create;
        LINewVarObjDic.Capacity := LJValue.ObjectValue.Count;
        LRTTIField.SetValue(AObj, LINewVarObjDic);
        for LPair in LJValue.ObjectValue do
        begin
          LNewObj := JX2AttrClass(LAttr).FData1.Create;
          InternalDeserialize(LNewObj, LPair.Value.ObjectValue, ASettings);
          Supports(LNewObj, IJX2, LIntf);
          LINewVarObjDic.Add(LPair.Name, LIntf);
        end;
      end else

      begin
        if LJValue.ObjectValue = Nil then Continue;
        LNewObj := JX2AttrClass(LAttr).FClass.Create;
        InternalDeserialize(LNewObj, LJValue.ObjectValue, ASettings);
        LRTTIField.SetValue(AObj, LNewObj);
      end;

    end;
   end;
end;

function TJsonX2.Deserialize<T>(AJsonStr: string; ASettings: TJX2Settings = []): T;
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
      InternalDeserialize(LObj, W3DJsonX2.Obj.TJsonObject(LJsonObj),  ASettings);
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

function TJsonX2.Deserialize(AIntfClass: TClass; AJsonStr: string; ASettings: TJX2Settings = []): IJX2;
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
      InternalDeserialize(LTIObj, W3DJsonX2.Obj.TJsonObject(LJsonObj),  ASettings);
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

initialization
  W3DJSX2 := TJsonX2.Create;
finalization
  W3DJSX2.Free;
end.




