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

  // JsonX2Patcher

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

var
  W3DJX2 : TJsonX2;

implementation
uses
  RTTI
{$IFNDEF JSX_NOVAR}
  , Variants
{$ENDIF}
  , DateUtils
  , SyncObjs
  , SysUtils
  , W3DJsonX2.Utils
  , W3DJsonX2.RTTI
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

procedure  TJsonX2.Serialize(
            AObj: TObject;
            AJsonObj: W3DJsonX2.Obj.TJsonObject;
            AJsonPatcher: TJX2Patcher;
            ASettings: TJX2Settings
          );
var

  {$IFNDEF JSX_NOVAR}
  LV: variant;
  Lvar: variant;
  LVarLoop: variant;
  LStrVarDic: TPair<string, Variant>;
  LVarObjPair: TPair<Variant, IJX2>;
  LVarObjLoopClass: TPair<Variant, TObject>;
  LStrVarDicIntf: IJX2StrVarDic;
  LVarObjDicObj: TIJX2VarObjDic;
  LVarListClass: TJX2VarList;
  LObjStrVarDic: TJX2StrVarDic;
  LObjVarObjDic: TJX2VarObjDic;
  {$ENDIF}
  LJsonStr: string;
  LFields: TArray<TRTTIField>;
  LField: TRTTIField;
  LVal: TValue;
  LJsonName: string;
  LAttr: TCustomAttribute;
  LCurObj: TObject;
  LTypedObj: TObject;
  LValueListObj: TIJX2ValueList;
  LSL: TStringList;
  LObjListObj: TIJX2ObjList;
  LJsonObj: TJsonObject;
  LObjLoop: IJX2;
  LCurIntf: IJX2;
  LStrValueDicIntf: IJX2StrValueDic;
  LValObjDicIntf: IJX2ValueObjDic;
  LStrValueObj: TJX2StrValueDic;
  LVarListIntf: IJX2VarList;
  LValObjPair: TPair<TValue, IJX2>;
  LStrValue: TPair<string, TValue>;
  LStrObjPair: TPair<string, IJX2>;
  LValueListClass: TJX2ValueList;
  LObjListClass: TJX2ObjList;
  LObjLoopClass: TObject;
  LObjStrObjDic: TJX2StrObjDic;
  LObjValueObjDic: TJX2ValueObjDic;
  LStrObjLoopClass: TPair<string, TObject>;
  LValueObjLoopClass: TPair<TValue, TObject>;

  LValueLoopClass: TValue;
  LTValue: TValue;
  LDouble: Double;
  LErrorMsgType: string;
  LAttrIntf: IJX2Converter;


  procedure SetToNull(LJsonName: string; ASettings: TJX2Settings);
  begin
    (*
    if not (jxoUnassignedToNull in ASettings) then exit;
    if LJsonName.Trim.IsEmpty then exit;
    *)
    AJsonObj.InternAddItem(LJsonName).ObjectValue := nil;
  end;

begin
  if (AObj = nil) then exit;
  LFields := GetFields(AObj);
  AJsonObj.Capacity := Length(LFields);
  for LField in LFields do
  begin
    LJsonName := LField.Name;
    LAttr := GetFieldAttribute(LField, JX2AttrName);
    if not Assigned(LAttr) and (jxExplicitBinding in ASettings) then Continue;
    if Assigned( GetFieldAttribute(LField, JX2AttrExclude) ) then Continue;
    if Assigned(LAttr) then LJsonName :=  JX2AttrName(LAttr).FName;
{$IFNDEF JSX_NOVAR}
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
{$ENDIF}
    if LField.FieldType.TypeKind in [tkRecord] then
    begin
      if LField.FieldType.Handle = TypeInfo(TValue) then
      begin
        if not LField.GetValue(AObj).TryAsType<TValue>(LTValue) then
          Continue;
        LErrorMsgType := '';
        case LTValue.Kind of
          tkUnknown:
            begin
              //if not (jxoUnassignedToNull in ASettings) then Continue;
              AJsonObj.InternAddItem(LJsonName).ObjectValue := nil;
            end;
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
          tkSet: LErrorMsgType := 'tkSet';
          tkClass: LErrorMsgType := 'tkClass';
          tkMethod: LErrorMsgType := 'tkMethod';
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
          tkArray: LErrorMsgType := 'tkArray';
          tkRecord: LErrorMsgType := 'tkRecord';
          tkInterface: LErrorMsgType := 'tkInterface';
          tkInt64: AJsonObj.InternAddItem(LJsonName).LongValue := LTValue.AsInt64;
          tkDynArray: LErrorMsgType := 'tkDynArray';
          tkUString: AJsonObj.InternAddItem(LJsonName).Value := LTValue.asString;
          tkClassRef: LErrorMsgType := 'tkClassRef';
          tkPointer: LErrorMsgType := 'tkPointer';
          tkProcedure: LErrorMsgType := 'tkProcedure';
          tkMRecord: LErrorMsgType := 'tkMRecord';
        end;
        if LErrorMsgType <> '' then
          raise Exception.Create(Format('Serialize TValue ( %s ) id %s !', [LJsonName, LErrorMsgType]));

      end;
      Continue;
    end else

    if LField.FieldType.TypeKind in [tkClass] then
    begin
      LCurObj := LField.GetValue(AObj).AsObject;
      if LCurObj = Nil then begin SetToNull(LJsonName, ASettings); Continue; end;

      if LCurObj.ClassType = TJX2ValueList then
      begin
        LValueListClass := TJX2ValueList(LCurObj);
        LSL := TStringList.Create(#0, ',', [soStrictDelimiter]);
        LSL.Capacity := LValueListClass.count + 1;
        for LVal in LValueListClass do
          LSL.Add(ValueToJSONValue(LVal));
        aJsonObj.AddItem(LJsonName).Value := AJsonPatcher.Encode('"' + LJsonName + '":[' + LSL.DelimitedText + ']', '"' + LJsonName + '":"', '"');
        LSL.Free;
      end else
{$IFNDEF JSX_NOVAR}
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
{$ENDIF}
      if LCurObj.ClassType = TJX2ObjList then
      begin
        LObjListClass := TJX2ObjList(LCurObj);
        LSL := TStringList.Create(#0, ',', [soStrictDelimiter]);
        LSL.Capacity := LObjListClass.count;
        LJsonObj := TJsonObject(TJsonObject.NewInstance);
        for LObjLoopClass in LObjListClass do
        begin
          LJsonObj.Clear;
          Serialize(LObjLoopClass, LJsonObj, AJsonPatcher, ASettings);
          LSL.Add(LJsonObj.ToJSON(True));
        end;
        LJsonObj.Free;
        AJsonObj.InternAddItem(LJsonName).Value := AJsonPatcher.Encode('[' + LSL.DelimitedText + ']');
        LSL.Free;
      end else

{$IFNDEF JSX_NOVAR}
      if LCurObj.ClassType = TJX2StrVarDic then
      begin
        LObjStrVarDic := TJX2StrVarDic(LCurObj);
        LSL := TStringList.Create(#0, ',', [soStrictDelimiter]);
        LSL.Capacity := LObjStrVarDic.count;
        for LStrVarDic in LObjStrVarDic do
          LSL.Add(StrToJSONValue(LStrVarDic.Key) + ':' + VariantToJSONValue(LStrVarDic.Value));
        aJsonObj.InternAddItem(LJsonName).Value := AJsonPatcher.Encode('{' + LSL.DelimitedText + '}');
        LSL.Free;
      end
      else
{$ENDIF}

      if LCurObj.ClassType = TJX2StrValueDic then
      begin
        LStrValueObj := TJX2StrValueDic(LCurObj);
        LSL := TStringList.Create(#0, ',', [soStrictDelimiter]);
        LSL.Capacity := LStrValueObj.count;
        for LStrValue in LStrValueObj do
          LSL.Add(StrToJSONValue(LStrValue.Key) + ':' + ValueToJSONValue(LStrValue.Value));
        aJsonObj.InternAddItem(LJsonName).Value := AJsonPatcher.Encode('{' + LSL.DelimitedText + '}');
        LSL.Free;
      end
      else

      if LCurObj.ClassType = TJX2ValueObjDic then
      begin
        LObjValueObjDic := TJX2ValueObjDic(LCurObj);
        LSL := TStringList.Create(#0, ',', [soStrictDelimiter]);
        LSL.Capacity := LObjValueObjDic.Count;
        LJsonObj := TJsonObject(TJsonObject.NewInstance);
        for LValueObjLoopClass in LObjValueObjDic do
        begin
          LJsonObj.Clear;
          Serialize(LValueObjLoopClass.Value, LJsonObj, AJsonPatcher, ASettings);
          LJsonStr := LJsonObj.ToJSON(True);
          LSL.Add(ValueToJSONValue(LValueObjLoopClass.Key, True)+ ':' + LJsonStr);
        end;
        AJsonObj.AddItem(LJsonName).Value := AJsonPatcher.Encode('"' + LJsonName + '":{' + LSL.DelimitedText + '}', '"' + LJsonName + '":"', '"');
        LJsonObj.Free;
        LSL.Free;
      end else

{$IFNDEF JSX_NOVAR}
      if LCurObj.ClassType = TJX2VarObjDic then
      begin
        LObjVarObjDic := TJX2VarObjDic(LCurObj);
        LSL := TStringList.Create(#0, ',', [soStrictDelimiter]);
        LSL.Capacity := LObjVarObjDic.count;
        LJsonObj := TJsonObject(TJsonObject.NewInstance);
        for LVarObjLoopClass in LObjVarObjDic do
        begin
          LJsonObj.Clear;
          Serialize(LVarObjLoopClass.Value, LJsonObj, AJsonPatcher, ASettings);
          LJsonStr := LJsonObj.ToJSON(True);
          LSL.Add(VariantToJSONValue( LVarObjLoopClass.Key, True)+ ':' + LJsonStr);
        end;
        AJsonObj.AddItem(LJsonName).Value := AJsonPatcher.Encode('"' + LJsonName + '":{' + LSL.DelimitedText + '}', '"' + LJsonName + '":"', '"');
        LJsonObj.Free;
        LSL.Free;
      end else
{$ENDIF}

      if LCurObj.ClassType = TJX2StrObjDic then
      begin
        LObjStrObjDic := TJX2StrObjDic(LCurObj);
        LSL := TStringList.Create(#0, ',', [soStrictDelimiter]);
        LSL.Capacity := LObjStrObjDic.count;
        LJsonObj := TJsonObject(TJsonObject.NewInstance);
        for LStrObjLoopClass in LObjStrObjDic do
        begin
          LJsonObj.Clear;
          Serialize(LStrObjLoopClass.Value, LJsonObj, AJsonPatcher, ASettings);
          LJsonStr := LJsonObj.ToJSON(True);
          LSL.Add(StrToJSONValue(LStrObjLoopClass.Key) + ':' + LJsonStr);
        end;
        AJsonObj.AddItem(LJsonName).Value := AJsonPatcher.Encode('"' + LJsonName + '":{' + LSL.DelimitedText + '}', '"' + LJsonName + '":"', '"');
        LJsonObj.Free;
        LSL.Free;
      end else

      begin

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

        LJsonObj := TJsonObject(W3DJsonX2.Obj.TJsonObject.NewInstance);
        Serialize(LCurObj, LJsonObj, AJsonPatcher, ASettings);
        aJsonObj.InternAddItem(LJsonName).Value := AJsonPatcher.Encode( LJsonObj.ToJSON(True) );
        LJsonObj.Free;
      end;

      SetToNull(LJsonName, ASettings);
      Continue;
    end else
    if LField.FieldType.TypeKind in [tkInterface] then
    begin
      if LField.GetValue(AObj).AsInterface = Nil then begin SetToNull(LJsonName, ASettings); Continue; end;
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
        end else
        	begin SetToNull(LJsonName, ASettings); Continue; end;
      end;
      LTypedObj  := LCurIntf as TObject;

      if Supports(LTypedObj, IJX2ValueList) then
      begin
        LValueListObj := TIJX2ValueList(LTypedObj);
        LSL := TStringList.Create(#0, ',', [soStrictDelimiter]);
        LSL.Capacity := LValueListObj.Count;
        for LValueLoopClass in LValueListObj do LSL.Add(ValueToJSONValue(LValueLoopClass));
        AJsonObj.AddItem(LJsonName).Value := AJsonPatcher.Encode('"' + LJsonName + '":[' + LSL.DelimitedText + ']', '"' + LJsonName + '":"', '"');
        LSL.Free;
		Continue;
      end else

{$IFNDEF JSX_NOVAR}
      if Supports(LTypedObj, IJX2VarList) then
      begin
        LVarListIntf := TIJX2VarList(LTypedObj);
        LSL := TStringList.Create(#0, ',', [soStrictDelimiter]);
        LSL.Capacity := TIJX2VarList(LVarListIntf).Count;
        for LVarLoop in  TIJX2VarList(LVarListIntf) do LSL.Add(VariantToJSONValue(LVarLoop));
        AJsonObj.AddItem(LJsonName).Value := AJsonPatcher.Encode('"' + LJsonName + '":[' + LSL.DelimitedText + ']', '"' + LJsonName + '":"', '"');
        LSL.Free;
		Continue;
      end else
{$ENDIF}
      if Supports(LTypedObj, IJX2ObjList) then
      begin
        LObjListObj := TIJX2ObjList(LTypedObj);
        LSL := TStringList.Create(#0, ',', [soStrictDelimiter]);
        LSL.Capacity := LObjListObj.count;
        LJsonObj := TJsonObject(TJsonObject.NewInstance);
        for LObjLoop in LObjListObj do
        begin
          LJsonObj.Clear;
          Serialize(TObject(LObjLoop), LJsonObj, AJsonPatcher, ASettings);
          LSL.Add(LJsonObj.ToJSON(True));
        end;
        LJsonObj.Free;
        AJsonObj.InternAddItem(LJsonName).Value := AJsonPatcher.Encode('[' + LSL.DelimitedText + ']');
        LSL.Free;
		Continue;
      end else

{$IFNDEF JSX_NOVAR}
      if Supports(LTypedObj, IJX2StrVarDic) then
      begin
        LStrVarDicIntf := TIJX2StrVarDic(LTypedObj);
        LSL := TStringList.Create(#0, ',', [soStrictDelimiter]);
        LSL.Capacity := TIJX2StrVarDic(LStrVarDicIntf).count;
        for LStrVarDic in TIJX2StrVarDic(LStrVarDicIntf) do
          LSL.Add(StrToJSONValue(LStrVarDic.Key) + ':' + VariantToJSONValue(LStrVarDic.Value));
        AJsonObj.InternAddItem(LJsonName).Value := AJsonPatcher.Encode('{' + LSL.DelimitedText + '}');
        LSL.Free;
		Continue;
      end else
{$ENDIF}

      if Supports(LTypedObj, IJX2StrValueDic) then
      begin
        LStrValueDicIntf := TIJX2StrValueDic(LTypedObj);
        LSL := TStringList.Create(#0, ',', [soStrictDelimiter]);
        LSL.Capacity := TIJX2StrValueDic(LStrValueDicIntf).count;
        for LStrValue in TIJX2StrValueDic(LStrValueDicIntf) do
          LSL.Add(StrToJSONValue(LStrValue.Key) + ':' + ValueToJSONValue(LStrValue.Value));
        AJsonObj.InternAddItem(LJsonName).Value := AJsonPatcher.Encode('{' + LSL.DelimitedText + '}');
        LSL.Free;
    Continue;
      end else

      if Supports(LTypedObj, IJX2StrObjDic) then
      begin
        var LStrObjDicIntf := TIJX2StrObjDic(LTypedObj);
        LSL := TStringList.Create(#0, ',', [soStrictDelimiter]);
        LSL.Capacity := LStrObjDicIntf.count;
        LJsonObj := TJsonObject(TJsonObject.NewInstance);
        for LStrObjPair in LStrObjDicIntf do
        begin
          LJsonObj.Clear;
          Serialize(TObject(LStrObjPair.Value), LJsonObj, AJsonPatcher, ASettings);
          LSL.Add(StrToJSONValue(LStrObjPair.Key) + ':' +  LJsonObj.ToJSON(True));
        end;
        LJsonObj.Free;
        AJsonObj.AddItem(LJsonName).Value := AJsonPatcher.Encode('"' + LJsonName + '":{' + LSL.DelimitedText + '}', '"' + LJsonName + '":"', '"');
        LSL.Free;
		Continue;
      end else

      if Supports(LTypedObj, IJX2ValueObjDic) then
      begin
        LValObjDicIntf := TIJX2ValueObjDic(LTypedObj);
        LSL := TStringList.Create(#0, ',', [soStrictDelimiter]);
        LSL.Capacity := TIJX2ValueObjDic(LValObjDicIntf).count;
        LJsonObj := TJsonObject(TJsonObject.NewInstance);
        for LValObjPair in TIJX2ValueObjDic(LValObjDicIntf) do
        begin
          LJsonObj.Clear;
          Serialize(TObject(LValObjPair.Value), LJsonObj, AJsonPatcher, ASettings);
          LSL.Add(ValueToJSONValue(LValObjPair.Key, True) + ':' +  LJsonObj.ToJSON(True));
        end;
        LJsonObj.Free;
        AJsonObj.AddItem(LJsonName).Value := AJsonPatcher.Encode('"' + LJsonName + '":{' + LSL.DelimitedText + '}', '"' + LJsonName + '":"', '"');
        LSL.Free;
		Continue;
      end else

{$IFNDEF JSX_NOVAR}
      if Supports(LTypedObj, IJX2VarObjDic) then
      begin
        LVarObjDicObj := TIJX2VarObjDic(LTypedObj);
        LSL := TStringList.Create(#0, ',', [soStrictDelimiter]);
        LSL.Capacity := LVarObjDicObj.count;
        LJsonObj :=TJsonObject(TJsonObject.NewInstance);
        for LVarObjPair in LVarObjDicObj do
        begin
          LJsonObj.Clear;
          Serialize(TObject(LVarObjPair.Value), LJsonObj, AJsonPatcher, ASettings);
          LSL.Add(VariantToJSONValue(LVarObjPair.Key, True) + ':' +  LJsonObj.ToJSON(True));
        end;
        LJsonObj.Free;
        AJsonObj.AddItem(LJsonName).Value := AJsonPatcher.Encode('"' + LJsonName + '":{' + LSL.DelimitedText + '}', '"' + LJsonName + '":"', '"');
        LSL.Free;
		Continue;
      end else
{$ENDIF}

      if Supports(LTypedObj, IJX2) then
      begin
        LJsonObj := W3DJsonX2.Obj.TJsonObject(W3DJsonX2.Obj.TJsonObject.NewInstance);
        Serialize(LTypedObj, LJsonObj, AJsonPatcher, ASettings);
        AJsonObj.InternAddItem(LJsonName).Value := AJsonPatcher.Encode( LJsonObj.ToJSON(True));
        LJsonObj.Free;
       end

       else SetToNull(LJsonName, ASettings);

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
  LJsonPatcher: TJX2Patcher;
begin
  Result := '';
  LJsonObj := Nil;
  LJsonPatcher := Nil;
  try
    try
      LJsonObj := W3DJsonX2.Obj.TJsonObject(W3DJsonX2.Obj.TJsonObject.NewInstance);
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

{$REGION}

procedure TJsonX2.Deserialize(
            AObj: TObject;
            AJsonObj: TJsonObject;
            ASettings: TJX2Settings);
var
  {$IFNDEF JSX_NOVAR}
  LNewVarObj: TJX2VarObjDic;
  LNewStrVar: TJX2StrVarDic;
  LNewVarList : TJX2VarList;
  LINewVarList: TIJX2VarList;
  LINewStrVarDic: TIJX2StrVarDic;
  LINewVarObjDic: TIJX2VarObjDic;
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
  LNewValueObj: TJX2ValueObjDic;
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

  function GetFieldName(const AJsonName: string; var AExplicit: Boolean): TRTTIField;
  var
    AAttr: JX2AttrName;
  begin
    AExplicit := False;
    for Result in LFields do
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
    LRTTIField := GetFieldName(LJName, LExplicit);
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

      if LInstance.MetaclassType = TJX2ValueObjDic then
      begin
        LAttr := GetFieldAttribute(LRTTIField, JX2AttrClass);
        if LAttr = Nil then
          raise Exception.Create('TJX2ValueObjDic is missing JX2AttrClass : ' + LRTTIField.Name);
        if LJValue.ObjectValue = Nil then Continue;
        LNewValueObj := TJX2ValueObjDic.Create([doOwnsValues]);
        LNewValueObj.Capacity := LJValue.ObjectValue.Count;
        LRTTIField.SetValue(AObj, LNewValueObj);
        for LPair in LJValue.ObjectValue do
        begin
          LNewObj := JX2AttrClass(LAttr).FClass.Create;
          Deserialize(LNewObj, LPair.Value.ObjectValue, ASettings);
          LNewValueObj.Add(LPair.Name, LNewObj);
        end;
        Continue;
      end else

      {$IFNDEF JSX_NOVAR}
      if LInstance.MetaclassType = TJX2VarObjDic then
      begin
        LAttr := GetFieldAttribute(LRTTIField, JX2AttrClass);
        if LAttr = Nil then
          raise Exception.Create('TJX2VarObjDic is missing JX2AttrClass : ' + LRTTIField.Name);
        if LJValue.ObjectValue = Nil then Continue;
        LNewVarObj := TJX2VarObjDic.Create([doOwnsValues]);
        LNewVarObj.Capacity := LJValue.ObjectValue.Count;
        LRTTIField.SetValue(AObj, LNewVarObj);
        for LPair in LJValue.ObjectValue do
        begin
          LNewObj := JX2AttrClass(LAttr).FClass.Create;
          Deserialize(LNewObj, LPair.Value.ObjectValue, ASettings);
          LNewVarObj.Add(LPair.Name, LNewObj);
        end;
        Continue;
      end else
      {$ENDIF}

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
        if (LJValue.IsNull) then Continue;
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
            LNewObj := LAttrIntf.OnDeserialize(TJX2DataBlock.Create(ASettings, Self, LRTTIField, '', AJsonObj, LJValue));
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
      end else

      if Supports(JX2AttrClass(LAttr).FClass, IJX2ValueList) then
      begin
        LINewValList := TIJX2ValueList.Create;
        LINewValList.Capacity := LJValue.ArrayValue.Count;
        LRTTIField.SetValue(AObj, LINewValList);
        for LIdx := 0 to LJValue.ArrayValue.count - 1 do
          LINewValList.Add(JsonToTValue(LJValue.ArrayValue.Values[LIdx]));
      end else

      {$IFNDEF JSX_NOVAR}
      if Supports(JX2AttrClass(LAttr).FClass, IJX2VarList) then
      begin
        LINewVarList := TIJX2VarList.Create;
        LINewVarList.Capacity := LJValue.ArrayValue.Count;
        LRTTIField.SetValue(AObj, LINewVarList);
        for LIdx := 0 to LJValue.ArrayValue.count - 1 do
          LINewVarList.Add(LJValue.ArrayValue.V[LIdx]);
      end else

      if Supports(JX2AttrClass(LAttr).FClass, IJX2StrVarDic) then
      begin
        if LJValue.ObjectValue = Nil then Continue;
        LINewStrVarDic := TIJX2StrVarDic.Create;
        LRTTIField.SetValue(AObj, LINewStrVarDic);
        LJsObj := LJValue.ObjectValue;
        for LIdx := 0 to LJsObj.count - 1 do
          LINewStrVarDic.Add(LJsObj.Names[LIdx], LJsObj.Values[LJsObj.Names[LIdx]].VariantValue);
      end else
      {$ENDIF}

      if Supports(JX2AttrClass(LAttr).FClass, IJX2StrValueDic) then
      begin
        if LJValue.ObjectValue = Nil then Continue;
        LINewStrValueDic := TIJX2StrValueDic.Create;
        LRTTIField.SetValue(AObj, LINewStrValueDic);
        LJsObj := LJValue.ObjectValue;
        for LIdx := 0 to LJsObj.count - 1 do
          LINewStrValueDic.Add(LJsObj.Names[LIdx], JsonToTValue(LJsObj.Values[LJsObj.Names[LIdx]]));
      end else

      {$IFNDEF JSX_NOVAR}
      if Supports(JX2AttrClass(LAttr).FClass, IJX2VarObjDic) then
      begin
        if LJValue.ObjectValue = Nil then Continue;
        LINewVarObjDic := TIJX2VarObjDic.Create;
        LINewVarObjDic.Capacity := LJValue.ObjectValue.Count;
        LRTTIField.SetValue(AObj, LINewVarObjDic);
        for LPair in LJValue.ObjectValue do
        begin
          LNewObj := JX2AttrClass(LAttr).FData1.Create;
          Deserialize(LNewObj, LPair.Value.ObjectValue, ASettings);
          Supports(LNewObj, IJX2, LIntf);
          LINewVarObjDic.Add(LPair.Name, LIntf);
        end;
      end else
      {$ENDIF}

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
      end else

      begin
        if LJValue.ObjectValue = Nil then Continue;
        LNewObj := JX2AttrClass(LAttr).FClass.Create;
        Deserialize(LNewObj, LJValue.ObjectValue, ASettings);
        LRTTIField.SetValue(AObj, LNewObj);
      end;

    end;
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
  LJsonObj := TJsonObject(W3DJsonX2.Obj.TJsonObject.NewInstance);
  try
    LJsonObj.FromJSON(AJsonStr);
    Result := LJsonObj.ToJSon(Compact);
  finally
    LJsonObj.Free;
  end;
end;

initialization
  W3DJX2 := TJsonX2.Create;
finalization
  W3DJX2.Free;
end.

