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

unit W3DJsonX2.Types;

interface

uses
  Classes
  , System.Generics.Collections
  , W3DCloneable
  ;

type

  TJX2Setting = (
    jxoReturnEmptyObject,
    jxoReturnEmptyJsonString,
    jxoUnassignedToNull,
    jxoWarnOnMissingField,
    jxoRaiseException,
    jxoPropertiesOnly
  );
  TJX2Settings = set of TJX2Setting;


{$REGION 'Attributes'}

  JX2AttrClass = class(TCustomAttribute)
  public
    FClass: TClass;
    FData1: TClass;
    FData2: TClass;
    constructor Create(const AClass: TClass; const AData1: TClass = Nil ; const AData2: TClass = Nil);
  end;

  JX2AttrName = class(TCustomAttribute)
  public
    FName: string;
    constructor Create(const AName: string);
  end;

{$ENDREGION}

{$REGION 'Interfaces'}

  IJX2 = interface(IW3DCloneable)
  ['{CA357255-FAC5-4D38-8980-4B89875FE491}']
    procedure CloneTo(ADestIntf: IJX2);
    function  CloneSelf: IJX2;
  end;

  TIJX2 = class(TInterfacedObject, IJX2, IW3DCloneable)
    destructor Destroy; override;
    procedure CloneTo(ADestIntf: IJX2);
    function CloneSelf: IJX2;
    function Clone: IW3DCloneable;
  end;

  IJX2VarList = Interface
  ['{CAD572B8-B99C-405C-8946-B10988518E83}']
  end;

  TIJX2VarList = class(TList<Variant>, IJX2VarList, IJX2)
   protected
    _FRefCount: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function CloneSelf: IJX2;
    procedure CloneTo(ADestIntf: IJX2);
   function Clone: IW3DCloneable;
  end;

  IJX2ObjList = Interface
  ['{A4BF9FFE-1CCB-45F7-A1B6-F643C2895483}']
  end;

  TIJX2ObjList = class(TList<IJX2>, IJX2ObjList, IJX2)
   protected
    FRefCount: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function CloneSelf: IJX2;
    procedure CloneTo(ADestIntf: IJX2);
    function Clone: IW3DCloneable;
  end;

  IJX2VarVarDic = Interface
  ['{606B2C55-0D3F-4A65-B881-2920D3E3478A}']
  end;

  TIJX2VarVarDic = class(TDictionary<Variant, Variant>, IJX2VarVarDic, IJX2)
   protected
    FRefCount: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function CloneSelf: IJX2;
    procedure CloneTo(ADestIntf: IJX2);
    function Clone: IW3DCloneable;
   end;

  IJX2VarObjDic = Interface
  ['{6450B27F-3F2F-4079-8EC1-42AF868D3408}']
  end;

  TIJX2VarObjDic = class(TObjectDictionary<Variant, IJX2>, IJX2VarObjDic, IJX2)
   protected
    FRefCount: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    function CloneSelf: IJX2;
    procedure CloneTo(ADestIntf: IJX2);
    function Clone: IW3DCloneable;
  end;

{$ENDREGION'}

{$REGION 'Classes'}

  TJX2 = class(TObject)
    destructor Destroy; override;
    function Clone: TJX2;
    procedure CloneTo(ADest: TJX2);
  end;

  TJX2VarList  = class(TList<variant>)
    function Clone: TJX2VarList;
  end;

  TJX2ObjList  = class(TObjectList<TObject>) // Requires AJsonXClassType Attribute(Object class type)
    function Clone: TJX2ObjList;
  end;

  TJX2VarVarDic = class(TDictionary<variant, variant>)
    function Clone: TJX2VarVarDic;
  end;

  TJX2VarObjDic = class(TObjectDictionary<variant, TObject>)  // Requires AJsonXClassType Attribute(Object class type)
    function Clone: TJX2VarObjDic;
  end;

{$ENDREGION 'Classes'}

implementation
uses
    RTTI
  , W3DJsonX2.RTTI
  , SysUtils
  , Variants
  , System.TypInfo
  ;

{$REGION 'Attributes'}

constructor JX2AttrClass.Create(const AClass: TClass; const AData1: TClass = Nil ; const AData2: TClass = Nil);
begin
   FClass := AClass;
   FData1 := AData1;
   FData2 := AData2;
end;

constructor JX2AttrName.Create(const AName: string);
begin
   FName := AName;
end;

{$REGION}

{$REGION 'Interfaces'}

  {$REGION 'TIJX2'}

destructor TIJX2.Destroy;
var
  Field: TRTTIField;
begin
  for Field in W3DJsonX2.RTTI.GetFields(Self) do
    if Field.FieldType.TypeKind in [tkClass] then
      Field.GetValue(Self).AsObject.Free;
  inherited;
end;

function TIJX2.Clone: IW3DCloneable;
begin
    Result := CloneSelf;
end;

function TIJX2.CloneSelf: IJX2;
var
  LObj: TObject;
begin
  if Self = nil then Exit;
  LObj := Self.ClassType.Create;
  if not Supports(LObj, IJX2, Result) then
  begin
    LObj.Free;
    Exit;
  end;
  Self.CloneTo(Result);
end;

procedure TIJX2.CloneTo(ADestIntf: IJX2);
var
  LNewObj: TObject;
  Field: TRttiField;
  LValue: TValue;
  LDestObj, LFieldObj: TObject;
  LObjIntf, LFieldIntf: IJX2;
begin
  if ADestIntf = nil then exit;
  LDestObj := ADestIntf as TObject;
  for Field in GetFields(Self) do
  begin
    if Field.FieldType.TypeKind = tkVariant then
    begin
      LValue := Field.GetValue(Self);
      if not VarIsEmpty(LValue.AsVariant) then Field.SetValue(LDestObj, LValue);
    end else
      if Field.FieldType.TypeKind in [tkInterface] then
      begin
        LFieldObj := field.GetValue(Self).Asinterface as TObject;
        if LFieldObj <> nil then
        begin
          if LFieldObj is TIJX2VarList then
          begin
            LValue := TValue.From<IJX2VarList>( IJX2VarList( TIJX2VarList(LFieldObj).Clone ) );
            Field.SetValue(LDestObj, LValue);
          end else
          if LFieldObj is TIJX2ObjList then
          begin
            LValue := TValue.From<IJX2ObjList>( IJX2ObjList( TIJX2ObjList(LFieldObj).Clone ) );
            Field.SetValue(LDestObj, LValue);
          end else
          if LFieldObj is TIJX2VarVarDic then
          begin
            LValue := TValue.From<IJX2VarVarDic>( IJX2VarVarDic( TIJX2VarVarDic(LFieldObj).Clone ) );
            Field.SetValue(LDestObj, LValue);
          end else
          if LFieldObj is TIJX2VarObjDic then
          begin
            LValue := TValue.From<IJX2VarObjDic>( IJX2VarObjDic( TIJX2VarObjDic(LFieldObj).Clone ) );
            Field.SetValue(LDestObj, LValue);
          end else begin
            LNewObj := LFieldObj.ClassType.Create;
            if Supports(LNewObj, IJX2, LObjIntf) then
              if Supports(LFieldObj, IJX2, LFieldIntf) then
                IJX2(LFieldIntf).CloneTo(LObjIntf);
            Field.SetValue(LDestObj, LNewObj);
          end;
        end;
      end else
      if Field.FieldType.TypeKind in [tkClass] then
      begin
        LFieldObj := field.GetValue(Self).AsObject;
        if LFieldObj = Nil then Continue;
        Field.SetValue(LDestObj, TJX2(LFieldObj).Clone);
      end;
  end;
end;

  {$ENDREGION 'TIJX2'}

  {$REGION 'TIJX2VarList'}

function TIJX2VarList.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TIJX2VarList._AddRef: Integer;
begin
  Result := AtomicIncrement(_FRefCount);
end;

function TIJX2VarList._Release: Integer;
begin
  Result := AtomicDecrement(_FRefCount);
  if Result = 0 then
    Destroy;
end;

function TIJX2VarList.Clone: IW3DCloneable;
begin
  Result := CloneSelf;
end;

function TIJX2VarList.CloneSelf: IJX2;
begin
  Result:= IJX2(TIJX2VarList.Create);
  Self.CloneTo(Result);
end;

procedure TIJX2VarList.CloneTo(ADestIntf: IJX2);
var
  LValue: Variant;
begin
  For LValue in Self do
    TIJX2VarList(ADestIntf).Add(LValue);
end;

  {$ENDREGION 'TIJX2VarList'}

  {$REGION 'TIJX2ObjList'}

function TIJX2ObjList.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TIJX2ObjList._AddRef: Integer;
begin
  Result := AtomicIncrement(FRefCount);
end;

function TIJX2ObjList._Release: Integer;
begin
  Result := AtomicDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

function TIJX2ObjList.Clone: IW3DCloneable;
begin
  Result := CloneSelf;
end;

function TIJX2ObjList.CloneSelf: IJX2;
begin
  Result:= IJX2(TIJX2ObjList.Create);
  Self.CloneTo(Result);
end;

procedure TIJX2ObjList.CloneTo(ADestIntf: IJX2);
var
  LNewIntf: IJX2;
  LObj: IJX2;
begin
  for LNewIntf in Self do
    if Supports(LNewIntf, IJX2, LObj)  then
      TIJX2ObjList(ADestIntf).Add(LNewIntf.CloneSelf)
    else
      ADestIntf := Nil;
end;

  {$ENDREGION 'TIJX2ObjList'}

  {$REGION 'TIJX2VarVarDic'}

function TIJX2VarVarDic.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;
function TIJX2VarVarDic._AddRef: Integer;
begin
  Result := AtomicIncrement(FRefCount);
end;
function TIJX2VarVarDic._Release: Integer;
begin
  Result := AtomicDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

function TIJX2VarVarDic.Clone: IW3DCloneable;
begin
  Result := CloneSelf;
end;

function TIJX2VarVarDic.CloneSelf: IJX2;
begin
  Result:= IJX2(TIJX2VarVarDic.Create);
  Self.CloneTo(Result)
end;

procedure TIJX2VarVarDic.CloneTo(ADestIntf: IJX2);
var
  Lkv: TPair<Variant, Variant>;
begin
  for Lkv in Self do
    TIJX2VarVarDic(ADestIntf).Add(Lkv.Key, Lkv.Value);
end;

  {$ENDREGION 'TIJX2VarVarDic'}

  {$REGION 'TIJX2VarObjDic'}

function TIJX2VarObjDic.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TIJX2VarObjDic._AddRef: Integer;
begin
  Result := AtomicIncrement(FRefCount);
end;

function TIJX2VarObjDic._Release: Integer;
begin
  Result := AtomicDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

function TIJX2VarObjDic.Clone: IW3DCloneable;
begin
  Result := CloneSelf;
end;

function TIJX2VarObjDic.CloneSelf: IJX2;
begin
  Result := IJX2(TIJX2VarObjDic.Create);
  IJX2(Self).CloneTo(Result);
end;

procedure TIJX2VarObjDic.CloneTo(ADestIntf: IJX2);
var
  Lkv: TPair<Variant, IJX2>;
begin
  for Lkv in Self do
    TIJX2VarObjDic(ADestIntf).Add(Lkv.Key, TIJX2(Lkv.Value).CloneSelf);
end;

{$ENDREGION 'TIJX2VarObjDic'}


{$ENDREGION 'Interfaces'}

{$REGION 'Classes'}

function TJX2VarList.Clone: TJX2VarList;
var
  LV: Variant;
begin
  Result := TJX2VarList.create;
  for LV in Self do Result.Add(LV);
end;

function TJX2ObjList.Clone: TJX2ObjList;
var
  Lo: TObject;
begin
  Result := TJX2ObjList.Create(True);
  for Lo in Self do
    Result.Add(TJX2(Lo).Clone);
end;

function TJX2VarVarDic.Clone: TJX2VarVarDic;
var
  Lkv: TPair<Variant, Variant>;
begin
  Result := TJX2VarVarDic.Create;
  for Lkv in Self do Result.Add(Lkv.Key, Lkv.Value);
end;

function TJX2VarObjDic.Clone: TJX2VarObjDic;
var
  Lkv: TPair<Variant, TObject>;
begin
  Result := TJX2VarObjDic.Create([doOwnsValues]);
  for Lkv in Self do
    if Assigned(Lkv.Value) then
      Result.Add(Lkv.Key, TJX2(Lkv.Value).Clone)
    else
      Result.Add(Lkv.Key, Nil);
end;

destructor TJX2.Destroy;
var
  LField: TRTTIField;
begin
  for LField in W3DJsonX2.RTTI.GetFields(Self) do
    if LField.FieldType.TypeKind in [tkClass] then
      LField.GetValue(Self).AsObject.Free;
  inherited;
end;

function TJX2.Clone: TJX2;
begin
  Result := TJX2(Self.ClassType.Create);
  Self.CloneTo(Result);
end;

procedure TJX2.CloneTo(ADest: TJX2);
var
  LField: TRTTIField;
  LV: TValue;
  LClonable: TIJX2;
  LInstance: TRTTIInstanceType;
  LObj: TObject;
  LIntf: IJX2;
begin
  if ADest = nil then exit;
  for LField in GetFields(ADest) do
  begin
    if LField.FieldType.TypeKind = tkVariant then
    begin
      LV :=  LField.GetValue(Self);
      if not VarIsEmpty(LV.AsVariant) then LField.SetValue(ADest, LV);
    end else
    if LField.FieldType.TypeKind in [tkClass] then
    begin
      LInstance := LField.FieldType.AsInstance;
      LObj := LField.GetValue(Self).AsObject;
      if Assigned(LObj) then
      begin
        if LInstance.MetaclassType = TJX2VarList then
          LField.SetValue(ADest, TJX2VarList(LObj).Clone)
        else
        if LInstance.MetaclassType = TJX2ObjList then
          LField.SetValue(ADest, TJX2ObjList(LObj).Clone)
        else
        if LInstance.MetaclassType = TJX2VarVarDic then
          LField.SetValue(ADest, TJX2VarVarDic(LObj).Clone)
        else
        if LInstance.MetaclassType = TJX2VarObjDic then
          LField.SetValue(ADest, TJX2VarObjDic(LObj).Clone)
        else
          LField.SetValue(ADest, TJX2(LObj).Clone);
      end;
    end else
    if LField.FieldType.TypeKind in [tkInterface] then
    begin
      LIntf := LField.GetValue(Self).AsInterface as IJX2;
      LObj  := LIntf as TObject;

      if Supports(LObj, IJX2VarList) then
        LField.SetValue(ADest, TValue.From<IJX2VarList>(IJX2VarList(TIJX2VarList( TIJX2VarList(LObj).Clone ).Clone)) )
      else
      if Supports(LObj, IJX2ObjList) then
        LField.SetValue(ADest, TValue.From<IJX2ObjList>(IJX2ObjList(TIJX2ObjList( TIJX2ObjList(LObj).Clone ).Clone)) )
      else
      if Supports(LObj, IJX2VarVarDic) then
        LField.SetValue(ADest, TValue.From<IJX2VarVarDic>(IJX2VarVarDic(TIJX2VarVarDic( TIJX2VarVarDic(LObj).Clone ).Clone)) )
      else
      if Supports(LObj, IJX2VarObjDic) then
        LField.SetValue(ADest, TValue.From<IJX2VarObjDic>(IJX2VarObjDic(TIJX2VarObjDic( TIJX2VarObjDic(LObj).Clone ).Clone)) )
      else
      begin
        LClonable:= LField.GetValue(Self).AsInterface as TIJX2;
        if Assigned(LClonable) then
          LField.SetValue(ADest, TValue.From<IJX2>(TIJX2(LClonable).CloneSelf));
      end;
    end;

  end;
end;

{$ENDREGION 'Classes'}

end.


