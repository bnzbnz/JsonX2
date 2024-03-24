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

unit JsonX2.Types;

{$IFDEF W3DCLIENT}
  {$DEFINE JSX_NOVAR}
{$ENDIF}

interface

uses
  Classes
  , System.Generics.Collections
  , RTTI
{$IFDEF W3DCLIENT}
  , W3DCloneable
{$ENDIF}  
  , JsonX2.Obj
  , JsonX2.RTTI
  , JsonX2.Utils
  , JsonX2.Patch
  ;

type

  TJX2Setting = (
    jxExplicitBinding           // Serialize/Deserialize only fields having an explicit JX2AttrName attribute
    , jxoRaiseException         // Re-raise exception instead of an empty result;
    , jxoEmptyString            // Serialize: return an empty string instead of '{}' when the object is empty
    , jxoNullify                // Serialize empty fields as null, if not they are removed
    , jxoInnerJson              // Deserialize: Get the inner json string of the object (_InnerJson)
    , jxoPublishedBinding       // Publicshed field will be Ser/Des
    , jxoPublicBinding          // Public field will be Ser/Des
    , jxoProtectedBinding       // Protected field will be Ser/Des
    , jxoPrivateBinding         // Private field will be Ser/Des
    , jxoRaiseOnMissingField     // Raise on RRTIField is missing / Debug
  );
  TJX2Settings = set of TJX2Setting;

  TJX2Stats = class(TObject)
    DurationMS: Int64;
    OpCount: Int64;
    VariantCount: Int64;
    TValueCount: Int64;
    procedure Clear;
    constructor Create;
  end;

  IJX2 = Interface;

  TJX2DataBlock = record
  SelfObj: TObject;
  Field: TRttiField;
  JsonStr: string;
  JsonObj: TJsonObject;
  Settings: TJX2Settings;
  JsonVal: PJsonDataValue;
  Patcher: TJX2Patcher;
  constructor Create(
    ASettings: TJX2Settings;
    ASelfObj: TObject;
    AField: TRttiField = nil;
    AJsonStr: string = '';
    AJsonObj: TJsonObject = nil;
    AJsonVal: PJsonDataValue = nil;
    APatcher: TJX2Patcher = nil
  );
  end;

  IJX2Converter = interface(IInterface)
    ['{A8023240-7045-4CA8-8773-A202B369910B}']
    function  OnClone(AData: TJX2DataBlock): TObject;
    function  OnSerialize(AData: TJX2DataBlock): string;
    function  OnDeserialize(AData: TJX2DataBlock) : TObject;
    procedure OnDestroy(AData: TJX2DataBlock);
  end;

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

  JX2AttrConv = class(TCustomAttribute)
  public
    FConv: TClass;
    FClass: TClass;
    constructor Create(AConv: TClass; AClass: TClass = nil);
  end;

  JX2AttrExclude = class(TCustomAttribute)
  public
  end;

{$ENDREGION}

{$REGION 'Interfaces'}

{ TJX2ToJsonDataBlock }

{$IF not defined(W3DCLIENT)}
  IW3DCloneable = interface
  ['{C6437503-760C-47EA-8F61-4B7F20BE9CA9}']
    function Clone : IW3DCloneable;
  end;
{$ENDIF}

  IJX2 = interface(IW3DCloneable)
  ['{CA357255-FAC5-4D38-8980-4B89875FE491}']
    procedure CloneTo(ADestIntf: IJX2);
    function  CloneSelf: IJX2;
  end;

  TIJX2 = class(TInterfacedObject, IJX2, IW3DCloneable)
    _InnerJson: TValue;
    destructor Destroy; override;
    procedure CloneTo(ADestIntf: IJX2);
    function CloneSelf: IJX2;
    function Clone: IW3DCloneable;
  end;

  IJX2ValueList = Interface
  ['{FABCD782-2317-4429-854F-A9F9A7ADAECB}']
  end;

  TIJX2ValueList = class(TList<TValue>, IJX2ValueList, IJX2)
  protected
    FRefCount: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    _InnerJson: TValue;
    function CloneSelf: IJX2;
    procedure CloneTo(ADestIntf: IJX2);
    function Clone: IW3DCloneable;
  end;

  IJX2VarList = Interface
  ['{CAD572B8-B99C-405C-8946-B10988518E83}']
  end;

  {$IFNDEF JSX_NOVAR}
  TIJX2VarList = class(TList<Variant>, IJX2VarList, IJX2)
  protected
    FRefCount: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    _InnerJson: TValue;
    function CloneSelf: IJX2;
    procedure CloneTo(ADestIntf: IJX2);
    function Clone: IW3DCloneable;
  end;
  {$ENDIF}

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
    _InnerJson: TValue;
    function CloneSelf: IJX2;
    procedure CloneTo(ADestIntf: IJX2);
    function Clone: IW3DCloneable;
  end;

  IJX2StrVarDic = Interface
  ['{606B2C55-0D3F-4A65-B881-2920D3E3478A}']
  end;

  {$IFNDEF JSX_NOVAR}
  TIJX2StrVarDic = class(TDictionary<string, Variant>, IJX2StrVarDic, IJX2)
   protected
    FRefCount: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    _InnerJson: TValue;
    function CloneSelf: IJX2;
    procedure CloneTo(ADestIntf: IJX2);
    function Clone: IW3DCloneable;
   end;
   {$ENDIF}

  IJX2StrValueDic = Interface
  ['{201214EF-CA76-4E9C-B22A-AACCF1AF94DF}']
  end;

  TIJX2StrValueDic = class(TDictionary<string, TValue>, IJX2StrValueDic, IJX2)
   protected
    FRefCount: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    _InnerJson: TValue;
    function CloneSelf: IJX2;
    procedure CloneTo(ADestIntf: IJX2);
    function Clone: IW3DCloneable;
  end;

  IJX2StrObjDic = Interface
  ['{587F5F53-976C-48E6-95E1-E7331390E727}']
  end;

  TIJX2StrObjDic = class(TObjectDictionary<string, IJX2>, IJX2StrObjDic, IJX2)
  protected
     FRefCount: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    _InnerJson: TValue;
    function CloneSelf: IJX2;
    procedure CloneTo(ADestIntf: IJX2);
    function Clone: IW3DCloneable;
  end;

{$ENDREGION'}

{$REGION 'Classes'}

  TJX2 = class(TObject)
    _InnerJson: TValue;
    destructor Destroy; override;
    function Clone: TJX2;
    procedure CloneTo(ADest: TJX2);
  end;

  TJX2ValueList  = class(TList<TValue>)
    _InnerJson: TValue;
    function Clone: TJX2ValueList;
  end;

  {$IFNDEF JSX_NOVAR}
  TJX2VarList  = class(TList<variant>)
    _InnerJson: TValue;
    function Clone: TJX2VarList;
  end;
  {$ENDIF}

  TJX2ObjList  = class(TObjectList<TObject>) // Requires AJsonXClassType Attribute(Object class type)
    _InnerJson: string;
    function Clone: TJX2ObjList;
  end;

  {$IFNDEF JSX_NOVAR}
  TJX2StrVarDic = class(TDictionary<string, variant>)
    _InnerJson: TValue;
    function Clone: TJX2StrVarDic;
  end;
  {$ENDIF}

  TJX2StrValueDic = class(TDictionary<string, TValue>)
    _InnerJson: TValue;
    function Clone: TJX2StrValueDic;
  end;

  TJX2StrObjDic = class(TObjectDictionary<string, TObject>)  // Requires AJsonXClassType Attribute(Object class type)
    _InnerJson: TValue;
    constructor Create; overload;
    function Clone: TJX2StrObjDic;
  end;

{$ENDREGION 'Classes'}

implementation
uses
  SysUtils
  , System.TypInfo
  , JsonX2
{$IFNDEF JSX_NOVAR}
  , Variants
{$ENDIF}
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

constructor JX2AttrConv.Create(AConv: TClass; AClass: TClass = nil);
begin
  FConv := AConv;
  FClass := AClass;
end;

{$ENDREGION}

procedure GenericClone(ASelf: TObject; ADest: TObject; AField: TRTTIField);
var
  LValue: TValue;
  LInstance: TRTTIInstanceType;
  LObj, LNewObj: TObject;
  LAttr: TCustomAttribute;
  LAttrIntf: IJX2Converter;
  LIJX2: IJX2;
begin
  if not Assigned(ADest) then exit;
  if not Assigned(ASelf) then
  begin
    AField.SetValue(ADest, nil);
    Exit;
  end;
  if (AField.FieldType.TypeKind = tkRecord) and (AField.FieldType.Handle = TypeInfo(TValue)) then
  begin
    if AField.GetValue(ASelf).TryAsType<TValue>(LValue) then
      AField.SetValue(ADest, LValue);
  end else
  {$IFNDEF JSX_NOVAR}
  if AField.FieldType.TypeKind = tkVariant then
  begin
    LValue :=  AField.GetValue(ASelf);
    if not VarIsEmpty(LValue.AsVariant) then AField.SetValue(ADest, LValue);
  end else
  {$ENDIF}
  if AField.FieldType.TypeKind = tkClass then
  begin
    {$REGION 'tkClass'}
    LInstance := AField.FieldType.AsInstance;
    LObj := AField.GetValue(ASelf).AsObject;
    if Assigned(LObj) then
    begin

      {$IFNDEF JSX_NOVAR}
      if LInstance.MetaclassType = TJX2VarList then
        AField.SetValue(ADest, TJX2VarList(LObj).Clone)
      else
      {$ENDIF}
      if LInstance.MetaclassType = TJX2ObjList then
        AField.SetValue(ADest, TJX2ObjList(LObj).Clone)
      else
      {$IFNDEF JSX_NOVAR}
      if LInstance.MetaclassType = TJX2StrVarDic then
        AField.SetValue(ADest, TJX2StrVarDic(LObj).Clone)
      else
      {$ENDIF}
      if LInstance.MetaclassType = TJX2StrValueDic then
        AField.SetValue(ADest, TJX2StrValueDic(LObj).Clone)
      else
      if LInstance.MetaclassType = TJX2ValueList then
        AField.SetValue(ADest, TJX2ValueList(LObj).Clone)
      else
      if LInstance.MetaclassType = TJX2StrObjDic then
        AField.SetValue(ADest, TJX2StrObjDic(LObj).Clone)
      else
      begin
        LAttr := GetRTTIAttribute(AField, JX2AttrConv);
        if Assigned(LAttr) then
        begin
          try
            if not Assigned(JX2AttrConv(LAttr)) then Exit;
            if not Supports(JX2AttrConv(LAttr).FConv.Create, IJX2Converter, LAttrIntf) then Exit;
            AField.SetValue(ADest, LAttrIntf.OnClone(TJX2DataBlock.Create([], LObj, nil, '', nil)) );
          except end;
          Exit;
        end;
        if LObj is TJX2 then
          AField.SetValue(ADest, TJX2(LObj).Clone)
        else
        if LObj is TIJX2 then
          AField.SetValue(ADest, TJX2(LObj).Clone)
        else
          AField.SetValue(ADest, nil);
      end;
    end;
   {$ENDREGION 'tkClass'}
  end else
  if AField.FieldType.TypeKind = tkInterface then
  begin

   {$REGION 'tkInterface'}

    if AField.GetValue(ASelf).AsInterface= nil then  Exit;

    LAttr := GetRTTIAttribute(AField, JX2AttrConv);
    if Assigned(LAttr) then
    begin
      LAttr := GetRTTIAttribute(AField, JX2AttrConv);
      if Assigned(LAttr) then
      begin
      try
        if not Assigned(JX2AttrConv(LAttr)) then Exit;
        if not Supports(JX2AttrConv(LAttr).FConv.Create, IJX2Converter, LAttrIntf) then Exit;
        AField.SetValue(ADest, LAttrIntf.OnClone(TJX2DataBlock.Create([], ASelf, nil, '', nil)) );
      except end;
        Exit;
      end;
    end;

     if Supports(AField.GetValue(ASelf).AsInterface, IJX2, LIJX2) then
     begin

      LObj  := AField.GetValue(ASelf).AsInterface as TObject;

      {$IFNDEF JSX_NOVAR}
      if Supports(LObj, IJX2VarList) then
        AField.SetValue(ADest, TValue.From<IJX2VarList>(IJX2VarList(TIJX2VarList(LObj).Clone)) )
      else
      if Supports(LObj, IJX2StrVarDic) then
        AField.SetValue(ADest, TValue.From<IJX2StrVarDic>(IJX2StrVarDic(TIJX2StrVarDic(LObj).Clone)))
      else
      {$ENDIF}
      if Supports(LObj, IJX2ObjList) then
        AField.SetValue(ADest, TValue.From<IJX2ObjList>(IJX2ObjList(TIJX2ObjList(LObj).Clone)))
      else
      if Supports(LObj, IJX2ValueList) then
        AField.SetValue(ADest, TValue.From<IJX2ValueList>(IJX2ValueList(TIJX2ValueList(LObj).Clone)))
      else
      if Supports(LObj, IJX2StrValueDic) then
        AField.SetValue(ADest, TValue.From<IJX2StrValueDic>(IJX2StrValueDic(TIJX2StrValueDic(LObj).Clone)))
      else
      if Supports(LObj, IJX2StrObjDic) then
        AField.SetValue(ADest, TValue.From<IJX2StrObjDic>(IJX2StrObjDic(TIJX2StrObjDic(LObj).Clone)))
      else
      begin
        LNewObj := LObj.ClassType.Create;
        Supports(LNewObj, IJX2, LIJX2);
        TIJX2(LObj).CloneTo(LIJX2);
        AField.SetValue(ADest, LNewObj);
      end;
    end;

   {$ENDREGION 'tkInterface'}
  end else
  begin

  end;
end;

{$REGION 'Interfaces'}

  {$REGION 'TIJX2'}

destructor TIJX2.Destroy;
var
  LField: TRTTIField;
  LAttr: JX2AttrConv;
  LAttrIntf: IJX2Converter;
begin
  for LField in JsonX2.RTTI.GetRTTIFields(Self) do
    if LField.FieldType.TypeKind in [tkClass] then
    begin
      LAttr := JX2AttrConv(GetRTTIAttribute(LField, JX2AttrConv));
      if Assigned(LAttr) and Assigned(LAttr.FConv) then
        if Supports(JX2AttrConv(LAttr).FConv.Create, IJX2Converter, LAttrIntf) then
          if (LField.GetValue(Self).AsObject <> nil) then
            LAttrIntf.OnDestroy( TJX2DataBlock.Create([], LField.GetValue(Self).AsObject, LField) );
      FreeAndNil(LField.GetValue(Self).AsObject);
    end;
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
  Field: TRttiField;
begin
  for Field in GetRTTIFields(ADestIntf as TObject) do
    GenericClone(Self, ADestIntf as TObject, Field);
end;

  {$ENDREGION 'TIJX2'}

  {$REGION 'TIJX2ValueList'}

function TIJX2ValueList.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TIJX2ValueList._AddRef: Integer;
begin
  Result := AtomicIncrement(FRefCount);
end;

function TIJX2ValueList._Release: Integer;
begin
  Result := AtomicDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

function TIJX2ValueList.Clone: IW3DCloneable;
begin
  Result := CloneSelf;
end;

function TIJX2ValueList.CloneSelf: IJX2;
begin
  Result:= IJX2(TIJX2ValueList.Create);
  Self.CloneTo(Result);
end;

procedure TIJX2ValueList.CloneTo(ADestIntf: IJX2);
var
  LValue: TValue;
begin
  For LValue in Self do
    TIJX2ValueList(ADestIntf).Add(LValue);
end;

  {$ENDREGION 'TIJX2ValueList'}
{$IFNDEF JSX_NOVAR}
  {$REGION 'TIJX2StrVarDic'}

function TIJX2StrVarDic.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;
function TIJX2StrVarDic._AddRef: Integer;
begin
  Result := AtomicIncrement(FRefCount);
end;
function TIJX2StrVarDic._Release: Integer;
begin
  Result := AtomicDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

function TIJX2StrVarDic.Clone: IW3DCloneable;
begin
  Result := CloneSelf;
end;

function TIJX2StrVarDic.CloneSelf: IJX2;
begin
  Result:= IJX2(TIJX2StrVarDic.Create);
  Self.CloneTo(Result)
end;

procedure TIJX2StrVarDic.CloneTo(ADestIntf: IJX2);
var
  Lkv: TPair<string, Variant>;
begin
  for Lkv in Self do
    TIJX2StrVarDic(ADestIntf).Add(Lkv.Key, Lkv.Value);
end;

  {$ENDREGION 'TIJX2StrVarDic'}

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
  Result := AtomicIncrement(FRefCount);
end;

function TIJX2VarList._Release: Integer;
begin
  Result := AtomicDecrement(FRefCount);
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

{$ENDIF}
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

  {$REGION 'TIJX2StrValueDic'}

function TIJX2StrValueDic.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;
function TIJX2StrValueDic._AddRef: Integer;
begin
  Result := AtomicIncrement(FRefCount);
end;
function TIJX2StrValueDic._Release: Integer;
begin
  Result := AtomicDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

function TIJX2StrValueDic.Clone: IW3DCloneable;
begin
  Result := CloneSelf;
end;

function TIJX2StrValueDic.CloneSelf: IJX2;
begin
  Result:= IJX2(TIJX2StrValueDic.Create);
  Self.CloneTo(Result)
end;

procedure TIJX2StrValueDic.CloneTo(ADestIntf: IJX2);
var
  Lkv: TPair<string, TValue>;
begin
  for Lkv in Self do
    TIJX2StrValueDic(ADestIntf).Add(Lkv.Key, Lkv.Value);
end;

  {$ENDREGION 'TIJX2StrValueDic'}

  {$REGION 'TIJX2StrObjDic'}

function TIJX2StrObjDic.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TIJX2StrObjDic._AddRef: Integer;
begin
  Result := AtomicIncrement(FRefCount);
end;

function TIJX2StrObjDic._Release: Integer;
begin
  Result := AtomicDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

function TIJX2StrObjDic.Clone: IW3DCloneable;
begin
  Result := CloneSelf;
end;

function TIJX2StrObjDic.CloneSelf: IJX2;
begin
  Result := IJX2(TIJX2StrObjDic.Create);
  IJX2(Self).CloneTo(Result);
end;

procedure TIJX2StrObjDic.CloneTo(ADestIntf: IJX2);
var
  Lkv: TPair<string, IJX2>;
begin
  for Lkv in Self do
    TIJX2StrObjDic(ADestIntf).Add(Lkv.Key, Lkv.Value);
end;

  {$ENDREGION 'TIJX2StrObjDic'}

{$ENDREGION 'Interfaces'}

{$REGION 'Classes'}

function TJX2ValueList.Clone: TJX2ValueList;
var
  LV: TValue;
begin
  Result := TJX2ValueList.create;
  for LV in Self do Result.Add(LV);
end;

{$IFNDEF JSX_NOVAR}
function TJX2VarList.Clone: TJX2VarList;
var
  LV: Variant;
begin
  Result := TJX2VarList.create;
  for LV in Self do Result.Add(LV);
end;
{$ENDIF}

function TJX2ObjList.Clone: TJX2ObjList;
var
  Lo: TObject;
begin
  Result := TJX2ObjList.Create(True);
  for Lo in Self do
    Result.Add(TJX2(Lo).Clone);
end;

{$IFNDEF JSX_NOVAR}
function TJX2StrVarDic.Clone: TJX2StrVarDic;
var
  Lkv: TPair<string, Variant>;
begin
  Result := TJX2StrVarDic.Create;
  for Lkv in Self do Result.Add(Lkv.Key, Lkv.Value);
end;
{$ENDIF}

function TJX2StrValueDic.Clone: TJX2StrValueDic;
var
  Lkv: TPair<string, TValue>;
begin
  Result := TJX2StrValueDic.Create;
  for Lkv in Self do Result.Add(Lkv.Key, Lkv.Value);
end;

function TJX2StrObjDic.Clone: TJX2StrObjDic;
var
  Lkv: TPair<string, TObject>;
begin
  Result := TJX2StrObjDic.Create([doOwnsValues]);
  for Lkv in Self do
    if Assigned(Lkv.Value) then
      Result.Add(Lkv.Key, TJX2(Lkv.Value).Clone)
    else
      Result.Add(Lkv.Key, Nil);
end;

constructor TJX2StrObjDic.Create;
begin
  inherited Create([doOwnsValues]);
end;

destructor TJX2.Destroy;
var
  LField: TRTTIField;
  LAttr: JX2AttrConv;
  LAttrIntf: IJX2Converter;
begin
  for LField in JsonX2.RTTI.GetRTTIFields(Self) do
    if LField.FieldType.TypeKind in [tkClass] then
    begin
      LAttr := JX2AttrConv(GetRTTIAttribute(LField, JX2AttrConv));
      if Assigned(LAttr) and Assigned(LAttr.FConv) then
        if Supports(JX2AttrConv(LAttr).FConv.Create, IJX2Converter, LAttrIntf) then
          if (LField.GetValue(Self).AsObject <> nil) then
            LAttrIntf.OnDestroy( TJX2DataBlock.Create([], LField.GetValue(Self).AsObject, LField) );
      FreeAndNil(LField.GetValue(Self).AsObject);
    end;
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
begin
   for LField in GetRTTIFields(ADest) do
    GenericClone(Self, ADest, LField);
end;

constructor TJX2DataBlock.Create(
    ASettings: TJX2Settings;
    ASelfObj: TObject;
    AField: TRttiField = nil;
    AJsonStr: string = '';
    AJsonObj: TJsonObject = nil;
    AJsonVal: PJsonDataValue = nil;
    APatcher: TJX2Patcher = nil
  );
begin
  SelfObj := ASelfObj;
  Field := AField;
  JsonStr := AJsonStr;
  JsonObj := AJsonObj;
  Settings := ASettings;
  JsonVal := AJsonVal;
  Patcher := APatcher;
end;

procedure TJX2Stats.Clear;
begin
  Self.DurationMS := 0;
  Self.OpCount := 0;
  Self.VariantCount := 0;
  Self.TValueCount := 0;
end;

constructor TJX2Stats.Create;
begin
  inherited;
  Clear;
end;

end.


