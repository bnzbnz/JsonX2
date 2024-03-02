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

{$IFDEF W3DCLIENT}
  {$DEFINE JSX_NOVAR}
{$ENDIF}

interface

uses
  Classes
  , System.Generics.Collections
  , RTTI
  , W3DCloneable
  ;

type

  TJX2Setting = (
    jxExplicitBinding           // Serialize/Deserialize only fields having an explicit JX2AttrName attribut
    , jxoRaiseException         // Re-raise exception instead of an empty result;
    , jxoUnassignedToNull
    , jxoReturnEmptyJsonString  // Serialize: return an empty string instead of '{}' when the object is empty
  );
  TJX2Settings = set of TJX2Setting;

  IJX2 = Interface;

  TJX2Converter = class
    function ToJson(ASelfObj: TObject): string; virtual; abstract;
    function FromJson(AJson: string) : TObject; virtual; abstract;
    function Clone(ASelfObj: TObject): TObject; virtual; abstract;
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
    constructor Create(AConv: TClass);
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

  IJX2ValueList = Interface
  ['{FABCD782-2317-4429-854F-A9F9A7ADAECB}']
  end;

  TIJX2ValueList = class(TList<TValue>, IJX2ValueList, IJX2)
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

  IJX2VarList = Interface
  ['{CAD572B8-B99C-405C-8946-B10988518E83}']
  end;

  {$IFNDEF JSX_NOVAR}
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
    function CloneSelf: IJX2;
    procedure CloneTo(ADestIntf: IJX2);
    function Clone: IW3DCloneable;
  end;

  IJX2ValueObjDic = Interface
  ['{EEFCBBEC-C695-4FEB-9F43-745D1AB20A3D}']
  end;

  TIJX2ValueObjDic = class(TObjectDictionary<TValue, IJX2>, IJX2ValueObjDic, IJX2)
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

  {$IFNDEF JSX_NOVAR}
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
  {$ENDIF}

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

  TJX2ValueList  = class(TList<TValue>)
    function Clone: TJX2ValueList;
  end;

  {$IFNDEF JSX_NOVAR}
  TJX2VarList  = class(TList<variant>)
    function Clone: TJX2VarList;
  end;
  {$ENDIF}

  TJX2ObjList  = class(TObjectList<TObject>) // Requires AJsonXClassType Attribute(Object class type)
    function Clone: TJX2ObjList;
  end;

  {$IFNDEF JSX_NOVAR}
  TJX2StrVarDic = class(TDictionary<string, variant>)
    function Clone: TJX2StrVarDic;
  end;
  {$ENDIF}

  TJX2StrValueDic = class(TDictionary<string, TValue>)
    function Clone: TJX2StrValueDic;
  end;

  TJX2ValueObjDic = class(TObjectDictionary<TValue, TObject>)  // Requires AJsonXClassType Attribute(Object class type)
    constructor Create; overload;
    function Clone: TJX2ValueObjDic;
  end;

  {$IFNDEF JSX_NOVAR}
  TJX2VarObjDic = class(TObjectDictionary<variant, TObject>)  // Requires AJsonXClassType Attribute(Object class type)
    constructor Create; overload;
    function Clone: TJX2VarObjDic;
  end;
  {$ENDIF}

  TJX2StrObjDic = class(TObjectDictionary<string, TObject>)  // Requires AJsonXClassType Attribute(Object class type)
    constructor Create; overload;
    function Clone: TJX2StrObjDic;
  end;

{$ENDREGION 'Classes'}

implementation
uses
  W3DJsonX2.RTTI
  , SysUtils
  , System.TypInfo
  , W3DJsonX2
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

constructor JX2AttrConv.Create(AConv: TClass);
begin
  FConv := AConv;
end;


{$ENDREGION}

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
  LObjIntf: IJX2;
begin
  if ADestIntf = nil then exit;
  LDestObj := ADestIntf as TObject;
  for Field in GetFields(Self) do
  begin
    if (Field.FieldType.TypeKind = tkRecord) and (Field.FieldType.Handle = TypeInfo(TValue)) then
    begin
      LValue := Field.GetValue(Self);
      Field.SetValue(LDestObj, LValue);
    end else
    {$IFNDEF JSX_NOVAR}
    if Field.FieldType.TypeKind = tkVariant then
    begin
      LValue := Field.GetValue(Self);
      Field.SetValue(LDestObj, LValue);
    end else
    {$ENDIF}
      if Field.FieldType.TypeKind in [tkInterface] then
      begin
        if not Supports(field.GetValue(Self).Asinterface, IJX2, LObjIntf) then Continue;
        LFieldObj := LObjIntf as TObject;
        if LFieldObj <> nil then
        begin
          {$IFNDEF JSX_NOVAR}
          if LFieldObj is TIJX2VarList then
          begin
            LValue := TValue.From<IJX2VarList>( IJX2VarList( TIJX2VarList(LFieldObj).Clone ) );
            Field.SetValue(LDestObj, LValue);
          end else
          {$ENDIF}
          if LFieldObj is TIJX2ValueList then
          begin
            LValue := TValue.From<IJX2ValueList>( IJX2ValueList( TIJX2ValueList(LFieldObj).Clone ) );
            Field.SetValue(LDestObj, LValue);
          end else
          if LFieldObj is TIJX2StrValueDic then
          begin
            LValue := TValue.From<IJX2StrValueDic>( IJX2StrValueDic( TIJX2StrValueDic(LFieldObj).Clone ) );
            Field.SetValue(LDestObj, LValue);
          end else
          if LFieldObj is TIJX2ObjList then
          begin
            LValue := TValue.From<IJX2ObjList>( IJX2ObjList( TIJX2ObjList(LFieldObj).Clone ) );
            Field.SetValue(LDestObj, LValue);
          end else
          {$IFNDEF JSX_NOVAR}
          if LFieldObj is TIJX2StrVarDic then
          begin
            LValue := TValue.From<IJX2StrVarDic>( IJX2StrVarDic( TIJX2StrVarDic(LFieldObj).Clone ) );
            Field.SetValue(LDestObj, LValue);
          end else
          if LFieldObj is TIJX2VarObjDic then
          begin
            LValue := TValue.From<IJX2VarObjDic>( IJX2VarObjDic( TIJX2VarObjDic(LFieldObj).Clone ) );
            Field.SetValue(LDestObj, LValue);
          end else
          {$ENDIF}
          begin
            LNewObj := LFieldObj.ClassType.Create;
            if not Supports(LNewObj, IJX2, LObjIntf) then continue;
            TIJX2(LFieldObj).CloneTo(LObjIntf);
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
  Result := AtomicIncrement(_FRefCount);
end;

function TIJX2ValueList._Release: Integer;
begin
  Result := AtomicDecrement(_FRefCount);
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

{$ENDREGION
 'TIJX2VarObjDic'}
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

  {$REGION 'TIJX2ValueObjDic'}

function TIJX2ValueObjDic.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TIJX2ValueObjDic._AddRef: Integer;
begin
  Result := AtomicIncrement(FRefCount);
end;

function TIJX2ValueObjDic._Release: Integer;
begin
  Result := AtomicDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

function TIJX2ValueObjDic.Clone: IW3DCloneable;
begin
  Result := CloneSelf;
end;

function TIJX2ValueObjDic.CloneSelf: IJX2;
begin
  Result := IJX2(TIJX2ValueObjDic.Create);
  IJX2(Self).CloneTo(Result);
end;

procedure TIJX2ValueObjDic.CloneTo(ADestIntf: IJX2);
var
  Lkv: TPair<TValue, IJX2>;
begin
  for Lkv in Self do
    TIJX2ValueObjDic(ADestIntf).Add(Lkv.Key, TIJX2(Lkv.Value).CloneSelf);
end;

  {$ENDREGION 'TIJX2ValueObjDic'}

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

{$IFNDEF JSX_NOVAR}
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
{$ENDIF}

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

function TJX2ValueObjDic.Clone: TJX2ValueObjDic;
var
  Lkv: TPair<TValue, TObject>;
begin
  Result := TJX2ValueObjDic.Create([doOwnsValues]);
  for Lkv in Self do
    if Assigned(Lkv.Value) then
      Result.Add(Lkv.Key, TJX2(Lkv.Value).Clone)
    else
      Result.Add(Lkv.Key, Nil);
end;

constructor TJX2ValueObjDic.Create;
begin
  inherited Create([doOwnsValues]);
end;

{$IFNDEF JSX_NOVAR}
constructor TJX2VarObjDic.Create;
begin
  inherited Create([doOwnsValues]);
end;
{$ENDIF}

constructor TJX2StrObjDic.Create;
begin
  inherited Create([doOwnsValues]);
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
  LInstance: TRTTIInstanceType;
  LObj: TObject;
  LIntf: IJX2;
  LTValue: TValue;
  LTIJX2: TIJX2;
  LAttrConv: TCustomAttribute;
  LConverter: TJX2Converter;
begin
  if ADest = nil then exit;
  for LField in GetFields(ADest) do
  begin
{$IFNDEF JSX_NOVAR}
    if LField.FieldType.TypeKind = tkVariant then
    begin
      LV :=  LField.GetValue(Self);
      if not VarIsEmpty(LV.AsVariant) then LField.SetValue(ADest, LV);
    end else
{$ENDIF}
    if LField.FieldType.TypeKind = tkRecord then
    begin
      if LField.GetValue(Self).TryAsType<TValue>(LTValue) then
      begin
        if LField.FieldType.Handle = TypeInfo(TValue) then
        begin
          LField.SetValue(ADest, LTValue);
        end;
      end;
    end;
    if LField.FieldType.TypeKind in [tkClass] then
    begin
      LInstance := LField.FieldType.AsInstance;
      LObj := LField.GetValue(Self).AsObject;
      if Assigned(LObj) then
      begin
        {$IFNDEF JSX_NOVAR}
        if LInstance.MetaclassType = TJX2VarList then
          LField.SetValue(ADest, TJX2VarList(LObj).Clone)
        else
        {$ENDIF}
        if LInstance.MetaclassType = TJX2ObjList then
          LField.SetValue(ADest, TJX2ObjList(LObj).Clone)
        else
        {$IFNDEF JSX_NOVAR}
        if LInstance.MetaclassType = TJX2StrVarDic then
          LField.SetValue(ADest, TJX2StrVarDic(LObj).Clone)
        else
        {$ENDIF}
        if LInstance.MetaclassType = TJX2StrValueDic then
          LField.SetValue(ADest, TJX2StrValueDic(LObj).Clone)
        else
        {$IFNDEF JSX_NOVAR}
        if LInstance.MetaclassType = TJX2VarObjDic then
          LField.SetValue(ADest, TJX2VarObjDic(LObj).Clone)
        else
        {$ENDIF}
        if LInstance.MetaclassType = TJX2ValueList then
          LField.SetValue(ADest, TJX2ValueList(LObj).Clone)
        else
        if LInstance.MetaclassType = TJX2ValueObjDic then
          LField.SetValue(ADest, TJX2ValueObjDic(LObj).Clone)
        else
        if LInstance.MetaclassType = TJX2StrObjDic then
          LField.SetValue(ADest, TJX2StrObjDic(LObj).Clone)
        else begin
          LAttrConv := GetFieldAttribute(LField, JX2AttrConv);
          if Assigned(LAttrConv) then
          begin
            LConverter := TJX2Converter(JX2AttrConv(LAttrConv).FConv.Create);
            LField.SetValue(ADest, LConverter.Clone(LObj));
            LConverter.Free;
            Continue;
          end;
          LField.SetValue(ADest, TJX2(LObj).Clone);
        end;
      end;
    end else
    if LField.FieldType.TypeKind in [tkInterface] then
    begin
      var x := LField.Name;
      LIntf := LField.GetValue(Self).AsInterface as IJX2;
      LObj  := LIntf as TObject;

      {$IFNDEF JSX_NOVAR}
      if Supports(LObj, IJX2VarList) then
        LField.SetValue(ADest, TValue.From<IJX2VarList>(IJX2VarList(TIJX2VarList( TIJX2VarList(LObj).Clone ).Clone)) )
      else
      {$ENDIF}
      if Supports(LObj, IJX2ObjList) then
        LField.SetValue(ADest, TValue.From<IJX2ObjList>(IJX2ObjList(TIJX2ObjList( TIJX2ObjList(LObj).Clone ).Clone)) )
      else
      {$IFNDEF JSX_NOVAR}
      if Supports(LObj, IJX2StrVarDic) then
        LField.SetValue(ADest, TValue.From<IJX2StrVarDic>(IJX2StrVarDic(TIJX2StrVarDic( TIJX2StrVarDic(LObj).Clone ).Clone)) )
      else
      if Supports(LObj, IJX2VarObjDic) then
        LField.SetValue(ADest, TValue.From<IJX2VarObjDic>(IJX2VarObjDic(TIJX2VarObjDic( TIJX2VarObjDic(LObj).Clone ).Clone)) )
      else
      if Supports(LObj, IJX2VarObjDic) then
        LField.SetValue(ADest, TValue.From<IJX2VarObjDic>(IJX2VarObjDic(TIJX2VarObjDic( TIJX2VarObjDic(LObj).Clone ).Clone)) )
      else
      {$ENDIF}
      if Supports(LObj, IJX2ValueList) then
        LField.SetValue(ADest, TValue.From<IJX2ValueList>(IJX2ValueList(TIJX2ValueList( TIJX2ValueList(LObj).Clone ).Clone)) )
      else
      if Supports(LObj, IJX2ValueObjDic) then
        LField.SetValue(ADest, TValue.From<IJX2ValueObjDic>(IJX2ValueObjDic(TIJX2ValueObjDic( TIJX2ValueObjDic(LObj).Clone ).Clone)) )
      else
      if Supports(LObj, IJX2ValueObjDic) then
        LField.SetValue(ADest, TValue.From<IJX2ValueObjDic>(IJX2ValueObjDic(TIJX2ValueObjDic( TIJX2ValueObjDic(LObj).Clone ).Clone)) )
      else
      if Supports(LObj, IJX2StrValueDic) then
        LField.SetValue(ADest, TValue.From<IJX2StrValueDic>(IJX2StrValueDic(TIJX2StrValueDic( TIJX2StrValueDic(LObj).Clone ).Clone)) )
      else
      if Supports(LObj, IJX2StrObjDic) then
        LField.SetValue(ADest, TValue.From<IJX2StrObjDic>(IJX2StrObjDic(TIJX2StrObjDic( TIJX2StrObjDic(LObj).Clone ).Clone)) )
      else
      begin
        LTIJX2:= LField.GetValue(Self).AsInterface as TIJX2;
        if Assigned(LTIJX2) then
          LField.SetValue(ADest, TValue.From<IJX2>(TIJX2(LTIJX2).CloneSelf));

      end;
    end;
  end;
end;

{$ENDREGION 'Classes'}

end.


