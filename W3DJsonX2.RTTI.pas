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

unit W3DJsonX2.RTTI;

interface
uses RTTI, System.Generics.Collections, SyncObjs;

{.$DEFINE JSX_NOCACHE}

function  GetFields(aObj: TObject): TArray<TRTTIField>;
function  GetProps(aObj: TObject): TArray<TRTTIProperty>;
function  GetFieldAttribute(Field: TRTTIField; AttrClass: TClass): TCustomAttribute;
function  GetFieldInstance(Field: TRTTIField) : TRttiInstanceType;

{$IFNDEF JSX_NOCACHE}
var
  _Cleaner: Integer;
  _RTTIFieldsCacheDic: TDictionary<TClass, TArray<TRttiField>>;
  _RTTIPropsCacheDic: TDictionary<TClass, TArray<TRTTIProperty>>;
  _RTTIAttrsCacheDic: TDictionary<TRTTIField, TArray<TCustomAttribute>>;
  _RTTIInstCacheDic: TDictionary<TRTTIField, TRttiInstanceType>;
  _RTTIctx: TRttiContext;
  _JRTTICache: array[0..65535] of TArray<TRttiField>;
  _FielddLock : TCriticalSection;
{$ELSE}
var
  _RTTIctx: TRttiContext;
{$ENDIF}

implementation
uses W3DJsonX2.Types, W3DJsonX2.Utils;


function GetFields(aObj: TObject): TArray<TRTTIField>;
{$IFNDEF JSX_NOCACHE}
var
  CType: TClass;
begin
  MonitorEnter(_RTTIFieldsCacheDic);
  CType := aObj.ClassType;
  if not _RTTIFieldsCacheDic.TryGetValue(CType, Result) then
  begin
    Result :=  _RTTIctx.GetType(CType).GetFields;
    _RTTIFieldsCacheDic.Add(CType, Result);
  end;
  MonitorExit(_RTTIFieldsCacheDic);
end;
{$ELSE}
begin
  Result := _RTTIctx.GetType(aObj.ClassType).GetFields;
end;
{$ENDIF}

function GetProps(aObj: TObject): TArray<TRTTIProperty>;
{$IFNDEF JSX_NOCACHE}
var
  CType: TClass;
begin
  MonitorEnter(_RTTIPropsCacheDic);
  CType := aObj.ClassType;
  if not _RTTIPropsCacheDic.TryGetValue(CType, Result) then
  begin
    Result :=  _RTTIctx.GetType(CType).GetProperties;
    _RTTIPropsCacheDic.Add(CType, Result);
  end;
  MonitorExit(_RTTIPropsCacheDic);
end;
{$ELSE}
begin
  Result := _RTTIctx.GetType(aObj.ClassType).GetProperties;
end;
{$ENDIF}


function GetFieldAttribute(Field: TRTTIField; AttrClass: TClass): TCustomAttribute;
var
  LIdx: Integer;
  LAttr: TArray<TCustomAttribute>;
begin

{$IFNDEF JSX_NOCACHE}
  MonitorEnter(_RTTIAttrsCacheDic);
  Result := Nil;
  if not _RTTIAttrsCacheDic.TryGetValue(Field, LAttr) then
  begin
     LAttr := Field.GetAttributes;
    _RTTIAttrsCacheDic.Add(Field, LAttr);
  end;
  for LIdx := 0 to Length(LAttr) - 1 do
    if LAttr[LIdx].ClassType = AttrClass then
    begin
      Result := LAttr[LIdx];
      Break;
    end;
  MonitorExit(_RTTIAttrsCacheDic);
{$ELSE}

  {$IF CompilerVersion >= 35.0} // Alexandria 11.0
  begin
    Result := Field.GetAttribute(TCustomAttributeClass(AttrClass));
  end;
  {$ELSE}
  begin
    Result := Nil;
    LAttr := Field.GetAttributes;
    for LIdx := 0 to Length(LAttr) - 1 do
      if LAttr[LIdx].ClassType = AttrClass then
      begin
          Result := LAttr[LIdx];
          Break;
      end;
  end;
  {$ENDIF}

{$ENDIF}
end;

function  GetFieldInstance(Field: TRTTIField) : TRttiInstanceType;
begin
{$IFNDEF JSX_NOCACHE}
  MonitorEnter(_RTTIInstCacheDic);
  if not _RTTIInstCacheDic.TryGetValue(Field, Result) then
  begin
    Result := Field.FieldType.AsInstance;
    _RTTIInstCacheDic.Add(Field, Result);
  end;
  MonitorExit(_RTTIInstCacheDic);
{$ELSE}
  Result := Field.FieldType.AsInstance;
{$ENDIF}
end;

initialization
{$IFNDEF JSX_NOCACHE}
  _RTTIFieldsCacheDic := TDictionary<TClass, TArray<TRttiField>>.Create;
  _RTTIPropsCacheDic := TDictionary<TClass, TArray<TRttiProperty>>.Create;
  _RTTIAttrsCacheDic := TDictionary<TRTTIField, TArray<TCustomAttribute>>.Create;
  _RTTIInstCacheDic := TDictionary<TRTTIField, TRttiInstanceType>.Create;
  for _Cleaner :=0 to 65535 do _JRTTICache[_Cleaner] := Nil;
  _FielddLock := TCriticalSection.Create;
{$ENDIF}
finalization
{$IFNDEF JSX_NOCACHE}
  _FielddLock.Free;
  _RTTIInstCacheDic.Free;
  _RTTIAttrsCacheDic.Free;
  _RTTIPropsCacheDic.Free;
  _RTTIFieldsCacheDic.Free;
{$ENDIF}
end.
