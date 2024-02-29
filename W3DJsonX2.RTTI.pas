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

{.$DEFINE JSX_NOCACHE}

interface
uses
    RTTI
  , System.Generics.Collections
  , SyncObjs
  ;



function  GetFields(aObj: TObject): TArray<TRTTIField>;
function  GetProps(aObj: TObject): TArray<TRTTIProperty>;
function  GetFieldAttribute(Field: TRTTIField; AttrClass: TClass): TCustomAttribute;
function  GetFieldInstance(Field: TRTTIField) : TRttiInstanceType;

{$IFNDEF JSX_NOCACHE}
var
  _GCleaner: Integer;
  _GRTTIFieldsCacheDic: TDictionary<TClass, TArray<TRttiField>>;
  _GRTTIPropsCacheDic: TDictionary<TClass, TArray<TRTTIProperty>>;
  _GRTTIAttrsCacheDic: TDictionary<TRTTIField, TCustomAttribute>;
  _GRTTIInstCacheDic: TDictionary<TRTTIField, TRttiInstanceType>;
  _GRTTIctx: TRttiContext;
  _GJRTTICache: array[0..65535] of TArray<TRttiField>;
  _GFielddLock : TCriticalSection;
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
  CType := aObj.ClassType;
  MonitorEnter(_GRTTIFieldsCacheDic);
  if not _GRTTIFieldsCacheDic.TryGetValue(CType, Result) then
  begin
    Result :=  _GRTTIctx.GetType(CType).GetFields;
    _GRTTIFieldsCacheDic.Add(CType, Result);
  end;
  MonitorExit(_GRTTIFieldsCacheDic);
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
  CType := aObj.ClassType;
  MonitorEnter(_GRTTIPropsCacheDic);
  if not _GRTTIPropsCacheDic.TryGetValue(CType, Result) then
  begin
    Result :=  _GRTTIctx.GetType(CType).GetProperties;
    _GRTTIPropsCacheDic.Add(CType, Result);
  end;
  MonitorExit(_GRTTIPropsCacheDic);
end;
{$ELSE}
begin
  Result := _RTTIctx.GetType(aObj.ClassType).GetProperties;
end;
{$ENDIF}


function GetFieldAttribute(Field: TRTTIField; AttrClass: TClass): TCustomAttribute;

  function GetRTTIFieldAttribute(RTTIField: TRTTIField; AttrClass: TClass): TCustomAttribute;
  {$IF CompilerVersion >= 35.0} // Alexandria 11.0
  begin
    Result := RTTIField.GetAttribute(TCustomAttributeClass(AttrClass));
  end;
  {$ELSE}
  var
    Attr: TArray<TCustomAttribute>;
  begin
    Result := Nil;
    for Attr in RTTIField.GetAttributes do
      if Attr.ClassType = AttrClass then
      begin
          Result := Attr;
          Break;
      end;
  end;
  {$IFEND}

begin
{$IFNDEF JSX_NOCACHE}
  MonitorEnter(_GRTTIAttrsCacheDic);
  if not _GRTTIAttrsCacheDic.TryGetValue(Field, Result) then
  begin
    Result := GetRTTIFieldAttribute(Field, AttrClass);
    _GRTTIAttrsCacheDic.Add(Field, Result);
  end;
  MonitorExit(_GRTTIAttrsCacheDic);
{$ELSE}
    Result := GetRTTIFieldAttribute(Field, AttrClass);
{$ENDIF}
end;

function  GetFieldInstance(Field: TRTTIField) : TRttiInstanceType;
begin
{$IFNDEF JSX_NOCACHE}
  MonitorEnter(_GRTTIInstCacheDic);
  if not _GRTTIInstCacheDic.TryGetValue(Field, Result) then
  begin
    Result := Field.FieldType.AsInstance;
    _GRTTIInstCacheDic.Add(Field, Result);
  end;
  MonitorExit(_GRTTIInstCacheDic);
{$ELSE}
  Result := Field.FieldType.AsInstance;
{$ENDIF}
end;

initialization
{$IFNDEF JSX_NOCACHE}
  _GRTTIFieldsCacheDic := TDictionary<TClass, TArray<TRttiField>>.Create;
  _GRTTIPropsCacheDic := TDictionary<TClass, TArray<TRttiProperty>>.Create;
  _GRTTIAttrsCacheDic := TDictionary<TRTTIField, TCustomAttribute>.Create;
  _GRTTIInstCacheDic := TDictionary<TRTTIField, TRttiInstanceType>.Create;
  for _GCleaner :=0 to 65535 do _GJRTTICache[_GCleaner] := Nil;
  _GFielddLock := TCriticalSection.Create;
{$ENDIF}
finalization
{$IFNDEF JSX_NOCACHE}
  _GFielddLock.Free;
  _GRTTIInstCacheDic.Free;
  _GRTTIAttrsCacheDic.Free;
  _GRTTIPropsCacheDic.Free;
  _GRTTIFieldsCacheDic.Free;
{$ENDIF}
end.
