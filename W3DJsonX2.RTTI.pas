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

{$DEFINE JSX_NOCACHE}

function  GetFields(aObj: TObject): TArray<TRTTIField>;
function  GetProps(aObj: TObject): TArray<TRTTIProperty>;
function  GetFieldAttribute(Field: TRTTIField; AttrClass: TClass): TCustomAttribute;
function  GetFieldInstance(Field: TRTTIField) : TRttiInstanceType;

{$IFNDEF JSX_NOCACHE}
var
  _Cleaner: Integer;
  _RTTIFieldsCacheDic: TDictionary<TClass, TArray<TRttiField>>;
  _RTTIPropsCacheDic: TDictionary<TClass, TArray<TRTTIProperty>>;
  _RTTIAttrsCacheDic: TDictionary<TRTTIField, TCustomAttribute>;
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
  CType := aObj.ClassType;
  MonitorEnter(_RTTIFieldsCacheDic);
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
  CType := aObj.ClassType;
  MonitorEnter(_RTTIPropsCacheDic);
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
  MonitorEnter(_RTTIAttrsCacheDic);
  if not _RTTIAttrsCacheDic.TryGetValue(Field, Result) then
  begin
    Result := GetRTTIFieldAttribute(Field, AttrClass);
    _RTTIAttrsCacheDic.Add(Field, Result);
  end;
  MonitorExit(_RTTIAttrsCacheDic);
{$ELSE}
    Result := GetRTTIFieldAttribute(Field, AttrClass);
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
  _RTTIAttrsCacheDic := TDictionary<TRTTIField, TCustomAttribute>.Create;
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
