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

unit JsonX2.RTTI;

interface
uses
    RTTI
  , System.Generics.Collections
  , SyncObjs
  , JsonX2.Sync
  ;

{.$DEFINE JSX_NOCACHE}

function  GetRTTIFields(aObj: TObject): TArray<TRTTIField>;
function  GetRTTIProps(aObj: TObject): TArray<TRTTIProperty>;
function  GetRTTIAttribute(Field: TRTTIField; AttrClass: TClass): TCustomAttribute;
function  GetRTTIInstance(Field: TRTTIField) : TRttiInstanceType;

{$IFNDEF JSX_NOCACHE}
var
  _RTTIFieldsCacheDic: TThreadDict<TClass, TArray<TRttiField>>;
  _RTTIPropsCacheDic: TThreadDict<TClass, TArray<TRTTIProperty>>;
  _RTTIAttrsCacheDic: TThreadDict<TRTTIField, TArray<TCustomAttribute>>;
  _RTTIInstCacheDic: TThreadDict<TRTTIField, TRttiInstanceType>;
  _RTTIctx: TRttiContext;
{$ELSE}
var
  _RTTIctx: TRttiContext;
{$ENDIF}

implementation
uses JsonX2.Utils;


function GetRTTIFields(aObj: TObject): TArray<TRTTIField>;
{$IFNDEF JSX_NOCACHE}
begin
  if not _RTTIFieldsCacheDic.TryGetValue(aObj.ClassType, Result) then
  begin
    Result :=  _RTTIctx.GetType(aObj.ClassType).GetFields;
    _RTTIFieldsCacheDic.Add(aObj.ClassType, Result);
  end;
end;
{$ELSE}
begin
  Result := _RTTIctx.GetType(aObj.ClassType).GetFields;
end;
{$ENDIF}

function GetRTTIProps(aObj: TObject): TArray<TRTTIProperty>;
{$IFNDEF JSX_NOCACHE}
begin
  if not _RTTIPropsCacheDic.TryGetValue(aObj.ClassType, Result) then
  begin;
    Result :=  _RTTIctx.GetType(aObj.ClassType).GetProperties;
    _RTTIPropsCacheDic.Add(aObj.ClassType, Result);
  end;
end;
{$ELSE}
begin
  Result := _RTTIctx.GetType(aObj.ClassType).GetProperties;
end;
{$ENDIF}

function GetRTTIAttribute(Field: TRTTIField; AttrClass: TClass): TCustomAttribute;
var
  LIdx: Integer;
  LAttrs: TArray<TCustomAttribute>;
begin

  {$IFNDEF JSX_NOCACHE}
  Result := nil;
  if not _RTTIAttrsCacheDic.TryGetValue(Field, LAttrs) then
  begin
    LAttrs := Field.GetAttributes;
    _RTTIAttrsCacheDic.Add(Field, LAttrs);
  end;
  for LIdx := Length(LAttrs) - 1 downto 0 do
    if LAttrs[LIdx].ClassType = AttrClass then
    begin
      Result := LAttrs[LIdx];
      Break;
    end;
  {$ELSE}

    {$IF CompilerVersion > 34.0} // Alexandria 11, Athens 12
    begin
      Result := Field.GetAttribute(TCustomAttributeClass(AttrClass));
    end;
    {$ELSE}
    begin
      Result := nil;
      LAttrs := Field.GetAttributes;
      for LIdx := 0 to Length(LAttrs) - 1 do
        if LAttrs[LIdx].ClassType = AttrClass then
        begin
            Result := LAttrs[LIdx];
            Break;
        end;
    end;
    {$ENDIF}

{$ENDIF}
end;

function GetRTTIInstance(Field: TRTTIField) : TRttiInstanceType;
begin
{$IFNDEF JSX_NOCACHE}
  if not _RTTIInstCacheDic.TryGetValue(Field, Result) then
  begin
   Result := Field.FieldType.AsInstance;
    _RTTIInstCacheDic.Add(Field, Result);
  end;
{$ELSE}
  Result := Field.FieldType.AsInstance;
{$ENDIF}
end;

initialization
{$IFNDEF JSX_NOCACHE}
  _RTTIFieldsCacheDic := TThreadDict<TClass, TArray<TRttiField>>.Create;
  _RTTIPropsCacheDic := TThreadDict<TClass, TArray<TRttiProperty>>.Create;
  _RTTIAttrsCacheDic := TThreadDict<TRTTIField, TArray<TCustomAttribute>>.Create;
  _RTTIInstCacheDic := TThreadDict<TRTTIField, TRttiInstanceType>.Create;

{$ENDIF}
finalization
{$IFNDEF JSX_NOCACHE}
  _RTTIInstCacheDic.Free;
  _RTTIAttrsCacheDic.Free;
  _RTTIPropsCacheDic.Free;
  _RTTIFieldsCacheDic.Free;
{$ENDIF}
end.
