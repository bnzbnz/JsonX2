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

unit W3DJsonX2.Utils;

{$IFDEF W3DCLIENT}
  {$DEFINE JSX_NOVAR}
{$ENDIF}

interface
uses
    Classes
  , Sysutils
  , RTTI;

type
  // TStream Helpers
  TStreamHelper = class helper for TStream
    function WriteRawAnsiString(Str: AnsiString): Integer;
    function WriteRawUTF8String(Str: UTF8String): Integer;
    function WriteRawUnicodeString(Str: string): Integer;
    function ReadRawString(Encoding: TEncoding): string;
    function ToStringStream(DefaultString: string;  Encoding: TEncoding): TStringStream;
    function ToString(Encoding: TEncoding): string; overload;
  end;
  // HTTP
  function  URLEncode(const ToEncode: string): string;
  // Strings
  function  LoadStringFromFile(Filename: string; Encoding: TEncoding): string;
  // Tools
  function  IIF(Condition: Boolean; IsTrue, IsFalse: TObject): TObject; overload;
  {$IF defined(JSX_NOVAR)}
  function  IIF(Condition: Boolean; IsTrue, IsFalse: variant): variant; overload;
  {$ELSE}
  function  IIF(Condition: Boolean; IsTrue, IsFalse: TValue): TValue; overload;
  {$ENDIF}
  function  StringGUID: string;
  function  DelphiGUID: string;
  procedure BreakPoint(Msg: string = 'BreakPoint'; NoBreak: Boolean = False);
  // Windows System
  {$IF defined(MSWINDOWS)}
  function  GetProcessMemory(ProcessHandle: THandle): NativeUInt;
  function  Is_x64: Boolean;
  {$ENDIF}

implementation

uses
  System.Net.URLClient
  {$IF defined(MSWINDOWS)}
  , psAPI
  , Windows
  {$ENDIF}
  ;

function URLEncode (const ToEncode: string): string;
begin
  // Having issues with TNetEncoding.URL.Encode()   (space being + instead of %20...)
  {$WARNINGS OFF}
  Result := System.Net.URLClient.TURI.URLEncode(ToEncode);
  {$WARNINGS ON}
end;

{$IF defined(JSX_NOVAR)}
function IIF(Condition: Boolean; IsTrue: variant; IsFalse: variant): variant;
begin
  if Condition then Result := IsTrue else Result := IsFalse;
end;
{$ENDIF}


function IIF(Condition: Boolean; IsTrue, IsFalse: TObject): TObject;
begin
  if Condition then Result := IsTrue else Result := IsFalse;
end;

function IIF(Condition: Boolean; IsTrue, IsFalse: TValue): TValue;
begin
  if Condition then Result := IsTrue else Result := IsFalse;
end;

{$IF defined(DEBUG) and defined(MSWINDOWS)}
procedure X64BRK ; assembler;
asm
  int 3; // Press F7 to access the error message
end;
{$ENDIF}

procedure BreakPoint(Msg: string; NoBreak: Boolean);
begin
  {$IF defined(DEBUG) and defined(MSWINDOWS)}
    {$IF defined(CPUX86)}
      OutputDebugString(PChar(Msg));
      if Not NoBreak then
        asm int 3; end;
      Msg := Msg; // << Error Message Here
    {$ENDIF}
    {$IF defined(CPUX64)}
      OutputDebugString(PChar(Msg));
      if Not NoBreak then
      X64BRK;
      Msg := Msg; // << Error Message Here
    {$ENDIF}
  {$ELSE}
     raise Exception.Create(Msg);
  {$ENDIF}
end;

function StringGUID: string;
var
  Guid: TGUID;
begin
  CreateGUID(Guid);
  Result := Format(
    '%0.8X%0.4X%0.4X%0.2X%0.2X%0.2X%0.2X%0.2X%0.2X%0.2X%0.2X',
    [Guid.D1, Guid.D2, Guid.D3,
    Guid.D4[0], Guid.D4[1], Guid.D4[2], Guid.D4[3],
    Guid.D4[4], Guid.D4[5], Guid.D4[6], Guid.D4[7]]
  );
end;

function DelphiGUID: string;
var
  Guid: TGUID;
begin
  CreateGUID(Guid);
  Result := Format(
    '[''{%0.8X-%0.4X-%0.4X-%0.4X-%0.6x}'']',
    [Guid.D1, Guid.D2, Guid.D3,
    Guid.D4[0], Guid.D4[1]]
  );
end;

{ TStreamHelper }

function TStreamHelper.WriteRawAnsiString(Str: AnsiString): Integer;
begin
  Result := Length(Str);
  Self.Write(Str[1], Result);
end;

function TStreamHelper.WriteRawUTF8String(Str: UTF8String): Integer;
begin
  Result := Length(Str);
  Self.Write(Str[1], Result);
end;

function TStreamHelper.WriteRawUnicodeString(Str: string): Integer;
begin
  REsult :=  Length(Str) * SizeOf(Char);
  Self.Write(Str[1], Result);
end;

function TStreamHelper.ReadRawString( Encoding: TEncoding ): string;
var
  StringBytes: TBytes;
  OPs: Int64;
begin
  OPs := Position;
  Self.Position := 0;
  SetLength(StringBytes, Self.Size);
  self.ReadBuffer(StringBytes, Self.Size);
  Result := Encoding.GetString(StringBytes);
  Position := OPs;
end;

function TStreamHelper.ToString(Encoding: TEncoding): string;
var
  SS: TStringStream;
  OPs: Int64;
begin
  SS := TStringStream.Create('', Encoding);
  OPs := Self.Position;
  SS.CopyFrom(Self, -1);
  Result := SS.DataString;
  Self.Position := OPs;
  SS.Free;
end;

function TStreamHelper.ToStringStream( DefaultString: string;  Encoding: TEncoding ): TStringStream;
var
  OPs: Int64;
begin
  Result := TStringStream.Create(DefaultString, Encoding);
  if Self.Size = 0 then Exit;
  OPs := Self.Position;
  Result.Position := Result.Size;
  Result.CopyFrom(Self, -1);
  Self.Position := OPs;
  Result.Position := 0;
end;

function StringToBytes(const Value : WideString): TBytes;
begin
  SetLength(Result, Length(Value)*SizeOf(WideChar));
  if Length(Result) > 0 then
    Move(Value[1], Result[0], Length(Result));
end;

function BytesToString(const Value: TBytes): WideString;
begin
  SetLength(Result, Length(Value) div SizeOf(WideChar));
  if Length(Result) > 0 then
    Move(Value[0], Result[1], Length(Value));
end;

function LoadStringFromFile(Filename: string; Encoding: TEncoding): string;
var
  FS : TFileStream;
begin
  Result := '';
  FS := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
  try
    Result := FS.ReadRawString(Encoding);
  finally
    FS.Free;
  end;
end;

{$IF defined(MSWINDOWS)}
function GetProcessMemory(ProcessHandle: THandle): NativeUInt;
var
  MemCounters: TProcessMemoryCounters;
begin
  Result := 0;
  MemCounters.cb := SizeOf(MemCounters);
  if GetProcessMemoryInfo(ProcessHandle,
      @MemCounters,
      SizeOf(MemCounters)) then
    Result := MemCounters.WorkingSetSize
end;

function Is_x64: Boolean;
type
  TIsWow64Process = function(AHandle: DWORD; var AIsWow64: BOOL): BOOL; stdcall;
var
  hIsWow64Process: TIsWow64Process;
  hKernel32: DWORD;
  IsWow64: BOOL;
begin
  Result := False;
  hKernel32 := LoadLibrary('kernel32.dll');
  if hKernel32 = 0 then Exit;
  try
    @hIsWow64Process := GetProcAddress(hKernel32, 'IsWow64Process');
    if not System.Assigned(hIsWow64Process) then Exit;
    IsWow64 := False;
    if hIsWow64Process(GetCurrentProcess, IsWow64) then
      Result := IsWow64;
  finally
    FreeLibrary(hKernel32);
  end;
end;
{$ENDIF}
end.
