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

interface
uses Sysutils, Classes;

const
  BStrL: array[Boolean] of string = ('false','true');
  BStrK: array[Boolean] of string = ('False','True');
  BStrU: array[Boolean] of string = ('FALSE','TRUE');

type
  // TStream Helpers
  TStreamHelper = class helper for TStream
    function WriteRawAnsiString( Str: AnsiString ): Integer;
    function WriteRawUTF8String( Str: UTF8String ): Integer;
    function WriteRawUnicodeString( Str: string ): Integer;
    function ReadRawString( Encoding: TEncoding ): string;
    function ToStringStream( DefaultString: string;  Encoding: TEncoding ): TStringStream;
    function ToString(  Encoding: TEncoding  ): string; overload;
  end;
  // HTTP
  function URLEncode(const ToEncode: string): string;
  // Strings
  function  FPos(const aSubStr, aString : string; aStartPos: Integer = 1): Integer;
  function  RPos(const aSubStr, aString : string; aStartPos: Integer): Integer;
  function  OnceFastReplaceStr(var Str: string; const  SubStr: string; const RplStr : string; StartPos: integer; Backward: Boolean = False): Integer;
  // Tools
  function  IIF(Condition: Boolean; IsTrue, IsFalse: variant): variant; overload;
  function  IIF(Condition: Boolean; IsTrue, IsFalse: TObject): TObject; overload;
  function  StringGUID: string;
  function  DelphiGUID: string;
  procedure BreakPoint(Msg: string = 'BreakPoint'; NoBreak: Boolean = False);
  {$IF defined(MSWINDOWS)}
  function  GetProcessMemory(ProcessHandle: THandle): NativeUInt;
  // System
  
  function Is_x64: Boolean;
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

function IIF(Condition: Boolean; IsTrue: variant; IsFalse: variant): variant;
begin
  if Condition then Result := IsTrue else Result := IsFalse;
end;

function IIF(Condition: Boolean; IsTrue, IsFalse: TObject): TObject;
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

function LenStr(const Str: string): Integer; inline;
begin
  Result :=  PInteger(@PByte(Str)[-4])^;
end;

function FPos(const aSubStr, aString : string; aStartPos: Integer): Integer; inline;
var
  Len: Int64;
begin
  Len := LenStr(aSubStr) * Sizeof(Char);
  for Result := 1 to  LenStr(aString) -  LenStr(aSubStr) + 1  do
    if CompareMem(Pointer(aSubStr),  PByte(aString) +((Result - 1) * Sizeof(Char)), Len) then Exit;
  Result := 0;
end;

function RPos(const aSubStr, aString : string; aStartPos: Integer): Integer; inline;
var
  Len: Int64;
begin
  Len :=  LenStr(aSubStr) * Sizeof(Char);
  for Result := (aStartPos - LenStr(aSubStr)) + 1  downto 1 do
    if CompareMem(Pointer(aSubStr),  PByte(aString) +((Result - 1) * Sizeof(Char)), Len) then Exit;
  Result := 0;
end;

function OnceFastReplaceStr(var Str: string; const SubStr: string; const RplStr : string; StartPos: integer; Backward: Boolean = False) : Integer;
var
  s: string;
  N0, N1, N2, Src, Dst, Len : Int64;
begin

  if Backward then
    Result := RPos(SubStr, Str, StartPos)
  else
    Result := FPos(SubStr, Str, StartPos);

  if Result = 0 then Exit;

  N1 := LenStr(SubStr);
  N2 := LenStr(RplStr);
  if N2 = N1 then
  begin
    Move(RplStr[1], Str[Result], N2 * SizeOf(Char));
  end else
  if N2 > N1 then
  begin
    N0 := LenStr(Str);
    SetLength(Str, N0 +  N2 - N1) ;
    Src := Result + N1;
    Dst := Result + N2;
    Len := LenStr(Str) - (Result-1 + N2);
    Move(Str[Src], Str[Dst], Len * SizeOf(Char));
    Move(RplStr[1], Str[Result], N2 * SizeOf(Char));
  end else
  if N1 > N2 then
  begin
    N0 := LenStr(Str);
    Move(RplStr[1], Str[Result], N2 * SizeOf(Char));
    Src := Result + N1;
    Dst := Result + N2;
    Len := N0 - (Result-1 + N1) ;
    s := Src.ToString + ' - ' + Dst.ToString + ' - ' + Len.ToString;
    Move( Str[Src], Str[Dst], Len * SizeOf(Char) );
    SetLength(Str, N0 + (N2 - N1));
  end;

  if Backward then Result := Result + N2;

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
