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

unit JsonX2.Patch;

{$IFDEF W3DCLIENT}
  {$DEFINE JSX_NOVAR}
{$ENDIF}

interface
uses
  System.Generics.Collections
  , JsonX2.Utils
  ;

type

  TJX2Patcher = class(TObject)
  private type
    TJsonXPatcherCtnr = class
    public
      PHdr, PJsn: string;
    end;
  private
    FParts: TObjectList<TJsonXPatcherCtnr>;
    FLowMemL2: Boolean;
  public
    constructor Create; overload;
    destructor Destroy; override;
    procedure Decode(var AJsonStr: string);
    function Encode(AJsonStr: string; AHeader: string = '"';  AFooter: string = '"'): string;
  end;

  function  FPos(const aSubStr, aString : string; aStartPos: Integer = 1): Integer;
  function  RPos(const aSubStr, aString : string; aStartPos: Integer): Integer;
  function  OnceFastReplaceStr(var Str: string; const  SubStr: string; const RplStr : string; StartPos: integer; Backward: Boolean = False): Integer;


implementation
uses SysUtils;

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
    if CompareMem(Pointer(aSubStr),  PByte(aString) + ((Result - 1) * Sizeof(Char)), Len) then Exit;
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


{$REGION 'TJsonXPatcher'}

constructor TJX2Patcher.Create;
begin
  FParts := TObjectList<TJsonXPatcherCtnr>.Create(True);
end;

destructor TJX2Patcher.Destroy;
begin
  FParts.Free;
  inherited;
end;

procedure TJX2Patcher.Decode(var AJsonStr: string);
var
  Idx, Position: Integer;
begin
  Position :=  Length(AJsonStr);
  try
    for Idx := FParts.count - 1 downto 0 do
    begin
      Position := OnceFastReplaceStr(AJsonStr, TJsonXPatcherCtnr(FParts[Idx]).PHdr, TJsonXPatcherCtnr(FParts[Idx]).PJsn, Position, True);
      FParts.Delete(Idx);
    end;
  finally
    FParts.Clear;
  end
end;

function TJX2Patcher.Encode(AJsonStr, AHeader, AFooter: string): string;
var
  Part: TJsonXPatcherCtnr;
  Len: Int64;
begin
  Result  := StringGUID;
  Part := TJsonXPatcherCtnr.Create;
  if Self.FLowMemL2 then
  begin
    Part.PHdr := AHeader + Result + AFooter;
    Part.PJsn := AJsonStr;
  end else begin
    Len := Length(AHeader) + Length(Result) + Length(AFooter) - Length(AJsonStr);
    if Len > 0 then  AJsonStr := AJsonStr + StringOfChar(' ', Len)
    else
    if Len < 0 then  Result := Result + StringOfChar(' ', -Len);
    Part.PHdr := AHeader + Result + AFooter;
    Part.PJsn := AJsonStr;
  end;
  FParts.Add(Part);
end;

{$ENDREGION 'TJsonXPatcher'}



end.
