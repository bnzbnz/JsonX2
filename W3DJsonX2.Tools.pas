unit W3DJsonX2.Tools;

interface

uses
  W3DJsonX2.Obj
  {$IFNDEF JSX_NOVAR}
  , System.Variants
  {$ENDIF}
  , SysUtils
  , RTTI
  ;

  function LenStr(var AStr: string): Integer; inline;
  function JsonBeautifier(AJsonStr : string): string;

  {$IFNDEF JSX_NOVAR}
  function JsonTypeToTVariant(AJValues: TJsonDataValueHelper): Variant;
  function VariantToJSONValue(AVariant: Variant; AForcedString: Boolean = False): string;
  {$ENDIF}

  function JsonTypeToTValue(AJValues: TJsonDataValueHelper): TValue;
  function ValueToStr(aValue: TValue; var ASuccess: Boolean): string;
  function EscapeJSONStr(AStr: string): string;
  function StrToJSONValue(AString: string): string;
  function ValueIsString(AValue: TValue): Boolean;
  function ValueToJSONValue(AValue: TValue; AForcedString: Boolean = False): string;

implementation

function JsonTypeToTValue(AJValues: TJsonDataValueHelper): TValue;
begin
  Result := nil;
  case AJValues.Typ of
    jdtNone:;
    jdtString: Result := AJValues.Value;
    jdtInt: Result := AJValues.IntValue;
    jdtLong: Result := AJValues.LongValue;
    jdtULong: Result := AJValues.ULongValue;
    jdtFloat: Result := AJValues.FloatValue;
    jdtDateTime: Result :=AJValues.DateTimeValue;
    jdtUtcDateTime: Result := AJValues.UtcDateTimeValue;
    jdtBool: Result := AJValues.BoolValue;
    jdtArray: raise Exception.Create('JsonTypeToValue cannot convert to Array');
    jdtObject: raise Exception.Create('JsonTypeToValue cannot convert to Objects');
  end;
end;

{$IFNDEF JSX_NOVAR}
function JsonTypeToTVariant(AJValues: TJsonDataValueHelper): Variant; // TJsonDataValueHelper
begin
  Result := null;
  case AJValues.Typ of
    jdtNone:;
    jdtString: Result := AJValues.Value;
    jdtInt: Result := AJValues.IntValue;
    jdtLong: Result := AJValues.LongValue;
    jdtULong: Result := AJValues.ULongValue;
    jdtFloat: Result := AJValues.FloatValue;
    jdtDateTime: Result :=AJValues.DateTimeValue;
    jdtUtcDateTime: Result := AJValues.UtcDateTimeValue;
    jdtBool: Result := AJValues.BoolValue;
    jdtArray: raise Exception.Create('JsonTypeToValue cannot convert to Array');
    jdtObject: raise Exception.Create('JsonTypeToValue cannot convert to Objects');
  end;
end;
{$ENDIF}

function ValueToStr(aValue: TValue; var ASuccess: Boolean): string;
begin
  ASuccess := True;
  Result := '';
  case AValue.kind of
    tkUnknown: Result :='null';
    tkInteger: Result := IntToStr(AValue.AsInteger);
    tkChar: Result := AValue.asString;
    tkEnumeration: Result := '';
    tkFloat: Result := FloatToStr(AValue.AsExtended);
    tkString: Result := AValue.asString;
    tkSet: ASuccess := False;
    tkClass: ASuccess := False;
    tkMethod: ASuccess := False;
    tkWChar: Result := AValue.asString;
    tkLString: Result := AValue.asString;
    tkWString: Result := AValue.asString;
{$IFNDEF JSX_NOVAR}
    tkVariant: Result := VarToStr(AValue.AsVariant);
{$ENDIF}
    tkArray: ASuccess := False;
    tkRecord: ASuccess := False;
    tkInterface: ASuccess := False;
    tkInt64: Result := IntToStr(AValue.AsInt64);
    tkDynArray: ASuccess := False;
    tkUString:  Result := AValue.asString;
    tkClassRef: ASuccess := False;
    tkPointer: ASuccess := False;
    tkProcedure: ASuccess := False;
    tkMRecord: ASuccess := False;
  end;
end;


{$IFNDEF JSX_NOVAR}
function VariantToJSONValue(AVariant: Variant; AForcedString: Boolean = False): string;
begin
  if VarIsStr(AVariant) or AForcedString then
    Result := StrToJSONValue(AVariant)
  else
    Result := VarToStr(AVariant);
end;
{$ENDIF}


function EscapeJSONStr(AStr: string): string;
var
  LP: PChar;
  LEndP: PChar;
begin
  Result := '';
  if Astr.IsEmpty then
    exit;
  LP := PChar(Pointer(AStr));
  LEndP := LP + PInteger(@PByte(AStr)[-4])^;
  while LP < LendP do
  begin
    case LP^ of
      #0 .. #31, '\', '"':
          Result := Result + '\' + LP^;
      else
        Result := Result + LP^;
    end;
    Inc(LP);
  end;
end;

function StrToJSONValue(AString: string): string;
begin
  Result := '"' + EscapeJSONStr(AString) + '"';
end;

function ValueIsString(AValue: TValue): Boolean;
begin
  Result := (AValue.Kind in [tkChar, tkString, tkWChar, tkLString, tkWString, tkUString]);
end;

function ValueToJSONValue(AValue: TValue; AForcedString: Boolean = False): string;
var
  LSuccess: Boolean;
begin
  if AForcedString or ValueIsString(AValue) then
    Result := StrToJSONValue(ValueToStr(AValue, LSuccess))
  else
    Result := ValueToStr(AValue, LSuccess);
end;

function LenStr(var AStr: string): Integer; inline;
begin
  Result := PInteger(@PByte(AStr)[-4])^
end;

function JsonBeautifier(AJsonStr : string): string;
var
  LJsonObj: TJsonObject;
begin
  Result := '{}';
  LJsonObj := TJsonObject(W3DJsonX2.Obj.TJsonObject.NewInstance);
  try
    LJsonObj.FromJSON(AJsonStr);
    Result := LJsonObj.ToJSon(False);
  finally
    LJsonObj.Free;
  end;
end;


end.
