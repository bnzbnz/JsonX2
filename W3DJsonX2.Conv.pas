unit W3DJsonX2.Conv;

interface
uses
    W3DJsonX2.Types
  , W3DJsonX2.Obj
  , W3DJsonX2.Tools
  , SysUtils
  , RTTI
  , System.Generics.Collections
  , Classes
  ;

type

  TStringListConv = class(TJX2Converter)
    function ToJson(ASelfObj: TObject): string;  override;
    function FromJson(JsonObject: PJsonDataValue) : TObject; override;
    function Clone(ASelfObj: TObject): TObject; override;
  end;
  TStrValueDicConv = class(TJX2Converter)
    function ToJson(ASelfObj: TObject): string;  override;
    function FromJson(JsonObject: PJsonDataValue) : TObject; override;
    function Clone(ASelfObj: TObject): TObject; override;
  end;

implementation

function TStringListConv.Clone(ASelfObj: TObject): TObject;
begin
  Result := TStringList.Create;
  for var LStr in TStringList(ASelfObj) do
    TStringList(Result).Add(LStr);
end;

function TStringListConv.FromJson(JsonObject: PJsonDataValue): TObject;
var
  LIdx: string;
begin
  Result := TStringList.Create;
  for LIdx in JsonObject.ArrayValue do
    TStringList(Result).Add(LIdx);
end;

function TStringListConv.ToJson(ASelfObj: TObject): string;
var
  LStr: string;
  LArr: TJSONArray;
begin
  LArr := (TJSONArray).Create;
  for LStr in TStringList(ASelfObj) do LArr.Add(LStr);
  Result := LArr.ToJSON();
  LArr.Free;
end;

function TStrValueDicConv.Clone(ASelfObj: TObject): TObject;
var
  Lkv: TPair<string, TValue>;
begin
  Result := TDictionary<string,TValue>.Create;
  for Lkv in TDictionary<string, TValue>(ASelfObj) do
    TDictionary<string, TValue>(Result).Add(Lkv.Key, Lkv.Value);
end;

function TStrValueDicConv.FromJson(JsonObject: PJsonDataValue): TObject;
var
  LIdx: Integer;
begin
  Result := TJX2StrValueDic.Create;
  for LIdx := 0 to JsonObject.ObjectValue.count - 1 do
    TJX2StrValueDic(Result).Add(
        JsonObject.ObjectValue.Names[LIdx]
      , JsonTypeToTValue(JsonObject.ObjectValue.Values[JsonObject.ObjectValue.Names[LIdx]])
    );
end;

function TStrValueDicConv.ToJson(ASelfObj: TObject): string;
var
  LStrValueObj : TDictionary<string, TValue>;
  LSL: TStringList;
  LStrValue: TPair<string, TValue>;
begin
  LStrValueObj := TDictionary<string, TValue>(ASelfObj);
  LSL := TStringList.Create(#0, ',', [soStrictDelimiter]);
  LSL.Capacity := LStrValueObj.count;
  for LStrValue in LStrValueObj do
      LSL.Add(StrToJSONValue(LStrValue.Key) + ':' + ValueToJSONValue(LStrValue.Value));
  Result := '{' + LSL.DelimitedText + '}';
  LSL.Free;
end;

end.

end.
