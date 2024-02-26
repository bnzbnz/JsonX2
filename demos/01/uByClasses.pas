unit uByClasses;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.TypInfo,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo
  {$IFNDEF JSX2_NOVAR}
  , System.Variants
  {$ENDIF}
  , System.Generics.Collections
  , RTTI
  , W3DJsonX2.Obj
  , W3DJsonX2
  , W3DJsonX2.RTTI
  , W3DJsonX2.Types
  , W3DJsonX2.Utils
  , W3DCloneable
  ;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Log(Str: string);
  end;

  ISimpleObject = interface(IInterface) ['{D259E2E2-2EDD-456D-A4F2-4AD9AFA6494C}'] end;


  TSimpleObject = class(TIJX2, ISimpleObject)
  private
    var1: TValue;
  public
    var2: TValue;
    procedure SetVar1(v: TValue);
    function GetVar1: TValue;

    property PVar1: TValue read GetVar1 write SetVar1;
  end;

  TSimpleObject2 = Class(TIJX2);

  TComplexObj = class(TJX2)
  public

//----------------------------------------------------------------------------//

    // The fields order does not matter

    valNull: TValue;
    valString: TValue;
    valInteger: TValue;
{$IFNDEF JSX2_NOVAR}
    valVariantString: Variant;
    valBoolean: Variant;
    valDouble: Variant;
{$ENDIF}
    valDateTime: variant;
    valUTCDateTime: variant; // ISO 8601 UTC Date string
    [JX2AttrName('type')]
    valType: TValue;

    // Object / Interface
    ObjectType: TSimpleObject;
    [JX2AttrClass(TSimpleObject)]
    IntfType: IJX2;

    //List of values;
{$IFNDEF JSX2_NOVAR}
    VaRList: TJX2VarList;
{$ENDIF}
    VaLlist: TJX2ValueList;
{$IFNDEF JSX2_NOVAR}
    [JX2AttrClass(TIJX2VaRList)]
    IVaRlist: IJX2VaRList;
{$ENDIF}
    [JX2AttrClass(TIJX2ValueList)]
    IVaLlist: IJX2ValueList;

    // List of Object;
    [JX2AttrClass(TSimpleObject)]
    ObjList: TJX2ObjList;
    [JX2AttrClass(TIJX2ObjList, TSimpleObject)]
    IObjList: IJX2ObjList;

    // Dictionary of string/Values
{$IFNDEF JSX2_NOVAR}
    StrVarDic: TJX2StrVarDic;
{$ENDIF}
    StrValueDic: TJX2StrValueDic;
    [JX2AttrClass(TSimpleObject)]
    StrObjDic: TJX2StrObjDic;
{$IFNDEF JSX2_NOVAR}
    [JX2AttrClass(TIJX2StrVarDic, TSimpleObject)]
    IStrVarDic: IJX2StrVarDic;
{$ENDIF}
    [JX2AttrClass(TIJX2StrValueDic, TSimpleObject)]
    IStrValueDic: IJX2StrValueDic;
    [JX2AttrClass(TIJX2StrObjDic, TSimpleObject)]
    IStrObjDic: IJX2StrObjDic;
  end;

var
  Form2: TForm2;

implementation
uses DateUtils;

{$R *.fmx}

procedure TForm2.Log(Str: string);
begin
  Memo1.Lines.Add(Str);
end;

procedure TForm2.Button1Click(Sender: TObject);
var
  Json, JsonBeauty: string;
  Obj, CloneObj, ReadObj: TComplexObj;
  Simple: TSimpleObject;

  s: string;
  d: TDateTime;
  v: variant;
  w: TObject;
  y: TPair<Variant,Variant>;
  z: TPair<Variant,TObject>;
  val: TValue;

  FS:  TFileStream;

begin

//----------------------------------------------------------------------------//

  // Create a Json Object
  Obj := TComplexObj.Create;

  //Primitives

  Obj.valNull := nil;
  Obj.valString := 'ooŘaa鱇bb😃cc';
  Obj.valInteger := 15;
{$IFNDEF JSX2_NOVAR}
  Obj.valVariantString := 'bbb';
  Obj.valBoolean := False;
  Obj.valDouble := 2.2;
{$ENDIF}

  Obj.valDateTime := Double(Now);
  Obj.valUTCDateTime := DateToIso8601( TTimeZone.Local.ToUniversalTime(Now) );
  Obj.valType := 'erroneous delphi field name...';

  // Object/Interface

  Obj.ObjectType := TSimpleObject.Create;
  Obj.ObjectType.var1 := 'a var 1 Object';
  Obj.ObjectType.var2 := 'a var 2 Object';

  Obj.IntfType := TSimpleObject.Create;
  TSimpleObject(Obj.IntfType).var1 := 'a var 3 Intf';
  TSimpleObject(Obj.IntfType).var2 := 'a var 4 Intf';

  // Lists (arrays) of Var/Value/Obj

{$IFNDEF JSX2_NOVAR}
  Obj.VaRList := TJX2VarList.Create;
  Obj.VaRList.Add(1);   Obj.VaRList.Add(2);
{$ENDIF}
  Obj.VaLlist := TJX2ValueList.Create;
  Obj.VaLlist.Add(3);   Obj.VaLlist.Add(4);

  Obj.ObjList := TJX2ObjList.Create;
  Simple := TSimpleObject.Create;
  Simple.var1 := 5;
  Simple.var2 := 'five';
  TJX2ObjList(Obj.ObjList).Add(Simple);

{$IFNDEF JSX2_NOVAR}
  Obj.IVaRList := TIJX2VarList.Create;
  TIJX2VaRList(Obj.IVaRList).Add('6A');
  TIJX2VaRList(Obj.IVaRList).Add('7B');

  Obj.IVaLList := TIJX2ValueList.Create;
  TIJX2ValueList(Obj.IVaLList).Add('8C');
  TIJX2ValueList(Obj.IVaLList).Add('9D');
{$ENDIF}

  Obj.IObjList := TIJX2ObjList.Create;
  Simple := TSimpleObject.Create;
  Simple.var1 := 10;
  Simple.var2 := 'ten';
  TIJX2ObjList(Obj.IObjList).Add(Simple);
  Simple := TSimpleObject.Create;
  Simple.var1 := 20;
  Simple.var2 := 'twenny';
  TIJX2ObjList(Obj.IObjList).Add(Simple);

  // Dictionnaries Var/Values/Object
{$IFNDEF JSX2_NOVAR}
  Obj.StrVarDic := TJX2StrVarDic.Create;
  Obj.StrVarDic.Add('1','A');
  Obj.StrVarDic.Add('2', 12);
{$ENDIF}

  Obj.StrValueDic := TJX2StrValueDic.Create;
  Obj.StrValueDic.Add('1', 7);
  Obj.StrValueDic.Add('2', 13);

  Obj.StrObjDic := TJX2StrObjDic.Create;
  Simple := TSimpleObject.Create;
  Simple.var1 := 'ValueKey';
  Simple.var2 := 'ObjValue';
  Obj.StrObjDic.Add('Key1', Simple);
{$IFNDEF JSX2_NOVAR}
  Obj.IStrVarDic := TIJX2StrVarDic.Create;
  TIJX2StrVarDic(Obj.IStrVarDic).Add('3','B');
  TIJX2StrVarDic(Obj.IStrVarDic).Add('4', 13);
{$ENDIF}
  Obj.IStrValueDic := TIJX2StrValueDic.Create;
  TIJX2StrValueDic(Obj.IStrValueDic).Add('3','D');
  TIJX2StrValueDic(Obj.IStrValueDic).Add('4', 14);

  Obj.IStrObjDiC := TIJX2StrObjDic.Create;
  Simple := TSimpleObject.Create;
  Simple.var1 := 'V1';
  Simple.var2 := 'V2';
  TIJX2StrObjDic(Obj.IStrObjDiC).Add('SimpleX', Simple);

//----------------------------------------------------------------------------//

  // Obj Serialization
  Json := W3DJSX2.Serialize(Obj, [jxoReturnEmptyObject]);
  Memo1.Lines.Add( 'Object:');
  Memo1.Lines.Add( Json );

  // Cloning through serialization
  CloneObj := W3DJSX2.Deserialize<TComplexObj>(Json);
  Json := W3DJSX2.Serialize(CloneObj);
  Log( 'Cloned Object (Json Des/Ser):');
  Log( Json );
  CloneObj.Free;

  // Native CLoning
  CloneObj := TComplexObj(Obj.Clone);
  Log( 'Natively Cloned Object:');
  Log( W3DJSX2.Serialize(CloneObj) );
  CloneObj.Free;

  JsonBeauty := JsonBeautifier(Json);
  Log('');
  Log('Json Beautifier :');
  Log(JsonBeauty);

  FS := TFileStream.Create('Beauty.json', fmCreate);
  FS.WriteRawUTF8String(UTF8String(JsonBeauty));
  FS.Free;

//----------------------------------------------------------------------------//

  Log('');
  Log('String (TValue): ' + Obj.valString.AsString);
{$IFNDEF JSX2_NOVAR}
  Log('String (Variant): ' + Obj.valVariantString);
{$ENDIF}
  Log('Double: ' + FloatToStr(Obj.valDouble));

  Log('ObjectType.var1: ' + Obj.ObjectType.Var1.ToString);
  Log('IntfType.var1: ' + TSimpleObject(Obj.IntfType).Var1.ToString());

  Log('VaLlist : ');
  for var i in Obj.VaLlist do
    Log('  ' + IntToStr(i.AsInteger));

  Log('IVaLlist : ');
  for var i in TIJX2ValueList(Obj.IVaLlist) do
    Log(i.AsString);

  Log('ObjList : ');
  for var i in TJX2ObjList(Obj.ObjList) do
  begin
    var O := TSimpleObject(i);
    Log('  ObjectType.var1: ' + o.var1.ToString);
  end;

  Log('StrValueDic : ');
  for var i in TJX2StrValueDic(Obj.StrValueDic) do
    Log(' ' + i.Key + ': ' + IntToStr(TValue(i.Value).asInteger));

  Log('StrObjDic : ');
  for var i in TJX2StrObjDic(Obj.StrObjDic) do
  begin
    var o := TSimpleObject(i.Value);
    Log(' ' + i.Key + ' : ' + o.var1.AsString + ', ' + o.var2.AsString);
  end;

  Obj.Free;

//----------------------------------------------------------------------------//

  Log('Read JsonFile');
  FS := TFileStream.Create('Beauty.json', fmOpenRead + fmShareDenyRead);
  var RJson := FS.ReadRawString(TEncoding.UTF8);
  FS.Free;

  Obj := W3DJSX2.Deserialize<TComplexObj>(Json);
  Log( W3DJSX2.Serialize(Obj) );
  Obj.Free;

end;

procedure TSimpleObject.SetVar1(v: TValue); begin Var1 := v; end;
function TSimpleObject.GetVar1: TValue; begin Result := Var1; end;
end.

