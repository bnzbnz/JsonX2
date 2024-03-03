unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.TypInfo,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.Layouts, Activex
  {$IFNDEF JSX_NOVAR}
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

  TStringListConv = class(TJX2Converter)
    function ToJson(ASelfObj: TObject): string;  override;
    function FromJson(AJson: string) : TObject; override;
    function Clone(ASelfObj: TObject): TObject; override;
  end;
  TIIntfListConv = class(TJX2Converter)
    function ToJson(ASelfObj: TObject): string;  override;
    function FromJson(AJson: string) : TObject; override;
    function Clone(ASelfObj: TObject): TObject; override;
  end;

  TForm2 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Layout1: TLayout;
    Layout2: TLayout;
    Memo2: TMemo;
    Layout3: TLayout;
    Memo3: TMemo;
    Memo4: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

//----------------------------------------------------------------------------//
// Json Objectx Definition :

 IGenericInterface = interface
  ['{70DE2C9D-81FD-4524-B886-C48D71A6BEE0}']
 end;

  TGenericInterfacedObject = class(TInterfacedObject, IGenericInterface)
    var AValue: Integer;
 end;

  // A really simple container
 TSimpleSubObject = Class(TJX2)
    var3: TValue;
    var4: TValue;
  end;

  // Interface on a TSimpleObject
  ISimpleObject = interface(IInterface) ['{D259E2E2-2EDD-456D-A4F2-4AD9AFA6494C}'] end;

  // a simple object with property and sub. object
  // ALL OBJECTS AND INTERFACED OBJECTS ARE OWNED
  // It means that you don't have to take care of their destruction (freeing)
  TSimpleObject = class(TIJX2, ISimpleObject)                                   // TSimple Object, must inherit from TIJX2 if interfaced
  private
    [JX2AttrName('var1')]                                                       // Fvar1 field name will be mapped as json "var" name
    Fvar1: TValue;                                                              // Fvar1 varianle
  public
    var2: TValue;                                                               //  a TValue var2 variable
    Obj: TSimpleSubObject;                                                      // an Object (TObject)
    procedure SetVar1(v: TValue);                                               // Settet
    function GetVar1: TValue;                                                   // Getter
    property Var1: TValue read GetVar1 write SetVar1;                           // Property Var1
  end;

  // A complex object definitionr
  TComplexObj = class(TJX2)                                                     // inherit from TJX2)
  public

    // The fields order does not matter

    valNull: TValue;                                                            // null value
    valString: TValue;                                                          // string
    valInteger: TValue;                                                         // Integer
    {$IFNDEF JSX_NOVAR}
    valVariantString: Variant;                                                  // variant: string;
    valBoolean: Variant;
    [JX2AttrExclude]                                                        // Boolean
    valDouble: Variant;                                                         // Double
    valDateTime: variant;                                                       // DateTime in fact Double
    valUTCDateTime: variant;                                                    // DateTime ISO 8601 UTC, standard
    {$ENDIF}
    [JX2AttrName('type')]
    valType: TValue;                                                             // A TValue 'valType' mapped to Json 'type'

                                                                                // Object and Interface definition
    ObjectType: TSimpleObject;
    [JX2AttrClass(TSimpleObject)]                                               // The interfaced object of the interface
    IntfType: IJX2;

    {$IFNDEF JSX_NOVAR}                                                                            //List of values (array) :
    VaRList: TJX2VarList;                                                       // variant list
    {$ENDIF}
    VaLlist: TJX2ValueList;                                                     // TValue List


    {$IFNDEF JSX_NOVAR}
    [JX2AttrClass(TIJX2VaRList)]                                                // Object mapped to the variant list interface
    IVaRlist: IJX2VaRList;
    {$ENDIF}                                                                    // Interfaced variant list
    [JX2AttrClass(TIJX2ValueList)]                                              // Object mapped to the TValue list interface
    IVaLlist: IJX2ValueList;                                                    // Interfaced TValue list

    // List of Objects;
    [JX2AttrClass(TSimpleObject)]
    ObjList: TJX2ObjList;
    [JX2AttrClass(TIJX2ObjList, TSimpleObject)]                                 // Define the Interface object of the IObjList interface and the contained one.
    IObjList: IJX2ObjList;

    // Dictionary of string/Values
    {$IFNDEF JSX_NOVAR}
    StrVarDic: TJX2StrVarDic;                                                   // a string/variant dictionnary
    {$ENDIF}
    StrValueDic: TJX2StrValueDic;                                               // a string/TValue dictionnary
    [JX2AttrClass(TSimpleObject)]                                              // Define the object contained in the following dictionnary :
    StrObjDic: TJX2StrObjDic;                                                   // a string/TObject dictionnary
    {$IFNDEF JSX_NOVAR}
    [JX2AttrClass(TIJX2StrVarDic)]
    IStrVarDic: IJX2StrVarDic;                                                  //Same with interfaced object
    {$ENDIF}
    [JX2AttrClass(TIJX2StrValueDic)]
    IStrValueDic: IJX2StrValueDic;
    [JX2AttrClass(TIJX2StrObjDic, TSimpleObject)]                               // Define the objext type and containned objects of the following Interfaced dictionnary :
    IStrObjDic: IJX2StrObjDic;


    [JX2AttrName('ConvertedIntf')]
    [JX2AttrConv(TIIntfListConv)]
    IIntf: IGenericInterface;

    //Generic Object/Inteface CallBack Converter
    [JX2AttrConv(TStringListConv)]
    TSL: TStringList;

  end;

var
  Form2: TForm2;

implementation
uses DateUtils;

{$R *.fmx}

procedure TForm2.Button1Click(Sender: TObject);
var
  Json, JsonBeauty: string;
  Obj, CloneObj:  TComplexObj;
  Simple: TSimpleObject;
  FS:  TFileStream;
begin

  Memo1.Lines.Clear;
  Memo2.Lines.Clear;
  Memo3.Lines.Clear;
  Memo4.Lines.Clear;

//----------------------------------------------------------------------------//

  // Create the main Json Object
  Obj := TComplexObj.Create;

  //Primitives

  Obj.valNull := nil; // a Null (nil) value
  Obj.valString := 'Value : ooŘaa鱇bb😃cc'; // a string UTF8
  Obj.valInteger := 15; // an Integer
  {$IFNDEF JSX_NOVAR}
  Obj.valVariantString := 'Variant : ooŘaa鱇bb😃cc'; // variant string
  Obj.valBoolean := False;  // Boolean value
  Obj.valDouble := 2.2;     // Double value
  Obj.valDateTime := Double(Now); // Datetime: double value
  Obj.valUTCDateTime := DateToIso8601( TTimeZone.Local.ToUniversalTime(Now) ); // an UTC ISO8601 DateTime (string);
  {$ENDIF}
  Obj.valType := 'erroneous delphi field name...'; // string of a renamed field

  // Object/Interface

  Obj.ObjectType := TSimpleObject.Create;
  Obj.ObjectType.var1 := 'a var 1 Object';
  Obj.ObjectType.var2 := 'a var 2 Object';
  Obj.ObjectType.Obj := TSimpleSubObject.Create;
  Obj.ObjectType.Obj.var3 := '3';
  Obj.ObjectType.Obj.var4 := '4';
  TJX2(Obj.ObjectType.Clone).Free;


  Obj.IntfType := TSimpleObject( Obj.ObjectType.Clone);     // Use cloning for copying previous object, Inteface
  TSimpleObject(Obj.IntfType).var1 := 'a var 5 Intf';       // Set values :
  TSimpleObject(Obj.IntfType).var2 := 'a var 6 Intf';
  TSimpleObject(Obj.IntfType).Obj.var3 := 'a var 7 Intf';   // Set new object values :
  TSimpleObject(Obj.IntfType).Obj.var4 := 'a var 8 Intf';

  Obj.IntfType := TSimpleObject.Create;
  TSimpleObject(Obj.IntfType).var1 := 'a var 3 Intf';
  TSimpleObject(Obj.IntfType).var2 := 'a var 4 Intf';
  TJX2(Obj.IntfType.Clone).Free;
                                                             // Testing Object Clone
  // Lists (arrays) of Var/Value/Obj

  {$IFNDEF JSX_NOVAR}
  Obj.VaRList := TJX2VarList.Create;
  Obj.VaRList.Add(1);   Obj.VaRList.Add(2);
  {$ENDIF}
  Obj.VaLlist := TJX2ValueList.Create;
  Obj.VaLlist.Add(3);   Obj.VaLlist.Add(4);
  Obj.VaLlist.Clone.Free;

  Obj.ObjList := TJX2ObjList.Create;
  Simple := TSimpleObject.Create;
  Simple.var1 := 5;
  Simple.var2 := 'five';
  TJX2ObjList(Obj.ObjList).Add(Simple);
  Obj.ObjList.Clone.Free;

  {$IFNDEF JSX_NOVAR}
  Obj.IVaRList := TIJX2VarList.Create;
  TIJX2VaRList(Obj.IVaRList).Add('6A');
  TIJX2VaRList(Obj.IVaRList).Add('7B');
  TIJX2(TIJX2VaRList(Obj.IVaRList).Clone).Free;
  {$ENDIF}

  Obj.IVaLList := TIJX2ValueList.Create;
  TIJX2ValueList(Obj.IVaLList).Add('8C');
  TIJX2ValueList(Obj.IVaLList).Add('9D');
  TIJX2(TIJX2ValueList(Obj.IVaLList).Clone).Free;

  Obj.IObjList := TIJX2ObjList.Create;
  Simple := TSimpleObject.Create;
  Simple.var1 := 10;
  Simple.var2 := 'ten';
  TIJX2ObjList(Obj.IObjList).Add(Simple);
  Simple := TSimpleObject.Create;
  Simple.var1 := 20;
  Simple.var2 := 'twenny';
  TIJX2ObjList(Obj.IObjList).Add(Simple);
  TIJX2(TIJX2ObjList(Obj.IObjList).Clone).Free;

  // Dictionnaries Var/Values/Object
  {$IFNDEF JSX_NOVAR}
  Obj.StrVarDic := TJX2StrVarDic.Create;
  Obj.StrVarDic.Add('1','A');
  Obj.StrVarDic.Add('2', 12);
  Obj.StrVarDic.Clone.Free;
  {$ENDIF}

  Obj.StrValueDic := TJX2StrValueDic.Create;
  Obj.StrValueDic.Add('1', 7);
  Obj.StrValueDic.Add('2', 13);
  Obj.StrValueDic.Clone.Free;

  Obj.StrObjDic := TJX2StrObjDic.Create;
  Simple := TSimpleObject.Create;
  Simple.var1 := 'ValueKey';
  Simple.var2 := 'ObjValue ''V2/''/ escaped';
  Obj.StrObjDic.Add('Key1', Simple);
  Obj.StrObjDic.Clone.Free;

  {$IFNDEF JSX_NOVAR}
  Obj.IStrVarDic := TIJX2StrVarDic.Create;
  TIJX2StrVarDic(Obj.IStrVarDic).Add('3','B');
  TIJX2StrVarDic(Obj.IStrVarDic).Add('4', 13);
  TIJX2(TIJX2StrVarDic(Obj.IStrVarDic).Clone).Free;
  {$ENDIF}

  Obj.IStrValueDic := TIJX2StrValueDic.Create;
  TIJX2StrValueDic(Obj.IStrValueDic).Add('3','D');
  TIJX2StrValueDic(Obj.IStrValueDic).Add('4', 14);
  TIJX2(TIJX2StrValueDic(Obj.IStrValueDic).Clone).Free;

  Obj.IStrObjDiC := TIJX2StrObjDic.Create;
  Simple := TSimpleObject.Create;
  Simple.var1 := 'V1';
  Simple.var2 := 'V2';
  TIJX2StrObjDic(Obj.IStrObjDiC).Add('SimpleX', Simple);
  TIJX2(TIJX2StrObjDic(Obj.IStrObjDiC).Clone).Free;

  Obj.TSL := TStringList.Create;
  Obj.TSL.Add('TSL value 1');
  Obj.TSL.Add('TSL value 2');

  Obj.IIntf := TGenericInterfacedObject.Create;


//----------------------------------------------------------------------------//


  // Obj Serialization
  Json := W3DJSX2.Serialize(Obj, []);
  Memo1.Lines.Add( 'Object (create file Beauty.json):');
  Memo1.Lines.Add( Json + '    Lenght: ' + Length(Json).ToString);

  FS := TFileStream.Create('Beauty.json', fmCreate);
  FS.WriteRawUTF8String(UTF8String(Json));
  FS.Free;

  // Cloning through serialization
  CloneObj := W3DJSX2.Deserialize<TComplexObj>(Json, []);
  Json := W3DJSX2.Serialize(CloneObj, []);
  Memo1.Lines.Add( 'Cloned Object (Json Des/Ser):');
  Memo1.Lines.Add( Json + '    Lenght: ' + Length(Json).ToString);
  CloneObj.Free;

  // Native CLoning
  CloneObj := TComplexObj(Obj.Clone);
  Memo1.Lines.Add( 'Natively Cloned Object :');
  Json := W3DJSX2.Serialize(CloneObj);
  Memo1.Lines.Add( Json + '    Lenght: ' + Length(Json).ToString);
  CloneObj.Free;

  JsonBeauty := JsonBeautifier(Json);
  Memo2.Lines.Add('Json Beautifier :');
  Memo2.Lines.Add(JsonBeauty);



//----------------------------------------------------------------------------//

  Memo3.Lines.Add('Read few values from Object:');

  Memo3.Lines.Add('String (TValue): ' + Obj.valString.AsString);
  {$IFNDEF JSX_NOVAR}
  Memo3.Lines.Add('String (Variant): ' + Obj.valVariantString);
  Memo3.Lines.Add('Double: ' + FloatToStr(Obj.valDouble));
  {$ENDIF}
  Memo3.Lines.Add('UTC: ' + Obj.valUTCDateTime);

  Memo3.Lines.Add('ObjectType.var1: ' + Obj.ObjectType.Var1.ToString);
  Memo3.Lines.Add('IntfType.var1: ' + TSimpleObject(Obj.IntfType).Var1.ToString());

  Memo3.Lines.Add('VaLlist : ');
  for var i in Obj.VaLlist do
    Memo3.Lines.Add('  ' + IntToStr(i.AsInteger));

  Memo3.Lines.Add('IVaLlist : ');
  for var i in TIJX2ValueList(Obj.IVaLlist) do
    Memo3.Lines.Add(i.AsString);

  Memo3.Lines.Add('ObjList : ');
  for var i in TJX2ObjList(Obj.ObjList) do
  begin
    var O := TSimpleObject(i);
    Memo3.Lines.Add('  ObjectType.var1: ' + o.var1.ToString);
  end;

  Memo3.Lines.Add('StrValueDic : ');
  for var i in TJX2StrValueDic(Obj.StrValueDic) do
    Memo3.Lines.Add(' ' + i.Key + ': ' + IntToStr(TValue(i.Value).asInteger));

  Memo3.Lines.Add('StrObjDic : ');
  for var i in TJX2StrObjDic(Obj.StrObjDic) do
  begin
    var o := TSimpleObject(i.Value);
    Memo3.Lines.Add(' ' + i.Key + ' : ' + o.var1.AsString + ', ' + o.var2.AsString);
  end;

  Obj.Free;

//----------------------------------------------------------------------------//

  Memo1.Lines.Add('Read Json file (Beauty.json)');
  Memo4.Lines.Add('Read Json file (Beauty.json)');
  FS := TFileStream.Create('Beauty.json', fmOpenRead + fmShareDenyNone);
  Json := FS.ReadRawString(TEncoding.UTF8);
  FS.Free;

  Obj := W3DJSX2.Deserialize<TComplexObj>(Json);
  Json := W3DJSX2.Serialize(Obj);
  Memo1.Lines.Add( Json + '    Lenght: ' + Length(Json).ToString);
  Memo4.Lines.Add( JsonBeautifier(Json) );

  Obj.Free;

end;

procedure TSimpleObject.SetVar1(v: TValue); begin Fvar1 := v; end;
function TSimpleObject.GetVar1: TValue; begin Result := Fvar1; end;

function TStringListConv.Clone(ASelfObj: TObject): TObject;
begin
  Result := TStringList.Create;
  for var LStr in TStringList(ASelfObj) do
    TStringList(Result).Add(LStr);
end;

function TStringListConv.FromJson(AJson: string): TObject;
var
  LIds: TJSONArray;
  LIdx: string;
begin
  Result := TStringList.Create;
  LIds := TJSONObject.Parse(AJson) as TJSONArray;
  for LIdx in LIds do
    TStringList(Result).Add(LIdx);
  LIds.Free;
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

{ TIIntfListConv }

function TIIntfListConv.Clone(ASelfObj: TObject): TObject;
begin
  Result := TGenericInterfacedObject.Create;
  TGenericInterfacedObject(Result).AValue :=  TGenericInterfacedObject(ASelfObj).AValue;
end;

function TIIntfListConv.FromJson(AJson: string): TObject;
begin
  Result := TGenericInterfacedObject.Create;
end;

function TIIntfListConv.ToJson(ASelfObj: TObject): string;
begin
  Result := '"XXXXXX I DONT CARE... XXXXXXX"';
end;

end.

