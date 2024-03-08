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
  , W3DJsonX2.Conv
  , W3DJsonX2.Utils
  , W3DCloneable
  ;

type

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
  TSimpleObject = class(TJX2)                                   // TSimple Object, must inherit from TIJX2 if interfaced
  private
    [JX2AttrName('var1')]                                                       // Fvar1 field name will be mapped as json "var" name
    Fvar1: TValue;                                                              // Fvar1 varianle
  public
    var2: TValue;                                                               //  a TValue var2 variable
    SubObj: TSimpleSubObject;                                                      // an Object (TObject)
    procedure SetVar1(v: TValue);                                               // Settet
    function GetVar1: TValue;                                                   // Getter
    property Var1: TValue read GetVar1 write SetVar1;                           // Property Var1
  end;

  TISimpleIntf = class(TIJX2)
    var5 : TValue;
  end;

  // A complex object definitionr
  TComplexObj = class(TJX2)                                                     // inherit from TJX2)
  public

    // The fields order does not matter

    VaRList: TJX2VarList;

    valNull: TValue;                                                            // null value
    valString: TValue;                                                          // string
    valInteger: TValue;
    valBoolean: TValue;                                                         // Integer
    {$IFNDEF JSX_NOVAR}
    valVariantString: Variant;                                                  // variant: string;
    [JX2AttrExclude]                                                        // Boolean
    valDouble: Variant;                                                         // Double
    valDateTime: variant;                                                       // DateTime in fact Double
    valUTCDateTime: variant;                                                    // DateTime ISO 8601 UTC, standard
    {$ENDIF}
    [JX2AttrName('type')]
    valType: TValue;                                                             // A TValue 'valType' mapped to Json 'type'

                                                                                // Object and Interface definition
    ObjectType: TSimpleObject;
    [JX2AttrClass(TISimpleIntf)]                                               // The interfaced object of the interface
    IntfType: IJX2;

    {$IFNDEF JSX_NOVAR}                                                                            //List of values (array) :

    [JX2AttrConv(TIJX2VariantListConv)]
    VaRList2: TList<Variant>;
                                                          // variant list
    {$ENDIF}
    VaLlist: TJX2ValueList;                                                     // TValue List
    [JX2AttrConv(TIJX2ValueListConv)]
    VaLListWithConc: TList<TValue>;


    {$IFNDEF JSX_NOVAR}
    [JX2AttrClass(TIJX2VaRList)]                                                // Object mapped to the variant list interface
    IVaRlist: IJX2VaRList;
    {$ENDIF}                                                                    // Interfaced variant list
    [JX2AttrClass(TIJX2ValueList)]                                              // Object mapped to the TValue list interface
    IVaLlist: IJX2ValueList;                                                    // Interfaced TValue list

    // List of Objects;
    [JX2AttrClass(TSimpleObject)]
    ObjList: TJX2ObjList;
    [JX2AttrClass(TIJX2ObjList, TISimpleIntf)]                                 // Define the Interface object of the IObjList interface and the contained one.
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
    [JX2AttrClass(TIJX2StrObjDic, TISimpleIntf)]                               // Define the objext type and containned objects of the following Interfaced dictionnary :
    IStrObjDic: IJX2StrObjDic;

    //Generic Object/Inteface CallBack Converter

    {$IFNDEF JSX_NOVAR}
    [JX2AttrConv(TIJX2VariantListConv)]
    ConvVariantList: TList<Variant>;
    {$ENDIF}

    [JX2AttrConv(TIJX2ValueListConv)]
    ConvValueList: TList<TValue>;

    [JX2AttrConv(TIStringListConv)]
    TSL: TStringList;

  end;

var
  Form2: TForm2;
  bbb: TISimpleIntf;

implementation
uses DateUtils;

{$R *.fmx}

procedure TForm2.Button1Click(Sender: TObject);
var
  Json, JsonBeauty: string;
  Obj, CloneObj:  TComplexObj;
  Simple: TSimpleObject;
  ISimple: TISimpleIntf;
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
  Obj.valUTCDateTime := DateToIso8601( TTimeZone.Local.ToUniversalTime(Now) ); // an UTC ISO8601 DateTime (string);
  {$IFNDEF JSX_NOVAR}
  Obj.valVariantString := 'Variant : ooŘaa鱇bb😃cc'; // variant string
  Obj.valBoolean := False;  // Boolean value
  Obj.valDouble := 2.2;     // Double value
  Obj.valDateTime := Double(Now); // Datetime: double value
  {$ENDIF}
  Obj.valType := 'erroneous delphi field name...'; // string of a renamed field


  // Object/Interface

  Obj.ObjectType := TSimpleObject.Create;
  Obj.ObjectType.var1 := 'a var 1 Object';
  Obj.ObjectType.var2 := 'a var 2 Object';
  Obj.ObjectType.SubObj := TSimpleSubObject.Create;
  Obj.ObjectType.SubObj.var3 := '3 SubObj';
  Obj.ObjectType.SubObj.var4 := '4 SubObj';

  Obj.IntfType := TISimpleIntf.Create;     // Use cloning for copying previous object, Inteface
  TISimpleIntf(Obj.IntfType).var5 := 'a var 5 Intf';       // Set values :

  // Lists (arrays) of Var/Value/Obj

  {$IFNDEF JSX_NOVAR}
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


  {$IFNDEF JSX_NOVAR}
  Obj.IVaRList := TIJX2VarList.Create;
  TIJX2VaRList(Obj.IVaRList).Add('6A');
  TIJX2VaRList(Obj.IVaRList).Add('7B');
  {$ENDIF}


  Obj.IVaLList := TIJX2ValueList.Create;
  TIJX2ValueList(Obj.IVaLList).Add('8C');
  TIJX2ValueList(Obj.IVaLList).Add('9D');

  Obj.StrValueDic := TJX2StrValueDic.Create;
  Obj.StrValueDic.Add('1', 7);
  Obj.StrValueDic.Add('2', 13);

  Obj.IObjList := TIJX2ObjList.Create;
  ISimple := TISimpleIntf.Create;
  ISimple.var5 := 10;
  TIJX2ObjList(Obj.IObjList).Add(ISimple);
  ISimple := TISimpleIntf.Create;
  ISimple.var5 := 20;
  TIJX2ObjList(Obj.IObjList).Add(ISimple);

  // Dictionnaries Var/Values/Object
  {$IFNDEF JSX_NOVAR}
  Obj.StrVarDic := TJX2StrVarDic.Create;
  Obj.StrVarDic.Add('1','A');
  Obj.StrVarDic.Add('2', 12);
  {$ENDIF}


  Obj.StrObjDic := TJX2StrObjDic.Create;
  Simple := TSimpleObject.Create;
  Simple.var1 := 'ValueKey';
  Simple.var2 := 'ObjValue ''V2/''/ escaped';
  Obj.StrObjDic.Add('Key1', Simple);

  {$IFNDEF JSX_NOVAR}
  Obj.IStrVarDic := TIJX2StrVarDic.Create;
  TIJX2StrVarDic(Obj.IStrVarDic).Add('3','B');
  TIJX2StrVarDic(Obj.IStrVarDic).Add('4', 13);
  {$ENDIF}

  Obj.IStrValueDic := TIJX2StrValueDic.Create;
  TIJX2StrValueDic(Obj.IStrValueDic).Add('3','D');
  TIJX2StrValueDic(Obj.IStrValueDic).Add('4', 14);

  Obj.IStrObjDiC := TIJX2StrObjDic.Create;
  ISimple := TISimpleIntf.Create;
  ISimple.var5 := 'V1';
  TIJX2StrObjDic(Obj.IStrObjDic).Add('1', ISimple);


  Obj.TSL := TStringList.Create;
  Obj.TSL.Add('TSL value 1');
  Obj.TSL.Add('TSL value 2');

  Obj.ConvValueList := TList<TValue>.Create;
  Obj.ConvValueList.Add('AAA');
  Obj.ConvValueList.Add(123);

  {$IFNDEF JSX_NOVAR}
  Obj.ConvVariantList := TList<Variant>.Create;
  Obj.ConvVariantList.Add('BBB');
  Obj.ConvVariantList.Add(456);
  {$ENDIF}


//----------------------------------------------------------------------------//


  // Obj Serialization

  Json := W3DJX2.Beautifier(W3DJX2.Serialize(Obj, []), True);
  Memo1.Lines.Add( 'Object (create file Beauty.json):');
  Memo1.Lines.Add( Json + '    Lenght: ' + Length(Json).ToString);

  FS := TFileStream.Create('Beauty.json', fmCreate);
  FS.WriteRawUTF8String(UTF8String(Json));
  FS.Free;

  // Cloning through serialization
  CloneObj := W3DJX2.Deserialize<TComplexObj>(Json, []);
  Json := W3DJX2.Beautifier(W3DJX2.Serialize(CloneObj, []), True);
  Memo1.Lines.Add( 'Cloned Object (Json Des/Ser):');
  Memo1.Lines.Add( Json + '    Lenght: ' + Length(Json).ToString);
  CloneObj.Free;

  // Native CLoning
  CloneObj := TComplexObj(Obj.Clone);
  Memo1.Lines.Add( 'Natively Cloned Object :');
  Json := W3DJX2.Beautifier(W3DJX2.Serialize(CloneObj), True);
  Memo1.Lines.Add( Json + '    Lenght: ' + Length(Json).ToString);
  CloneObj.Free;

  JsonBeauty := W3DJX2.Beautifier(Json, False);
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
  Memo3.Lines.Add('IntfType.var1: ' + TISimpleIntf(Obj.IntfType).Var5.ToString());

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

//----------------------------------------------------------------------------//

  Memo1.Lines.Add('Read Json file (Beauty.json)');
  Memo4.Lines.Add('Read Json file (Beauty.json)');
  FS := TFileStream.Create('Beauty.json', fmOpenRead + fmShareDenyNone);
  Json := FS.ReadRawString(TEncoding.UTF8);
  FS.Free;

  Memo1.Lines.Add( Json + '    Lenght: ' + Length(Json).ToString);
  Memo4.Lines.Add( W3DJX2.Beautifier(Json) );

  Obj.Free;

end;

procedure TSimpleObject.SetVar1(v: TValue); begin Fvar1 := v; end;
function TSimpleObject.GetVar1: TValue; begin Result := Fvar1; end;


end.

