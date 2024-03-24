unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.TypInfo,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.Layouts
  {$IFNDEF JSX_NOVAR}
  , System.Variants
  {$ENDIF}
  , System.Generics.Collections
  , RTTI
  , JsonX2.Obj
  , JsonX2
  , JsonX2.RTTI
  , JsonX2.Types
  , JsonX2.Conv
  , JsonX2.Utils
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


  // A really simple container
  TSimpleSubObject = Class(TJX2)                            // A really simple class (TJX2)
    var3: TValue;                                           // string
    var4: TValue;                                           // string... or anything else
  end;

  // Interface on a TSimpleObject                           // Generic interface
  ISimpleObject = interface(IInterface) ['{D259E2E2-2EDD-456D-A4F2-4AD9AFA6494C}'] end;

  TSimpleObject = class(TJX2)                               // TSimple Object, must inherit from TIJX2 if interfaced
  private
    [JX2AttrName('var1')]                                                       // Fvar1 field name will be mapped as json "var" name
    Fvar1: TValue;                                                              // Fvar1 varianle
  public
    SubObjXXX: TSimpleSubObject;                                                      // an Object (TObject)
    var2: TValue;                                                               //  a TValue var2 variable
    procedure SetVar1(v: TValue);                                               // Settet
    function GetVar1: TValue;                                                   // Getter
    property Var1: TValue read GetVar1 write SetVar1;                           // Property Var1
  end;

  TISimpleIntf = class(TIJX2, ISimpleObject)
    var5 : TValue;
  end;

  // ALL OBJECTS AND INTERFACED OBJECTS ARE OWNED
  // It means that you don't have to take care of their destruction (freeing)

  TComplexObj = class(TJX2)                                 // A complex object definition, which herits from TJX2
  public

    // The fields order does not matter !!!

    vNull: TValue;                                          // A null value, serialized only if with jxoNullify option
    vString: TValue;                                        // string
    vInteger: TValue;                                       // Integer
    vBoolean: TValue;                                       // Boolean
    {$IFNDEF JSX_NOVAR}                                     // => Variant Support may be disabled
    varString: Variant;                                     // string;
    [JX2AttrExclude]                                        // The subsequent field will not Ser/Deser (explicit)
    varDouble: Variant;                                     // Double
    varDateTime: variant;                                   // DateTime, in fact this is a Double
    {$ENDIF}
    vUTCDateTime: variant;                                  // DateTime, ISO 8601 UTC, standard time string
    [JX2AttrName('type')]                                   // Map the following Field to this Json value name
    vType: TValue;                                          // Json "type" = RTTIField "valType"
                                                            // Object and Interface definition
    ObjectType: TSimpleObject;                              // An object (TSimpleObject inherits from JX2)
    [JX2AttrClass(TISimpleIntf)]                            // An generic interface IJX2 of TISimpleInterface type
    IntfType: IJX2;                                         // IJX2

    {$IFNDEF JSX_NOVAR}                                                                            //List of values (array) :
    VaRList: TJX2VarList;                                   // Variants List
    {$ENDIF}
    VaLlist: TJX2ValueList;                                 // TValues List

    {$IFNDEF JSX_NOVAR}
    [JX2AttrClass(TIJX2VaRList)]                            // Variant Interfaced List :
    IVaRlist: IJX2VaRList;                                  // AttrClass defined the class associated to the interface
    {$ENDIF}
    [JX2AttrClass(TIJX2ValueList)]                          // Variant Interfaced List
    IVaLlist: IJX2ValueList;                                // AttrClass defines the class associated to the interface

    // List of Objects;
    [JX2AttrClass(TSimpleObject)]                           // Objects List
    ObjList: TJX2ObjList;                                   // AttrClass defines the list contains
    [JX2AttrClass(TIJX2ObjList, TISimpleIntf)]              // Same but Interfaced :
    IObjList: IJX2ObjList;                                  // The Interface class and the list content

    // Dictionary of string/Values
    {$IFNDEF JSX_NOVAR}
    StrVarDic: TJX2StrVarDic;                               // A string/Variant Dictionary
    {$ENDIF}
    StrValueDic: TJX2StrValueDic;                           // A string/TValueariant Dictionary
    [JX2AttrClass(TSimpleObject)]
    StrObjDic: TJX2StrObjDic;                               // A String/Object Dictionary
    {$IFNDEF JSX_NOVAR}
    [JX2AttrClass(TIJX2StrVarDic)]                          // An Interfaced string/Variant Dictionary
    IStrVarDic: IJX2StrVarDic;
    {$ENDIF}
    [JX2AttrClass(TIJX2StrValueDic)]
    IStrValueDic: IJX2StrValueDic;                          // An Interfaced string/TValue Dictionary
    [JX2AttrClass(TIJX2StrObjDic, TISimpleIntf)]
    IStrObjDic: IJX2StrObjDic;                              // An Interfaced string/Object Dictionary

    //Generic Object/Interface CallBack Converter

    [JX2AttrConv(TIStringListConv)]                         // an random object type with its own converter
    TSL: TStringList;                                       // see Jsonx2.conv

  end;

var
  Form2: TForm2;

implementation
uses DateUtils;

{$R *.fmx}

procedure TForm2.Button1Click(Sender: TObject);
var
  Json: string;
  Obj, CloneObj, LObj:  TComplexObj;
  Simple: TSimpleObject;
  ISimple: TISimpleIntf;
begin

  Memo1.Lines.Clear;
  Memo2.Lines.Clear;
  Memo3.Lines.Clear;
  Memo4.Lines.Clear;

//----------------------------------------------------------------------------//

  // Create the main Json Object
  Obj := TComplexObj.Create;

  //Primitives

  Obj.vNull := nil;
  Obj.vString := 'Value : UTF8:Ř-鱇-😃';
  Obj.vInteger := 15;
  Obj.vBoolean := False;
  {$IFNDEF JSX_NOVAR}
  Obj.varString := 'Variant : UTF8:Ř-鱇-😃';
  Obj.varDouble := 2.2;
  Obj.varDateTime := Now;
  {$ENDIF}
  Obj.vUTCDateTime := DateToIso8601( TTimeZone.Local.ToUniversalTime(Now) );
  Obj.vType := 'erroneous delphi field name...';

  // Object/Interface

  Obj.ObjectType := TSimpleObject.Create;
  Obj.ObjectType.var1 := 'a var 1 Object';
  Obj.ObjectType.var2 := 'a var 2 Object';
  Obj.ObjectType.SubObjXXX := TSimpleSubObject.Create;
  Obj.ObjectType.SubObjXXX.var3 := '3 SubObj';
  Obj.ObjectType.SubObjXXX.var4 := '4 SubObj';

  Obj.IntfType := TISimpleIntf.Create;
  TISimpleIntf(Obj.IntfType).var5 := 'a var 5 Intf';

  // Lists (arrays) of Var/Value/Obj

  {$IFNDEF JSX_NOVAR}
  Obj.VaRList := TJX2VarList.Create;
  Obj.VaRList.Add(11);   Obj.VaRList.Add('22');
  {$ENDIF}

  Obj.VaLlist := TJX2ValueList.Create;
  Obj.VaLlist.Add(3);
  Obj.VaLlist.Add(4);

  Obj.ObjList := TJX2ObjList.Create;
  Simple := TSimpleObject.Create;
  Simple.var1 := 5;
  Simple.var2 := 'five';
  TJX2ObjList(Obj.ObjList).Add(Simple);
  Simple := TSimpleObject.Create;
  Simple.var1 := 6;
  Simple.var2 := 'six';
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
  Simple.var1 := 'ValueKey 2';
  Simple.var2 := 'ObjValue 2';
  Obj.StrObjDic.Add('Key1', Simple);
  Simple := TSimpleObject.Create;
  Simple.var1 := 'ValueKey 2';
  Simple.var2 := 'ObjValue 2';
  Obj.StrObjDic.Add('Key2', Simple);

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

//----------------------------------------------------------------------------//

  // Obj Serialization

  Json := JX2.Beautifier(JX2.Serialize(Obj, []), True);
  Memo1.Lines.Add( 'Object Creation (generate Beauty.json file), Object is :');
  Memo1.Lines.Add( Json + '    Lenght: ' + Length(Json).ToString);

  SaveStringToFile('Beauty.json', Json, TEncoding.UTF8);

  // Cloning through serialization
  CloneObj := JX2.Deserialize<TComplexObj>(Json, []);
  Json := JX2.Beautifier(JX2.Serialize(CloneObj, []), True);
  Memo1.Lines.Add( 'Cloned Object (Json Des/Ser), New Cloned Object is :');
  Memo1.Lines.Add( Json + '    Lenght: ' + Length(Json).ToString);
  CloneObj.Free;

  // Native CLoning
  CloneObj := TComplexObj(Obj.Clone);
  Memo1.Lines.Add( 'Natively Cloned Object, New Cloned Object is :');
  Json := JX2.Beautifier(JX2.Serialize(CloneObj), True);
  Memo1.Lines.Add( Json + '    Lenght: ' + Length(Json).ToString);
  CloneObj.Free;

  // so Beautyful !!! :)
  Memo2.Lines.Add('Json Beautifier :');
  Memo2.Lines.Add( JX2.Beautifier(Json, False) );

//----------------------------------------------------------------------------//


  Memo3.Lines.Add('Read few values from Object:');

  Memo3.Lines.Add('String (TValue): ' + Obj.vString.AsString);
  {$IFNDEF JSX_NOVAR}
  Memo3.Lines.Add('String (Variant): ' + Obj.varString);
  Memo3.Lines.Add('Double: ' + FloatToStr(Obj.varDouble));
  Memo3.Lines.Add('UTC: ' + Obj.vUTCDateTime);
 {$ENDIF}

  Memo3.Lines.Add('ObjectType.var1: ' + Obj.ObjectType.Var1.ToString);
  Memo3.Lines.Add('IntfType.var1: ' + TISimpleIntf(Obj.IntfType).Var5.ToString());

  Memo3.Lines.Add('VaLlist : ');
  for var i in Obj.VaLlist do
    Memo3.Lines.Add('    ' + IntToStr(i.AsInteger));

  Memo3.Lines.Add('IVaLlist : ');
   for var i in TIJX2ValueList(Obj.IVaLlist) do
     Memo3.Lines.Add(i.AsString);

  Memo3.Lines.Add('ObjList : ');
  for var i in TJX2ObjList(Obj.ObjList) do
  begin
    var O := TSimpleObject(i);
    Memo3.Lines.Add('    ObjectType.var1: ' + o.var1.ToString);
  end;

  Memo3.Lines.Add('StrValueDic : ');
  for var i in TJX2StrValueDic(Obj.StrValueDic) do
    Memo3.Lines.Add('   ' + i.Key + ': ' + IntToStr(TValue(i.Value).asInteger));

  Memo3.Lines.Add('StrObjDic : ');
  for var i in TJX2StrObjDic(Obj.StrObjDic) do
  begin
    var o := TSimpleObject(i.Value);
    Memo3.Lines.Add('   ' + i.Key + ' : ' + o.var1.AsString + ', ' + o.var2.AsString);
  end;

//----------------------------------------------------------------------------//

  Memo1.Lines.Add('Read Json from geerated file (Beauty.json)');
  Memo4.Lines.Add('Read Json from geerated file (Beauty.json)');
  Json := LoadStringFromFile('Beauty.json', TEncoding.UTF8);

  Memo1.Lines.Add( Json + '    Lenght: ' + Length(Json).ToString);
  Memo4.Lines.Add( JX2.Beautifier(Json) );

  Memo1.Lines.Add('Deserialize Object from Beauty.json, New Object is :');
  LObj := JX2.Deserialize<TComplexObj>(Json);
  Json :=  JX2.Beautifier(JX2.Serialize(LObj, []), True);
  Memo1.Lines.Add( Json + '    Lenght: ' + Length(Json).ToString);

  LObj.Free;
  Obj.Free;

end;

procedure TSimpleObject.SetVar1(v: TValue); begin Fvar1 := v; end;
function TSimpleObject.GetVar1: TValue; begin Result := Fvar1; end;


end.

