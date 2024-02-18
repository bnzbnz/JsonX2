unit uByInterfaces;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.TypInfo,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo
  , System.Generics.Collections
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

  ISimpleIntf = interface(IJX2)
    ['{24ECF6FF-0FDA-4747-A356-99DD3CCE7289}']
    procedure Setvar1(v: Variant);
    function Getvar1: variant;
    procedure Setvar2(v: Variant);
    function Getvar2: variant;
    property var1: Variant read Getvar1 write Setvar1;
    property var2: Variant read Getvar2 write Setvar2;
  end;

  TSimpleObj = class(TIJX2, ISimpleIntf, IW3DCloneable)
    var1: variant;
    var2: variant;
    procedure Setvar1(v: Variant);
    function Getvar1: variant;
    procedure Setvar2(v: Variant);
    function Getvar2: variant;
  end;

  IComplexIntf = interface
    ['{24ECF6FF-0FDA-4747-A356-99DD3CCE7289}']
    procedure SetvalNull(v: Variant);
    function GetvalNull: variant;
    procedure SetvalString(v: Variant);
    function GetvalString: variant;
    procedure SetvalInteger(v: Variant);
    function GetvalInteger: variant;
    procedure SetvalDouble(v: Variant);
    function GetvalDouble: variant;
    procedure SetvalBoolean(v: Variant);
    function GetvalBoolean: variant;
    procedure SetvalDateTime(v: Variant);
    function GetvalDateTime: variant;
    procedure SetvalType(v: Variant);
    function GetvalType: variant;
    procedure SetArrayOfVar(v: IJX2VarList);
    function GetArrayOfVar: IJX2VarList;
    procedure SetArrayOfObj(v: IJX2ObjList);
    function GetArrayOfObj: IJX2ObjList;
    procedure SetDicOfVarVar(v: IJX2VarVarDic);
    function GetDicOfVarVar: IJX2VarVarDic;
    procedure SetDicOfVarObj(v: IJX2VarObjDic);
    function GetDicOfVarObj: IJX2VarObjDic;

    property ArrayOfVar: IJX2VarList read GetArrayOfVar write SetArrayOfVar;
    property ArrayOfObj: IJX2ObjList read GetArrayOfObj write SetArrayOfObj;
    property DicOfVarVar: IJX2VarVarDic read GetDicOfVarVar write SetDicOfVarVar;
    property DicOfVarObj: IJX2VarObjDic read GetDicOfVarObj write SetDicOfVarObj;
  end;

  TComplexObj = class(TIJX2, IComplexIntf, IW3DCloneable)
  public
    // The fields order does not matter
    valNull: variant;
    valString: variant;
    valInteger: variant;
    valBoolean: variant;
    valDouble: variant;
    valDateTime: variant; // ISO 8601 UTC Date string
    [JX2AttrName('type')] // 'type' is a reserved Pascal keyword
    valType: variant;

    [JX2AttrClass(TIJX2VarList)]  // Array of variant (enumerable List)
    ArrayOfVar: IJX2VarList;
    [JX2AttrClass(TIJX2ObjList, TSimpleObj)]  // The class of the contained object type
    ArrayOfObj: IJX2ObjList;    // Array of object (enumerable List)

    [JX2AttrClass(TIJX2VarVarDic)]
    DicOfVarVar: IJX2VarVarDic; // Dictionary of variant(string)/variant

    [JX2AttrClass(TIJX2VarObjDic, TSimpleObj)]  // The class of the contained object type
    DicOfVarObj: IJX2VarObjDic; // Dictionary of variant(string)/object

    procedure SetvalNull(v: Variant);
    function GetvalNull: variant;
    procedure SetvalString(v: Variant);
    function GetvalString: variant;
    procedure SetvalInteger(v: Variant);
    function GetvalInteger: variant;
    procedure SetvalDouble(v: Variant);
    function GetvalDouble: variant;
    procedure SetvalBoolean(v: Variant);
    function GetvalBoolean: variant;
    procedure SetvalDateTime(v: Variant);
    function GetvalDateTime: variant;
    procedure SetvalType(v: Variant);
    function GetvalType: variant;
    procedure SetArrayOfVar(v: IJX2VarList);
    function GetArrayOfVar: IJX2VarList;
    procedure SetArrayOfObj(v: IJX2ObjList);
    function GetArrayOfObj: IJX2ObjList;
    procedure SetDicOfVarVar(v: IJX2VarVarDic);
    function GetDicOfVarVar: IJX2VarVarDic;
    procedure SetDicOfVarObj(v: IJX2VarObjDic);
    function GetDicOfVarObj: IJX2VarObjDic;

  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.Log(Str: string);
begin
  Memo1.Lines.Add(Str);
end;

procedure TForm2.Button1Click(Sender: TObject);
var
  Json: string;
  Intf, CloneIntf: IComplexIntf;
  SimpleIntf: ISimpleIntf;
  SimpleObj: TSimpleObj;

  begin

  Intf := TComplexObj.Create;

  Intf.SetvalNull(Null);
  Intf.SetvalString('a string');
  Intf.SetvalInteger(11);
  Intf.SetvalBoolean(False);
  Intf.SetvalDateTime(Now);
  Intf.SetvalType('erroneous field name...');

  Intf.ArrayOfVar := TIJX2VarList.Create;
  TIJX2VarList(Intf.ArrayOfVar).Add('A');
  TIJX2VarList(Intf.ArrayOfVar).Add('B');

  Intf.ArrayOfObj := TIJX2ObjList.Create;
  SimpleIntf := TSimpleObj.Create;
  SimpleIntf.var1 := 1;
  SimpleIntf.var2 := 'one';
  TIJX2ObjList(Intf.ArrayOfObj).Add(SimpleIntf);
  SimpleObj := TSimpleObj.Create;
  SimpleObj.var1 := 2;
  SimpleObj.var2 := 'two';
  TIJX2ObjList(Intf.ArrayOfObj).Add(SimpleObj);

  Intf.DicOfVarVar := TIJX2VarVarDic.Create;
  TIJX2VarVarDic(Intf.DicOfVarVar).Add('a','one');
  TIJX2VarVarDic(Intf.DicOfVarVar).Add('b','two');
  TIJX2VarVarDic(Intf.DicOfVarVar).Add('c','three');


  Intf.DicOfVarObj := TIJX2VarObjDic.Create;
  SimpleIntf := TSimpleObj.Create;
  SimpleIntf.var1 := 3;
  SimpleIntf.var2 := 'three';
  TIJX2VarObjDic(Intf.DicOfVarObj).Add('c', SimpleIntf);
  SimpleIntf := TSimpleObj.Create;
  SimpleIntf.var1 := 4;
  SimpleIntf.var2 := 'four';
  TIJX2VarObjDic(Intf.DicOfVarObj).Add('d', SimpleIntf);

  // Obj Serialization
  Json := W3DJSX2.Serialize(Intf);
  Memo1.Lines.Add( 'Interface:');
  Memo1.Lines.Add( Json );

   // Cloning through serialization
  CloneIntf := W3DJSX2.Deserialize<TComplexObj>(Json);
  Json := W3DJSX2.Serialize(CloneIntf);
  Log( 'Cloned Object:' );
  Log( Json );


  // Native CLoning
  CloneIntf := TIJX2(Intf).Clone as IComplexIntf;
  Log( 'Natively Cloned Object:');
  Log( W3DJSX2.Serialize(CloneIntf) );

end;


procedure TSimpleObj.Setvar1(v: Variant); begin var1 := v; end;
function TSimpleObj.Getvar1: variant; begin Result := var1; end;
procedure TSimpleObj.Setvar2(v: Variant); begin var2 := v; end;
function TSimpleObj.Getvar2: variant; begin Result := var2; end;

procedure TComplexObj.SetvalNull(v: Variant); begin valNull := v; end;
function TComplexObj.GetvalNull: variant; begin Result := valNull; end;
procedure TComplexObj.SetvalString(v: Variant); begin valString := v; end;
function TComplexObj.GetvalString: variant; begin Result := valString; end;
procedure TComplexObj.SetvalInteger(v: Variant); begin valInteger := v; end;
function TComplexObj.GetvalInteger: variant; begin Result := valInteger; end;
procedure TComplexObj.SetvalDouble(v: Variant); begin valDouble := v; end;
function TComplexObj.GetvalDouble: variant; begin Result := valDouble; end;
procedure TComplexObj.SetvalBoolean(v: Variant); begin valBoolean := v; end;
function TComplexObj.GetvalBoolean: variant; begin Result := valBoolean; end;
procedure TComplexObj.SetvalDateTime(v: Variant); begin valDateTime := v; end;
function TComplexObj.GetvalDateTime: variant; begin Result := valDateTime; end;
procedure TComplexObj.SetvalType(v: Variant); begin valType := v; end;
function TComplexObj.GetvalType: variant; begin Result := valType; end;
procedure TComplexObj.SetArrayOfVar(v: IJX2VarList); begin ArrayOfVar := v; end;
function TComplexObj.GetArrayOfVar: IJX2VarList; begin Result := ArrayOfVar; end;
procedure TComplexObj.SetArrayOfObj(v: IJX2ObjList); begin ArrayOfObj := v; end;
function TComplexObj.GetArrayOfObj: IJX2ObjList; begin Result := ArrayOfObj; end;
procedure TComplexObj.SetDicOfVarVar(v: IJX2VarVarDic); begin DicOfVarVar := v; end;
function TComplexObj.GetDicOfVarVar: IJX2VarVarDic; begin Result := DicOfVarVar; end;
procedure TComplexObj.SetDicOfVarObj(v: IJX2VarObjDic); begin DicOfVarObj := v; end;
function TComplexObj.GetDicOfVarObj: IJX2VarObjDic; begin Result := DicOfVarObj; end;

end.



