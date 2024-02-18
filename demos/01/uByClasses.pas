unit uByClasses;

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

  TSimpleObj = class(TJX2)
    var1: variant;
    var2: variant;
  end;

  TComplexObj = class(TJX2)
  public
    // The fields order does not matter
    valNull: variant;
    valString: variant;
    valInteger: variant;
    valBoolean: variant;
    valDouble: variant;
    valDateTime: variant; // ISO 8601 UTC Date string
    [JX2AttrName('type')] // JsonName is a reserved Pascal keyword
    valType: variant;

    ArrayOfVar: TJX2VarList; // Array of variant (enumerable List)

    [JX2AttrClass(TSimpleObj)]  // The class of the contained object type
    ArrayOfObj: TJX2ObjList;    // Array of object (enumerable List)

    DicOfVarVar: TJX2VarVarDic; // Dictionary of variant(string)/variant

    [JX2AttrClass(TSimpleObj)]  // The class of the contained object type
    DicOfVarObj: TJX2VarObjDic; // Dictionary of variant(string)/object
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
  Obj, CloneObj: TComplexObj;
  Simple: TSimpleObj;
begin

  // Create a Json Object
  Obj := TComplexObj.Create;
  Obj.valNull := Null;
  Obj.valString := 'a string';
  Obj.valInteger := 11;
  Obj.valBoolean := False;
  Obj.valDateTime := Now;
  Obj.valType := 'erroneous field name...';

  Obj.ArrayOfVar := TJX2VarList.Create;
  Obj.ArrayOfVar.Add('A');
  Obj.ArrayOfVar.Add('B');

  Obj.ArrayOfObj := TJX2ObjList.Create;
  Simple := TSimpleObj.Create;
  Simple.var1 := 1;
  Simple.var2 := 'one';
  Obj.ArrayOfObj.Add(Simple);

  Simple := TSimpleObj.Create;
  Simple.var1 := 2;
  Simple.var2 := 'two';
  Obj.ArrayOfObj.Add(Simple);

  Obj.DicOfVarVar := TJX2VarVarDic.Create;
  Obj.DicOfVarVar.Add('a','one');
  Obj.DicOfVarVar.Add('b','two');
  Obj.DicOfVarVar.Add('c','three');

  Obj.DicOfVarObj := TJX2VarObjDic.Create;
  Simple := TSimpleObj.Create;
  Simple.var1 := 3;
  Simple.var2 := 'three';
  Obj.DicOfVarObj.Add('a', Simple);
  Simple := TSimpleObj.Create;
  Simple.var1 := 4;
  Simple.var2 := 'four';
  Obj.DicOfVarObj.Add('b', Simple);

  // Obj Serialization
  Json := W3DJSX2.Serialize(Obj);
  Memo1.Lines.Add( 'Object:');
  Memo1.Lines.Add( Json );

  // Cloning through serialization
  CloneObj := W3DJSX2.Deserialize<TComplexObj>(Json);
  Json := W3DJSX2.Serialize(CloneObj);
  Log( 'Cloned Object:');
  Log( Json );
  CloneObj.Free;

  // Native CLoning
  CloneObj := TComplexObj(Obj.Clone);
  Log( 'Native Cloned Object:');
  Log( W3DJSX2.Serialize(CloneObj) );
  CloneObj.Free;

  //Accessing values
  var s := Obj.valString;
  var d := Obj.valDateTime;

  Log('ArrayOfVar :');
  for var v in Obj.ArrayOfVar  do
    Log(v);

  Log('ArrayOfObj : TSimpleObj');
  for var v in Obj.ArrayOfObj  do
  begin
    Simple := TSimpleObj(v);
    Log( VarToStr(Simple.var1) + ', ' + Simple.var2 );
  end;

  Log('DicOfVarVar :');
  for var v in Obj.DicOfVarVar  do
    Log( v.Key + '=> ' + v.Value);

  Log('DicOfVarObj : variant, TSimpleObj');
  for var v in Obj.DicOfVarObj  do
  begin
    Simple := TSimpleObj(v.Value);
    Log( v.Key + ' => ' + VarToStr(Simple.var1) + ', ' + Simple.var2 );
  end;

  // Clean Up
  Obj.Free;
end;

end.
