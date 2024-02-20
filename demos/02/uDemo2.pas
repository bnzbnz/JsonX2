unit uDemo2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls
  , System.Generics.Collections
  , W3DJsonX2.Obj
  , W3DJsonX2
  , W3DJsonX2.RTTI
  , W3DJsonX2.Types
  , W3DJsonX2.Utils
  , W3DCloneable, FMX.Layouts
  ;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Splitter1: TSplitter;
    Layout1: TLayout;
    Memo2: TMemo;
    Memo3: TMemo;
    Splitter2: TSplitter;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Log(Str: string);
    procedure Parser(JX2Object: TJX2; Spacer: string = '');
  end;

var
  Form2: TForm2;

type

  // Json Mapping :: https://json-generator.com/

  TJGFriendObj = class(TJX2)
    id: variant;
    name: variant;
  end;

  TJGArray = class(TJX2)
    _id: variant;
    index: variant;
    guid: variant;
    isActive: variant;
    balance: variant;
    picture: variant;
    age: variant;
    eyeColor: variant;
    name: variant;
    gender: variant;
    company: variant;
    email: variant;
    phone: variant;
    address: variant;
    about: variant;
    registered: variant;
    latitude: variant;
    longitude: variant;
    tags: TJX2VarList;
    [JX2AttrClass(TJGFriendObj)]
    friends: TJX2ObjList;
    greeting: variant;
    favoriteFruit: variant;
  end;

  TJG = class(TJX2)
    [JX2AttrClass(TJGArray)]
    JsonGenerator: TJX2ObjList;
  end;

implementation
uses RTTI;

{$R *.fmx}

procedure TForm2.Log(Str: string);
begin
  Memo2.Lines.Add(Str);
end;

procedure TForm2.Parser(JX2Object: TJX2; Spacer: string = '');
var
  LField: TRTTIField;
  LInstance: TRTTIInstanceType;
  LObj: TObject;
  JX2Obj : TJX2;
  v: variant;
begin
  for LField in W3DJsonX2.RTTI.GetFields(JX2Object) do
    if LField.FieldType.TypeKind in [tkVariant] then
      Log(Spacer + LField.Name + '=' + VarToStr(LField.GetValue(JX2Object).AsVariant))
    else if LField.FieldType.TypeKind in [tkClass] then
    begin
      LInstance := LField.FieldType.AsInstance;
      LObj := LField.GetValue(JX2Object).AsObject;
      if Assigned(LObj) then
      begin
        if LInstance.MetaclassType = TJX2VarList then
        begin
          Log(Format('%s%s : Array Of %d Variants:', [Spacer,  LField.Name, TJX2ObjList(LObj).Count]));
          for v in TJX2VarList(LObj) do
             Log(Spacer + '  '  + VarToStr(v));
        end else
        if LInstance.MetaclassType = TJX2ObjList then
        begin
          Log(Format('%s%s : Array Of %d Objects:', [Spacer,  LField.Name, TJX2ObjList(LObj).Count]));
          for LObj in TJX2ObjList(LObj) do Parser(LObj as TJX2, Spacer + '  ');
        end else
        if LInstance.MetaclassType = TJX2VarVarDic then
          // not used
        else
        if LInstance.MetaclassType = TJX2VarObjDic then
          // not used
        else
        begin
          Parser( LField.GetValue(JX2Object).AsObject as TJX2, Spacer );
        end;
      end;
    end;
end;

procedure TForm2.Button1Click(Sender: TObject);
Var
  Obj: TJG;
begin
  Obj := W3DJSX2.Deserialize<TJG>(Memo1.Lines.Text);
  Parser(Obj);
  Memo3.Lines.Text := W3DJSX2.Serialize(Obj);
  Obj.Free;
end;

end.
