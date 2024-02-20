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
  , W3DCloneable
  ;

type
  TForm2 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    Splitter1: TSplitter;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
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

{$R *.fmx}

procedure TForm2.Button1Click(Sender: TObject);
Var
  Obj: TJG;
begin
  Obj := W3DJSX2.Deserialize<TJG>(Memo1.Lines.Text);
  Caption := Obj.JsonGenerator.Count.ToString;
  Memo2.Lines.Text := W3DJSX2.Serialize(Obj);
  Obj.Free;
end;

end.
