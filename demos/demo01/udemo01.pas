unit udemo01;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.TypInfo,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo
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
  end;

(*
    Simple JSON :

    { "_id": "AAA", "index"="bbb" }
*)

  // Object ex: Mapping simple object

  TSimpleObj = class(TJX2)
  public
    [JX2AttrName('_id')] //==> JsonName<>Fieldname conversion
    id: variant;
    index: variant;
  end;

  // Interface ex: Mapping ~sinple Interface

  ISimpleObjIntf = interface(IJX2)
  ['{F7645E0D-39A3-4819-BC9F-F17FAA621466}']
    function GetId: variant;
    procedure SetId(Value: variant);
    function GetIndex: variant;
    procedure SetIndex(Value: variant);
  end;
    
  TSimpleObjIntf = class(TIJX2, ISimpleObjIntf, IW3DCloneable)
  public
    [JX2AttrName('_id')]
    id: variant;
    index: variant;
    function GetId: variant;
    procedure SetId(Value: variant);
    function GetIndex: variant;
    procedure SetIndex(Value: variant);
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.Button1Click(Sender: TObject);
begin
  var str := '{ "_id": "AAA", "index": "bbb" }';

  var Obj := W3DJSX2.Deserialize<TSimpleObj>(str);
  Memo1.Lines.Add( W3DJSX2.Serialize(Obj) );
  Obj.Free;

  var Intf := W3DJSX2.Deserialize<TSimpleObjIntf>(str);
  Memo1.Lines.Add( W3DJSX2.Serialize(Intf) );
end;

function TSimpleObjIntf.GetId: variant; begin Result := Id; end;
procedure TSimpleObjIntf.SetId(Value: variant); begin Id := Value; end;
function TSimpleObjIntf.GetIndex: variant; begin Result := Index; end;
procedure TSimpleObjIntf.SetIndex(Value: variant); begin Index := Value; end;

end.
